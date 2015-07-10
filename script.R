#+ license, echo=FALSE
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("data.table")
require("dplyr")
require("foreign")
require("ggplot2")
require("gridExtra")

#+ download.raw.data, echo=FALSE
data.info <- data.table(YEAR = 1975:2012) %.%
  mutate(URL = paste0("ftp://ftp.nhtsa.dot.gov/fars/", YEAR, "/DBF/FARS*"),
         DEST = paste0("data/raw/FARS", YEAR, ".zip")) %.%
  group_by(YEAR) %.%
  mutate(downloaded = ifelse(file.exists(DEST), 
                             TRUE, 
                             download.file(URL, DEST, quiet = TRUE, method = "wget") == 0))

#+ extract.fatalities, echo=FALSE
if (file.exists("data/fatalities.csv")) {
  data.fatalities <- fread("data/fatalities.csv")
} else {  
  codes <- as.data.table(fread("data/US_FIPS_Codes.csv"))
  setnames(codes, names(codes), make.names(names(codes)))
  
  data.fatalities <- data.table()
  
  for (i in 1:nrow(data.info)) {
    unzip(data.info$DEST[i], exdir = "tmp")
    
    filename <- grep("^per*", list.files("tmp/"), ignore.case = TRUE, value = TRUE)
    filename <- paste0("tmp/", filename)
    
    tmp <- as.data.table(read.dbf(filename)) %.%
      group_by(STATE, COUNTY) %.%
      summarize(FATALITIES = sum(INJ_SEV == 4)) %.%
      mutate(STATE.NAME = codes$State[match(STATE, codes$FIPS.State)],
             COUNTY.NAME = codes$County.Name[match(paste(STATE, "-", COUNTY), 
                                                   paste(codes$FIPS.State, "-", codes$FIPS.County))],
             YEAR = data.info$YEAR[i])
    setcolorder(tmp, c("YEAR", "STATE", "STATE.NAME", "COUNTY", "COUNTY.NAME", "FATALITIES"))
    data.fatalities <- rbind(data.fatalities, tmp)
    
    unlink("tmp", recursive=TRUE)
    
    print(data.info$YEAR[i])
  }
  
  write.csv(data.fatalities, "data/fatalities.csv", row.names = FALSE)
}

#+ fatalities.by.state, echo=FALSE
tmp <- data.fatalities %.%
  group_by(YEAR, STATE.NAME) %.%
  summarise(FATALITIES = sum(FATALITIES)) %.%
  ungroup() %.%
  mutate(ID = 1:length(FATALITIES))

graph <- ggplot(data = tmp,
                aes(x = YEAR,
                    y = FATALITIES,
                    fill = STATE.NAME)) +   
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.title.y = element_text(vjust = 0.4),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines"),
        legend.title = element_blank(),
        legend.key = element_rect(color = "white", size = 2),
        legend.position = "bottom",
        legend.background = element_rect(fill="gray90", size=.5)) +
  guides(fill = guide_legend(ncol = 6)) +  
  xlab("Year\n\n") + 
  ylab("Number of traffic fatalities") +
  geom_area(position = "stack") +
  geom_area(position = "stack", color = "white", show_guide=FALSE)

banner <- ggplot(data = data.table(x = 0, y = 0),
                 aes(x = x, y = y)) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.margin = unit(c(0,0,-1,-1), "lines"),
        panel.background = element_rect(fill = "grey40", color = "grey40")) + 
  geom_text(label = "") + 
  xlim(0, 10) +
  annotate("text", x = c(0, 10), y = 0, 
           label = c("GRAPHZOO.TUMBLR.COM", "SOURCE: NHTSA"),
           color = "white", hjust = c(0.1, 0.8),
           size = 4, family = "Avenir Next Condensed")

png("US_traffic_fatalities_by_state.png", width = 1200, height = 800)
grid.arrange(graph, banner, heights = c(1, .05))
dev.off()

#+ fatalities.NJ, echo=FALSE
graph <- ggplot(data = filter(tmp, STATE.NAME == "New Jersey"),
                aes(x = YEAR,
                    y = FATALITIES)) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.title.y = element_text(vjust = 0.4),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines")) +
  coord_cartesian(ylim = c(0, 1250)) +
  xlab("Year") + 
  ylab("Number of traffic fatalities") +
  geom_smooth(method = "lm", color = "tomato3") +
  geom_path(color = "dodgerblue4", size = 1) + 
  geom_point(color = "dodgerblue4", size = 4)

inset <- ggplotGrob(ggplot(data = filter(map_data("state"), region == "new jersey"), 
                           aes(x = long, 
                               y = lat, 
                               group = group)) + 
                      theme_minimal() + 
                      theme(line = element_blank(),
                            text = element_blank(),
                            title = element_blank(),
                            plot.margin = unit(c(0,0,-1,-1), "lines")) +
                      coord_fixed(ratio = 1) + 
                      geom_polygon(color = "dodgerblue4", fill = "dodgerblue4"))

graph <- graph + 
  annotation_custom(grob = inset, xmin = 1975, xmax = 1990, ymax = 750) +
  annotate("text", label = "New\nJersey", x = 1990, y = 150, family = "Courier", 
           size = 12, hjust = 1, fontface = "bold")

banner <- ggplot(data = data.table(x = 0, y = 0),
                 aes(x = x, y = y)) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.margin = unit(c(0,0,-1,-1), "lines"),
        panel.background = element_rect(fill = "grey40", color = "grey40")) + 
  geom_text(label = "") + 
  xlim(0, 10) +
  annotate("text", x = c(0, 10), y = 0, 
           label = c("GRAPHZOO.TUMBLR.COM", "SOURCE: NHTSA"),
           color = "white", hjust = c(0.1, 0.8),
           size = 4, family = "Avenir Next Condensed")

png("NJ_traffic_fatalities_by_year.png", width = 800, height = 600)
grid.arrange(graph, banner, heights = c(1, .05))
dev.off()

#+ change.by.state, echo=FALSE
changes <- tmp %.%
  group_by(STATE.NAME) %.%
  summarize(CHANGE.PERCENT = ((FATALITIES[YEAR == 2012] / FATALITIES[YEAR == 1975]) - 1) * 100) %.%
  mutate(STATE.NAME = factor(STATE.NAME, levels = STATE.NAME[order(CHANGE.PERCENT, decreasing = TRUE)])) %.%
  ungroup()

graph <- ggplot(data = changes,
                aes(x = STATE.NAME,
                    y = CHANGE.PERCENT,
                    fill = log(abs(CHANGE.PERCENT)) * sign(CHANGE.PERCENT))) +
  theme_minimal(base_size = 20) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.title.y = element_text(vjust = 0.4),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines")) +
  coord_flip(xlim = c(0, 55)) +
  xlab("US state") +
  ylab("Percent change\nin traffic fatalities\n1975-2012") +
  guides(fill = FALSE) +
  scale_fill_gradient2(low = "dodgerblue4", mid = "white", high = "red") +
  geom_bar(stat = "identity", position = "identity", color = "#000000") + 
  geom_hline(yintercept = 0) +
  geom_segment(aes(y = -2, x = 53, yend = -25, xend = 53), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(y = 2, x = 53, yend = 25, xend = 53), arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", y = c(-2, 2), x = 54, label = c("better", "worse"), 
           hjust = c(1, 0), size = 10, family = "Courier")
  

inset <- ggplotGrob(ggplot(data = map_data("state") , 
                           aes(x = long, 
                               y = lat, 
                               group = group)) + 
                      theme_minimal() + 
                      theme(line = element_blank(),
                            text = element_blank(),
                            title = element_blank(),
                            plot.margin = unit(c(0,0,-1,-1), "lines")) +
                      coord_fixed(ratio = 1) + 
                      geom_polygon(color = "dodgerblue4", fill = "dodgerblue4"))

graph <- graph + annotation_custom(grob = inset,
                                   ymin = -75, ymax = -15, xmax = 15)

banner <- ggplot(data = data.table(x = 0, y = 0),
                 aes(x = x, y = y)) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.margin = unit(c(0,0,-1,-1), "lines"),
        panel.background = element_rect(fill = "grey40", color = "grey40")) + 
  geom_text(label = "") + 
  xlim(0, 10) +
  annotate("text", x = c(0, 10), y = 0, 
           label = c("GRAPHZOO.TUMBLR.COM", "SOURCE: NHTSA"),
           color = "white", hjust = c(0.1, 0.8),
           size = 4, family = "Avenir Next Condensed")

png("US_traffic_fatalities_change_by_state.png", width = 800, height = 1000)
grid.arrange(graph, banner, heights = c(1, .05))
dev.off()

#+ map.by.county, echo=FALSE
tmp <- data.fatalities %.%
  filter(YEAR == 2012) %.%
  mutate(COUNTY.NAME = tolower(COUNTY.NAME),
         STATE.NAME = tolower(STATE.NAME))

map.counties <- as.data.table(map_data("county")) %.%
  mutate(fatalities = tmp$FATALITIES[match(paste0(region, subregion), paste0(tmp$STATE.NAME, tmp$COUNTY.NAME))])
  
ggplot(map.counties, 
       aes(x = long, 
           y = lat,
           group = group,
           fill = fatalities)) +
  theme_minimal(base_size = 20) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.title.y = element_text(vjust = 0.4),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines")) +
  scale_fill_gradient(low = "dodgerblue4", high = "red", na.value = "dodgerblue4", limits = c(0, 900)) +
  coord_fixed() +
  geom_polygon(color = "#FFFFFF25")




