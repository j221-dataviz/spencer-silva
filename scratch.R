
# box and whisker plot?

library(dplyr)
library(readr)
library(ggplot2)

## install fonts

install.packages("extrafont");library(extrafont)
font_import("Trebuchet MS")
library(ggplot2)
qplot(1:10)+theme(text=element_text(family="Trebuchet MS"))


#data import

qual_hitters <- read_csv("data/qual_hitters.csv")

qual_hitters2017 <- read_csv("data/qual_hitters_2017.csv")

## Stripchart for all data

stripchart(qual_hitters$HR, method = "jitter")
  
# box and whisker

qualhitters_since95_box <- ggplot(qual_hitters, aes(x=Season, y=HR, group=Season)) +
  geom_boxplot(notch = TRUE, fill = "white", colour = "#3366FF") +
  scale_x_continuous(breaks = c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015, 2016, 2017)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75), limits = c(0,75)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 24, base_family = "Helvetica") +
  geom_boxplot_interactive(aes(tooltip = Name))
  
print(qualhitters_since95_box)

## interactive box plot 

#load package

library(ggiraph)
library(htmlwidgets)

# make interactive version of the chart
boxplot_interactive <- ggiraph(code = print(qualhitters_since95_box), height_svg=4)

print(boxplot_interactive)

# save chart as a web page
saveWidget(boxplot_interactive, "boxplot_interactive.html", selfcontained = TRUE, libdir = NULL, background = "white")


# ggplot(qual_hitters, aes(x=Season, y=HR, group = hr_bin)) +
#   geom_bar(stat = "identity", fill = "#CCCCCC", alpha = 0.5, aes(color = hr_bin)) 

## filter from 2010-2017

qual_hitters1017 <- qual_hitters %>%
  filter(Season>=2010)

qual_hitters_96 <- qual_hitters %>%
  filter(Season>=1996)

# violin and jitter plots

# 1995 onwards

# jitter
ggplot(qual_hitters_96, aes(x=Season, y=HR, group=Season)) +
  geom_jitter(shape = 21, colour="black", size=2, stroke=0.1, width=0.3, aes(group=hr_bin, fill=hr_bin)) +
  scale_fill_brewer(palette = "Blues", guide=FALSE) +
  scale_x_continuous(breaks = c(1996:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 20, base_family = "Georgia") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  ylab("Home runs per player") +
  xlab("") +
  coord_flip()

# combined
ggplot(qual_hitters_96, aes(x=Season, y=HR, group=Season)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF", size = 0.2) +
  geom_jitter(shape = 21, colour="black", size=2, stroke=0.4, width=0.3, alpha = 0.5, aes(group=hr_bin, fill=hr_bin)) +
  scale_fill_brewer(palette = "Blues", guide=FALSE) +
  scale_x_continuous(breaks = c(1996:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 30, base_family = "Trebuchet MS") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  ylab("Home runs per player") +
  xlab("") 

# violin
ggplot(qual_hitters, aes(x=Season, y=HR, group=Season)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF", size = 0.2) +
  scale_x_continuous(breaks = c(1995:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 20, base_family = "Georgia") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  ylab("Home runs per player") +
  xlab("") +
  coord_flip()

# 2010 onwards

# jitter
ggplot(qual_hitters1017, aes(x=Season, y=HR, group=Season)) +
  geom_jitter(shape = 21, colour="black", size=2, stroke=0.1, width=0.3, aes(group=hr_bin, fill=hr_bin)) +
  scale_fill_brewer(palette = "Blues", guide=FALSE) +
  scale_x_continuous(breaks = c(2010:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 20, base_family = "Georgia") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylab("Home runs per player") +
  xlab("")

# violin
ggplot(qual_hitters1017, aes(x=Season, y=HR, group=Season)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF", size = 0.2) +
  scale_x_continuous(breaks = c(2010:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 20, base_family = "Georgia") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylab("Home runs per player") +
  xlab("")

# combined
ggplot(qual_hitters1017, aes(x=Season, y=HR, group=Season, label=Name)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF", size = 1) +
  geom_jitter(shape = 21, colour="black", size=10, stroke=0.5, width=0.3, alpha = 0.5, aes(group=hr_bin, fill=hr_bin)) +
  geom_text(size=9, aes(label=ifelse(HR>50,as.character(Name),'')),hjust=0.5,vjust=-1.25) +
  scale_fill_brewer(palette = "Blues", guide=FALSE) +
  scale_x_continuous(breaks = c(2010:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 55, base_family = "Trebuchet MS") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  theme(axis.title.x = element_text(size=36)) +
  theme(strip.text = element_text(size = 24, family = "Trebuchet MS", color = "#E65523")) +
  ylab("Home runs per player") +
  xlab("")

# combined interactive

ggplot(qual_hitters1017, aes(x=Season, y=HR, group=Season)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF", size = 0.2) +
  geom_jitter(shape = 21, colour="black", size=2, stroke=0.1, width=0.3, alpha = 0.5, aes(group=hr_bin, fill=hr_bin)) +
  scale_fill_brewer(palette = "Blues", guide=FALSE) +
  scale_x_continuous(breaks = c(2010:2017)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 20, base_family = "Georgia") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylab("Home runs per player") +
  xlab("")


## line chart (1910-2017)

# import data

leaguestats <- read_csv("data/league_stats.csv")

ggplot(leaguestats, aes(x=Season, y=(HR_BB*100))) +
  geom_line() +
  geom_point(size=12, shape=21, fill= "#08306b") +
  theme_minimal(base_size = 36, base_family = "Trebuchet MS") +
  theme(panel.grid = element_blank()) +
  ylab("Home runs per 100 batted balls") + 
  scale_y_continuous(limits = c(0,5)) +
  scale_x_continuous(breaks = seq(1910, 2015, by=10)) +
  geom_hline(yintercept=seq(0, 5, by=1), color = "gray", size = 0.1) +
  geom_vline(xintercept=seq(1910,2015, by=20), color = "gray", size = 0.1)

       


################################# 
### bringing in hr_tracker data

#import data

hr_tracker <- read_csv("data/hr_trackerclean.csv")

hr_totals <- read_csv("data/hr_trackertotals.csv")

cats <- c("season","mean_dist","median_dist","mean_speed", "med_speed")

season <- c(2010,2011,2012,2013,2014,2015,2016,2017)
  
mean_dist <- hr_tracker %>%
  group_by(season) %>%
  summarize(mean = mean(true_dist, na.rm = TRUE))

med_dist <- hr_tracker %>%
  group_by(season) %>%
  summarize(median = median(true_dist, na.rm = TRUE))

mean_speed <- hr_tracker %>%
  group_by(season) %>%
  summarize(mean = mean(speed_off_bat, na.rm = TRUE))
  
med_speed <- hr_tracker %>%
  group_by(season) %>%
  summarize(median = median(speed_off_bat, na.rm = TRUE))

type <- c("je","nd","pl")

type_luck <- hr_tracker %>%
  group_by(season) %>%
  summarize(count = n()) %>%
  
typeluck2010 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2010) %>%
  summarise(count = n())
  
typeluck2011 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2011) %>%
  summarise(count = n())

typeluck2012 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2012) %>%
  summarise(count = n())

typeluck2013 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2013) %>%
  summarise(count = n())

typeluck2014 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2014) %>%
  summarise(count = n())

typeluck2015 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2015) %>%
  summarise(count = n())

typeluck2016 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2016) %>%
  summarise(count = n())

typeluck2017 <- hr_tracker %>%
  group_by(type_luck) %>%
  filter(season==2017) %>%
  summarise(count = n())

hr_totals <- inner_join(hr_totals, mean_dist)

hr_totals <- inner_join(hr_totals, med_dist)

hr_totals <- inner_join(hr_totals, mean_speed)

hr_totals <- inner_join(hr_totals, med_speed)


hr_totals <- rename(hr_totals,c("mean"="mean_speed","median"="med_speed"))

write_csv(hr_totals, "hr_totals.csv")





  


