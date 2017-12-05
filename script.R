# load required packaged
library(dplyr)
library(ggplot2)
library(readr)
library(ggridges)
library(gganimate)
library(tidyr) # this has function separate to separate e.g. first and last name
library(forcats)

#home runs by year

# create empty data frame to hold data
hr_tracker <- data_frame()

# list files in hr_tracker folder
files <- list.files("hr_tracker")

# loop to read in each file, convert Date form text to standard date format, appead to data frame
for (f in files) {
  print(f)
  tmp <- read_csv(paste0("hr_tracker/",f)) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%y"))
  names(tmp) <- c("Date","Video","Path","Hitter","Hitter_Team","Pitcher","Pitcher_Team","INN","Ballpark","Type_Luck","True_Dist","Speed_Off_Bat","Elev_Angle","Horiz_Angle","Apex","N_Parks")
  hr_tracker <- bind_rows(hr_tracker,tmp)
}
# remove the temporary frame
rm(tmp)

# write out as csv
write_csv(hr_tracker, "data/hr_tracker.csv", na="")

# import data for analysis
hitters <- read_csv("data/hitters_standard_1985_2017.csv")
hr_tracker <- read_csv("data/hr_tracker.csv")

## new variables in "hitters" = "batted_balls" and "hr_per_bb"
hitters <- hitters %>% 
  mutate(batted_balls = (AB-SO),
         hr_per_bb=HR/batted_balls)

# clean to remove any NaN from division by zero
hitters[hitters == "NaN"] <- 0

# filter for qualified hitters
qual_hitters <- hitters %>%
  filter(PA >= 350)

# MLB home runs per batted ball, by season
mlb_hr_per_100bb <- hitters %>%
  group_by(Season) %>%
  summarise(hr_per_100bb=sum(HR)/sum(batted_balls)*100)

# basic dot-and-line chart, to set the scene
ggplot(mlb_hr_per_100bb, aes(x=Season, y=hr_per_100bb)) +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  theme(panel.grid = element_blank()) +
  ylab("Home runs per 100 batted balls") + 
  scale_y_continuous(limits = c(0,5)) +
  scale_x_continuous(breaks = seq(1985, 2015, by=5)) +
  geom_rect(xmin = 1991, 
            xmax = 2004,
            ymin = 0, 
            ymax = 5, 
            fill="#f8f8f8",
            alpha = 0.1) +
  geom_rect(xmin = 2015, 
            xmax = 2017,
            ymin = 0, 
            ymax = 5, 
            fill="#eff3ff",
            alpha = 0.1) +
  geom_line(color="#08306b") +
  geom_point(color="#08306b") +
  annotate("text", 
           x = 1997.5, 
           y = 0.3, 
           label = "Steroid era",
           family = "Georgia",
           size = 6,
           color = "#808080",
           size = 5) +
  annotate("text", 
           x = 2016, 
           y = 0.3, 
           label = "?",
           family = "Georgia",
           size = 6,
           color = "#08306b",
           size = 5) +
  geom_hline(yintercept=seq(0, 5, by=1), color = "gray", size = 0.1) +
  geom_vline(xintercept=seq(1985,2015, by=5), color = "gray", size = 0.1)

# group qualified hitters into categories by numbe of HRs per year

breaks <- c(0,10,20,30,40,50,80)

qual_hitters <- qual_hitters %>%
  mutate(hr_bin = cut(HR, breaks, include.lowest = TRUE))

levels(qual_hitters$hr_bin) <- c("0-10","11-20","21-30","31-40","41-50","51+")

# Count players per bin for each season (here I've added some more code to add zeros where the count is zero)

hr_summary <- qual_hitters %>%
  group_by(Season, hr_bin) %>%
  summarize(count = n()) %>%
  filter(Season >= 1995)

# convert to wide format, adds NA when missing values
hr_summary<- hr_summary %>%
  spread(Season, count)

# replace NA by zero
hr_summary[is.na(hr_summary)] <- 0

# convert back to long format
hr_summary <- hr_summary %>%
  gather(Season,count,-hr_bin) %>%
  mutate(Season= as.integer(Season))

# Animated binned histogram (using geom_bar, rather than geom_histogram)

hr_animate <- ggplot(hr_summary, aes(x=hr_bin, y=count, fill = hr_bin, frame = Season)) +
  geom_bar(stat="identity", position="identity") +
  scale_fill_brewer(palette = "Blues", guide = FALSE) +
  theme_minimal(base_size = 20, base_family = "Georgia") +
  theme(panel.grid = element_blank()) +
  ylab("Number of players") +
  xlab("Home runs for players with 350+ plate appearances") +
  geom_hline(yintercept=seq(0, 100, by=10), color = "white", size = 0.5) +
  scale_y_continuous(breaks=seq(0, 120, by=20))

# save to graphs folder as GIF
gganimate(hr_animate, "graphs/hr_animation.gif", ani.width = 750, ani.height = 450, interval = 1)

# increase delay on the final frame of GIF
system("convert graphs/hr_animation.gif \\( +clone -set delay 300 \\) +swap +delete graphs/hr_animation.gif")


# dot-and-line chart by bin
ggplot(hr_summary, aes(x=Season, y=count)) +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  theme(panel.grid = element_blank()) +
  ylab("Number of players") + 
  scale_y_continuous(breaks = seq(0, 120, by=20)) +
  scale_x_continuous(breaks = seq(1985, 2015, by=5)) +
  geom_rect(xmin = 1991, 
            xmax = 2004,
            ymin = 0, 
            ymax = 120, 
            fill="#f8f8f8") +
  geom_rect(xmin = 2015, 
            xmax = 2017,
            ymin = 0, 
            ymax = 120, 
            fill = "#eff3ff") +
  geom_line(aes(group=hr_bin), color="gray", size=0.2) +
  geom_point(shape = 21, colour="black", size=2, stroke=0.2, aes(group=hr_bin, fill=hr_bin)) +
  scale_fill_brewer(palette = "Blues", name="Home runs") +
  annotate("text", 
           x = 1999, 
           y = 115, 
           label = "Steroid era",
           family = "Georgia",
           size = 6,
           color = "#808080",
           size = 5) +
  annotate("text", 
           x = 2016, 
           y = 115, 
           label = "?",
           family = "Georgia",
           size = 6,
           color = "#08306b",
           size = 5) +
  geom_hline(yintercept=seq(0, 100, by=10), color = "gray", size = 0.1) +
  geom_vline(xintercept=seq(1995,2015, by=5), color = "gray", size = 0.1)

# filtered version of that chart, to show increase for particular bins
ggplot(subset(hr_summary, hr_bin=="21-30"|hr_bin=="31-40"), aes(x=Season, y=count)) +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  theme(panel.grid = element_blank()) +
  ylab("Number of players") + 
  scale_y_continuous(breaks = seq(0, 120, by=20)) +
  scale_x_continuous(breaks = seq(1985, 2015, by=5)) +
  geom_rect(xmin = 1991, 
            xmax = 2004,
            ymin = 0, 
            ymax = 120, 
            fill="#f8f8f8") +
  geom_rect(xmin = 2015, 
            xmax = 2017,
            ymin = 0, 
            ymax = 120, 
            fill = "#eff3ff") +
  geom_line(aes(group=hr_bin), color="gray", size=0.2) +
  geom_point(shape = 21, colour="black", size=2, stroke=0.2, aes(group=hr_bin, fill=hr_bin)) +
  scale_fill_manual(values=c("#9ecae1","#6baed6"), name="Home runs") +
  annotate("text", 
           x = 1999, 
           y = 115, 
           label = "Steroid era",
           family = "Georgia",
           size = 6,
           color = "#808080",
           size = 5) +
  annotate("text", 
           x = 2016, 
           y = 115, 
           label = "?",
           family = "Georgia",
           size = 6,
           color = "#08306b",
           size = 5) +
  geom_hline(yintercept=seq(0, 100, by=10), color = "gray", size = 0.1) +
  geom_vline(xintercept=seq(1995,2015, by=5), color = "gray", size = 0.1)

# asfter both charts saved to temp_charts folder, this makes a GIF

system("convert -delay 10 temp_graphs/*.png -morph 10 graphs/bin_dot_line.gif")

# increase delay on final frame
system("convert graphs/bin_dot_line.gif \\( +clone -set delay 300 \\) +swap +delete graphs/bin_dot_line.gif")

# increase delay on first frame
system("convert graphs/bin_dot_line.gif \\( -clone 0  -set delay 300 \\) -swap 0,-1 +delete graphs/bin_dot_line.gif")


## messing with hr_tracker data

hr_trackerclean <- read_csv("hr_tracker_clean.csv")

## filter hr_tracker into seasons

type.by.season <- format(hr_trackerclean$date, "%Y")

hr_trackerclean <- hr_trackerclean %>%
  mutate(season=type.by.season)
  
ggplot(hr_trackerclean,aes(x=season,y=nrow(hr_trackerclean),fill=type_luck)) +
  geom_bar(stat="identity") 

#write new csv with "season" column

write_csv(hr_trackerclean, "hr_trackerclean.csv", na="")

############################################












# stacked area chart 

ggplot(hr_tracker, aes(x=Speed_off_Bat, y=Date)) +
  geom_dotplot()

#import data

hitters <- read_csv("hitters_updated.csv")
hr_tracker <- read_csv("hr_tracker.csv")
hr_summary <- read_csv("hr_summary.csv")
qual_hitters <- read_csv("qual_hitters.csv")
qual_hitters_2017 <- read_csv("qual_hitters_2017.csv")
  
  
    
# filter for qualified hitters
qual_hitters <- hitters %>%
  filter(PA >= 350)

## new categories in "hitters" = "batted_balls" and "hr_per_bb"

# batted_balls
hitters <- hitters %>% 
  mutate(batted_balls = (AB-SO))

# new object HR/Batted ball

#hr_per_bb
hitters <- hitters %>%
  mutate(hr_per_bb=HR/batted_balls)

# hr_per_bb <- hitters %>%
#   filter(batted_balls >= 1 & PA >=502 & HR>=1)

# clean to remove any NaN from division by zero
hitters[hitters == "NaN"] <- 0

# facted histograms, by year
ggplot(qual_hitters, aes(x=HR)) +
  geom_histogram() +
  facet_wrap(~Season)

# ridgeplot
ggplot(qual_hitters, aes(x=HR, y=Season, group=Season)) +
  geom_density_ridges(scale = 10, size = 0.1, rel_min_height = 0, fill = "red", alpha = 0.5) +
  scale_x_continuous(limits=c(10, 40)) +
  scale_y_reverse()


  
# Look at same charts as before with hr_per_bb instead

## facted histograms, by year
ggplot(hr_per_bb, aes(y=hr_per_bb) + 
         geom_point()
  
## 11-2
## make an area chart with HR for qualified hitters? (Bins: 10-15, 15-20, 20-25, 30-35, 35-40, 40-45, 45-50+)

breaks <- c(0,10,20,30,40,30,80)
  
qual_hitters <- qual_hitters %>%
  mutate(hr_bin = cut(HR, breaks, include.lowest = TRUE))

levels(qual_hitters$hr_bin) <- c("0-9","10-19","20-29","30-39","40-49","51+")

hr_summary <- qual_hitters %>%
  group_by(Season, hr_bin) %>%
  summarize(count = n()) %>%
  filter(Season >= 1995)

ggplot(hr_summary, aes(x=Season, fill=hr_bin)) +
  geom_histogram()

#screwing with histo

ggplot(hr_summary, aes(x=Season, y=count, color=hr_bin)) +
  geom_histogram()

#line chart with HR summary 

ggplot(hr_summary, aes(x=Season,y=count, color=hr_bin)) +
  geom_point(size=14) +
  geom_line(size=8) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 45, base_family = "Georgia") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017)) 

# ridgeplot with bins

ggplot(qual_hitters, aes(x=Season, y=hr_bin, group=hr_bin, fill = hr_bin)) +
  geom_density_ridges(scale = 10, size = 0.1, rel_min_height = 0, alpha = 0.5) +
  scale_x_continuous(limits=c(1995,2017)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 60, base_family = "Georgia") 


#messing around with ridgeplot
#ggplot(hr_summary, aes(x=count, y=Season, group=hr_bin, fill = hr_bin)) +
  #geom_density_ridges(scale = 10, size = 0.1, rel_min_height = 0, alpha = 0.5) +
  #scale_x_continuous(limits=c(1995,2017))

ggplot(qual_hitters, aes(x=hr_bin)) + geom_histogram(stat="count")

hr_animate <- ggplot(qual_hitters, aes(x=HR, fill=hr_bin, frame = Season)) +
  geom_histogram(binwidth=5)

gganimate(hr_animate)

qual_hitters_2017 <- qual_hitters %>%
  filter(Season == 2017)

# making the lines striped by year [not working :( ]

#ggplot(hr_summary, aes(y=Season, fill=hr_bin)) +
  #geom_histogram(binwidth=10,color="#888888", alpha = 0.75) +
  #scale_color_brewer(palette = "Set1", name = "") 

#ggplot(hr_summary, aes(x = Season, fill = count)) + 
  #geom_bar(stat = "count", color = "white", alpha = 0.7, aes(fill=hr_bin)) +
  #scale_color_brewer(palette = "Set1", name = "") +
    

# styled histogram with bin data from 2017

ggplot(qual_hitters_2017, aes(x=HR, fill=hr_bin)) +
  geom_histogram(binwidth=1, color="#888888", alpha = 0.75) +
  scale_color_brewer(palette = "Set1", name = "") 
  
#Histos from all of the seasons?
  
# Step 1: create data frame with qual hitters from just 1995-2017

qual_hitters <- qual_hitters %>%
  filter(Season >= 1995)

# Step 2: Histograms from every year since '95 (facet_wrap histos)

# histogram code 10/17 from, before data was put in bins: 
#ggplot(qual_hitters, aes(x=HR)) +
#geom_histogram() +
#facet_wrap(~Season)

ggplot(qual_hitters, aes(x=HR, fill=hr_bin)) +
  geom_histogram(binwidth=1, color="#888888", alpha = 0.75) +
  scale_color_brewer(palette = "Set1", name = "") +
  facet_wrap(~Season) +
  theme_minimal(base_size = 100, base_family = "Georgia") +
  ylab("Players in each home run bin") +
  xlab("Home Run totals of players with 350+ plate appearances") +
  ggtitle("Players in each home run category (1995-2017)")

# write csvs for qual_hitters, hitters, hr_summary

write_csv(hitters, "hitters_updated.csv", na="")

write_csv(hr_summary, "hr_summary.csv", na="")

write_csv(qual_hitters, "qual_hitters.csv", na="")

write_csv(qual_hitters_2017, "qual_hitters_2017.csv", na="")







