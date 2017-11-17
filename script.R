library(dplyr)
library(ggplot2)
library(readr)
library(ggridges)
library(gganimate)
library(tidyr) # this has function separate to separate e.g. first and last name
library(forcats)

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
write_csv(hr_tracker, "hr_tracker.csv", na="")

# stacked area chart 

ggplot(hr_tracker, aes(x=Speed_off_Bat, y=Date)) +
  geom_dotplot()
  
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







