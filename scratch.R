
# box and whisker plot?

library(dplyr)
library(readr)
library(ggplot2)


#data import

qual_hitters <- read_csv("qual_hitters.csv")

qual_hitters2017 <- read_csv("qual_hitters_2017.csv")

## Stripchart for all data

stripchart(qual_hitters$HR, method = "jitter") %>%
  
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
  

ggplot(qual_hitters, aes(x=Season, y=HR, group = hr_bin)) +
  geom_bar(stat = "identity", fill = "#CCCCCC", alpha = 0.5, aes(color = hr_bin)) 
  
since95_violin <- ggplot(qual_hitters, aes(x=Season, y=HR, group=Season)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF") +
  scale_x_continuous(breaks = c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015, 2016, 2017)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75), limits = c(0,75)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 12, base_family = "Helvetica") +
  coord_flip()
  
print(since95_violin)

## violin from 2010-2017

qual_hitters1017 <- qual_hitters %>%
  filter(Season>=2010)

ggplot(qual_hitters1017, aes(x=Season, y=HR, group=Season)) +
  geom_violin(fill = "#CCD6F5", color = "#3366FF") +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60), limits = c(0,60)) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_size = 12, base_family = "Helvetica") 

#jitter

ggplot(qual_hitters1017,aes(x=Season, y=HR, group=Season, fill=hr_bin)) +
  geom_jitter() 
  
  

  
  




