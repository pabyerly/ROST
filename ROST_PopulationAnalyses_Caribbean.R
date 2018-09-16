library(ggplot2)
library(forecast)
library(dplyr)
library(tidyr)
library(colortools)
library(readr)
library(broom)
library(ggthemes)
library(maps)
library(rgbif)
library(CoordinateCleaner)
library(ggrepel)
library(png)
library(gridExtra)
library(data.table)

rost=read.csv("ROST_colonycount_2.csv")
#format data by island ("site") by year 
rost=gather(rost, 'Year', "Count", 5:33)%>% 
  filter(is.na(Count) == FALSE)%>%
  group_by(Year) %>%
  summarise(yearly=)
  rost$Year=parse_number(rost$Year) #change year from column format to row
  rost$Count=as.numeric(rost$Count) #change count from character to numeric
  
#filter to exclude SWPR. For now, omit Culebra as part of VI subpopulation 
VI=filter(rost, Region %in% c("USVI", "BVI"))
SWP=filter(rost, Region=="SWPR")
USVI=filter(rost, Region=="USVI")
BVI=filter(rost, Region=="BVI")

#counts by year/region
VI_yearly=group_by(VI, Year)
  summarize(VI_yearly, sum(Count))
  
#for whole Virgin Islands 
rost_vi=VI%>%
  do(mod=lm(Count~Year, data=.))%>%
  tidy(mod)
View(rost_vi)

#make intercepts and slopes columns and not rows
rost_vi=rost_vi %>%
  dplyr::select(Region, term, estimate) %>%
  spread(term, estimate) %>%
  ungroup()

#theme from Our Coding Club 
theme_marine <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype="blank"))
}
#plot abundance through time with a linear model fit of pop change for each populations
#Returns point for each cay--need to figure out how to summarize 
print(rost_vi$Year[rost_vi$Region=="VI"])
(BVIMAP=ggplot(VI, aes(x=Year, y=Count, shape=as.factor(Region))) +
    geom_point( fill="#76EEC6", size=4)+
    scale_shape_manual(values=c(21, 23, 24)) +
    geom_smooth(method = "lm", colour = "#76EEC6", fill = "#76EEC6", alpha = 0.4) +
    labs(x = "", y = "Individuals\n", title = "VI\n") +
    theme_marine())+
    guides(shape=FALSE)

(popcounts <- ggplot(VI, aes (x=Year, y=Count, colour=Region)) +
    geom_point(size=2) +                                                # Changing point size
    geom_smooth(method=lm, aes(fill=Region)) +                    # Adding a linear model fit and colour-coding by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels=c("BVI", "USVI")) +                 # Adding labels for the legend
    ylab("Estimated Number Breeding Pairs\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),       # making the years at a bit of an angle
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm")) +                    # Adding a 1cm margin around the plot
    theme(legend.text = element_text(size=12, face="italic"),                  # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position=c(0.9, 0.9)))                  # Setting the position for the legend - 0 is left/bottom, 1 is top/right
