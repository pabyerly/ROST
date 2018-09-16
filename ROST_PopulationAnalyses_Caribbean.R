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

rost=read.csv("ROST_colonycount.csv")

#format data by island ("site") by year 
rost=gather(rost, 'Year', "Count", 5:33) %>%
  filter(is.na(Count) == FALSE)%>%
           
rost$Year=parse_number(rost$Year) #change year from column format to row
rost$Count=as.numeric(rost$Count) #change count from character to numeric
 
#filter to exclude SWPR. For now, omit Culebra as part of VI subpopulation 
VI=filter(rost, Region %in% c("USVI", "BVI"))
PR=filter(rost, Region %in% c("SWPR", "Culebra"))
USVI=filter(rost, Region=="USVI")
BVI=filter(rost, Region=="BVI")

#summarize counts by year/region for plotting purposes 
VI_yearly=group_by(VI, Year) %>%
  VI_yearly=summarize(VI_yearly, Sum=sum(Count))
  plot(VI_yearly$Sum~VI_yearly$Year) #check
  
  # model population growth rate (for whole PRB) 
  PRB_growth=rost%>%
    group_by(Region) %>% #include if want to fit one model per population
    do(mod=lm(Count~Year, data=.))%>%
    tidy(mod)
  View(PRB_growth)
  
 #make intercepts and slopes columns and not rows
 PRB_growth=PRB_growth %>%
    dplyr::select(Region, term, estimate) %>%
    spread(term, estimate) %>%
    ungroup()
  
# model population growth rate (for just VI) 
VI_growth=VI%>%
  #group_by(Region) %>% #include if want to fit one model per population
  do(mod=lm(Count~Year, data=.))%>%
  tidy(mod)
  View(VI_growth)
#make intercepts and slopes columns and not rows
 VI_growth=VI_growth %>%
    dplyr::select(term, estimate) %>%
    spread(term, estimate) %>%
    ungroup()

#theme
theme_rosy <- function(){
  theme_bw() +
    theme(axis.text.x = element_text (size=20, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=20),
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

#plot abundance through time with a linear model fit of pop change for each populations (if specified group by region above)
#Returns point for each cay--need to figure out how to remove "0"
#print(VI_growth$Year[VI_growth$Region=="VI"])
VI=filter(VI, Count>"0")
print(VI_growth$Year)
(MAP=ggplot(VI, aes(x=Year, y=Count)) +
    geom_point( fill="#76EEC6", size=4)+
    geom_smooth(method = "lm", colour = "##0000EE", fill = "##0000EE", alpha = 0.4) +
    labs(x = "", y = "Estimated NUmber of Breeding Pairs\n", title = "VI\n") +
    theme_rosy())

#SCatter plot figure w 95% confidence intervals on abundance data from Coding Club
(popcounts <- ggplot(VI_yearly, aes (x=Year, y=Sum)) +
    geom_point(size=5, colour="#3A5FCD", fill = "#3A5FCD") +                                                # Changing point size
    geom_smooth(method = "lm", colour = "#009ACD", fill = "#009ACD", alpha = 0.4) +                    # Adding a linear model fit and colour-coding by country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels=c("BVI", "USVI")) +                 # Adding labels for the legend
    ylab("Estimated Number Breeding Pairs\n") +                             
    xlab("")  +
    theme_rosy()) 
