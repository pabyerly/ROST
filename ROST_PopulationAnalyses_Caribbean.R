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
library(ggrepel)
library(png)
library(gridExtra)


rost=read.csv("ROST_colonycount.csv")

#format data by island ("site") by year in long form; don't omit NAs so that non-survey years are excluded  
rost=gather(rost, 'Year', "Count", 5:33) #%>%
  #filter(is.na(Count) == FALSE)
           
rost$Year=parse_number(rost$Year) #change year from column format to row
rost$Count=as.numeric(rost$Count) #change count from character to numeric
 
#filter to exclude SWPR. For now, omit Culebra as part of VI subpopulation 
VI=filter(rost, Region %in% c("USVI", "BVI"))
PR=filter(rost, Region %in% c("SWPR", "Culebra"))
USVI=filter(rost, Region=="USVI")
BVI=filter(rost, Region=="BVI")

#summarize counts by year/region for plotting purposes 
VI_yearly=group_by(VI, Year) 
  VI_yearly=summarize(VI_yearly, Sum=sum(Count))
  plot(VI_yearly$Sum~VI_yearly$Year) #check
  
 #summarize counts by year/region for plotting purposes 
  USVI_yearly=group_by(USVI, Year) 
  USVI_yearly=summarize(USVI_yearly, Sum=sum(Count))
  plot(USVI_yearly$Sum~USVI_yearly$Year) #check
  
  #summarize counts by year/region for plotting purposes 
  BVI_yearly=group_by(BVI, Year) 
  BVI_yearly=summarize(BVI_yearly, Sum=sum(Count))
  plot(BVI_yearly$Sum~BVI_yearly$Year) #check
  
  
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
          plot.title = element_text(size = 20, hjust=0.5))
}

#plot abundance through time with a linear model fit of pop change for each populations (if specified group by region above)
#Returns point for each cay--need to figure out how to remove "0"
#print(VI_growth$Year[VI_growth$Region=="VI"])
VI=filter(VI, Count>"0")
print(VI_growth$Year)
(MAP=ggplot(VI, aes(x=Year, y=Count)) +
    geom_point( fill="#76EEC6", size=4)+
    geom_smooth(method = "lm", colour = "#0000EE", fill = "#0000EE", alpha = 0.4) +
    labs(x = "", y = "Estimated Number of Breeding Pairs\n", title = "VI\n") +
    theme_rosy())

#SCatter plot figure w 95% confidence intervals on abundance data from Coding Club
(popcounts <- ggplot(VI_yearly, aes (x=Year, y=Sum)) +
    
    geom_point(size=5, colour="#3A5FCD", fill = "#3A5FCD") +                                                # Changing point size
    geom_smooth(method = "lm", colour = "#009ACD", fill = "#009ACD", alpha = 0.4) +                    # Adding a linear model fit and colour-coding by country
    theme_bw() +
    ylab("Nest Counts\n") +
    ggtitle("Roseate Tern Breeding Pairs in the Virgin Islands, 1993-2018") +
    scale_y_continuous(limits=c(0, 3000), breaks=seq(0, 3000, 500)) +
    scale_x_continuous(name="", limits=c(1993, 2018), breaks=seq(1993, 2018, 5))  +
    annotate("text", x=2013, y=2000, label="yearly growth rate = -1.81", size=6)+
    theme_rosy()) 

#SCatter plot figure w 95% confidence intervals on abundance data from USVI
(popcountUSVI <- ggplot(USVI_yearly, aes (x=Year, y=Sum)) +
    geom_point(size=5, colour="#3A5FCD", fill = "#3A5FCD") +                                                # Changing point size
    geom_smooth(method = "lm", colour = "#009ACD", fill = "#009ACD", alpha = 0.4) +                    # Adding a linear model fit and colour-coding by countr
    ylab("Estimated Number Breeding Pairs\n") + 
    title("Roseate Tern Breeding Pairs in the Virgin Islands, 1993-2018") +
    scale_y_continuous(limits=c(0, 3000), breaks=seq(0, 3000, 500)) +
    scale_x_continuous(name="", limits=c(1988, 2018), breaks=seq(1988, 2018, 5))  +
    annotate("text", x=2005, y=2500, label="yearly growth rate = -1.03", size=6)+
    theme_rosy())

#SCatter plot figure w 95% confidence intervals on abundance data from BVI
(popcountBVI <- ggplot(BVI_yearly, aes (x=Year, y=Sum)) +
    geom_point(size=5, colour="#3A5FCD", fill = "#3A5FCD") +                                                # Changing point size
    geom_smooth(method = "lm", colour = "#009ACD", fill = "#009ACD", alpha = 0.4) +                    # Adding a linear model fit and colour-coding by country
    theme_bw() +
    ylab("Estimated Number Breeding Pairs\n") + 
    title("Roseate Tern Breeding Pairs in the Virgin Islands, 1993-2018") +
    scale_y_continuous(limits=c(0, 3000), breaks=seq(0, 3000, 500)) +
    scale_x_continuous(name="", limits=c(1993, 2018), breaks=seq(1993, 2018, 5))  +
    annotate("text", x=2005, y=2500, label="yearly growth rate = -4.22", size=6)+
    theme_rosy())

#Grid panel of USV and BVI pop abundance, separated 
panel=grid.arrange(popcountUSVI + ggtitle("USVI") + ylab ("Nest Counts"),  popcountBVI +ggtitle("BVI") + ylab("") , ncol=2)