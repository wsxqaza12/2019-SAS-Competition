library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(maps)
library(readxl)
library(ggrepel)
library(janitor)
library(tidyr)
library(ggthemes)
library(geosphere)
library(fmsb)
library(forcats)
library(hrbrthemes)
library(viridis)

setwd("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/")

flt_merge <- read.csv("flt_1.csv")

time <- factor(unique(flt_merge$Dep_Tm), levels= c("00:00~05:59", "06:00~11:59",
                                                   "12:00~17:59", "18:00~23:59"))

flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[1]] <- "midnight"
flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[2]] <- "morning"
flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[3]] <- "afternoon"
flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[4]] <- "night"

flt_merge$Dep_absTime <- factor(flt_merge$Dep_absTime,
                                levels= c("morning", "afternoon", "night", "midnight"))

flt_merge$Route <- factor(flt_merge$Route, levels = c("TSA-YNZ", "KHH-YNZ", "KHH-NGO", "TPE-NGO",
                                                      "TPE-YNZ", "TPE-PNH", "TPE-LOP", "TPE-SJC",
                                                      "TPE-MXP", "TPE-IAH"))

# DA for Dep Time

Dep_absTime <- flt_merge %>% group_by(Dep_absTime) %>% summarise(mean= mean(Fuel_Cost_Index), sd= sd(Fuel_Cost_Index),
                                                                 min = min(Fuel_Cost_Index), "25%" = quantile(Fuel_Cost_Index, 0.25),
                                                                 "50%" = quantile(Fuel_Cost_Index, 0.5), "75%" = quantile(Fuel_Cost_Index, 0.75),
                                                                 max= max(Fuel_Cost_Index))

summary(aov(Fuel_Cost_Index~Dep_absTime, flt_merge))

# bar for Dep time
flt_merge$Dep_absTime <- factor(flt_merge$Dep_absTime,
                                levels= c("morning", "afternoon", "night", "midnight"))

ggplot(data = flt_merge, aes(x= Dep_absTime)) +
  geom_bar(fill= c("#F6EED3", "#E8BA1F", "#547C87","#0C3540"), color="black", lwd= 1.5) +
  scale_x_discrete("Dep Time")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))
  # 539CB8

sum(flt_merge$Dep_absTime== "morning")
sum(flt_merge$Dep_absTime== "afternoon")
sum(flt_merge$Dep_absTime== "night")
sum(flt_merge$Dep_absTime== "midnight")

####################################
##########  Dep time & Y  ########## 
####################################
# boxplot
ggplot(flt_merge %>% group_by(Dep_absTime) %>%
         mutate(medMPG = as.numeric(Dep_absTime))
       , aes(x = Dep_absTime, y = Fuel_Cost_Index)) +
  geom_boxplot(aes(fill=medMPG), lwd=1.2, show.legend = FALSE) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("Dep. Time") +
  scale_y_continuous("Fuel Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))+
  scale_fill_gradientn(colours = c("#F6EED3", "#E8BA1F", "#547C87","#383A3C"))

# Horizontal violin
flt_merge %>%
  ggplot(aes(x=Dep_absTime, y=Fuel_Cost_Index, fill=Dep_absTime, color=Dep_absTime)) +
  geom_violin(width=.7, size=0.2) +
  scale_fill_manual(values= c("#F6EED3", "#E8BA1F", "#547C87","#383A3C")) +
  scale_color_manual(values= c("#F6EED3", "#E8BA1F", "#547C87","#383A3C")) +
  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  theme_ipsum() +
  theme(legend.position="none") 

# Dep Time & Block_Tm & Y
ggplot(flt_merge %>% group_by(Dep_absTime) %>%
         mutate(medMPG = as.numeric(Dep_absTime))
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Dep_absTime), size=6 , alpha= I(5/10), shape= 16, stroke = 0) +
  scale_color_manual(values= c("#F6EED3", "#E8BA1F", "#547C87","#383A3C"),
                     name="Dep. Time")+
  scale_x_continuous("Block Tm")+
  scale_y_continuous("Fuel Cost")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

######### Dep Time & Route & Y #########
# Dep Time & Route
Deptime_Route_precent <- 
  flt_merge %>% 
  mutate(id = 1) %>%
  group_by(Route, Dep_absTime) %>%
  summarise(SUM = sum(id)) %>%
  mutate(Type = as.numeric(Dep_absTime))
  ungroup() %>%
  mutate(Route = fct_reorder(Route, Type))

Deptime_Route20_precent <- 
  flt_merge %>% 
  mutate(id = 1) %>%
  group_by(Route20, Dep_absTime, Route) %>%
  summarise(SUM = sum(id)) %>%
  mutate(Type = as.numeric(Route)) %>%
  ungroup() %>%
  mutate(Route20 = fct_reorder(Route20, Type))

  # Route Route_Y 10
ggplot(Deptime_Route_precent
       , aes(fill=Dep_absTime, x=Route, y=SUM))+
  geom_bar(stat="identity", position="fill", width=.8) +
  scale_fill_manual(values= c("#FC918B", "#E9DD6F", "#547C87","#383A3C"),
                    name="Dep. Time")+
  # scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

  # Route Route_Y 20
ggplot(Deptime_Route20_precent
       , aes(fill=Dep_absTime, x=Route20, y=SUM))+
  geom_bar(stat="identity", position="fill", width=.8) +
  scale_fill_manual(values= c("#FC918B", "#E9DD6F", "#547C87","#383A3C"),
                    name="Dep. Time")+
  # scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

############################################
######### jitter ###########################
############################################
  # Route Route_Y 10
ggplot(flt_merge, aes(x = Route, y= Fuel_Cost_Index)) +
  geom_jitter(aes(color= Dep_absTime),
              size=5 , alpha= I(8/10), shape= 16, stroke = 0, width = 0.2, height = 0) +
  scale_color_manual(values= c("#FC918B", "#E9DD6F", "#547C87","#383A3C"),
  # scale_color_manual(values= c("#F6EED3", "#E8BA1F", "#547C87","#383A3C"),
                     name="Dep. Time")+
  scale_x_discrete("Route")+
  scale_y_continuous("Fuel Cost")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

  # Route Route_Y 20
ggplot(flt_merge %>% mutate(Type = as.numeric(Route)) %>%
         mutate(Route20 = fct_reorder(Route20, Type))
       , aes(x = Route20, y= Fuel_Cost_Index)) +
  geom_jitter(aes(color= Dep_absTime),
              size=3 , alpha= I(8/10), shape= 16, stroke = 0, width = 0.4, height = 0) +
  scale_color_manual(values= c("#FC918B", "#E9DD6F", "#547C87","#383A3C"),
  # scale_color_manual(values= c("#F6EED3", "#E8BA1F", "#547C87","#383A3C"),
                     name="Dep. Time")+
  scale_x_discrete("")+
  scale_y_continuous("", limits = c(3, 100))+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 10), 
        axis.line=element_line(color="gray",size=1))




