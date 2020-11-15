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
flt_merge <- read.csv("flt_merge_BorF_1_W.csv")

# flt_schedule_1 <- read.csv("flt_schedule_1.csv")
# flt_data_1 <- read.csv("flt_data_1.csv")
# 
# flt_merge <- flt_data_1 %>%
#   left_join(flt_schedule_1, by= "Flt_Id")

# DA for week

Dep_Day_Of_Wk <- flt_merge %>% group_by(Dep_Day_Of_Wk) %>% summarise(mean= mean(Fuel_Cost_Index), sd= sd(Fuel_Cost_Index),
                                                                 min = min(Fuel_Cost_Index), "25%" = quantile(Fuel_Cost_Index, 0.25),
                                                                 "50%" = quantile(Fuel_Cost_Index, 0.5), "75%" = quantile(Fuel_Cost_Index, 0.75),
                                                                 max= max(Fuel_Cost_Index))

summary(aov(Fuel_Cost_Index~Dep_absTime, flt_merge))

# bar for Dep time
flt_merge$Dep_Day_Of_Wk <- factor(flt_merge$Dep_Day_Of_Wk,
                                levels= c("Monday", "Tuesday", "Wednesday", "Thursday",
                                          "Friday", "Saturday", "Sunday"))

ggplot(data = flt_merge, aes(x= Dep_Day_Of_Wk, fill= Dep_Day_Of_Wk)) +
  geom_bar(color="black", lwd= 1.5) +
  scale_x_discrete("Dep Time")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  scale_fill_brewer(palette="Set3")

# 539CB8

sum(flt_merge$Dep_Day_Of_Wk== "Monday")
sum(flt_merge$Dep_Day_Of_Wk== "Tuesday")
sum(flt_merge$Dep_Day_Of_Wk== "Wednesday")
sum(flt_merge$Dep_Day_Of_Wk== "Thursday")
sum(flt_merge$Dep_Day_Of_Wk== "Friday")
sum(flt_merge$Dep_Day_Of_Wk== "Saturday")
sum(flt_merge$Dep_Day_Of_Wk== "Sunday")

####################################
##########  Dep time & Y  ########## 
####################################
# Horizontal violin
flt_merge %>%
  ggplot(aes(x=Dep_Day_Of_Wk, y=Fuel_Cost_Index, fill=Dep_Day_Of_Wk, color=Dep_Day_Of_Wk)) +
  geom_violin(width=.7, size=0.2) +
  scale_fill_brewer(palette="Set3") +
  scale_color_brewer(palette="Set3") +
  # coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  theme_ipsum() +
  theme(legend.position="none") 

# Dep Time & No_Pushback & Y
ggplot(flt_merge %>% group_by(Dep_Day_Of_Wk) %>%
         mutate(medMPG = as.numeric(Dep_Day_Of_Wk))
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Dep_Day_Of_Wk), size=6 , alpha= I(7/10), shape= 16, stroke = 0) +
  scale_color_brewer(palette="Set3", name= "Week") +
  scale_x_continuous("Block Tm")+
  scale_y_continuous("Fuel Cost")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

######### Week Time & Route & Y #########
# Week Time & Route
Week_Route_precent <- 
  flt_merge %>% 
  mutate(id = 1) %>%
  group_by(Route, Dep_Day_Of_Wk) %>%
  summarise(SUM = sum(id)) %>%
  mutate(Type = as.numeric(Dep_Day_Of_Wk)) %>%
  ungroup() %>%
  mutate(Route = fct_reorder(Route, Type))


ggplot(Week_Route_precent
       , aes(fill=Dep_Day_Of_Wk, x=Route, y=SUM))+
  geom_bar(stat="identity", position="fill", width=.8) +
  scale_fill_brewer(palette = "Set3", name="Week")+
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# jitter
# Route Route_Y
ggplot(flt_merge, aes(x = Route, y= Fuel_Cost_Index)) +
  geom_jitter(aes(color= Dep_Day_Of_Wk),
              size=5 , alpha= I(8/10), shape= 16, stroke = 0) +
  scale_color_brewer(palette = "Set3", name="Week")+
  scale_x_discrete("")+
  scale_y_continuous("Fuel Cost")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))




