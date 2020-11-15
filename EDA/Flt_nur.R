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

setwd("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/")
flt_merge <- read.csv("flt_2.csv")

T <- flt_merge %>% filter(Route== "TPE-SJC")
# A　<-  read.csv("flt_merge_Direction_2_W.csv") %>% select(Flt_Id, Direction) %>%
#   right_join(flt_merge, by = "Flt_Id")
# 
# write.csv(A, "flt_2.csv", row.names = F)

# add <- read.csv("flt_All2 (1).csv")
# flt_merge <- 
#   flt_merge %>% 
#   left_join(add %>% select(Flt_Id, pax_weight, baggage_weight, total_weight), by = "Flt_Id")
# 
# write.csv(flt_merge, "C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/flt_2.csv", row.names = F)
  
flt_merge$BorF <- factor(flt_merge$BorF, labels = c("Come", "Go"), levels= c("0", "1"))
flt_merge$BorF <- factor(flt_merge$BorF, labels = c("East", "West"), levels= c("1", "0"))

########################################################
# devtools::install_github('cttobin/ggthemr')
# install.packages("backports")
library(ggthemr)
ggthemr_reset()
dust_theme <- ggthemr('chalk', set_theme = FALSE, type = 'outer')
dust_theme <- ggthemr('flat', set_theme = FALSE, type = 'outer')
dust_theme <- ggthemr('dust', set_theme = FALSE, type = 'outer')
example_plot
########################################################

# TPA-IAH
m <- flt_merge %>% filter(Route== "TPE-IAH")
max(m$Block_Tm)
min(m$Block_Tm)

ggplot(flt_merge %>% filter(Route== "TPE-IAH")
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= BorF), size=6 , shape= 16, stroke = 0) +
  scale_color_manual(values= c("#CC9966","#666666"), name="")+
  scale_x_continuous("Block Tm", limits = c(839, 1072))+
  scale_y_continuous("Fuel Cost", limits = c(79, 100))+
  dust_theme$theme+
  # theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


T <- flt_merge %>% filter(Route== "KHH-YNZ") %>% filter(Flt_Nbr== "3694")

T %>% 
  # filter(BorF== "Come") %>% 
  mutate(STnEn= Taxi_Out + Taxi_In,
         total_weight= total_weight/1000) %>%
  ggplot(aes(x=Block_Tm, y=Fuel_Cost_Index, size = total_weight, color=Direction)) +
    geom_point(alpha=0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))+
    scale_y_continuous() +
    scale_size(range = c(.1, 30), name="")
    scale_color_viridis(discrete=TRUE)

length(unique(flt_merge$Flt_Nbr))

D <- T %>% filter(BorF== "Come") %>% filter(Flt_Nbr== "3690")



# Route Route_Y 20
flt_merge$Route <- factor(flt_merge$Route, levels = c("TSA-YNZ", "KHH-YNZ", "KHH-NGO", "TPE-NGO",
                                                      "TPE-YNZ", "TPE-PNH", "TPE-LOP", "TPE-SJC",
                                                      "TPE-MXP", "TPE-IAH"))

ggplot(flt_merge %>% mutate(Type = as.numeric(Route)) %>%
         mutate(Route20 = fct_reorder(Route20, Type))
       , aes(x = Route, y= Fuel_Cost_Index)) +
  geom_jitter(aes(color= Direction),
              size=4, alpha= I(8/10), shape= 16, stroke = 0, width = 0.2, height = 0) +
  scale_color_manual(values= c("#CC9966","#666666"),
                     name="Dep. Time")+
  scale_x_discrete("")+
  scale_y_continuous("Fuel Cost", limits = c(3, 100))+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))



########################################################
########################################################
########################################################
T <- flt_merge %>% filter(Route== "TPE-PNH")

T %>% 
  # filter(BorF== "Come") %>% 
  mutate(STnEn= Taxi_Out + Taxi_In,
         total_weight= total_weight/1000) %>%
  ggplot(aes(x=Block_Tm, y=Fuel_Cost_Index, color=Direction)) +
  geom_point(alpha=0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))+
  scale_y_continuous() +
  scale_size(range = c(.1, 30), name="")
scale_color_viridis(discrete=TRUE)


ggplot(flt_merge %>% 
         filter(Route== "TPE-SJC") %>% 
         # filter(Direction== "East")
         filter(Direction== "West")
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= as.factor(Flt_Nbr)), size=8 , shape= 16, stroke = 0) +
  scale_color_brewer(palette="Set3")+
  scale_x_continuous("Block Tm")+
  scale_y_continuous("Fuel Cost")+
  # dust_theme$theme+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


########################################################
########################################################
########################################################

NUM <- flt_merge %>%
  mutate(cou = 1) %>%
  group_by(Route20, Flt_Nbr) %>%
  summarise(MEAN = mean(total_weight), sum= sum(cou),
            fuel= mean(Fuel_Cost_Index), time= mean(Block_Tm),
            out= mean(Taxi_Out), In= mean(Taxi_In))

NUM1 <- flt_merge %>%
  group_by(Route, Flt_Nbr, Direction) %>%
  summarise(MEAN = mean(total_weight),
            fuel= mean(Fuel_Cost_Index), time= mean(Block_Tm),
            out= mean(Taxi_Out), In= mean(Taxi_In)) %>% 
  mutate(cou = 1)

HowmanyNUMS <- flt_merge %>%
  group_by(Route20, Flt_Nbr) %>%
  summarise(MEAN = mean(total_weight)) %>%
  ungroup() %>%
  group_by(Route20) %>%
  mutate(cou = 1) %>%
  summarise(N= sum(cou))

length(unique(flt_merge$Flt_Nbr))


NUM1$Route <- factor(NUM1$Route, levels = c("TSA-YNZ", "KHH-YNZ", "KHH-NGO", "TPE-NGO",
                                                      "TPE-YNZ", "TPE-PNH", "TPE-LOP", "TPE-SJC",
                                                      "TPE-MXP", "TPE-IAH"))

ggplot(NUM1, aes(x=Route, fill=Direction))+
  geom_bar(position ="stack", width=.8)+
  scale_fill_manual(values= c("#CC9966","#666666"), name="Acft Type")+
  # scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))+
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
            color = I("white"), size = 4)

