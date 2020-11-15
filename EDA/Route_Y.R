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

Flightroute <- read_xlsx("飛機地點代碼.xlsx", sheet= 1)

########### MAP 用的 ########### 
Count_route_flight <- flt_merge %>% 
  select(Flt_Id, Dep_Apt_Cd, Arr_Apt_Cd, Acft_Typ_Cd) %>%
  mutate(cou= 1) %>%
  group_by(Dep_Apt_Cd, Arr_Apt_Cd) %>%
  summarise(No= sum(cou))

curve_connect <- 
  Count_route_flight[Count_route_flight$Dep_Apt_Cd == "TPE" | 
                       Count_route_flight$Dep_Apt_Cd == "TSA" | 
                       Count_route_flight$Dep_Apt_Cd == "KHH", ] %>%
  merge(Flightroute %>% select(代碼, lat, long), by.x = "Dep_Apt_Cd", by.y = "代碼")

names(curve_connect)[4:5] <- c("Dlat", "Dlong")

curve_connect <- 
  curve_connect %>% merge(Flightroute %>% select(代碼, lat, long), by.x = "Arr_Apt_Cd", by.y = "代碼") 

names(curve_connect)[6:7] <- c("Alat", "Along")
########### MAP 用的 ########### 

############################
########### 整理 ########### 
############################

# 10個航線分組
flt_merge$Route <- "A"
for (i in 1:10) {

  flt_merge$Route[flt_merge$Dep_Apt_Cd== curve_connect$Dep_Apt_Cd[i] & 
                   flt_merge$Arr_Apt_Cd== curve_connect$Arr_Apt_Cd[i]] <- 
    paste(curve_connect$Dep_Apt_Cd[i], "-",curve_connect$Arr_Apt_Cd[i], sep="")

  flt_merge$Route[flt_merge$Dep_Apt_Cd== curve_connect$Arr_Apt_Cd[i] & 
                  flt_merge$Arr_Apt_Cd== curve_connect$Dep_Apt_Cd[i]] <- 
    paste(curve_connect$Dep_Apt_Cd[i], "-",curve_connect$Arr_Apt_Cd[i], sep="")
  
}

flt_merge$Route <- as.factor(flt_merge$Route)

# 分20組
flt_merge$Route20 <- "A"
for (i in 1:20) {
  flt_merge$Route20[flt_merge$Dep_Apt_Cd== Count_route_flight$Dep_Apt_Cd[i] &
                      flt_merge$Arr_Apt_Cd== Count_route_flight$Arr_Apt_Cd[i]] <-
    paste(Count_route_flight$Dep_Apt_Cd[i], "-",Count_route_flight$Arr_Apt_Cd[i], sep="")
}

flt_merge$Route <- factor(flt_merge$Route, levels = c("TSA-YNZ", "KHH-YNZ", "KHH-NGO", "TPE-NGO",
                                                      "TPE-YNZ", "TPE-PNH", "TPE-LOP", "TPE-SJC",
                                                      "TPE-MXP", "TPE-IAH"))


# 往返的code 1從台出發 0回來
flt_merge$BorF <- 0
flt_merge$BorF[flt_merge$Dep_Apt_Cd== "TPE"| 
                 flt_merge$Dep_Apt_Cd== "TSA"| 
                 flt_merge$Dep_Apt_Cd== "KHH"] <- 1

############################
########### 畫圖 ########### 
############################

# 畫Horizontal violin
# Route 10
flt_merge %>%
  mutate(Route = fct_reorder(Route, Fuel_Cost_Index)) %>% # Reorder data
  ggplot(aes(x=Route, y=Fuel_Cost_Index, fill=Route, color=Route), family="sans") +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Fuel Cost Index") +
  theme_ipsum() +
  theme(legend.position="none", text=element_text(family="sans"))

# Route 20
flt_merge %>%
  mutate(Route20 = fct_reorder(Route20, as.numeric(Route))) %>% # Reorder data
  ggplot(aes(x=Route20, y=Fuel_Cost_Index, fill=Route20, color=Route20), family="Arial") +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Fuel Cost Index")+
  theme_ipsum() +
  theme(legend.position="none", text=element_text(family="sans"),
        axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 20))

################################
########### 加入時間 ########### 
################################

flt_merge <-
  flt_merge %>% mutate(No_Pushback= Block_Tm- Taxi_Out)

ggplot(flt_merge %>% group_by(Route) %>%
         mutate(medMPG = as.numeric(Route))
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Route), size=3 , alpha= I(5/10), shape= 16, stroke = 0) +
  # scale_color_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_x_continuous("Block Tm")+
  scale_y_continuous("Fuel Cost Index")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

################################## 
########### 畫寬窄比例 ########### 
################################## 

# Histogram
Acft_Typ_Cd_precent <- 
  flt_merge %>% 
  mutate(id = 1) %>%
  group_by(Route, Acft_Typ_Cd) %>%
  summarise(SUM = sum(id)) %>%
  mutate(Type = as.numeric(Acft_Typ_Cd)) %>%
  ungroup() %>%
  mutate(Route = fct_reorder(Route, Type))

ggplot(Acft_Typ_Cd_precent
       , aes(fill=Acft_Typ_Cd, x=Route, y=SUM))+
  geom_bar(stat="identity", position="fill", width=.8) +
  scale_fill_manual(values= c("#99CCCC","#FFCC99"), name="Acft Type")+
  # scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_discrete("")+
  scale_y_continuous("")+
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))



  
  