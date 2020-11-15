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

Flightroute <- read_xlsx("飛機地點代碼.xlsx", sheet= 1)
flt_merge <- read.csv("flt_merge.csv")

flight249 <- flt_merge %>% gather('Dep_Apt_Cd', 'Arr_Apt_Cd', key = "Airport_type", value = "Airport")

gloabal.flight.route <-
  flight249 %>% merge(Flightroute %>% select(代碼, lat, long), by.x = "Airport", by.y = "代碼")

# 算各台灣機場有幾條線
Count_route <- unique(flt_merge %>% select(Dep_Apt_Cd, Arr_Apt_Cd)) %>% 
  mutate(id= seq(1, 20, 1), cou= 1) %>% 
  group_by(Dep_Apt_Cd) %>%
  summarise(sum= sum(cou))

# 各航線有幾筆資料
Count_route_flight <- flt_merge %>% 
  select(Flt_Id, Dep_Apt_Cd, Arr_Apt_Cd, Acft_Typ_Cd) %>%
  mutate(cou= 1) %>%
  group_by(Dep_Apt_Cd, Arr_Apt_Cd) %>%
  summarise(No= sum(cou))

########################################################
# 拉出連結線起終的lat&long，方便畫curve
curve_connect <- 
  Count_route_flight[Count_route_flight$Dep_Apt_Cd == "TPE" | 
  Count_route_flight$Dep_Apt_Cd == "TSA" | 
  Count_route_flight$Dep_Apt_Cd == "KHH", ] %>%
  merge(Flightroute %>% select(代碼, lat, long), by.x = "Dep_Apt_Cd", by.y = "代碼")

names(curve_connect)[4:5] <- c("Dlat", "Dlong")

curve_connect <- 
  curve_connect %>% merge(Flightroute %>% select(代碼, lat, long), by.x = "Arr_Apt_Cd", by.y = "代碼") 

names(curve_connect)[6:7] <- c("Alat", "Along")


# gather過的 畫lines
Connect_route <- unique(flt_merge %>% select(Dep_Apt_Cd, Arr_Apt_Cd)) %>% 
  mutate(id= seq(1, 20, 1)) 

Connect_route$Group <- 1
Connect_route$Group[Connect_route$Dep_Apt_Cd == "KHH" | Connect_route$Arr_Apt_Cd== "KHH"] <- 2
Connect_route$Group[Connect_route$Dep_Apt_Cd == "TSA" | Connect_route$Arr_Apt_Cd== "TSA"] <- 3

Connect_route$id <- as.factor(Connect_route$id)
Connect_route$Group <- as.factor(Connect_route$Group)

Connect_route <- Connect_route %>%
  gather('Dep_Apt_Cd', 'Arr_Apt_Cd', key = "Airport_type", value = "Airport") %>% 
  merge(Flightroute %>% select(代碼, lat, long), by.x = "Airport", by.y = "代碼")

########################################################
# 畫世界地圖

world.map <- map_data ("world")
world.map <- world.map %>% 
  filter(region != "Antarctica")

worldmap <- ggplot() + 
  geom_map(data= world.map, map= world.map,
           aes(x= long, y= lat, group= group, map_id= region),
           fill="white", colour="grey", size = 0.1) 
  
mycols<-c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
          "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928")

########################################################
# 以顏色飽和表達航班忙度
worldmap + 
  geom_point(data= Flightroute, aes(x= long, y= lat),size= 2, color= 'blue', alpha= 1) + 
  geom_line(data = gloabal.flight.route, 
            aes(x = long, y = lat, group = Flt_Id), 
            alpha = 0.05, lwd=1.2)+
  # geom_line(data= allConnections_list[[1]], aes(x=lon, y=lat, color= end_airport)) +
  geom_label_repel(data=Flightroute, aes(x=long, y=lat,label=Flightroute$代碼),
                   size=3,
                   fontface = 'bold', 
                   color = 'black',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.color = 'black',alpha=.75) + 
  # theme_fivethirtyeight() + 
  theme(legend.position='none') + 
  scale_color_manual(values=colorRampPalette(mycols)(10)) + 
  ggtitle(paste0('Departures from '))

########################################################
# 色彩表示3站點的航線

# pdf("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/R/Map_3.pdf",width= 17, height= 10)
worldmap + 
  geom_line(data = Connect_route, 
            aes(x = long, y = lat, group = id, color= Group), lwd= 1.2)+
  geom_point(data= Flightroute, aes(x= long, y= lat),size= 2, color= '#666666', alpha= I(7/10)) + 
  # geom_line(data= allConnections_list[[1]], aes(x=lon, y=lat, color= end_airport)) +
  geom_label_repel(data=Flightroute, aes(x=long, y=lat,label=Flightroute$代碼),
                   size=8,
                   fontface = 'bold', 
                   color = 'black',
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(1, "lines"),
                   segment.color = 'black',alpha=.75) + 
  theme_fivethirtyeight() +
  scale_fill_brewer(palette="Set3")+
  theme(legend.position='none') + 
  ggtitle(paste0('Airline Routes'))

# dev.off()

# 色彩表示3站點的航線- 亞太線
# pdf("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/R/Map_asia.pdf",width= 10, height= 14)
worldmap + 
  geom_line(data = Connect_route, 
            aes(x = long, y = lat, group = id, color= Group), lwd= 1.2)+
  geom_point(data= Flightroute, aes(x= long, y= lat),size= 2, color= '#666666', alpha= I(7/10)) + 
  # geom_line(data= allConnections_list[[1]], aes(x=lon, y=lat, color= end_airport)) +
  geom_label_repel(data=Flightroute, aes(x=long, y=lat,label=Flightroute$代碼),
                   size=5,
                   fontface = 'bold', 
                   color = 'black',
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(1, "lines"),
                   segment.color = 'black',alpha=.75) + 
  # theme_fivethirtyeight() +
  xlim(100, 140) + ylim(-10, 40) +
  scale_fill_brewer(palette="Set3")+
  theme(legend.position='none')

# dev.off()


########################################################
# 畫curve的地圖線
allConnections_list <- list()

for(i in 1:nrow(curve_connect)){
  allConnections_df<-data.frame(lon=double(), lat=double(), start_airport=character(), end_airport = character())
  start<-c(curve_connect$Dlong[i],curve_connect$Dlat[i])
  end<-c(curve_connect$Along[i],curve_connect$Alat[i])
  inter <- data.frame(gcIntermediate(start,  end, n=100, addStartEnd=TRUE, breakAtDateLine=F))
  inter$start_airport <- rep(curve_connect$Dep_Apt_Cd[i],nrow(inter))
  inter$end_airport <- rep(curve_connect$Arr_Apt_Cd[i],nrow(inter))
  allConnections_df<-rbind(allConnections_df,inter)
  allConnections_list[[i]]<-allConnections_df
}


# pdf("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/R/Map_3_curve.pdf",width= 17, height= 10)
worldmap + 
  geom_line(data=subset(allConnections_list[[1]], lon >=0),aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=subset(allConnections_list[[1]], lon <0),aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[2]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[3]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[4]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[5]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[6]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=subset(allConnections_list[[7]], lon >=0),aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=subset(allConnections_list[[7]], lon <0),aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[8]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[9]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[10]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_point(data= Flightroute, aes(x= long, y= lat), pch=20,cex=5, color= 'slateblue') + 
  # geom_line(data= allConnections_list[[1]], aes(x=lon, y=lat, color= end_airport)) +
  geom_label_repel(data=Flightroute, aes(x=long, y=lat,label=Flightroute$代碼),
                   size=8,
                   fontface = 'bold', 
                   color = 'black',
                   box.padding = unit(1, "lines"),
                   point.padding = unit(2, "lines"),
                   segment.color = 'black',alpha=.75) + 
  theme_fivethirtyeight() +
  scale_color_manual(values= c('#F9B426',"skyblue",'#EB5E4F'))+
  theme(legend.position='none') 

# dev.off()


# 色彩表示3站點的航線- 亞太線
# png("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/R/Map_asia_curve.png",width= 10, height= 14, units= "cm")
worldmap + 
  geom_line(data=allConnections_list[[2]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[4]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[5]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[6]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[8]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[9]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[10]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_point(data= Flightroute, aes(x= long, y= lat), pch=20,cex=5, color= 'slateblue') + 
  geom_label_repel(data=Flightroute, aes(x=long, y=lat,label=Flightroute$代碼),
                   size=5,
                   fontface = 'bold', 
                   color = 'black',
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(1, "lines"),
                   segment.color = 'black',alpha=.75) + 
  # theme_fivethirtyeight() +
  xlim(100, 140) + ylim(-10, 40) +
  scale_color_manual(values= c('#F9B426',"skyblue",'#EB5E4F'), name= "Start Airport")+
  theme(legend.position='none')

# dev.off()

########################################################
# radar chart
summer <- read.csv("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/flt_1.csv") 
winter <- read.csv("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/flt_2.csv")
winter$BorF <- factor(winter$BorF, labels = c("Come", "Go"), levels= c("0", "1"))
flt_merge <- rbind(summer, winter)


ALLstate <- Flightroute$代碼[-c(2, 8, 9)]

TPE <- data.frame()
TSA <- data.frame()
KHH <- data.frame()

for( i in 1:length(ALLstate)){
  TPE[1,i] <- dim(flt_merge %>% filter(Dep_Apt_Cd== "TPE" & Arr_Apt_Cd== ALLstate[i]))[1]
  TSA[1,i] <- dim(flt_merge %>% filter(Dep_Apt_Cd== "TSA" & Arr_Apt_Cd== ALLstate[i]))[1]
  KHH[1,i] <- dim(flt_merge %>% filter(Dep_Apt_Cd== "KHH" & Arr_Apt_Cd== ALLstate[i]))[1]
}

colnames(TPE) <-  ALLstate
colnames(TSA) <-  ALLstate
colnames(KHH) <-  ALLstate

RC_TPE <- rbind(rep(28,10) , rep(0,10) , TPE)
RC_TSA <- rbind(rep(28,10) , rep(0,10) , TSA)
RC_KHH <- rbind(rep(28,10) , rep(0,10) , KHH)

# TPE
radarchart( RC_TPE  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(63,130,178,215, maxColorValue=255) , pfcol=rgb(145,192,225,150, maxColorValue=255) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty= 1, axislabcol="grey", caxislabels=seq(0,28,7), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

col2rgb('#3F82B2')
col2rgb('#91C0E1')

# TSA
radarchart( RC_TSA  , axistype=1 , 
            pcol=rgb(235,94,79,215, maxColorValue=255) , pfcol=rgb(255,172,163,150, maxColorValue=255) , plwd=4 , 
            cglcol="grey", cglty= 1, axislabcol="grey", caxislabels=seq(0,28,7), cglwd=0.8,
            vlcex=0.8 
)

col2rgb('#EB5E4F')
col2rgb('#FFACA3')

# KHH
radarchart( RC_KHH  , axistype=1 , 
            pcol=rgb(249,180,38,215, maxColorValue=255) , pfcol=rgb(255,211,120,215, maxColorValue=255) , plwd=4 , 
            cglcol="grey", cglty= 1, axislabcol="grey", caxislabels=seq(0,28,7), cglwd=0.8,
            vlcex=0.8 
)

col2rgb('#F9B426')
col2rgb('#FFD378')
