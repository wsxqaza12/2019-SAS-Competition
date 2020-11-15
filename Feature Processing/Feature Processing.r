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
setwd("~/Documents/SAS長榮/複賽/")

pax <- read.csv("pax_detail_1.csv")
od <- read.csv("od_segment_new.csv")

# 1.OD 處理 ####
# 有幾個OD
odn <- 
  od %>%
  mutate(C= 1) %>%
  group_by(Tvl_Seq, OD_Grp_Id) %>%
  summarise(ODnumber = sum(C)) %>%
  ungroup() %>%
  group_by(Tvl_Seq) %>%
  mutate(C=1) %>%
  summarise(ODnumber = sum(C))

# 有幾個seq
seq <- 
  od %>%
  mutate(C= 1) %>%
  group_by(Tvl_Seq) %>%
  summarise(seqnumber = sum(C))

# 偷看
Trans %>% filter(Tvl_Seq== "114995639") %>% arrange(OD_Grp_Id)
od %>% filter(Tvl_Seq== "114995639") %>% arrange(OD_Grp_Id)

# 有無轉機
Trans <- 
  od %>%
  mutate(C = 1) %>%
  group_by(Tvl_Seq, OD_Grp_Id) %>%
  summarise(No = sum(C)) 
  

Trans$Tr <- 0
Trans$Tr[Trans$No > 1] <- 1

Trans$Trn <- Trans$No-1

Trans_Tvl <-
  Trans %>% 
  group_by(Tvl_Seq) %>%
  summarise(Trans = sum(Tr), Trn = sum(Trn)) %>%
  left_join(odn, by = "Tvl_Seq") %>%
  left_join(seq, by = "Tvl_Seq")

Trans_Tvl$TransYN[Trans_Tvl$Trans== 0] <- "No"
Trans_Tvl$TransYN[Trans_Tvl$Trans > 0] <- "Yes"

Trans_Tvl <- Trans_Tvl %>% select(-Trans)
  
Trans_Tvl %>% filter(Tvl_Seq== "118791546") 
Trans %>% filter(Tvl_Seq== "118791546") %>% arrange(OD_Grp_Id)
od %>% filter(Tvl_Seq== "103658305") %>% arrange(OD_Grp_Id)
odn %>% filter(Tvl_Seq== "118791546")

# 有無移民
od <- od %>% arrange(Tvl_Seq)
Trans_Tvl <- Trans_Tvl %>% arrange(Tvl_Seq)
end <- 0
i <- 1
Trans_Tvl$Immigration <- "Yes"
for (i in 1:dim(Trans_Tvl)[1]) {
  start <- end+1
  end <- start + Trans_Tvl$seqnumber[i] -1
  
    if (od$Ori_Region_Desc[start] == od$Dest_Region_Desc[end]) {
      Trans_Tvl$Immigration[i] <- "No"
    }
}

i <- 1

# 1.1旅行過程 ####
Step <- 
  od %>%
  mutate(C = 1) %>%
  group_by(Tvl_Seq, OD_Grp_Id) %>%
  summarise(No = sum(C)) 

  # 從Tvl & OD table填上旅遊過程
end <- 0
Step$step <- "mid"
Step$step[Step$OD_Grp_Id== 1] <- "begin"
i
for (i in 1:dim(Trans_Tvl)[1]) {
  start <- end+1
  end <- start + Trans_Tvl$ODnumber[i] -1
  Step$step[end] <- "end"
}

 # 併入有seg的表(od)
Step_seg <- 
  od %>%
  left_join(Step %>% select(Tvl_Seq, OD_Grp_Id, step),
            by = c("Tvl_Seq", "OD_Grp_Id"))

pax_step <- 
  pax %>%
  left_join(Step_seg %>% select(Tvl_Seq, Seg_Id, step),
            by = c("Tvl_Seq", "Seg_Id"))

# 2.MAP ####
Flightroute <- read_xlsx("Airport.xlsx", sheet= 1)
flt_merge <- read.csv("flt_schedule_1.csv")

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

# 拉出連結線起終的lat&long，方便畫curve #####
curve_connect <- 
  Count_route_flight[Count_route_flight$Dep_Apt_Cd == "TPE" | 
                       Count_route_flight$Dep_Apt_Cd == "TSA" | 
                       Count_route_flight$Dep_Apt_Cd == "KHH", ] %>%
  merge(Flightroute %>% select(代碼, lat, long), by.x = "Dep_Apt_Cd", by.y = "代碼")

names(curve_connect)[4:5] <- c("Dlat", "Dlong")

curve_connect <- 
  curve_connect %>% merge(Flightroute %>% select(代碼, lat, long), by.x = "Arr_Apt_Cd", by.y = "代碼") 

names(curve_connect)[6:7] <- c("Alat", "Along")

curve_connect$Arr_Apt_Cd <- as.character(curve_connect$Arr_Apt_Cd)
curve_connect[8:11, ] <- curve_connect[7, ] 
i <- 8
for (i in 8:11) {
  curve_connect[i, 1] <- Flightroute$代碼[i+1]
  curve_connect[i, 6] <- Flightroute$lat[i+1]
  curve_connect[i, 7] <- Flightroute$long[i+1]
  
}

# 加上這次的


# 畫世界地圖 #####

world.map <- map_data ("world")
world.map <- world.map %>% 
  filter(region != "Antarctica")

worldmap <- ggplot() + 
  geom_map(data= world.map, map= world.map,
           aes(x= long, y= lat, group= group, map_id= region),
           fill="white", colour="grey", size = 0.1) 

mycols<-c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
          "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928")

# 畫curve的地圖線 ######
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
  geom_line(data=subset(allConnections_list[[6]], lon >=0),aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=subset(allConnections_list[[6]], lon <0),aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[7]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[8]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[9]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[10]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
  geom_line(data=allConnections_list[[11]],aes(x=lon,y=lat,color=start_airport ), lwd= 2) +
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



# write functione for auto ####
read.csv("od_segment_new.csv")

name <- "od_segment_new.csv"
a <- paste("read.csv('", name, "')", sep= "")
a <- paste0("read.csv('", name, "')")
a
setwd("~/Documents/SAS長榮/複賽/")
eval(parse(text = a))

DATA <- name
WHERE <- "複賽"

TEST <- function(DATA, WHERE) {
  a <- paste0("read.csv('~/Documents/SAS長榮/", WHERE, "/", DATA, "')")
  b <- eval(parse(text = a))
  return(b)
}


paste("/opt/workspace/sas46/", a, "/", )
Vaild <- TEST("od_segment_new.csv", "複賽")
