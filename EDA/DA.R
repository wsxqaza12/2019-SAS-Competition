library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(GGally)
        
setwd("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/")
flt_merge <- read.csv("flt_1.csv")


######################
########  Y   ########
######################

# DA
Fuel_Cost_Index <- flt_merge  %>% summarise(mean= mean(Fuel_Cost_Index), sd= sd(Fuel_Cost_Index),
                                            min = min(Fuel_Cost_Index), "25%" = quantile(Fuel_Cost_Index, 0.25),
                                            "50%" = quantile(Fuel_Cost_Index, 0.5), "75%" = quantile(Fuel_Cost_Index, 0.75),
                                            max= max(Fuel_Cost_Index))

# Histogram
ggplot(data = flt_merge, aes(x= Fuel_Cost_Index)) +
  geom_histogram(aes(y=..density..), fill= "#8FBC94", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  scale_x_continuous(limits = c(-20, 120))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# boxplot
# pdf("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/R/boxplot_Y.pdf")
ggplot(data = flt_merge) +
  geom_boxplot(aes(y= Fuel_Cost_Index), fill= "#CC9966", lwd=1)+
  scale_x_discrete(breaks=NULL)+
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 15))
# dev.off()


#################################
########  寬體 vs 窄體   ########
#################################

# DA
Acft_Typ_Cd <- flt_merge %>% group_by(Acft_Typ_Cd) %>% summarise(mean= mean(Fuel_Cost_Index), sd= sd(Fuel_Cost_Index),
                                                    min = min(Fuel_Cost_Index), "25%" = quantile(Fuel_Cost_Index, 0.25),
                                                    "50%" = quantile(Fuel_Cost_Index, 0.5), "75%" = quantile(Fuel_Cost_Index, 0.75),
                                                    max= max(Fuel_Cost_Index))

summary(aov(Fuel_Cost_Index~Acft_Typ_Cd, flt_merge))


# bar
ggplot(data = flt_merge, aes(x= Acft_Typ_Cd)) +
  geom_bar(fill= c("#99CCCC","#FFCC99"), color="black", lwd= 1.5) +
  scale_x_discrete("Acft Type")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


sum(flt_merge$Acft_Typ_Cd== "寬體客機")
sum(flt_merge$Acft_Typ_Cd== "窄體客機")


# and Y (all)
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = Acft_Typ_Cd, y = Fuel_Cost_Index)) +
  geom_boxplot(aes(fill=medMPG), lwd=1.2, show.legend = FALSE) +
  # geom_line(data = month_summary, aes(group = season), size=3.5) +
  scale_x_discrete("Acft_Type") +
  scale_y_continuous("Fuel_Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20))+
  scale_fill_gradientn(colours = c("#99CCCC","#FFCC99"))

# and Y (寬)
ggplot(data = flt_merge %>% filter(Acft_Typ_Cd== "寬體客機")) +
  geom_boxplot(aes(y= Fuel_Cost_Index), fill= "#77AAAD", lwd=1)+
  scale_x_discrete(breaks=NULL)+
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 15))

# and Y (窄)
ggplot(data = flt_merge %>% filter(Acft_Typ_Cd== "窄體客機")) +
  geom_boxplot(aes(y= Fuel_Cost_Index), fill= "#6E7783", lwd=1)+
  scale_x_discrete(breaks=NULL)+
  scale_y_continuous(limits = c(3, 7))+
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 15))

# Histogram
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = Fuel_Cost_Index)) +
  geom_histogram(aes(fill= Acft_Typ_Cd), position='identity') +
  scale_fill_manual(values= c("#99CCCC","#FFCC99"), name="Acft type")+
  scale_x_continuous("Fuel Cost")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


#############################
########  Block_Tm   ########
#############################

# DA
Block_Tm <- flt_merge  %>% summarise(mean= mean(Block_Tm), sd= sd(Block_Tm),
                                     min = min(Block_Tm), "25%" = quantile(Block_Tm, 0.25),
                                     "50%" = quantile(Block_Tm, 0.5), "75%" = quantile(Block_Tm, 0.75),
                                     max= max(Block_Tm))

# Histogram
ggplot(data = flt_merge, aes(x= Block_Tm)) +
  geom_histogram(aes(y=..density..), fill= "#77AAAD", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  # geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  # scale_x_continuous(limits = c(-20, 120))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# Histogram + Type
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = Block_Tm)) +
  geom_histogram(aes(fill= Acft_Typ_Cd, color=Acft_Typ_Cd), position='identity', alpha= I(5/10), size= 1.3) +
  scale_fill_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_continuous("Acft_Type")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# plot + Type + Fuel
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Acft_Typ_Cd), size= 3, alpha= I(5/10)) +
  scale_color_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_x_continuous("Block Tm")+
  scale_y_continuous("Fuel Cost Index")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


###########################
########  Flt_Tm   ########
###########################
# DA
Flt_Tm <- flt_merge  %>% summarise(mean= mean(Flt_Tm), sd= sd(Flt_Tm),
                                     min = min(Flt_Tm), "25%" = quantile(Flt_Tm, 0.25),
                                     "50%" = quantile(Flt_Tm, 0.5), "75%" = quantile(Flt_Tm, 0.75),
                                     max= max(Flt_Tm))

# Histogram
ggplot(data = flt_merge, aes(x= Flt_Tm)) +
  geom_histogram(aes(y=..density..), fill= "#77AAAD", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  # geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  # scale_x_continuous(limits = c(-20, 120))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# Histogram + Type
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = Flt_Tm)) +
  geom_histogram(aes(fill= Acft_Typ_Cd, color=Acft_Typ_Cd), position='identity', alpha= I(5/10), size= 1.3) +
  scale_fill_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_continuous("Acft_Type")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# plot + Type + Fuel
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = Flt_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Acft_Typ_Cd), size= 5, alpha= I(5/10)) +
  scale_color_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_x_continuous("Flt_Tm")+
  scale_y_continuous("Fuel_Cost")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


########################################
########  Block_Tm- Taxi_Out    ########
########################################
flt_merge <-
  flt_merge %>% mutate(No_Pushback= Block_Tm- Taxi_Out)

# DA
No_Pushback <- flt_merge  %>% summarise(mean= mean(No_Pushback), sd= sd(No_Pushback),
                                   min = min(No_Pushback), "25%" = quantile(No_Pushback, 0.25),
                                   "50%" = quantile(No_Pushback, 0.5), "75%" = quantile(No_Pushback, 0.75),
                                   max= max(No_Pushback))

# Histogram
ggplot(data = flt_merge, aes(x= No_Pushback)) +
  geom_histogram(aes(y=..density..), fill= "#6E7783", alpha= I(8/10)) + 
  # geom_density(aes(y=..density..), color= "#548687", lwd= 1)+
  # geom_line(stat = "density", color= "#548687", lwd= 1.2)+
  # scale_x_continuous(limits = c(-20, 120))+
  scale_x_continuous("No Pushback")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# Histogram + Type
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = No_Pushback)) +
  geom_histogram(aes(fill= Acft_Typ_Cd, color=Acft_Typ_Cd), position='identity', alpha= I(5/10), size= 1.3) +
  scale_fill_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_color_manual(values= c("#99CCCC","#FFCC99"))+
  scale_x_continuous("Acft Type")+
  scale_y_continuous("Numbers")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# plot + Type + Fuel
ggplot(flt_merge %>% group_by(Acft_Typ_Cd) %>%
         mutate(medMPG = as.numeric(Acft_Typ_Cd))
       , aes(x = No_Pushback, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Acft_Typ_Cd), size= 5, alpha= I(5/10)) +
  scale_color_manual(values= c("#99CCCC","#FFCC99"), name="Acft_Type")+
  scale_x_continuous("No Pushback")+
  scale_y_continuous("Fuel Cost")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))



#########################
########  Time   ########
#########################

# DA (all)
my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_point(..., alpha = I(7/10)) +
    scale_color_manual(values= c("#CC9966","#666666"))
}

ggally_mysmooth <- function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(fill=NA, lwd= 1)+
    scale_color_manual(values= c("#CC9966","#666666"))
  
}

A <- paste("TPE-IAH")
ggpairs(data = flt_merge %>% 
          filter(Route== A), 
        mapping = aes_string(color = "Direction"),
        columns = c("Taxi_Out", "Block_Tm", "total_weight", "Fuel_Cost_Index"),
        upper = list(continuous = function(data, mapping, ...) {
          ggally_cor(data = data, mapping = mapping, size = 4) + 
            scale_color_manual(values= c("#CC9966","#666666"))}),
        
        lower = list(continuous = my_dens, discrete = "facetbar", combo = "box"),
        diag = list(continuous = ggally_mysmooth, discrete = "barDiag"))+
  theme(plot.subtitle = element_text(size= 25))
