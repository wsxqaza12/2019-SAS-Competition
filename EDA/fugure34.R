library(ggthemr)
ggthemr_reset()
dust_theme <- ggthemr('chalk', set_theme = FALSE, type = 'outer')
dust_theme <- ggthemr('flat', set_theme = FALSE, type = 'outer')
dust_theme <- ggthemr('dust', set_theme = FALSE, type = 'outer')
example_plot

setwd("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/")
flt_merge <- read.csv("flt_2.csv")

summer <- read.csv("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/flt_1.csv")
winter <- read.csv("C:/Users/User/Desktop/研究所/碩二上/SAS長榮/DATA/flt_2.csv")
winter$BorF <- factor(winter$BorF, labels = c("Come", "Go"), levels= c("0", "1"))
combind <- rbind(summer, winter)


time <- factor(unique(flt_merge$Dep_Tm), levels= c("00:00~05:59", "06:00~11:59",
                                                   "12:00~17:59", "18:00~23:59"))

flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[1]] <- "midnight"
flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[2]] <- "morning"
flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[3]] <- "afternoon"
flt_merge$Dep_absTime[flt_merge$Dep_Tm == levels(time)[4]] <- "night"

flt_merge$Dep_absTime <- factor(flt_merge$Dep_absTime,
                                levels= c("morning", "afternoon", "night", "midnight"))


########################################################
########################################################
flt_merge <- read.csv("flt_2.csv")
A <- paste("TPE-PNH")

a <- min(combind %>% filter(Route== A) %>%select(Block_Tm))
b <- max(combind %>% filter(Route== A) %>%select(Block_Tm))
x <- c(a,b)

c <- min(combind %>% filter(Route== A) %>%select(Fuel_Cost_Index))
d <- max(combind %>% filter(Route== A) %>%select(Fuel_Cost_Index))
y <- c(c,d)


dd <- flt_merge %>% 
  filter(Route== A)
# 
ggplot(flt_merge %>% 
         filter(Route== A)
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  # geom_point(aes(color= Dep_absTime), size= 5) +
  geom_point(aes(color= Dep_absTime), size= 5) +
  # scale_color_manual(values= c("#FC918B", "#547C87"))+
  scale_color_manual(values= c("#FC918B", "#E9DD6F", "#547C87","#383A3C"))+
  # scale_color_brewer(palette = "Set2") +
  scale_x_continuous("Block Tm", limits = c(as.numeric(x[1]), as.numeric(x[2])))+
  scale_y_continuous("Fuel Cost", limits = c(as.numeric(y[1]), as.numeric(y[2])))+
  # dust_theme$theme+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

# Flt_Nur
ggplot(flt_merge %>% 
         filter(Route== A)
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  # geom_point(aes(color= Dep_absTime), size= 5) +
  geom_point(aes(color= as.factor(Flt_Nbr)), size= 5) +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous("Block Tm", limits = c(as.numeric(x[1]), as.numeric(x[2])))+
  scale_y_continuous("Fuel Cost", limits = c(as.numeric(y[1]), as.numeric(y[2])))+
  # dust_theme$theme+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

########################################################
# Block_Tm
########################################################
flt_merge <- read.csv("flt_2_8.csv")
A <- paste("TPE-SJC")

a <- min(combind %>% filter(Route== A) %>%select(Block_Tm))
b <- max(combind %>% filter(Route== A) %>%select(Block_Tm))
x <- c(a,b)

c <- min(combind %>% filter(Route== A) %>%select(Fuel_Cost_Index))
d <- max(combind %>% filter(Route== A) %>%select(Fuel_Cost_Index))
y <- c(c,d)

pdf("~/Desktop/SAS長榮/figure/TPE-SJC_W.pdf", width = 8.55, height = 5.74)
ggplot(flt_merge %>% filter(Route== A)
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Dep_absTime), size= 5) +
  # scale_color_manual(values= c("#CC9966","#666666"))+
  scale_color_manual(values= c("#FC918B", "#547C87"))+
  # scale_color_manual(values= c("#FC918B", "#E9DD6F", "#547C87","#383A3C"))+
  scale_x_continuous("Block Tm", limits = c(as.numeric(x[1]), as.numeric(x[2])))+
  scale_y_continuous("Fuel Cost", limits = c(as.numeric(y[1]), as.numeric(y[2])))+
  dust_theme$theme+
  # theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

dev.off()

# Flt_Nur
ggplot(flt_merge %>% 
         filter(Route== A)
       , aes(x = Block_Tm, y= Fuel_Cost_Index)) +
  # geom_point(aes(color= Dep_absTime), size= 5) +
  geom_point(aes(color= as.factor(Flt_Nbr)), size= 5) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous("Block Tm", limits = x)+
  scale_y_continuous("Fuel Cost", limits = y)+
  dust_theme$theme+
  # theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))


########################################################
# total_weight
########################################################
flt_merge <- read.csv("flt_1.csv")
A <- paste("TPE-SJC")

a <- min(combind %>% filter(Route== A) %>%select(total_weight))
b <- max(combind %>% filter(Route== A) %>%select(total_weight))
x <- c(a,b)

c <- min(combind %>% filter(Route== A) %>%select(Fuel_Cost_Index))
d <- max(combind %>% filter(Route== A) %>%select(Fuel_Cost_Index))
y <- c(c,d)

# pdf("~/Desktop/SAS長榮/figure/TPE-SJC_W.pdf", width = 8.55, height = 5.74)
ggplot(flt_merge %>% filter(Route== A)
       , aes(x = total_weight, y= Fuel_Cost_Index)) +
  geom_point(aes(color= Dep_absTime), size= 5) +
  # scale_color_manual(values= c("#CC9966","#666666"))+
  # scale_color_manual(values= c("#FC918B", "#547C87"))+
  scale_color_manual(values= c("#FC918B", "#547C87"))+
  scale_x_continuous("total weight", limits = c(as.numeric(x[1]), as.numeric(x[2])))+
  scale_y_continuous("Fuel Cost", limits = c(as.numeric(y[1]), as.numeric(y[2])))+
  dust_theme$theme+
  # theme_minimal() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.line=element_line(color="gray",size=1))

dev.off()