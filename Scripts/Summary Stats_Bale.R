setwd("C:/Users/alexw/OneDrive/Documents/PHD/Bale Mountains Playback Experiments/Data")

library(tidyr)
library(dplyr)
library(ggplot2)

FID <- read.csv("FID Raw Data.csv")
glimpse(FID)


FID <- FID[2:34,1:17]
FID <- FID[-3,]

hist(FID$Group.Size)
hist(FID$Distance.to.Cover)
hist(FID$Flight.Distance)


model <- lm(Flight.Distance ~ Location, data = FID)

summary(model)
plot(model)


plot(FID$Longitude,FID$Latitude)


## Playbacks

playback.data <- read.csv("Raw Data_Playbacks.csv")
glimpse(playback.data)

# What sex were playbacks done on?

nrow(playback.data %>% filter(Sex=="F")) # 95 Females
nrow(playback.data %>% filter(Sex=="M")) # 68 Males
## One playback is unclear sex (Calf)


## Where did playbacks occur?

nrow(playback.data %>% filter(Location=="Gaysay")) # 115 in Gaysay
nrow(playback.data %>% filter(Location!="Gaysay")) # 49 around Lodge


# Habitats


playback.data$Habitat<-as.factor(playback.data$Habitat)

## Simplify factors
levels(playback.data$Habitat)[levels(playback.data$Habitat) == "Artemisia"] <- "Scrub" 
levels(playback.data$Habitat)[levels(playback.data$Habitat) == "Closed Woodland"] <- "Woodland" 
levels(playback.data$Habitat)[levels(playback.data$Habitat) == "Open Woodland"] <- "Woodland" 


nrow(playback.data %>% filter(Habitat=="Woodland")) # 53 in Woodland
nrow(playback.data %>% filter(Habitat=="Scrub")) # 81 in Scrub
nrow(playback.data %>% filter(Habitat=="Grassland")) # 24 in Grassland
nrow(playback.data %>% filter(Habitat=="Wetland")) # 6 around Wetland

##

library(plyr)

playback.data$Location<-as.factor(playback.data$Location)
levels(playback.data$Location)[levels(playback.data$Location) == "Lodge"] <- "HQ" 


mu <- ddply(playback.data, "Location", summarise, grp.mean=mean(Distance..m.))
head(mu)


ggplot(playback.data, aes(x=Distance..m., fill=Location, color=Location)) + geom_histogram(binwidth=10,alpha=0.5, position="identity") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Location), linetype="dashed", linewidth=1.5) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2") + xlab("Distance (m)") +ylab("Count") + theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),legend.title = element_text(size = 16))


