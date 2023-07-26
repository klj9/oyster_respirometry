library(tidyverse)
project_wd <- "C:/Users/Kathe/OneDrive/Documents/CICOES/oyster_respirometry/carbonate_chem"
setwd(project_wd)

junedat <- read.csv("june2023/june2023_carb_chem_system.csv")
julydat <- read.csv("july2023/july2023_carb_chem_system.csv")

junedat$month="June"
julydat$month="July"

junejuly <- rbind(junedat, julydat)

site_avgs <- junejuly %>%
  group_by(Site, month) %>%
  summarize(avg_pH=mean(pH.sample.with.dye.corrections), 
            avg_TA=mean(TA_evap), 
            avg_temp=mean(temp.Field), 
            avg_sal=mean(Salinity),
            avg_arag=mean(seacarb.OmegaAragonite))

ggplot(data=site_avgs, aes(x=Site, y=avg_pH, fill=month))+
  geom_bar(stat="identity", position='dodge')

ggplot(data=site_avgs, aes(x=Site, y=avg_temp, fill=month))+
  geom_bar(stat="identity", position='dodge')

ggplot(data=site_avgs, aes(x=Site, y=avg_TA, fill=month))+
  geom_bar(stat="identity", position='dodge')  

ggplot(data=site_avgs, aes(x=Site, y=avg_arag, fill=month))+
  geom_bar(stat="identity", position='dodge')
