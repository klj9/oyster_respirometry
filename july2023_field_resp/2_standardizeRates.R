library(tidyverse)
project_wd <- "C:/Users/Kathe/OneDrive/Documents/CICOES/oyster_respirometry/july2023_field_resp"
setwd(project_wd)
animalratedat <- read.csv("animal_resp_rates.csv")

###### CORRECT FOR BACKGROUND ###################################################################################

bgrddat <- animalratedat %>% 
  filter(tolower(Colour) == "blank") %>% 
  select(run, rate) %>%
  filter(run != 5)#comment back in to remove outlier bg rates

head(bgrddat)
  
#now runs 2 and 14 are missing background values -- take mean of runs before and after
missing_blanks <- data.frame(run=5,
                             rate=mean(bgrddat$rate[which(bgrddat$run==4 | bgrddat$run==6)]))
bgrddat <- rbind(bgrddat, missing_blanks)
bgrddat <- bgrddat %>% rename(bgrd_rate=rate)#prep for merge by renaming rate column

animalratedat <- merge(animalratedat, bgrddat, by="run")


#subtract the background rates from the animal rates
animalratedat$corrected_rate <- animalratedat$rate - animalratedat$bgrd_rate

#check for positive rates -- indicates outlier bg rate
animalratedat$positve <- animalratedat$corrected_rate > 0
summary(animalratedat)
animalratedat$Respirometry.number[which(animalratedat$positve==TRUE)]

ggplot(data=animalratedat, aes(x=Site, y=corrected_rate))+
  geom_boxplot()+
  geom_jitter(width=0.2)




###### STANDARDIZE BY DRY MASS ###################################################################################

massdat <- read.csv("dry_mass.csv")
summary(massdat)

#calculate tissue dry mass values if any are missing
massdat$Oyster.tissue.dry.mass <- massdat$Tin.and.oyster.tissue.dry.mass - massdat$Empty.tin.mass
#select necessary info from massdat
july_dry_mass <- massdat %>% filter(Origin=="JulyResp") %>% select(Oyster.Number, Oyster.tissue.dry.mass)
#left join mass data onto rates data by oyster/repirometry number
animalratesdat_withmass <- merge(animalratedat, july_dry_mass, by.x="Respirometry.number", by.y="Oyster.Number")
animalratesdat_withmass$standardized_rate <- animalratesdat_withmass$corrected_rate/animalratesdat_withmass$Oyster.tissue.dry.mass




###### FINAL CLEANUP ###################################################################################

#check through notes section to find any problem runs missed in the initial data cleaning
#I realize that this is an unhinged way to filter this but it was the only way I could find to remove only this one measurement
animalratesdat_withmass <- animalratesdat_withmass %>% 
  filter(rate!=animalratesdat_withmass$rate[which(animalratesdat_withmass$run==11 & animalratesdat_withmass$probe==8)])

summary(animalratesdat_withmass)
animalratesdat_withmass$Colour <- as.factor(tolower(animalratesdat_withmass$Colour))

#correct misspellings of the bag colors
animalratesdat_withmass$Colour[which(animalratesdat_withmass$Colour=="blue "|animalratesdat_withmass$Colour=="bue")] <- "blue"
animalratesdat_withmass$Colour[which(animalratesdat_withmass$Colour=="green ")] <- "green"

#assign ploidy based on bag color
for(i in 1:length(animalratesdat_withmass$Colour)){
  if(animalratesdat_withmass$Colour[i]=='red'){
    animalratesdat_withmass$Ploidy[i] <- 'Mated triploid'
  } else if(animalratesdat_withmass$Colour[i]=='blue'){
    animalratesdat_withmass$Ploidy[i] <- 'Induced triploid'
  } else if(animalratesdat_withmass$Colour[i]=='green'){
    animalratesdat_withmass$Ploidy[i] <- 'Diploid'
  }
}

#exporatory visulaizations
ggplot(data=animalratesdat_withmass)+
  geom_boxplot(aes(x=Site, y=standardized_rate))+
  geom_jitter(aes(x=Site, y=standardized_rate, color=Ploidy))
ggplot(data=animalratesdat_withmass)+
  geom_boxplot(aes(x=Ploidy, y=standardized_rate))+
  geom_jitter(aes(x=Ploidy, y=standardized_rate), width=0.2)+
  facet_wrap(~Site)
ggplot(data=animalratesdat_withmass)+
  geom_boxplot(aes(x=Site, y=standardized_rate))+
  geom_jitter(aes(x=Site, y=standardized_rate), width=0.2)+
  facet_wrap(~Ploidy)

write.csv(animalratesdat_withmass, "corrected_resp_rates.csv")
