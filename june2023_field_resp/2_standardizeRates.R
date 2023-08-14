library(tidyverse)
project_wd <- "C:/Users/Kathe/OneDrive/Documents/CICOES/oyster_respirometry/june2023_field_resp"
setwd(project_wd)
animalratedat <- read.csv("animal_resp_rates_best.csv")

###### CORRECT FOR BACKGROUND ###################################################################################
bgrddat <- animalratedat %>% 
  filter(Colour == "Blank" | Colour == "blank") %>% 
  select(run, rate) %>%
  filter(run != 14, run != 6)#background rates for runs 6 and 14 were too high -- produced positive resp rates

#now runs 2, 6 and 14 are missing background values -- take mean of runs before and after
missing_blanks <- data.frame(run=c(2, 14, 6),
                             rate=c(mean(bgrddat$rate[which(bgrddat$run==1 | bgrddat$run==3)]),
                                    mean(bgrddat$rate[which(bgrddat$run==13 | bgrddat$run==15)]),
                                    mean(bgrddat$rate[which(bgrddat$run==5 | bgrddat$run==7)])))
bgrddat <- rbind(bgrddat, missing_blanks)
bgrddat <- bgrddat %>% rename(bgrd_rate=rate)#prep for merge by renaming rate column

animalratedat <- merge(animalratedat, bgrddat, by="run")

#subtract the background rates from the animal rates
animalratedat$corrected_rate <- animalratedat$rate - animalratedat$bgrd_rate

###### STANDARDIZE BY DRY MASS ###################################################################################
massdat <- read.csv("june2023_dry_mass.csv")

#calculate missing tissue dry mass values
massdat$Oyster.tissue.dry.mass <- massdat$Tin.and.oyster.tissue.dry.mass - massdat$Empty.tin.mass
#select necessary info
june_dry_mass <- massdat %>% filter(Origin=="JuneResp") %>% select(Oyster.Number, Oyster.tissue.dry.mass)
#left join mass data onto rates data by oyster/repirometry number
animalratesdat_withmass <- merge(animalratedat, june_dry_mass, by.x="Respirometry.number", by.y="Oyster.Number")
animalratesdat_withmass$standardized_rate <- animalratesdat_withmass$corrected_rate/animalratesdat_withmass$Oyster.tissue.dry.mass




###### FINAL CLEANUP #############################################################################################

#check for positive respiration rates -- there shouldn't be any
animalratesdat_withmass$positive <- animalratesdat_withmass$standardized_rate > 0
summary(animalratesdat_withmass)#for June data there are 8 positive rates -- deal with on a case-by-case basis
animalratesdat_withmass$run[which(animalratesdat_withmass$positive)]

#run 3 & 4 - one rate was so small that background subtracted it into a positive value
animalratesdat_withmass <- animalratesdat_withmass %>% 
  filter(Respirometry.number!=29, Respirometry.number!=22)

#run 14 - positive value started out positive, which should be impossible so we'll throw it out
#there were also two impossibly high rates from this run (probe may not have been calibrated correctly?)
animalratesdat_withmass <- animalratesdat_withmass %>% 
  filter(Respirometry.number!=89, Respirometry.number!=90, Respirometry.number!=86)

#correct misspellings of the bag colors
animalratesdat_withmass$Colour <- as.factor(tolower(animalratesdat_withmass$Colour))
summary(animalratesdat_withmass)

animalratesdat_withmass$Colour[which(animalratesdat_withmass$Colour=="red ")] <- "red"
animalratesdat_withmass$Colour[which(animalratesdat_withmass$Colour=="green ")] <- "green"

#correct misspelling of sites
animalratesdat_withmass$Site <- as.factor(animalratesdat_withmass$Site)
summary(animalratesdat_withmass)
animalratesdat_withmass$Site[which(animalratesdat_withmass$Site=="Thordyke")] <- "Thorndyke"


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

#filtered out crazy outliers and blanks in my plot
ggplot(data=animalratesdat_withmass)+
  geom_boxplot(aes(x=Site, y=standardized_rate))+
  geom_jitter(aes(x=Site, y=standardized_rate))+
  facet_wrap(~Ploidy)

#save cleaned and standardized rates as csv
write.csv(animalratesdat_withmass, "corrected_rates_best.csv")
