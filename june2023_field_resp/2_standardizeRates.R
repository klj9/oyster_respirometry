library(tidyverse)
project_wd <- "C:/Users/Kathe/OneDrive/Documents/CICOES/oyster_respirometry/june2023_field_resp"
setwd(project_wd)
animalratedat <- read.csv("animal_resp_rates.csv")

###### CORRECT FOR BACKGROUND ###################################################################################
bgrddat <- animalratedat %>% 
  filter(Colour == "Blank" | Colour == "blank") %>% 
  select(run, rate) %>%
  filter(run != 14, run != 6)#background rates for runs 6 and 14 were too high -- produced positive resp rates

#now runs 2 and 14 are missing background values -- take mean of runs before and after
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
animalratesdat_withmass$positive <- animalratesdat_withmass$standardized_rate > 0

summary(animalratesdat_withmass)

#filtered out crazy outliers and blanks in my plot
ggplot(data=animalratesdat_withmass%>%filter(Respirometry.number!=90 & Respirometry.number!=89 & tolower(Colour)!="blank" & run!=3))+
  geom_boxplot(aes(x=Site, y=standardized_rate))+
  geom_jitter(aes(x=Site, y=standardized_rate, color=positive))