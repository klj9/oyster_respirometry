library(tidyverse)
project_wd <- "C:/Users/Kathe/OneDrive/Documents/CICOES/oyster_respirometry/carbonate_chem/july2023"
setwd(project_wd)

#read in pH data from spec
specdat <- read.csv("spec_data_all_jul2023_field_samples.csv", header=TRUE)
summary(specdat)

specdat$Site <- as.factor(specdat$Site)



ggplot(data=specdat)+
  geom_boxplot(aes(x=Site, y=pH.sample.with.dye.corrections))+
  geom_jitter(aes(x=Site, y=pH.sample.with.dye.corrections), width=0.2)

#read in total alkalinity data from the titrator
tadat1 <- read.csv("20230720TA_final_run1.csv")
tadat2 <- read.csv("20230720TA_final_run2.csv")
tadat3 <- read.csv("20230721TA_final_run1.csv")
tadat4 <- read.csv("20230721TA_final_run2.csv")
ta_data <- bind_rows(tadat1, tadat2, tadat3, tadat4) %>% filter(SampleID!='JUNK1' & SampleID!='JUNK2')
ta_data <- separate(data=ta_data, col=SampleID, into=c("Site", "Date", "Rep"), sep='_')

ta_data <- ta_data %>% filter(Site!="PBB")



summary(ta_data)

#change site names from titrator to match those from spec
for(i in 1:length(ta_data$Site)){
  if(ta_data$Site[i] == "MANC"){
    ta_data$Site[i] = "Manchester"
  } else if(ta_data$Site[i] == "HOOD"){
    ta_data$Site[i] = "Hood Head"
  } else if(ta_data$Site[i] == "CHEL"){
    ta_data$Site[i] = "Chelsea"
  } else if(ta_data$Site[i] == "TB"){
    ta_data$Site[i] = "Thorndyke"
  } 
}


ta_data$Site <- as.factor(ta_data$Site)

ggplot(data=ta_data, aes(x=Site, y=TA))+
  geom_boxplot()+
  geom_jitter(width=0.2)

#change rep labels from spec to match those from titrator
specdat$Rep <- toupper(specdat$Rep)
spec_data <- specdat %>% select(Site, Rep, Date.Collected, ph.Field, temp.Field, Temperature...spec.C, Salinity, pH.sample.with.dye.corrections, pH.of.sample.without.correction) %>% 
                         filter(Site!="PBB Storage tank")

spec_data$temp.Field[which(spec_data$Site=="PBB Storage tank")] <- 25
spec_data$Salinity[which(spec_data$Site=="PBB Storage tank")] <- 29
spec_data$Rep[which(spec_data$Site=="PBB Storage tank")] <- NA


#carbchem_data <- data.frame(Site=c("PBB Storage Tank"), 
#                            TA_evap=c(mean(ta_data$TA_evap[which(ta_data$Site=="PBB")])),
#                            temp=c(mean(spec_data$temp.Field[which(spec_data$Site=="PBB Storage tank")])),
#                            Salinity=c(mean(spec_data$Salinity[which(spec_data$Site=="PBB Storage tank")])),
#                            pH.sample.with.dye.corrections=c(mean(spec_data$pH.sample.with.dye.corrections[which(spec_data$Site=="PBB Storage tank")])))
carbchem_data <- full_join(spec_data, ta_data, by=c("row.names"))

ggplot(data=carbchem_data, aes(x=pH.sample.with.dye.corrections, y=TA_evap))+
  geom_point(aes(color=Site))

ggplot(data=carbchem_data, aes(x=pH.sample.with.dye.corrections, y=temp.Field))+
  geom_point(aes(color=Site))



##### SEACARB #######################################################################
library(seacarb)

carbchem_data$seacarb <- carb(flag=8, #flag 8 uses pH as var1 and TA as var2
                var1=carbchem_data$pH.sample.with.dye.corrections,
                var2=(carbchem_data$TA_evap/1000000),
                T=carbchem_data$temp.Field,
                S=carbchem_data$Salinity,
                P=0)

#change file name          
write.csv(carbchem_data, "july2023_carb_chem_system.csv")

ggplot(data=carbchem_data, aes(x=Site, y=seacarb$OmegaAragonite))+
  geom_boxplot()
