library(tidyverse)
project_wd <- "C:/Users/Kathe/OneDrive/Documents/CICOES/oyster_respirometry/carbonate_chem"
setwd(project_wd)

junedat <- read.csv("june2023/june2023_carb_chem_system.csv")
julydat <- read.csv("july2023/july2023_carb_chem_system.csv")

junedat$month="June"
julydat$month="July"

junejuly <- rbind(junedat, julydat)


#plots using averages from field measurements

site_avgs <- junejuly %>%
  group_by(Site, month) %>%
  summarize(avg_pH=mean(pH.sample.with.dye.corrections), 
            avg_TA=mean(TA_evap), 
            avg_temp=mean(temp.Field), 
            avg_sal=mean(Salinity),
            avg_arag=mean(seacarb.OmegaAragonite),
            avg_pCO2=mean(seacarb.pCO2))

junjul_pH <- ggplot(data=site_avgs, aes(x=Site, y=avg_pH, fill=forcats::fct_rev(month)))+
  geom_bar(stat="identity", position='dodge')+
  labs(fill="Month")+
  coord_cartesian(ylim=c(7,9))+
  ylab("pH")

#ggplot(data=site_avgs, aes(x=Site, y=avg_temp, fill=forcats::fct_rev(month)))+
#  geom_bar(stat="identity", position='dodge')

junjul_TA <- ggplot(data=site_avgs, aes(x=Site, y=avg_TA, fill=forcats::fct_rev(month)))+
  geom_bar(stat="identity", position='dodge')+
  labs(fill="Month")+
  ylab("Total Alkalinity (" ~mu~ "mol/kg)")+
  coord_cartesian(ylim=c(1500,2150))

junjul_arag <- ggplot(data=site_avgs, aes(x=Site, y=avg_arag, fill=forcats::fct_rev(month)))+
  geom_bar(stat="identity", position='dodge')+
  labs(fill="Month")+
  ylab("Omega Aragonite")

junjul_pCO2 <- ggplot(data=site_avgs, aes(x=Site, y=avg_pCO2, fill=forcats::fct_rev(month)))+
  geom_bar(stat="identity", position='dodge')+
  labs(fill="Month")+
  ylab(expression("pCO2 (" ~mu~ "atm)"))

ggsave(filename="~/CICOES/oyster_respirometry/carbonate_chem/july2023/figures/junjul_pH.pdf", plot=junjul_pH, width=5, height=3)
ggsave(filename="~/CICOES/oyster_respirometry/carbonate_chem/july2023/figures/junjul_TA.pdf", plot=junjul_TA, width=5, height=3)
ggsave(filename="~/CICOES/oyster_respirometry/carbonate_chem/july2023/figures/junjul_arag.pdf", plot=junjul_arag, width=5, height=3)
ggsave(filename="~/CICOES/oyster_respirometry/carbonate_chem/july2023/figures/junjul_pCO2.pdf", plot=junjul_pCO2, width=5, height=3)
