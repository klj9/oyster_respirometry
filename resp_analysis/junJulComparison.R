

june_respdat <- read.csv("~/CICOES/oyster_respirometry/june2023_field_resp/corrected_rates_best.csv")
july_respdat <- read.csv("~/CICOES/oyster_respirometry/july2023_field_resp/corrected_resp_rates.csv")

glimpse(june_respdat)
glimpse(july_respdat)

june_respdat$month <- "June"
june_respdat <- june_respdat %>%
  select(!positive)


july_respdat$month <- "July"
july_respdat <- july_respdat %>%
  select(!positve)

junjul_respdat <- rbind(june_respdat, july_respdat)
glimpse(junjul_respdat)



ggplot(data=junjul_respdat)+
  geom_boxplot(aes(x=month, y=standardized_rate))+
  facet_wrap(~Site)
  
ggplot(data=junjul_respdat%>%filter(Site=="Chelsea"))+
  geom_boxplot(aes(x=forcats::fct_rev(month), y=standardized_rate))+
  geom_jitter(aes(x=forcats::fct_rev(month), y=standardized_rate), width=0.2)+
  facet_wrap(~Colour)+
  ggtitle("Chelsea")
ggplot(data=junjul_respdat%>%filter(Site=="Manchester"))+
  geom_boxplot(aes(x=forcats::fct_rev(month), y=standardized_rate))+
  geom_jitter(aes(x=forcats::fct_rev(month), y=standardized_rate), width=0.2)+
  facet_wrap(~Colour)+
  ggtitle("Manchester")
ggplot(data=junjul_respdat%>%filter(Site=="Hood Head"))+
  geom_boxplot(aes(x=forcats::fct_rev(month), y=standardized_rate))+
  geom_jitter(aes(x=forcats::fct_rev(month), y=standardized_rate), width=0.2)+
  facet_wrap(~Colour)+
  ggtitle("Hood Head")
ggplot(data=junjul_respdat%>%filter(Site=="Thorndyke"))+
  geom_boxplot(aes(x=forcats::fct_rev(month), y=standardized_rate))+
  geom_jitter(aes(x=forcats::fct_rev(month), y=standardized_rate), width=0.2)+
  facet_wrap(~Colour)+
  ggtitle("Thorndyke")

ggplot(data=junjul_respdat%>%filter(month=="June"))+
  geom_boxplot(aes(x=Site, y=-(standardized_rate), fill=Site), show.legend = FALSE)+
  geom_jitter(aes(x=Site, y=-(standardized_rate)), width=0.2)+
  facet_wrap(~Ploidy)+
  ggtitle("June Field Oyster Respiration")+
  ylab(expression("Respiration Rate ("~mu~"mol O2 / L / min / g)"))+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        strip.background = element_rect(fill="white"),
        text=element_text(size=32),
        axis.text.x = element_text(angle = 30, vjust = 0.8, hjust=0.7, size=36))+
  scale_fill_manual(values=c( "#B3C863", "#756A8C","#037D64", "#CEB08E"))+
  coord_cartesian(ylim=c(0,4))

ggplot(data=junjul_respdat%>%filter(month=="July"))+
  geom_boxplot(aes(x=Site, y=-(standardized_rate), fill=Site),show.legend =FALSE)+
  geom_jitter(aes(x=Site, y=-(standardized_rate)), width=0.2)+
  facet_wrap(~Ploidy)+
  ggtitle("July Field Oyster Respiration")+
  ylab(expression("Respiration Rate ("~mu~"mol O2 / L / min / g)"))+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        strip.background = element_rect(fill="white"),
        text=element_text(size=32),
        axis.text.x = element_text(angle = 30, vjust = 0.8, hjust=0.7, size=36))+
  scale_fill_manual(values=c( "#B3C863", "#756A8C","#037D64", "#CEB08E"))+
  coord_cartesian(ylim=c(0,4))


ggplot(data=junjul_respdat%>%filter(month=="July"))+
  geom_boxplot(aes(x=Site, y=-(standardized_rate), fill=Site))+
  geom_jitter(aes(x=Site, y=-(standardized_rate)), width=0.2)+
  facet_wrap(~Ploidy)+
  ggtitle("June Field Oyster Respiration")+
  ylab(expression("Respiration Rate ("~mu~"mol O2 / L / min / g)"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=0.7), plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values=c( "#C4D12F", "#C98A94","#0C5B4F", "#DDAC69"))

?theme_bw()

ggplot(data=june_respdat%>%filter(tolower(Colour)!="blank" & run!=3  & run!=14 & Site=="Chelsea"),
       position='dodge2')+
  geom_boxplot(aes(x=Colour, y=standardized_rate, fill="orange"))+
  geom_jitter(aes(x=Site, y=standardized_rate), width=0.2)+
  geom_boxplot(data=july_respdat%>%filter(Site=="Chelsea"), aes(x=Colour, y=standardized_rate))+
  theme(axis.text.x = element_text(angle = 45))
  
