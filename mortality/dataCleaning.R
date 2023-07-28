
mortdat <- read.csv("~/CICOES/oyster_respirometry/mortality/Mortalities.csv")

summary(mortdat)
mortdat$Site <- as.factor(mortdat$Site)
mortdat$Colour <- as.factor(tolower(mortdat$Colour))
summary(mortdat)

for(i in 1:length(mortdat$Colour)){
  if(mortdat$Colour[i]=="gren") {
    mortdat$Colour[i] <- "green"
  }
}

ggplot(data=mortdat, aes(x=Site))+
  geom_bar(stat='count')

