library(tidyverse)

jun_resp <- read.csv("C:/Users/kathe/OneDrive/Documents/CICOES/oyster_respirometry/june2023_field_resp/corrected_resp_rates.csv")

jun_resp_summary <- jun_resp %>%
  group_by(Colour, Bag.Number) %>%
  summarize(bag_st_dev = sd(standardized_rate)+
            count = count())

ggplot(data=jun_resp%>%filter(Colour=="red", tolower(Bag.Number)!="Blank", Bag.Number!=""))+
  geom_boxplot(aes(x=reorder(Bag.Number, Date), y=standardized_rate, color=Site))

ggplot(data=jun_resp%>%filter(Colour=="blue", tolower(Bag.Number)!="Blank", Bag.Number!=""))+
  geom_boxplot(aes(x=reorder(Bag.Number, Date), y=standardized_rate, color=Site))

ggplot(data=jun_resp%>%filter(Colour=="green", tolower(Bag.Number)!="Blank", Bag.Number!=""))+
  geom_boxplot(aes(x=reorder(Bag.Number, Date), y=standardized_rate, color=Site))


ggplot(data=jun_resp%>%filter(tolower(Bag.Number)!="Blank", Bag.Number!="", 
                              Respirometry.number!=86, Respirometry.number!=90, Respirometry.number!=89))+
  geom_histogram(aes(x=standardized_rate))

jun_resp_rem_outliers <- jun_resp%>%filter(tolower(Bag.Number)!="Blank", 
                                          Bag.Number!="", 
                                          Respirometry.number!=86, 
                                          Respirometry.number!=90, 
                                          Respirometry.number!=89)


shapiro.test(jun_resp_rem_outliers$standardized_rate)

summary(aov(standardized_rate~ Bag.Number, 
            data=jun_resp_rem_outliers))

ggplot(data=jun_resp_rem_outliers%>%filter(Colour=="red", tolower(Bag.Number)!="Blank", Bag.Number!=""))+
  geom_boxplot(aes(x=reorder(Bag.Number, Date), y=standardized_rate, color=Site))

ggplot(data=jun_resp_rem_outliers%>%filter(Colour=="blue", tolower(Bag.Number)!="Blank", Bag.Number!=""))+
  geom_boxplot(aes(x=reorder(Bag.Number, Date), y=standardized_rate, color=Site))

ggplot(data=jun_resp_rem_outliers%>%filter(Colour=="green", tolower(Bag.Number)!="Blank", Bag.Number!=""))+
  geom_boxplot(aes(x=reorder(Bag.Number, Date), y=standardized_rate, color=Site))

warnings()
