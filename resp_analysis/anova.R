library(ggpubr)
library(tidyverse)
library(rstatix)

june_respdat <- read.csv("~/CICOES/oyster_respirometry/june2023_field_resp/corrected_rates_best.csv")
july_respdat <- read.csv("~/CICOES/oyster_respirometry/july2023_field_resp/corrected_resp_rates.csv")

june_by_bag <- june_respdat %>%
  group_by(Site, Colour, Bag.Number) %>%
  summarize(resp_rate=mean(standardized_rate), month="June")

july_by_bag <- july_respdat %>%
  group_by(Site, Colour, Bag.Number) %>%
  summarize(resp_rate=mean(standardized_rate), month="July")

june_by_bag$Bag.Number <- as.numeric(june_by_bag$Bag.Number)
june_by_bag$month <- as.factor(june_by_bag$month)
july_by_bag$Bag.Number <- as.numeric(july_by_bag$Bag.Number)
july_by_bag$month <- as.factor(july_by_bag$month)

anova_dat <- rbind(june_by_bag, july_by_bag)
anova_dat$bag_id <- as.factor(paste(anova_dat$Colour, anova_dat$Bag.Number, sep=" "))
str(anova_dat)
anova_dat$Site <- as.factor(anova_dat$Site)
anova_dat$Colour <- as.factor(anova_dat$Colour)


#Check assumptions of normality and no extreme outliers
ggplot(data=anova_dat, aes(x=resp_rate))+
  geom_histogram()

anova_dat$log_rate <- log(-anova_dat$resp_rate) #log transformation
ggplot(data=anova_dat, aes(x=log_rate))+
  geom_histogram()

shapiro.test(anova_dat$log_rate) #p value <<0.05 so not normal, but this test is pretty rigid
#log transformed rates are much closer to normal than non-transformed

anova_dat %>% identify_outliers(resp_rate) #no extreme outliers
anova_dat %>% identify_outliers(log_rate) #one extreme outlier

ggqqplot(anova_dat, "log_rate", ggtheme = theme_bw()) +
  facet_grid(~month, labeller = "label_both")


anova_dat <-anova_dat %>% ungroup()

resp_anova <- anova_test(data=anova_dat, 
                         dv=resp_rate, 
                         wid=bag_id, 
                         within=month,
                         between=c(Site, Colour))
get_anova_table(resp_anova)
plot(resp_anova)

log_anova <- anova_test(data=anova_dat, 
                         dv=log_rate, 
                         wid=bag_id, 
                         within=month,
                         between=c(Site, Colour))
get_anova_table(log_anova)

log_anova_by_ploidy <- anova_test(data=anova_dat%>%filter(Site=="Hood Head"), 
                                  log_rate~month)
get_anova_table(log_anova_by_ploidy)

?anova_test
anova_dat %>%
  pairwise_t_test(
    log_rate ~ Site,
    p.adjust.method = "bonferroni"
  )

anova_dat %>%
  filter(Colour=="green", month=="June") %>%
  pairwise_t_test(
    log_rate ~ Site,
    p.adjust.method = "bonferroni"
  )

anova_dat %>%
  pairwise_t_test(
    log_rate ~ Colour,
    p.adjust.method = "bonferroni"
  )

###############################################################################################
paired_respdat <- full_join(june_by_bag, july_by_bag, by=c('Colour', 'Bag.Number'))
paired_respdat$bag_id <- NA

for(i in 1:length(paired_respdat$Bag.Number)){
  paired_respdat$bag_id[i] <- paste(paired_respdat$Colour[i], paired_respdat$Bag.Number[i], sep=" ")
}

paired_respdat$bag_id <- as.factor(paired_respdat$bag_id)

aov

