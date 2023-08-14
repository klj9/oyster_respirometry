library(ggpubr)
library(tidyverse)
library(rstatix)

june_respdat <- read.csv("~/CICOES/oyster_respirometry/june2023_field_resp/corrected_rates_best.csv")
july_respdat <- read.csv("~/CICOES/oyster_respirometry/july2023_field_resp/corrected_resp_rates.csv")

june_respdat$month <- "June"
june_respdat <- june_respdat %>% select(!positive)

july_respdat$month <- "July"
july_respdat <- july_respdat %>% select(!positve)

june_respdat$Bag.Number <- as.numeric(june_respdat$Bag.Number)
june_respdat$month <- as.factor(june_respdat$month)
july_respdat$Bag.Number <- as.numeric(july_respdat$Bag.Number)
july_respdat$month <- as.factor(july_respdat$month)

anova2_dat <- rbind(june_respdat, july_respdat)
anova2_dat$bag_id <- as.factor(paste(anova2_dat$Colour, anova2_dat$Bag.Number, sep=" "))
str(anova2_dat)
anova_dat$Site <- as.factor(anova_dat$Site)
anova_dat$Colour <- as.factor(anova_dat$Colour)


#Check assumptions of normality and no extreme outliers
ggplot(data=anova2_dat, aes(x=standardized_rate))+
  geom_histogram()

anova2_dat$log_rate <- log(-anova2_dat$standardized_rate) #log transformation
ggplot(data=anova2_dat, aes(x=log_rate))+
  geom_histogram()

shapiro.test(anova2_dat$log_rate) #p value <<0.05 so not normal, but this test is pretty rigid
#log transformed rates are much closer to normal than non-transformed

anova2_dat %>% identify_outliers(standardized_rate) #no extreme outliers
anova2_dat %>% identify_outliers(log_rate) #one extreme outlier

ggqqplot(anova2_dat, "log_rate", ggtheme = theme_bw()) +
  facet_grid(~month, labeller = "label_both")


anova2_dat <-anova2_dat %>% ungroup()

resp_anova2 <- anova_test(data=anova2_dat, 
                         standardized_rate~Site*Colour*month)
get_anova_table(resp_anova2)
plot(resp_anova)

log_anova2 <- anova_test(data=anova2_dat, 
                        log_rate~Site*Colour*month)
get_anova_table(log_anova2)

log_anova_by_ploidy <- anova_test(data=anova2_dat%>%filter(Site=="Thorndyke", Colour=="blue"), 
                                  log_rate~month)
get_anova_table(log_anova_by_ploidy)

?anova_test
anova2_dat %>%
  pairwise_t_test(
    log_rate ~ Site,
    p.adjust.method = "bonferroni"
  )

anova2_dat %>%
  filter(Colour=="red", month=="June") %>%
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
library(lme4)

mixed.lmer <- lmer(log_rate~Site+(1|bag_id), data=anova2_dat)
summary(mixed.lmer)
plot(mixed.lmer)
