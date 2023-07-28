library(tidyverse)

raw_data_wd <- "C:/Users/kathe/OneDrive/Documents/CICOES/raw_data/growth_sampling"
project_wd <- "C:/Users/kathe/OneDrive/Documents/CICOES/oyster_respirometry/growth"

setwd(raw_data_wd)
growth_files <- list.files(raw_data_wd, all.files = FALSE)

apr_growth1 <- read.csv(file=growth_files[1])
apr_growth2 <- read.csv(file=growth_files[2])
apr_growth3 <- read.csv(file=growth_files[3])


tempdat <- data.frame(Tag.colour=c(apr_growth2$Tag.colour[451:700]),
              Tag.number=c(apr_growth2$Tag.number[451:700]),
              Height=c(apr_growth2$Height[451:700]))
summary(tempdat)

#fix the section in the data where mass and height are switched
for(i in 451:700){
  apr_growth2$Height[i] <- apr_growth2$Mass[i]
  apr_growth2$Mass[i] <- tempdat$Height[(i-450)]
}
apr_growth2$Height <- as.numeric(apr_growth2$Height)
apr_growth2$Field.tag.colour <- apr_growth2$Tag.colour
apr_growth2$Field.bag.number <- apr_growth2$Site.bag.number
apr_growth2$Lab.bag <- apr_growth2$Lab.Bag


apr_growth <- bind_rows(apr_growth2, apr_growth3) %>% 
  left_join(apr_growth1, by=c('Field.tag.colour'='Colour',
                                                                                  'Field.bag.number'='Number'))


apr_avgs <- apr_growth %>%
  group_by(Site, Ploidy) %>%
  summarize(avg_mass=mean(Mass),avg_height=mean(Height))


summary(apr_growth)
row.names(apr_growth$X.x[which(apr_growth$X.2==38)])

###### JUNE GROWTH SAMPLING ###########################################################

#Read in the field data for each site -- do this once
for(i in 8:11){
  df <- read.csv(file = growth_files[i])
  assign(paste('jun_growth',i,sep=''),df)
}

#Look at each june growth file individually to check data quality and clean up
jun_growth11$Site <- "Thorndyke"
jun_growth11$Bag.colour <- as.factor(jun_growth11$Bag.colour)
summary(jun_growth11)

#problems:
#   - NAs in bag number
#   - Mass read as character (because of added unit from scale?)
#   - Height read as character (because of m's?)
#   - Bag colours are all over the place


#Chelsea had NA's for bag number -- 2 sections of unknowns were missing bag numbers
# jun_growth8$Bag.number[310:318] <- 46
# jun_growth8$Bag.number[419:438] <- 35

#Separate numeric value from unit to parse mass as numeric type
jun_growth11 <- separate(data=jun_growth11, col=Mass, into=c("Mass", "Mass.unit"), sep=' ')
jun_growth11$Mass <- as.numeric(jun_growth11$Mass)
summary(jun_growth11)

#Turn m's into NAs and also get rid of masses that correspond with m's
#Check which column has the m's -- sometimes heights, sometimes masses
#May or may not be necessary after using as.numeric()
#Also beware of mixed format (i.e. make sure NAs in height match mass after running this step)
for(i in 1:length(jun_growth11$Height)){
  if(jun_growth11$Height[i]=='m'){
    jun_growth11$Height[i] <- NA
    jun_growth11$Mass[i] <- NA
  }
}
for(i in 1:length(jun_growth11$Height)){
  if(is.na(jun_growth11$Height[i])){
    jun_growth11$Mass[i] <- NA
  }
}
for(i in 1:length(jun_growth11$Height)){
  if(jun_growth11$Height[i] < 1){
    jun_growth11$Mass[i] <- NA
    jun_growth11$Height[i] <- NA
  }
}
jun_growth11$Height <- as.numeric(jun_growth11$Height)
summary(jun_growth11)

jun_growth11$Mass[which(jun_growth11$Mass<1)]

jun_growth11$Bag.colour[which(is.na(jun_growth11$Height)& !is.na(jun_growth11$Mass))]
jun_growth11$Bag.number[which(is.na(jun_growth11$Height)& !is.na(jun_growth11$Mass))]
jun_growth11$Tag.number[which(is.na(jun_growth11$Height)& !is.na(jun_growth11$Mass))]

#Fix wacky bag colors -- separate by space and get rid of junk, then parse as factor instead of character
jun_growth8 <- separate(data=jun_growth8, col=Bag.colour, into="Bag.colour", sep=" ")
jun_growth11$Bag.colour <- as.factor(tolower(jun_growth11$Bag.colour))
summary(jun_growth11)

#Bonus points: use color column to assign ploidy
for(i in 1:length(jun_growth11$Bag.colour)){
  if(jun_growth11$Bag.colour[i]=='red'){
    jun_growth11$Ploidy[i] <- 'Mated triploid'
  } else if(jun_growth11$Bag.colour[i]=='blue'){
    jun_growth11$Ploidy[i] <- 'Induced triploid'
  } else if(jun_growth11$Bag.colour[i]=='green'){
    jun_growth11$Ploidy[i] <- 'Diploid'
  }
}
jun_growth11$Ploidy <- as.factor(jun_growth11$Ploidy)
summary(jun_growth11)

ggplot(data=jun_growth10)+
  geom_boxplot(aes(x=Ploidy, y=Mass))

setwd(project_wd)
write.csv(jun_growth11, "june_tb_growth.csv")

clean_jun_growth <- rbind(jun_growth8, jun_growth9, jun_growth10, jun_growth11)

ggplot(data=clean_jun_growth)+
  geom_histogram()
