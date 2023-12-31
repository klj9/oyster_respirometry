##Author Craig Norrie - cnorrie@uw.edu
#Last modified: November 2022

# Load packages ------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(lubridate)
library(respR)
# Define working directories ----------------------------------------------
project_wd <- "C:/Users/kathe/OneDrive/Documents/CICOES/oyster_respirometry/july2023_field_resp"
data_wd_temperature <- "C:/Users/kathe/OneDrive/Documents/CICOES/raw_data/july2023_field_resp"
expt <- "temperature"
# Read in all the data ----------------------------------------------------
##This code will create a numbered DF corresponsing to the order of the runs. 
#make sure the index of the file numbers in "resp_files" matches up to the run number
###Future goal - make this into a function###
setwd(data_wd_temperature)#setwd() #set wd for raw data location
resp_files_temperature <- list.files(data_wd_temperature, 
                                     all.files = FALSE) #Creates a vector with all the filenames from respirometry
respdat_all_temperaure <- data.frame()#Creates an empty data frame to store respirometry data
#read in all the data from all files in the data folder and place them all into a single dataframe
for(i in 1:length(resp_files_temperature)){
  tempdat <- read.csv(file = resp_files_temperature[i], skip = 1)
  tempdat <- slice(tempdat, 1:(n() - 2))#removes the last two rows becasue it is junk
  tempdat$run <- i#add a new column based on the number sheet that it is
  respdat_all_temperaure <- rbind(respdat_all_temperaure, tempdat)#used plyr::rbind instead of dplyr::bind_rows because this deals with duplicate column names
  rm(tempdat)
}
setwd(project_wd)#change the working directory back to the project folder
#create a df for each run and pivot wider

#glimpse(respdat_all_temperaure)

#Assign a run number to each of the CSVs
respdat_all_temperaure$run <- as.factor(respdat_all_temperaure$run)
runlist <- levels(respdat_all_temperaure$run)#Gets a list of all the dates to filter by
for(i in 1:length(runlist)){
  df <- respdat_all_temperaure %>% select(delta_t,Channel,Value, run) %>% filter(run == runlist[i])
  df <- df %>% select(delta_t,Channel,Value) %>% pivot_wider(names_from = Channel, values_from = Value)
  df$delta_t <- as.numeric(df$delta_t)
  assign(paste('run_',i,sep=''),df) #creates a df for each of the da
  
}

# #Create a new DF with just the data that we want to use in calculating rates
# oxylevels <- respdat_all_temperaure %>% select(delta_t,Channel,Value, run)
# oxylevels$delta_t <- as.numeric(oxylevels$delta_t)
# oxylevels$Channel <- as.factor(oxylevels$Channel)
# oxylevels %>% filter(run == "39") %>% ggplot(aes(x=delta_t, y=Value, colour = Channel))+geom_line()#inspect individual runs

# Calculate oxygen uptake rates using the RespR package ------------------
###https://januarharianto.github.io/respR/index.html
#create a df to store data from all runs
allratesdat <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("probe", "rate", "run"))))
#inspect each run to determine where we are going to calculate the rate (i.e. remove any weird spots of data)
run_num <- run_4#Put the number of the run that you are caclulcating rate for here
df.name <- "4" ##Update this each time you update the above
inspect(run_num, time = 1, oxygen = 2:8)#*Assumes 8 probe respirometer****
#Set the start and end times over the period of o2 consumption based on inspection of data
#About a 30 minute period with no blips/weird behavior
start_time <- 42
end_time <- 55

#run_8 = run_8[-c(4)]#to remove problematic probe 7 readings--RAN ONLY ONCE

######YOU SHOULD NOT HAVE TO CHANGE ANY OF THE BELOW CODE FOR EACH RUN


#Calculate oxygen uptake levels for each probe over the specified period
#What is the rate over a specific 25 minute period?” from = 4, to = 29, by = "time"
#Create an empty data frame to save the results of the loop
respdf <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("probe", "rate"))))#create an empty df to store rates.
#i should be in the first column of the df and tempoxydf$summary$rate should be in the second
for(i in 2:ncol(run_num)){
  tempoxy <- inspect(run_num, time = 1, oxygen = i)
  oxrate <- calc_rate(tempoxy, from = start_time, to = end_time)#loop this for each probe in each run and save the data
  tempoxydf <- data.frame(matrix(ncol=3,nrow=1, dimnames=list(NULL, c("probe", "rate","run"))))
  tempoxydf$probe <- colnames(run_num)[i]
  tempoxydf$rate <- oxrate$summary$rate
  tempoxydf$run <- df.name
  respdf <- rbind(respdf, tempoxydf)
}

#warnings()

#rename the df to the name of teh run that you just used
assign(paste('rates_',df.name,sep=''),respdf)#names the df with the run name

##Add all this data to one big data frame 

allratesdat <- rbind(allratesdat, respdf)

# Match the respiometery log with the rates -------------------------------
#Read in your respirometry log - make sure that it is in the appropriate folder
resplog <- read.csv("resplog.csv")
#ensure data classes are the same - not really necessary
resplog$probe <- as.factor(resplog$Chamber)
resplog$run <- as.factor(resplog$Run)
allratesdat$run <- as.factor(allratesdat$run)
allratesdat$probe <- as.factor(allratesdat$probe)
#Merge the data to one df
animalratedat <- merge(resplog, allratesdat, by=c('probe', 'run'))
animalratedat$Colour <- as.factor(animalratedat$Colour)
#animalratedat$Treatment <- as.factor(animalratedat$Treatment)

write.csv(animalratedat, "animal_resp_rates.csv", row.names=FALSE)
