# MAIN ESTIMATION
setwd("~/Documents/uni/Year 2/Semester 1/Applied Microeconometrics/term project/code")
# This script calculates summary statistics for the data based upon each respective paper

library(dplyr)
library(tidyr)
library(knitr)
library(lubridate)
library(data.table)

#First, we read the parquet file into R
library(arrow)
taxi <- read_parquet("sample_data.parquet")

# Create a new variable to identify residential or hotel trips
taxi <- taxi %>%
  mutate(trip_type = ifelse(DOLocationID == 229, "Residential", "Hotel"))

# Feature engineering: Extract hour and calculate trip duration
taxi <- taxi %>%
  mutate(
    pickup_hour = hour(tpep_pickup_datetime),
    dropoff_hour = hour(tpep_dropoff_datetime),
    trip_duration = as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")),
    # working_hours = ceiling(as.numeric(difftime(dropoff_hour, pickup_hour, units = "hours")))  # Round up to the nearest whole number
  )

#Remove trips where the pickup = 132 (JFK) has a distance less than 10 and more than 40
#And where the pickup = 138 (LGA) has a distance less than 5 and more than 20
taxi <- taxi %>%
  filter(trip_duration > 0) %>% 
  filter(!(PULocationID == 132 & (trip_distance < 10 | trip_distance > 40))) %>%
  filter(!(PULocationID == 138 & (trip_distance < 5 | trip_distance > 20)))

#Create a new variable to identify the respective trip type based on departure and drop off location
#By combining the ID's of the departure and drop off location, we can identify the trip type
#For example, if the departure location is 132 (JFK) and the drop off location is 229 (Residential), then the trip type is 132229
taxi <- taxi %>%
  mutate(tripID = paste(PULocationID, DOLocationID, sep = ""))

#Create a policy treatment variable to identify when the policy intervention took place: 1 = after, 0 = before
#The policy took effect on September 4, 2012, at 12:01 am
taxi <- taxi %>%
  mutate(treatment = ifelse(tpep_pickup_datetime >= as.POSIXct("2012-09-04 00:01:00"), 1, 0))




############# VARIABLE CONSTRUCTION ############
## EXPECTED OCCUPANCY ##
#This is a trial attempt at recreating the expected occupancy from the paper to be used as a covariate
#This is done with regressing the actual occupancy on a number of time based indicator controls
#This includes year indicators (0-3), hour of day by day of week (0-167), week of year (0-51)

#Create year indicator (0-3)
taxi <- taxi %>%
  mutate(
    year = year(tpep_pickup_datetime) - 2011
  )

#Create hour of day by day of week indicator (0-167)
taxi <- taxi %>%
  mutate(
    hour_of_day = hour(tpep_pickup_datetime),
    day_of_week = wday(tpep_pickup_datetime),
    hour_of_day_by_day_of_week = hour_of_day + (day_of_week - 1) * 24
  )

#Create week of year indicator (0-51)
taxi <- taxi %>%
  mutate(
    week_of_year = week(tpep_pickup_datetime) - 1
  )

#Reformulation of actual occupancy
taxi <- taxi %>%
  mutate(
    pickup_hour = hour(tpep_pickup_datetime),
    dropoff_hour = hour(tpep_dropoff_datetime),
    trip_duration = as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins"))
  )

taxi <- taxi %>%
  mutate(
    working_hours = ceiling(ifelse(dropoff_hour < pickup_hour, 24 + dropoff_hour - pickup_hour, trip_duration / 60)),
    occupancy = trip_duration / (working_hours*60)
  )

#Regression of controls on actual occupancy
occ_model <- lm(occupancy ~ year + hour_of_day_by_day_of_week + week_of_year, data = taxi)

#Extract the predicted occupancy
occ_predict <- predict(occ_model, newdata = taxi)

#Add the predicted occupancy to the taxi data
taxi$pred_occ <- occ_predict



## DETOUR INDEX ##
#Join the detour indices from result_df to the main taxi data, matching based on "VendorID", "tpep_pickup_datetime", "PULocationID" and "DOLocationID". 
#This would add the detour_distance, detour_duration, and detour_fare columns to the main taxi data
taxi <- taxi %>%
  left_join(result_df, by = c("VendorID", "tpep_pickup_datetime", "PULocationID", "DOLocationID"))

#Export the taxi data to a stata dta file
# library(haven)
# write_dta(taxi, "taxi.dta")




#### DID IMPLEMENTATION ####
#Make tripID numeric
taxi <- taxi %>%
  mutate(tripID = as.numeric(tripID))

#Create a date variable from the drop off date containing the date only
taxi <- taxi %>%
  mutate(date = as.Date(tpep_dropoff_datetime))

#Make pred_occ numeric
taxi <- taxi %>%
  mutate(pred_occ = as.numeric(pred_occ))

#Make date numeric
taxi <- taxi %>%
  mutate(date = as.numeric(date))

#Drop rows where the detour_distance and pred_occ are NA
taxi <- taxi %>%
  drop_na(detour_distance, pred_occ)

library(did)

#The following seeks to replicate Table V from the paper
#We will estimate each column of the table separately to fall in line wit the empirical specification

#Column 1: LGA Distance
lga_d = att_gt(
  yname         = "detour_distance",
  tname         = "date",
  idname        = "tripID",
  gname         = "pred_occ",
  # xformla       = NULL,            # No additional controls in this dataset 
  control_group = "notyettreated", # Too few groups for "nevertreated" default
  clustervars   = "tripID", 
  data          = taxi,
  panel = FALSE
)

library(fixest)
feols(detour_distance ~ pred_occ + treatment + pred_occ*treatment | tripID + tpep_dropoff_datetime, taxi)


#Drop duplicates for tripID and dropoff time
taxi <- taxi %>%
  distinct(tripID, tpep_dropoff_datetime, .keep_all = TRUE)

library(panelView)
panelview(detour_distance ~ treatment, data = taxi, display.all = FALSE, 
          index = c("tripID","tpep_dropoff_datetime"), xlab = "Date", ylab = "Trip ID",
          axis.lab.gap = c(0,1), by.timing = TRUE)









