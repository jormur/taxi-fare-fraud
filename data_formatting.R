##### SUMMARY STATS #####
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

# TABLE II #
#Create table summary detailing driver characteristics
# Feature engineering: Extract hour and calculate trip duration
taxi <- taxi %>%
  mutate(
    pickup_hour = hour(tpep_pickup_datetime),
    dropoff_hour = hour(tpep_dropoff_datetime),
    trip_duration = as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")),
    # working_hours = ceiling(as.numeric(difftime(dropoff_hour, pickup_hour, units = "hours")))  # Round up to the nearest whole number
  )

# Adjust working hours for trips that cross midnight
# taxi <- taxi %>%
#   mutate(
#     working_hours = ifelse(dropoff_hour < pickup_hour, 24 + dropoff_hour - pickup_hour, working_hours)
#   )

# Create the summary table
driver_summary_table <- taxi %>%
  group_by(PULocationID) %>%
  summarize(
    num_providers = length(unique(VendorID)),
    num_trips = n(),
    working_hours = sum(ceiling(ifelse(dropoff_hour < pickup_hour, 24 + dropoff_hour - pickup_hour, trip_duration / 60))),  # Round up to the nearest whole number
    occupied_hours = sum(ifelse(passenger_count > 0, trip_duration / 60, 0)),  # Convert trip_duration to hours
    occupancy = occupied_hours / working_hours,
    num_trips_LGA = sum(PULocationID == 138),  # Replace LGA_Location_ID with the actual Location ID for LGA
    num_trips_JFK = sum(PULocationID == 132)   # Replace JFK_Location_ID with the actual Location ID for JFK
  )

# Use knitr to format the table
kable(driver_summary_table, caption = "Driver Characteristics by Departure Location and Trip Type")


# TABLE III #
#Create table summary statistics including the number of trips, the average trip distance, duration and fare
#This is done for each respective departure location further breaking down into drop off location
#Each departure location is to be broken down into "All" and then residential and hotel
#Residential is defined as a trip that ends in DOLocationID = 229, otherwise it is a hotel trip

table_summary <- taxi %>%
  group_by(PULocationID, trip_type) %>%
  summarize(
    num_trips = n(),
    avg_trip_distance = mean(trip_distance),
    avg_duration = mean(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins")),
    avg_fare = mean(fare_amount)
  )

# Use knitr to format the table
kable(table_summary, caption = "Summary Statistics by Departure Location and Trip Type")


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







