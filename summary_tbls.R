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









