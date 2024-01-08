##### SUMMARY STATS #####
setwd("~/Documents/uni/Year 2/Semester 1/Applied Microeconometrics/term project/code")
# This script calculates summary statistics for the data based upon each respective paper

library(dplyr)
library(tidyr)
library(knitr)
library(lubridate)
library(data.table)
library(kableExtra)


# Create the summary table
driver_summary_table <- taxi %>%
  group_by(PULocationID) %>%
  summarize(
    num_trips = n(),
    working_hours = sum(ceiling(ifelse(dropoff_hour < pickup_hour, 24 + dropoff_hour - pickup_hour, trip_duration / 60))),  # Round up to the nearest whole number
    occupied_hours = sum(ifelse(passenger_count > 0, trip_duration / 60, 0)),  # Convert trip_duration to hours
    occupancy = occupied_hours / working_hours
  )

#Rename PUlocationID to Airport
driver_summary_table <- driver_summary_table %>%
  rename(Airport = PULocationID)

#Change Airport values to JK and LGA
driver_summary_table$Airport[driver_summary_table$Airport == 132] <- "JFK"
driver_summary_table$Airport[driver_summary_table$Airport == 138] <- "LGA"

driver_summary_table %>%
  kbl(caption = "Table II: Driver Characteristics") %>%
  kable_classic(full_width = F, html_font = "Georgia")

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

#Rename PUlocationID to Airport
table_summary <- table_summary %>%
  rename(Airport = PULocationID)

#Change Airport values to JK and LGA
table_summary$Airport[table_summary$Airport == 132] <- "JFK"
table_summary$Airport[table_summary$Airport == 138] <- "LGA"

table_summary %>%
  kbl(caption = "Table III: Summary Statistics by Departure Location and Trip Type") %>%
  kable_classic(full_width = F, html_font = "Georgia")







