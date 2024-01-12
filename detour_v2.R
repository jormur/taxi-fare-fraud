#### Detour Index Construction ####
#Consolidated solution for Detour Index
library(dplyr)
library(lubridate)

# Assuming your dataset is named 'taxi'
taxi$tpep_pickup_datetime <- as.POSIXct(taxi$tpep_pickup_datetime)
taxi$tpep_dropoff_datetime <- as.POSIXct(taxi$tpep_dropoff_datetime)

# Filter for non-local passengers
nonlocal_trips <- taxi %>%
  filter(PULocationID %in% c("132", "138") & DOLocationID %in% c("163", "161", "162", "164"))

# Set time window (e.g., 15 mins)
time_window <- 15 * 60

# Define a function to calculate detour indexes for a given airport-hotel trip
calculate_detour_indexes <- function(trip, comparison_trips, time_window) {
  comparison_trips <- comparison_trips %>%
    filter(
      tpep_pickup_datetime >= (trip$tpep_pickup_datetime - minutes(time_window)) &
        tpep_pickup_datetime <= (trip$tpep_pickup_datetime + minutes(time_window)) &
        DOLocationID != trip$DOLocationID
    )
  
  avg_distance <- mean(comparison_trips$trip_distance)
  avg_duration <- mean(comparison_trips$trip_duration)
  avg_fare <- mean(comparison_trips$fare_amount)
  
  detour_indexes <- trip %>%
    summarise(
      detour_distance = trip_distance - avg_distance,
      detour_duration = trip_duration - avg_duration,
      detour_fare = fare_amount - avg_fare
    )
  
  return(detour_indexes)
}

# Apply the function to each airport-hotel trip
detour_indexes <- nonlocal_trips %>%
  group_by(across(c("VendorID", "tpep_pickup_datetime", "PULocationID", "DOLocationID"))) %>%
  do(calculate_detour_indexes(., nonlocal_trips, time_window))

# Combine the results into a data frame
result_df <- bind_rows(detour_indexes)




#### Index Stats ####

#View the mean of each detour index based on pickup location
detour_summary <- result_df %>%
  filter(!is.na(detour_distance) & !is.na(detour_duration) & !is.na(detour_fare)) %>%
  group_by(PULocationID) %>%
  summarise(
    avg_detour_distance = mean(detour_distance),
    avg_detour_duration = mean(detour_duration),
    avg_detour_fare = mean(detour_fare)
  )

#Rename PUlocationID to Airport
detour_summary <- detour_summary %>%
  rename(Airport = PULocationID)

#Change Airport values to JK and LGA
detour_summary$Airport[detour_summary$Airport == 132] <- "JFK"
detour_summary$Airport[detour_summary$Airport == 138] <- "LGA"

detour_summary %>%
  kbl(caption = "Table IV: Mean Detours by Airport") %>%
  kable_classic(full_width = F, html_font = "Georgia")


