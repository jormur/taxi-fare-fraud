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
# taxi <- read_parquet("sample_data.parquet")
taxi <- read_parquet("sample_data_big.parquet")

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



### DETOUR INDEX ###
# !!!! RUN THE DETOUR INDEX SCRIPT FIRST !!!!
#Join the detour indices from result_df to the main taxi data, matching based on "VendorID", "tpep_pickup_datetime", "PULocationID" and "DOLocationID". 
#This would add the detour_distance, detour_duration, and detour_fare columns to the main taxi data
taxi <- taxi %>%
  left_join(result_df, by = c("VendorID", "tpep_pickup_datetime", "PULocationID", "DOLocationID"))



### ESTIMATION PREPARATION ###
#Make tripID numeric
taxi <- taxi %>%
  mutate(tripID = as.numeric(tripID),
         pred_occ = as.numeric(pred_occ))

#Drop duplicates for tripID and dropoff time
taxi <- taxi %>%
  distinct(tripID, tpep_dropoff_datetime, .keep_all = TRUE)

#Create a date variable as a categorical variable denoting the day out of the entire data period (1,2,3,etc)
# taxi <- taxi %>%
#   mutate(date = as.numeric(as.Date(tpep_pickup_datetime) - as.Date("2011-01-01")))

#Create a date variable from the datetime of dropoff
taxi <- taxi %>%
  mutate(date = as.Date(tpep_dropoff_datetime))


#Export the taxi data to a stata dta file
# library(haven)
# write_dta(taxi, "taxi.dta")

#Drop records where the detour distance, detour duration, or detour fare are in the lowest 1% or highest 1% of their respective distributions
#This is done to remove outliers, excluding NaN values
taxi <- taxi %>%
  filter(detour_distance > quantile(detour_distance, 0.01, na.rm = TRUE) & detour_distance < quantile(detour_distance, 0.99, na.rm = TRUE)) %>%
  filter(detour_duration > quantile(detour_duration, 0.01, na.rm = TRUE) & detour_duration < quantile(detour_duration, 0.99, na.rm = TRUE)) %>%
  filter(detour_fare > quantile(detour_fare, 0.01, na.rm = TRUE) & detour_fare < quantile(detour_fare, 0.99, na.rm = TRUE))




#### "DiD" IMPLEMENTATION ####
#The following seeks to replicate Table V from the paper
#We will estimate each column of the table separately to fall in line wit the empirical specification
library(fixest)

# Column 1: LGA Distance
lga_d <- taxi %>%
  filter(PULocationID == 138) %>%
  feols(detour_distance ~  pred_occ + treatment + treatment*pred_occ | tripID + date, 
        cluster = "tripID")

# summary(lga_d)

# Column 2: LGA Duration
lga_t <- taxi %>%
  filter(PULocationID == 138) %>%
  feols(detour_duration ~  pred_occ + treatment + treatment*pred_occ | tripID + date, 
        cluster = "tripID")

# summary(lga_t)

# Column 3: LGA Fare
lga_f <- taxi %>%
  filter(PULocationID == 138) %>%
  feols(detour_fare ~  pred_occ + treatment + treatment*pred_occ | tripID + date, 
        cluster = "tripID")

# summary(lga_f)

# Column 4: JFK Distance
jfk_d <- taxi %>%
  filter(PULocationID == 132) %>%
  feols(detour_distance ~  pred_occ + treatment + treatment*pred_occ | tripID + date, 
        cluster = "tripID")

# summary(jfk_d)

# Column 5: JFK Duration
jfk_t <- taxi %>%
  filter(PULocationID == 132) %>%
  feols(detour_duration ~ pred_occ + treatment + treatment*pred_occ | tripID + date, 
        cluster = "tripID")

# summary(jfk_t)

models <- list(lga_d, lga_t, lga_f, jfk_d, jfk_t)

#Export
myDict <- c("pred_occ" = "occ",
            "treatment" = "g",
            "treatment:pred_occ" = "g x occ",
            "detour_distance" = "Distance",
            "detour_duration" = "Duration",
            "detour_fare" = "Fare",
            "tripID" = "Trip Unit",
            "date" = "Date",
            "PULocationID" = "Pickup Location",
            "DOLocationID" = "Dropoff Location",
            "tip_amount" = "Tip Amount",
            "local" = "Local dummy",
            "fare_amount" = "Fare Amount",
            "trip_distance" = "Trip Distance",
            "trip_duration" = "Trip Duration",
            "passenger_count" = "Passenger Count",
            "hour_of_day_by_day_of_week" = "Hour of Week")

etable(lga_d, lga_t, lga_f, jfk_d, jfk_t, 
       dict = myDict,
       title = "Estimations Results of Equation (1)",
       headers = c("LGA Distance", "LGA Duration", "LGA Fare", "JFK Distance", "JFK Duration"),
       # file = "tableV.tex",
       # replace = TRUE,
       view = TRUE,
       depvar = FALSE)




########### EXTENSION ############
#Create a dummy variable for whether the trip is done by a local
taxi <- taxi %>%
  mutate(local = ifelse(DOLocationID == 229, 1, 0))

#OLS
# Column 1: LGA
ols_lga <- taxi %>%
  filter(PULocationID == 138) %>%
  lm(tip_amount ~ local + trip_duration + trip_distance + fare_amount + passenger_count, data = .)

# Column 2: JFK
ols_jfk <- taxi %>%
  filter(PULocationID == 132) %>%
  lm(tip_amount ~ local + trip_duration + trip_distance + fare_amount + passenger_count, data = .)

#PLM
library(plm)

# Column 3: LGA
plm_lga <- taxi %>%
  filter(PULocationID == 138) %>%
  plm(tip_amount ~ local + trip_duration + trip_distance + fare_amount + passenger_count + hour_of_day_by_day_of_week, data = ., index = c("date"), model = "within")

# Column 4: JFK
plm_jfk <- taxi %>%
  filter(PULocationID == 132) %>%
  plm(tip_amount ~ local + trip_duration + trip_distance + fare_amount + passenger_count + hour_of_day_by_day_of_week, data = ., index = c("date"), model = "within")

#TWFE
library(estimatr)

# Column 5: LGA
twfe_lga <- taxi %>%
  filter(PULocationID == 138) %>%
  lm_robust(tip_amount ~ local + trip_duration + trip_distance + fare_amount + passenger_count + hour_of_day_by_day_of_week,
            fixed_effects = ~ date,
            data = .,
            se_type = "stata")

# Column 6: JFK
twfe_jfk <- taxi %>%
  filter(PULocationID == 132) %>%
  lm_robust(tip_amount ~ local + trip_duration + trip_distance + fare_amount + passenger_count + hour_of_day_by_day_of_week,
            fixed_effects = ~ date,
            data = .,
            se_type = "stata")

model_ext <- list("LGA OLS" = ols_lga, "JKF OLS" = ols_jfk, "LGA PLM" = plm_lga,
                  "JFK PLM" = plm_jfk, "LGA TWFE" = twfe_lga, "JFK TWFE" = twfe_jfk)

#Table Export
library(modelsummary)
library(kableExtra)
modelsummary(model_ext, 
             stars = TRUE,
             coef_map = myDict,
             title = "Estimations Results of Equation (2) with Robustiness Checks",
             output = "ext_results.tex"
)
