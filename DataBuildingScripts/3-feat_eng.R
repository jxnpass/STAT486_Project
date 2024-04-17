library(tidyverse)

crime_num <- read_csv("Data/crime_num.csv")[,-1]
crime_violent <- read_csv("Data/crime_violent.csv")[,-1]

# Feature Engineering Pt 1 ------------------------------------------------

## NOTE: All of the feature engineering was done in 1-editdata.R, and little in 2-eda.R. 
# I will discuss some of these new variables here as well

crime_num$suntime
# number of hours from sunrise to sunset

crime_num$Time_Interval
# 6-hr blocks within the day. B1 = midnight to 6 am, B2 = 6 am to noon, etc.

crime_num$Num_Crimes
# the chosen response, based off of number of crimes committed within day + time interval

crime_num$TS_interval
# basically an index column

crime_num$is_FullMoon_BLOCK
# if its B1 or B4, is the fullmoon out (binary)

crime_num$Holiday
# name of holiday

crime_num$NationalIncident
# was a national incident recorded for this date (binary)

# Feature Engineering Pt 2 ------------------------------------------------

# convert temperatures from C to F
CtoF <- function(temps){
  new_temps = temps * (9/5) + 32 
  return(new_temps)
}

crime_num$temp <- CtoF(crime_num$temp)
crime_num$tempmax <- CtoF(crime_num$tempmax)
crime_num$tempmin <- CtoF(crime_num$tempmin)
crime_num$feelslike <- CtoF(crime_num$feelslike)
crime_num$feelslikemax <- CtoF(crime_num$feelslikemax)
crime_num$feelslikemin <- CtoF(crime_num$feelslikemin)

crime_violent$temp <- CtoF(crime_violent$temp)
crime_violent$tempmax <- CtoF(crime_violent$tempmax)
crime_violent$tempmin <- CtoF(crime_violent$tempmin)
crime_violent$feelslike <- CtoF(crime_violent$feelslike)
crime_violent$feelslikemax <- CtoF(crime_violent$feelslikemax)
crime_violent$feelslikemin <- CtoF(crime_violent$feelslikemin)

# did it rain or not?
crime_num$precipprob <- ifelse(crime_num$precipprob == 100, 1, 0)
crime_violent$precipprob <- ifelse(crime_violent$precipprob == 100, 1, 0)

# This was more of a section that handled scaling and variable reorganizing

# now dropping certain vars because I have more descriptive (however I am keeping fullmoon block)
crime_num <- crime_num %>% select(-sunrise, -sunset, -moonphase)
crime_violent <- crime_violent %>% select(-sunrise, -sunset, -moonphase)

# these all have to do with cloud cover
crime_num <- crime_num %>% select(-description, -icon, -conditions)
crime_violent <- crime_violent %>% select(-description, -icon, -conditions)

write_csv(file = "Data/FE/crime_num_fe.csv", x = crime_num)
write_csv(file = "Data/FE/crime_violent_fe.csv", x = crime_violent)













