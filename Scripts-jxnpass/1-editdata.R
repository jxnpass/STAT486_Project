library(tidyverse)
library(lubridate)

crimes <- read_csv("https://richardson.byu.edu/data_contest/2024/Crimes.csv")
weather <- read_csv("https://richardson.byu.edu/data_contest/2024/weather.csv")
holidays <- read_csv("https://richardson.byu.edu/data_contest/2024/holidays.csv")
fullMoons <- read_csv("https://richardson.byu.edu/data_contest/2024/full_moon.csv")

# View(crimes)
# View(weather)
# View(holidays)
# View(full_moon)

### Data Formation -----------

# Step 1:
# I want to make sure every day/time is the same class and format for when I join them together
crimes <- crimes %>%
  mutate(Time = format(as.POSIXct(Date, format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "America/Chicago"), "%H:%M"), 
         Date = as.Date(Date, format = "%m/%d/%Y %I:%M:%S %p", 
                                  tz = "America/Chicago")) 

full_moon <- full_moon %>%
  mutate(Date = as.Date(FullMoonDates, format = "%d %B %Y"))

weather <- weather %>% 
  mutate(sunrise = format(as.POSIXct(sunrise, format = "%m/%d/%Y %I:%M:%S %p", 
                                     tz = "America/Chicago"), "%H:%M:%S")) %>% 
  mutate(sunset = format(as.POSIXct(sunset, format = "%m/%d/%Y %I:%M:%S %p", 
                                     tz = "America/Chicago"), "%H:%M:%S"))

is(crimes$Date, "Date") & is(weather$datetime, "Date") & is(holidays$Date, "Date") & is(full_moon$Date, "Date")

# Step 2: 
# Combine data based on Date IDs

crimes_weather <- inner_join(crimes, weather, by = join_by(Date == datetime)) %>% 
  arrange(Date, Time)

# I also think that in crimes dataset, we want to specify which ones are at night, and which ones are during the day
# this should be according to weather's sunrise/sunset data

crimes_weather <- crimes_weather %>% 
  mutate(is_Night = ifelse(Time <= sunrise | Time >= sunset, T, F))


# Now to specify which crimes were committed during a full moon

# FIXME
crimes_weather <- crimes_weather %>% 
  mutate(is_FullMoon = 
           ifelse(((Date %in% full_moon$Date) & Time >= sunset) | 
                    ((Date %in% full_moon$Date+1) & Time <= sunrise), T, F)) 

crimes_weather$is_FullMoon <- NA
for (i in 1:nrow(crimes_weather)) {
  if (crimes_weather$Time[i] <= crimes_weather$sunrise[i] & crimes_weather$Date[i] %in% (full_moon$Date+1)) {
    crimes_weather$is_FullMoon[i] <- T
  }
  else if (crimes_weather$Time[i] >= crimes_weather$sunset[i] & crimes_weather$Date[i] %in% full_moon$Date) {
    crimes_weather$is_FullMoon[i] <- T
  }
  else {
    crimes_weather$is_FullMoon[i] <- F
  }
}

# I also want a month and year column
crimes_weather <- crimes_weather %>% 
  mutate(Day = day(Date),
         Month = month(Date),
         Year = year(Date))

# Finally, add holidays, because we can
crimes_full <- left_join(crimes_weather, holidays %>% 
                           select(-`Day of Week`), 
                         by = "Date") %>% 
  mutate(Holiday = ifelse(is.na(Holiday), "None", Holiday)) %>% 
  mutate(is_Holiday = ifelse(Holiday == "None", F, T))

# View(crimes_full)

# Step 3: Data imputation (For missing values)
which_na <- apply(X = is.na(crimes_full), MARGIN = 2, FUN = sum)
which_na[which_na > 0]

# precip type
crimes_full <- crimes_full %>% 
  mutate(preciptype = ifelse(is.na(preciptype), "None", preciptype))

# windgust
crimes_full <- crimes_full %>% 
  mutate(windgust = ifelse(is.na(windgust), mean(windgust, na.rm = T), windgust))

# severerisk
crimes_full <- crimes_full %>% 
  mutate(severerisk = ifelse(is.na(severerisk), 0, severerisk))

# Step 4: Feature engineering ---------

#Time Class
crimes_full <- crimes_full %>%
  mutate(Time_Class = case_when(Time < sunrise ~ "Before Dawn",
                                Time > sunset ~ "After Dusk",
                                (Time > sunrise) & (Time < sunset) ~ "Daytime")) 

# Optional: 6-hr interval

crimes_full <- crimes_full %>%
  mutate(Time_Interval = case_when(Time < "06:00:00" ~ "B1",
                                   (Time >= "06:00:00") & (Time < "12:00:00") ~ "B2",
                                   (Time >= "12:00:00") & (Time < "18:00:00") ~ "B3",
                                   (Time >= "18:00:00") & (Time <= "23:59:59") ~ "B4")) 
# according to above, can also fix is_fullmoon and is_night
crimes_full$is_FullMoon_BLOCK <- NA
for (i in 1:nrow(crimes_full)) {
  if (crimes_full$Time_Interval[i] == "B1" & crimes_full$Date[i] %in% (full_moon$Date+1)) {
    crimes_full$is_FullMoon_BLOCK[i] <- T
  }
  else if (crimes_full$Time_Interval[i] == "B4" & crimes_full$Date[i] %in% full_moon$Date) {
    crimes_full$is_FullMoon_BLOCK[i] <- T
  }
  else {
    crimes_full$is_FullMoon_BLOCK[i] <- F
  }
}

# Optional: Sun hours in day

crimes_full <- crimes_full %>%
  mutate(suntime = as.POSIXct(sunset, format = "%H:%M:%S") - as.POSIXct(sunrise, format = "%H:%M:%S")) %>%
  mutate(suntime = as.numeric(suntime))

# Optional, fix "Community Area"
# ????????


# Step 5: Write out to new dataframe 

write.csv(crimes_full, file = "Data/cleaned_data.csv")


