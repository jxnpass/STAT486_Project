library(tidyverse)
library(sf)
library(mapview)
library(leafpop)
library(forecast)
library(acp)
library(gridExtra)
library(corrplot)

crime <- read_csv("Data/cleaned_data.csv") %>% 
  select(-1) %>% 
  rename(Type = `Primary Type`) %>% 
  filter(!is.na(Time_Interval)) %>% 
  arrange(Date, Time)

# cbind to anamoly/incident point
US_tragedies <- read.csv("Data/nationalincidents.csv")[,-1] %>% 
  mutate(Date = as.Date(Date))

# Spatial EDA -------------------

# Assuming 'crime' is your dataset and it has a 'Year' column
crime_loc <- crime %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  filter(Longitude > -87.76) %>%
  mutate(Latitude = round(Latitude, digits = 3),
         Longitude = round(Longitude, digits = 3),
         Year = as.factor(Year)) %>%
  group_by(Latitude, Longitude, Violent, Year) %>%
  count() %>%
  ungroup() %>%
  # Calculate total crimes and proportion of violent crimes per location per year
  group_by(Latitude, Longitude, Year) %>%
  summarise(total_crimes = sum(n),
            violent_crimes = sum(n[Violent == 1]),
            prop_violent = violent_crimes / total_crimes,
            .groups = 'drop') %>%
  # Optionally, add a row for summing all years
  bind_rows(
    crime_loc %>%
      group_by(Latitude, Longitude) %>%
      summarise(Year = "All Years",
                total_crimes = sum(total_crimes),
                violent_crimes = sum(violent_crimes),
                prop_violent = violent_crimes / total_crimes,
                .groups = 'drop')
  )

# Violent Crime Prop
mapview(crime_loc, xcol = "Longitude", ycol = "Latitude", zcol = "prop_violent",
        col.regions = colorRampPalette(c("blue", "red"))(500), 
        crs = 4269, grid = FALSE,
        layer.name = "Violent Crime Rates")

# Num crimes
crime_loc$scaled_crimes <- sqrt(crime_loc$total_crimes) / max(sqrt(crime_loc$total_crimes)) * 200  # scale and cap size
mapview(crime_loc, xcol = "Longitude", ycol = "Latitude", zcol = "total_crimes",
        col.regions = colorRampPalette(c("green3", "maroon2"))(500), 
        cex = "scaled_crimes",  # Use scaled crimes for size
        alpha.regions = 1,    # Optional: set transparency of points
        crs = 4269, grid = FALSE,
        layer.name = "Overall Crime Rates")


# Temporal EDA ------------------------------------------------------------

# Regression: Number of crimes per day, poisson response
group_vars <- c("Date", "Day", "Month", "Year", "Time_Interval")
cols_interest <- c(group_vars, "tempmax" ,    "tempmin"      ,     "temp"  ,  
                   "feelslikemax"     ,    "feelslikemin"    ,     "feelslike"         ,  
                   "dew"              ,    "humidity"        ,     "precip"            ,  
                   "precipprob"       ,    "precipcover"     ,     "preciptype"        ,  
                   "snow"             ,    "snowdepth"       ,     "windgust"          ,  
                   "windspeed"        ,    "winddir"         ,     "sealevelpressure"  ,  
                   "cloudcover"       ,    "visibility"      ,     "solarradiation"    ,  
                   "solarenergy"      ,    "uvindex"         ,     "severerisk"        ,  
                   "sunrise"          ,    "sunset"          ,     "moonphase"         ,  
                   "conditions"       ,    "description"     ,     "icon"              ,  
                   "is_FullMoon_BLOCK",    "suntime",              'Holiday') 
cols_interest <- c(group_vars, "temp"  ,   "feelslike"         ,  
                   "dew"              ,    "humidity"        ,     "precip"            ,  
                   "snow"             ,   "windgust"          ,    "winddir"         ,     
                   "cloudcover"       ,   "severerisk"        ,  
                   "is_FullMoon_BLOCK",    "suntime",              'Holiday') 

crime_num <- crime %>% 
  group_by_at(vars(cols_interest)) %>% 
  summarise(Num_Crimes = n()) %>% 
  ungroup() %>% 
  complete(Date, nesting(Time_Interval), fill = list(Num_Crimes = 0), explicit = T) %>% 
  fill(everything(), .direction = "down") %>% 
  arrange(Date, Time_Interval)

TS_interval <- rep(1:(nrow(crime_num)))

# for easy index col
crime_num <- crime_num %>% 
  mutate(TS_interval = TS_interval) %>% 
  select(Date, TS_interval, Time_Interval, Num_Crimes, everything())

# cbind to national incident
crime_num <- crime_num %>% 
  mutate(NationalIncident = ifelse(Date %in% US_tragedies$Date, 1, 0)) 

# for violent counts
crime_num_violent <- crime %>% 
  filter(Violent == 1) %>% 
  group_by_at(vars(cols_interest)) %>% 
  summarise(Num_Crimes = n()) %>% 
  ungroup() %>% 
  complete(Date, nesting(Time_Interval), fill = list(Num_Crimes = 0), explicit = T) %>% 
  fill(everything(), .direction = "down") %>% 
  arrange(Date, Time_Interval)

crime_num_violent <- crime_num_violent %>% 
  mutate(TS_interval = TS_interval) %>% 
  select(Date, TS_interval, Time_Interval, Num_Crimes, everything())

# cbind to national incident
crime_num_violent <- crime_num_violent %>% 
  mutate(NationalIncident = ifelse(Date %in% US_tragedies$Date, 1, 0)) 

# for this graph only, makes it easy to see how much crime is being violent along with num crimes
crime_scale_violent <- crime_num_violent %>% 
  mutate(prop_Num_Crimes = Num_Crimes / (crime_num$Num_Crimes)) %>% 
  mutate(prop_Num_Crimes = ifelse(is.nan(prop_Num_Crimes), 0, prop_Num_Crimes))

# Plot for total crime count
p1 <- ggplot(crime_num, aes(x = Date, y = Num_Crimes)) +
  geom_line(color = "gray10") +
  labs(x = NULL, y = "Total Crimes") +
  theme_light() +
  theme(plot.margin = margin(1, 1, 0.5, 1, "cm")) + 
  labs(title = "Chicago Crime Rates From 2010-2023")

# Find the maximum value of Num_Crimes and its corresponding Date
max_crime <- crime_num[which.max(crime_num$Num_Crimes), ]
tragedy_dates <- crime_num %>% 
  right_join(., US_tragedies, by = "Date") %>% 
  group_by(Date, Title) %>% 
  summarize(max_crimes = max(Num_Crimes))
p1 <- p1 +
  annotate("point", x = max_crime$Date, y = max_crime$Num_Crimes, shape = 4, size = 4, color = "red") +
  annotate("text", x = max_crime$Date, y = max_crime$Num_Crimes, label = "George Floyd Protest", vjust = .2, hjust = -.1, color = "black") +
  annotate("point", x = tragedy_dates$Date, y = tragedy_dates$max_crimes, shape = 4, size = 3, color = "red")

# Plot for violent crime proportion
p2 <- ggplot(crime_scale_violent, aes(x = Date, y = prop_Num_Crimes)) +
  geom_smooth(color = "red", method = "loess", span = .1, se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Violent %") +
  theme_light() +
  theme(plot.margin = margin(0.5, 1, 1, 1, "cm"))

# Arrange the two plots vertically with different relative heights
pfinal <- grid.arrange(p1, p2, heights = c(3, 1.5)) 
ggsave("Graphs/violentbynumcrimeplot.png", pfinal, width = 8, height = 6, units = 'in')

# Correlation matrix ----------------------

holidays <- crime_num$Holiday %>% unique()
holidays <- c(holidays[2], holidays[-2])

crime_cor_mat <- crime_num %>% 
  select(Num_Crimes, Time_Interval, Month, temp:NationalIncident) %>% 
  mutate(Holiday = factor(Holiday, levels = holidays)) %>% 
  mutate(Holiday = as.integer(Holiday)) %>% 
  mutate(Time_Interval = as.factor(Time_Interval)) %>% 
  mutate(Time_Interval = as.integer(Time_Interval)) %>% 
  rename('FullMoon' = is_FullMoon_BLOCK)

cp1 <- corrplot(cor(crime_cor_mat), 
                method = "color", 
                addCoef.col="grey", 
                number.cex = .5,
                title = "Correlation Plot of Crime Number and Other Features (Post-Engineering)",
                mar=c(0,0,5,0))

write.csv(crime_num, file = "Data/crime_num.csv")
write.csv(crime_num_violent, file = "Data/crime_violent.csv")

