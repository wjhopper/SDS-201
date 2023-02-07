library(httr)
library(rvest)
library(tidyr)
library(magrittr)
library(dplyr)

min_date <- as.Date("2003-01-01", format="%Y-%m-%d")
min_year <- as.numeric(format(min_date, "%Y"))
min_month <- as.numeric(format(min_date, "%m"))
min_day <- as.numeric(format(min_date, "%d"))
  
current_date <- Sys.Date()
current_year <- as.numeric(format(current_date, format="%Y"))
current_month <- as.numeric(format(current_date, format="%m"))
current_day <- as.numeric(format(current_date, format="%d"))

if (current_month==1) {
  max_month <- 12
  max_year <- current_year - 1
} else {
  max_month <- current_month-1
  max_year <- current_year
}

## Figure out the maximum day of the previous month by computing days elapsed between
## first day of current month and first day of previous months
## HT: Dirk Eddelbuettel https://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-r

max_day <- as.numeric(difftime(paste(current_year, current_month, "01", sep="-"),
                               paste(max_year, max_month, "01", sep="-"),
                               )
                      )

# number_of_months <- max_month + ((max_year - min_year) * 12)
months_to_request <- expand_grid(year = min_year:max_year,
                                 month = 1:12
                                 ) %>%
  filter(year < max_year | (month <= max_month & year == max_year))


data_frames <- vector(mode="list", length=nrow(months_to_request))

for (i in 1:nrow(months_to_request)) {
  
  req_month <- months_to_request$month[i]
  req_year <- months_to_request$year[i]
  
  ## post request, receive raw HTML
  r <- POST("https://www.peacebridge.com/traffic.php",
            body = list(view="monthly", process="View",
                        month=req_month, year=req_year
                        ),
            encode = "form"
            )
  
  if (r$status_code==200) {
    
    cat(paste0("Processing ", req_month, "/", req_year, "\n"))
    
    ## extract second tables from page, convert to data frame
    raw_data_table <- r %>%
      read_html() %>%
      html_nodes("table") %>%
      magrittr::extract2(2) %>%
      html_table(na.strings = c("NA",""))
    
    ## raw table uses nested columns, so variable names are a combination
    ## of first and second rows. So we paste them, together ignoring empty cells
    varnames <- sapply(raw_data_table[1:2, ],
                       FUN = function(x) {paste(x[!is.na(x)], collapse="_")}
                       )
    
    ## Reshape into tidy layout, discarding rows/columns with marginal totals
    tidy_table <- raw_data_table[-c(1:2), ] %>%
      setNames(varnames) %>% 
      select(-Total) %>%
      filter(!is.na(Day)) %>%
      gather("var", "volume", -Day) %>%
      separate(var, into=c("direction", "vehicle"), sep="_") %>%
      mutate(direction = substr(direction, 1, 4),
             volume = readr::parse_number(volume)
      ) %>%
      mutate()
    
    data_frames[[i]] <- tidy_table
  
  } else {
    data_frames[[i]] <- NA
    warning(paste0("Request failed for ", req_month, "/", req_year))
  }
}

months_to_request$data <- data_frames
months_to_request <- tidyr::unnest(months_to_request, data)

# East-bound bus traffic is included in east-bound auto totals, so we'll homogenize
# and do the same for westbound
months_to_request <- months_to_request %>%
  mutate(vehicle = sub("Bus", "Auto", vehicle, fixed=TRUE)) %>%
  group_by(year, month, Day, direction, vehicle) %>%
  summarize(volume = sum(volume))

## Compute volume total for each year, month, and vehicle type
## So, we're summing over day and direction here
monthly_volumes <- group_by(months_to_request, year, month, vehicle) %>%
  summarize(volume = sum(volume),
            .groups = "drop"
            ) %>%
  arrange(year, month)

write.csv(monthly_volumes, file="peacebridge_monthly_volumes.csv", row.names = FALSE)

# TEMP - Mean temperature (.1 Fahrenheit)
# DEWP - Mean dew point (.1 Fahrenheit)
# SLP - Mean sea level pressure (.1 mb)
# STP - Mean station pressure (.1 mb)
# VISIB - Mean visibility (.1 miles)
# WDSP – Mean wind speed (.1 knots)
# MXSPD - Maximum sustained wind speed (.1 knots)
# GUST - Maximum wind gust (.1 knots)
# MAX - Maximum temperature (.1 Fahrenheit)
# MIN - Minimum temperature (.1 Fahrenheit)
# PRCP - Precipitation amount (.01 inches)
# SNDP - Snow depth (.1 inches)
# FRSHTT – Indicator for occurrence of:
#   Fog		
#   Rain or Drizzle
#   Snow or Ice Pellets
#   Hail
#   Thunder
#   Tornado/Funnel Cloud

## weather data for buffalo
# weather <- read.csv("https://www.ncei.noaa.gov/access/services/data/v1?dataset=global-summary-of-the-day&startDate=2008-01-01&endDate=2021-12-31&dataTypes=FRSHTT,MXSPD,GUST,WDSP,PRCP,SLP,TEMP,DEWP&stations=99725499999&format=csv",
#                     na.strings = c("9999.9", "999.9")
#                     )
# 
# weather <- mutate(weather,
#                   year = as.numeric(substr(DATE, 1, 4)),
#                   month = as.numeric(substr(DATE, 6, 7))
#                   )
