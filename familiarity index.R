#' Varalika Jain
#' Project:
#' Familiarity Index - number of ravens present at the wildboar feeding

#------------------------------------------------------------------------------
####----(i) LOAD LIBRARIES----####
#' For better understanding of the script, information from the associated 
#' guides or vignettes of the following packages have also been included
library(move)
library(lubridate)
library(dplyr)
library(sf)
library(tidyr)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(ii) DOWNLOAD DATA FROM MOVEBANK (MOVE OBJECT)----####
#' Permissions required to download data
#' Input movebank login details
login <- movebankLogin()

#' Source data from GPS tagged ravens from 1st July 2017 to 15th March 2020
ravens <- getMovebankData(study="Corvus corax, Common Raven - Eastern Alps", 
                          login=login,
                          timestamp_start = "20200121000000000", 
                          timestamp_end = "20220302000000000",
                          removeDuplicatedTimestamps=TRUE)

#' Assigning the correct time zone
timestamps(ravens) <- with_tz(timestamps(ravens), tz="Europe/Vienna")

#' Check the timezone
head(timestamps(ravens))

#' Check individuals
levels(ravens@trackId)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(iii) CONVERT MOVE OBJECT TO DATAFRAME----####
ravens_df <- as(ravens, "data.frame")
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(iv) MODIFY DATAFRAME FOR REQUIRED INFO----####
####----(1) Creating columns: season & year & date----####

#' Check to see if timestamps are still in local time
head(ravens_df$timestamps)

# Create a column with the date 
ravens_df$date <- as.Date(ravens_df$timestamps)

# Create a column with the time 
ravens_df$time <- format(ravens_df$timestamps, format = "%H:%M:%S")

####----(2) Selecting required columns----####
head(ravens_df)
ravens_filter <- ravens_df %>% dplyr::select(c("local_identifier", "location_lat", 
                                               "location_long", "timestamps", "date",
                                               "time"))
head(ravens_filter)

#' Local identifier as factor
class(ravens_filter$local_identifier)
ravens_filter$local_identifier <- as.factor(ravens_filter$local_identifier)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(v) LIST OF RAVENS TRACKED PER DAY----####
daily_list <- ravens_filter %>%
  group_by(date, local_identifier) %>% summarise()
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(vi) GPS PRESENCE LIST AT WILDBOAR ENCLOSURE----####
####----(1) Wildboar morning feeding time----####
# Filter according to a certain time 
wb_feed_start <- format("08:00:00", format = "%H:%M:%S" )
wb_feed_end <-  format("10:00:00", format = "%H:%M:%S" )

####----(2) Filter the data points according to the morning feeding time----####
morning_presence <- ravens_filter %>%
  filter(time > wb_feed_start & time < wb_feed_end)

####----(3) Convert raven data into UTM projection---####
head(morning_presence)

presence_coords <- morning_presence[c("location_long", "location_lat", "timestamps", "local_identifier")]
coordinates(presence_coords) <- c("location_long", "location_lat")
proj4string(presence_coords) <- CRS("+proj=longlat +datum=WGS84")
#' The units for the radius are those of the (x,y) coordinates 
#' (e.g., meters in the case of a UTM projection)
presence_coords_utm <- spTransform(presence_coords, CRS("+proj=utm +zone=33 +datum=WGS84"))
presence_coords_utm <- as.data.frame(presence_coords_utm)
presence_coords_utm <- presence_coords_utm[c(3,4,1,2)] #long, lat, timestamp, id
head(presence_coords_utm)

####----(4) Convert raven data (UTM projection) into an sf object----####
presence_sf <- st_as_sf(x = presence_coords_utm, 
                      coords = c("location_long", "location_lat"),
                      crs = "+proj=utm +zone=33 +datum=WGS84")

####----(5) Convert location data (UTM projection) into an sf object----####
lat <- 47.80517
long <- 13.94868
wb_coords <- data.frame(long, lat)
coordinates(wb_coords) <- c("long", "lat")
proj4string(wb_coords) <- CRS("+proj=longlat +datum=WGS84")
wb_coords_utm <- spTransform(wb_coords, CRS("+proj=utm +zone=33 +datum=WGS84"))
wb_coords_utm <- as.data.frame(wb_coords_utm)
head(wb_coords_utm)


wb_sf <- st_as_sf(x = wb_coords_utm, 
                   coords = c("long", "lat"),
                   crs = "+proj=utm +zone=33 +datum=WGS84")

####----(6) Get the geom of location values----####
geom_wildboar = st_geometry(wb_sf)


####----(7) Set buffers----####
#' In UTM, so dist is in meters
wb_buf <- st_buffer(geom_wildboar, dist = 20)

####----(8) Calculating the intersection between fixes and buffers----####
#' For each GPS fix, calculate whether or not it intersects with any of the bufs
#' #' If it does not intersect, in the intersection column, paste " ", else "AFS"
intersect_dat <- presence_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, wb_buf))
  , location = if_else(is.na(intersection), "0", paste0("1"))) 

intersect_df <- as.data.frame(intersect_dat)

####----(9) Calculating the intersection between fixes and buffers----####
presence_intersect <- merge.data.frame(intersect_df, morning_presence, 
                                            by = c("local_identifier", "timestamps"))
presence_intersect$location <- as.integer(presence_intersect$location)

presence_list <- as.data.frame(presence_intersect) %>%
  select(date, local_identifier, location) %>%
  droplevels() %>%
  group_by(date, local_identifier) %>% tally(location)
presence_list$presence <- ifelse(presence_list$n == 0, "absent", "present")

####----(10) Merge daily and presence list----####

final_list <- merge.data.frame(daily_list, presence_list, by = c("date", "local_identifier"), all = TRUE)
final_list$presence[is.na(final_list$presence)] <- as.character("NA")

final_list_by_day <- final_list %>% 
  select(date, local_identifier, presence) %>%
  pivot_wider(names_from = date, values_from = presence)

final_list_by_day[is.na(final_list_by_day)] <- as.character("no data")

#' present = true presence 
#' absent = true absence
#' NA = tracked on day, but not between 8am and 10am
#' no data = no data on the day
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(vii) FIELD PRESENCE DATA----####
####----(1) Read in presence data from dropbox (2020 onwards used)----####
#' Renamed the file to presence_2020
field <- read.csv("presence_2020.csv", check.names  = FALSE)
head(field)

####----(2) Modify dataframe----####
#' Convert column names to date format
names(field)[19:541] <- format(as.Date(as.character(names(field)[19:541]),
                                           format = "%d/%m/%Y"), format = "%Y-%m-%d")
head(field)

#' Check names match
setdiff(field_gps$individual, final_list_by_day$local_identifier)

#' Filter out gps-tagged individuals
gps_indv <- as.vector(unique(final_list$local_identifier))

field_gps <- field %>% subset(as.character(individual) %in% as.character(gps_indv))

#' For columns with dates, if blank then paste absent, else present
field_gps[19:541] <- ifelse(field_gps[19:541] == "", "absent", "present")

#' Subset the data
field_gps <- field_gps[c(1, 19:541)]
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(viii) COMPARE FIELD AND PRESENCE DATA----####

####----(1) Convert both dataframes to long format for comparison----####
final_list_long <- final_list_by_day %>% pivot_longer(!local_identifier, 
                                                      names_to = "date",
                                                      values_to = "presence_gps")
field_list_long <- field_gps %>% pivot_longer(!individual, 
                                              names_to = "date",
                                              values_to = "presence_field")
####----(2) Merge the dataframes----####
merged_lists <- merge(field_list_long, final_list_long, by.x = c("individual", "date"),
                      by.y = c("local_identifier", "date"))

merged_lists$identification <- paste0(merged_lists$presence_field,"-", merged_lists$presence_gps)
ggplot(merged_lists, aes(x = identification)) + geom_bar()
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
