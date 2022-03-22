#' Varalika Jain
#' Project:
#' Familiarity Index - number of ravens present at the wildboar feeding

#------------------------------------------------------------------------------
####----(i) LOAD LIBRARIES----####
library(move)
library(lubridate)
library(dplyr)
library(sf)
library(tidyr)
library(chron)
library(ggplot2)
library(reshape)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(ii) DOWNLOAD DATA FROM MOVEBANK (MOVE OBJECT)----####
#' Permissions required to download data
#' Input movebank login details
login <- movebankLogin()

#' Source data from GPS tagged ravens
ravens <- getMovebankData(study="Corvus corax, Common Raven - Eastern Alps", 
                          login=login,
                          timestamp_start = "20200121000000000", 
                          timestamp_end = "20210121000000000",
                          removeDuplicatedTimestamps=TRUE)

#' Assigning the correct time zone (accounts for daylight savings)
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
####----(1) Creating columns:date & time----####

#' Check to see if timestamps are still in local time
head(ravens_df$timestamps)

# Create a column with the date 
ravens_df$date <- as.Date(ravens_df$timestamps)

# Create a column with the time 
ravens_df$time <- format(ravens_df$timestamps, format = "%H:%M:%S")

####----(2) Specify weekdays and weekends ----####
#' Create column for weekend and month & day
ravens_df$weekend <- is.weekend(ravens_df$date)
ravens_df$mmdd <- format(as.Date(ravens_df$date), "%m-%d")

#' Specify public holidays
hols <- c("01-01", "01-06", "04-02", "04-05", "05-01", "05-13", 
          "05-23", "05-24", "06-03",
          "08-15", "10-26", "11-01", "12-08", "12-25", "12-26")

ravens_df <- within(ravens_df, 
                        {holiday = ifelse(mmdd %in% hols, "TRUE", "FALSE") })

ravens_df$weekday <- ifelse(ravens_df$weekend == "FALSE" & 
                              ravens_df$holiday == "FALSE", 
                                "weekday", "weekend" )

####----(3) Selecting required columns----####
head(ravens_df)
ravens_filter <- ravens_df %>% dplyr::select(c("local_identifier", "location_lat", 
                                               "location_long", "timestamps", "date",
                                               "time", "weekday"))
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
# Filter according to certain times 
wb_wkdy_start <- format("08:00:00", format = "%H:%M:%S" )
wb_wkdy_end <-  format("10:00:00", format = "%H:%M:%S" )

wb_wknd_start <- format("09:00:00", format = "%H:%M:%S" )
wb_wknd_end <-  format("11:00:00", format = "%H:%M:%S" )

####----(2) Filter the data points according to the morning feeding time----####
ravens_filter$weekday <- as.factor(ravens_filter$weekday)
wkdy <- ravens_filter %>% filter(weekday == "weekday") %>%
  filter(time > wb_wkdy_start & time < wb_wkdy_end)

wknd <- ravens_filter %>% filter(weekday == "weekend") %>%
  filter(time > wb_wknd_start & time < wb_wknd_end)

morning_presence <- rbind(wkdy, wknd)

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
final_list$presence[is.na(final_list$presence)] <- as.character("data")

final_list_by_day <- final_list %>% 
  select(date, local_identifier, presence) %>%
  pivot_wider(names_from = date, values_from = presence)

final_list_by_day[is.na(final_list_by_day)] <- as.character("no data")

#' present = true presence 
#' absent = true absence
#' data = tracked on day, but not between 8/9am and 10/11am (equivalent to no data)
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

#' Filter out gps-tagged individuals
gps_indv <- as.vector(unique(final_list$local_identifier))

field_gps <- field %>% subset(as.character(individual) %in% as.character(gps_indv))

#' For columns with dates, if blank then paste absent, else present
field_gps[19:541] <- ifelse(field_gps[19:541] == "", "absent", "present")

#' Subset the data
field_gps <- field_gps[c(1, 19:541)]
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(viii) COMPARE THE FIELD DATA AND GPS DATA----####

####----(1) Convert both dataframes to long format for comparison----####

gps_list_long <- final_list_by_day %>% pivot_longer(!local_identifier, 
                                                      names_to = "date",
                                                      values_to = "presence_gps")
field_list_long <- field_gps %>% pivot_longer(!individual, 
                                              names_to = "date",
                                              values_to = "presence_field")

####----(2) Filter data to match dataframe with shortest date range----####
range(gps_list_long$date) 
range(field_list_long$date)

field_list_long <- field_list_long %>% filter(field_list_long$date >= min(gps_list_long$date),
                                              field_list_long$date <= max(gps_list_long$date))

####----(3) Merge the dataframes----####
#' NB: Here, we are only taking the dates where both GPS & field data are present
#' The reason all data are not included is because it is not possible to compare 
#' presences and absences from GPS to field data on days when field data weren't collected
#' To see a comparison to all the dates of data, include 'all = TRUE' in the merge function

merged_lists <- merge(gps_list_long, field_list_long, by.y = c("individual", "date"),
                      by.x = c("local_identifier", "date"))


####----(4) Filter out true presence and absence from GPS data----####
pres_abs <- merged_lists %>% filter(presence_gps == "absent" | 
                                              presence_gps == "present")
levels(as.factor(pres_abs$presence_gps))
levels(as.factor(pres_abs$presence_field))

####----(5) Familiarity index----####
#' NB The index is calculated for dates where true presence and absence in 
#' BOTH GPS and field data were collected

#' GPS data 
gps_index <- pres_abs %>% 
  group_by(local_identifier, presence_gps) %>% tally() %>% 
  select(local_identifier, presence_gps,n) %>%  
  pivot_wider(names_from = presence_gps, values_from = n)
gps_index[is.na(gps_index)] <- 0

head(gps_index)

#' Number of days present versus total number of days tracked 
#' (excl. days with no data btwn wb feeding hrs)
gps_index$gps_indx <- gps_index$present/(gps_index$present + gps_index$absent)

#' Field data 
field_index <- pres_abs %>%
  group_by(local_identifier, presence_field) %>% tally() %>% 
  select(local_identifier, presence_field,n) %>%  
  pivot_wider(names_from = presence_field, values_from = n)
field_index[is.na(field_index)] <- 0

head(field_index)

#' Number of days present versus total number of days when observations occurred
field_index$fld_indx <- field_index$present/(field_index$present + field_index$absent)

fam_index <- merge(gps_index, field_index, by = "local_identifier")
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(ix) EXPLORING QUESTIONS----####

####----(1) How do the indices compare to each other?----####
ggplot(fam_index, aes(x = gps_indx, y = fld_indx))+ 
  geom_point()+
  ylab("Field Index")+
  xlab("GPS Index") +
  theme_classic() + 
  theme(
    axis.title.y = element_text(color =" black", size=12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 11)
  )+geom_smooth()

cor.test(fam_index$gps_indx, fam_index$fld_indx)

####----(2) How do the indices compare per individual?----####
fam_index_melt <-fam_index %>% select(local_identifier, gps_indx, fld_indx)%>%
  melt(id=c("local_identifier"))

ggplot(fam_index_melt, aes(x = local_identifier, y = value, fill = variable))+ 
  geom_bar(stat= "identity", position = "dodge")+
  ylab("Familiarity Index")+
  xlab("Individual") +
  theme_classic() + 
  theme(
    axis.title.y = element_text(color =" black", size=12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 11)
  )

#' Field index is consistently higher than the gps index (also see axes in prev. plot)
#' For each indv, the field data reports higher FI values than GPS data
fam_index %>% summarise(gps_mean = mean(gps_indx), fld_mean = mean(fld_indx))
#' Percent diff
fam_index %>% summarise(gps_mean = mean(gps_indx), fld_mean = mean(fld_indx))%>%
  summarise(percent_diff = ((fld_mean - gps_mean)/((fld_mean+gps_mean)/2))*100)

####----(3) How many agreements/ disagreements are there between the data----####
pres_abs$identification <- paste0(pres_abs$presence_field,"-", pres_abs$presence_gps)
total_identification <- as.numeric(nrow(pres_abs))

agreements <- pres_abs %>% group_by(identification) %>% tally() %>% group_by(identification) %>%
  summarise(percent = n*100/total_identification)

agreements$agree <- c("agree", "disagree", "disagree", "agree")

#Percent
agreements %>% group_by(agree) %>% summarise(percent = sum(percent))

#' Disagreement-only comparison between field and GPS data

total_disagree <- as.numeric(nrow(pres_abs %>% filter(identification == "absent-present" |
                                                        identification == "present-absent")))

pres_abs %>% filter(identification == "absent-present" |
                      identification == "present-absent") %>% 
  group_by(identification) %>% tally() %>% group_by(identification) %>% 
  summarise(percent = n*100/total_disagree)
#' identification as 'field-gps'
#' first row: % of times field has recorded absent when gps recorded present
#' second row: % of times field has recorded present when gps recorded absent

pres_abs_melt <-pres_abs %>% filter(identification == "absent-present" |
                                      identification == "present-absent") %>%
  select(local_identifier, presence_gps, presence_field)%>%
  melt(id=c("local_identifier"))

ggplot(pres_abs_melt, aes(x = value, fill = variable))+ 
  geom_bar(position=position_fill(reverse=F))+
  ylab("Proportion")+
  xlab("Presence") +
  theme_classic() + 
  theme(
    axis.title.y = element_text(color =" black", size=12),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 11)
  )
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------