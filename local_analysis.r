# Description   : Analysis of google location data in R.
# Authour       : Shane Whelan
# reference     : https://shiring.github.io/maps/2016/12/30/Standortverlauf_post 
#                 & https://www.cultureofinsight.com/blog/2018/01/31/2018-01-31-map-your-google-location-data-with-r-shiny/
#                 & https://rpubs.com/robchavez/60451
# documentation : wheeldogg.github.com
# date          : 23-07-2018

# devtools::install_github("dkahle/ggmap")
library(dplyr)
library(jsonlite)
library(lubridate)
library(ggmap)
library(raster)
library(zoo)
system.time(x <- fromJSON("/Users/wheeldogg/workspace/projects/Takeout/new/Takeout/Location History/LocationHistory.json"))

###-----------###
### Tidy data ### 
###-----------###
# extracting the locations dataframe
loc = x$locations
# converting time column from posix milliseconds into a readable time scale
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")
# converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7
head(loc)

###------------###
### Some Stats ### 
###------------###
# How many data points did Google record over what period of time?
# how many rows are in the data frame?
nrow(loc)
## [1] 642252
min(loc$time)
## [1] "2013-07-15 22:48:34 GMT"
max(loc$time)
## [1] "2018-07-23 16:03:06 GMT"
loc$date <- as.Date(loc$time, '%Y/%m/%d')
loc$year <- year(loc$date)
loc$month_year <- as.yearmon(loc$date)

points_p_day <- data.frame(table(loc$date), group = "day")
points_p_month <- data.frame(table(loc$month_year), group = "month")
points_p_year <- data.frame(table(loc$year), group = "year")

# How many days were recorded?
nrow(points_p_day)
## [1] 1383
nrow(points_p_month)
## [1] 49
# And how many years?
nrow(points_p_year)
## [1] 6

# Make some plots.
# set up plotting theme
library(ggplot2)
library(ggmap)

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      # axis.title.x=element_blank(),
      # axis.text.x=element_blank(),
      # axis.ticks.x=element_blank(),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "navy"),
      legend.position = "right",
      legend.background = element_blank(),
      panel.spacing = unit(.5, "lines"),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

points <- rbind(points_p_day[, -1], points_p_month[, -1], points_p_year[, -1])

ggplot(points, aes(x = group, y = Freq)) + 
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) + 
  geom_boxplot(aes(color = group), size = 1, outlier.colour = NA) + 
  facet_grid(group ~ ., scales = "free") + my_theme() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "",
    y = "Number of data points",
    title = "Quantity of my Google data points",
    subtitle = "Number of data points per day, month and year",
    caption = "\nGoogle collected between 0 and 1500 data points per day
    (median ~500), between 0 and 40,000 per month (median ~15,000) and 
    between 80,000 and 220,000 per year (median ~140,000)."
  )

# Construct GPS coordinates on maps.
NZ <- get_map(location = 'New Zealand', zoom = 5)
ggmap(NZ) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in New Zealand",
    caption = "\nA simple point plot shows recorded positions.")

IRE <- get_map(location = 'Ireland', zoom = 5)
ggmap(IRE) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Ireland",
    caption = "\nA simple point plot shows recorded positions.")

# Accuracy of recordings in Cork.
Cork <- get_map(location = 'Cork', zoom = 12)
# options(stringsAsFactors = T)
ggmap(Cork) +
  stat_summary_2d(geom = "tile", bins = 100, data = loc, aes(x = lon, y = lat, z = accuracy), alpha = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", guide = guide_legend(title = "Accuracy")) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Location history data points around Cork",
    subtitle = "Color scale shows accuracy (low: blue, high: red)",
    caption = "\nThis bin plot shows recorded positions
    and their accuracy in and around Cork")

# And velocity
loc_2 <- loc[which(!is.na(loc$velocity)), ]
Cork <- get_map(location = 'Cork', zoom = 10)
ggmap(Cork) + geom_point(data = loc_2, aes(x = lon, y = lat, color = velocity), alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "Longitude", y = "Latitude",
       title = "Location history data points in Cork",
       subtitle = "Color scale shows velocity measured for location",
       caption = "\nA point plot where points are colored means that my speed was slower") +
  scale_colour_gradient(low = "blue", high = "red", guide = guide_legend(title = "Velocity"))

# What distance did I travel.
loc3 <- with(loc, subset(loc, loc$time > as.POSIXct('2018-01-01 0:00:01')))
loc3 <- with(loc, subset(loc3, loc$time < as.POSIXct('2018-12-22 23:59:59')))

# Shifting vectors for latitude and longitude to include end position
shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

loc3$lat.p1 <- shift.vec(loc3$lat, -1)
loc3$lon.p1 <- shift.vec(loc3$lon, -1)
head(loc3)

# Convert southern hemisphere to positive.
# loc3$lat    <- abs(loc3$lat)
# loc3$lon    <- abs(loc3$lon)
# loc3$lat.p1 <- abs(loc3$lat.p1)
# loc3$lon.p1 <- abs(loc3$lon.p1)

# Calculating distances between points (in metres) with the function pointDistance from the 'raster' package.
loc3$dist.to.prev <- apply(loc3, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
                  as.numeric(as.character(row["lon.p1"]))),
                c(as.numeric(as.character(row["lat"])), 
                  as.numeric(as.character(row["lon"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})

# distance in km
round(sum(as.numeric(as.character(loc3$dist.to.prev)), na.rm = TRUE)*0.001, digits = 2)
## [1] 27149.42
tail(loc3)
nrow(loc3)
head(loc3)
# convert negative to plus
loc3$dist.to.prev <- abs(loc3$dist.to.prev)
# # remove NAS
loc3$dist.to.prev[is.na(loc3$dist.to.prev)] <- 0
# head(loc3)

# distance_p_month <- loc3 %>% 
  # group_by(month_year) %>% 
  # summarise(dist.to.prev = sum(dist.to.prev))
# alternatively for group by.
head(loc3)
# distance_p_month <- aggregate(loc3$dist.to.prev, by = list(month_year =
                                                             # as.factor(loc3$month_year)), FUN = sum)
distance_p_month <- aggregate(loc3$dist.to.prev, by = list(date =
                                                             as.factor(loc3$date)), FUN = sum)
distance_p_month$x <- distance_p_month$x*0.001

#
head(distance_p_month)
distance_p_month
# filter out less than 1
distance_p_month <- filter(distance_p_month, x > 1)

ggplot(distance_p_month[-1, ], aes(x = date, y = x,  fill = date)) + 
  geom_bar(stat = "identity")  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    y = "Distance in km",
    title = "Distance traveled per day since June in 2018"
    # caption = "This barplot shows the sum of distances between recorded 
    # positions for 2018. In May/June we travelled around Australia and back to Ireland"
  )

# Activities
# Google also guesses my activity based on distance travelled per time.
# Here again, it would take too long to look at activity from all data points.
activities <- loc3$activity
list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]
head(activities)
class(activities)
df <- do.call("rbind", activities)
head(df)
# View(head(df))
head(df$activities)

main_activity <- sapply(df$activity, function(x) x[[1]][1][[1]][1])
head(main_activity)

# Take the main activity for each data point.
activities_2 <- data.frame(main_activity = main_activity, 
                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, 
                                             origin = "1970-01-01"))

tail(activities_2)
##   main_activity                time
## 1         still 2016-12-22 08:52:45
## 2         still 2016-12-22 08:46:57
## 3         still 2016-12-22 08:33:24
## 4         still 2016-12-22 08:21:31
## 5         still 2016-12-22 08:15:32
## 6         still 2016-12-22 08:10:25
ggplot(activities_2, aes(x = main_activity, group = main_activity, fill = main_activity)) + 
  geom_bar()  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    x = "",
    y = "Count",
    title = "Main activities in 2018",
    caption = "Associated activity for recorded positions in 2016. 
    Because Google records activity probabilities for each position, 
    only the activity with highest likelihood were chosen for each position."
  )
