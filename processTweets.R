library(dplyr)
library(lubridate)
library(streamR)

# Coordinates for USA
usa <- c(-124.848974,24.396308,66.53076,49.23037)
filterStream(file.name = "dat/sunriseTrack-7h.json", oauth = my_oauth, 
             locations = usa, timeout = 25200) # 7 hours
parsed <- parseTweets("dat/sunriseTrack-7h.json")
parsed <- tbl_df(parsed)
saveRDS(parsed, "dat/sunriseTweetsDF-7h.rds")

# Grab the columns we want, filter out any NA entries, and do some mutations to get the structure we want
locations <- select(parsed, created_at, place_lat, place_lon, lat, lon) %>% 
      filter(!is.na(created_at)) %>%
      mutate(created_at = as.POSIXct(created_at, 
                                     format = "%a %b %d %H:%M:%S %z")) %>%
      rename(time = created_at) %>%
      mutate(time.by.min = format(time, 
                                  format = "%Y-%m-%d %H:%M %Z", tz = "America/New_York"))

# Some of the place_lat and place_lon values are NA, but there are entries for lat and lon. So, copy the lat/lon values to place_lat and place_lon
naInds <- which(is.na(locations$place_lat) & is.na(locations$place_lon))
for (i in naInds) {
            locations$place_lat[i] <- locations$lat[i]
            locations$place_lon[i] <- locations$lon[i]
}

# Get the number of tweets for each second; drop the first entry because it's usually not a full second
# tweets.by.second <- group_by(locations, time) %>%
#       summarize(entries = table(time)) %>%
#       arrange(time) %>%
#       slice(2:n())

# Get the number of tweets for each minute

tweets.by.minute <- group_by(locations, time.by.min) %>%
      summarize(entries = table(time.by.min)) %>%
      arrange(time.by.min) %>%
      slice(2:n())
