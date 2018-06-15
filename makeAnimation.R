library(ggplot2)
library(animation)
library(maps)

statemap <- map_data("state")

# By Second
# saveGIF ({
#       for(i in 1:nrow(tweets.by.second)) {
#             
#             # Add "cool down" for points using alpha
#             if (i > 1) {
#                   prev.match <- match
#                   match <- tweets.by.second[i,1]
#                   df <- filter(locations, time %in% c(match, prev.match)) %>%
#                         arrange(time)
#                   len.match <- nrow(filter(df, time %in% match))
#                   len.prevmatch <- nrow(filter(df, time %in% prev.match))
#                   alphalist <- c(rep(.3,len.prevmatch), rep(1,len.match))
#                   df <- ungroup(df) %>%
#                         mutate(alphaVal = alphalist)
#                   
#             } else {
#                   match <- tweets.by.second[i,1]
#                   df <- filter(locations, time %in% match) %>% 
#                         mutate(alphaVal = rep(1, n()))
#             }
#             
#             p1 <- ggplot(data = statemap, aes(x = long, y = lat, group = group)) +
#                   geom_polygon(fill = "#55acee", color = "white") + 
#                   coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50)) + 
#                   geom_point(data = df, aes(x = place_lon, y = place_lat), 
#                              alpha = df$alphaVal, size = 4, inherit.aes = FALSE, 
#                              color = "#292f33") + 
#                   annotate("text", label = df$time[1], x = -95, y = 20, size = 8) + 
#                   theme_void() + 
#                   theme(legend.position = "none")
#             
#             print(p1)
#       }
# }, interval = 0.02, ani.width = 1028, ani.height = 514, movie.name = "animate-tweets-600s.gif")

# Go through each individual time and make a data frame of each tweet. Then plot the location of that tweet in the US.

saveGIF ({
      for(i in 1:nrow(tweets.by.minute)) {
            
            # Add "cool down" for points using alpha
            if (i > 1) {
                  prev.match <- match
                  match <- tweets.by.minute[i,1]
                  df <- filter(locations, time.by.min %in% c(match, prev.match)) %>%
                        arrange(time.by.min)
                  len.match <- nrow(filter(df, time.by.min %in% match))
                  len.prevmatch <- nrow(filter(df, time.by.min %in% prev.match))
                  alphalist <- c(rep(.2,len.prevmatch), rep(.7,len.match))
                  df <- ungroup(df) %>%
                        mutate(alphaVal = alphalist)
                  
            } else {
                  match <- tweets.by.minute[i,1]
                  df <- filter(locations, time.by.min %in% match) %>% 
                        mutate(alphaVal = rep(1, n()))
            }
            
            p1 <- ggplot(data = statemap, aes(x = long, y = lat, group = group)) +
                  geom_polygon(fill = "#55acee", color = "white") + 
                  coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50)) + 
                  geom_point(data = df, aes(x = place_lon, y = place_lat), 
                             alpha = df$alphaVal, size = 3, inherit.aes = FALSE, 
                             color = "#292f33") + 
                  annotate("text", label = df$time.by.min[1], x = -95, y = 20, size = 8) + 
                  theme_void() + 
                  theme(legend.position = "none")
            
            print(p1)
      }
}, interval = 0.125, ani.width = 1028, ani.height = 514, movie.name = "sunrise-tweets-7h-8fps.gif")