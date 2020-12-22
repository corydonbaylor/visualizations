
# first we are going to create a bounding box for virginia
# using lat longs

lat <- data.frame(lat = seq(36, 40, by = .06))
long <- data.frame(long = seq(-85, -74, by = .06))

# create a lat long dataframe
positions = lat %>% 
  merge(long, all = TRUE)

# this dataframe contains both the lat long and the votes by county
positions = positions %>% 
  
  # the map.where function returns the county given a lat long
  mutate(county = map.where('county', long, lat))%>%
  separate(county, c("state", "county"), sep = ",")%>%
  mutate(county_state = paste0(county, ", ", state))%>%
  mutate(county_state = gsub(":chincoteague|:main", "",  county_state))


# next we are going to filter to just virginia
virginia = positions%>%
  filter(state == "virginia")  

# using the lat longs from the virginia df, we are going to return 
# elevations

# NOTE: THIS WILL TAKE FOREVER, SAVE THE RESULTS
elevation = c()

for(i in 1:nrow(virginia)){
  print(i)
  
  temp = fromJSON(paste0("https://nationalmap.gov/epqs/pqs.php?x=", 
                         as.numeric(virginia[i,2]), "&y=", as.numeric(virginia[i,1]), "&units=Feet&output=json"))
  val = temp$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation
  elevation = append(elevation, val)
  
}

# saving the results as they are quite important
virginia2 = cbind(virginia, elevation)
write.csv(virginia2, "virginia.csv")

# next we are going to create an ID to join virginia back to 
# the bounding box
virginia2 = virginia2%>%
  mutate(id = paste0(lat, long))

positions = positions%>%
  mutate(id = paste0(lat, long))

final = positions%>%
  left_join(virginia2)

# finally, the plot itself. 
final%>%
  ggplot(aes(long, lat + .15*(elevation/max(elevation, na.rm=TRUE)))) + # this is the key line
  geom_line(size=0.7, alpha=0.8, color='white', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +
  coord_map()+
  theme(panel.background = element_rect(fill = "black"))

ggsave("virginia.png", width = 20, units="in")

