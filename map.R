library(dplyr)
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv")
library(ggplot2)
library(maps)
library(mapproj)

Asian_jail_mean_pop_rate <- prison %>%
  filter(!is.na(aapi_jail_pop_rate)) %>%
  group_by(county_name) %>%
  summarise(mean_pop_rate=mean(aapi_jail_pop_rate)) %>%
  mutate(county = tolower(gsub("\\s.*", "", county_name))) 

WA_map <- map_data("county") %>%
  filter(region == "washington") %>%
  rename(county = subregion) %>%
  left_join(Asian_jail_mean_pop_rate, by = "county")

ggplot(data = WA_map, mapping = aes(x = long, y = lat, group = group, fill = mean_pop_rate))+
  geom_polygon(color="white")+
  scale_fill_continuous(low = "lightblue", high = "darkblue") +
  coord_map()+
  theme_minimal()+
  labs(title = "The distribution of Asian population rate by countries", 
       fill = "The Asian mean jail 
       population rate", 
       x = "Longitude", 
       y = "Latitude") 