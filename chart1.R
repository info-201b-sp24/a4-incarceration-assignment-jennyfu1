library(dplyr)
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv")
library(ggplot2)

jail_top5 <- prison %>%
  filter(!is.na(total_jail_pop_rate)) %>%
  group_by(county_name) %>%
  summarise(mean=mean(total_jail_pop_rate)) %>%
  arrange(desc(mean)) %>%
  slice_head(n=5)

jail_top5_data <- prison %>%
  filter(!is.na(total_jail_pop_rate)) %>%
  filter(county_name %in% c("Yakima County", "Douglas County", "Chelan County", "Franklin County", "Garfield County")) 



ggplot(data = jail_top5_data , mapping=aes(x=year, y=total_jail_pop_rate, color=county_name))+
  geom_line()+
  labs(title = "The trend over time chart", 
       subtitle = "year vs. total jail population rate", 
       x = "Year (from 1990 to 2018)", 
       y = "Total jail population rate (unit 0.01)")

