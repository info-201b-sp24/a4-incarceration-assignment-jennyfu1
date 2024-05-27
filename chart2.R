library(dplyr)
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv")
library(ggplot2)

King_County_data <- prison %>%
  filter(!is.na(total_pop), !is.na(total_jail_pop_rate)) %>%
  filter(county_name=="King County")

ggplot(data = King_County_data, mapping = aes(x=total_jail_pop_rate, y=white_jail_pop_rate))+
  geom_point()+
  geom_smooth(method="lm",color="red")+
  labs(title = "Variable comparison chart of King County", 
       subtitle ="total jail population rate vs. white people in jail population rate", 
       x = "Total jail population rate (unit 0.01)", 
       y = "White people in jail population rate (unit 0.01)")


Yakima_County_data <- prison %>%
  filter(!is.na(total_pop), !is.na(total_jail_pop_rate)) %>%
  filter(county_name=="Yakima County")

ggplot(data = Yakima_County_data, mapping = aes(x=total_jail_pop_rate, y=white_jail_pop_rate))+
  geom_point()+
  geom_smooth(method="lm",color="blue")+
  labs(title = "Variable comparison chart of Yakima County", 
       subtitle ="total jail population rate vs. white people in jail population rate", 
       x = "Total jail population rate (unit 0.01)", 
       y = "White people in jail population rate (unit 0.01)")