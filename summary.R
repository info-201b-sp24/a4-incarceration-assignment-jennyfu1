library(dplyr)
prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-prison-jail-rates-1990-WA.csv")

mean_byyear <- prison %>%
  filter(!is.na(total_jail_pop_rate), !is.na(total_prison_pop_rate)) %>%
  group_by(year) %>%
  summarise(mean_jail = mean(total_jail_pop_rate), 
            mean_prison = mean(total_prison_pop_rate))


maxmin_jail_bycounty_race <- prison %>%
  group_by(county_name) %>%
  summarise(max_white_rate=max(white_jail_pop_rate), 
            min_white_rate=min(white_jail_pop_rate), 
            max_black_rate=max(black_jail_pop_rate), 
            min_black_rate=min(black_jail_pop_rate), 
            max_latinx_rate=max(latinx_jail_pop_rate), 
            min_latinx_rate=min(latinx_jail_pop_rate), 
            max_aapi_rate=max(aapi_jail_pop_rate), 
            min_aapi_rate=min(aapi_jail_pop_rate), 
            max_native_rate=max(native_jail_pop_rate), 
            min_native_rate=min(native_jail_pop_rate) )


changes_jail_bycounty <- prison %>%
  group_by(county_name) %>%
  summarise(change_total_pop_rate = total_jail_pop_rate[year==2018] - total_jail_pop_rate[year==1990],
         change_white_pop_rate = white_jail_pop_rate[year==2018] - white_jail_pop_rate[year==1990], 
         change_black_pop_rate = black_jail_pop_rate[year==2018] - black_jail_pop_rate[year==1990], 
         change_latinx_pop_rate = latinx_jail_pop_rate[year==2018] - latinx_jail_pop_rate[year==1990],
         change_aapi_pop_rate = aapi_jail_pop_rate[year==2018] - aapi_jail_pop_rate[year==1990], 
         change_native_pop_rate = native_jail_pop_rate[year==2018] - native_jail_pop_rate[year==1990])




mean_2016 <- mean_byyear %>% filter(year==2016)

mean_1990 <- mean_byyear %>% filter(year==1990)

max_jail_black <- maxmin_jail_bycounty_race %>%
  arrange(desc(max_black_rate)) %>%
  slice_head(n=1) %>% 
  select(county_name, max_black_rate)

max_jail_white <- maxmin_jail_bycounty_race %>%
  arrange(desc(max_white_rate)) %>%
  slice_head(n=1) %>%
  select(county_name, max_white_rate)

max_jail_aapi <- maxmin_jail_bycounty_race %>%
  arrange(desc(max_aapi_rate)) %>%
  slice_head(n=1) %>%
  select(county_name, max_aapi_rate)

max_jail_latinx <- maxmin_jail_bycounty_race %>%
  arrange(desc(max_latinx_rate)) %>%
  slice_head(n=1) %>%
  select(county_name, max_latinx_rate)

max_changes_jail_total <- changes_jail_bycounty %>%
  arrange(desc(change_total_pop_rate)) %>%
  slice_head(n=1) %>%
  select(county_name, change_total_pop_rate)

min_changes_jail_total <- changes_jail_bycounty %>%
  arrange(change_total_pop_rate) %>%
  slice_head(n=1) %>%
  select(county_name, change_total_pop_rate)