library(ggplot2)
library(dplyr)
library(usmap)
df <- read.csv("/Users/xmy/Desktop/a3---data-visualization-and-applications-Susanxiaoao/incarceration-trends-master/incarceration_trends.csv")

#part2: five values
# value 1: total population in US in 1970 and 2018
group_year_start <- filter(df, year == "1970")
start_pop <- sum(group_year_start$total_pop)
group_year_end <- filter(df, year == "2018")
end_pop <- sum(group_year_end$total_pop)
#203882263 to 327167434

# value 2: total jail population count in US in 1970 and 2018
start_jail_pop <- sum(group_year_start$total_jail_pop, na.rm = TRUE)
end_jail_pop <- sum(group_year_end$total_jail_pop, na.rm = TRUE)
# 160727.03 to 737126.07

# value 3: population growth rate vs jail population growth rate
pop_growth_rate <- (end_pop - start_pop) / start_pop * 100
jail_pop_growth_rate <- (end_jail_pop - start_jail_pop) / start_jail_pop * 100
# 60.469 % compared to 358.620 %

# value 4: jail population rate for all races in 2018
black_jail_pop_end <- sum(group_year_end$black_jail_pop, na.rm = TRUE)
black_jail_rate <- black_jail_pop_end / end_jail_pop * 100

latinx_jail_pop_end <- sum(group_year_end$latinx_jail_pop, na.rm = TRUE)
latinx_jail_rate <- latinx_jail_pop_end / end_jail_pop * 100

white_jail_pop_end <- sum(group_year_end$white_jail_pop, na.rm = TRUE)
white_jail_rate <- white_jail_pop_end / end_jail_pop * 100
#Among all the population in jail in 2018, 33.58% are black, 15.38% are latinx, 46.95% are white

# value 5: group by state to find out the state with the most black pop in jail in 2018
max_black_jail_pop <- group_year_end %>% group_by(state) %>% 
  summarise(black_jail_pop_state = sum(black_jail_pop, na.rm = TRUE)) %>% 
  filter(black_jail_pop_state == max(black_jail_pop_state))
#GA: 21921.25

#part3: a chart that shows trends over time for a variable of your choice
year_range <- c(1970 : 2018)

group_by_year <- group_by(df, year) %>% 
  summarise(aapi_jail_pop_year = sum(aapi_jail_pop, na.rm = TRUE),
            black_jail_pop_year = sum(black_jail_pop, na.rm = TRUE), 
            latinx_jail_pop_year = sum(latinx_jail_pop, na.rm = TRUE),
            native_jail_pop_year = sum(native_jail_pop, na.rm = TRUE),
            white_jail_pop_year = sum(white_jail_pop,na.rm = TRUE))

options(scipen=400000)

data_plot <- ggplot (group_by_year, aes(x = year_range)) + 
  geom_line(aes(y = aapi_jail_pop_year, color = "asian american, pacific island people")) + 
  geom_line(aes(y = black_jail_pop_year, color = "black people")) +
  geom_line(aes(y = latinx_jail_pop_year, color = "latinx people")) +
  geom_line(aes(y = native_jail_pop_year, color = "native american people")) +
  geom_line(aes(y = white_jail_pop_year, color = "white people")) +
  xlab("year") + ylab("jail population") +
  ggtitle("Plot of time against jail population")

#part4: a chart that compares two variables
black_prison_admin_total <- sum(df$black_prison_adm, na.rm = TRUE)
white_prison_admin_total <- sum(df$white_prison_adm, na.rm = TRUE)
other_prison_admin_total <- sum(df$aapi_prison_adm + df$latinx_prison_adm + df$native_prison_adm + df$other_race_prison_adm, na.rm = TRUE)
#total_prison_admin <- black_prison_admin_total + white_prison_admin_total + other_prison_admin_total
#5850096 vs 5579082 vs 1516628

admin_df <- data.frame(races = c("black people", "white people", "other races people"),
                       values = c(black_prison_admin_total, white_prison_admin_total, other_prison_admin_total))

pie_plot <- ggplot(admin_df, aes(x = "", y = values, fill = races)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  ggtitle("Plot of comparison between total black prison admission 
          and total white prison admission over the years")

#part5: a map shows how black population in jail in 2018 varies geographically
black_jail_df <- group_year_end %>% group_by(state) %>% 
  summarise(black_jail_pop_state = sum(black_jail_pop, na.rm = TRUE))

map_plot <- plot_usmap(regions = "states", data = black_jail_df, values = "black_jail_pop_state") +
  ggtitle("US map for black population in jail")
