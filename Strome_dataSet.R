install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")
install.packages("stringr")
library("stringr")
install.packages("tidyverse")
library("tidyverse")

df <- read.csv("C:/Users/ragha/Documents/syllabus/ANA_515/StormEvents_details-ftp_v1.0_d1999_c20220425.csv")

head(df)

limit_df <- df %>% select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

#Arrange the data by the state name (STATE)
arrange_df <- limit_df %>% arrange(STATE)

#Change state and county names to title case (e.g., “New Jersey” instead of “NEW JERSEY”)
change_df <- arrange_df %>% mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME))

filtere_df <- change_df %>% filter(CZ_TYPE == "C") %>% select(-CZ_TYPE)

pad_df <- filtere_df %>% mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, pad = "0"), 
                                CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0")) %>% 
  unite(FIPS, STATE_FIPS, CZ_FIPS, sep = "")

rename_df <- pad_df %>% rename_all(tolower)

data("state")

state_df <- data.frame(
  state_name = state.name,
  area = state.area,
  region = state.region
)

year <- 1999 

filtered_year_df <- pad_df %>% filter(substr(BEGIN_YEARMONTH, 1, 4) == year)

events_per_state <- filtered_year_df %>%
  group_by(STATE) %>%
  summarise(events = n())

merged_df <- merge(events_per_state, state_df, by.x = "STATE", by.y = "state_name", all.x = TRUE)

scatter_plot <- ggplot(merged_df, aes(x = area, y = events)) +
  geom_point(aes(color = region)) +
  labs(x = "State Area", y = "Number of Events") +
  ggtitle("Scatter Plot of State Area vs. Number of Events")

scatter_plot
