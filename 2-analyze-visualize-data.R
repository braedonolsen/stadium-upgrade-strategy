# Analyze data and create objects for visualization in Flourish ----

# load packages and data ----
library(tidyverse)
ballpark_data <- read_csv("data/ballpark_data_full.csv")

## Initial basic analysis to see some very general trends ----

# Leaguewide trends over time
league_trends <- ballpark_data |> 
  summarize(
    avg_attendance = mean(attendance_game, na.rm = TRUE),
    avg_fci = mean(fci, na.rm = TRUE),
    .by = year
  )

write_csv(league_trends, file = "data/league_trends.csv")

# Attendance trend
ggplot(league_trends, aes(x = year, y = avg_attendance)) +
  geom_line() +
  geom_point(color = "red") +
  labs(title = "MLB Average Game Attendance (2000-2025)",
       x = "Year", 
       y = "Average Per-Game Attendance") +
  theme_minimal()

# FCI trend
ggplot(league_trends, aes(x = year, y = avg_fci)) +
  geom_line() +
  geom_point(color = "red") +
  labs(
    title = "MLB Average Fan Cost Index (2000-2022)",
    x = "Year", 
    y = "Average FCI ($)") +
  theme_minimal()

# Team rankings — average attendance across full window
ballpark_data |> 
  summarize(
    avg_attendance = mean(attendance_game, na.rm = TRUE),
    .by = team_abbr) |> 
  arrange(desc(avg_attendance)) |> 
  ggplot(aes(x = reorder(team_abbr, avg_attendance), y = avg_attendance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Game Attendance by Team (2000-2025)",
    x = NULL, 
    y = "Avg Attendance per Game") +
  theme_minimal()



## Create a clean stadium events reference table ----
# For teams with two renovation years, use the first one as the primary event
stadium_events <- ballpark_data |>
  distinct(team_abbr, opened, is_new, is_renovated, renovation_year) |>
  filter(is_new == 1 | is_renovated == 1) |>
  mutate(
    renovation_year_primary = as.integer(str_extract(renovation_year, "\\d{4}")),
    event_year = ifelse(is_new == 1, opened, renovation_year_primary)
  )

# Join event year back to main dataset and calculate years since event
ballpark_data <- ballpark_data |>
  left_join(stadium_events |> select(team_abbr, event_year), by = "team_abbr") |>
  mutate(years_since_event = year - event_year)

# Plot average attendance by years_since_event
# Filter to a reasonable window (-5 to +10 years around the event)
ballpark_data |>
  filter(!is.na(years_since_event),
         years_since_event >= -5,
         years_since_event <= 10,
         !year %in% c(2020, 2021)) |>
  summarize(
    avg_attendance = mean(attendance_game, na.rm = TRUE),
    .by = years_since_event) |>
  ggplot(aes(x = years_since_event, y = avg_attendance)) +
  geom_line() +
  geom_point(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Average Attendance Around Stadium Events",
    subtitle = "Year 0 = stadium opening or major renovation",
    x = "Years Since Stadium Event", 
    y = "Avg Attendance per Game") +
  theme_minimal()

# Same plot for FCI
ballpark_data |>
  filter(!is.na(years_since_event),
         !is.na(fci),
         years_since_event >= -5,
         years_since_event <= 10) |>
  summarise(
    avg_fci = mean(fci, na.rm = TRUE),
    .by = years_since_event) |>
  ggplot(aes(x = years_since_event, y = avg_fci)) +
  geom_line() +
  geom_point(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Average Fan Cost Index Around Stadium Events",
    subtitle = "Year 0 = stadium opening or major renovation",
    x = "Years Since Stadium Event", 
    y = "Average FCI ($)") +
  theme_minimal()

fci_centralized <- ballpark_data |>
  filter(!is.na(years_since_event),
         !is.na(fci),
         years_since_event >= -5,
         years_since_event <= 10) |>
  summarise(
    avg_fci = mean(fci, na.rm = TRUE),
    .by = years_since_event)
write_csv(fci_centralized, file = "data/fci_centralized.csv")

# split into attendance trends by event type (new opening vs renovation)
ballpark_data |>
  filter(!is.na(years_since_event),
         years_since_event >= -5,
         years_since_event <= 10,
         !year %in% c(2020, 2021),
         !is.na(is_new)) |>
  mutate(event_type = ifelse(is_new == 1, "New Stadium", "Renovation")) |>
  summarise(
    avg_attendance = median(attendance_game, na.rm = TRUE),
    .by = c(years_since_event, event_type)) |>
  ggplot(aes(x = years_since_event, y = avg_attendance, color = event_type)) +
  geom_line() +
  geom_point(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Median Attendance Around Stadium Events by Type (excl. COVID)",
       x = "Years Since Stadium Event", y = "Median Attendance per Game",
       color = NULL) +
  theme_minimal()

# save a dataset to use in Flourish
flourish_attendance <- ballpark_data |>
  filter(!is.na(years_since_event),
         years_since_event >= -5,
         years_since_event <= 10,
         !year %in% c(2020, 2021),
         !is.na(is_new)) |>
  mutate(event_type = ifelse(is_new == 1, "New Stadium", "Renovation")) |>
  summarise(
    median_attendance = median(attendance_game, na.rm = TRUE),
    .by = c(years_since_event, event_type)) |>
  pivot_wider(names_from = event_type, values_from = median_attendance)

write_csv(flourish_attendance, "data/flourish_attendance.csv")


## Now lets try to control for wins to see the effect of stadium renovations ----

# wins vs attendance, colored by proximity in time to stadium event
ballpark_data |>
  filter(!is.na(wins),
         !is.na(attendance_game),
         !year %in% c(2020, 2021)) |>
  mutate(near_event = case_when(
    is.na(years_since_event) ~ "No Event",
    years_since_event >= 0 & years_since_event <= 3 ~ "0-3 Years Post Event",
    years_since_event > 3 ~ "4+ Years Post Event",
    years_since_event < 0 ~ "Pre Event"
  )) |>
  ggplot(aes(x = wins, y = attendance_game, color = near_event)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Wins vs. Attendance by Stadium Event Proximity",
       x = "Wins", 
       y = "Attendance per Game",
       color = NULL) +
  theme_minimal()

# Linear regression: attendance ~ wins + years_since_event + fci
model <- lm(attendance_game ~ wins + years_since_event + fci,
            data = ballpark_data |>
              filter(!year %in% c(2020, 2021),
                     !is.na(wins),
                     !is.na(fci),
                     !is.na(years_since_event)))

summary(model)
