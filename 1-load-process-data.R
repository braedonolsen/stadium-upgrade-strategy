# Load in data from Google Sheets and complete some final cleaning/processing steps ----

## Load packages and data ----
library(tidyverse)

attendance <- read_csv("data/team_attendance.csv")
fci <- read_csv("data/fan_cost_index.csv")
wins <- read_csv("data/team_wins.csv")
stadium <- read_csv("data/stadium.csv")

## need to pivot the FCI data to longer format ----
fci_long <- fci |> 
  mutate(across(-c(team, team_abbr), as.character)) |> 
  pivot_longer(
    cols = -c(team, team_abbr),
    names_to = "year",
    values_to = "fci"
  ) |> 
  mutate(
    year = as.integer(year),
    fci = as.numeric(str_replace_all(fci, "[$, ]", ""))
  )

## Join datasets for easier analysis ----
ballpark_data_full <- attendance |> 
  left_join(wins, by = c("year", "team_abbr")) |> 
  left_join(fci_long, by = c("year", "team_abbr")) |> 
  left_join(stadium, by = "team_abbr")


## Save out joined/cleaned dataset ----
write_csv(ballpark_data_full, file = "data/ballpark_data_full.csv")












