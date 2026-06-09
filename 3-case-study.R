# Team-Specific Research/Case Studies to Support Final Project ----

## Load packages and data ----
library(tidyverse)
ballpark_data <- read_csv("data/ballpark_data_full.csv")

## Atlanta Braves--Truist Park ----
ballpark_data |>
  filter(team_abbr == "ATL", !year %in% c(2020, 2021)) |>
  select(year, attendance_game, wins, fci) |>
  arrange(year) |> 
  print(n = 24)

## Pittsburgh Pirates--PNC Park ----
pirates_nums <- ballpark_data |>
  filter(team_abbr == "PIT", !year %in% c(2020, 2021)) |>
  select(year, attendance_game, wins, fci) |>
  arrange(year) |> 
  print(n = 24)
write_csv(pirates_nums, file = "data/pirates_nums.csv")

## Chicago White Sox--Rate Field ----
ballpark_data |>
  filter(team_abbr == "CWS", !year %in% c(2020, 2021)) |>
  select(year, attendance_game, wins, fci) |>
  arrange(year) |> 
  print(n = 24)


















