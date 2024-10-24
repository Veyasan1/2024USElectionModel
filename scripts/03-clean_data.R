#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Veyasan Ragulan
# Date: 18 October 2023
# Contact: veyasan.ragulan@mail.utoronto.ca
# License: MIT
# Pre-requisites: president_polls.csv, sourced from https://projects.fivethirtyeight.com/polls/ (select Presidential general election polls current cycle)

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)
library(arrow)

#### Clean data ####
# Read in the data and clean variable names
data <- read_csv("data/01-raw_data/president_polls.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 3 # Need to investigate this choice - come back and fix. 
    # Also need to look at whether the pollster has multiple polls or just one or two - filter out later
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state), # Hacky fix for national polls - come back and check
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # When Harris declared
  mutate(
    num_harris = round((pct / 100) * sample_size, 0) # Need number not percent for some models
  )

#### Save data ####
write_csv(just_harris_high_quality, "data/02-analysis_data/harris.csv")

write_parquet(just_harris_high_quality, "data/02-analysis_data/harris.parquet")

