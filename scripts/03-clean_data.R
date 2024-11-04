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

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
data <- read_csv("data/01-raw_data/raw_elections_data.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.7 # Need to investigate this choice - come back and fix. 
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

# Filter data to Trump estimates based on high-quality polls after he declared
just_trump_high_quality <- data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.7
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # Update with Trump's declaration date if needed
  mutate(
    num_trump = round((pct / 100) * sample_size, 0)
  )

#### Save data ####
write_csv(just_harris_high_quality, "data/02-analysis_data/harris_data.csv")
write_csv(just_trump_high_quality, "data/02-analysis_data/trump_data.csv")