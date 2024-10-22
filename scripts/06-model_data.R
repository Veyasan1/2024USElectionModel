#### Preamble ####
# Purpose: Models the 2024 US presidential poll data based on who would vote for Harris
# Author: Veyasan Ragulan
# Date: 18 October 2023
# Contact: veyasan.ragulan@mail.utoronto.ca
# License: MIT
# Pre-requisites: president_polls.csv, which is the presidental general election poll sourced from 538


#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/harris.csv")

#### Bayesian models ####
# Change 'pollster' and 'state' to factor variables
analysis_data <- analysis_data |>
  mutate(
    pollster = factor(pollster),
    state = factor(state),
    sponsors = factor(sponsors),
    methodology = factor(methodology)
  )

# Model 
model_formula <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster) + (1 | state) + (1 | sponsors) + (1 | methodology)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit the models
bayesian_model <- stan_glmer(
  formula = model_formula,
  data = analysis_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

# Posterior predictive checks
pp_check(bayesian_model)

# Summarize the model
summary(bayesian_model)

# Plot random effects
plot(bayesian_model, pars = "(Intercept)", prob = 0.95)

#### Bayesian models and splines ####
# Change date to be number of days since she declared - it's a counter not a date
analysis_data <- analysis_data |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

# Fit Bayesian model with spline and pollster as fixed effect
spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) + pollster + state + sponsors + methodology, # Change df for the number of "bits" - higher numbers - more "wiggly" - but then need to worry about overfitting.
  data = analysis_data,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

# Summarize the model
summary(spline_model)

# Posterior predictive checks
pp_check(spline_model)

# Save model
saveRDS(
  bayesian_model,
  file = "models/electionmodel.rds"
)

saveRDS(
  spline_model,
  file = "models/splinemodel.rds"
)

