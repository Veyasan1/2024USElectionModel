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

#### Starter models ####
# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date, data = analysis_data)

# Model 2: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = analysis_data)

# Augment data with model predictions
analysis_data <- analysis_data |>
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

# Plot model predictions
# Model 1
ggplot(analysis_data, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Model 2
ggplot(analysis_data, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dotted") +
  facet_wrap(vars(pollster)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster")

# This is an example of how you get a results table that you could put into your Quarto doc
modelsummary(models = list("Model 1" = model_date, "Model 2" = model_date_pollster))


#### Bayesian models ####
# Change 'pollster' and 'state' to factor variables
analysis_data <- analysis_data |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

# Model 1
model_formula_1 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster)

# Model 2
model_formula_2 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit the models
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = analysis_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = analysis_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)
# Note the warnings here and follow the instructions for dealing with it - come back and fix.

# Posterior predictive checks
pp_check(bayesian_model_1)
pp_check(bayesian_model_2)

# Summarize the model
summary(bayesian_model_1)
summary(bayesian_model_2)

# Plot random effects
plot(bayesian_model_1, pars = "(Intercept)", prob = 0.95)
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95)

# Model summary works the same as above for Bayesian models.


#### Bayesian models and splines ####
# Change date to be number of days since she declared - it's a counter not a date
analysis_data <- analysis_data |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

# Fit Bayesian model with spline and pollster as fixed effect
# cf bayesian_model_1 and bayesian_model_2 where it's a random effect - note the different interpretations
spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) + pollster, # Change df for the number of "bits" - higher numbers - more "wiggly" - but then need to worry about overfitting.
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

