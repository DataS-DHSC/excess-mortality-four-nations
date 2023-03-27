
<!-- README.md is generated from README.Rmd. Please edit that file -->

# excessmortality

<!-- badges: start -->

[![R-CMD-check](https://github.com/DataS-DHSC/excess-mortality-four-nations/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DataS-DHSC/excess-mortality-four-nations/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/DataS-DHSC/excess-mortality-four-nations/branch/master/graph/badge.svg)](https://app.codecov.io/gh/DataS-DHSC/excess-mortality-four-nations?branch=master)
<!-- badges: end -->

The goal of excessmortality is to make it simpler for users to estimate
expected deaths each week in a similar way to how the Office for Health
Improvement and Disparities has done in their monthly publication of
[Excess Mortality in England and English
Regions](https://www.gov.uk/government/statistics/excess-mortality-in-england-and-english-regions).

## Installation

You can install the development version of excessmortality from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DataS-DHSC/excess-mortality-four-nations")
```

## Example

This is an example which shows you how to calculate excess mortality
using functions from this package along with your own data:

### Build the data for modelling

``` r
library(excessmortality)
library(dplyr)


holidays <- as.Date(
  c(
    "2014-01-01",
    "2014-12-25",
    "2014-12-26",
    "2015-01-01",
    "2015-12-25",
    "2015-12-26",
    "2016-01-01",
    "2016-12-25",
    "2016-12-26",
    "2017-01-01",
    "2017-12-25",
    "2017-12-26",
    "2018-01-01",
    "2018-12-25",
    "2018-12-26",
    "2019-01-01",
    "2019-12-25",
    "2019-12-26",
    "2020-01-01",
    "2020-12-25",
    "2020-12-26",
    "2021-01-01",
    "2021-12-25",
    "2021-12-26",
    "2022-01-01",
    "2022-12-25",
    "2022-12-26",
    "2023-01-01",
    "2023-12-25",
    "2023-12-26"
  )
)

# Populations
pops <- create_dummy_populations()

# Convert annual to monthly populations
baseline_weekly_pops <- convert_annual_to_weekly_populations(
  pops,
  from_date = as.Date("2015-01-03"),
  to_date = as.Date("2019-12-27"),
  holidays = holidays
)

# Registered deaths
# Note, in true datasets there may be weeks where there are 0 deaths registered for specific age groups, sex, deprivation quintile combinations (eg, incomplete = TRUE in the function below)
baseline_incomplete_deaths <- create_dummy_deaths(
  incomplete = TRUE
)

# Fill in the weeks for the missing deaths
baseline_complete_deaths <- baseline_incomplete_deaths |> 
  tidyr::complete(
    areacode,
    deprivation_quintile,
    sex,
    age_group,
    week_ending,
    fill = list(registered_deaths = 0)
  )

# Check the inputs

# Convert field to correct classes
baseline_weekly_pops <- baseline_weekly_pops |> 
  mutate(
    across(
      c(areacode, sex, age_group, deprivation_quintile),
      .fns = factor
    ),
    week_ending = as.Date(week_ending)
  )

baseline_complete_deaths <- baseline_complete_deaths |> 
  mutate(
    across(
      c(areacode, sex, age_group, deprivation_quintile),
      .fns = factor
    ),
    week_ending = as.Date(week_ending)
  )



# Combine deaths and populations
baseline_data <- baseline_complete_deaths |> 
  left_join(
    baseline_weekly_pops,
    by = c("areacode", "deprivation_quintile", "sex", "age_group", "week_ending"))

# Create the date dependent variables
from_date <- as.Date("2015-01-03")
to_date <- as.Date("2019-12-27")

# create date-dependent variables
  date_dependent_variables <- create_date_dependent_variables(
    from_date = from_date,
    to_date = to_date,
    holidays = holidays
  )

# Add on predictor variables
baseline_data <- baseline_data |> 
  left_join(date_dependent_variables,
            by = "week_ending")
```

### Perform modelling

``` r
model <- glm("registered_deaths ~ offset(log(denominator)) +
                           sex:age_group +                 
                           areacode +
                           deprivation_quintile:age_group +
                           years_from_20161231:deprivation_quintile +
                           years_from_20161231:age_group +
                           years_from_20161231 +
                           month1:age_group + month2:age_group + 
                           month3:age_group + month4:age_group +
                           month5:age_group + month6:age_group + 
                           month7:age_group + month8:age_group +
                           month9:age_group + month10:age_group + 
                           month11:age_group + month12:age_group +
                           easter_pre + easter_post_1 + easter_post_2 +
                           wk_nearest_BH + wk_next_nearest_BH +
                           wk_sat_to_mon_xmas + wk_post_sat_to_mon_xmas + wk2_post_sat_to_mon_xmas +
                           consecutive_bh + wk_after_tue_to_thu_xmas + wk2_after_tue_to_thu_xmas", 
               family = quasipoisson,
               data = baseline_data)

dispersion_parameter <- summary(model)$dispersion
```

### Create expected deaths

``` r
# obtain weekly populations for the prediction time period
expected_weekly_pops <- convert_annual_to_weekly_populations(
  pops,
  from_date = as.Date("2020-03-21"),
  to_date = as.Date("2023-12-29"),
  holidays = holidays
)

# build table of data to predict from
predictors <- build_prediction_dates(
  from_date = as.Date("2020-03-21"),
  to_date = as.Date("2023-12-29"),
  holidays = holidays,
  denominators = expected_weekly_pops
)

# apply model to the table of data
predictions <- predict(
  model, 
  newdata = predictors, 
  type = "response")
  
# reduce table down to component parts 
expected_deaths <- predictors |> 
  mutate(expected_deaths = predictions) |> 
  select(
    week_ending, areacode, sex, age_group, deprivation_quintile, expected_deaths
  )

# aggregate and add prediction intervals

expected_deaths_by_age <- expected_deaths |> 
  group_by(week_ending, areacode, age_group) |> 
  summarise(expected_deaths = sum(expected_deaths,
                                  na.rm = TRUE),
            .groups = "drop") |> 
  add_prediction_intervals(dispersion_parameter)
```
