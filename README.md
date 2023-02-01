
<!-- README.md is generated from README.Rmd. Please edit that file -->

# excessmortality

<!-- badges: start -->
<!-- badges: end -->

The goal of excessmortality is to …

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

# function that scrapes bank holidays from the internet for northern ireland
bank_holidays <- function(yr) {
  
  # function that applies the first row as the field names
  row_one_to_column_name <- function(data) {
    col_names <- data[1, ] |> 
      as.character()
    
    data <- data |> 
      slice(-1)
    
    names(data) <- col_names
    
    return(data)
  }
  
  url <- sprintf("https://uk-public-holidays.com/northern-ireland-public-holidays-%s/",
                 yr)
  
  date_table <- rvest::read_html(url) %>%
    rvest::html_elements("#zebra") %>%
    rvest::html_table(header = FALSE)
  
  date_table <- date_table[[1]]
  
  header_row <- match("Date", date_table[[1]])
  
  date <- date_table %>% 
    slice(header_row:nrow(.)) |> 
    row_one_to_column_name() |> 
    mutate(
      dy = stringr::str_extract(Date, "[0-9]+"),
      mnth = stringr::str_extract(Date, "[[:alpha:]]+"),
      yr = yr,
      bh = 
        paste(dy, mnth, yr,
              sep = "-"),
      bh_date = as.Date(
        bh,
        format = "%d-%B-%Y"
      )) |> 
    pull(bh_date)
  
  return(date)
}

holidays <- 2014:2023 |> 
  lapply(bank_holidays) |> 
  purrr::reduce(
    .f = c
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
      .fns = factor,
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

# variables related to bank holidays
hol_vars <- weekly_holiday_variables(
  from_date = from_date,
  to_date = to_date,
  holidays = holidays
)
  
# variables relates to mortality trends
trend_var <- weekly_trend_variable(
  from_date = from_date,
  to_date = to_date
)
  
# variables related to times of the year
seasonal_vars <- weekly_seasonal_variables(
  from_date = from_date,
  to_date = to_date
)  
  
date_dependent_variables <- hol_vars %>% 
  left_join(seasonal_vars, by = "week_ending") %>% 
  left_join(trend_var, by = "week_ending")  

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
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
#> prediction from a rank-deficient fit may be misleading
  
# reduce table down to component parts and add prediction intervals
expected_deaths <- predictors |> 
  mutate(expected_deaths = predictions) |> 
  select(
    week_ending, areacode, sex, age_group, deprivation_quintile, expected_deaths
  ) |> 
  add_prediction_intervals(dispersion_parameter)
```
