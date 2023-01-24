
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

This is a basic example which shows you how to solve a common problem:

``` r
library(excessmortality)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

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

# Check raw populations

raw_pops_checks <- check_raw_populations(pops)

# Convert annual to monthly populations
pops <- convert_annual_to_monthly_populations(
  pops,
  start_year = 2015,
  end_year = 2019)


# Check the inputs
monthly_pops_checks <- check_monthly_populations(pops)


# Convert the monthly to weekly populations


# Check the weekly populations



# Registered deaths

# Check the inputs

# Convert field to correct classes


# Combine deaths and populations


# Add on predictor variable


# Checks on final dataset


# Perform modelling


# Store model
```