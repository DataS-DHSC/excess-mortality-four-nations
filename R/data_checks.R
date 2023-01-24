#' Sense check monthly population data
#'
#' @param populations table of populations containing areacode, age_group, sex,
#'   deprivation_quintile, year and population
#' @description checks on the expected number of records, all the names are
#'   present, each record is unique, there are no NAs and that all population
#'   values are positive or zero
#' @return function will error if something is wrong with the data inputs,
#'   otherwise it will return the sum of the population inputs
#' @import dplyr
#' @import assertr
#' @importFrom lubridate year
#' @export
#'
#' @examples
#' pops <- create_dummy_populations() |>
#'    convert_annual_to_monthly_populations()
#' pop_checks <- check_monthly_populations(pops)
check_monthly_populations <- function(populations) {

  number_areacodes <- length(unique(populations$areacode))
  number_months <- 12
  number_years <- length(unique(lubridate::year(populations$month)))
  number_sexes <- 2
  number_age_groups <- length(unique(populations$age_group))
  number_deprivation_quintiles <- 5

  expected_records_denominators <- number_areacodes *
    number_months *
    number_sexes *
    number_age_groups *
    number_years *
    number_deprivation_quintiles

  populations %>%
    chain_start() %>%
    verify(has_all_names("areacode", "deprivation_quintile", "sex", "age_group", "month", "denominator")) %>%
    verify(nrow(.) == expected_records_denominators) %>%
    assert_rows(col_concat, is_uniq, areacode, deprivation_quintile, sex, age_group, month) %>%
    assert(not_na, everything()) %>%
    assert(within_bounds(0, Inf), denominator) %>%
    chain_end()

  total_denominators <- sum(populations$denominator) # for later checks
  return(total_denominators)
}


#' Sense check raw population data
#'
#' @param populations table of populations containing areacode, age_group, sex,
#'   deprivation_quintile, year and population
#' @description checks on the expected number of records, all the names are
#'   present, each record is unique, there are no NAs and that all population
#'   values are positive or zero
#' @return function will error if something is wrong with the data inputs,
#'   otherwise it will return the a table of populations by year
#' @import dplyr
#' @import assertr
#' @importFrom lubridate year
#' @export
#'
#' @examples
#' pops <- create_dummy_populations()
#' pop_checks <- check_raw_populations(pops)
check_raw_populations <- function(populations) {

  number_areacodes <- length(unique(populations$areacode))
  number_years <- length(unique(populations$year))
  number_sexes <- 2
  number_age_groups <- length(unique(populations$age_group))
  number_deprivation_quintiles <- 5

  expected_records_denominators <- number_areacodes *
    number_sexes *
    number_age_groups *
    number_years *
    number_deprivation_quintiles

  populations %>%
    chain_start() %>%
    verify(has_all_names("areacode", "deprivation_quintile", "sex", "age_group", "year", "population")) %>%
    verify(nrow(.) == expected_records_denominators) %>%
    assert_rows(col_concat, is_uniq, areacode, deprivation_quintile, sex, age_group, year) %>%
    assert(not_na, everything()) %>%
    assert(within_bounds(0, Inf), population) %>%
    chain_end()

  total_denominators <- populations |>
    group_by(year) |>
    summarise(population = sum(population), # for later checks
              .groups = "drop")
  return(total_denominators)
}
