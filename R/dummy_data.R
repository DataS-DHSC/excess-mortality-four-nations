#' Create a dummy population dataset for testing
#'
#' @param areacode string; areacodes for the areas included in modelling
#' @param deprivation_quintile numeric; deprivation quintile number (1 is least
#'   deprived quintile and 5 is the most deprived quintile)
#' @param sex 0 (male) or 1 (female)
#' @param age_group string; describing the age groups used for modelling
#' @param year numeric; the year the population estimate is for
#' @importFrom stats rnorm
#' @importFrom purrr map_dbl
#' @import dplyr
#' @return tibble containing areacode, sex, age_group, deprivation_quintile,
#'   year, population
#' @export
#'
#' @examples
#' df <- create_dummy_populations(
#'         areacode = c("abc", "def"))
create_dummy_populations <- function(
    areacode = c("E02000001", "E02000002", "E02000003"),
    deprivation_quintile = 1:5,
    sex = 0:1,
    age_group = c("0-24", "25-49", "50-64", "65-74", "75-84", "85+"),
    year = 2014:2024) {

  average_dummy_populations <- expand.grid(
    list(
      areacode = areacode,
      deprivation_quintile = deprivation_quintile,
      sex = sex,
      age_group = age_group)) %>%
    mutate(
      population = purrr::map_dbl(
        .x = .data$areacode,
        .f = ~ stats::rnorm(
          n = length(.x),
          mean = 500,
          sd = 250)
      ),
      population = case_when(
        .data$population < 0 ~ abs(.data$population),
        TRUE ~ .data$population
      ))

  dummy_populations <- expand.grid(
    list(
      areacode = areacode,
      deprivation_quintile = deprivation_quintile,
      sex = sex,
      age_group = age_group,
      year = year)) |>
    left_join(average_dummy_populations,
              by = c("areacode", "deprivation_quintile", "sex", "age_group")) |>
    mutate(
      population = purrr::map_dbl(
        .x = .data$population,
        .f = ~ stats::rnorm(
          n = 1,
          mean = .x,
          sd = 5)),
      population = case_when(
        .data$population < 0 ~ abs(.data$population),
        TRUE ~ .data$population
      ))

  return(dummy_populations)
}




#' Create a dummy registered deaths dataset for testing
#'
#' @param areacode string; areacodes for the areas included in modelling
#' @param deprivation_quintile numeric; deprivation quintile number (1 is least
#'   deprived quintile and 5 is the most deprived quintile)
#' @param sex 0 (male) or 1 (female)
#' @param age_group string; describing the age groups used for modelling
#' @param week_ending date with two items; item 1 is the date of the Friday at
#'   the end of the first full week of data in the period, and item 2 is the
#'   date of the Friday at the end of the last full week of data in the period
#' @param incomplete logical; TRUE will create a tibble with missing records
#'   simulating where there aren't deaths in a specific week/age
#'   group/sex/deprivation quintile. FALSE will provide a complete set of
#'   records for every combination of week, sex, age group and deprivation
#'   quintile
#' @importFrom purrr map_dbl
#' @import dplyr
#'
#' @return tibble containing areacode, sex, age_group, deprivation_quintile,
#'   week_end, registered_deaths
#' @export
#'
#' @examples
#' # creates a tibble of weekly registered deaths with some population
#' # subgroups missing (because there are no deaths in that subgroup)
#' df <- create_dummy_deaths(
#'         areacode = c("abc", "def"),
#'         incomplete = TRUE)
create_dummy_deaths <- function(
    areacode = c("E02000001", "E02000002", "E02000003"),
    deprivation_quintile = 1:5,
    sex = 0:1,
    age_group = c("0-24", "25-49", "50-64", "65-74", "75-84", "85+"),
    week_ending = as.Date(c("2015-01-09", "2019-12-27")),
    incomplete = FALSE) {

  set.seed(9442)

  dummy <- expand.grid(
    list(
      areacode = areacode,
      deprivation_quintile = deprivation_quintile,
      sex = sex,
      age_group = age_group,
      week_ending = seq(
        from = week_ending[1],
        to = week_ending[2],
        by = "weeks"
      )
    )) %>%
    mutate(
      registered_deaths = purrr::map_dbl(
        .x = .data$areacode,
        .f = ~ sample(1:150,
                      size = length(.x),
                      replace = TRUE)
      )
    )

  if (incomplete) {
    # remove 10% of the records
    dummy <- dummy |>
      slice_sample(prop = 0.9,
                   replace = FALSE)
  }
  return(dummy)
}

