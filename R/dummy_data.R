#' Create a dummy population dataset for testing
#'
#' @param areacode string; areacodes for the areas included in modelling
#' @param deprivation_quintile numeric; deprivation quintile number (1 is least
#'   deprived quintile and 5 is the most deprived quintile)
#' @param sex 0 (male) or 1 (female)
#' @param age_group string; describing the age groups used for modelling
#' @param year numeric; the year the population estimate is for
#'
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
      population = rnorm(n = nrow(.), mean = 500, sd = 250),
      population = case_when(
        population < 0 ~ abs(population),
        TRUE ~ population
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
        .x = population,
        .f = ~ rnorm(
          n = 1,
          mean = .x,
          sd = 5)),
      population = case_when(
        population < 0 ~ abs(population),
        TRUE ~ population
      ))

  return(dummy_populations)
}




create_dummy_data <- function(
    areacode = c("E02000001", "E02000002", "E02000003"),
    deprivation_quintile = 1:5,
    sex = 0:1,
    age_group = c("0-24", "25-49", "50-64", "65-74", "75-84", "85+"),
    week_ending = as.Date(c("2015-01-09", "2019-12-27"))) {

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
    ))

  average_population <- expand.grid(
    list(
      areacode = areacode,
      deprivation_quintile = deprivation_quintile,
      sex = sex,
      age_group = age_group)) %>%
    mutate(
      population = rnorm(n = nrow(.), mean = 500, sd = 250),
      population = case_when(
        population < 0 ~ abs(population),
        TRUE ~ population
      )
    )

  dummy <- dummy |>
    left_join(average_population,
              by = c("areacode", "deprivation_quintile", "sex", "age_group")) |>
    mutate(
      population = purrr::map_dbl(
        .x = population,
        .f = ~ rnorm(
          n = 1,
          mean = .x,
          sd = 5)),
      population = case_when(
        population < 0 ~ abs(population),
        TRUE ~ population
      ),
      registered_deaths = purrr::map_dbl(
        .x = population,
        .f = ~ rnorm(
          n = 1,
          mean = .x / 100,
          sd = 5)),
      registered_deaths = case_when(
        registered_deaths < 0 ~ 0,
        TRUE ~ round_correct(registered_deaths, 0)
      )
    )

  return(dummy)
}

