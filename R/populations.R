#' Convert population mid-year estimates into monthly estimates by population
#' subgroups
#'
#' @param denominators table that contains areacode, deprivation_quintile, sex,
#'   age_group, period, population
#' @param start_year numeric the start year for your output dataset
#' @param end_year numeric the end year for your output dataset
#'
#' @return tibble containing all the same input data and also month
#'
#' @import dplyr
#' @import lubridate
#'
convert_annual_to_monthly_populations <- function(denominators,
                                                  start_year,
                                                  end_year) {

  denominators <- denominators |>
    rename(period = "year") |>
    mutate(year = as.Date(paste(period, "07", "01",
                                sep = "-")))

  join_variables <- c("areacode", "deprivation_quintile", "sex", "age_group")

  denominator_groups <- denominators |>
    distinct(across(all_of(c(join_variables, "period")))) |>
    left_join(
      y = dplyr::tibble(
        month = seq(from = 1L,
                    to = 12L)
      ),
      by = character()
    )

  populations <- denominator_groups %>%
    mutate(earlier_year = case_when(
      .data$month < 7L ~ .data$period - 1L,
      TRUE ~ .data$period),
      later_year = case_when(
        .data$month < 7L ~ .data$period,
        TRUE ~ .data$period + 1L),
      start_month = as.Date(paste(.data$period, .data$month, "1",
                                  sep = "-"), format = "%Y-%m-%d"),
      end_month = .data$start_month %m+% months(1),
      start_year = as.Date(paste(.data$period, "1", "1",
                                 sep = "-"), format = "%Y-%m-%d"),
      end_year = .data$start_year + years(1),
      start_mid_yr_est = as.Date(paste(.data$earlier_year, "7", "1",
                                       sep = "-"), format = "%Y-%m-%d"),
      mid_point_month = .data$start_month - ((.data$start_month - .data$end_month) / 2),
      days_in_month = .data$end_month - .data$start_month,
      days_in_year = .data$end_year - .data$start_year,
      diff_mid_point_mid_yr_est = .data$mid_point_month - .data$start_mid_yr_est)

  populations <- populations %>%
    left_join(denominators[, c(join_variables, "population", "year")],
              by = c("areacode", "age_group", "sex", "deprivation_quintile", "start_mid_yr_est" = "year")) %>%
    rename(population1 = "population") %>%
    mutate(later_year_join = .data$start_mid_yr_est + years(1))

  populations <- populations %>%
    left_join(denominators[, c(join_variables, "population", "year")],
              by = c("areacode", "age_group", "sex", "deprivation_quintile", "later_year_join" = "year")) %>%
    rename(population2 = "population") %>%
    dplyr::select(!c("later_year_join")) %>%
    mutate(
      Cal1 = ((.data$population2 - .data$population1) *
                (as.integer(.data$diff_mid_point_mid_yr_est) /
                   as.integer(.data$days_in_year))) + .data$population1,
      Cal2 = as.integer(.data$days_in_month) / as.integer(.data$days_in_year),
      monthly_population = .data$Cal1 * .data$Cal2,
      denominator = .data$monthly_population / as.integer(.data$days_in_month)) |> #### Divide by days in month to get a daily pop per month
    dplyr::select("areacode",
                  "sex",
                  "age_group",
                  "deprivation_quintile",
                  month = "start_month",
                  denominator = "monthly_population") %>%
    filter(!is.na(.data$denominator),
           lubridate::year(.data$month) %in% start_year:end_year) %>%
    ungroup()

  return(populations)

}


#' aggregates denominator table containing monthly denominator data to a daily
#' version, then removes the weekends and bank holidays before aggregating it to
#' a monthly version
#'
#' @param denominators tibble; the tibble must contain fields for month,
#'   denominator, and any other population subgroup category desired (eg, age,
#'   sex, ethnic group, deprivation quintile)
#' @param from_date date; earliest date in the period of concern
#' @param to_date date; latest date in the period of concern
#' @param holidays date; a vector of dates which correspond to bank holidays in
#'   the period
#'
#' @import dplyr
#' @importFrom lubridate floor_date
#' @importFrom rlang .data
weekly_denominators <- function(denominators, from_date, to_date,
                                holidays) {

  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)

  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")

  # create all dates by day in time period
  all_dates <- seq(
    from = from_date,
    to = to_date,
    by = "days"
  )

  weekly_denominators <- dplyr::tibble(
    date = all_dates,
    month = floor_date(date,
                       unit = "month")
  ) |>
    inner_join(
      denominators,
      by = "month"
    ) |>
    remove_we_bh_denominators(
      holidays = holidays) |>
    mutate(
      date = round_to_friday(
        dt = .data$date,
        direction = "up"
      )
    ) |>
    rename(week_ending = date) |>
    group_by(across(!c("month", "denominator"))) |>
    summarise(
      denominator = sum(.data$denominator),
      .groups = "drop"
    )

  return(weekly_denominators)
}

#' Convert population mid-year estimates into weekly estimates by population
#' subgroups, accounting for lower populations in weeks containing bank holidays
#'
#' @param denominators table that contains areacode, deprivation_quintile, sex,
#'   age_group, period, population
#' @param from_date date; earliest date in the period of concern
#' @param to_date date; latest date in the period of concern
#' @param holidays date; a vector of dates which correspond to bank holidays in
#'   the period
#'
#' @return tibble containing all the same input data and also week_ending
#'
#' @export
#'
#' @examples
#' df <- create_dummy_populations() |>
#'  convert_annual_to_weekly_populations(
#'    from_date = as.Date("2015-01-03"),
#'    to_date = as.Date("2019-12-27"),
#'    holidays = as.Date("2017-12-25"))
convert_annual_to_weekly_populations <- function(denominators,
                                                 from_date, to_date,
                                                 holidays) {
  start_year <- lubridate::year(from_date)
  end_year <- lubridate::year(to_date)

  pops <- convert_annual_to_monthly_populations(
    denominators,
    start_year = start_year,
    end_year = end_year)

  baseline_weekly_pops <- weekly_denominators(
    denominators = pops,
    from_date = from_date,
    to_date = to_date,
    holidays = holidays
  )

  return(baseline_weekly_pops)
}
