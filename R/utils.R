# day and date functions --------------------------------------------------

#' converts a date to the following Saturday
#' @param dt date
#' @importFrom lubridate wday
round_up_to_saturday <- function(dt) {
  dt <- dt + (7 - lubridate::wday(dt))
  return(dt)
}


#' converts a date to the previous/following Friday
#' @param dt date
#' @param direction string; "up" or "down" - which direction to round the date
#' @importFrom lubridate wday
#' @importFrom dplyr between
round_to_friday <- function(dt, direction) {
  direction <- match.arg(direction,
                         c("down", "up"))

  days_of_week <- lubridate::wday(dt)
  saturdays <- days_of_week == 7

  if (direction == "up") {
    not_saturdays <- days_of_week != 7

    dt[saturdays] <- dt[saturdays] + 6
    dt[not_saturdays] <- dt[not_saturdays] + (6 - days_of_week[not_saturdays])
  } else if (direction == "down") {
    sundays <- days_of_week == 1
    mon_to_thurs <- between(days_of_week, 2, 5)

    dt[saturdays] <- dt[saturdays] - 1
    dt[sundays] <- dt[sundays] - 2
    dt[mon_to_thurs] <- dt[mon_to_thurs] - (days_of_week[mon_to_thurs] + 1)

  }
  return(dt)
}

#' @title calculate Easter Fridays from an input of dates
#' @param dates date; vector of holiday dates
#' @param include_christmas_friday logical; whether to treat a Christmas on a
#'   Friday in the same was as an Easter Friday (because the following Monday is
#'   also a bank holiday)
#' @description returns a vector of easter fridays when given a vector of
#'   holiday dates
#' @import dplyr
#' @importFrom lubridate wday month
calc_easter_fridays <- function(dates, include_christmas_friday = TRUE) {
  easter_fridays <- dplyr::tibble(holiday_date = dates) |>
    mutate(
      Day_name = lubridate::wday(.data$holiday_date,
                                 label = TRUE,
                                 abbr = FALSE),
      easter_friday = .data$holiday_date %in% (dates - 3),
      easter_monday = .data$holiday_date %in% (dates + 3),
      month = lubridate::month(.data$holiday_date),
      easter_weekend = case_when(
        .data$easter_monday | .data$easter_friday ~ TRUE,
        TRUE ~ FALSE
        ))

  if (include_christmas_friday == TRUE) {
    easter_fridays <- easter_fridays |>
      filter(.data$easter_friday)
  } else {
    easter_fridays <- easter_fridays |>
      filter(.data$easter_friday,
             .data$month != 12)
  }
  easter_fridays <- easter_fridays |>
    arrange(.data$holiday_date) |>
    pull(.data$holiday_date)
  return(easter_fridays)
}

#' removes denominators from weekend and bank holiday dates. Within the
#' baseline death period, deaths were very unlikely to be registered on
#' weekends or bank holidays therefore these days should have consistent
#' predictions with working days
#' @param data tibble of data where one field contains the denominator
#' @param holidays vector of dates which are bank holidays
#' @importFrom rlang .data
#' @import dplyr
remove_we_bh_denominators <- function(data, holidays) {
  data <- data |>
    mutate(
      denominator = case_when(
        lubridate::wday(.data$date) %in% c(1, 7) ~ 0,
        .data$date %in% holidays ~ 0,
      TRUE ~ as.numeric(.data$denominator)
    ))

  return(data)
}


# transformation functions ---------------------------------------------------------

#' @title Correct rounding
#' @param x number to round
#' @param n decimal places
#'
#' @description rounding function
#' @details from https://stackoverflow.com/questions/12688717/round-up-from-5
round_correct <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10 ^ n
  z <- z * posneg

  return(z)
}


# Functions to build modelling table ---------------------------------------

#' @title calculate the dates in the same week as Christmas
#' @description function that creates a vector of dates that sit in the same week
#'   as Christmas, where Christmas day is on a specified day of the week (ie, if
#'   interested in a Friday Christmas only and the years 2015 to 2020 were
#'   provided, only 19th to 25th December 2015 would be returned)
#' @param year integer; year of interest
#' @param day_of_interest integer between 1 and 7 for day of the week (1 is
#'   Sunday)
#' @importFrom lubridate wday
christmas_week <- function(year, day_of_interest) {
  xmas <- as.Date(paste(year, "12", "25",
                        sep = "-"))
  keep_xmas <- xmas[lubridate::wday(xmas) %in% day_of_interest]

  friday_dates <- keep_xmas + (7 - lubridate::wday(keep_xmas, week_start = 6))

  dates <- c(friday_dates,
             friday_dates - rep(1:6, each = length(friday_dates)))
  dates <- sort(dates)
  return(dates)
}

#' applies variables indicating the days which are in a week where Christmas day
#' falls on a Saturday, Sunday or Monday. Weeks are Saturday to Friday.
#' @param data tibble containing a "date" field
#' @importFrom lubridate year
#' @import dplyr
#'
sat_to_mon_xmas <- function(data) {
  date_range <- data |>
    pull(date) |>
    range()

  dates_in_week_where_xmas_on_sat_to_mon <- christmas_week(
    year(date_range[1]):year(date_range[2]),
                                                           c(1, 2, 7))

  data <- data|>
    mutate(
      sat_to_mon_xmas =
        date %in% dates_in_week_where_xmas_on_sat_to_mon,
      week_after_sat_to_mon_xmas =
        date %in% (dates_in_week_where_xmas_on_sat_to_mon + 7),
      week2_after_sat_to_mon_xmas =
        date %in% (dates_in_week_where_xmas_on_sat_to_mon + 14)
    )
  return(data)

}

#' @title add binary variable to the table for Easter
#' @description adds binary variables for days around easter holidays
#' @param data table that includes a "date" field
#' @param easter_fridays dates of easter fridays within the dataset
#' @import dplyr
add_easter_binary_variables <- function(data, easter_fridays) {
  data  <- data |>
    mutate(
      WedpreE = case_when(
        date %in% (easter_fridays - 2) ~ 1L,
        TRUE ~ 0L),
      ThurpreE = case_when(
        date %in% (easter_fridays - 1) ~ 1L,
        TRUE ~ 0L),
      TuespostE = case_when(
        date %in% (easter_fridays + 4) ~ 1L,
        TRUE ~ 0L),
      WedpostE = case_when(
        date %in% (easter_fridays + 5) ~ 1L,
        TRUE ~ 0L),
      ThurpostE = case_when(
        date %in% (easter_fridays + 6) ~ 1L,
        TRUE ~ 0L),
      FripostE = case_when(
        date %in% (easter_fridays + 7) ~ 1L,
        TRUE ~ 0L),
      MonpostE1 = case_when(
        date %in% (easter_fridays + 10) ~ 1L,
        TRUE ~ 0L),
      TuespostE1 = case_when(
        date %in% (easter_fridays + 11) ~ 1L,
        TRUE ~ 0L))
  return(data)
}

#' @title add binary variable to the data for bank holidays
#' @description adds binary variables for days around bank holidays that aren't
#'   easter or xmas. Bank holidays that occur on Tuesday, Wednesday or Thursday
#'   aren't picked out as it is assumed that the variability in registrations
#'   occurs within the same week that the bank holiday occurred
#' @param data table with a "date" field in it
#' @param holidays date; vector of bank holidays in the period
#' @import dplyr
add_bh_binary_variables <- function(data, holidays) {
  easter_fridays <- calc_easter_fridays(holidays)

  non_easter_holidays <- holidays[!(holidays %in% c(easter_fridays, (easter_fridays + 3)))]

  friday_bhols <- non_easter_holidays[lubridate::wday(non_easter_holidays) == 6 &
                                        lubridate::month(non_easter_holidays) != 12]
  monday_bhols <- non_easter_holidays[lubridate::wday(non_easter_holidays) == 2 &
                                        lubridate::month(non_easter_holidays) != 12]


  data  <- data |>
    mutate(
      BH_nearest_WD = case_when(
        (day == 5 & date %in% (friday_bhols - 1)) |
          (day == 3 & date %in% (monday_bhols + 1)) ~ 1L,
        TRUE ~ 0L),
      BH_next_nearest_WD = case_when(
        (day == 6 & date %in% (monday_bhols - 3)) |
          (day == 2 & date %in% (friday_bhols + 3)) ~ 1L,
        TRUE ~ 0L))
  return(data)
}

#' applies variables to weeks where there are two consecutive bank holidays,
#' that don't include a Monday/Tuesday combination. Where these occur on xmas,
#' the two following weeks will be identified with additional variables. This
#' function aims to pick up the Queen's Jubilee weekend
#' @param data tibble containing a date field
#' @param holidays date; vector of holiday dates
#' @importFrom rlang .data
#' @importFrom lubridate wday month
#' @import dplyr
#'
consecutive_bank_hols <- function(data, holidays) {
  date_range <- data %>%
    pull("date") %>%
    range()

  # create vector of dates where date and following date are bank holidays, and
  # first date isn't a Monday
  dates_with_consecutive_bhs <- tibble::tibble(
    date = seq(
      from = date_range[1],
      to = date_range[2],
      by = "day"
    ),
    holiday = .data$date %in% holidays,
    week_ending = round_to_friday(
      dt = .data$date,
      direction = "up"
    )) %>%
    filter(
      .data$holiday == TRUE,
      lead(.data$holiday == TRUE),
      lubridate::wday(.data$date) != 2) %>%
    pull("week_ending")

  data <- data %>%
    mutate(
      consecutive_bh =
        .data$date %in% dates_with_consecutive_bhs,
      wk_after_tue_to_thu_xmas =
        .data$date %in% (dates_with_consecutive_bhs + 7) &
        lubridate::month(.data$date) %in% c(1, 12),
      wk2_after_tue_to_thu_xmas =
        .data$date %in% (dates_with_consecutive_bhs + 14)  &
        lubridate::month(.data$date) %in% c(1, 12)
    )
  return(data)

}

#' creates a weekly table of bank holiday variables for modelling or predicting
#'
#' @param from_date date; first date in time period
#' @param to_date date; final date in the time period
#' @param holidays vector of dates that represent national holidays
#'
#' @import dplyr
#' @importFrom lubridate wday
#' @export
weekly_holiday_variables <- function(from_date, to_date, holidays) {
  # Parameters to make
  # easter_pre
  # easter_post_1
  # easter_post_2
  # wk_nearest_BH
  # wk_next_nearest_BH
  # wk_fri_xmas - this is included in easter_pre
  # wk_post_fri_xmas - this is included in easter_post_1
  # wk2_post_fri_xmas - this is included in easter_post_2
  # wk_sat_to_mon_xmas
  # wk_post_sat_to_mon_xmas
  # wk2_post_sat_to_mon_xmas


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

  # create vector of easter fridays
  easter_fridays <- calc_easter_fridays(
    holidays,
    include_christmas_friday = TRUE)

  # generate daily holiday variables
  holiday_variables <- dplyr::tibble(
    date = all_dates
  ) |>
    # add easter binary variables:
    # days before easter friday
    # days after easter Monday
    # days the week following easter Monday
    add_easter_binary_variables(
      easter_fridays = easter_fridays
    ) |>
    mutate(day = lubridate::wday(.data$date)) |>
    # create bank hol variables that aren't easter or Christmas
    add_bh_binary_variables(
      holidays = holidays
    ) |>
    sat_to_mon_xmas()|>
    consecutive_bank_hols(holidays) |>
    # round up the date field to the following Friday
    mutate(
      week_ending = round_to_friday(
        dt = .data$date,
        direction = "up"
      )
    ) |>
    group_by(.data$week_ending) |>
    summarise(
      easter_pre = max(.data$ThurpreE, na.rm = TRUE),
      easter_post_1 = max(.data$TuespostE, na.rm = TRUE),
      easter_post_2 = max(.data$MonpostE1, na.rm = TRUE),
      wk_nearest_BH = max(.data$BH_nearest_WD, na.rm = TRUE),
      wk_next_nearest_BH = max(.data$BH_next_nearest_WD, na.rm = TRUE),
      wk_sat_to_mon_xmas = max(.data$sat_to_mon_xmas, na.rm = TRUE),
      wk_post_sat_to_mon_xmas = max(.data$week_after_sat_to_mon_xmas, na.rm = TRUE),
      wk2_post_sat_to_mon_xmas = max(.data$week2_after_sat_to_mon_xmas, na.rm = TRUE),
      consecutive_bh = max(.data$consecutive_bh, na.rm = TRUE),
      wk_after_tue_to_thu_xmas = max(.data$wk_after_tue_to_thu_xmas, na.rm = TRUE),
      wk2_after_tue_to_thu_xmas = max(.data$wk2_after_tue_to_thu_xmas, na.rm = TRUE),
      .groups = "drop"
    )

  return(holiday_variables)
}

#' creates a weekly table mean number of days in that week since 31st Dec 2016
#' @param from_date date; first date in time period
#' @param to_date date; last date in time period
#' @import dplyr
#' @export
weekly_trend_variable <- function(from_date, to_date) {

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

  trend <- dplyr::tibble(
    date = all_dates,
    years_from_20161231 = as.numeric(.data$date - as.Date("2016-12-31")) / 365.25) %>%
    mutate(
      week_ending = round_to_friday(
        dt = .data$date,
        direction = "up"
      )
    ) |>
    group_by(.data$week_ending) |>
    summarise(
      years_from_20161231 = mean(.data$years_from_20161231)
    )
  return(trend)
}

#' creates table of month variables (ie, seasonal) aggregated to week
#' @param from_date date; first date in time period
#' @param to_date date; last date in time period
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export
weekly_seasonal_variables <- function(from_date, to_date) {
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

  seasonal <- dplyr::tibble(date = all_dates) |>
    add_day_weighting() |>
    tidyr::pivot_wider(
      names_from = "month",
      names_prefix = "month",
      values_from = "month_val",
      values_fill = list(month_val = 0)
    ) |>
    mutate(
      date = round_to_friday(
        dt = .data$date,
        direction = "up"
      )
    ) |>
    rename(week_ending = "date") |>
    group_by(.data$week_ending) |>
    summarise(
      across(starts_with("month"),
             mean),
      .groups = "drop"
    )

  expected_months <- paste0("month", 1:12)
  missing_months <- setdiff(expected_months,
                            names(seasonal))
  seasonal[missing_months] <- 0

  return(seasonal)

}

#' @title add a weighting to each day for the months it can influence
#' @description adds a weighting score between 0 and 1 to indicate how much of a
#'   weight the month that day falls into should influence the model
#' @param data tibble containing a "date" field containing every date in the
#'   time period of interest
#' @import dplyr
#' @importFrom lubridate month day
#' @importFrom rlang .data
add_day_weighting <- function(data) {
  month_earlier <- data |>
    mutate(
      dom = day(.data$date),
      month = case_when(
        .data$dom < 17 ~ case_when(
          month(.data$date) == 1 ~ 12L,
          TRUE ~ as.integer(month(.data$date) - 1)),
        TRUE ~ as.integer(month(.data$date))),
      month_val = case_when(
        .data$dom < 17 ~ (16 - .data$dom) / 30,
        TRUE ~ (46 - .data$dom) / 30))

  month_later <- data |>
    mutate(
      dom = day(.data$date),
      month = case_when(
        .data$dom > 16 ~ case_when(
          month(.data$date) == 12 ~ 1L,
          TRUE ~ as.integer(month(.data$date) + 1)),
        TRUE ~ as.integer(month(.data$date))),
      month_val = case_when(
        .data$dom > 16 ~ (.data$dom - 16) / 30,
        TRUE ~ (14 + .data$dom) / 30))

  data <- bind_rows(month_earlier, month_later) |>
    dplyr::select(!c("dom")) |>
    arrange(.data$month)

  return(data)
}


# functions for predictions -----------------------------------------------


#' @title build the prediction table for the model
#' @description build a data frame for prediction dates with same structure as
#'   modelled data that model can be used to predict deaths on
#' @param areacode character vector of area codes to generate data frame for
#' @param from_date date; a single date to make predications from
#' @param to_date date; a single date to make predictions to
#' @param holidays date; vector containing public holidays
#' @param denominators tibble containing weekly denominator data for each
#'   areacode, sex, age group, deprivation quintile
#' @inheritParams create_dummy_populations
#' @import dplyr
#' @export
build_prediction_dates <- function(
    areacode =  c("E02000001", "E02000002", "E02000003"),
    deprivation_quintile = 1:5,
    sex = 0:1,
    age_group = c("0-24", "25-49", "50-64", "65-74", "75-84", "85+"),
    from_date, to_date, holidays, denominators) {


  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)
  weekly_from_date <- round_to_friday(from_date,
                                      direction = "up")
  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")

  daily_dates <- dplyr::tibble
  (date = seq(from = from_date,
              to = to_date,
              by = 'days'))

  weekly_dates <- seq(from = weekly_from_date,
                      to = to_date,
                      by = 'weeks')

  # create subpopulation variables
  areacode <- factor(areacode)
  sex <- factor(sex)
  age_group <- factor(age_group)
  deprivation_quintile <- factor(deprivation_quintile)


  weekly_dates <- expand.grid(
    week_ending = weekly_dates,
    areacode = areacode,
    sex = sex,
    age_group = age_group,
    deprivation_quintile = deprivation_quintile
  ) |>
    dplyr::tibble()

  # # create denominator variable
  # # aggregate denominator data to weekly
  # denominators <- weekly_denominators(
  #   denominators = denominators,
  #   from_date = from_date,
  #   to_date = to_date
  # )


  # create date-dependent variables
  hol_vars <- weekly_holiday_variables(
    from_date = from_date,
    to_date = to_date,
    holidays = holidays
  )

  trend_var <- weekly_trend_variable(
    from_date = from_date,
    to_date = to_date
  )

  seasonal_vars <- weekly_seasonal_variables(
    from_date = from_date,
    to_date = to_date
  )

  date_dependent_variables <- hol_vars |>
    left_join(seasonal_vars, by = "week_ending") |>
    left_join(trend_var, by = "week_ending")


  # make denominators the correct variable types
  denominators <- denominators |>
    mutate(
      across(
        c(areacode, sex, age_group, deprivation_quintile),
        .fns = factor
      )
    )

  # join variables together
  recent_dates <- weekly_dates |>
    left_join(denominators, by = c(
      "week_ending", "areacode", "sex", "age_group", "deprivation_quintile"
    )) |>
    left_join(date_dependent_variables,
              by = "week_ending") |>
    mutate(registered_deaths = NA)

  return(recent_dates)
}


# modelling functions -----------------------------------------------------

#' simulate distribution around the modelled value
#'
#' @description creates distribution of expected values around the modelled
#'   value
#' @param n number of simulated values
#' @param mu expected value
#' @param dispersion_parameter dispersion parameter for the model
#' @param var variance
#' @importFrom stats rnbinom
#' @export
rqpois <- function(n, mu, dispersion_parameter = NULL, var = NULL) {
  if (!is.null(dispersion_parameter)) {
    theta <- dispersion_parameter
  } else if (!is.null(var)) {
    theta <- var/mu
  } else {
    return("error, you must specify a value for either the var argument or the dispersion parameter argument")
  }

  rqpois <- rnbinom(n = n, mu = mu, size = mu/(theta-1))
  return(rqpois)
}

#' add prediction intervals to the expected value
#'
#' @description add lower and upper prediction interval fields to data frame
#' @param data data frame containing data
#' @importFrom stats quantile
#' @importFrom purrr map map_dbl
#' @importFrom dplyr select mutate
#' @inheritParams rqpois
#' @export
add_prediction_intervals <- function(data, dispersion_parameter) {
  if (dispersion_parameter < 1) dispersion_parameter <- 1

  data <- data |>
    mutate(
      random_total = map(.data$expected_deaths, ~ rqpois(5000, .x, dispersion_parameter)),
      lpb = map_dbl(.data$random_total, ~quantile(.x, 0.00135)),
      upb = map_dbl(.data$random_total, ~quantile(.x, 0.99865))) |>
    dplyr::select(!c("random_total"))
  return(data)
}


