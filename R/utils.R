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


#' converts a date to the following Saturday
#' @param dt date
#' @importFrom lubridate wday
round_up_to_saturday <- function(dt) {
  dt <- dt + (7 - wday(dt))
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

  days_of_week <- wday(dt)
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


#' removes denominators from weekend and bank holiday dates. Within the
#' baseline death period, deaths were very unlikely to be registered on
#' weekends or bank holidays therefore these days should have consistent
#' predictions with working days
#' @param data tibble of data where one field contains the denominator
#' @param denominator_field unquoted field name of the field containing the
#'   denominator
#' @param date_field unquoted field name of the field containing the date
#' @param holidays vector of dates which are bank holidays
remove_we_bh_denominators <- function(data, denominator_field, date_field, holidays) {
  data <- data %>%
    mutate({{ denominator_field }} := case_when(
      wday({{ date_field }}) %in% c(1, 7) ~ 0,
      {{ date_field }} %in% holidays ~ 0,
      TRUE ~ as.numeric({{ denominator_field }})
    ))

  return(data)
}
