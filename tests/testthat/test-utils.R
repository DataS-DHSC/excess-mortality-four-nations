test_that("round_up_to_saturday converts all days to the right saturday", {
  dates <- seq(
    from = as.Date("2001-01-01"),
    to = as.Date("2020-12-31"),
    by = "days"
  )

  equivalent_saturdays <- round_up_to_saturday(
    dates
  )

  expect_true(
    unique(dates <= equivalent_saturdays),
    info = "all saturdays follow or equal dates provided")


  day_of_week <- unique(
    lubridate::wday(
      equivalent_saturdays,
      abbr = TRUE,
      label = TRUE
  )) |>
    as.character()

  expect_equal(
    day_of_week,
    "Sat",
    info = "only Saturdays are returned from round_up_to_saturday()"
  )
})

test_that("round_to_friday converts all days to the right friday", {
  dates <- seq(
    from = as.Date("2001-01-01"),
    to = as.Date("2020-12-31"),
    by = "days"
  )

  equivalent_up_fridays <- round_to_friday(
    dates,
    direction = "up"
  )

  equivalent_down_fridays <- round_to_friday(
    dates,
    direction = "down"
  )

  expect_true(
    unique(dates <= equivalent_up_fridays),
    info = "all fridays follow or equal dates provided")

  expect_true(
    unique(dates >= equivalent_down_fridays),
    info = "all fridays precede or equal dates provided")


  up_day_of_week <- unique(
    lubridate::wday(
      equivalent_up_fridays,
      abbr = TRUE,
      label = TRUE
    )) |>
    as.character()

  down_day_of_week <- unique(
    lubridate::wday(
      equivalent_down_fridays,
      abbr = TRUE,
      label = TRUE
    )) |>
    as.character()


  expect_equal(
    up_day_of_week,
    "Fri",
    info = "only Fridays are returned from round_to_friday() in the up direction"
  )

  expect_equal(
    down_day_of_week,
    "Fri",
    info = "only Fridays are returned from round_to_friday() in the down direction"
  )
})


test_that("calc_easter_fridays returns the correct dates", {
  bank_holidays_2020 <- as.Date(
    c(
      "2020-01-01",
      "2020-04-10",
      "2020-04-13",
      "2020-05-08",
      "2020-05-25",
      "2020-08-31",
      "2020-12-25",
      "2020-12-28"
    )
  )

  easter_friday <- calc_easter_fridays(
    bank_holidays_2020,
    include_christmas_friday = FALSE
  )

  easter_friday_with_xmas <- calc_easter_fridays(
    bank_holidays_2020,
    include_christmas_friday = TRUE
  )

  expect_equal(
    easter_friday,
    as.Date("2020-04-10"),
    info = "Easter Friday identified without Christmas"
  )

  expect_equal(
    easter_friday_with_xmas,
    as.Date(c("2020-04-10", "2020-12-25")),
    info = "Easter Friday identified with Christmas"
  )


  # error testing calc_easter_fridays ---------------------------------------

  days_in_2020 <- seq(
    from = as.Date("2020-01-01"),
    to = as.Date("2020-12-31"),
    by = "days"
  )

  expect_warning(
    calc_easter_fridays(
      days_in_2020,
      include_christmas_friday = TRUE
    ),
    "The number of holiday dates provided are unusually low or high",
    info = "Check on over supply of dates to the calc_easter_friday_function"
  )

  expect_error(
    calc_easter_fridays(
      as.character(days_in_2020),
      include_christmas_friday = TRUE
    ),
    "holidays must have a Date class",
    info = "ensure the holiday input is a Date"
  )
})


test_that("remove_we_bh_denominators removes denominators for the correct dates", {
  bank_holidays_2020 <- as.Date(
    c(
      "2020-01-01",
      "2020-04-10",
      "2020-04-13",
      "2020-05-08",
      "2020-05-25",
      "2020-08-31",
      "2020-12-25",
      "2020-12-28"
    )
  )

  test_date <- tibble(
    date = as.Date(paste0("2020-01-0", 1:7)),
    day = lubridate::wday(date, label = TRUE),
    denominator = 100
  )

  test_date_output <- remove_we_bh_denominators(
    test_date,
    bank_holidays_2020
  )

  expect_true(
    all(
      test_date_output |>
        filter(.data$day %in% c("Sat", "Sun") |
                 .data$date %in% bank_holidays_2020) |>
        pull(.data$denominator) == 0
    ),
    info = "All bank holidays and weekends have a zero denominator"
  )

  expect_true(
    all(
      test_date_output |>
        filter(!(.data$day %in% c("Sat", "Sun")) &
                 !(.data$date %in% bank_holidays_2020)) |>
        pull(.data$denominator) == 100
    ),
    info = "All non-bank holidays and non-weekends have a 100 denominator"
  )

})

test_that("christmas_week works as expected", {
  expect_equal(
    christmas_week(2023, 2),
    as.Date(paste0("2023-12-2", 3:9)),
    info = "Christmas 2023 on a Monday is identified")

  expect_equal(
    christmas_week(2023, 1),
    as.Date(x = integer(0), origin = "1970-01-01"),
    info = "Christmas falling on a day that isn't of interest returns empty date vector"
  )
})

test_that("sat_to_mon_xmas works as expected", {
  test_data <- tibble(
    date = seq(
      from = as.Date("2023-12-01"),
      to = as.Date("2024-01-31"),
      by = "day"
    )
  ) |>
    sat_to_mon_xmas()

  christmas_dates <- as.Date(paste0("2023-12-2", 3:9))

  expect_equal(
    test_data |>
      filter(.data$sat_to_mon_xmas == TRUE) |>
      pull(date),
    christmas_dates,
    info = "the dates where the week containing a Christmas on a Sat/Sun/Mon is identified"
  )

  expect_equal(
    test_data |>
      filter(.data$week_after_sat_to_mon_xmas == TRUE) |>
      pull(date),
    christmas_dates + 7,
    info = "the week following a week containing a Christmas on a Sat/Sun/Mon is identified"
  )

  expect_equal(
    test_data |>
      filter(.data$week2_after_sat_to_mon_xmas == TRUE) |>
      pull(date),
    christmas_dates + 14,
    info = "the second week following a week containing a Christmas on a Sat/Sun/Mon is identified"
  )

})

