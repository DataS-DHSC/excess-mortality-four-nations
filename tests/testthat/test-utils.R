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
      pull("date"),
    christmas_dates,
    info = "the dates where the week containing a Christmas on a Sat/Sun/Mon is identified"
  )

  expect_equal(
    test_data |>
      filter(.data$week_after_sat_to_mon_xmas == TRUE) |>
      pull("date"),
    christmas_dates + 7,
    info = "the week following a week containing a Christmas on a Sat/Sun/Mon is identified"
  )

  expect_equal(
    test_data |>
      filter(.data$week2_after_sat_to_mon_xmas == TRUE) |>
      pull("date"),
    christmas_dates + 14,
    info = "the second week following a week containing a Christmas on a Sat/Sun/Mon is identified"
  )

})

test_that("consecutive_bank_hols works as expected", {
  test_data <- tibble(
    date = seq(
      from = as.Date("2019-12-01"),
      to = as.Date("2020-01-31"),
      by = "days"
    )
  )

  pretend_bank_holidays <- as.Date(
    c("2019-12-02", "2019-12-03",
      "2019-12-10", "2019-12-11",
      "2019-12-25", "2019-12-26")
  )

  expected_dates <- as.Date(c("2019-12-13", "2019-12-27"))

  expect_identical(
    test_data |>
      consecutive_bank_hols(
        pretend_bank_holidays) |>
      filter(.data$consecutive_bh) |>
      pull("date"),
    expected_dates,
    info = "consecutive bank holidays that don't include a Monday or Friday are identified"
  )


})

test_that("create_date_dependent_variables works as expected", {
  from_date <- as.Date("2022-01-01")
  to_date <- as.Date("2022-12-31")

  bank_holidays_2022 <- as.Date(
    c(
      "2022-01-03",
      "2022-04-15",
      "2022-04-18",
      "2022-05-02",
      "2022-06-02",
      "2022-06-03",
      "2022-08-29",
      "2022-09-19",
      "2022-12-26",
      "2022-12-27"
    )
  )

  test_data <- create_date_dependent_variables(
    from_date = from_date,
    to_date = to_date,
    holiday = bank_holidays_2022
  )

  expect_identical(
    test_data |>
      filter(
        if_any(
          starts_with("easter"),
          ~ . == 1
        )
      ) |>
      pull("week_ending"),
    as.Date(c("2022-04-15", "2022-04-22", "2022-04-29")),
    info = "easter variables are identified correctly for 2022"
  )


  expect_identical(
    test_data |>
      filter(
        if_any(
          ends_with("BH",
                    ignore.case = FALSE),
          ~ . == 1
        )
      ) |>
      pull("week_ending"),
    as.Date(c(
      "2022-04-29",
      "2022-05-06",
      "2022-08-26",
      "2022-09-02",
      "2022-09-16",
      "2022-09-23")),
    info = "bank holiday variables are identified correctly for 2022"
  )

  expect_identical(
    test_data |>
      filter(
        if_any(
          ends_with("mon_xmas"),
          ~ . == 1
        )
      ) |>
      pull("week_ending"),
    as.Date(c(
      "2022-12-30")),
    info = "Monday Christmas variables are identified correctly for 2022"
  )

  expect_identical(
    test_data |>
      filter(
        if_any(
          ends_with("thu_xmas"),
          ~ . == 1
        )
      ) |>
      pull("week_ending"),
    as.Date(x = integer(0), origin = "1970-01-01"),
    info = "Tuesday to Thursday Christmas variables are identified correctly for 2022 as none"
  )

  expect_identical(
    test_data |>
      filter(.data$consecutive_bh == 1) |>
      pull("week_ending"),
    as.Date("2022-06-03"),
    info = "consecutive bank holiday variable is identified correctly for 2022"
  )

  month_sums <- range(colSums(select(test_data, starts_with("month"))))

  expect_gt(
    month_sums[1],
    4.15,
    label = "The lowest sum of month variables over a year is above 4.15"
  )

  expect_lt(
    month_sums[2],
    4.5,
    label = "The highest sum of month variables over a year is above 4.5"
  )

  years_from_2016_range <- range(test_data$years_from_20161231)

  expect_gt(
    years_from_2016_range[1],
    5,
    label = "The difference between the closest week in 2022 to the end of 2016 is over 5 years"
  )

  expect_lt(
    years_from_2016_range[2],
    6,
    label = "The difference between the furthest week in 2022 to the end of 2016 is under 6 years"
  )

})

test_that("build_prediction_dates works as expected", {
  areacodes <- letters[1:3]
  deprivation_quintiles <- 1:2
  sexs <- 0:1
  age_groups <- c("0-49", "50+")
  years <- 2023:2027

  from_date <- as.Date("2024-01-01")
  to_date <- as.Date("2025-12-31")

  pretend_bank_holidays <- as.Date(
    c("2024-12-02", "2024-12-03",
      "2024-12-10", "2024-12-11",
      "2024-12-25", "2024-12-26",
      "2025-12-02", "2025-12-03",
      "2025-12-10", "2025-12-11",
      "2025-12-25", "2025-12-26")
  )


  dummy_pops <- create_dummy_populations(
    areacode = areacodes,
    deprivation_quintile = deprivation_quintiles,
    sex = sexs,
    age_group = age_groups,
    year = years
  ) |>
    convert_annual_to_weekly_populations(
      from_date = from_date,
      to_date = to_date,
      holidays = pretend_bank_holidays
  )

  predictors <- build_prediction_dates(
    areacode = areacodes,
    deprivation_quintile = deprivation_quintiles,
    sex = sexs,
    age_group = age_groups,
    from_date = from_date,
    to_date = to_date,
    holidays = pretend_bank_holidays,
    denominators = dummy_pops)

  expect_equal(
    names(predictors),
    c("week_ending",
      "areacode",
      "sex",
      "age_group",
      "deprivation_quintile",
      "denominator",
      "easter_pre",
      "easter_post_1",
      "easter_post_2",
      "wk_nearest_BH",
      "wk_next_nearest_BH",
      "wk_sat_to_mon_xmas",
      "wk_post_sat_to_mon_xmas",
      "wk2_post_sat_to_mon_xmas",
      "consecutive_bh",
      "wk_after_tue_to_thu_xmas",
      "wk2_after_tue_to_thu_xmas",
      "month1",
      "month2",
      "month3",
      "month4",
      "month5",
      "month6",
      "month7",
      "month8",
      "month9",
      "month10",
      "month11",
      "month12",
      "years_from_20161231"),
    info = "All expected fields are produced in predictor matrix"
  )

  expect_equal(
    sum(is.na(predictors)),
    0,
    info = "There are no NA values in the predictor table"
  )

})

