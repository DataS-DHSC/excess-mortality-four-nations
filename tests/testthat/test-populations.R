test_that("convert_annual_to_monthly_populations works", {
  areacodes <- letters[1:3]
  deprivation_quintiles <- 1:2
  sexs <- 0:1
  age_groups <- c("0-49", "50+")
  years <- 2023:2026
  start_year_of_interest <- 2024
  end_year_of_interest <- 2024


  dummy_pops <- create_dummy_populations(
    areacode = areacodes,
    deprivation_quintile = deprivation_quintiles,
    sex = sexs,
    age_group = age_groups,
    year = years
  )

  monthly_pops <- convert_annual_to_monthly_populations(
    dummy_pops,
    start_year = start_year_of_interest,
    end_year = end_year_of_interest
  )

  expect_equal(
    nrow(monthly_pops),
    length(areacodes) * length(deprivation_quintiles) *
      length(sexs) * length(age_groups) *
      (12 * (end_year_of_interest - start_year_of_interest + 1)),
    info = "expected number of records are produced"
  )

  approximate_total_pops <- monthly_pops |>
    filter(lubridate::month(.data$month) == 7) |>
    pull("denominator") |>
    sum() * 12

  actual_total_pops <- dummy_pops |>
    filter(between(.data$year, start_year_of_interest, end_year_of_interest)) |>
    pull("population") |>
    sum()

  expect_equal(
    between(approximate_total_pops,
            actual_total_pops - (actual_total_pops * 0.1),
            actual_total_pops + (actual_total_pops * 0.1)),
    TRUE,
    info = "monthly pops are in the right ball park when compared to the annual estimates"
  )


  expect_equal(
    names(monthly_pops),
    c("areacode",
      "sex", "age_group",
      "deprivation_quintile",
      "month", "denominator"),
    info = "the names produced are as expected"
  )

  # error checks
  expect_error(
    convert_annual_to_monthly_populations(
      dummy_pops,
      start_year = min(years),
      end_year = max(years)
    ),
    "the denominators dataset provided must contain an extra year of data prior to the start_year value"
  )

  # error checks
  expect_error(
    convert_annual_to_monthly_populations(
      dummy_pops,
      start_year = min(years) + 1,
      end_year = max(years)
    ),
    "the denominators dataset provided must contain an extra year of data following to the end_year value"
  )

  expect_error(
    convert_annual_to_monthly_populations(
      dummy_pops,
      start_year = 2025,
      end_year = 2024
    ),
    "start_year must be less than end_year"
  )
})


test_that("weekly_denominators works", {
  areacodes <- letters[1:3]
  deprivation_quintiles <- 1:2
  sexs <- 0:1
  age_groups <- c("0-49", "50+")
  years <- 2023:2026
  start_year_of_interest <- 2024
  end_year_of_interest <- 2024


  dummy_pops <- create_dummy_populations(
    areacode = areacodes,
    deprivation_quintile = deprivation_quintiles,
    sex = sexs,
    age_group = age_groups,
    year = years
  )

  monthly_pops <- convert_annual_to_monthly_populations(
    dummy_pops,
    start_year = start_year_of_interest,
    end_year = end_year_of_interest
  )

  weekly_pops <- weekly_denominators(
    monthly_pops,
    from_date = as.Date("2024-01-02"),
    to_date = as.Date("2024-04-15"),
    holidays = as.Date(c(
      "2024-01-01",
      "2024-03-29",
      "2024-04-01",
      "2024-05-06",
      "2024-05-27",
      "2024-08-26",
      "2024-12-25",
      "2024-12-26"))
  )

  expect_equal(
    weekly_pops |>
      filter(.data$week_ending == as.Date("2024-01-12")) |>
      pull("denominator") |>
      sum(),
    monthly_pops |>
      filter(.data$month == as.Date("2024-01-01")) |>
      pull("denominator") |>
      sum() * 5 / 7, # because 2 days are removed for the weekend
    info = "Weekly pops give the right totals compared to monthly pops"
  )

  expect_equal(
    names(weekly_pops),
    c("week_ending",
      "areacode",
      "sex", "age_group",
      "deprivation_quintile",
      "denominator"),
    info = "the names produced are as expected"
  )

  expect_equal(
    sum(is.na(weekly_pops)),
    0,
    info = "there are no NAs in the dataset"
  )

  # error checks
  expect_error(
    weekly_denominators(
      monthly_pops,
      from_date = as.Date("2024-01-02"),
      to_date = as.Date("2025-04-15"),
      holidays = as.Date(c(
        "2024-01-01",
        "2024-03-29",
        "2024-04-01",
        "2024-05-06",
        "2024-05-27",
        "2024-08-26",
        "2024-12-25",
        "2024-12-26"))
    ),
    "denominators provided finish before the to_date input"
  )

  expect_error(
    weekly_denominators(
      monthly_pops,
      from_date = as.Date("2023-01-02"),
      to_date = as.Date("2024-04-15"),
      holidays = as.Date(c(
        "2024-01-01",
        "2024-03-29",
        "2024-04-01",
        "2024-05-06",
        "2024-05-27",
        "2024-08-26",
        "2024-12-25",
        "2024-12-26"))
    ),
    "denominators provided start after the from_date input"
  )
})
