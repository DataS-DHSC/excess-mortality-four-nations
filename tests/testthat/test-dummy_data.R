test_that("checking create_dummy_populations", {

  areacodes <- letters[1:3]
  deprivation_quintiles <- 1:2
  sexs <- 0:1
  age_groups <- c("0-49", "50+")
  years <- 2023:2026

  dummy_pops <- create_dummy_populations(
    areacode = areacodes,
    deprivation_quintile = deprivation_quintiles,
    sex = sexs,
    age_group = age_groups,
    year = years
  )

  expect_equal(
    nrow(dummy_pops),
    length(areacodes) * length(deprivation_quintiles) *
      length(sexs) * length(age_groups) *
      length(years),
    info = "expected number of records are produced"
  )

  expect_equal(
    sum(is.na(dummy_pops)),
    0,
    info = "There are no NAs in the dummy populations"
  )

  expect_equal(
    names(dummy_pops),
    c("areacode",
      "deprivation_quintile",
      "sex", "age_group",
      "year", "population"),
    info = "the names produced are as expected"
  )

})

test_that("checking create_dummy_deaths", {

  areacodes <- letters[1:3]
  deprivation_quintiles <- 1:2
  sexs <- 0:1
  age_groups <- c("0-49", "50+")
  input_dates <- as.Date(c("2010-01-01", "2010-02-28"))

  dummy_deaths <- create_dummy_deaths(
    areacode = areacodes,
    deprivation_quintile = deprivation_quintiles,
    sex = sexs,
    age_group = age_groups,
    date_limits = input_dates
  )

  expect_equal(
    as.character(
      unique(
        lubridate::wday(
          dummy_deaths$week_ending,
          label = TRUE))),
    "Fri",
    info = "Only Fridays exist in the week_ending field for dummy_pops"
  )

  expect_equal(
    sum(is.na(dummy_deaths)),
    0,
    info = "There are no NAs in the dummy deaths"
  )

  expect_equal(
    names(dummy_deaths),
    c("areacode",
      "deprivation_quintile",
      "sex", "age_group",
      "week_ending", "registered_deaths"),
    info = "the names produced are as expected"
  )

  expect_equal(
    floor(diff(as.Date(c("2010-01-01", "2010-02-28"))) / 7),
    diff(range(dummy_deaths$week_ending)) / 7,
    info = "The weeks are generated appropriately based on the input dates"
  )

})
