% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummy_data.R
\name{create_dummy_deaths}
\alias{create_dummy_deaths}
\title{Create a dummy registered deaths dataset for testing}
\usage{
create_dummy_deaths(
  areacode = c("E02000001", "E02000002", "E02000003"),
  deprivation_quintile = 1:5,
  sex = 0:1,
  age_group = c("0-24", "25-49", "50-64", "65-74", "75-84", "85+"),
  date_limits = as.Date(c("2015-01-09", "2019-12-27")),
  incomplete = FALSE
)
}
\arguments{
\item{areacode}{string; areacodes for the areas included in modelling}

\item{deprivation_quintile}{numeric; deprivation quintile number (1 is least
deprived quintile and 5 is the most deprived quintile)}

\item{sex}{0 (male) or 1 (female)}

\item{age_group}{string; describing the age groups used for modelling}

\item{date_limits}{date with two items; item 1 is the date of the Friday at
the end of the first full week of data in the period, and item 2 is the
date of the Friday at the end of the last full week of data in the period}

\item{incomplete}{logical; TRUE will create a tibble with missing records
simulating where there aren't deaths in a specific week/age
group/sex/deprivation quintile. FALSE will provide a complete set of
records for every combination of week, sex, age group and deprivation
quintile}
}
\value{
tibble containing areacode, sex, age_group, deprivation_quintile,
  week_end, registered_deaths
}
\description{
Create a dummy registered deaths dataset for testing
}
\examples{
# creates a tibble of weekly registered deaths with some population
# subgroups missing (because there are no deaths in that subgroup)
df <- create_dummy_deaths(
        areacode = c("abc", "def"),
        incomplete = TRUE)
}
