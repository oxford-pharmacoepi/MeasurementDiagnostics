writeSchema <- function(dbToTest = Sys.getenv("DB_TO_TEST", "duckdb")) {
  prefix <- paste0("coco_", sample(letters, 4) |> paste0(collapse = ""), "_")
  switch(dbToTest,
         "duckdb" = c(schema = "main", prefix = prefix),
         "sql server" = c(catalog = "ohdsi", schema = "dbo", prefix = prefix),
         "redshift" = c(schema = "resultsv281", prefix = prefix)
  )
}
connection <- function(dbToTest = Sys.getenv("DB_TO_TEST", "duckdb")) {
  switch(dbToTest,
         "duckdb" = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
         "sql server" = DBI::dbConnect(
           odbc::odbc(),
           Driver = "ODBC Driver 18 for SQL Server",
           Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
           Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
           UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
           PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
           TrustServerCertificate = "yes",
           Port = 1433
         ),
         "redshift" = DBI::dbConnect(
           RPostgres::Redshift(),
           dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
           port = Sys.getenv("CDM5_REDSHIFT_PORT"),
           host = Sys.getenv("CDM5_REDSHIFT_HOST"),
           user = Sys.getenv("CDM5_REDSHIFT_USER"),
           password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
         )
  )
}
copyCdm <- function(cdm) {
  CDMConnector::copyCdmTo(
    con = connection(), cdm = cdm, schema = writeSchema(), overwrite = TRUE
  )
}
testMockCdm <- function() {

  cdm <- omopgenerics::emptyCdmReference(cdmName = "test_mock")
  set.seed(111)

  # person
  birthRange = as.Date(c("1950-01-01", "2000-12-31"))
  proportionFemale = 0.5
  person_id <- seq_len(100)
  dob <- sample(seq(birthRange[1], birthRange[2], by = "day"),
                length(person_id), replace = TRUE)
  gender <- sample(c(8532, 8507), length(person_id), prob = c(proportionFemale, 1 - proportionFemale), TRUE)
  person <- dplyr::tibble(
    person_id = person_id,
    gender_concept_id = as.integer(gender),
    year_of_birth = as.integer(lubridate::year(dob)),
    month_of_birth = as.integer(lubridate::month(dob)),
    day_of_birth = as.integer(lubridate::day(dob)),
    race_concept_id = as.integer(NA),
    ethnicity_concept_id = as.integer(NA),
    location_id = as.integer(NA),
    provider_id = as.integer(NA),
    care_site_id = as.integer(NA),
    person_source_value = NA_character_,
    gender_source_value = NA_character_,
    gender_source_concept_id = as.integer(NA),
    race_source_value = NA_character_,
    race_source_concept_id = as.integer(NA),
    ethnicity_source_value = NA_character_,
    ethnicity_source_concept_id = as.integer(NA)
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name = "person", table = person)

  # observation period
  dob <- cdm$person |>
    dplyr::mutate(
      year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
      month_of_birth1 = as.character(as.integer(.data$month_of_birth)),
      day_of_birth1 = as.character(as.integer(.data$day_of_birth))
    ) |>
    dplyr::mutate(dob := as.Date(
      paste0(.data$year_of_birth1, "-", .data$month_of_birth1, "-", .data$day_of_birth1)
    )) |>
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1")) |>
    dplyr::select("dob") |>
    dplyr::pull()
  start <- dob + floor((as.Date(max(as.Date("2020-01-01"), max(as.Date(dob)))) - dob) * stats::runif(n = length(dob)))
  end <- start + ceiling((as.Date(max(as.Date("2020-01-01"), max(as.Date(dob)))) - start) * stats::runif(n = length(dob)))
  person_id <- dplyr::pull(cdm$person, person_id)
  observationPeriod <- dplyr::tibble(
    observation_period_id = person_id,
    person_id = person_id,
    observation_period_start_date = as.Date(start),
    observation_period_end_date = as.Date(end),
    period_type_concept_id = NA_integer_
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation_period", table = observationPeriod
  )

  # concept
  conceptSet <- c(8507, 8532, 3001467, 45875977, 194152, 4092121, 1033535, 4328749L, 4267416L, 9529)
  conceptName <- c("Male", "Female", "Alkaline phosphatase.bone [Enzymatic activity/volume] in Serum or Plasma", "PhenX", "Renal agenesis and dysgenesis", "Level of mood", "Minimum Data Set", "High", "Low", "kilogram")
  domain <- c("Gender", "Gender", "Measurement", "Measurement", "Condition", "Observation", "Observation", "Meas Value", "Meas Value", "Unit")
  concept <- dplyr::tibble(
    concept_id = conceptSet,
    concept_name = conceptName,
    domain_id = domain,
    vocabulary_id = NA_character_,
    standard_concept = "S",
    concept_class_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = NA,
    valid_end_date = NA,
    invalid_reason = NA
  )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "concept", table = concept
  )

  # measurement
  concept_id <- cdm$concept |>
    dplyr::filter(.data$domain_id == "Measurement" & .data$standard_concept == "S") |>
    dplyr::distinct(concept_id) |>
    dplyr::pull()
  concept_count <- length(concept_id)
  recordDates <- obsDate(cdm$observation_period$observation_period_end_date, cdm$observation_period$observation_period_end_date)
  measurement <- dplyr::tibble(
    measurement_id = 1:100L,
    measurement_concept_id = concept_id[1],
    person_id = sample(
      x = cdm$person |> dplyr::pull("person_id"),
      size = 100,
      replace = TRUE
    ),
    measurement_date = recordDates[[1]],
    measurement_type_concept_id = 1L,
    measurement_datetime = NA,
    measurement_time = NA,
    operator_concept_id = NA,
    range_low = NA,
    range_high = NA,
    provider_id = NA_integer_,
    visit_occurrence_id = NA_integer_,
    visit_detail_id = NA_integer_,
    measurement_source_value = NA_character_,
    measurement_source_concept_id = NA_integer_,
    unit_source_value = NA_character_,
    value_source_value = NA_character_
  )  |>
    dplyr::mutate(
      unit_concept_id = dplyr::if_else(dplyr::row_number()%%2 == 0, 9529, NA),
      value_as_number = dplyr::if_else(dplyr::row_number()<6, NA, seq(from = 5, to = 150, length.out = 100)),
      value_as_concept_id = dplyr::case_when(
        dplyr::row_number()%%3 == 0 ~ 4328749,
        dplyr::row_number()%%3 == 1 ~ 4267416,
        dplyr::row_number()%%3 == 2 ~ NA,
      )
    )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "measurement", table = measurement
  )

  # observation
  concept_id <- cdm$concept |>
    dplyr::filter(.data$domain_id == "Observation" & .data$standard_concept == "S") |>
    dplyr::distinct(concept_id) |>
    dplyr::pull()
  concept_count <- length(concept_id)
  recordDates <- obsDate(cdm$observation_period$observation_period_end_date, cdm$observation_period$observation_period_end_date)
  observation <- dplyr::tibble(
    observation_id = 1:100L,
    observation_concept_id = concept_id[1],
    person_id = sample(
      x = cdm$person |> dplyr::pull("person_id"),
      size = 100,
      replace = TRUE
    ),
    observation_date = recordDates[[1]],
    observation_type_concept_id = 1L,
    observation_datetime = NA,
    observation_time = NA,
    operator_concept_id = NA,
    range_low = NA,
    range_high = NA,
    provider_id = NA_integer_,
    visit_occurrence_id = NA_integer_,
    visit_detail_id = NA_integer_,
    observation_source_value = NA_character_,
    observation_source_concept_id = NA_integer_,
    unit_source_value = NA_character_,
    qualifier_source_value = NA_character_,
    value_as_string = NA,
    qualifier_concept_id = NA
  )  |>
    dplyr::mutate(
      unit_concept_id = dplyr::if_else(dplyr::row_number()%%2 == 0, 9529, NA),
      value_as_number = dplyr::if_else(dplyr::row_number()<6, NA, seq(from = 5, to = 150, length.out = 100)),
      value_as_concept_id = dplyr::case_when(
        dplyr::row_number()%%3 == 0 ~ 4328749,
        dplyr::row_number()%%3 == 1 ~ 4267416,
        dplyr::row_number()%%3 == 2 ~ NA,
      )
    )
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "observation", table = observation
  )

  # cohort
  my_cohort <- list()
  for (i in 1:2) {
    dates <- obsDate(cdm$observation_period$observation_period_end_date, cdm$observation_period$observation_period_end_date)
    my_cohort[[i]] <- dplyr::tibble(
      cohort_definition_id = i,
      subject_id = sample(
        x = cdm$person |> dplyr::pull("person_id"),
        size = 100,
        replace = TRUE
      )
    ) |>
      dplyr::mutate(
        cohort_start_date = dates[[1]],
        cohort_end_date = dates[[2]]
      )
  }
  my_cohort <- my_cohort |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$cohort_definition_id,
                   .data$subject_id,
                   .data$cohort_start_date) |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::mutate(
      next_observation = dplyr::lead(
        x = .data$cohort_start_date,
        n = 1,
        order_by = .data$cohort_start_date
      ),
      cohort_end_date =
        dplyr::if_else(
          .data$cohort_end_date >=
            .data$next_observation &
            !is.na(.data$next_observation),
          .data$next_observation - 1,
          .data$cohort_end_date
        ),
      cohort_end_date = dplyr::if_else(
        .data$cohort_end_date <
          .data$cohort_start_date,
        NA,
        .data$cohort_end_date
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"next_observation") |>
    stats::na.omit() |>
    dplyr::distinct() |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select("subject_id" = "person_id", "observation_period_start_date", "observation_period_end_date"),
      by = "subject_id"
    ) |>
    dplyr::filter(
      cohort_start_date >= observation_period_start_date,
      cohort_end_date <= observation_period_end_date
    ) |>
    dplyr::select(!c("observation_period_start_date", "observation_period_end_date"))
  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "my_cohort", table = my_cohort
  )
  cdm[["my_cohort"]] <- cdm[["my_cohort"]] |>
    omopgenerics::newCohortTable(
      cohortSetRef = dplyr::tibble(cohort_definition_id = 1:2L, cohort_name = paste0("cohort_", 1:2)),
      cohortAttritionRef = NULL
    )
  return(cdm)
}

obsDate <- function(start, end) {
  r1 <- stats::runif(n = length(start))
  start <- start + floor((as.Date(end) - start) * r1)
  r2 <- stats::runif(n = length(start))
  end <- start + ceiling((as.Date(end) - start) * r2)
  end <- pmax(start, end)
  list(start, end)
}
