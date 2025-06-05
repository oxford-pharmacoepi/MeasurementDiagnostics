#' Diagnostics of a codelist of measurement codes in the database
#'
#' @param cdm A reference to the cdm object.
#' @param codes A codelist of measurement codes for which to perform diagnostics.
#'
#' @return A summarised result
#' @export
#'
#' @examples
#' \donttest{
#' library(measurementDiagnostics)
#'
#' cdm <- mockMeasurementDiagnostics()
#'
#' result <- measurementDiagnostics(
#'   cdm = cdm, codes = list("test_codelist" = c(3001467L, 45875977L))
#' )
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
measurementDiagnostics <- function(cdm,
                                   codes) {
  # check inputs
  codes <- omopgenerics::validateConceptSetArgument(codes)
  prefix <- omopgenerics::tmpPrefix()

  result <- measurementDiagnosticsInternal(
    cdm = cdm,
    codes = codes,
    cohort = NULL,
    timing = "any",
    timingName = NULL
  )

  return(result)
}


measurementDiagnosticsInternal <- function(cdm,
                                           codes,
                                           cohort,
                                           timing,
                                           timingName) {
  prefix <- omopgenerics::tmpPrefix()

  if (is.null(cohort)) {
    cohortName <- NULL
  } else {
    cohort <- omopgenerics::validateCohortArgument(cohort)
    cohortName <- omopgenerics::tableName(cohort)
  }

  installedVersion <- as.character(utils::packageVersion("measurementDiagnostics"))

  ## measurement cohort
  # settings
  measurementSettings <- purrr::imap_dfr(
    .x = codes,
    .f = ~ dplyr::tibble(cohort_name = paste0(.y, "_", .x), codelist_name = .y, concept_id = .x)
  ) |>
    dplyr::mutate(cohort_definition_id = dplyr::row_number())
  settingsTableName <- omopgenerics::uniqueTableName(prefix = prefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = settingsTableName,
    table = measurementSettings
  )
  cdm[[settingsTableName]] <- cdm[[settingsTableName]] |>
    dplyr::left_join(
      cdm$concept |> dplyr::select(dplyr::all_of(c("concept_id", "concept_name", "domain_id"))),
      by = "concept_id"
    )
  nStart <- dplyr::pull(dplyr::tally(cdm[[settingsTableName]]))
  cdm[[settingsTableName]] <- cdm[[settingsTableName]] |>
    dplyr::filter(tolower(.data$domain_id) == "measurement") |>
    dplyr::compute(name = settingsTableName, temporary = FALSE)
  nEnd <- dplyr::pull(dplyr::tally(cdm[[settingsTableName]]))
  if (nStart != nEnd) cli::cli_inform(c("!" = "{nStart-nEnd} concept{?s} excluded for not being in the measurement domain"))
  addIndex(cdm[[settingsTableName]], cols = "concept_id")

  # cohort
  cli::cli_inform(c(">" = "Subsetting measurement table to the subjects and timing of interest."))
  measurementCohortName <- omopgenerics::uniqueTableName(prefix = prefix)
  # subset to cohort and timing
  measurement <- subsetMeasurementTable(cdm, cohortName, timing, measurementCohortName)
  cli::cli_inform(c(">" = "Getting measurement records based on measurement codes."))
  measurement <- measurement |>
    dplyr::rename("concept_id" = "measurement_concept_id") |>
    dplyr::inner_join(
      cdm[[settingsTableName]] |>
        dplyr::select(dplyr::all_of(c("cohort_definition_id", "concept_id", "codelist_name"))),
      by = "concept_id"
    ) |>
    dplyr::select(dplyr::all_of(c(
      "cohort_definition_id", "subject_id" = "person_id",
      "cohort_start_date" = "measurement_date", "measurement_id",
      "codelist_name", "concept_id", "unit_concept_id", "value_as_number",
      "value_as_concept_id"
    ))) |>
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      unit_concept_id = as.integer(.data$unit_concept_id),
      value_as_concept_id = as.integer(.data$value_as_concept_id)
    ) |>
    dplyr::compute(name = measurementCohortName, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = measurementSettings,
      .softValidation = TRUE # allow overlap
    ) |>
    PatientProfiles::addDemographics(
      ageGroup = list(c(0, 17), c(18, 64), c(65, 150)),
      priorObservation = FALSE,
      futureObservation = FALSE,
      name = measurementCohortName
    )

  if (dplyr::pull(dplyr::tally(measurement)) == 0) {
    cli::cli_warn("No records with the measurement codes were found.")
    return(omopgenerics::emptySummarisedResult())
  }

  ## measurements per subject
  cli::cli_inform(c(">" = "Getting time between records per person."))
  cohortTable <- "cohort_table"[!is.null(cohortName)]
  measurementTiming <- measurement |>
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
    dplyr::arrange(.data$cohort_start_date) |>
    dplyr::mutate(previous_measurement = dplyr::lag(.data$cohort_start_date)) %>%
    dplyr::mutate(time = !!CDMConnector::datediff("previous_measurement", "cohort_start_date")) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    PatientProfiles::summariseResult(
      group = list(c("codelist_name", "concept_id")),
      includeOverallGroup = FALSE,
      strata = list("sex", "age_group", c("age_group", "sex")),
      includeOverallStrata = TRUE,
      variables = "time",
      estimates = c("min", "q25", "median", "q75", "max"),
      counts = TRUE
    ) |>
    suppressMessages() |>
    omopgenerics::splitGroup() |>
    groupIdToName(newSet = cdm[[settingsTableName]] |> dplyr::collect()) |>
    dplyr::select(!dplyr::starts_with("additional"))
  # fill 0 counts
  concepts <- as.character(cdm[[settingsTableName]] |> dplyr::pull("concept_id"))
  empty <- !concepts %in% unique(measurementTiming$concept_id)
  if (any(empty)){
    measurementTiming <- dplyr::bind_rows(
      measurementTiming,
      dplyr::tibble(
        result_id = 1L,
        strata_name = "overall",
        strata_level = "overall",
        variable_level = NA_character_,
        estimate_name = "count",
        estimate_type = "integer",
        estimate_value = "0",
        concept_id = concepts[empty]
      ) |>
        dplyr::inner_join(
          cdm[[settingsTableName]] |>
            dplyr::collect() |>
            dplyr::mutate(concept_id = as.character(.data$concept_id)) |>
            dplyr::select(dplyr::all_of(c("concept_name", "concept_id", "codelist_name"))),
          by = "concept_id"
        ) |>
        omopgenerics::uniteGroup(cols = c("codelist_name", "concept_name")) |>
        dplyr::cross_join(dplyr::tibble(variable_name = c("number records", "number subjects")))
    )
  }
  # to summarise result
  measurementTiming <- measurementTiming |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm),
      cohort_table = cohortName
    ) |>
    omopgenerics::uniteAdditional(cols = c("concept_id", cohortTable)) |>
    dplyr::select(omopgenerics::resultColumns()) |>
    omopgenerics::newSummarisedResult(
      settings = omopgenerics::settings(measurementTiming) |>
        dplyr::mutate(
          result_type = "measurement_records",
          package_name = "measurementDiagnostics",
          package_version = installedVersion,
          group = "codelist_name &&& concept_name",
          additional = paste0(c("concept_id", cohortTable), collapse = " &&& "),
          timing = timingName
        )
    )

  ## measurement value
  cli::cli_inform(c(">" = "Summarising measurement results - value as number."))
  # as numeric
  # 1) summarise numeric distribution
  measurementNumeric <- measurement |>
    dplyr::select(!"value_as_concept_id") |>
    dplyr::collect() |>
    PatientProfiles::summariseResult(
      group = list(c("codelist_name", "concept_id", "unit_concept_id")),
      includeOverallGroup = FALSE,
      strata = list("sex", "age_group", c("age_group", "sex")),
      includeOverallStrata = TRUE,
      variables = "value_as_number",
      estimates = c("min", "q25", "median", "q75", "max", "count_missing", "percentage_missing"),
      counts = TRUE,
      weights = NULL
    ) |>
    suppressMessages() |>
    transformMeasurementValue(
      cdm = cdm, newSet = cdm[[settingsTableName]] |> dplyr::collect(),
      cohortName = cohortName, installedVersion = installedVersion,
      timing = timingName
    )

  # counts as concept
  cli::cli_inform(c(">" = "Summarising measurement results - value as concept."))
  measurementConcept <- measurement |>
    dplyr::mutate(value_as_concept_id = as.character(.data$value_as_concept_id)) |>
    PatientProfiles::summariseResult(
      group = list(c("codelist_name", "concept_id")),
      includeOverallGroup = FALSE,
      strata = list("sex", "age_group", c("age_group", "sex")),
      includeOverallStrata = TRUE,
      variables = "value_as_concept_id",
      estimates = c("count", "percentage"),
      counts = FALSE,
      weights = NULL
    ) |>
    suppressMessages() |>
    transformMeasurementConcept(
      cdm = cdm, newSet = cdm[[settingsTableName]] |> dplyr::collect(),
      cohortName = cohortName, installedVersion = installedVersion,
      timing = timingName
    )

  cli::cli_inform(c(">" = "Binding all diagnostic results."))
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(prefix))

  return(
    omopgenerics::bind(
      measurementTiming, measurementNumeric, measurementConcept
    )
  )
}


groupIdToName <- function(x, newSet, cols = c("codelist_name", "concept_name")) {
  x |>
    dplyr::inner_join(
      newSet |>
        dplyr::select(dplyr::all_of(c("concept_id", "concept_name"))) |>
        dplyr::mutate(concept_id = as.character(.data$concept_id)),
      by = "concept_id"
    ) |>
    omopgenerics::uniteGroup(cols = cols)
}

subsetMeasurementTable <- function(cdm, cohortName, timing, name) {
  # if ANY : no need to filter for dates
  # if DURING : needs to be in observation / in cohort
  # if COHORT_START_DATE : cohort_start_date/observation_period_start_date = measurement date
  if (is.null(cohortName) & timing == "any") {
    return(
      cdm$measurement |>
        dplyr::compute(name = name, temporary = FALSE)
    )
  }
  cohort <- CohortConstructor::addCohortTableIndex(cdm[[cohortName]])
  if (timing == "during") {
    measurement <- cdm$measurement |>
      dplyr::inner_join(
        cohort |>
          dplyr::select(
            "person_id" = "subject_id", "cohort_start_date", "cohort_end_date"
          ),
        by = "person_id",
        relationship = "many-to-many"
      ) |>
      dplyr::filter(
        .data$measurement_date >= .data$cohort_start_date,
        .data$measurement_date <= .data$cohort_end_date
      ) |>
      dplyr::select(!dplyr::starts_with("cohort_")) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  if (timing == "cohort_start_date") {
    measurement <-   measurement <- cdm$measurement |>
      dplyr::inner_join(
        cohort |>
          dplyr::select(
            "person_id" = "subject_id", "measurement_date" = "cohort_start_date"
          ),
        by = c("person_id", "measurement_date"),
        relationship = "many-to-many"
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  if (timing == "any") {
    measurement <-   measurement <- cdm$measurement |>
      dplyr::inner_join(
        cohort |>
          dplyr::select("person_id" = "subject_id") |>
          dplyr::distinct(),
        by = c("person_id")
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  return(measurement)
}

addIndex <- function(cohort, cols) {
  # From CohortConstructor
  cdm <- omopgenerics::cdmReference(cohort)
  name <- omopgenerics::tableName(cohort)

  tblSource <- attr(cohort, "tbl_source")
  if(is.null(tblSource)){
    return(invisible(NULL))
  }
  dbType <- attr(tblSource, "source_type")
  if(is.null(dbType)){
    return(invisible(NULL))
  }

  if (dbType == "postgresql") {
    con <- attr(cdm, "dbcon")
    schema <- attr(cdm, "write_schema")
    if(length(schema) > 1){
      prefix <- attr(cdm, "write_schema")["prefix"]
      schema <- attr(cdm, "write_schema")["schema"]
    } else {
      prefix <- NULL
    }

    existingIndex <- DBI::dbGetQuery(con,
                                     paste0("SELECT * FROM pg_indexes WHERE",
                                            " schemaname = '",
                                            schema,
                                            "' AND tablename = '",
                                            paste0(prefix, name),
                                            "';"))
    if(nrow(existingIndex) > 0){
      cli::cli_inform("Index already existing so no new index added.")
      return(invisible(NULL))
    } else {
      cli::cli_inform("Adding indexes to table")
    }

    cols <- paste0(cols, collapse = ",")

    query <- paste0(
      "CREATE INDEX ON ",
      paste0(schema, ".", prefix, name),
      " (",
      cols,
      ");"
    )
    suppressMessages(DBI::dbExecute(con, query))
  }

  return(invisible(NULL))
}

transformMeasurementValue <- function(x, cdm, newSet, cohortName, installedVersion, timing) {
  cohortTable <- "cohort_table"[!is.null(cohortName)]
  x |>
    dplyr::filter(.data$variable_name != "number subjects") |>
    omopgenerics::splitGroup() |>
    omopgenerics::splitAdditional() |>
    dplyr::left_join(
      cdm$concept |>
        dplyr::select(
          "unit_concept_id" = "concept_id",
          "unit_concept_name" = "concept_name"
        ) |>
        dplyr::mutate(unit_concept_id = as.character(.data$unit_concept_id)) |>
        dplyr::collect(),
      by = "unit_concept_id"
    ) |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm),
      unit_concept_name = dplyr::if_else(is.na(.data$unit_concept_name), "NA", .data$unit_concept_name),
    ) |>
    groupIdToName(newSet = newSet, cols = c("codelist_name", "concept_name", "unit_concept_name")) |>
    dplyr::mutate(cohort_table = cohortName) |>
    omopgenerics::uniteAdditional(cols = c("concept_id", "unit_concept_id", cohortTable)) |>
    dplyr::select(omopgenerics::resultColumns()) |>
    omopgenerics::newSummarisedResult(
      settings = omopgenerics::settings(x) |>
        dplyr::mutate(
          result_type = "measurement_value_as_numeric",
          package_name = "measurementDiagnostics",
          package_version = installedVersion,
          group = "codelist_name &&& concept_name &&& unit_concept_name",
          additional = paste0(c("concept_id", "unit_concept_id", cohortTable), collapse = " &&& "),
          timing = timing
        )
    )
}

transformMeasurementConcept <- function(x, cdm, newSet, cohortName,
                                        installedVersion, timing) {
  cohortTable <- "cohort_table"[!is.null(cohortName)]
  x |>
    dplyr::select(!c("additional_name", "additional_level")) |>
    dplyr::rename("value_as_concept_id" = "variable_level") |>
    dplyr::left_join(
      cdm$concept |>
        dplyr::select(
          "variable_level" = "concept_name",
          "value_as_concept_id" = "concept_id"
        ) |>
        dplyr::mutate(value_as_concept_id = as.character(.data$value_as_concept_id)) |>
        dplyr::collect(),
      by = "value_as_concept_id"
    ) |>
    dplyr::mutate(
      variable_name = gsub("_id", "_name", "value_as_concept_id"),
      cohort_table = cohortName,
      value_as_concept_id = dplyr::if_else(is.na(.data$value_as_concept_id), "NA", .data$value_as_concept_id)
    ) |>
    omopgenerics::splitGroup() |>
    groupIdToName(newSet = newSet) |>
    omopgenerics::uniteAdditional(cols = c("concept_id", "value_as_concept_id", cohortTable)) |>
    dplyr::select(omopgenerics::resultColumns()) |>
    omopgenerics::newSummarisedResult(
      settings = omopgenerics::settings(x) |>
        dplyr::mutate(
          result_type = "measurement_value_as_concept",
          package_name = "measurementDiagnostics",
          package_version = installedVersion,
          group = "codelist_name &&& concept_name",
          additional = paste0(c("concept_id", "value_as_concept_id", cohortTable), collapse = " &&& "),
          timing = timing
        )
    )
}
