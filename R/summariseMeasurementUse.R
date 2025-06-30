#' Diagnostics of a codelist of measurement codes in the database
#'
#' @inheritParams summariseMeasurementDoc
#'
#' @return A summarised result
#' @export
#'
#' @examples
#' \donttest{
#' library(MeasurementDiagnostics)
#'
#' cdm <- mockMeasurementDiagnostics()
#'
#' result <- summariseMeasurementUse(
#'   cdm = cdm, codes = list("test_codelist" = c(3001467L, 45875977L))
#' )
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
summariseMeasurementUse <- function(cdm,
                                    codes,
                                    byConcept = TRUE,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL,
                                    dateRange = as.Date(c(NA, NA)),
                                    checks = c("measurement_timings", "measurement_value_as_numeric", "measurement_value_as_concept")) {
  # check inputs
  cdm <- omopgenerics::validateCdmArgument(cdm)
  prefix <- omopgenerics::tmpPrefix()

  result <- summariseMeasurementUseInternal(
    cdm = cdm,
    codes = codes,
    cohort = NULL,
    timing = "any",
    timingName = NULL,
    byConcept = byConcept,
    byYear = byYear,
    bySex = bySex,
    ageGroup = ageGroup,
    dateRange = dateRange,
    checks = checks
  )

  return(result)
}


summariseMeasurementUseInternal <- function(cdm,
                                            codes,
                                            cohort,
                                            timing,
                                            timingName,
                                            byConcept,
                                            byYear,
                                            bySex,
                                            ageGroup,
                                            dateRange,
                                            checks) {
  # checks
  codes <- omopgenerics::validateConceptSetArgument(codes)
  ageGroup <- omopgenerics::validateAgeGroupArgument(ageGroup = ageGroup)
  omopgenerics::assertLogical(byConcept, length = 1)
  omopgenerics::assertLogical(byYear, length = 1)
  omopgenerics::assertLogical(bySex, length = 1)
  omopgenerics::assertDate(dateRange, length = 2, na = TRUE)
  omopgenerics::assertChoice(
    checks, choices = c("measurement_timings", "measurement_value_as_numeric", "measurement_value_as_concept")
  )
  if (all(!is.na(dateRange))) {
    if (dateRange[1] > dateRange[2]) {
      cli::cli_abort("First date component in `dateRange` must be smaller than the second.")
    }
  }

  if (length(checks) == 0) return(NULL)

  prefix <- omopgenerics::tmpPrefix()

  if (is.null(cohort)) {
    cohortName <- NULL
  } else {
    cohort <- omopgenerics::validateCohortArgument(cohort)
    cohortName <- omopgenerics::tableName(cohort)
  }

  installedVersion <- as.character(utils::packageVersion("MeasurementDiagnostics"))

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
    dplyr::filter(tolower(.data$domain_id) %in% c("measurement", "observation")) |>
    dplyr::compute(name = settingsTableName, temporary = FALSE)
  nEnd <- dplyr::pull(dplyr::tally(cdm[[settingsTableName]]))
  if (nStart != nEnd) cli::cli_inform(c("!" = "{nStart-nEnd} concept{?s} excluded for not being in the measurement/observation domain"))
  addIndex(cdm[[settingsTableName]], cols = "concept_id")

  # cohort
  measurementCohortName <- omopgenerics::uniqueTableName(prefix = prefix)
  cdm[[measurementCohortName]] <- getCohortFromCodes(cdm, codes, settingsTableName, name = measurementCohortName)

  cli::cli_inform(c(">" = "Subsetting records to the subjects and timing of interest."))
  # subset to cohort and timing
  measurement <- subsetMeasurementTable(cdm, cohortName, timing, measurementCohortName, dateRange)

  measurement <- measurement |>
    dplyr::rename("subject_id" = "person_id", "cohort_start_date" = "record_date") |>
    dplyr::mutate(cohort_definition_id = 1L, cohort_end_date = .data$cohort_start_date) |>
    dplyr::compute(name = measurementCohortName, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = measurementSettings,
      .softValidation = TRUE # allow overlap
    )

  if (dplyr::pull(dplyr::tally(measurement)) == 0) {
    cli::cli_warn("No records were found.")
    return(omopgenerics::emptySummarisedResult())
  }

  # group and strata
  baseGroup <- c("cohort_name", "codelist_name")[c(!is.null(cohort), TRUE)]
  strata <- as.list(c("sex", "age_group", "year")[c(bySex, length(ageGroup)>0, byYear)])
  measurement <- measurement |> addStrata(bySex, byYear, ageGroup, measurementCohortName)

  ## measurements per subject
  if ("measurement_timings" %in% checks) {
    cli::cli_inform(c(">" = "Getting time between records per person."))
    measurementTiming <- measurement |>
      dplyr::group_by(.data$codelist_name, .data$subject_id) |>
      dplyr::arrange(.data$cohort_start_date) |>
      dplyr::mutate(previous_measurement = dplyr::lag(.data$cohort_start_date)) %>%
      dplyr::mutate(time = !!CDMConnector::datediff("previous_measurement", "cohort_start_date")) |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      PatientProfiles::summariseResult(
        group = list(baseGroup),
        includeOverallGroup = FALSE,
        strata = strata,
        includeOverallStrata = TRUE,
        variables = "time",
        estimates = c("min", "q25", "median", "q75", "max"),
        counts = TRUE
      ) |>
      suppressMessages() |>
      transformMeasurementRecords(
        cdm, newSet = cdm[[settingsTableName]] |> dplyr::collect(),
        installedVersion, timingName, cohortName, dateRange
      )
  } else {
    measurementTiming <- NULL
  }

  ## measurement value
  if ("measurement_value_as_numeric" %in% checks) {
    cli::cli_inform(c(">" = "Summarising results - value as number."))
    # as numeric
    # 1) summarise numeric distribution
    measurementNumeric <- measurement |>
      dplyr::select(!"value_as_concept_id") |>
      dplyr::collect() |>
      PatientProfiles::summariseResult(
        group = list(c(baseGroup, "unit_concept_id"), c(baseGroup, "concept_id", "unit_concept_id"))[c(TRUE, byConcept)],
        includeOverallGroup = FALSE,
        strata = strata,
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
        timing = timingName, byConcept = byConcept, dateRange
      )
  } else {
    measurementNumeric <- NULL
  }

  ## counts as concept
  if ("measurement_value_as_concept" %in% checks) {
    cli::cli_inform(c(">" = "Summarising results - value as concept."))
    measurementConcept <- measurement |>
      dplyr::mutate(value_as_concept_id = as.character(.data$value_as_concept_id)) |>
      PatientProfiles::summariseResult(
        group = list(baseGroup, c(baseGroup, "concept_id"))[c(TRUE, byConcept)],
        includeOverallGroup = FALSE,
        strata = strata,
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
        timing = timingName, byConcept = byConcept, dateRange
      )
  } else {
    measurementConcept <- NULL
  }

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
    dplyr::left_join(
      newSet |>
        dplyr::select(dplyr::all_of(c("concept_id", "concept_name", "domain_id"))) |>
        dplyr::mutate(concept_id = as.character(.data$concept_id)),
      by = "concept_id"
    ) |>
    omopgenerics::uniteGroup(cols = cols)
}

subsetMeasurementTable <- function(cdm, cohortName, timing, name, dateRange) {
  # if ANY : no need to filter for dates
  # if DURING : needs to be in observation / in cohort
  # if COHORT_START_DATE : cohort_start_date/observation_period_start_date = measurement date

  if (is.null(cohortName) & timing == "any") {
    return(
      cdm[[name]] |>
        measurementInDateRange(dateRange, name)
    )
  }
  cohort <- CohortConstructor::addCohortTableIndex(cdm[[cohortName]])
  if (timing == "during") {
    measurement <- cdm[[name]] |>
      dplyr::inner_join(
        cohort |>
          PatientProfiles::addCohortName() |>
          dplyr::select(
            "person_id" = "subject_id", "cohort_start_date", "cohort_end_date", "cohort_name"
          ),
        by = "person_id",
        relationship = "many-to-many"
      ) |>
      dplyr::filter(
        .data$record_date >= .data$cohort_start_date,
        .data$record_date <= .data$cohort_end_date
      ) |>
      dplyr::select(!dplyr::all_of(c("cohort_start_date", "cohort_end_date"))) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  if (timing == "cohort_start_date") {
    measurement <-   measurement <- cdm[[name]] |>
      dplyr::inner_join(
        cohort |>
          PatientProfiles::addCohortName() |>
          dplyr::select(
            "person_id" = "subject_id", "record_date" = "cohort_start_date", "cohort_name"
          ),
        by = c("person_id", "record_date"),
        relationship = "many-to-many"
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  if (timing == "any") {
    measurement <-   measurement <- cdm[[name]] |>
      dplyr::inner_join(
        cohort |>
          PatientProfiles::addCohortName() |>
          dplyr::select("person_id" = "subject_id", "cohort_name") |>
          dplyr::distinct(),
        by = c("person_id")
      ) |>
      dplyr::compute(name = name, temporary = FALSE)
  }

  measurement <- measurement |> measurementInDateRange(dateRange, name)
  return(measurement)
}

measurementInDateRange <- function(x, dateRange, name) {
  if (!is.na(dateRange[1])) {
    x <- x |>
      dplyr::filter(.data$record_date >= !!dateRange[1])
  }
  if (!is.na(dateRange[2])) {
    x <- x |>
      dplyr::filter(.data$record_date <= !!dateRange[2])
  }
  return(
    x |> dplyr::compute(name = name, temporary = FALSE)
  )
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

transformMeasurementRecords <- function(x, cdm, newSet, installedVersion, timingName, cohortName, dateRange) {
  x <- x |>
    dplyr::select(!dplyr::starts_with("additional"))

  # fill codelist 0 counts
  if (!is.null(cohortName)) {
    empty <- tidyr::expand_grid(
      cohort_name = omopgenerics::settings(cdm[[cohortName]])$cohort_name,
      codelist_name = as.character(newSet |> dplyr::pull("codelist_name"))
    ) |>
      omopgenerics::uniteGroup(cols = c("cohort_name", "codelist_name")) |>
      dplyr::anti_join(x, by = c("group_name", "group_level"))
  } else {
    codelists <- as.character(newSet |> dplyr::pull("codelist_name"))
    empty <- dplyr::tibble(
      group_name = "codelist_name",
      group_level = codelists[!codelists %in% unique(x$group_level)]
    )
  }
  if (nrow(empty) > 0) {
    x <- dplyr::bind_rows(
      x,
      empty |>
        dplyr::mutate(
          result_id = 1L,
          strata_name = "overall",
          strata_level = "overall",
          variable_level = NA_character_,
          estimate_name = "count",
          estimate_type = "integer",
          estimate_value = "0",
        ) |>
        dplyr::cross_join(dplyr::tibble(variable_name = c("number records", "number subjects")))
    )
  }
  # to summarise result
  x <- x |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm)
    ) |>
    omopgenerics::uniteAdditional() |>
    dplyr::select(omopgenerics::resultColumns()) |>
    updateSummarisedResultSettings(resultType = "measurement_timings", installedVersion, timingName, dateRange)

  return(x)
}

transformMeasurementValue <- function(x, cdm, newSet, cohortName, installedVersion, timing, byConcept, dateRange) {
  x <- x |>
    dplyr::filter(.data$variable_name != "number subjects") |>
    omopgenerics::splitGroup() |>
    omopgenerics::splitAdditional() |>
    dplyr::left_join(
      cdm$concept |>
        dplyr::select(
          "unit_concept_id" = "concept_id",
          "unit_concept_name" = "concept_name"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(unit_concept_id = as.character(.data$unit_concept_id)),
      by = "unit_concept_id"
    ) |>
    dplyr::mutate(
      cdm_name = omopgenerics::cdmName(cdm),
      unit_concept_name = dplyr::if_else(
        is.na(.data$unit_concept_name), "-", .data$unit_concept_name
      ),
      unit_concept_id = dplyr::if_else(
        .data$unit_concept_id == "NA" | is.na(.data$unit_concept_id), "-", .data$unit_concept_id
      )
    )

  if (byConcept) {
    x <- x |>
      groupIdToName(newSet = newSet, cols = c("cohort_name"[!is.null(cohortName)], "codelist_name", "concept_name", "unit_concept_name")) |>
      omopgenerics::uniteAdditional(cols = c("concept_id", "unit_concept_id", "domain_id"))
  } else {
    x <- x |>
      omopgenerics::uniteGroup(cols = c("cohort_name"[!is.null(cohortName)], "codelist_name", "unit_concept_name")) |>
      omopgenerics::uniteAdditional(cols = c("unit_concept_id"))
  }

  x <- x  |>
    dplyr::select(omopgenerics::resultColumns()) |>
    updateSummarisedResultSettings(resultType = "measurement_value_as_numeric", installedVersion, timing, dateRange)

  return(x)
}

transformMeasurementConcept <- function(x, cdm, newSet, cohortName,
                                        installedVersion, timing, byConcept, dateRange) {
  x <- x |>
    dplyr::select(!c("additional_name", "additional_level")) |>
    dplyr::rename("value_as_concept_id" = "variable_level") |>
    dplyr::left_join(
      cdm$concept |>
        dplyr::select(
          "variable_level" = "concept_name",
          "value_as_concept_id" = "concept_id"
        )|>
        dplyr::collect() |>
        dplyr::mutate(value_as_concept_id = as.character(.data$value_as_concept_id)),
      by = "value_as_concept_id"
    ) |>
    dplyr::mutate(
      variable_name = gsub("_id", "_name", "value_as_concept_id"),
      cohort_table = cohortName,
      value_as_concept_id = dplyr::if_else(is.na(.data$value_as_concept_id), "-", .data$value_as_concept_id),
      variable_level = dplyr::if_else(is.na(.data$variable_level), "-", .data$variable_level)
    )

  if (byConcept) {
    x <- x |>
      omopgenerics::splitGroup() |>
      groupIdToName(newSet = newSet, cols = c("cohort_name"[!is.null(cohortName)], "codelist_name", "concept_name")) |>
      omopgenerics::uniteAdditional(cols = c("concept_id", "value_as_concept_id", "domain_id")) |>
      dplyr::select(omopgenerics::resultColumns())
  } else {
    x <- x |>
      omopgenerics::uniteAdditional(cols = c("value_as_concept_id")) |>
      dplyr::select(omopgenerics::resultColumns())
  }

  x <- x|>
    updateSummarisedResultSettings(resultType = "measurement_value_as_concept", installedVersion, timing, dateRange)

  return(x)
}

addStrata <- function(x, bySex, byYear, ageGroup, name) {
  if (bySex | length(ageGroup)>0) {
    x <- x |>
      PatientProfiles::addDemographics(
        sex = bySex,
        ageGroup = ageGroup,
        priorObservation = FALSE,
        futureObservation = FALSE,
        name = name
      )
  }

  if (byYear) {
    x <- x %>%
      dplyr::mutate(year = !!CDMConnector::datepart("cohort_start_date", "year")) |>
      dplyr::compute(name = name, temporary = FALSE)
  }

  return(x)
}

updateSummarisedResultSettings <- function(x, resultType, installedVersion, timingName, dateRange) {
  group <- omopgenerics::groupColumns(x)
  if (length(group) > 0) paste0(unique(unlist(group)), collapse = " &&& ")
  additional <- omopgenerics::additionalColumns(x)
  if (length(additional) > 0) paste0(unique(unlist(additional)), collapse = " &&& ")
  if (!all(is.na(dateRange)) & length(dateRange) > 1) {
    date_range <- paste0(dateRange[1], " to ", dateRange[2])
  } else {
    date_range <- NULL
  }
  x |>
    omopgenerics::newSummarisedResult(
      settings = omopgenerics::settings(x) |>
        dplyr::mutate(
          result_type = resultType,
          package_name = "MeasurementDiagnostics",
          package_version = installedVersion,
          group = group,
          additional = additional,
          timing = timingName,
          date_range = date_range
        )
    )
}

getCohortFromCodes <- function(cdm, codes, settingsTableName, name) {

  domains <- cdm[[settingsTableName]] |> dplyr::pull("domain_id") |> unique() |> tolower()
  tables <- list()

  for (tab in domains) {
    n <- cdm[[settingsTableName]] |>
      dplyr::filter(tolower(.data$domain_id) == tab) |>
      dplyr::tally() |>
      dplyr::pull()
    cli::cli_inform(c(">" = "Getting {tab} records based on {n} concept{?s}."))
    tables[[tab]] <- cdm[[tab]] |>
      dplyr::rename("concept_id" = !!paste0(tab, "_concept_id")) |>
      dplyr::inner_join(
        cdm[[settingsTableName]] |>
          dplyr::select(dplyr::all_of(c("cohort_definition_id", "concept_id", "codelist_name"))),
        by = "concept_id"
      ) |>
      dplyr::select(dplyr::all_of(c(
        "cohort_definition_id",
        "person_id",
        "record_date" = paste0(tab, "_date"),
        "record_id" = paste0(tab, "_id"),
        "codelist_name",
        "concept_id",
        "unit_concept_id",
        "value_as_number",
        "value_as_concept_id"
      ))) |>
      dplyr::mutate(
        unit_concept_id = as.integer(.data$unit_concept_id),
        value_as_concept_id = as.integer(.data$value_as_concept_id)
      )
  }

  Reduce(dplyr::union_all, tables) |>
    dplyr::compute(name = name, temporary = FALSE)
}
