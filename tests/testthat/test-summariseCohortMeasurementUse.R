test_that("summariseCohortMeasurementUse works", {
  skip_on_cran()
  # with cohort
  cdm <- mockMeasurementDiagnostics()
  res <- summariseCohortMeasurementUse(codes = list("test" = 3001467L), cohort = cdm$my_cohort, timing = "any")
  expect_equal(
    omopgenerics::settings(res),
    dplyr::tibble(
      result_id = 1:3L,
      result_type = c("measurement_timings", "measurement_value_as_numeric", "measurement_value_as_concept"),
      package_name = "MeasurementDiagnostics",
      package_version = as.character(utils::packageVersion("MeasurementDiagnostics")),
      group = c("codelist_name", "codelist_name &&& concept_name &&& unit_concept_name", "codelist_name &&& concept_name"),
      strata = c(rep("", 3)),
      additional = c("cohort_table", "concept_id &&& unit_concept_id &&& cohort_table", "concept_id &&& value_as_concept_id &&& cohort_table"),
      min_cell_count = "0",
      timing = "any"
    )
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    as.character(c("0", "104.5", "1207", "22.5", "301", "59", "83"))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(variable_name) |>
      sort(),
    c("number records", "number subjects", rep("time", 5))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c("count", "count", "max", "median", "min", "q25", "q75")
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('10.5490245122561', '10.5490245122561', '12.1085542771386', '12.1085542771386',
      '12.1810905452726', '12.1810905452726', '2', '2', '3', '3', '39', '39',
      '4.54545454545455', '4.54545454545455', '44', '44', '5.36268134067033',
      '5.36268134067033', '5.4352176088044', '5.4352176088044', '6.84967483741871',
      '6.84967483741871', '6.99474737368684', '6.99474737368684', '7.69230769230769',
      '7.69230769230769', '8.33666833416708', '8.33666833416708', '8.77188594297149',
      '8.77188594297149', '9.96873436718359', '9.96873436718359')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c('count', 'count', 'count', 'count', 'count_missing', 'count_missing',
      'count_missing', 'count_missing', 'max', 'max', 'max', 'max', 'median',
      'median', 'median', 'median', 'min', 'min', 'min', 'min', 'percentage_missing',
      'percentage_missing', 'percentage_missing', 'percentage_missing', 'q25',
      'q25', 'q25', 'q25', 'q75', 'q75', 'q75', 'q75')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('26', '26', '27', '27', '30', '30', '31.3253012048193', '31.3253012048193',
      '32.5301204819277', '32.5301204819277', '36.144578313253', '36.144578313253')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c("count", "count", "count", "count", "count", "count", "percentage",
      "percentage", "percentage", "percentage", "percentage", "percentage")
  )
})

test_that("test timings with eunomia", {
  skip_on_cran()
  skip_if(Sys.getenv("EUNOMIA_DATA_FOLDER") == "")
  # without cohort
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmName = "eunomia", cdmSchema = "main", writeSchema = "main")
  cohort <- CohortConstructor::conceptCohort(cdm = cdm, conceptSet = list("condition" = 40481087L), name = "cohort")
  res_any <- summariseCohortMeasurementUse(
    codes = list("bmi" = c(4024958L, 36304833L), "egfr" = c(1619025L, 1619026L, 3029829L, 3006322L)),
    cohort = cohort, timing = "any"
  )
  res_during <- summariseCohortMeasurementUse(
    codes = list("bmi" = c(4024958L, 36304833L), "egfr" = c(1619025L, 1619026L, 3029829L, 3006322L)),
    cohort = cohort, timing = "during"
  )
  res_start <- summariseCohortMeasurementUse(
    codes = list("bmi" = c(4024958L, 36304833L), "egfr" = c(1619025L, 1619026L, 3029829L, 3006322L)),
    cohort = cohort, timing = "cohort_start_date"
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('1035', '12852', '1487', '2329', '2442', '2656', '31573', '31880', '3493',
      '38', '39', '4961.5', '5498', '7481')
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('1602', '1602', '1602', '1602', '1602', '1602', '1602', '1602', '1602',
      '1602', '28', '29', '60', '61')
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("0", "0", "1", "1")
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall", group_name != "codelist_name &&& unit_concept_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('100', '100', '12852', '12852', '5498', '5498')
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall", group_name != "codelist_name &&& unit_concept_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('100', '100', '29', '29', '61', '61')
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall", group_name != "codelist_name &&& unit_concept_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "1", "100")
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall", group_name != "codelist_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("100", "100", "12852", "5498")
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall", group_name != "codelist_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("100", "100", "29", "61")
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall", group_name != "codelist_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "100")
  )
})

test_that("summariseCohortMeasurementUse straifications work", {
  skip_on_cran()
  # without cohort
  cdm <- mockMeasurementDiagnostics()
  res <- summariseCohortMeasurementUse(
    cohort = cdm$my_cohort,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = TRUE,
    ageGroup = NULL,
    dateRange = as.Date(c("2000-01-01", "2010-01-01"))
  )
  expect_equal(
    res$strata_level |> unique(), c("overall", "Female", "Male", "2000", "2006", "2007", "2009")
  )
  expect_equal(
    res |>
      dplyr::filter(result_id == 3, estimate_name == "count", strata_name == "year", group_level == "test") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "1", "1", "1", "2", "2", "3", "3", "6")
  )
  expect_equal(
    omopgenerics::settings(res),
    dplyr::tibble(
      result_id = 1:3L,
      result_type = c("measurement_timings", "measurement_value_as_numeric", "measurement_value_as_concept"),
      package_name = "MeasurementDiagnostics",
      package_version = as.character(utils::packageVersion("MeasurementDiagnostics")),
      group = c("codelist_name", "codelist_name &&& concept_name &&& unit_concept_name", "codelist_name &&& concept_name"),
      strata = c(rep("sex &&& year", 3)),
      additional = c("cohort_table", "concept_id &&& unit_concept_id &&& cohort_table", "concept_id &&& value_as_concept_id &&& cohort_table"),
      min_cell_count = "0",
      timing = "during"
    )
  )

  res <- summariseCohortMeasurementUse(
    cohort = cdm$my_cohort,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    byConcept = FALSE,
    bySex = FALSE,
    byYear = FALSE,
    ageGroup = NULL
  )
  expect_equal(
    omopgenerics::settings(res),
    dplyr::tibble(
      result_id = 1:3L,
      result_type = c("measurement_timings", "measurement_value_as_numeric", "measurement_value_as_concept"),
      package_name = "MeasurementDiagnostics",
      package_version = as.character(utils::packageVersion("MeasurementDiagnostics")),
      group = c("codelist_name", "codelist_name &&& unit_concept_name", "codelist_name"),
      strata = c(rep("", 3)),
      additional = c("cohort_table", "unit_concept_id &&& cohort_table", "value_as_concept_id &&& cohort_table"),
      min_cell_count = "0",
      timing = "during"
    )
  )
  expect_equal(
    res |>
      dplyr::filter(group_level == "test3") |>
      dplyr::pull("estimate_value"),
    c("0", "0")
  )
})

test_that("summariseMeasurementUse checks", {
  skip_on_cran()
  cdm <- mockMeasurementDiagnostics()
  res <- summariseCohortMeasurementUse(
    cohort = cdm$my_cohort,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = FALSE,
    byYear = FALSE,
    ageGroup = NULL,
    dateRange = as.Date(c("2000-01-01", "2005-01-01")),
    checks = "measurement_timings"
  )
  expect_true(unique(res$result_id) == 1)
  expect_true(omopgenerics::settings(res)$result_type == "measurement_timings")

  res <- summariseCohortMeasurementUse(
    cohort = cdm$my_cohort,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = FALSE,
    byYear = FALSE,
    ageGroup = NULL,
    dateRange = as.Date(c("2000-01-01", "2005-01-01")),
    checks = c("measurement_value_as_numeric", "measurement_value_as_concept")
  )
  expect_true(all(omopgenerics::settings(res)$result_type %in% c("measurement_value_as_numeric", "measurement_value_as_concept")))

  expect_null(
    summariseCohortMeasurementUse(
      cohort = cdm$my_cohort,
      codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
      bySex = FALSE,
      byYear = FALSE,
      ageGroup = NULL,
      dateRange = as.Date(c("2000-01-01", "2005-01-01")),
      checks = character()
    )
  )
})
