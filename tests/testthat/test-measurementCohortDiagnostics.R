test_that("measurementCohortDiagnostics works", {
  skip_on_cran()
  # with cohort
  cdm <- mockMeasurementDiagnostics()
  res <- measurementCohortDiagnostics(codes = list("test" = 3001467L), cohort = cdm$my_cohort, timing = "any")
  expect_equal(
    omopgenerics::settings(res),
    dplyr::tibble(
      result_id = 1:3L,
      result_type = c("measurement_records", "measurement_value_as_numeric", "measurement_value_as_concept"),
      package_name = "MeasurementDiagnostics",
      package_version = as.character(utils::packageVersion("MeasurementDiagnostics")),
      group = c("codelist_name &&& concept_name", "codelist_name &&& concept_name &&& unit_concept_name", "codelist_name &&& concept_name"),
      strata = c(rep("sex &&& age_group", 3)),
      additional = c("concept_id &&& cohort_table", "concept_id &&& unit_concept_id &&& cohort_table", "concept_id &&& value_as_concept_id &&& cohort_table"),
      min_cell_count = "0",
      timing = "any"
    )
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    as.character(c("0", "1072", "22", "282", "53", "72", "85"))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(variable_name) |>
      sort(),
    c("number records", "number subjects", rep("time", 5))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
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
    c('1', '2.77777777777778', '3', '36', '36', '58.0961428943556',
      '58.0961428943556', '58.0961428943556', '58.0961428943556',
      '58.0961428943556', '58.0961428943556', '58.0961428943556',
      '58.0961428943556', '58.0961428943556', '58.0961428943556',
      '8.33333333333333')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c('count', 'count', 'count_missing', 'count_missing', 'max', 'max', 'median',
      'median', 'min', 'min', 'percentage_missing', 'percentage_missing', 'q25',
      'q25', 'q75', 'q75')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("22", "24", "26", "30.5555555555556", "33.3333333333333", "36.1111111111111")
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c("count", "count", "count", "percentage", "percentage", "percentage")
  )
})

test_that("test timings with eunomia", {
  skip_on_cran()
  skip_if(Sys.getenv("EUNOMIA_DATA_FOLDER") == "")
  # without cohort
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmName = "eunomia", cdmSchema = "main", writeSchema = "main")
  cohort <- CohortConstructor::conceptCohort(cdm = cdm, conceptSet = list("condition" = 40481087L), name = "cohort")
  res_any <- measurementCohortDiagnostics(
    codes = list("bmi" = c(4024958L, 36304833L), "egfr" = c(1619025L, 1619026L, 3029829L, 3006322L)),
    cohort = cohort, timing = "any"
  )
  res_during <- measurementCohortDiagnostics(
    codes = list("bmi" = c(4024958L, 36304833L), "egfr" = c(1619025L, 1619026L, 3029829L, 3006322L)),
    cohort = cohort, timing = "during"
  )
  res_start <- measurementCohortDiagnostics(
    codes = list("bmi" = c(4024958L, 36304833L), "egfr" = c(1619025L, 1619026L, 3029829L, 3006322L)),
    cohort = cohort, timing = "cohort_start_date"
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('1035', '12852', '1487', '2329', '2442', '2656', '31573', '31880', '3493',
      '38', '39', '4961.5', '5498', '7481')
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('1602', '1602', '1602', '1602', '1602', '1602', '1602', '1602', '1602',
      '1602', '28', '29', '60', '61')
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("0", "0", "1", "1")
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('100', '100', '12852', '12852', '5498', '5498')
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('100', '100', '29', '29', '61', '61')
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "1", "100")
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("100", "100", "12852", "5498")
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("100", "100", "29", "61")
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "100")
  )
})
