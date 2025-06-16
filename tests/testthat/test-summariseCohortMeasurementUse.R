test_that("summariseCohortMeasurementUse works", {
  skip_on_cran()
  cdm <- testMockCdm()
  cdm <- copyCdm(cdm)
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
    as.character(c("1498", "21", "2332", "28", "4338.5", "5026", "96"))
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
    c('0', '0', '0', '0', '108.989898989899', '108.989898989899', '12', '12',
      '127.29797979798', '127.29797979798', '130.227272727273', '130.227272727273',
      '147.070707070707', '147.070707070707', '148.535353535354', '148.535353535354',
      '16', '16', '16.6666666666667', '16.6666666666667', '19.6464646464646',
      '19.6464646464646', '2', '2', '26.969696969697', '26.969696969697',
      '60.6565656565657', '60.6565656565657', '65.0505050505051', '65.0505050505051',
      '79.6969696969697', '79.6969696969697')
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
    c('11', '11', '28.5714285714286', '28.5714285714286', '32.1428571428571',
      '32.1428571428571', '39.2857142857143', '39.2857142857143', '8', '8', '9', '9')
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
  cdm <- testMockCdm()
  cdm <- copyCdm(cdm)
  res <- summariseCohortMeasurementUse(
    cohort = cdm$my_cohort,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = TRUE,
    ageGroup = NULL,
    dateRange = as.Date(c("1995-01-01", "2020-01-01"))
  )
  expect_equal(
    res$strata_level |> unique(), c("overall", "Male", "2015" )
  )
  expect_equal(
    res |>
      dplyr::filter(result_id == 3, estimate_name == "count", strata_name == "year", group_level == "test") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1")
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
      date_range = "1995-01-01 to 2020-01-01",
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
  cdm <- testMockCdm()
  cdm <- copyCdm(cdm)
  res <- summariseCohortMeasurementUse(
    cohort = cdm$my_cohort,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = FALSE,
    byYear = FALSE,
    ageGroup = NULL,
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
