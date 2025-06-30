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
      group = c("cohort_name &&& codelist_name", "cohort_name &&& codelist_name &&& concept_name &&& unit_concept_name", "cohort_name &&& codelist_name &&& concept_name"),
      strata = c(rep("", 3)),
      additional = c("", "concept_id &&& unit_concept_id &&& domain_id", "concept_id &&& value_as_concept_id &&& domain_id"),
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
    as.character(c('0', '0', '11', '1206', '14', '1506', '1761', '20', '2316', '4322.75', '5026', '651', '9', '96'))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(variable_name) |>
      sort(),
    c("number records", "number records", "number subjects", "number subjects", rep("time", 10))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_timings") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c(rep("count", 4), rep("max", 2), rep("median", 2), rep("min", 2), rep("q25", 2), rep("q75", 2))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('0', '0', '0', '0', '0', '0', '0', '0', '10', '10', '10', '10', '101.666666666667',
      '101.666666666667', '108.989898989899', '108.989898989899', '119.242424242424',
      '119.242424242424', '123.636363636364', '123.636363636364', '123.636363636364',
      '123.636363636364', '128.030303030303', '128.030303030303', '132.424242424242',
      '132.424242424242', '133.156565656566', '133.156565656566', '133.888888888889',
      '133.888888888889', '136.818181818182', '136.818181818182', '141.212121212121',
      '141.212121212121', '147.070707070707', '147.070707070707', '148.535353535354',
      '148.535353535354', '19.6464646464646', '19.6464646464646', '2', '2', '2', '2',
      '20', '20', '26.969696969697', '26.969696969697', '4', '4', '40.1515151515152',
      '40.1515151515152', '50', '50', '52.6010101010101', '52.6010101010101',
      '59.1919191919192', '59.1919191919192', '66.5151515151515', '66.5151515151515',
      '7', '7', '76.7676767676768', '76.7676767676768')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c('count', 'count', 'count', 'count', 'count', 'count', 'count', 'count',
      'count_missing', 'count_missing', 'count_missing', 'count_missing', 'count_missing',
      'count_missing', 'count_missing', 'count_missing', 'max', 'max', 'max', 'max',
      'max', 'max', 'max', 'max', 'median', 'median', 'median', 'median', 'median',
      'median', 'median', 'median', 'min', 'min', 'min', 'min', 'min', 'min', 'min',
      'min', 'percentage_missing', 'percentage_missing', 'percentage_missing',
      'percentage_missing', 'percentage_missing', 'percentage_missing', 'percentage_missing',
      'percentage_missing', 'q25', 'q25', 'q25', 'q25', 'q25', 'q25', 'q25', 'q25',
      'q75', 'q75', 'q75', 'q75', 'q75', 'q75', 'q75', 'q75')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('1', '1', '25', '25', '30', '30', '36.3636363636364', '36.3636363636364',
      '4', '4', '45', '45', '5', '5', '54.5454545454545', '54.5454545454545',
      '6', '6', '6', '6', '9', '9', '9.09090909090909', '9.09090909090909')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c('count', 'count', 'count', 'count', 'count', 'count', 'count', 'count', 'count',
    'count', 'count', 'count', 'percentage', 'percentage', 'percentage', 'percentage',
    'percentage', 'percentage', 'percentage', 'percentage', 'percentage', 'percentage',
    'percentage', 'percentage')
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
      dplyr::filter(strata_name == "overall", group_name != "cohort_name &&& codelist_name &&& unit_concept_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('100', '100', '12852', '12852', '5498', '5498')
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall", group_name != "cohort_name &&& codelist_name &&& unit_concept_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c('100', '100', '29', '29', '61', '61')
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall", group_name != "cohort_name &&& codelist_name &&& unit_concept_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "1", "100")
  )
  expect_equal(
    res_any |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall", group_name != "cohort_name &&& codelist_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("100", "100", "12852", "5498")
  )
  expect_equal(
    res_during |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall", group_name != "cohort_name &&& codelist_name") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("100", "100", "29", "61")
  )
  expect_equal(
    res_start |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall", group_name != "cohort_name &&& codelist_name") |>
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
      dplyr::filter(result_id == 3, estimate_name == "count", strata_name == "year", group_level == "cohort_1 &&& test") |>
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
      group = c("cohort_name &&& codelist_name", "cohort_name &&& codelist_name &&& concept_name &&& unit_concept_name", "cohort_name &&& codelist_name &&& concept_name"),
      strata = c(rep("sex &&& year", 3)),
      additional = c("", "concept_id &&& unit_concept_id &&& domain_id", "concept_id &&& value_as_concept_id &&& domain_id"),
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
      group = c("cohort_name &&& codelist_name", "cohort_name &&& codelist_name &&& unit_concept_name", "cohort_name &&& codelist_name"),
      strata = c(rep("", 3)),
      additional = c("", "unit_concept_id", "value_as_concept_id"),
      min_cell_count = "0",
      timing = "during"
    )
  )
  expect_equal(
    res |>
      dplyr::filter(group_level == "cohort_1 &&& test3") |>
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
