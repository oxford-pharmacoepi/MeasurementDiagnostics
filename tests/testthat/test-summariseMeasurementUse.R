test_that("summariseMeasurementUse works", {
  skip_on_cran()
  # without cohort
  cdm <- mockMeasurementDiagnostics()
  res <- summariseMeasurementUse(
    cdm = cdm,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    ageGroup = list(c(0, 17), c(18, 64), c(65, 150))
  )
  expect_equal(
    omopgenerics::settings(res),
    dplyr::tibble(
      result_id = 1:3L,
      result_type = c("measurement_records", "measurement_value_as_numeric", "measurement_value_as_concept"),
      package_name = "MeasurementDiagnostics",
      package_version = as.character(utils::packageVersion("MeasurementDiagnostics")),
      group = c("codelist_name &&& concept_name", "codelist_name &&& concept_name &&& unit_concept_name", "codelist_name &&& concept_name"),
      strata = c(rep("sex &&& age_group", 3)),
      additional = c("concept_id", "concept_id &&& unit_concept_id", "concept_id &&& value_as_concept_id"),
      min_cell_count = "0"
    )
  )

  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    as.character(c(0, 0, 0, 100, 1207, 21, 294, 67, 89))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(variable_name) |>
      sort(),
    c(rep("number records", 2), rep("number subjects", 2), rep("time", 5))
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_records") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c(rep("count", 4), "max", "median", "min", "q25", "q75")
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("10.440220110055", "10.4764882441221", "12.1085542771386", "12.1810905452726", "2",
      "3", "4", "5.36268134067033", "5.4352176088044", "50", "50", "6",
      "7.06728364182091", "7.10355177588794", "8.77188594297149", "8.77188594297149")
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_numeric") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c('count', 'count', 'count_missing', 'count_missing', 'max',
      'max', 'median', 'median', 'min', 'min', 'percentage_missing', 'percentage_missing',
      'q25', 'q25', 'q75', 'q75')
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("33", "33", "33", "33", "34", "34")
  )
  expect_equal(
    res |>
      omopgenerics::filterSettings(result_type == "measurement_value_as_concept") |>
      dplyr::filter(strata_name == "overall") |>
      dplyr::pull(estimate_name) |>
      sort(),
    c('count', 'count', 'count', 'percentage', 'percentage', 'percentage')
  )

  # suppress
  resSup <- res |> omopgenerics::suppress(minCellCount = 68)
  expect_equal(resSup$estimate_value |> unique(), c("100", "-", "0"))
})

test_that("summariseMeasurementUse straifications work", {
  skip_on_cran()
  # without cohort
  cdm <- mockMeasurementDiagnostics()
  res <- summariseMeasurementUse(
    cdm = cdm,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = TRUE,
    ageGroup = NULL,
    dateRange = as.Date(c("2000-01-01", "2005-01-01"))
  )
  expect_equal(
    res$strata_level |> unique(), c("overall", "Female", "2000", "2002", "2003")
  )
  expect_equal(
    res |>
      dplyr::filter(strata_level == "2000", result_id == 3, estimate_name == "count") |>
      dplyr::pull(estimate_value) |>
      sort(),
    c("1", "1", "2")
  )

  res <- summariseMeasurementUse(
    cdm = cdm,
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
      result_type = c("measurement_records", "measurement_value_as_numeric", "measurement_value_as_concept"),
      package_name = "MeasurementDiagnostics",
      package_version = as.character(utils::packageVersion("MeasurementDiagnostics")),
      group = c("codelist_name", "codelist_name &&& unit_concept_name", "codelist_name"),
      strata = c(rep("", 3)),
      additional = c("", "unit_concept_id", "value_as_concept_id"),
      min_cell_count = "0"
    )
  )
  expect_equal(
    res |>
      dplyr::filter(group_level == "test3") |>
      dplyr::pull("estimate_value"),
    c("0", "0")
  )

})

test_that("summariseMeasurementUse expected fails", {
  skip_on_cran()
  cdm <- mockMeasurementDiagnostics()

  expect_error(summariseMeasurementUse(
    cdm = cdm,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = TRUE,
    ageGroup = NULL,
    dateRange = as.Date(c("2006-01-01", "2005-01-01"))
  ))
  expect_error(summariseMeasurementUse(
    cdm = cdm,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = 0,
    ageGroup = NULL,
    dateRange = as.Date(c("2000-01-01", "2005-01-01"))
  ))
  expect_error(summariseMeasurementUse(
    cdm = cdm,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = TRUE,
    ageGroup = "0 to 10",
    dateRange = as.Date(c("2000-01-01", "2005-01-01"))
  ))
  expect_error(summariseMeasurementUse(
    cdm = cdm,
    codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L),
    bySex = TRUE,
    byYear = TRUE,
    ageGroup = NULL,
    dateRange = c(0, as.Date("2005-01-01"))
  ))
})
