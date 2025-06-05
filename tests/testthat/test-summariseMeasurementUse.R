test_that("summariseMeasurementUse works", {
  skip_on_cran()
  # without cohort
  cdm <- mockMeasurementDiagnostics()
  res <- summariseMeasurementUse(cdm = cdm, codes = list("test" = 3001467L, "test2" = 1L, "test3" = 45875977L))
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
    c('2', '3', '4', '50', '50', '58.0961428943556', '58.0961428943556',
      '58.0961428943556', '58.0961428943556', '58.0961428943556', '58.0961428943556',
      '58.0961428943556', '58.0961428943556', '58.0961428943556', '58.0961428943556', '6')
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
