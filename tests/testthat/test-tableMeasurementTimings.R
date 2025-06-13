test_that("check that it works ", {

  cdm <- testMockCdm()
  cdm <- copyCdm(cdm)

  # Summarise measurement use ----
  result <- summariseMeasurementUse(cdm = cdm,
                                    codes = list("test_codelist" = c(3001467L, 45875977L)))
  # Table types
  expect_no_error(x <- tableMeasurementTimings(result,
                                               type = "gt",
                                               header = c(visOmopResults::strataColumns(result)),
                                               groupColumn = c("codelist_name"),
                                               hide = c("variable_name", "variable_level", "cohort_table"),
                                               .options = list()))
  expect_true("gt_tbl" %in% class(x))
  expect_true(all(c("Codelist name", "CDM name", "Estimate name", "Estimate value") %in% colnames(x$`_data`)))

  expect_no_error(x <- tableMeasurementTimings(result, type = "flextable"))
  expect_true("flextable" %in% class(x))

  expect_no_error(x <- tableMeasurementTimings(result, type = "tibble"))
  expect_true(all(class(x) %in% c("tbl_df", "tbl", "data.frame")))

  expect_error(tableMeasurementTimings(result, type = "hola"))

  # Different package versions
  x <- result |>
    omopgenerics::newSummarisedResult(
      "settings" = omopgenerics::settings(result) |>
        dplyr::mutate("package_version" = "0.0.0")
    )
  expect_message(tableMeasurementTimings(x))

  # Empty output message
  expect_warning(x <- tableMeasurementTimings(
    result = omopgenerics::emptySummarisedResult(), type = "gt"
  ))

  # Summarise cohort measurement use ----
  result <- summariseCohortMeasurementUse(cohort = cdm$my_cohort, bySex = TRUE,
                                          codes = list("test_codelist" = c(3001467L, 45875977L)))
  expect_no_error(x <- tableMeasurementTimings(result,
                                               type = "gt",
                                               header = c(visOmopResults::strataColumns(result)),
                                               groupColumn = c("codelist_name"),
                                               hide = c("variable_level", "cohort_table"),
                                               .options = list()))

  expect_true(all(
    c('Codelist name', 'CDM name', 'Variable name', 'Estimate name', 'Timing',
      '[header_name]Sex\n[header_level]overall', '[header_name]Sex\n[header_level]Male') %in%
      colnames(x$`_data`)))

  CDMConnector::cdmDisconnect(cdm = cdm)
})

