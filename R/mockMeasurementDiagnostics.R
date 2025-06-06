#' Function to create a mock cdm reference.
#'
#' @description
#' Creates an example dataset that can be used to show how
#' the package works
#'
#' @param nPerson number of people in the cdm.
#' @param con  A DBI connection to create the cdm mock object.
#' @param writeSchema Name of an schema on the same connection with writing
#' permissions.
#' @param seed seed to use when creating the mock data.
#'
#' @return cdm object
#' @export
#'
#' @examples
#' \donttest{
#' library(MeasurementDiagnostics)
#'
#' cdm <- mockMeasurementDiagnostics()
#'
#' cdm
#' }
mockMeasurementDiagnostics <- function(nPerson = 100,
                                       con = DBI::dbConnect(duckdb::duckdb()),
                                       writeSchema = "main",
                                       seed = 111) {

  rlang::check_installed("omock")
  rlang::check_installed("CDMConnector")

  omopgenerics::assertNumeric(nPerson, length = 1, na = FALSE, null = FALSE)
  omopgenerics::assertNumeric(seed, length = 1, na = FALSE, null = FALSE)
  omopgenerics::assertCharacter(writeSchema, length = 1, na = FALSE, null = FALSE)

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100, seed = seed) |>
    omock::mockObservationPeriod(seed = seed) |>
    omock::mockConditionOccurrence(seed = seed) |>
    omock::mockVisitOccurrence(seed = seed) |>
    omock::mockDrugExposure(seed = seed) |>
    omock::mockObservation(seed = seed) |>
    omock::mockMeasurement(seed = seed) |>
    omock::mockProcedureOccurrence(seed = seed) |>
    omock::mockCohort(name = "my_cohort", numberCohorts = 2, seed = seed)
  cdm_local$measurement <- cdm_local$measurement |>
    dplyr::mutate(
      unit_concept_id = dplyr::if_else(dplyr::row_number()%%2 == 0, 9529, NA),
      value_as_number = dplyr::if_else(dplyr::row_number()<6, NA, stats::runif(1, min = 5, max = 150)),
      value_as_concept_id = dplyr::case_when(
        dplyr::row_number()%%3 == 0 ~ 4328749,
        dplyr::row_number()%%3 == 1 ~ 4267416,
        dplyr::row_number()%%3 == 2 ~ NA,
      )
    )
  cdm_local$concept <- dplyr::bind_rows(
    cdm_local$concept,
    dplyr::tibble(
      concept_id = c(4328749L, 4267416L),
      concept_name = c("High", "Low"),
      domain_id = "Meas Value",
      vocabulary_id = "SNOMED",
      concept_class_id = "Qualifier Value",
      standard_concept = "S",
      concept_code = c(62482003, 75540009) |> as.character(),
      valid_start_date = as.character("1970-01-01"),
      valid_end_date = as.character("2099-01-01"),
      invalid_reason = NA
    )
  )

  cdm <- CDMConnector::copyCdmTo(con = con,
                                 cdm = cdm_local,
                                 schema = writeSchema,
                                 overwrite = TRUE)

  attr(cdm, "write_schema") <- writeSchema

  return(cdm)
}
