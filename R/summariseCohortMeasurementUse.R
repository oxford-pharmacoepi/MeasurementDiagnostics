#' Diagnostics of a codelist of measurement codes within a cohort
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
#' result <- summariseCohortMeasurementUse(
#'   codes = list("test_codelist" = c(3001467L, 45875977L)),
#'   cohort = cdm$my_cohort, timing = "cohort_start_date"
#' )
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
summariseCohortMeasurementUse <- function(codes,
                                          cohort,
                                          timing = "during",
                                          byConcept = TRUE,
                                          byYear = FALSE,
                                          bySex = FALSE,
                                          ageGroup = NULL,
                                          dateRange = as.Date(c(NA, NA)),
                                          checks = c("measurement_timings", "measurement_value_as_numeric", "measurement_value_as_concept")) {

  # check inputs
  timing <- omopgenerics::assertChoice(timing, choices = c("any", "during", "cohort_start_date"))
  cohort <- omopgenerics::validateCohortArgument(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cdm <- omopgenerics::cdmReference(cohort)

  result <- summariseMeasurementUseInternal(
    cdm = cdm,
    codes = codes,
    cohort = cohort,
    timing = timing,
    timingName = timing,
    byConcept = byConcept,
    byYear = byYear,
    bySex = bySex,
    ageGroup = ageGroup,
    dateRange = dateRange,
    checks = checks
  )

  return(result)
}

