#' Diagnostics of a codelist of measurement codes within a cohort
#'
#' @param codes A codelist of measurement codes for which to perform diagnostics.
#' @param cohort A cohort in which to perfom the diagnostics of the measurement
#' codes provided.
#' @param timing Three options: 1) "any" if the interest is on measurement
#' recorded any time, 2) "during", if interested in measurements while the
#' subject is in the cohort (or in observation if cohort = NULL), and 3)
#' "cohort_start_date" for measurements ocurring at cohort start date (or at
#' "observation_period_start_date if cohort = NULL).
#' @param byConcept TRUE or FALSE. If TRUE code use will be summarised by concept.
#' @param byYear TRUE or FALSE. If TRUE code use will be summarised by year.
#' @param bySex TRUE or FALSE. If TRUE code use will be summarised by sex.
#' @param ageGroup If not NULL, a list of ageGroup vectors of length two.
#' @param dateRange Two dates. The first indicating the earliest measurement
#' date and the second indicating the latest possible measurement date.
#' @param checks Diagnostics to run. Options are: "measurement_timing",
#' "measurement_value_as_numeric", and "measurement_value_as_concept".
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
                                          checks = c("measurement_timing", "measurement_value_as_numeric", "measurement_value_as_concept")) {

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

