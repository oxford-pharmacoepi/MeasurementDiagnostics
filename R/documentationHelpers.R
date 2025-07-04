#' Helper for consistent documentation of `summariseMeasurement` functions.
#'
#' @param cdm A reference to the cdm object.
#' @param codes A codelist of measurement/observation codes for which to perform
#' diagnostics.
#' @param cohort A cohort in which to perform the diagnostics of the measurement
#' codes provided.
#' @param timing Three options: 1) "any" if the interest is on measurement
#' recorded any time, 2) "during", if interested in measurements while the
#' subject is in the cohort (or in observation if cohort = NULL), and 3)
#' "cohort_start_date" for measurements occurring at cohort start date (or at
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
#' @name summariseMeasurementDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `result`.
#'
#' @param result A summarised_result object.
#'
#' @name resultDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `table`.
#'
#' @param type Type of table. Check supported types with
#' `visOmopResults::tableType()`.
#' @param header Columns to use as header. See options with
#' `visOmopResults::tableColumns(result)`.
#' @param groupColumn Columns to group by. See options with
#' `visOmopResults::tableColumns(result)`.
#' @param settingsColumn Columns from settings to include in results. See
#' options with `visOmopResults::settingsColumns(result)`.
#' @param hide Columns to hide from the visualisation. See options with
#' `visOmopResults::tableColumns(result)`.
#' @param style Named list that specifies how to style the different parts of
#'  the table generated. It can either be a pre-defined style ("default" or
#'  "darwin" - the latter just for gt and flextable), NULL to get the table type
#'  default style, or custom. Keep in mind that styling code is different for
#'  all table styles. To see the different styles use visOmopResults::tableStyle().
#' @param .options A named list with additional formatting options.
#' `visOmopResults::tableOptions()` shows allowed arguments and their default values.
#'
#' @name tableDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `plot`.
#'
#' @param x Columns to use as horizontal axes. See options with
#' `visOmopResults::plotColumns(result)`.
#' @param y Columns to use as horizontal axes. See options with
#' `visOmopResults::plotColumns(result)`.
#' @param facet Columns to facet by. See options with
#' `visOmopResults::plotColumns(result)`. Formula input is also allowed to
#' specify rows and columns.
#' @param colour Columns to color by. See options with
#' `visOmopResults::plotColumns(result)`.
#'
#' @name plotDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `uniqueCombinations`.
#'
#' @param uniqueCombinations Whether to restrict to unique reference and
#' comparator comparisons.
#'
#' @name uniqueCombinationsDoc
#' @keywords internal
NULL

#' Helper for consistent documentation of `timeScale`.
#'
#' @param timeScale Time scale to show, it can be "days" or "years".
#'
#' @name timeScaleDoc
#' @keywords internal
NULL
