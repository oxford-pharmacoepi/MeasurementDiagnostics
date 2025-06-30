#' Format a measurement_timings object into a visual table
#'
#' @inheritParams resultDoc
#' @inheritParams tableDoc
#'
#' @return A formatted table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(MeasurementDiagnostics)
#'
#' cdm <- mockMeasurementDiagnostics()
#' result <- summariseMeasurementUse(
#'               cdm = cdm,
#'               codes = list("test_codelist" = c(3001467L, 45875977L)))
#'
#' tableMeasurementValueAsConcept(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
tableMeasurementValueAsConcept <- function(result,
                                           type = "gt",
                                           header = c(visOmopResults::strataColumns(result)),
                                           groupColumn = c("codelist_name"),
                                           settingsColumn = character(),
                                           hide = character(),
                                           style = "default",
                                           .options = list()){
  rlang::check_installed("visOmopResults")

  # check inputs
  result <- omopgenerics::validateResultArgument(result, call = call)

  # subset to rows of interest
  result <- result |>
    omopgenerics::filterSettings(.data$result_type == "measurement_value_as_concept")

  if (nrow(result) == 0) {
    cli::cli_warn("There are no results with `result_type = measurement_value_as_concept`")
    return(visOmopResults::emptyTable(type = type))
  }

  checkVersion(result)

  columnOrder <- c(
    "cdm_name", "cohort_name", "codelist_name", "concept_name", "concept_id" ,
    "domain_id", "sex", "age_group", "year", settingsColumn, "variable_name",
    "variable_level", "value_as_concept_id", "estimate_name", "estimate_value"
  )
  # temp fix for visOmpReuslts issue 355
  columnOrder <- columnOrder[columnOrder %in% visOmopResults::tableColumns(result)]

  factors <- result |>
    dplyr::filter(.data$variable_name == "count") |>
    visOmopResults::splitAll() |>
    dplyr::select(dplyr::any_of(c("cdm_name", "codelist_name", "concept_name", "variable_level"))) |>
    dplyr::distinct()

  if (nrow(factors) == 0) {
    factors <- NULL
  }  else {
    factors <- factors |> as.list() |> purrr::map(\(x){unique(x)})
  }

  result |>
    dplyr::mutate(variable_name = visOmopResults::customiseText(.data$variable_name)) |>
    visOmopResults::visOmopTable(
      estimateName = c("N (%)" = "<count> (<percentage>%)"),
      header = header,
      settingsColumn = settingsColumn,
      groupColumn = groupColumn,
      rename = c(
        "Domain ID" = "domain_id",
        "CDM name" = "cdm_name",
        "Concept ID" = "concept_id",
        "Value as concept name" = "variable_level",
        "Value as concept ID" = "value_as_concept_id"
      ),
      type = type,
      hide = hide,
      columnOrder = columnOrder,
      factor = factors,
      style = style,
      showMinCellCount = TRUE,
      .options = .options
    ) |>
    suppressWarnings()
}
