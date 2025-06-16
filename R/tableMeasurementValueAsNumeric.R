#' Format a measurement_timings object into a visual table
#'
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
#' tableMeasurementValueAsNumeric(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
tableMeasurementValueAsNumeric <- function(result,
                                           type = "gt",
                                           header = c(visOmopResults::strataColumns(result)),
                                           groupColumn = c("codelist_name"),
                                           settingsColumn = character(),
                                           hide = c("variable_level", "cohort_table"),
                                           style = "default",
                                           .options = list()){
  rlang::check_installed("visOmopResults")

  # check inputs
  result <- omopgenerics::validateResultArgument(result, call = call)

  # subset to rows of interest
  result <- result |>
    omopgenerics::filterSettings(.data$result_type == "measurement_value_as_numeric")

  if (nrow(result) == 0) {
    cli::cli_warn("There are no results with `result_type = measurement_value_as_numeric`")
    return(visOmopResults::emptyTable(type = type))
  }

  checkVersion(result)

  columnOrder <- c("cdm_name", "cohort_table", "codelist_name", "concept_name", "concept_id" , "unit_concept_name", "unit_concept_id", "sex", "age_group", "year", settingsColumn, "variable_name", "variable_level", "estimate_name", "estimate_value")
  # temp fix for visOmpReuslts issue 355
  columnOrder <- columnOrder[columnOrder %in% visOmopResults::tableColumns(result)]

  factors <- result |>
    dplyr::filter(.data$variable_name == "number records") |>
    visOmopResults::splitAll() |>
    dplyr::select(dplyr::any_of(c("cdm_name", "codelist_name", "concept_name", "unit_concept_name", "estimate_value"))) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    dplyr::arrange(.data$estimate_value) |>
    dplyr::select(!"estimate_value")

  if (nrow(factors) == 0) {
    factors <- NULL
  }  else {
    factors <- factors |> as.list() |> purrr::map(\(x){unique(x)})
  }

  result |>
    dplyr::mutate(variable_name = visOmopResults::customiseText(.data$variable_name)) |>
    visOmopResults::visOmopTable(
      estimateName = c(
        "N" = "<count>",
        "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
        "Range" = "<min> to <max>",
        "Missing value, N (%)" = "<count_missing> (<percentage_missing>%)"
      ),
      header = header,
      settingsColumn = settingsColumn,
      groupColumn = groupColumn,
      rename = c("CDM name" = "cdm_name", "Concept ID" = "concept_id", "Unit concept ID" = "unit_concept_id"),
      type = type,
      hide = hide,
      columnOrder = columnOrder,
      factor = factors,
      style = style,
      showMinCellCount = TRUE,
      .options = .options
    )
}
