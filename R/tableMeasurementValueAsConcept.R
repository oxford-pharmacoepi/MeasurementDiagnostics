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
#' tableMeasurementValueAsConcept(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
tableMeasurementValueAsConcept  <- function(result,
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
    cli::cli_warn("There are no results with `result_type = {resultType}`")
    return(visOmopResults::emptyTable(type = type))
  }

  checkVersion(result)

  if (is.function(modifyResults)) {
    result <- do.call(modifyResults, list(x = result, call = call))
  }

  columnOrder <- c("cdm_name", "codelist_name", "concept_name", "concept_id" , "unit_concept_name", "unit_concept_id", "variable_name", "variable_level", "estimate_name", "estimate_value")
  columnOrder <- columnOrder[columnOrder %in% visOmopResults::tableColumns(result)]

  factors <- result |>
    dplyr::filter(variable_name == "Number records") |>
    visOmopResults::splitAll() |>
    dplyr::select(dplyr::any_of(c("cdm_name", "codelist_name", "concept_name", "unit_concept_name", "estimate_value"))) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    dplyr::arrange(.data$estimate_value) |>
    dplyr::select(!"estimate_value")

  visOmopResults::visOmopTable(
    result = result,
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
    factor = factors |> as.list() |> purrr::map(\(x){unique(x)}),
    style = style,
    showMinCellCount = TRUE,
    .options = .options
  )
}
