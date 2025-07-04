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
#' tableMeasurementTimings(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
tableMeasurementTimings <- function(result,
                                    type = "gt",
                                    header = c(visOmopResults::strataColumns(result)),
                                    groupColumn = c("codelist_name"),
                                    settingsColumn = character(),
                                    hide = c("variable_level"),
                                    style = "default",
                                    .options = list()){
  rlang::check_installed("visOmopResults")

  # check inputs
  result <- omopgenerics::validateResultArgument(result, call = call)

  # subset to rows of interest
  result <- result |>
    omopgenerics::filterSettings(.data$result_type == "measurement_timings")

  if (nrow(result) == 0) {
    cli::cli_warn("There are no results with `result_type = measurement_timings`")
    return(visOmopResults::emptyTable(type = type))
  }

  checkVersion(result)

  columnOrder <- c("cdm_name", "cohort_name", "codelist_name", "sex", "age_group", "year", settingsColumn, "variable_name", "variable_level", "estimate_name", "estimate_value")
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
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y")) |>
    dplyr::mutate(variable_name = visOmopResults::customiseText(.data$variable_name)) |>
    visOmopResults::visOmopTable(
      estimateName = c(
        "N" = "<count>",
        "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
        "Range" = "<min> to <max>"
      ),
      header = header,
      settingsColumn = settingsColumn,
      groupColumn = groupColumn,
      rename = c("CDM name" = "cdm_name"),
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


checkVersion <- function(result) {
  pkg <- "MeasurementDiagnostics"
  set <- omopgenerics::settings(result)
  version <- unique(set$package_version[set$package_name == pkg])
  installedVersion <- as.character(utils::packageVersion(pkg))
  difVersions <- version[!version %in% installedVersion]
  if (length(difVersions) > 0) {
    c("!" = "`result` was generated with a different version ({.strong {difVersions}}) of {.pkg {pkg}} than the one installed: {.strong {installedVersion}}") |>
      cli::cli_inform()
  }
  invisible()
}
