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
#' tableMeasurementTimings(result)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
tableMeasurementTimings <- function(result,
                                    type = "gt",
                                    header = c(visOmopResults::strataColumns(result)),
                                    groupColumn = c("codelist_name"),
                                    hide = c("variable_name", "variable_level", "cohort_table"),
                                    .options = list()){

  result |>
    tableInternal1(
      resultType = "measurement_records",
      header = header,
      groupColumn = groupColumn,
      hide = hide,
      rename = c("CDM name" = "cdm_name"),
      modifyResults = \(x, ...) {
        x |>
          dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))
      },
      estimateName = c(
        "N" = "<count>",
        "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
        "Range" = "<min> to <max>"
      ),
      type = type,
      .options = .options
    )
}

tableInternal1 <- function(result,
                          resultType,
                          header,
                          groupColumn,
                          hide,
                          rename,
                          modifyResults,
                          estimateName,
                          type,
                          .options = list(),
                          call = parent.frame()) {

  rlang::check_installed("visOmopResults")

  # check inputs
  result <- omopgenerics::validateResultArgument(result, call = call)

  # subset to rows of interest
  result <- result |>
    omopgenerics::filterSettings(.data$result_type == .env$resultType)

  if (nrow(result) == 0) {
    cli::cli_warn("There are no results with `result_type = {resultType}`")
    return(visOmopResults::emptyTable(type = type))
  }

  checkVersion(result)

  set <- omopgenerics::settings(result)
  if (is.function(modifyResults)) {
    result <- do.call(modifyResults, list(x = result, call = call))
  }

  # settings columns
  ignore <- c(
    "result_id", "result_type", "package_name", "package_version", "group",
    "strata", "additional"
  )
  setColumns <- set |>
    dplyr::filter(.data$result_id %in% unique(.env$result$result_id)) |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::compact() |>
    names() |>
    purrr::discard(\(x) x %in% ignore)

  result |>
    dplyr::left_join(
      set |>
        dplyr::select("result_id", dplyr::all_of(setColumns)),
      by = "result_id"
    ) |>
    dplyr::mutate(estimate_value = dplyr::if_else(
      stringr::str_detect(.data$estimate_name, "count") & .data$estimate_value == "-",
      paste0("<", as.character(.data$min_cell_count)),
      .data$estimate_value
    )) |>
    omopgenerics::splitAll() |>
    dplyr::select(!c("result_id", "min_cell_count")) |>
    # dplyr::relocate("concept_id", .after = "concept_name") |>
    # dplyr::relocate(dplyr::any_of("timing"), .after = "concept_id") |>
    visOmopResults::visTable(
      estimateName = estimateName,
      header = header,
      rename = rename,
      type = type,
      hide = c("estimate_type", hide),
      groupColumn = groupColumn,
      .options = .options
    )
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
