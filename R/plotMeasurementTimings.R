#' Plot summariseMeasurementTiming results.
#'
#' @inheritParams resultDoc
#' @param x Columns to use as horizontal axes.
#' @param plotType Type of desired formatted table, possibilities are "boxplot" and
#' "densityplot".
#' @inheritParams timeScaleDoc
#' @inheritParams plotDoc
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(MeasurementDiagnostics)
#'
#' cdm <- mockMeasurementDiagnostics()
#'
#' result <- summariseMeasurementUse(
#'               cdm = cdm,
#'               codes = list("test_codelist" = c(3001467L, 45875977L)))
#' plotMeasurementTimings(result)
#'
#' cdmDisconnect(cdm)
#' }
plotMeasurementTimings <- function(result,
                             x = "concept_id",
                             plotType = "boxplot",
                             timeScale = "days",
                             facet = visOmopResults::strataColumns(result),
                             colour = "cdm_name") {
  # specific checks
  # omopgenerics::assertChoice(x, c("concept_name", "concept_id"), length = 1)
  omopgenerics::assertChoice(plotType, c("boxplot", "densityplot"), length = 1)
  omopgenerics::assertChoice(timeScale, c("days", "years"), length = 1)
  result <- omopgenerics::validateResultArgument(result)
  rlang::check_installed("visOmopResults")

  # pre process
  result <- result |>
    omopgenerics::filterSettings(.data$result_type == "measurement_records") |>
    dplyr::filter(.data$variable_name == "time")

  if (nrow(result) == 0) {
    mes <- cli::cli_warn("No results found with `result_type == 'measurement_records'`")
    return(emptyPlot(mes))
  }

  checkVersion(result)

  if(timeScale == "years"){
    result <- result |>
      dplyr::mutate("estimate_value" = as.character(as.numeric(.data$estimate_value)/365.25))
    lab <- "Years between measurements"
  }else{
    lab <- "Days between measurements"
  }

  if(plotType == "densityplot"){
    mes <- "plotType = 'densityplot' is not yet available. Please use plotType = 'boxplot'"
    return(emptyPlot(mes))
  }

  p <- visOmopResults::boxPlot(result,
                          x = x,
                          lower = "q25",
                          middle = "median",
                          upper  = "q75",
                          ymin = "min",
                          ymax = "max",
                          facet  = facet,
                          colour = colour,
                          label = character()) +
    ggplot2::labs(
      title = ggplot2::element_blank(),
      y = lab,
      x = ggplot2::element_blank()
    )

  return(p)
}

emptyPlot <- function(title = "No result to plot",
                      subtitle = "") {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )
}
pkgVersion <- function() {
  as.character(utils::packageVersion("MeasurementDiagnostics"))
}
