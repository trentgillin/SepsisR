#' Assess Organ Failure Function
#'
#' Finds the current organ failure score.
#'
#' @param .data The dataset you are working with.
#' @param method A character value indicating what method you would like to use to score organ failure.
#' @examples
#' score <- assess_organ(dataset, method = "SIRS")
#' score <- assess_rogan(dataset, method = "SOFA")
#' @export

assess_organ <- function(.data, method = "SIRS") {
  if (method == "SIRS") {
    new_data <- find_sirs(.data, patientid = encounter, time = service_timestamp, period = 1)
  } else if (method == "SOFA") {
    new_data <- find_sofa(.data)
  } else {
    new_data <- find_qsofa(.data)
  }

  return(new_data)
}
