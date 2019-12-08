#' Function that finds the blood culture window according to the CDC definition of Sepsis.
#'
#' According to the CDC, in order for sepsis to take place, ordan dysfunction and antibiotic administration
#' must have taken place within 2 dasy before or 2 days after blood cultures being ordered. This function creates
#' window.
#'
#' @param dataset The dataset you are working with.
#' @return Gives a dataset with a new variable consisting of the difference between two time periods
#' @examples
#' @export


find_bx_window <- function(.data, timestamp_variable, blood_culture_time) {
  if (!lubridate::is.POSIXct(.data$timestamp_variable) == TRUE) {
    stop("timestamp_variable must be POSIXct")
  }

  timestamp_variable <- rlang::enquo(timestamp_variable)
  blood_cutlre_time <- rlang::enquo(blood_culture_time)

  .data <- mutate(.data, begining_time = !!blood_culture_time - lubridate::days(2),
                  ending_time = !!blood_culture_time + lubridate::days(2))

  .data$within_window <- dplyr::if_else(dplyr::between(timestamp_variable, begining_time, ending_time), TRUE, FALSE)

  .data <- .data[[-c("begining_time", "ending_time")]]

  return(.data)

}

