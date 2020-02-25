#' Function that finds the blood culture window according to the CDC definition of Sepsis.
#'
#' According to the CDC, in order for sepsis to take place, ordan dysfunction and antibiotic 
#' administration must have taken place within 2 dasy before or 2 days after blood cultures 
#' being ordered. 
#'
#' @param .data The dataset you are working with.
#' @param timestamp_variable The time for the variable you want to test it it occurs within the
#' blood culture window
#' @param blood_culture_time The timestamp of your blood culture value
#' @return Gives a dataset with a new variable consisting of the difference between two time periods
#' @examples
#' \dontrun{
#' result <- find_bx_window(data, servie_time, culture_draw_time)
#' }
#' @export


find_bx_window <- function(.data, timestamp_variable, blood_culture_time) {

  timestamp_variable <- rlang::enquo(timestamp_variable)
  blood_culture_time <- rlang::enquo(blood_culture_time)
  
  if (!rlang::as_label(timestamp_variable) %in% colnames(.data)) {
    stop("Your timestamp_variable is not a column in your dataset")
  }

  if (lubridate::is.POSIXct(.data[[rlang::as_label(timestamp_variable)]]) == FALSE) {
    stop("timestamp_variable must be POSIXct")
  }
  
  .data <- calc_time_between(.data, !!blood_culture_time, !!timestamp_variable, unitx = "days")
  .data <- dplyr::mutate(.data, time_diff = abs(time_diff),
                  within_window = dplyr::if_else(time_diff <= 2, TRUE, FALSE))
  
  .data <- dplyr::select(.data, -time_diff)

  return(.data)

}

# so that check will ignore begining and ending time
utils::globalVariables(c("begining_time", "ending_time"))

