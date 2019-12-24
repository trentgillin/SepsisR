#' Time Between Function
#'
#' Finds the difference between two timestamps.
#'
#' @param dataset The dataset you are working with.
#' @param begining_time POSIXct variable that denotes the start of the time interval.
#' @param end_time POSIXct variable that that denotes the end of the time interval.
#' @param new_name Character variable that indicates what you want to the time 
#' difference variable to be called.
#' @param unitx Character variable that indicates what units to return time difference, 
#' default is days.
#' @return Gives a dataset with a new variable consisting of the difference between 
#' two time periods
#' @examples
#' \dontrun{
#' data <- data %>% calc_time_between(culture_start, culture_end, unitx = "days")
#' data <- calc_time_between(data, lactate_start, lactate_end, unitx = "hours")
#' }
#' @export

calc_time_between <- function(dataset, begining_time, end_time, 
                              new_name = "time_diff", unitx = "days") {
  begining_time <- rlang::enquo(begining_time)
  end_time <- rlang::enquo(end_time)

  beginings_time <- rlang::quo_name(begining_time)
  ends_time <- rlang::quo_name(end_time)

  if (is.numeric(dataset[[beginings_time]])|is.numeric(dataset[[ends_time]])) {
    stop("Begining and start times cannot be numeic, please convert to date or POSIXct.")
  }

  if (is.character(dataset[[beginings_time]])|is.character(dataset[[ends_time]])) {
    stop("Begining and start times cannot be character, please convert to date or POSIXct.")
  }

  if (!unitx %in% c("days", "mins", "hours")) {
    stop("Unitx can only beo one of days, mins, or hours")
  }

  if (lubridate::is.Date(dataset[[beginings_time]]) |
      lubridate::is.Date(dataset[[ends_time]])) {
    dataset <- dplyr::mutate(dataset,
                            time_diff = as.numeric(lubridate::as.difftime(!!end_time - !!begining_time), units = unitx))
  } else {

    dataset <- dplyr::mutate(dataset, 
                             time_diff_1 = lubridate::as.difftime(!!end_time - !!begining_time))
    dataset <- dplyr::mutate(dataset, time_diff = as.numeric(lubridate::as.period(time_diff_1), unitx))
    dataset <- dplyr::select(dataset, -time_diff_1)
  }
  colnames(dataset)[colnames(dataset) == "time_diff"] <- new_name
  return(dataset)
}

# so that check will ignore time_diff_1
utils::globalVariables(c("time_diff_1"))
