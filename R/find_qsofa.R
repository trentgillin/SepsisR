#' qSOFA calculation function
#'
#' Finds the current qSOFA score
#'
#'@param .data Your dataset
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long one vital is good for, default is one hour.
#' @examples
#' @export

find_qsofa <- function(.data, patientid, time, period = 1) {
  if (!"rr" %in% colnames(.data)) {
    stop("You need respiratory rate to calcuate qsofa")
  } else if (!"sbp" %in% colnames(.data)) {
    stop("You need systolic blood pressure to calcuate qsofa")
  } else if (!"gcs" %in% colnames(.data)) {
    stop("You need glasgow coma scale to calcuate qsofa")
  }

}
