#' MAP calculation
#'
#' Finds the current Mean Arterial Pressure (MAP) score
#'
#'@param .data Your dataset
#' @param sbp A numeric column that represents the systolic blood pressure
#' @param dbp A numeric column that represents the dystolic blood pressure
#' @examples
#' result <- get_map(sbp, dbp)

get_map <- function(sbp, dpb) {
  map <- (sbp + (2 *dbp))/3
}

#' SOFA Cardiac calculation function
#'
#' Finds the current cardiac score for the SOFA score
#'
#'@param .data Your dataset
#' @param sbp A numeric column that represents the systolic blood pressure
#' @param dbp A numeric column that represents the dystolic blood pressure
#' @param vasopressor A character column that represents a list of vasopressors given to a patient
#' @param vasopressor_dose A numeric column that represents the dosage of vasopressor given
#' @examples
#' cardiovascular_flag = calc_card_sofa(.data, sbp, dbp, vasopressor, vasopressor_dose)
#' @export

calc_card_sofa <- function(.data, sbp, dpb, vasopressor, vasopressor_dose) {
  .data <- mutate(.data, MAP = get_map(sbp, dpb))

}


