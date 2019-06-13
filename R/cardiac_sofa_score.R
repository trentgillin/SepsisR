#' MAP calculation
#'
#' Finds the current Mean Arterial Pressure (MAP) score
#'
#'@param .data Your dataset
#' @param sbp A numeric column that represents the systolic blood pressure
#' @param dbp A numeric column that represents the dystolic blood pressure
#' @examples
#' result <- get_map(sbp, dbp)

get_map <- function(sbp, dbp) {
  map <- (sbp + (2 * dbp))/3
  return(map)
}

#' SOFA Cardiac calculation function
#'
#' Finds the current cardiac score for the SOFA score
#'
#'@param .data Your dataset
#' @param sbp A numeric column that represents the systolic blood pressure
#' @param dbp A numeric column that represents the dystolic blood pressure
#' @param vasopressor A character column that represents a list of vasopressors given to a patient
#' @param vasopressor_dose A numeric column that represents the dosage of vasopressor given, needs to be in mcg/kg/min
#' @examples
#' cardiovascular_flag = calc_card_sofa(.data, sbp, dbp, vasopressor, vasopressor_dose)

calc_card_sofa <- function(.data, sbp, dbp, vasopressor, vasopressor_dose) {
  # get map
  .data <- mutate(.data, MAP = get_map(sbp, dbp))

  # create cardiac flag
  .data <- mutate(.data, cardiac_flag = case_when(MAP < 70 ~ 1,
                                                  str_detect(str_to_lower(vasopressor), "dobutamine")|
                                                    (str_detect(str_to_lower(vasopressor), "dopamine") & vasopressor_dose <= 5)~2,
                                                  ))
  return(.data)
}


