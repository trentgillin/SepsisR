#' SOFA Cardiac calculation function
#'
#' Finds the current cardiac score for the SOFA score
#'
#'@param .data Your dataset
#' @param SBP A numeric column that represents the systolic blood pressure
#' @param DBP A numeric column that represents the dystolic blood pressure
#' @param Vasopressor A character column that represents a list of vasopressors 
#' given to a patient
#' @param Vasopressor_dose A numeric column that represents the dosage of vasopressor 
#' given, needs to be in mcg/kg/min
#' @examples
#' \dontrun{
#' cardiovascular_flag = calc_card_sofa(.data, sbp, dbp, vasopressor, vasopressor_dose)}

calc_card_sofa <- function(.data, SBP, DBP, Vasopressor, Vasopressor_dose) {
  # get map
  .data <- dplyr::mutate(.data, MAP = (SBP + (2 * DBP))/3)

  # create cardiac flag
  .data <- dplyr::mutate(.data, cardiovascular_flag = dplyr::case_when(MAP < 70 ~ 1,
                                            str_detect(str_to_lower(Vasopressor), "dobutamine")|
                                            (str_detect(str_to_lower(Vasopressor), "dopamine") & 
                                                       Vasopressor_dose <= 5)~2,
                                                  ))
  return(.data)
}


