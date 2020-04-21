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
  SBP <- rlang::enquo(SBP)
  DBP <- rlang::enquo(DBP)
  Vasopressor <- rlang::enquo(Vasopressor)
  Vasopressor_dose <- rlang::enquo(Vasopressor_dose)
  
  # get map
  .data <- dplyr::mutate(.data, MAP = (!!SBP + (2 * !!DBP))/3)
  
  # warn if detect vasopressors other than dobutamine and dopamine
  .vaso_check <- dplyr::filter_at(.data, dplyr::vars(!!Vasopressor), dplyr::any_vars(!stringr::str_detect(stringr::str_to_lower(.), "dopamine|dobutamine|norepinephrine|epinephrine")))
  
  if (nrow(.vaso_check) > 0) {
  warning("Detected vasopressors other than dopamine, dobutamine, norepinephrine, and epinephrine, as of right now, those are the only vasopressors considered")
  }

  # create cardiac flag
  .data <- dplyr::mutate(.data, cardiovascular_flag = dplyr::case_when(
                                            (stringr::str_detect(stringr::str_to_lower(!!Vasopressor), "norepinephrine|epinephrine") & !!Vasopressor_dose > 0.1) |
                                              (stringr::str_detect(stringr::str_to_lower(!!Vasopressor), "dopamine") & !!Vasopressor_dose > 15)~ 4,
                                            (stringr::str_detect(stringr::str_to_lower(!!Vasopressor), "dopamine") &
                                               !!Vasopressor_dose > 5) | (stringr::str_detect(stringr::str_to_lower(!!Vasopressor), "norepinephrine|epinephrine") &
                                                                                       !!Vasopressor_dose <= 0.1) ~ 3,
                                            stringr::str_detect(stringr::str_to_lower(!!Vasopressor), "dobutamine")|
                                              (stringr::str_detect(stringr::str_to_lower(!!Vasopressor), "dopamine") & 
                                                 !!Vasopressor_dose <= 5) ~ 2,
                                            MAP < 70 ~ 1,
                                            MAP >= 70 ~ 0))
  return(.data)
}

# so that check will ignore "."
utils::globalVariables(c("."))

