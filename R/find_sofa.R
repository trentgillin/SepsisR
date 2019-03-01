#' SOFA calculation function
#'
#' Finds the current organ failure score using SOFA.
#'
#' @param .data The dataset you are working with.
#' @param patientid A value indicating the unique patient id, usually an encounter number.
#' @param time A POSIXct value indicating the timestamp for when vitals where taken.
#' @param period A numeric value that indicates how long one vital is good for, default is one hour.
#' @examples
#' dataset <- find_sofa(dataset, Encounter, `Vital Timestamp`)
#' @export

find_sofa <- function(.data, patientid, time, period = 1) {
  if(!"PaO2" %in% colnames(.data)) {
    stop("You need PaO2 to calculate sofa.")
  }  else if (!"FiO2" %in% colnames(.data)) {
    stop("You need FiO2 to calculate sofa")
  } else if (!"bilirubin" %in% colnames(.data)) {
    stop("You need bilirubin to calculate sofa, please make sure it is in mg/dl")
  } else if (!"MAP" %in% colnames(.data)) {
    stop("You need MAP to calculate sofa")
  } else if (!"vasopressor" %in% colnames(.data)) {
    stop("You need vasopressor to calculate sofa")
  } else if (!"gcs" %in% colnames(.data)) {
    stop("You need gcs to calculate sofa")
  } else if (!c("creatinine", "urine_output") %in% colnames(.data)) {
    stop("You need either creatinine or urine_output to calculate sofa")
  } else if (!"platelets" %in% colnames(.data)) {
    stop("You need platelets to calculate sofa")
  }

  parentid <- enquo(parentid)
  time <- enquo(time)

# calculate Pao2/FiO2
  .data <- mutate(.data, PaO2_FiO2 = PaO2/FiO2)

# indicate when desired levels are hit
  .data <- mutate(.data, PaO2_FiO2_flag = case_when(
    PaO2_FiO2 < 400 ~ 1,
    PaO2_FiO2 < 300 ~ 2,
    PaO2_FiO2 < 200 ~ 3,
    PaO2_FiO2 < 100 ~ 4),
  platelets_flag = case_when(platelets < 150 ~ 1,
                             platelets < 100 ~ 2,
                             platelets < 50 ~ 3,
                             platelets < 20 ~ 4),
  bilirubin_flag = case_when(between(bilirubin, 1.2, 1.9) ~ 1,
                             between(bilirubin, 2.0, 5.9) ~ 2,
                             between(bilirubin, 6.0, 11.9) ~ 3,
                             bilirubin > 12.0 ~ 4),
  gcs_flag = case_when(between(gcs, 13, 14) ~ 1,
                       between(gcs, 10, 12) ~ 2,
                       between(gcs, 6, 9) ~ 3,
                       gcs <6 ~ 4),
  cardiovascular_flag = calc_card_sofa(.data, sbp, dbp, vasopressor, vasopressor_dose),
  creatinine_flag = case_when(between(creatinine, 1.2, 1.9) ~ 1,
                              between(creatinine, 2.0. 3.4) ~ 2,
                              between(creatinine, 3.5, 4.9) ~ 3,
                              creatinine > 5.0, ~ 4))
}
