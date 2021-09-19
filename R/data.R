#' sirs dataset
#'
#' A mock-up dataset to test the sirs organ failure score for one patient.
#' 
#' @format A data frame with 9 rows and 6 variables:
#' \describe{
#'   \item{Encounter}{A unique patient id}
#'   \item{Service_Timestamp}{Timestamps of when a variable was taken}
#'   \item{HR}{Heart Rate, A list of heart rate readings}
#'   \item{Temperature}{A list of temperature readings in Celsius}
#'   \item{RR}{Respiratory Rate, A list of respiratory rate readings}
#'   \item{Wbc}{White Blood Cell, A list of white blood cell readings in mmm^3}
#'   ...
#' }
"sirs_data"


#' sofa dataset
#'
#' A mock-up dataset to test the sofa organ failure score for one patient.
#' 
#' @format A data frame with 9 rows and 12 variables:
#' \describe{
#'   \item{Encounter}{A unique patient id}
#'   \item{Service_Timestamp}{Timestamps of when a variable was taken}
#'   \item{Pa02}{Partial Pressure of oxygen, A list of Pa02s}
#'   \item{Fi02}{Fraction of inspired oxygen, A list of Fi02s}
#'   \item{Platelet}{A list of platelet counts}
#'   \item{Bili}{Bilirubin, A list of Bilirubin levels}
#'   \item{GCS}{Glasgow Coma Score, A list of GCSs}
#'   \item{Creatinine}{A list of creatinine levels}
#'   \item{SBP}{Systolic Blood Pressure, A list of SBPs}
#'   \item{DBP}{Diastolic Blood Pressure, A list of DBPs}
#'   \item{Vasopressor}{A list of vasopressors given to a patient}
#'   \item{Vasopressor Dosage}{A list of corresponding dosages of vasopresssors given to a patient}
#'   ...
#' }
"sofa_data"

#' qsofa dataset
#'
#' A mock-up dataset to test the qsofa organ failure score.
#' 
#' @format A data frame with 9 rows and 5 variables:
#' \describe{
#'   \item{Encounter}{A unique patient id}
#'   \item{Service_Timestamp}{Timestamps of when a variable was taken}
#'   \item{RR}{Respiratory Rate, A list of respiratory rate readings}
#'   \item{SBP}{Systolic Blood Pressure, A list of SBPs}
#'   \item{GCS}{Glasgow Coma Score, A list of GCSs}
#'   ...
#' }
"qsofa_data"