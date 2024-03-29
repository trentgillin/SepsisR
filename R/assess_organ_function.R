#' Assess Organ Failure Function
#'
#' Finds the current organ failure score.
#'
#' @param .data The dataset you are working with.
#' @param method A character value indicating what method you would like to use to score 
#' organ failure.
#' @param patientid A character or numeric value unique to each patient.
#' @param time A POSIXct value that represents when a vital was taken.
#' @param period A numeric value that indicates how long to carry over a vital when next is 
#' missing, default is one hour.
#' @param vitals A character vector denoting the certain vitals necessary for each organ 
#' failure score.
#' @return Returns a dataset with a new column that denotes the desired score
#' @examples
#' score <- assess_organ(sirs_data, method = "SIRS", patientid = Encounter, 
#' time = Service_Timestamp, vitals = c("RR" = "RR","HR" = "HR","Temperature" = "Temperature",
#' "WBC" = "Wbc"))
#' @export

assess_organ <- function(.data, method = c("SIRS", "SOFA", "qSOFA"),
                         patientid,
                         time,
                         period = 1,
                         vitals) {

  patientid <- rlang::enquo(patientid)
  time <- rlang::enquo(time)

  method <- match.arg(method)
  switch(method,
         SIRS = {
           new_data <- find_sirs(.data,
                          !!patientid,
                          !!time,
                          period,
                          vitals)},

         SOFA = {
           new_data <- find_sofa(.data,
                          !!patientid,
                          !!time,
                          period,
                          vitals)},

         qSOFA = {
           new_data <-find_qsofa(.data,
                            !!patientid,
                            !!time,
                            period,
                            vitals)})

         return(new_data)
}
