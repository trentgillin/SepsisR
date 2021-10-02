#' Assess Organ Failure Function
#'
#' Finds the current organ failure score.
#'
#' @param .data The dataset you are working with.
#' @param method A character value indicating what method you would like to use to score 
#' organ failure.
#' @param patientid A character or numeric value unique to each patient.
#' @param time A POSIXct value that represents when a vital was taken.
#' @param period A numeric value that indicates how long one vital is good for, default is 
#' one hour. Note: This will only fill in missing vitals.
#' @param vitals A character vector denoting the certain vitals necessary for each organ 
#' failure score.
#' @return Returns a data frame or tibble with a new column that denotes the desired score
#' @examples
#' score <- assess_organ(dataset, method = "SIRS", patientid = Encounter, 
#' time = Service_Timestamp, vitals = c("RR" = NA,"HR" = NA,"Temperature" = NA,
#' "WBC" = NA)}
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
