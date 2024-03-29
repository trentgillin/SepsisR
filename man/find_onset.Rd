% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_onset_type.R
\name{find_onset}
\alias{find_onset}
\title{Onset Type Function
A function used to determine if a patient has community sepsis or hospital onset sepsis}
\usage{
find_onset(.data, blood_day, first_qad, patientid, admission_day)
}
\arguments{
\item{.data}{Your dataset}

\item{blood_day}{This is a Date column that represents when your blood cultures were taken}

\item{first_qad}{This is a Date column that represents your first qualifying antibiotic day (see CDC definition) time}

\item{patientid}{A unique identifier for each patient}

\item{admission_day}{A Date object for the day a patient was admitted to the hospital}
}
\value{
Returns a dataset with a new column that flags a patient has having 'Community' or 'Hosptial' onset
}
\description{
Onset Type Function
A function used to determine if a patient has community sepsis or hospital onset sepsis
}
\examples{
\dontrun{
final_data <- data \%>\% find_onset(blood_day = blood_service_day, 
                                  first_qad = first_qad, 
                                  patientid = encounter, 
                                 admission_day = admisison_date)
}
}
