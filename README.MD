
# SepsisR: A R package to help identify sepsis patients
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/trentgillin/Sepsis.svg?branch=master)](https://travis-ci.org/trentgillin/Sepsis)
<!-- badges: end -->

# Package Details

Sepsis has a complicated and varied presentation. Many professionals recognize
sepsies presentation, but the literature is undecided when it comes to an exact
clincal definition. The purpose of the package is to provide functions in order 
to make the process of identifying and analyzing sepsis easier. The assess_organ 
function is the main function of this package; it allows you to assess organ failure using
one of the three common organ failure scores, SIRS, SOFA, and qSOFA.
Currently, all score vital thresholds are based off the MD Calc Website 
(https://www.mdcalc.com/sirs-sepsis-septic-shock-criteria). 
The package also has a few helper functions including find_bx_window and find_qad, 
from the CDC (Centers for Disease and Control) definition of Sepsis. 
This package is not in anyway supported by the CDC or Emory Healthcare. 

# Example

your_dataset <- asses_organ(your_dataset, method = "SIRS",
				 patientid = Encounter, 
				time = Service_Timestamp, 
				vitals = c("RR" = "RR", "SBP" = "SBP", 
					"GCS" = "GCS", 
					"Temperature" = "Temperature",
					"HR" = "HR"))

