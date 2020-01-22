###########################################################################
# SepsisR: A R package to help identify sepsis patients
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->
###########################################################################

# Package Details----------------------------------------------------------

Sepsis has a complicated and varied presentation [Ref 1]. Many people
know what sepsis is, but it is hard to assess clear vital indicators [Ref 2].
The purpose of the package is to provide functions in order to make the process
of identifying and analyzing sepsis easier. The assess_organ function is the
main function of this package; it allows you to assess organ failure using
one of the three common organ failure scores, SIRS, SOFA, and qSOFA [Ref 3].
Currently, all score vital thresholds are based off the MD Calc Website 
(https://www.mdcalc.com/sirs-sepsis-septic-shock-criteria). 
The package also has a few helper functions including find_bx_window and find_qad.
These functions are from the CDC (Centers for Disease and Control) definition
of Sepsis. This package is not in anyway supported by the CDC or Emory Healthcare. 

# Example------------------------------------------------------------------

your_dataset <- asses_organ(your_dataset, method = "SIRS",
				 patientid = Encounter, 
				time = Service_Timestamp, 
				vitals = c("RR" = "RR", "SBP" = "SBP", 
					"GCS" = "GCS", 
					"Temperature" = "Temperature",
					"HR" = "HR"))


# References---------------------------------------------------------------

R. Ihaka and R. Gentleman. R: A language for data analysis and graphics. Journal of Computational and Graphical Statistics, 5(3):299–314, 1996. URL https://doi.org/10.1080/ 10618600.1996.10474713. [p1]

[Ref 1]
[Ref 2]
[Ref 3]
[MD Calc Website Reference]
