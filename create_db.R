# remove all objects in the global environment:
# rm(list = ls())
# ------------------------------------------------------------------------------
# Script to create a database from input prepared by UNHCR Geneva
# Author:
#   Maarten-Jan Kallen <mj@bedatadriven.com>
# Instructions:
#   Edit the values in the next section if desired and then source the file.
# ------------------------------------------------------------------------------

# database properties
database.name <- "SY-UNHCR-Programme"
database.description <- paste("UNHCR Programme in Syria:", Sys.Date())

## ISO 2 code for the country to be configured in Activity Info
country.code <- "SY"

# Default values for the forms:
location.type <- "UNHCR-Syria-Sites"
reporting.frequency <- "monthly"


###################################################################
## Focus API variable
country.focus <- "Syrian%20Arab%20Republic"
year.focus <- "2016"

## External file - not shared on github -- where we have the login details for 
source("login.R")

input <- list(
  partners = paste0(api.unhcr,"query=Partners&YEAR=",year.focus,"&OPERATION=",country.focus ,sep = "" ),
  output.indicators = paste0(api.unhcr,"query=PerfIndicators&YEAR=",year.focus,"&OPERATION=",country.focus ,sep = "" ),
  budget.indicators = paste0(api.unhcr,"query=OutputBudget&YEAR=",year.focus,"&OPERATION=",country.focus ,sep = "" )
)

#input <- list(
#  partners = file.path("data", "4_Partner.csv"),
#  output.indicators = file.path("data", "1_Form_Output-Indicator-Target.csv"),
#  budget.indicators = file.path("data", "3_Form_Output-$-Target.csv")
#)

###################################################################


# Target details (dates in YYYY-MM-DD format):
target.properties <- list(
  name = "zzz_OL",
  fromDate = "2016-01-01",
  toDate = "2016-12-31"
)

# ------------------------------------------------------------------------------
# Perform some checks before continuing:
# ------------------------------------------------------------------------------

# Check that the description of the database is not too long:
if (nchar(database.description) > 50L) {
  stop("database description must not exceed 50 characters")
}

# ------------------------------------------------------------------------------
# Proceed with core workflow:
# ------------------------------------------------------------------------------

action <- "create"

source("workflow.R")
