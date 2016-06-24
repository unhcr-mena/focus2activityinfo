# remove all objects in the global environment:
# rm(list = ls())
# ------------------------------------------------------------------------------
# Script to update an existing database from input prepared by UNHCR Geneva
# Author:
#   Maarten-Jan Kallen <mj@bedatadriven.com>
# Instructions:
#   Edit the values in the next section if desired and then source the file.
# ------------------------------------------------------------------------------

# database to be updated:
database.id <- 5158
database.description <- paste("Pilot 2016 - DB updated on", Sys.Date())

# Default values for the forms:
location.type <- "UNHCR-Tunisia-Sites"
reporting.frequency <- "monthly"

# Target details (dates in YYYY-MM-DD format):
target.properties <- list(
  name = "zzz_OL",
  fromDate = "2016-01-01",
  toDate = "2016-12-31"
)




executeCommand("UpdateEntity", 
               entityName = "UserDatabase", 
               id = database.id, changes = 
                 list(fullName = database.description))



###################################################################
## Focus API variable
country.focus <- "Syrian%20Arab%20Republic"
year.focus <- "2016"

## External file - not shared on github -- where we have the login details for 
source("login.R")

input <- list(
  partners = paste0(api.unhcr,"query=Partners&YEAR=",year.focus,"&OPERATION=",country.focus),
  output.indicators = paste0(api.unhcr,"query=PerfIndicators&YEAR=",year.focus,"&OPERATION=",country.focus),
  budget.indicators = paste0(api.unhcr,"query=OutputBudget&YEAR=",year.focus,"&OPERATION=",country.focus)
)
###################################################################



# ------------------------------------------------------------------------------

action <- "update"

source("workflow.R")
