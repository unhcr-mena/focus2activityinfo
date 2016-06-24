# ------------------------------------------------------------------------------
# Script to create a database from input prepared by UNHCR Geneva
# Author:
#   Maarten-Jan Kallen <mj@bedatadriven.com>
# Versions:
#   - 2015-02-09: initial version
#   - 2015-03-12: revised to use new format input data and the new targets API
#   - 2015-03-23: revised (and renamed) to also update target values in an 
#                 existing database
#   - 2015-04-23: completely revised to match the work-flow diagrams
#   - 2015-05-04: further refactorings and addition of a test runner with dummy 
#                 data
#   - 2015-05-26: small corrections after initial test by UNHCR Geneva
#   - 2015-11-13: revise script to not delete indicators if their target value 
#                 is null
#   - 2015-11-30: major revision of the scripts to add the creation of impact 
#                 indicators
#   - 2015-12-14: move checks on country code and database description to be
#                 executed only when a new database is created
# ------------------------------------------------------------------------------

if (!exists("action")) {
  stop("Input is missing. Did you source 'create_db.R' or 'update_db.R' before?")
}

# This script requires the "activityinfo" package to be installed.
# See https://www.githun.com/bedatadriven/activityinfo-R for details.
library("activityinfo")
# To access a JSON web API, this script requires the 'rjson' package (available
# from CRAN):
library("rjson")

# Log into ActivityInfo:
# activityInfoLogin()

# load function definitions:
source("functions.R")

### load the partner table from file
partners <- loadPartners(input)

### load in the output indicators ("Output Indicator" and "Budget/Expenditure")
### from file
cat("loading indicators from FOCUS...")
output.indicators <- checkIndicatorData(loadIndicators(input))
cat("done.\n")

# transform the information into a list with the following structure:
# forms:
# - form category 1:
#   - form 1:
#     activityId (storage place for numeric identifier as returned by the API)
#     indicators (data frame with indicator name, category, target value and 
#                 indicatorId as returned by the API)
#   - form 2:
#     ...
# - form category 2:
# ...
forms <- lapply(
  split(output.indicators, output.indicators$form.category),
  function(x) {
    lapply(split(x, x$form),
           function(y) {
             list(activityId = NA_integer_,
                  indicators = data.frame(name = y$name,
                                          category = y$category,
                                          target = y$target,
                                          id = y$id,
                                          code = y$code,
                                          formula = y$formula,
                                          indicatorId = NA_integer_,
                                          unit = NA_character_,
                                          aggregation = NA_character_,
                                          stringsAsFactors = FALSE)
             )
           }
    )
  }
)

### if no database id is provided, we need to create a new database. Else, we
### need to check if the database is available:
switch(action,
       create = {
         # create the database using the API:
         cat("Creating database '", database.name,
             "' in ", country.code, "\n", sep = "")
         database.id <- createDatabase(name = database.name,
                                       description = database.description,
                                       countryId = country.code)
       },
       update = {
         # check if the database exists
         databases <- getResource("databases")
         if (!database.id %in% sapply(databases, function(db) db$id)) {
           stop("you do not have access to database with id ", database.id)
         }
         cat("Found database with id ", database.id,
             ", proceeding with update\n", sep = "")
       },
       stop("unrecognised action, must be one of 'create' or 'update'")
)

cat("Fetching schema for database", database.id, "\n")
schema <- getDatabaseSchema(database.id)

### check if the location type is a valid location type for the given country:
if (action == "update") {
  # The country code needs to be determined from the database schema when updating:
  countries <- getCountries()
  # Bug in activityinfo package which returns the character columns as factors:
  countries[] <- lapply(countries, function(col) {
    if (is.factor(col)) as.character(col) else col
  })
  country.code <- countries$code[match(schema$country$id, countries$id, nomatch = 0L)]
}

cat("Fetching location types for country ", country.code, " from ActivityInfo... ")
location.types <- do.call(rbind, lapply(getLocationTypes(country.code), function(loc) {
  data.frame(id = loc$id,
             name = loc$name,
             stringsAsFactors = FALSE)
}))
cat("done.\n")

if (!is.na(location.type)) {
  # Check if a valid location type is supplied to this script:
  i <- match(location.type, location.types$name, nomatch = 0L)
  if (i) {
    cat("Found location '", location.type, "' in country ", country.code,
        " with id ", location.types$id[i], "\n", sep = "")
  } else {
    stop("No location '", location.type, "' exists in country ", country.code,
         "; please use one of the following: ",
         paste(location.types$name, collapse = ", "))
  }
} else {
  # No location type is hard-coded in this script, therefore we assume "Village"
  # as the default, but ask the user to select from a list of available location
  # types:
  if (interactive()) {
    location.type <- getLocationTypeFromUser(location.types)
  } else {
    stop("this script cannot be run in batch mode, please run interactively from the command line")
  }
}

### check if the target set exists and, if not, create it:
targets <- executeCommandWithRetry("GetTargets", databaseId = database.id)
getTargetId <- lookupTarget(targets)
target.id <- getTargetId(name = target.properties$name,
                         fromDate = target.properties$fromDate,
                         toDate = target.properties$toDate)

if (is.na(target.id)) {
  # target set doesn't exist, create it now
  target.id <- executeCommandWithRetry("AddTarget",
                                       databaseId = database.id,
                                       target = target.properties)[[1]]
  # update the list of targets from ActivityInfo:
  targets <- executeCommandWithRetry("GetTargets", databaseId = database.id)
}

# create a lookup function for target values:
getTargetValue <- lookupTargetValue(targets, target.id)
# create a lookup function for partners:
getPartnerId <- lookupPartner(schema)

### add the partners to the new database:
for (i in seq(nrow(partners))) {
  partner.id <- getPartnerId(partners$name[i])
  if (is.na(partner.id)) {
    # partner doesn't exist, add it now:
    cat("-- Adding partner (", i, "/", nrow(partners), "): ",
        partners$name[i], "\n", sep = "")
    success <- TRUE
    tryCatch(
      partner.id <-
        addPartner(databaseId = database.id,
                   partnerName = substr(partners$name[i], 0, 16),
                   fullPartnerName = substr(partners$full.name[i], 0, 64)),
      error=function(e) {
        success <<- FALSE
      },
      finally=if (!success) {
        cat("-- WARNING: failed to add partner ", partners$name[i],
            " with error: ", geterrmessage(), "\n", sep="")
        next
      }
    )
    cat("   succesfully added partner with id ", partner.id, "\n", sep = "")
  } else {
    cat("   partner '", partners$name[i],
        "' already exists in database with id ", database.id, "\n", sep = "")
  }
  # store the identifier of the new or existing partner:
  partners$id[i] <- partner.id
}

### prepare a number of lookup functions:
getActivityId <- activityFactory(schema,
                                 locationType = location.type,
                                 reportingFrequency = reporting.frequency,
                                 classicView = TRUE)
getIndicatorId <- lookupIndicator(schema)
getIndicatorName <- lookupIndicatorName(schema)

### add/update the forms and their indicators in the database:
for (form.category in names(forms)) {
  cat("Form category '", form.category, "' has ",
      length(forms[[form.category]]), " forms.\n", sep = "")
  
  for (form in names(forms[[form.category]])) {
    
    form.indicators <- forms[[form.category]][[form]][["indicators"]]
    
    cat("-- Form '", form, "' has ", nrow(form.indicators), " indicators in FOCUS.\n", sep="")
    
    # loop over indicators in the current (FOCUS) form:
    for (i in seq(nrow(form.indicators))) {
      
      # lookup the FOCUS indicator in ActivityInfo:
      indicator.id <- getIndicatorId(code = form.indicators$id[i])
      
      if (is.na(indicator.id)) {
        # FOCUS indicator doesn't (yet) exist in ActivityInfo, create indicator:
        
        # retrieve identifier of existing or new form:
        activity.id <- getActivityId(name = form, category = form.category)
        
        # skip to the next indicator if we failed to create the form:
        if (is.na(activity.id)) next
        
        # Prepare the properties of the indicator, we 
        properties <- list(activityId = activity.id,
                           name = form.indicators$name[i],
                           category = form.indicators$category[i],
                           units = guessUnits(form.indicators$name[i]),
                           aggregation = guessAggregation(form.indicators$name[i]))
        if (!is.na(form.indicators$formula[i])) {
          # calculated indicator:
          properties$expression <- form.indicators$formula[i]
          properties$calculatedAutomatically <- TRUE
        }
        if (is.na(form.indicators$code[i])) {
          # indicator NOT to be used in an expression, use the FOCUS id as the code:
          properties$nameInExpression <- form.indicators$id[i]
        } else {
          # indicator to be used in an expression, use the code:
          properties$nameInExpression <- form.indicators$code[i]
        }
        
        indicator.id <- executeCommandWithRetry("CreateEntity",
                                                entityName = "Indicator",
                                                properties = properties)$newId
        
        cat("   succesfully added indicator with id ", indicator.id, "\n", sep = "")
        
        
      } else {
        # FOCUS indicator exists in ActivityInfo, remove obsolete mark if present:
        indicator.name <- getIndicatorName(indicator.id)
        if (isObsolete(indicator.name)) {
          toggleObsolete(indicator.name, indicator.id)
        }
      }
      
      # retrieve identifier of existing or new form:
      activity.id <- getActivityId(name = form, category = form.category)
      
      # store the activity identifier in our forms list:
      forms[[form.category]][[form]][["activityId"]] <- activity.id
      # store the indicator identifier in our forms list:
      forms[[form.category]][[form]]$indicators$indicatorId[i] <- indicator.id
      # add units and aggregation functions to the forms list:
      forms[[form.category]][[form]]$indicators$unit[i] <-
        guessUnits(form.indicators$name[i])
      forms[[form.category]][[form]]$indicators$aggregation[i] <-
        guessAggregation(form.indicators$name[i])
      
      if (action == "update" || (action == "create" && !is.na(form.indicators$target[i]))) {
        # update target value or set one if a target value is given
        
        cat("   setting/updating target value on indicator with id ", indicator.id,
            "\n", sep = "")
        
        indicator.target <- if (is.na(form.indicators$target[i])) {
          # sending NULL to the API will remove any existing target values
          NULL
        } else {
          form.indicators$target[i]
        }
        
        executeCommandWithRetry("UpdateTargetValue",
                                targetId = target.id,
                                indicatorId = indicator.id,
                                changes = list(value = indicator.target))
      } else {
        cat("   no target value on indicator with id ", indicator.id,
            " to be set\n", sep = "")
      }
      
    } # end of loop over indicators
  } # end of loop over forms
} # end of loop over form categories

if (action == "update") {
  # when we update indicators and their target values, we need to check for
  # indicators which were added earlier, but no longer exist in the FOCUS input
  # data
  
  indicator.table <- extractIndicatorTable(schema)
  rows <- which(is.FOCUS(indicator.table$id) &
                  !indicator.table$id %in% output.indicators$id)
  
  if (length(rows)) {
    # one or more indicators in AI come from FOCUS, but are no longer in the
    # list of indicators to update
    
    cat("UPDATE: ", length(rows),
        " indicator(s) in ActivityInfo is/are FOCUS indicator(s), ",
        "but is/are not in the input data.\n", sep = "")
    
    for (row in rows) {
      indicators.with.data <-
        getIndicatorsWithValues(indicator.table$activityId[row])
      
      if (indicator.table$indicatorId[row] %in% indicators.with.data) {
        # the indicator has data, mark as obsolete
        cat("UPDATE: indicator with id '", indicator.table$indicatorId[row],
            "' in ActivityInfo is a (former) FOCUS indicator and has data entries: ",
            "marking indicator as obsolete...\n", sep = "")
        
        indicator.name <- getIndicatorName(indicator.table$indicatorId[row])
        if (!isObsolete(indicator.name)) {
          toggleObsolete(indicator.name, indicator.table$indicatorId[row])
        }
        
        # remove target value:
        executeCommandWithRetry("UpdateTargetValue",
                       targetId = target.id,
                       indicatorId = indicator.table$indicatorId[row],
                       changes = list(value = ""))
      } else {
        # the indicator has no data entries, therefore we delete it
        deleteIndicator(indicator.table$indicatorId[row])
        cat("UPDATE: indicator with id '", indicator.table$indicatorId[row],
            "' in ActivityInfo is a (former) FOCUS indicator and has no data entries: ",
            "deleting indicator...\n", sep ="")
      }
    }
  }
  
  cat("UPDATE: finished going over ActivityInfo indicators.\n")
}

# remove forms with no indicators:
cat("Fetching schema for database", database.id, "to remove forms without indicators\n")
schema <- getDatabaseSchema(database.id)

for (activity in schema$activities) {
  if (!length(activity$indicators)) {
    cat("Form '", activity$name, "' has no indicators: deleting form ", activity$id, sep = "")
    deleteActivity(activity$id)
  }
}

cat("Done, ", action, "d database with id ", database.id,"!\n", sep = "")
