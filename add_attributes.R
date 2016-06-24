# remove all objects in the global environment:
# rm(list = ls())
# ------------------------------------------------------------------------------
# Script to add one or more attributes to all forms within a database
# Author:
#   Maarten-Jan Kallen <mj@bedatadriven.com>
# Instructions:
#   Edit the values in the next section if desired and then source the file.
# ------------------------------------------------------------------------------

database.id <- 3639

# 'attributes' must be a list of lists:
# list(
#   list(...),
#   list(...),
#   ...
# )
# The following is an example:
attributes <- list(
  list(
    # a single string:
    name    = "Funded By",
    # 'SINGLE' or 'MULTIPLE':
    type    = "SINGLE",
    # at least two strings:
    options = c("UNHCR", "ECHO", "UNICEF", "UNOCHA"),
    # allow only users with design priviliges to edit the attributes ('TRUE', otherwise 'FALSE'):
    locked  = TRUE
  ),       
  list(
    name    = "Appeal",
    type    = "MULTIPLE",
    options = c("Refugee Response Plan", "Humanitarian Response Plan"),
    locked  = TRUE
  )
)

# ------------------------------------------------------------------------------
# FUNCTION DEFINITIONS
# ------------------------------------------------------------------------------

getOkayFromUser <- function(msg) {
  choice <- readline(sprintf("%s [Y/n] ", msg))
  if (choice %in% c("", "Y", "y", "Yes", "yes")) {
    # "" is the result of pressing Enter
    TRUE
  } else if (choice %in% c("n", "N", "No", "no")) {
    FALSE
  } else {
    stop(choice, " is not a recognized option, please re-run this script and try again")
  }
}


# ------------------------------------------------------------------------------
# SCRIPT BODY
# ------------------------------------------------------------------------------
library("activityinfo")

stopifnot(length(attributes) > 0L)

cat("Checking ", length(attributes), " attributes... ", sep = "")
attribute.names <- sapply(attributes, function(a) {
  # attribute should have type SINGLE or MULTIPLE + have at least two options:
  stopifnot(a$type %in% c("SINGLE", "MULTIPLE"), length(a$options) > 1L)
  a$name
  })
cat("OK.\n")

if (is.na(database.id)) {
  stop("you forgot to set the database identifier at the top of the script!")
}
cat("Retrieving schema for database ", database.id, "... ", sep = "")
schema <- getDatabaseSchema(database.id)
cat("OK.\n")

form.ids <- sapply(schema$activities, function(form) form$id)

# Check if any of the attributes already exist in one or more of the database forms:
cat("Checking new attributes against those in the forms... ")
attributeGroups <- lapply(schema$activities, function(form) {
  if (is.null(form$attributeGroups)) {
    character(0)
  } else {
    sapply(form$attributeGroups, function(group) group$name)
  }
})
attributePresence <- lapply(attributes, function(attribute) {
  sapply(attributeGroups, function(group) attribute$name %in% group)
})
cat("OK\n")

# If attributes already exist we need permission from the user to proceed with
# adding options to these attributes in the database:
if (any(unlist(attributePresence))) {
  for (i in length(attributePresence)) {
    cat("Attribute '", attributes[[i]]$name, "' exists in one or more forms.\n", sep = "")
  }
  stopifnot(getOkayFromUser("Existing attributes will have options added, do you want to proceed?"))
}

# All is well, proceed with adding/updating attributes in ActivityInfo:
for (i in seq_along(form.ids)) {
  form.id <- form.ids[i]
  attributeGroups <- do.call(rbind, lapply(schema$activities[[i]]$attributeGroups, function(x) {
    data.frame(id = x$id, name = x$name, stringsAsFactors = FALSE)
    }))
  for (j in seq_along(attributes)) {
    attribute <- attributes[[j]]
    if (match(attribute$name, attributeGroups$name, nomatch = 0L)) {
      # attribute exists
      group.id <- attributeGroups$id[match(attribute$name, attributeGroups$name, nomatch = 0L)]
      existingOptions <- sapply(schema$activities[[i]]$attributeGroups[[j]]$attributes, function(x) x$name)
    } else {
      # attribute doesn't exist
      cat("Adding attribute '", attribute$name, "' to form ", form.id, "...", sep = "")
      group.id <- createEntity("AttributeGroup", properties = list(
        activityId = form.id,
        name = attribute$name,
        multipleAllowed = switch(attribute$type, SINGLE=FALSE, MULTIPLE=TRUE),
        mandatory = FALSE,
        workflow = attribute$locked)
      )
      cat("OK.\n")
      existingOptions <- character(0)
    }
    for (option in setdiff(attribute$options, existingOptions)) {
      cat("Adding option '", option, "' to attribute '", attribute$name, "' in form ", form.id, "... ", sep = "")
      attribute.id <- createAttribute(attributeGroupId = group.id,
                                      name = option)
      cat("OK.\n")
    }
    
  }
}
