requireNumeric <- function(x) {
  if (is.numeric(x)) x else stop("value must be numeric")
}

requireCharacter <- function(x) {
  if (is.character(x)) x else stop("value must be a character vector")  
}

requireValidFreq <- function(freq) {
  ifelse(freq %in% c("once", "monthly"),
         freq,
         stop("reporting frequency is not one of 'once' or 'monthly'"))
}

requireValidDate <- function(date) {
  ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}", date) & length(date) == 1L,
         date,
         stop("date is not a string in the format 'YYYY-MM-DD'"))
}

strip.white <- function(x) {
  gsub("^\\s+|\\s+$", "", requireCharacter(x))
}
# unit tests for 'strip.white':
local({
  stopifnot(identical(strip.white(" foobar"), "foobar"))
  stopifnot(identical(strip.white("foobar   "), "foobar"))
  stopifnot(identical(strip.white("foobar"), "foobar"))
  stopifnot(identical(strip.white(c(" foobar1", "foobar2 ")),
                      c("foobar1", "foobar2")))
#   cat("strip.white() passes unit tests\n")
})

obsolete.marker <- "OBSOLETE - DO NOT USE: "

isObsolete <- function(indicator.name) {
  grepl(paste("^", obsolete.marker, sep = ""), indicator.name)
}
# unit tests for 'isObsolete':
local({
  stopifnot(isTRUE(isObsolete("OBSOLETE - DO NOT USE: foobar")))
  stopifnot(!isTRUE(isObsolete("foobar")))
#   cat("isObsolete() passes unit tests\n")
})

toggleObsolete <- function(indicator.name, indicator.id) {
  # Mark an indicator in ActivityInfo as obsolete or not. Returns (invisibly)
  # the new name if the renaming in ActivityInfo is succesful, otherwise it
  # returns the old name.
    
  new.name <- if (isObsolete(indicator.name)) {
    # remove obsolete mark
    sub(obsolete.marker, "", indicator.name)
  } else {
    # mark as obsolete
    paste(obsolete.marker, indicator.name, sep = "")
  }
  
  success <- TRUE
  
  tryCatch(executeCommand("UpdateEntity",
                 entityName = "Indicator",
                 id = indicator.id,
                 changes = list(name = new.name)),
           error = function(e) {
             warning("could not rename indicator with name '",
                     indicator.name, "'' (id: ", indicator.id, ")")
             success <<- FALSE
           })
  
  invisible(if (success) {
    new.name
  } else {
    # output the old name
    indicator.name
  })
}

optionalValue <- function(x) {
  if (is.null(x)) NA else x
}
# unit tests for 'optionalValue':
local({
  stopifnot(is.na(optionalValue(NULL)))
  stopifnot(identical(optionalValue("foobar"), "foobar"))
#   cat("optionalValue() passes unit tests\n")
})

loadPartners <- function(input) {
  # a function to load the partners from a CSV file
  
  stopifnot("partners" %in% names(input))
  
  if (grepl("^http|.json$", input$partners)) {
    # load JSON from a URL or from a local JSON file
    
    json <- rjson::fromJSON(file = input$partners)
    
    tmp <- do.call(rbind, lapply(json, function(p) {
      data.frame(Short.Name = p$PARTNER_SHORTNAME,
                 Full.Name = p$PARTNER_NAME,
                 stringsAsFactors = FALSE)
    }))
    
    if (anyDuplicated(tmp)) {
      stop("partner input contains duplicate combinations of partner name and short name")
    }
  } else {
    # load from a local CSV file
    
    tmp <- read.table(file = input$partners,
                      header = TRUE,
                      sep = ";",
                      stringsAsFactors = FALSE
    )
  }
  
  if (any(!c("Short.Name", "Full.Name") %in% names(tmp))) {
    stop("partner data is missing at least one of columns 'Short.Name' and 'Full.Name'")
  }
  
  data.frame(name = tmp$Short.Name,
             full.name = tmp$Full.Name,
             id = NA_integer_,
             stringsAsFactors = FALSE)
}

checkIndicatorData <- function(x) {
  # A function to check that the input provided to the script is as expected.
  
  msg.prefix <- "The input data "
  
  if (!is.data.frame(x)) stop(msg.prefix, "must be a data frame")
  if (nrow(x) < 1L)  stop(msg.prefix, "doesn't contain any rows")
  
  required.cols <-
    list(form = list(mode= "character", max.width = 255),           # form name
         form.category = list(mode = "character", max.width = 255), # form category
         name = list(mode = "character", max.width = Inf),          # indicator name
         category = list(mode = "character", max.width = 50),       # indicator category
         target = list(mode = "numeric"),                           # indicator target value
         id = list(mode = "character", max.width = Inf),            # indicator FOCUS id
         code = list(mode = "character", max.width = Inf),          # name used in formula of calculated indicator
         formula = list(mode = "character", max.width = Inf))       # formula for a calculated indicator
  
  results <- mapply(function(name, properties) {
    if (is.null(x[[name]])) return("doesn't exist")
    if (mode(x[[name]]) != properties$mode) return("wrong type")
    if (is.character(x[[name]]) && nchar(x[[name]]) > properties$max.width) return("too wide")
    "ok"
    }, names(required.cols), required.cols)
  
  ii <- which(results != "ok")
  if (length(ii)) {
    stop(msg.prefix, " has issues with the following columns: ",
         paste(names(results)[ii], " (", results[ii], ")", sep = "", collapse = ", "))
  }
  
  # Give a warning if there is a form without a budget indicator and at least
  # one performance indicator. For this purpose we create a (named) 'status'
  # vector:
  # complete            = form has at least one performance and one budget indicator
  # missing budget      = form is missing a budget indicator
  # missing performance = form is missing a performance indicator
  # missing both        = form is missing a budget and performance indicator
  status <- sapply(split(x, list(x$form.category, x$form), drop = TRUE), function(form) {
    status <- 0
    if (!"Budget/Expenditure" %in% form$category) status <- status + 1
    if (!"Output Indicator" %in% form$category) status <- status + 10
    # ignore forms with only impact indicators:
    if ("Impact Indicator" %in% form$category) status <- 0
    switch(as.character(status),
           "0"="complete",
           "1"="missing budget",
           "10"="missing performance",
           "11"="missing both")
  })
  if (!all(status == "complete")) {
    for (case in setdiff(status, "complete")) {
      cat("WARNING: the following forms are ", case, " indicators:\n",
          paste(names(status[status == case]), collapse = "\n"), sep = "")
    }
  }
  
  # OPTIONAL: check if there are no duplicate indicators in a single form/form.category combo
  
  # all is well
  x
}



convertFormula <- function(x, sequence.id) {
  # A function to make the codes of impact indicators unique within a single form.
  
  # Argument 'x' must be a data.frame with at least the columns "Formula",
  # "Code" and "Sequence.ID":
  if (!is.data.frame(x) || !c("Formula", "Code") %in% names(x)) {
    stop("'x' must be a data frame with columns 'Formula' and 'Code'")
  }
  
  if (any(duplicated(x$Code))) {
    stop("the 'Code' column should not contain duplicate elements")
  }
  codes <- x$Code
  
  if (length(unique(x$Formula)) > 1L) {
    stop("the 'Formula' column should contain a single unique value")
  }
  formula <- x$Formula[1]
  
  for (i in seq_along(codes)) {
    new.code <- sprintf("%s%03dv", codes[i], sequence.id)
    formula <- gsub(codes[i], new.code, formula)
    codes[i] <- new.code
  }
  
  x$Formula <- rep(formula, times = nrow(x))
  x$Code <- codes
  x
}
# Unit tests for convertFormula(x):
local({
  x <- data.frame(ID = seq(3),
                  Formula = "v0v/(v3v*v5v)",
                  Code = c("v0v", "v3v", "v5v"),
                  stringsAsFactors = FALSE)
  y <- convertFormula(x, 12)
  stopifnot(y$Formula[1] == "v0v012v/(v3v012v*v5v012v)", identical(y$Code, c("v0v012v", "v3v012v", "v5v012v")))
})


loadIndicators <- function(input) {
  # A function to load the indicators
  
  # -----------------------------------------------------------------------
  # load performance/output indicators
  # -----------------------------------------------------------------------
  
  if ("output.indicators" %in% names(input)) {   
    if (grepl("^http|.json$", input$output.indicators)) {
      # load output indicators as JSON from a URL or a local file
      
      json <- fromJSON(file = input$output.indicators)
      
      tmp <- do.call(rbind, lapply(json, function(x) {
        data.frame(Objective    = x$OBJECTIVE,
                   Output       = x$OUTPUT,
                   PPG          = x$PPG_NAME,
                   Goal         = x$GOAL,
                   Indicator    = x$INDICATOR,
                   Target       = ifelse(is.null(x$IMP_TARGET), NA_real_, x$IMP_TARGET),
                   MSRP.PPG.ID  = x$PPG_CODE,
                   MSRP.Goal.ID = x$GOAL_MSRP_ID,
                   ID           = x$INDICATOR_ID,
                   stringsAsFactors = FALSE)
      }))
    } else {
      # load output indicators from a local CSV file
      
      tmp <- read.table(file = input$output.indicators,
                        header = TRUE,
                        sep = ";",
                        stringsAsFactors = FALSE)
    }
    
    output <- data.frame(
      # Form = Objective - Output
      form          = paste(strip.white(tmp$Objective),
                            strip.white(tmp$Output), sep = " - "),
      # Form Category = PPG - Goal
      form.category = paste(strip.white(tmp$PPG),
                            strip.white(tmp$Goal), sep = " - "),
      # name = Indicator
      name          = strip.white(tmp$Indicator),
      # category = "Output Indicator"
      category      = "Output Indicator",
      target        = requireNumeric(tmp$Target),
      # unique identifier for each indicator
      id            = paste("FOCUS",
                            strip.white(tmp[["MSRP.PPG.ID"]]),
                            strip.white(tmp[["MSRP.Goal.ID"]]),
                            strip.white(tmp[["ID"]]), sep = "-"),
      code = NA_character_,
      formula = NA_character_,
      stringsAsFactors = FALSE
    )
    
    if (any(is.na(output[setdiff(names(output), c("target", "code", "formula"))]))) {
      stop("one of the required columns, other than the 'target', 'code' and 'formula' columns, in ",
           file, " has a missing value")
    }
  } else {
    # no input location given for output indicators
    output <- NULL
  }
  
  # -----------------------------------------------------------------------
  # load budget indicators
  # -----------------------------------------------------------------------
  
  if ("budget.indicators" %in% names(input)) {
    
    if (grepl("^http|.json$", input$budget.indicators)) {
      # load budget indicators as JSON from a URL or a local file
      
      json <- fromJSON(file = input$budget.indicators)
      
      tmp <- do.call(rbind, lapply(json, function(x) {
        data.frame(Objective    = x$OBJECTIVE,
                   Output       = x$OUTPUT,
                   PPG          = x$PPG_NAME,
                   Goal         = x$GOAL,
                   Indicator    = x$OUTPUT,
                   Target       = ifelse(is.null(x$OL_BUDGET_OPS), NA_real_, x$OL_BUDGET_OPS),
                   MSRP.PPG.ID  = x$PPG_CODE,
                   MSRP.Goal.ID = x$GOAL_MSRP_ID,
                   ID           = x$OUTPUT_MSRP_ID,
                   stringsAsFactors = FALSE)
      }))
    } else {
      # load budget indicators from a local CSV file
      
      tmp <- read.table(file = input$budget.indicators,
                        header = TRUE,
                        sep = ";",
                        stringsAsFactors = FALSE)
    } 
    
    budget <- data.frame(
      form          = paste(strip.white(tmp$Objective), strip.white(tmp$Output), sep = " - "),
      form.category = paste(strip.white(tmp$PPG), strip.white(tmp$Goal), sep = " - "),
      name          = paste(strip.white(tmp$Output), "$"),
      category      = "Budget/Expenditure",
      target        = requireNumeric(tmp$Target),
      # unique identifier for each indicator
      id            = paste("FOCUS",
                            strip.white(tmp[["MSRP.PPG.ID"]]),
                            strip.white(tmp[["MSRP.Goal.ID"]]),
                            strip.white(tmp[["ID"]]), sep = "-"),
      code = NA_character_,
      formula = NA_character_,
      stringsAsFactors = FALSE
    )
    
    if (any(is.na(budget[setdiff(names(budget), c("target", "code", "formula"))]))) {
      stop("one of the required columns, other than the 'target', 'code' and 'formula' columns, in ",
           file, " has a missing value")
    }
  } else {
    # no input location given for budget indicators
    budget <- NULL
  }
  
  # -----------------------------------------------------------------------
  # load impact indicators
  # -----------------------------------------------------------------------
  
  if (all(c("impact.indicators", "attributes") %in% names(input))) {
    
    if (!all(grepl("^http|.json$", unlist(input[c("impact.indicators", "attributes")])))) {
      stop("Sorry, I can't load impact indicators from anything other than JSON.")
    }
    
    # Load impact indicators specific to the current operation:
    impact.indicators <-
      do.call(rbind, lapply(fromJSON(file = input$impact.indicators), function(x) {
        data.frame(Indicator    = x$INDICATOR,
                   ID           = x$INDICATOR_ID,
                   Output       = NA_character_,
                   Target       = ifelse(is.null(x$IMP_TARGET), NA_real_, x$IMP_TARGET),
                   Objective    = x$OBJECTIVE,
                   Goal         = x$GOAL,
                   PPG          = x$PPG_NAME,
                   MSRP.PPG.ID  = x$PPG_CODE,
                   MSRP.Goal.ID = x$GOAL_MSRP_ID,
                   stringsAsFactors = FALSE)  
      }))
    
    # Give each unique combination of Objective and Indicator a unique sequence
    # number and merge this number into the table of impact indicators:
    unique.cols <- c("ID", "Objective", "PPG", "Goal")
    unique.indicators <- unique(impact.indicators[unique.cols])
    unique.indicators$Sequence.ID <- seq(nrow(unique.indicators))
    impact.indicators <- merge(impact.indicators, unique.indicators, by = unique.cols)
    
    # Load the reference file with all impact indicators, many of which are
    # calculated from individual "attributes":
    attributes <-
      do.call(rbind, lapply(fromJSON(file = input$attributes), function(x) {
        data.frame(ID           = x$INDICATOR_ID,
                   Attribute    = x$ATTRIBUTE,
                   Attribute.ID = x$ATTRIBUTE_ID,
                   Formula      = x$FORMULA,
                   Code         = x$VARIABLE_NAME,
                   stringsAsFactors = FALSE
        )
      }))
    
    # Merge the impact indicators with the reference list:
    impact.indicators <- merge(impact.indicators, attributes, by = "ID")
    
    # Perform some tests to ensure the sanity of the input data:
    # 1) each indicator must have a formula and it must be the same in all 
    #    rows for that indicator
    # 2) the codes of the attributes for an indicator must be unique
    # 3) the code of each attribute must be in the formula of the indicator
    # 4) if there is only a single attribute for an indicator then the code 
    #    of the attribute must match the formula of the indicator
    impact.list <- split(impact.indicators,
                  with(impact.indicators, list(ID, Objective, PPG, Goal)),
                  drop = TRUE)
    for (indicator in impact.list) {
      if (nrow(indicator) == 1L) {
        if (indicator$Code[1] != indicator$Formula[1]) {
          stop("variable name of attribute ", indicator$Attribute.ID[1],
               " doesn't match the formula of the indicator ", indicator$ID[1])
        }
      } else {
        if (length(unique(indicator$Formula)) != 1L) {
          stop("indicator ", indicator$ID[1],
               " has multiple formulas whereas it should have only one")
        }
        if (any(duplicated(indicator$Code))) {
          stop("not all attributes for indicator ", indicator$ID[1], " have a unique code")
        }
        for (code in indicator$Code) {
          if (!grepl(code, indicator$Formula[1])) {
            stop("variable name ", code, " is not in the formula ", indicator$Formula[1],
                 " of indicator ", indicator$ID[1])
          }
        }
      }
    }
    
    # Once the impact indicators pass the test, we expand them into a table
    # which includes the attributes as separate indicators:
    impact <- do.call(rbind, mapply(function(x, i) {
      n <- nrow(x)
      x <- convertFormula(x, i)
      indicators <- if (n == 1L) {
        # only add the identifier of the indicator
        data.frame(id      = paste("FOCUS",
                                   strip.white(x[["MSRP.PPG.ID"]]),
                                   strip.white(x[["MSRP.Goal.ID"]]),
                                   strip.white(x[["ID"]]), sep = "-"),
                   name    = strip.white(x$Indicator),
                   target  = requireNumeric(x$Target),
                   code    = NA_character_,
                   formula = NA_character_,
                   stringsAsFactors = FALSE)
      } else {
        # bind together the indicator and the attributes:
        rbind(data.frame(id = paste("FOCUS",
                                    strip.white(x[["MSRP.PPG.ID"]][1]),
                                    strip.white(x[["MSRP.Goal.ID"]][1]),
                                    strip.white(x[["ID"]][1]), sep = "-"),
                         name    = strip.white(x$Indicator[1]),
                         target  = requireNumeric(x$Target[1]),
                         code    = NA_character_,
                         formula = x$Formula[1],
                         stringsAsFactors = FALSE),
              data.frame(id      = paste("FOCUS",
                                         strip.white(x[["MSRP.PPG.ID"]]),
                                         strip.white(x[["MSRP.Goal.ID"]]),
                                         strip.white(x[["Attribute.ID"]]), sep = "-"),
                         name    = strip.white(x$Attribute),
                         target  = NA_real_,
                         code    = x$Code,
                         formula = NA_character_,
                         stringsAsFactors = FALSE))
      }
      # Add general fields:
      indicators$form <- rep(strip.white(x$Objective[1]), times = nrow(indicators))
      indicators$form.category <- rep(paste(strip.white(x$PPG[1]),
                                            strip.white(x$Goal[1]), sep = " - "), times = nrow(indicators))
      indicators$category <- rep("Impact Indicator", times = nrow(indicators))
      
      indicators
    }, impact.list, seq_along(impact.list), SIMPLIFY = FALSE))
    
  } else {
    # no input location given for output indicators
    impact <- NULL
  }
  
  rbind(output, budget, impact)
}

is.FOCUS <- function(s) {
  # Is an indicator a FOCUS indicator?
  grepl("^FOCUS-", s)
}

getIndicatorsWithValues <- local({
  # For the given activity (i.e. form) this function returns a vector with
  # indicator ids for those indicators that have data attached to them.
  
  # To avoid repeated calls to the server, we cache the results:
  indicators <- list()
  
  function(activityId) {
    id <- as.character(activityId)
    
    if (!id %in% names(indicators)) {
      
      cat("downloading data for activity with id ", activityId, "...\n", sep = "")
      sites <- getSites(activityId)
      
      # obtain a vector with unique indicator identifiers:
      ids <- unique(do.call(c, lapply(sites, function(site) {
        as.integer(names(site$indicatorValues))
      })))
      
      # if no indicator ids are present, store an empty vector:
      indicators[[id]] <<- if (is.null(ids)) integer(0) else ids
    }
    
    indicators[[id]]
  }
})

getLocationTypesDataFrame <- function(country) {
  # a wrapper around getLocationTypes() from the "activityinfo" package
  
  do.call(rbind,
          lapply(getLocationTypes(country.code),
                 function(x) {
                   data.frame(id = x$id, name = x$name, stringsAsFactors = FALSE)
                 }
          )
  )
}

extractIndicatorTable <- function(schema) {
  # takes a schema (list) from an ActivityInfo database and extracts all the 
  # forms/activities as a data frame. An empty database will also return a data
  # frame, albeit one with zero rows.
  if (length(schema$activities)) {
    do.call(rbind, lapply(schema$activities, function(form) {
      do.call(rbind, lapply(form$indicators, function(indicator) {
        data.frame(indicatorId = indicator$id,
                   id = optionalValue(indicator$code),
                   indicator = indicator$name,
                   form = form$name,
                   form.category = optionalValue(form$category),
                   activityId = form$id,
                   stringsAsFactors = FALSE)
      }))
    }))
  } else {
    # empty database with no indicators
    data.frame(indicatorId = integer(0),
               id = character(0),
               indicator = character(0),
               form = character(0),
               form.category = character(0),
               activityId = integer(0))
  }
}

activityFactory <- function(schema, ...) {
  # '...' should contain locationType and reportingFrequency arguments to be
  # passed to createActivity.
  
  # cache the forms that already exist in the schema:
  forms <- if (length(schema$activities)) {
    do.call(rbind, lapply(schema$activities, function(form) {
      data.frame(id = form$id,
                 name = form$name,
                 category = optionalValue(form$category),
                 stringsAsFactors = FALSE)
    }))
  } else {
    data.frame(id = integer(0),
               name = character(0),
               category = character(0),
               stringsAsFactors = FALSE)
  }
  
  database.id <- schema$id
  
  function(name, category = NULL, create.new = TRUE) {
    # A function to retrieve an activity identifier from a database schema. A
    # new activity is created in ActivityInfo if a form with the given name and
    # (optionally) category doesn't exist yet. In this case, the identifier of
    # the newly created form is returned. This function keeps track of both
    # existing and newly created forms. Returns NA if a new form cannot be
    # created.
    # 
    # name: form name
    # category: form category (optional)
    
    args <- list(...)
    if (!"locationType" %in% names(args)) {
      stop("required argument 'locationType' is missing")
    }
    
    idx <- if (is.null(category)) {
      which(forms$name == name & is.na(forms$category))
    } else {
      which(forms$name == name & forms$category == category)
    }
                                              
    if (length(idx)) {
      id <- forms$id[idx[1]]
    } else if (isTRUE(create.new)) {
      success <- TRUE
      id <- tryCatch(createActivity(database.id, name, category, ...),
                     error = function(e) {
                       cat("    WARNING: failed to create form with name '", name,
                               "' in category '", category, "'")
                       success <<- FALSE
                       NA
                     })
      # cache the newly created form:
      if (isTRUE(success)) {
        forms <<- rbind(forms, data.frame(id = id,
                                          name = name,
                                          category = category,
                                          stringsAsFactors = FALSE))
      }
    } else {
      # form doesn't exist and should not be created
      NA
    }
    # return identifier of existing or new form or NA if a new form could not be
    # created:
    id
  }
}

lookupActivity <- function(schema) {
  # Takes a database schema from ActivityInfo and returns a function that can be
  # used to look up the activity indentifier in the database by comparing the
  # name of the form/activity and the category name. If no form/category can be
  # found in the schema, NA is returned. If there are multiple matches, the
  # identifier of the first matching form/activity is returned.
  #
  # Example:
  # schema <- getDatabaseSchema(1234)
  # getActivityId <- formLookup(schema)
  # id <- getActivityId(name = "Form name", category = "Form category name")
  indicator.table <- extractIndicatorTable(schema)
  
  function(name, category) {
    # find a form/activity named 'name' with category 'category' in the database:
    idx <- which(indicator.table$form == name & indicator.table$form.category == category)
    if (length(idx)) indicator.table$activityId[idx[1]] else NA
  }
}

lookupIndicator <- function(schema) {
  # Same function as lookupActivity() but for indicators. However, it also adds
  # the identifier of the form as an attribute called 'activity.id'.
  indicator.table <- extractIndicatorTable(schema)
  
  function(code) {
    # find an indicator with 'code':
    idx <- which(indicator.table$id == code)
    if (length(idx)) {
      structure(indicator.table$indicatorId[idx[1]],
                activity.id = indicator.table$activityId[idx[1]])
    } else {
      NA
    }
  }
}

lookupIndicatorName <- function(schema) {
  
  indicator.table <- extractIndicatorTable(schema)
  
  function(indicatorId) {
    idx <- which(indicator.table$indicatorId == indicatorId)
    if (length(idx)) indicator.table$indicator[idx[1]] else NA
  }
}
# unit test for lookupIndicator' and 'lookupIndicatorName':
local({
  # create a dummy schema
  schema <- list(activities = list(
    list(indicators = list(list(id = 11,
                                code = "foobar",
                                name = "Indicator A.1"),
                           list(id = 12,
                                code = "barfoo",
                                name = "Indicator A.2")),
         name = "Form A",
         category = "Category Y",
         id = 1),
    list(indicators = list(list(id = 13,
                                code = "fobaro",
                                name = "Indicator B.1")),
         name = "Form B",
         category = "Category X",
         id = 2)
  )
  )
  getIndicator <- lookupIndicator(schema)
  stopifnot(identical(getIndicator("fobaro"), structure(13, activity.id = 2)))
  stopifnot(is.na(getIndicator(999)))
#   cat("lookupIndicator() passes unit tests\n")
  
  getIndicatorName <- lookupIndicatorName(schema)
  stopifnot(identical(getIndicatorName(12), "Indicator A.2"))
  stopifnot(is.na(getIndicatorName(999)))
#   cat("lookupIndicatorName() passes unit tests\n")
})

lookupDatabase <- function(databases) {
  db.table <- if (length(databases)) {
    do.call(rbind, lapply(databases, function(db) {
      data.frame(id = db$id,
                 name = db$name,
                 stringsAsFactors = FALSE)
    }))
  } else {
    data.frame(id = integer(0),
               name = character(0))
  }
  
  function(name) {
    # Look up the indentifier of a database named 'name'.
    idx <- which(db.table$name == name)
    if (length(idx)) db.table$id[idx[1]] else NA
  }
}

lookupTarget <- function(targets) {
  # Look up the identifier of a target.
  target.table <- if (length(targets)) {
    do.call(rbind, lapply(targets, function(target) {
      data.frame(id = target$id,
                 name = target$name,
                 fromDate = ifelse(is.null(target$fromDate), NA, target$fromDate),
                 toDate = ifelse(is.null(target$toDate), NA, target$toDate),
                 stringsAsFactors = FALSE)
    }))
  } else {
    data.frame(id = integer(0),
               name = character(0),
               fromDate = character(0),
               toDate = character(0))
  }
  
  function(name, fromDate = NULL, toDate = NULL) {
    # Look up the identifier of a target set with name 'name' and, optionally,
    # the 'fromDate' and the 'toDate'.
    #
    # If 'fromDate' and/or 'toDate' are NULL, then we don't include these in the
    # search (e.g. if both are NULL, we don't check if we find a target named 
    # 'name' with the dates not set, but we just look for a target named 'name')

    idx <- which(target.table$name == name &
                   ifelse(is.null(fromDate),
                          TRUE,
                          target.table$fromDate == requireValidDate(fromDate)) &
                   ifelse(is.null(toDate),
                          TRUE,
                          target.table$toDate == requireValidDate(toDate)))
    if (length(idx)) target.table$id[idx[1]] else NA
  }
}

lookupTargetValue <- function(targets, targetId) {
  # takes a list of targets and creates a function that can be used to look up
  # the values of an indicator within the target that has identifier 'targetId'.
  target.values <- targets[match(targetId,
                                 sapply(targets, function(target) target$id),
                                 nomatch = 0)]
  values <- if (length(target.values)) {
    do.call(rbind, lapply(target.values[[1]]$targetValues, function(target) {
      data.frame(indicatorId = target$indicatorId,
                 value = target$value,
                 stringsAsFactors = FALSE)
    }))
  } else {
    data.frame(indicatorId = integer(0),
               value = numeric(0))
  }
  
  function(indicatorId) {
    # Return the value of the target for indicator with identifier 'indicatorId'.
    idx <- which(values$indicatorId == indicatorId)
    if (length(idx)) values$value[idx[1]] else NA
  }
}

lookupPartner <- function(schema) {
  partner.table <- if (length(schema$partners)) {
    do.call(rbind, lapply(schema$partners, function(partner) {
      data.frame(id = partner$id,
                 name = partner$name,
                 stringsAsFactors = FALSE)
    }))
  } else {
    data.frame(id = integer(0),
               name = character(0))
  }
  
  function(name) {
    # Look up the indentifier of a partner named 'name'.
    idx <- which(partner.table$name == name)
    if (length(idx)) partner.table$id[idx[1]] else NA
  }
}

guessAggregation <- function(indicator.name) {
  # return "sum" if the indicator name starts with "#" or ends with "$",
  # "yes/no" if it ends with "(yes/no)",
  # else return "mean"
  
  # NOTE: the API doesn't seem to accept the string versions of the aggregation
  # function, therefore the corresponding numeric id is returned for now.
  
  if (length(indicator.name) != 1L) stop("argument must be of length 1")
  
  if (grepl("^(Total )?#|\\$$|\\(value in USD\\)$", indicator.name)) {
#     "sum"
    0L
  } else if (grepl("\\(yes/no\\)$", indicator.name)) {
#     "sum"
#    2L
    0L
  } else {
#     "mean" which is also called average in the UI
    1L
  }
}
local({
  stopifnot(identical(guessAggregation("# of bikes"), 0L))
  stopifnot(identical(guessAggregation("Total # of bikes"), 0L))
  stopifnot(identical(guessAggregation("% of males"), 1L))
  stopifnot(identical(guessAggregation("effective (yes/no)"), 0L))
  stopifnot(identical(guessAggregation("Total of GNA financial needs (value in USD)"), 0L))
})

guessUnits <- function(indicator.name) {
  # guess the units from the indicator name
  
  if (length(indicator.name) != 1L) stop("argument must be of length 1")
  
  verbs <- paste(c("completed", "conducted", "constructed", "enrolled", "ensured",
                   "equipped", "established", "held", "implemented", "made",
                   "maintained", "monitored", "organized", "published", "provided",
                   "reached", "received", "receiving", "recorded", "rehabilitated",
                   "strengthened", "submitted", "taken", "trained"), collapse = "|")
  
  if (grepl("\\$$|\\(value in USD\\)$", indicator.name)) {
    "$"
  } else if (grepl("^%|^Extent", indicator.name)) {
    "%"
  } else if (grepl("\\(yes/no\\)$", indicator.name)) {
    "yes/no"
  } else if (grepl("^Average # of days", indicator.name)) {
    "days"
  } else if (grepl("^Average # of litres", indicator.name)) {
    "litres"
  } else if (grepl("^(Total )?#( of)? ", indicator.name)) {
    # indicator name startes with "Total # ", "Total # of ", "# " or "# of "
    pattern <- if (grepl(paste("(", verbs, ")", sep = ""), indicator.name)) {
      paste("^(Total )?#( of)? (.*?)\\s*(", verbs , ").*$", sep = "")
    } else {
      paste("^(Total )?#( of)? (.*)$", sep = "")
    }
    sub(pattern, "\\3", indicator.name, perl = TRUE)
  } else if (grepl("^#\\s+of reported ", indicator.name)) {
    # indicator name starts with "# of reported " (and allows for extra spaces between "#" and "of")
    pattern <- "#\\s+of reported (.*)$"
    sub(pattern, "\\1", indicator.name, perl = TRUE)
  } else {
    # failed to guess unit
    "unknown" 
  }
}
# unit tests for 'guessUnits':
local({
  stopifnot(identical(guessUnits("# of women receiving bla bla"), "women"))
  stopifnot(identical(guessUnits("bla bla (yes/no)"), "yes/no"))
  stopifnot(identical(guessUnits("Total # of bikes"), "bikes"))
  stopifnot(identical(guessUnits("Total of GNA financial needs (value in USD)"), "$"))
  stopifnot(identical(guessUnits("Extent persons of concern can obtain identity documentation"), "%"))
#   cat("guessUnits() passes unit tests\n")
})

getLocationTypeFromUser <- function(location.types) {
  # Print options to the screen:
  cat(paste(row.names(location.types), location.types$name, sep = ": ", collapse = "\n"))
  
  prompt <- sprintf(
    "Please type the number of the location type from the list above and press Enter (1 - %d): ",
    nrow(location.types)
  )
  
  choice <- readline(prompt)
  if (!choice %in% row.names(location.types)) {
    stop(choice, " is not a valid option, please re-run this script to try again")
  }
  
  # Return the name of the location type when a valid option is given:
  location.types$name[as.integer(choice)]
}

executeCommandWithRetry <- function(command, ...) {
  # Execute a ActivityInfo RPC command and retry a few times if there is an error:
  n <- 3
  retry <- n
  
  while (retry) {
    retry <- retry - 1
    success <- TRUE
    tryCatch(
      result <- executeCommand(command, ...),
      error = function(e) {
        success <<- FALSE
        if (retry) {
          cat("failed to execute the ", command,
              " command, retry ", n - retry, " of ", n - 1, " in 5 seconds...\n", sep = "")
          Sys.sleep(5)
        } else {
          stop("an error occurred while executing the ", command,
               " command with the following message: ", conditionMessage(e), call. = FALSE)
        }
      }
    )
    if (success) break
  } # end of retry-while statement on UpdateTargetValue
  
  invisible(result)
}
