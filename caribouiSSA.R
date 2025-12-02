defineModule(sim, list(
  name = "caribouiSSA",
  description = "",
  keywords = "",
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(caribouiSSA = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribouiSSA.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2", "reproducible", "data.table", "glmmTMB", "broom.mixed", "performance"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(name = "iSSAformula", class = "character",
                    default = "case_ ~ -1 +
                              I(log(sl_ + 1)) +
                              I(cos(ta_)) +
                              I(log(sl_ + 1)) : I(cos(ta_)) +
                              prop_needleleaf_start : I(log(sl_ + 1)) +
                              prop_mixedforest_start : I(log(sl_ + 1)) +
                              prop_veg_start : I(log(sl_ + 1)) +
                              prop_wets_start : I(log(sl_ + 1)) +
                              I(log(timeSinceFire_start + 1)) : I(log(sl_ + 1)) +
                              I(log(timeSinceHarvest_start + 1)) : I(log(sl_ + 1)) +
                              I(log(distpaved_start + 1)) : I(log(sl_ + 1)) +
                              I(log(distunpaved_start + 1)) : I(log(sl_ + 1)) +
                              I(log(distpolys_start + 1)) : I(log(sl_ + 1)) +
                              prop_needleleaf_end +
                              prop_mixedforest_end +
                              prop_veg_end +
                              prop_wets_end +
                              I(log(timeSinceFire_end + 1)) +
                              I(log(timeSinceHarvest_end + 1)) +
                              I(log(distpaved_end + 1)) +
                              I(log(distunpaved_end + 1)) +
                              I(log(distpolys_end + 1)) +
                              (1 | indiv_step_id) +
                              (0 + I(log(sl_ + 1)) | id) +
                              (0 + I(cos(ta_)) | id) +
                              (0 + I(log(sl_ + 1)) : I(cos(ta_)) | id) +
                              (0 + prop_needleleaf_start : I(log(sl_ + 1)) | id) +
                              (0 + prop_mixedforest_start : I(log(sl_ + 1)) | id) +
                              (0 + prop_veg_start : I(log(sl_ + 1)) | id) +
                              (0 + prop_wets_start : I(log(sl_ + 1)) | id) +
                              (0 + prop_needleleaf_end | id) +
                              (0 + prop_mixedforest_end | id) +
                              (0 + prop_veg_end | id) +
                              (0 + prop_wets_end | id) +
                              (0 + I(log(timeSinceFire_end + 1)) | id) +
                              (0 + I(log(timeSinceFire_start + 1)) : I(log(sl_ + 1)) | id) +
                              (0 + I(log(timeSinceHarvest_end + 1)) | id) +
                              (0 + I(log(timeSinceHarvest_start + 1)) : I(log(sl_ + 1)) | id) +
                              (0 + I(log(distpaved_end + 1)) | id) +
                              (0 + I(log(distunpaved_end + 1)) | id) +
                              (0 + I(log(distpolys_end + 1)) | id) +
                              (0 + I(log(distpaved_start + 1)) : I(log(sl_ + 1)) | id) +
                              (0 + I(log(distunpaved_start + 1)) : I(log(sl_ + 1)) | id) +
                              (0 + I(log(distpolys_start + 1)) : I(log(sl_ + 1)) | id) +
                              (1 | year)",
                    desc = 'The iSSA formula used for each jurisdiction'),
    defineParameter("jurisdiction", "character",c("BC", "SK", "MB", "YT", "NT", "ON"),
                    desc = "A list of jurisdictions to run"),
    defineParameter("modelSelection", "character", "iSSA",
                    desc = "model selection for analysis")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "extractVar", objectClass = "data.table",
                 desc = "Variables of landscape values and distance calculations matched by year to points")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "iSSAmodels", objectClass = "list",
                  desc = "A list of iSSA models by jurisdiction"),
    createsOutput(objectName = "iSSAsummaries", objectClass = "list",
                  desc = "A list of iSSA model summaries by jurisdiction")
  )
))

doEvent.caribouiSSA = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  if (Par$modelSelection == "iSSA") {
    #
    jurisList <- iSSAprep(sim)
    sim <- iSSAmodel(sim, jurisList)
  }
  else if (Par$modelSelection != "iSSA") {
    message("Other models are not completed yet, please run an iSSA")
  }

  return(invisible(sim))
  #xgboost function can also be called here
  #maybe a parameter that gets the model that the user wants?
}


.inputObjects <- function(sim) {

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")


  return(invisible(sim))
}

iSSAprep <- function(sim) {

  dat <- sim$extractVar

  juris_list <- list()

  for (j in Par$jurisdiction) {

    message("Preparing jurisdiction: ", j)

    # Subset jurisdiction
    if (j == "MB") {

      # MB special sampling
      mb.2015 <- dat[jurisdiction == "mb" & int.year == 2015]

      mb.sub.id <- sample(
        unique(mb.2015$id),
        floor(length(unique(mb.2015$id)) * 0.50)
      )

      dt <- mb.2015[id %in% mb.sub.id]

    } else {

      # standard jurisdictions
      dt <- dat[jurisdiction == tolower(j)]
    }

    # Skip empty jurisdictions
    if (nrow(dt) == 0) {
      warning("Jurisdiction ", j, " had zero rows. Skipped.")
      next
    }

    dt[, id := as.factor(id)]
    dt[, indiv_step_id := as.factor(indiv_step_id)]
    dt[, year := as.factor(year)]

    # Construct covariates
    dt <- construct_covariates(dt)

    # Save to list
    juris_list[[j]] <- dt
  }

  return(juris_list)
}

# iSSAprep <- function(sim) {
#   dat <- sim$extractVar
#
#   browser()
#   # restrict years only once
#   #probably can change this to only the years that we're running (a param maybe?)
#   dat.sub <- dat[year >= 2013 & year <= 2021]
#
#   # list to hold each cleaned jurisdiction dt
#   juris_list <- list()
#
#   # loop only over jurisdictions provided
#   for (j in Par$jurisdiction) {
#
#     message("Preparing jurisdiction: ", j)
#
#     if (j == "MB") {
#       # MB gets special handling
#       mb.2015 <- dat[jurisdiction == "mb" & int.year == 2015]
#       mb.sub.id <- sample(unique(mb.2015$id),
#                           floor(length(unique(mb.2015$id)) * 0.50))
#       dt <- mb.2015[id %in% mb.sub.id]
#     }
#
#     else if (j == "SK") {
#       dt <- dat[jurisdiction == "sk"]
#     }
#
#     else if (j == "BC") {
#       dt <- dat.sub[jurisdiction == "bc"]
#     }
#
#     else if (j == "ON") {
#       dt <- dat.sub[jurisdiction == "on"]
#     }
#
#     else if (j == "NT") {
#       dt <- dat.sub[jurisdiction == "nt"]
#     }
#
#     else if (j == "YT") {
#       dt <- dat.sub[jurisdiction == "yt"]
#     }
#
#     else {
#       warning("Jurisdiction ", j, " is not recognized. Skipping.")
#       next  # skip to next jurisdiction
#     }
#
#     # clean / prep the dt if it exists
#     if (nrow(dt) > 0) {
#       dt[, id := as.factor(id)]
#       dt[, indiv_step_id := as.factor(indiv_step_id)]
#       juris_list[[j]] <- dt
#     } else {
#       warning("Jurisdiction ", j, " had zero rows. Skipped.")
#     }
#     dt <- construct_covariates(dt)
#   }
#   return(juris_list)
# }

construct_covariates <- function(dt) {
  # Non-forest vegetation (veg)
  dt[, prop_veg_start := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_herbs_start", "prop_shrub_start", "prop_bryoids_start")]

  dt[, prop_veg_end := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_herbs_end", "prop_shrub_end", "prop_bryoids_end")]

  # Wetlands only
  dt[, prop_wets_start := prop_wetland_start]
  dt[, prop_wets_end   := prop_wetland_end]

  # Conifer - needleleaf + wet_treed
  dt[, prop_needleleaf_start := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_needleleaf_start", "prop_wet_treed_start")]

  dt[, prop_needleleaf_end := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_needleleaf_end", "prop_wet_treed_end")]

  # Mixed - mixed + deciduous
  dt[, prop_mixedforest_start := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_mixed_start", "prop_deciduous_start")]

  dt[, prop_mixedforest_end := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_mixed_end", "prop_deciduous_end")]

  return(invisible(dt))
}


# construct_covariates <- function(dt) {
#   ## Non-forest vegetation (veg)
#   dt[, prop_veg_start := rowSums(.SD, na.rm = TRUE),
#      .SDcols = c("prop_herbs_start", "prop_shrub_start", "prop_bryoids_start")]
#
#   dt[, prop_veg_end := rowSums(.SD, na.rm = TRUE),
#      .SDcols = c("prop_herbs_end", "prop_shrub_end", "prop_bryoids_end")]
#
#   ## Wetlands combined (wets)
#   dt[, prop_wets_start := rowSums(.SD, na.rm = TRUE),
#      .SDcols = c("prop_wetland_start", "prop_wet_treed_start")]
#
#   dt[, prop_wets_end := rowSums(.SD, na.rm = TRUE),
#      .SDcols = c("prop_wetland_end", "prop_wet_treed_end")]
#
#
#   ## Minimum distance to *any* road (combined)
#   dt[, distlf_start := pmin(distpaved_start, distunpaved_start, na.rm = TRUE)]
#   dt[, distlf_end   := pmin(distpaved_end,   distunpaved_end,   na.rm = TRUE)]
#
#   return(invisible(dt))
# }

iSSAmodel <- function(sim, jurisList) {
  models <- list()
  summaries <- list()

  for (j in names(jurisList)) {

    dat <- jurisList[[j]]

    message("Starting iSSA for: ", j)
    mod <- glmmTMB(
      formula = as.formula(Par$iSSAformula),
      family  = poisson(),
      data    = dat,
      map     = list(theta = factor(c(NA, 1:21))),
      start   = list(theta = c(log(1000), rep(0, 21)))
    )

    models[[j]] <- mod
    summaries[[j]] <- summary(mod)
    message("Finished iSSA for: ", j)
  }

  #add a iSSA covariate output
  sim$iSSAmodels <- models
  sim$iSSAsummaries <- summaries

  return(sim)
}
