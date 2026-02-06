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
                    desc = "A list of jurisdictions to run iSSA models on"),
    defineParameter("modelSelection", "character", "iSSA",
                    desc = "model selection for analysis"),
    defineParameter("modeScale", "character", "jurisdictional",
                    desc = "define the scale to run the iSSA. Can be 'jurisdictional', 'global', or 'both'.")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = 'extractedVariables', objectClass = "data.table",
                 desc = "variables of landscape values at the animal locations and distance calculations matched by year to the animal locations")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "iSSAmodels", objectClass = "list",
                  desc = "A list of 'glmmTMB' iSSA models by jurisdiction and/or global scales"),
    createsOutput(objectName = "iSSAsummaries", objectClass = "list",
                  desc = "A list of iSSA model summaries by jurisdiction and/or global scales")
  )
))

doEvent.caribouiSSA = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- scheduleEvent(sim, time(sim), "caribouiSSA", "prepareCovariates")
      sim <- scheduleEvent(sim, time(sim), "caribouiSSA", "runiSSAmodel")
    },
    prepareCovariates = {
      # harmonize the covariates with the iSSA formula
      dat <- sim$extractedVariables
      sim$juris_list <- iSSAprep(dat)
    },
    runiSSAmodel = {
      # run jurisdictional and/or global iSSA models
      iSSAoutput <- iSSAmodel(
        juris_list = sim$juris_list,
        scale = Par$modeScale)
      sim$iSSAmodels <- iSSAoutput$models
      sim$iSSAsummaries <- iSSAoutput$summaries
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")


  return(invisible(sim))
}
