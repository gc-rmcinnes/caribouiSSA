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
                     I(log(sl_+1)) +
                     I(cos(ta_)) +
                     I(log(sl_+1)):I(cos(ta_)) +
                     prop_needleleaf_start:I(log(sl_+1)) + 
                     prop_mixforest_start:I(log(sl_+1)) + 
                     prop_veg_start:I(log(sl_+1)) + 
                     prop_wets_start:I(log(sl_+1)) +
                     prop_needleleaf_end +
                     prop_mixforest_end +
                     prop_veg_end +
                     prop_wets_end +
                     I(log(ts_fires_end+1)) + 
                     I(log(sl_+1)):I(log(ts_fires_start+1)) +
                     I(log(ts_harv_end+1)) + 
                     I(log(sl_+1)):I(log(ts_harv_start+1)) +
                     I(log(distlf_end+1)) + 
                     I(log(sl_+1)):I(log(distlf_start+1)) +
                     I(log(distlf_other_end+1)) + 
                     I(log(sl_+1)):I(log(distlf_other_start+1)) +
                     disturbance_end +
                     (1|indiv_step_id) +
                     (0 + I(log(sl_ +1))|id) +
                     (0 + I(cos(ta_))|id) +
                     (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                     (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                     (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
                     (0 + prop_veg_start:I(log(sl_+1))|id) + 
                     (0 + prop_wets_start:I(log(sl_+1))|id) +
                     (0 + prop_needleleaf_end|id) +
                     (0 + prop_mixforest_end|id) +
                     (0 + prop_veg_end|id) +
                     (0 + prop_wets_end|id) +
                     (0 + (I(log(ts_fires_end+1)))|id) +
                     (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
                     (0 + (I(log(ts_harv_end+1)))|id) + 
                     (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
                     (0 + I(log(distlf_end+1))|id) + 
                     (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
                     (0 + I(log(distlf_other_end+1))|id) + 
                     (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
                     (0 + disturbance_end|id) +
                     (1|year)",
                    desc = 'The iSSA formula used for each jurisdiction'),
    defineParameter("jurisdiction", "character",c("BC", "SK", "MB", "YT", "NT", "ON"),
                    desc = "A list of jurisdictions to run"),
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "extractLand", objectClass = "data.table", 
                 desc = "Landscape values and distance calculations matched by year to points")
    #_targets outpputs
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #i need to put the issa output here, i dont think there is any others, maybe forcasted issa but that will likely be a different module
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
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
  #This will be where the iSSA gets created
  jurisList <- iSSAprep(sim)
  sim <- iSSAmodel(sim, jurisList)
  
  return(invisible(sim))
  #xgboost function can also be called here
  #maybe a parameter that gets the model that the user wants?
  

  return(invisible(sim))
}


.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  
  return(invisible(sim))
}

iSSAprep <- function(sim) {
  dat <- sim$extractLand
  
  # subset years
  dat.sub <- dat[year >= 2014 & year <= 2019]
  
  jurisList <- list()
  
  # Manitoba (special case due to abundance of data)
  mb.2015 <- dat[jurisdiction == "mb" & int.year == 2015]
  mb.sub.id <- sample(unique(mb.2015$id), floor(length(unique(mb.2015$id)) * 0.50))
  mb <- mb.2015[id %in% mb.sub.id]
  mb[, id := as.factor(id)]
  mb[, indiv_step_id := as.factor(indiv_step_id)]
  jurisList[["mb"]] <- mb
  
  # list of other jurisdictions to process identically
  otherJuris <- c("sk", "bc", "on", "nt", "yt")
  
  for (j in otherJuris) {
    dt <- dat.sub[jurisdiction == j]
    dt[, id := as.factor(id)]
    dt[, indiv_step_id := as.factor(indiv_step_id)]
    jurisList[[j]] <- dt
  }
  
  return(jurisList)
}

iSSAmodel <- function(sim, jurisList) {
  
  models <- list()
  summaries <- list()
  
  for (j in names(jurisList)) {
    
    dat <- jurisList[[j]]
    
    # MB uses 20 variance parameters (no year RE)
    if (j == "mb") {
      theta.map <- factor(c(NA, 1:20))
      theta.start <- c(log(1000), rep(0, 20))
    } else {
      theta.map <- factor(c(NA, 1:21))
      theta.start <- c(log(1000), rep(0, 21))
    }
    
    mod <- glmmTMB(
      formula = sim$param$iSSAformula,
      family = poisson(),
      data = dat,
      map = list(theta = theta.map),
      start = list(theta = theta.start)
    )
    
    models[[j]] <- mod
    summaries[[j]] <- summary(mod)
  }
  
  sim$iSSAmodels <- models
  sim$iSSAsummaries <- summaries
  
  return(sim)
}