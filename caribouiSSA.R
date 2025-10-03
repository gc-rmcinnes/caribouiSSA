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
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2", "reproducible", "data.table", "glmmTMB", "broom/mixed", "performance"),
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
    defineParameter(name = "iSSAformula", class = "formula", 
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
    expectsInput(objectName = "caribouLoc", objectClass = "data.table", 
                 desc = "Harmonized and cleaned caribou locations of all jurisdictions provided")
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
  #This will be where the iSSA gets forcast
  runiSSA <- iSSA(dat_iSSA)
  #xgboost function can also be called here
  #maybe a parameter that gets the model that the user wants?
  

  return(invisible(sim))
}


.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # i think this should be a prepInputs call for the iSSAprep function
  dat_iSSA <- sim$caribouLoc 
  
  return(invisible(sim))
}

iSSAprep <- function(dat_iSSA){
  set.seed(53)
  #########################################################################################
  
  #dat <- readRDS(file.path(derived, 'dat_iSSA.RDS'))
  #maybe have the subsetting dynamically with params would be a good idea
  dat <- data.frame(sim$caribouLoc)
  
  dat[,range(year), by = .(jurisdiction)]
  
  dat.sub <- dat[year>=2014 & year<=2019]
  dat.sub[,.N, by=.(jurisdiction)]
  #setindex(dat, NULL)
  
  juris <- list()
  ### mb ----
  mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
  length(unique(mb.2015$id))*.5
  # worked when sampled 150 indivs
  mb.sub.id <- sample(unique(mb.2015$id), floor(length(unique(mb.2015$id))*.50))
  mb.2015.sub <- mb.2015[id %in% mb.sub.id]
  mb.2015.sub[,id:=as.factor(id)]
  mb.2015.sub[,indiv_step_id := as.factor(indiv_step_id)]
  juris <- append(mb)
  
  #this is repeated code and should be made into a loop
  ### sk ----
  sk <- dat[jurisdiction == 'sk']
  sk[,id:=as.factor(id)]
  sk[,indiv_step_id := as.factor(indiv_step_id)]
  juris <- append(sk)
  
  ### bc ----
  bc <- dat.sub[jurisdiction == 'bc']
  bc[,id:=as.factor(id)]
  bc[,indiv_step_id := as.factor(indiv_step_id)]
  juris <- append(sk)
  
  ### on ----
  on <- dat.sub[jurisdiction == 'on']
  on[,id:=as.factor(id)]
  on[,indiv_step_id := as.factor(individ_step_id)]
  juris <- append(on)
  
  ### nt ----
  nt <- dat.sub[jurisdiction %in% c('nt')]
  nt[,id:=as.factor(id)]
  nt[,indiv_step_id := as.factor(indiv_step_id)]
  juris <- append(nt)
  
  ### yt ----
  yt <- dat.sub[jurisdiction == 'yt']
  yt[,id:=as.factor(id)]
  yt[,indiv_step_id := as.factor(individual_step_id)]
  juris <- append(yt)
  return(juris)
}
  
  ### MODELS ----
  
  #map loop for running each jurisdiction
  iSSAmodel <- function(){
    #model run with 
    glmm.sum <- list()
    runglmm <- function (jur){
      glmmTMB(Par$iSSAformula,
              family = poisson(), data = juris,
              map= list(theta = factor(c(NA,1:21))),
              start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
      )}
    
    glmm.all <- map(jur = juris, function(jur){
      runglmm(jur)
      glmm.sum <- append(summary(jur))
    })
    sim$glmm.sum
  }
  
  
  #### nwt ----
  # gc()
  # m.nwt <- glmmTMB(case_ ~ -1 +
  #                    I(log(sl_+1)) +
  #                    I(cos(ta_)) +
  #                    I(log(sl_+1)):I(cos(ta_)) +
  #                    prop_needleleaf_start:I(log(sl_+1)) + 
  #                    prop_mixforest_start:I(log(sl_+1)) + 
  #                    prop_veg_start:I(log(sl_+1)) + 
  #                    prop_wets_start:I(log(sl_+1)) +
  #                    prop_needleleaf_end +
  #                    prop_mixforest_end +
  #                    prop_veg_end +
  #                    prop_wets_end +
  #                    I(log(ts_fires_end+1)) + 
  #                    I(log(sl_+1)):I(log(ts_fires_start+1)) +
  #                    I(log(ts_harv_end+1)) + 
  #                    I(log(sl_+1)):I(log(ts_harv_start+1)) +
  #                    I(log(distlf_end+1)) + 
  #                    I(log(sl_+1)):I(log(distlf_start+1)) +
  #                    I(log(distlf_other_end+1)) + 
  #                    I(log(sl_+1)):I(log(distlf_other_start+1)) +
  #                    disturbance_end +
  #                    (1|indiv_step_id) +
  #                    (0 + I(log(sl_ +1))|id) +
  #                    (0 + I(cos(ta_))|id) +
  #                    (0 + I(log(sl_+1)):I(cos(ta_))|id) +
  #                    (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
  #                    (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
  #                    (0 + prop_veg_start:I(log(sl_+1))|id) + 
  #                    (0 + prop_wets_start:I(log(sl_+1))|id) +
  #                    (0 + prop_needleleaf_end|id) +
  #                    (0 + prop_mixforest_end|id) +
  #                    (0 + prop_veg_end|id) +
  #                    (0 + prop_wets_end|id) +
  #                    (0 + (I(log(ts_fires_end+1)))|id) +
  #                    (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
  #                    (0 + (I(log(ts_harv_end+1)))|id) + 
  #                    (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
  #                    (0 + I(log(distlf_end+1))|id) + 
  #                    (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
  #                    (0 + I(log(distlf_other_end+1))|id) + 
  #                    (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
  #                    (0 + disturbance_end|id) +
  #                    (1|year),
  #                  family = poisson(), data = nwt,
  #                  map= list(theta = factor(c(NA,1:21))),
  #                  start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
  # )
  # 
  # 
  # 
  # summary(m.nwt)
  # saveRDS(m.nwt, file.path(derived, 'mod_selmove_nwt.RDS'))
  # 
  # 
  # #### bc ----
  # gc()
  # m.bc <- glmmTMB(case_ ~ -1 +
  #                   I(log(sl_+1)) +
  #                   I(cos(ta_)) +
  #                   I(log(sl_+1)):I(cos(ta_)) +
  #                   prop_needleleaf_start:I(log(sl_+1)) + 
  #                   prop_mixforest_start:I(log(sl_+1)) + 
  #                   prop_veg_start:I(log(sl_+1)) + 
  #                   prop_wets_start:I(log(sl_+1)) +
  #                   prop_needleleaf_end +
  #                   prop_mixforest_end +
  #                   prop_veg_end +
  #                   prop_wets_end +
  #                   I(log(ts_fires_end+1)) + 
  #                   I(log(sl_+1)):I(log(ts_fires_start+1)) +
  #                   I(log(ts_harv_end+1)) + 
  #                   I(log(sl_+1)):I(log(ts_harv_start+1)) +
  #                   I(log(distlf_end+1)) + 
  #                   I(log(sl_+1)):I(log(distlf_start+1)) +
  #                   I(log(distlf_other_end+1)) + 
  #                   I(log(sl_+1)):I(log(distlf_other_start+1)) +
  #                   disturbance_end +
  #                   (1|indiv_step_id) +
  #                   (0 + I(log(sl_ +1))|id) +
  #                   (0 + I(cos(ta_))|id) +
  #                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
  #                   (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
  #                   (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
  #                   (0 + prop_veg_start:I(log(sl_+1))|id) + 
  #                   (0 + prop_wets_start:I(log(sl_+1))|id) +
  #                   (0 + prop_needleleaf_end|id) +
  #                   (0 + prop_mixforest_end|id) +
  #                   (0 + prop_veg_end|id) +
  #                   (0 + prop_wets_end|id) +
  #                   (0 + (I(log(ts_fires_end+1)))|id) +
  #                   (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
  #                   (0 + (I(log(ts_harv_end+1)))|id) + 
  #                   (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
  #                   (0 + I(log(distlf_end+1))|id) + 
  #                   (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
  #                   (0 + I(log(distlf_other_end+1))|id) + 
  #                   (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
  #                   (0 + disturbance_end|id) +
  #                   (1|year),
  #                 family = poisson(), data = bc,
  #                 map= list(theta = factor(c(NA,1:21))),
  #                 start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
  # )
  # 
  # 
  # 
  # summary(m.bc)
  # saveRDS(m.bc, file.path(derived, 'mod_selmove_bc.RDS'))
  # 
  # 
  # #### sk ----
  # gc()
  # m.sk <- glmmTMB(case_ ~ -1 +
  #                   I(log(sl_+1)) +
  #                   I(cos(ta_)) +
  #                   I(log(sl_+1)):I(cos(ta_)) +
  #                   prop_needleleaf_start:I(log(sl_+1)) + 
  #                   prop_mixforest_start:I(log(sl_+1)) + 
  #                   prop_veg_start:I(log(sl_+1)) + 
  #                   prop_wets_start:I(log(sl_+1)) +
  #                   prop_needleleaf_end +
  #                   prop_mixforest_end +
  #                   prop_veg_end +
  #                   prop_wets_end +
  #                   I(log(ts_fires_end+1)) + 
  #                   I(log(sl_+1)):I(log(ts_fires_start+1)) +
  #                   I(log(ts_harv_end+1)) + 
  #                   I(log(sl_+1)):I(log(ts_harv_start+1)) +
  #                   I(log(distlf_end+1)) + 
  #                   I(log(sl_+1)):I(log(distlf_start+1)) +
  #                   I(log(distlf_other_end+1)) + 
  #                   I(log(sl_+1)):I(log(distlf_other_start+1)) +
  #                   disturbance_end +
  #                   (1|indiv_step_id) +
  #                   (0 + I(log(sl_ +1))|id) +
  #                   (0 + I(cos(ta_))|id) +
  #                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
  #                   (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
  #                   (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
  #                   (0 + prop_veg_start:I(log(sl_+1))|id) + 
  #                   (0 + prop_wets_start:I(log(sl_+1))|id) +
  #                   (0 + prop_needleleaf_end|id) +
  #                   (0 + prop_mixforest_end|id) +
  #                   (0 + prop_veg_end|id) +
  #                   (0 + prop_wets_end|id) +
  #                   (0 + (I(log(ts_fires_end+1)))|id) +
  #                   (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
  #                   (0 + (I(log(ts_harv_end+1)))|id) + 
  #                   (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
  #                   (0 + I(log(distlf_end+1))|id) + 
  #                   (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
  #                   (0 + I(log(distlf_other_end+1))|id) + 
  #                   (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
  #                   (0 + disturbance_end|id) +
  #                   (1|year),
  #                 family = poisson(), data = sk,
  #                 map= list(theta = factor(c(NA,1:21))),
  #                 start = list(theta =c(log(1000), seq(0,0, length.out = 21)))
  # )
  # 
  # 
  # 
  # summary(m.sk)
  # saveRDS(m.sk, file.path(derived, 'mod_selmove_sk.RDS'))
  # 
  # #### mb ----
  # gc()
  # p1 <- m.mb.2015.sub <- glmmTMB(case_ ~ -1 +
  #                                  I(log(sl_+1)) +
  #                                  I(cos(ta_)) +
  #                                  I(log(sl_+1)):I(cos(ta_)) +
  #                                  prop_needleleaf_start:I(log(sl_+1)) + 
  #                                  prop_mixforest_start:I(log(sl_+1)) + 
  #                                  prop_veg_start:I(log(sl_+1)) + 
  #                                  prop_wets_start:I(log(sl_+1)) +
  #                                  prop_needleleaf_end +
  #                                  prop_mixforest_end +
  #                                  prop_veg_end +
  #                                  prop_wets_end +
  #                                  I(log(ts_fires_end+1)) + 
  #                                  I(log(sl_+1)):I(log(ts_fires_start+1)) +
  #                                  I(log(ts_harv_end+1)) + 
  #                                  I(log(sl_+1)):I(log(ts_harv_start+1)) +
  #                                  I(log(distlf_end+1)) + 
  #                                  I(log(sl_+1)):I(log(distlf_start+1)) +
  #                                  I(log(distlf_other_end+1)) + 
  #                                  I(log(sl_+1)):I(log(distlf_other_start+1)) +
  #                                  disturbance_end +
  #                                  (1|indiv_step_id) +
  #                                  (0 + I(log(sl_ +1))|id) +
  #                                  (0 + I(cos(ta_))|id) +
  #                                  (0 + I(log(sl_+1)):I(cos(ta_))|id) +
  #                                  (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
  #                                  (0 + prop_mixforest_start:I(log(sl_+1))|id) + 
  #                                  (0 + prop_veg_start:I(log(sl_+1))|id) + 
  #                                  (0 + prop_wets_start:I(log(sl_+1))|id) +
  #                                  (0 + prop_needleleaf_end|id) +
  #                                  (0 + prop_mixforest_end|id) +
  #                                  (0 + prop_veg_end|id) +
  #                                  (0 + prop_wets_end|id) +
  #                                  (0 + (I(log(ts_fires_end+1)))|id) +
  #                                  (0 + I(log(sl_+1)):I(log(ts_fires_start+1))|id) +
  #                                  (0 + (I(log(ts_harv_end+1)))|id) + 
  #                                  (0 + I(log(sl_+1)):I(log(ts_harv_start+1))|id) + 
  #                                  (0 + I(log(distlf_end+1))|id) + 
  #                                  (0 + I(log(sl_+1)):I(log(distlf_start+1))|id) +
  #                                  (0 + I(log(distlf_other_end+1))|id) + 
  #                                  (0 + I(log(sl_+1)):I(log(distlf_other_start+1))|id) +
  #                                  (0 + disturbance_end|id) 
  #                                ,
  #                                family = poisson(), data = mb.2015.sub,
  #                                map= list(theta = factor(c(NA,1:20))),
  #                                start = list(theta =c(log(1000), seq(0,0, length.out = 20))),
  #                                verbose = TRUE
  # )
  # 
  # 
  # 
  # summary(m.mb.2015.sub)