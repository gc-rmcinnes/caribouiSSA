iSSAmodel <- function(scale, juris_list) {
  models <- list()
  summaries <- list()

  # run the jurisdictional models
  if (scale == "jurisdictional"){
    for (j in names(juris_list)) {

      dat <- juris_list[[j]]

      message("Starting iSSA for: ", j)
      mod <- glmmTMB(
        formula = as.formula(Par$iSSAformula),
        family  = poisson(),
        data    = dat,
        map     = list(theta = factor(c(NA, 1:22))),
        start   = list(theta = c(log(1000), rep(0, 22)))
      )

      models[[j]] <- mod
      summaries[[j]] <- summary(mod)
      message("Finished iSSA for: ", j)
    }
  }
  # run the global model of all data points
  # add the global formula
  if (scale == "global"){
    dat.all <- rbindlist(juris_list)
    message("Starting global iSSA")
    mod <- glmmTMB(
      formula = as.formula(Par$iSSAglobalformula),
      family  = poisson(),
      data    = dat.all,
      map     = list(theta = factor(c(NA, 1:21))),
      start   = list(theta = c(log(1000), rep(0, 21)))
    )
    models[["global"]] <- mod
    summaries[["global"]] <- summary(mod)
  }
  # run both the jurisdictional models and the global model
  if (scale == "both"){
    # run the iSSA for each jurisdiction provided by the workflow
    # add the global formula and update the outputs
    for (j in names(juris_list)) {

      dat <- juris_list[[j]]

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

    # global model
    dat.all <- rbindlist(juris_list)
    message("Starting global iSSA")
    mod <- glmmTMB(
      formula = as.formula(Par$iSSAglobalformula),
      family  = poisson(),
      data    = dat.all,
      map     = list(theta = factor(c(NA, 1:21))),
      start   = list(theta = c(log(1000), rep(0, 21)))
    )
    models[["global"]] <- mod
    summaries[["global"]] <- summary(mod)
  }
  return(list(
    models = models,
    summaries = summaries
  ))
}
