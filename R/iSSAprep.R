iSSAprep <- function(dat) {

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
