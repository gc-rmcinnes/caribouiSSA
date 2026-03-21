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

    # Most recent N-year filter (GLOBAL)
    if (Par$modelScale %in% c("global", "both")) {

      n_years <- Par$globalModelFilter
      max_year <- max(Par$histLandYears, na.rm = TRUE)

      keep_years <- (max_year - n_years + 1):max_year

      message(
        "Applying most recent ", n_years,
        "-year filter for ", j
      )

      # get year per strata (safe even if duplicated)
      strata_years <- dt[, .(year = max(year)), by = indiv_step_id]

      # identify strata to keep
      keep_strata <- strata_years[year %in% keep_years, indiv_step_id]

      n_before <- nrow(dt)

      # filter entire strata
      dt <- dt[indiv_step_id %in% keep_strata]

      n_after <- nrow(dt)

      message(
        "  Jurisdiction: ", j,
        " | Years: ", paste(keep_years, collapse = ", "),
        " | Rows: ", n_after,
        " | Strata: ", length(unique(dt$indiv_step_id))
      )
    }

    dt[, id := as.factor(id)]
    dt[, indiv_step_id := as.factor(indiv_step_id)]
    dt[, year := as.factor(year)]

    # Construct covariates
    dt <- construct_covariates(dt)

    # End point check
    n_before <- nrow(dt)

    # Identify strata with missing endpoints
    bad_steps <- dt[!complete.cases(dt[, c("x2_", "y2_")]), unique(indiv_step_id)]

    # Remove entire strata
    if (length(bad_steps) > 0) {
      dt <- dt[!indiv_step_id %in% bad_steps]
    }

    n_after <- nrow(dt)

    if (n_before != n_after) {
      message("  Removed ", n_before - n_after,
              " rows belonging to strata with missing endpoints in ", j)
    }

    # Save to list
    juris_list[[j]] <- dt
  }
  return(juris_list)
}
