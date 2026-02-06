# harmonize the covariates with the iSSA model
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
