defineModule(sim, list(
  name = "caribouiSSA",
  description = "",
  keywords = "",
  authors = c(person("Julie W.", "Turner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "rory_mcinnes@hotmail.com", role = c("aut", "cre"))),
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
                              (0 + I(log(timeSinceHarvest_end + 1)) | id) +
                              (0 + I(log(distpaved_end + 1)) | id) +
                              (0 + I(log(distunpaved_end + 1)) | id) +
                              (0 + I(log(distpolys_end + 1)) | id) +
                              ECOREGION_start +
                              (1 | year)",
                    desc = 'The iSSA formula used for each jurisdiction'),
    defineParameter("jurisdiction", "character",c("BC", "SK", "MB", "YT", "NT", "ON"),
                    desc = "A list of jurisdictions to run iSSA models on"),
    defineParameter("modelSelection", "character", "iSSA",
                    desc = "model selection for analysis"),
    defineParameter("modelScale", "character", "jurisdictional",
                    desc = "define the scale to run the iSSA. Can be 'jurisdictional', 'global', or 'both'."),
    defineParameter("outputFolderID", "character", "https://drive.google.com/drive/folders/1CRSY_tJucL3E8VDgUYEv9WXuG1nKImH3", NA, NA,
      "Google Drive folder ID for workflow outputs"),
    defineParameter("globalModelFilter", "numeric", 5, NA, NA,
                    "The number of years to subset the tracks for the global model")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = 'extractedVariables', objectClass = "data.table",
                 desc = "variables of landscape values at the animal locations and distance calculations matched by year to the animal locations"),
    expectsInput(objectName = "landscapeYearly", objectClass = "SpatRaster",
                 desc = "spatRaster stack of the yearly landscape layers"),

    expectsInput(objectName = "landscape5Yearly", objectClass = "SpatRaster",
                 desc = "SpatRaster stacks of the 5 year interval anthropogenic landscape layers")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "iSSAmodels", objectClass = "list",
                  desc = "A list of 'glmmTMB' iSSA models by jurisdiction and/or global scales"),
    createsOutput(objectName = "iSSAsummaries", objectClass = "list",
                  desc = "A list of iSSA model summaries by jurisdiction and/or global scales"),
    createsOutput(objectName = "modelLand", objectClass = "list",
                  desc = "A list of rasters aligned with the model outputs to be used in UD modules")
  )
))

doEvent.caribouiSSA = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- scheduleEvent(sim, time(sim), "caribouiSSA", "prepareCovariates")
      sim <- scheduleEvent(sim, time(sim), "caribouiSSA", "prepareModelLand")
      sim <- scheduleEvent(sim, time(sim), "caribouiSSA", "runiSSAmodel")
      sim <- scheduleEvent(sim, time(sim), "caribouiSSA", "save")
    },
    prepareCovariates = {
      # harmonize the covariates with the iSSA formula
      dat <- sim$extractedVariables
      sim$juris_list <- iSSAprep(dat)
    },

    prepareModelLand = {
      # prepare the landscape rasters that will be used in the UD modules
      sim$modelLand <- composeLandscape(sim$landscapeYearly, sim$landscape5Yearly)
    },

    runiSSAmodel = {

      iSSAoutput <- iSSAmodel(
        juris_list = sim$juris_list,
        scale = Par$modelScale)

      sim$iSSAmodels <- iSSAoutput$models
      sim$iSSAsummaries <- iSSAoutput$summaries

      for (j in names(sim$iSSAmodels)) {

        saveRDS(
          sim$iSSAmodels[[j]],
          file.path(outputPath(sim), paste0("iSSA_", j, ".rds"))
        )
      }
    },
    save = {
      message("Saving outputs to workflowOutputs on Google Drive")

      if (!requireNamespace("googledrive", quietly = TRUE)) {
        stop("Package 'googledrive' is required for this event.")
      }

      # # Get root folder
      # root_id <- getOption("reproducible.cloudFolderID")
      #
      # if (is.null(root_id)) {
      #   stop("reproducible.cloudFolderID is not set.")
      # }
      #
      # root_folder <- googledrive::as_id(root_id)
      #
      # # Create / get subfolder
      # subfolder_name <- "workflowOutputs"
      #
      # subfolder <- googledrive::drive_find(
      #   q = paste0(
      #     "name = 'workflowOutputs' and '",
      #     root_folder,
      #     "' in parents and trashed = false"
      #   )
      # )
      #
      # if (nrow(subfolder) == 0) {
      #   subfolder <- googledrive::drive_mkdir(
      #     name = "workflowOutputs",
      #     path = root_folder
      #   )
      # }
      folder_id <- Par$outputFolderID

      if (is.na(folder_id)) {
        stop("outputFolderID parameter must be supplied")
      }

      output_folder <- googledrive::as_id(folder_id)

      tmp_land   <- tempfile(fileext = ".tif")
      tmp_models <- tempfile(fileext = ".RData")

      # Get layer names
      layer_names <- names(sim$modelLand)

      # Initialize correctly
      tmp_files <- vector("list", length(layer_names))

      # Loop over layers
      for (i in seq_along(layer_names)) {

        nm <- layer_names[i]

        r <- sim$modelLand[[i]]  # <- IMPORTANT: index layer, not name

        f <- tempfile(pattern = nm, fileext = ".tif")

        terra::writeRaster(
          r,
          f,
          overwrite = TRUE
        )

        tmp_files[[i]] <- f
      }

      # Rebuild stack
      land_rast <- terra::rast(unlist(tmp_files))
      names(land_rast) <- layer_names

      # Write final raster
      terra::writeRaster(
        land_rast,
        tmp_land,
        overwrite = TRUE,
        gdal = c("COMPRESS=LZW")
      )
      browser()
      message("Uploading modelLand")

      upload_with_retry <- function(file, path, name, max_attempts = 3) {

        for (i in seq_len(max_attempts)) {

          message("Upload attempt ", i)

          tryCatch({

            googledrive::drive_upload(
              media = file,
              path = path,
              name = name,
              overwrite = TRUE
            )

            message("Upload successful")
            return(TRUE)

          }, error = function(e) {

            message("Upload failed: ", e$message)

            if (i == max_attempts) stop(e)

            Sys.sleep(5 * i)  # backoff

          })
        }
      }
      upload_with_retry(tmp_land, output_folder, "modelLand.tif")

      # Save iSSA models
      iSSAmodels <- sim$iSSAmodels

      save(iSSAmodels, file = tmp_models)

      message("Uploading iSSA models")

      googledrive::drive_upload(
        media = tmp_models,
        path = output_folder,
        name = "iSSAmodels.RData",
        overwrite = TRUE
      )

      unlink(c(tmp_land, tmp_models))

      message("Upload complete → caribouWorkflow/workflowOutputs")

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
