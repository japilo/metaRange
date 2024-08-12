#' @title metaRangeParallel object 
#' 
#' @description Create infrastructure that can spin up and run multiple 
#' metaRangeSimulations in parallel, each parameterized using a row of a sample
#' data frame.
#' 
#' @return A `<metaRangeParallel>` object.
#' @importFrom cli cli_abort
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @export 
metaRangeParallel <- R6::R6Class("metaRangeParallel",
  cloneable = TRUE,
  portable = TRUE,
  lock_objects = FALSE,
  public = list(

    ## Methods ##
    #' @description Create a new [metaRangeParallel] object.
    #' @param simulation_template A metaRangeSimulation object already loaded 
    #' with species and processes common to all simulations.
    #' @param sample_data A data.frame where each column represents a trait 
    #' and each row is a different value for that trait. The number of rows 
    #' determines the number of simulations.
    #' @param results_dir Results directory path where the simulation results 
    #' and simulation log will be stored.
    #' @param ... Parameters listed individually.
    initialize = function(simulation_template, sample_data, results_dir, ...) {
      args <- list(...)
      if (!missing(simulation_template)) {
        self$simulation_template <- simulation_template
      }
      if (!missing(sample_data)) {
        self$sample_data <- sample_data
      }
      if (!missing(results_dir)) {
        self$results_dir <- results_dir
      }
      for (arg in names(args)) {
        self[[arg]] <- args[[arg]]
      }
      if (getOption("metaRange.verbose", default = FALSE)) {
          message("created handler for ", self$n_sims, "simulations")
      }
    },

    #' @description
    #' Substitutes the specified sample details into a status message (using sprintf) and returns the result.
    #' @param status_message Character string message with a placeholder for sample details.
    #' @param sample_index Row index of sample data frame containing details of substitution parameters.
    #' @return Status message with substituted sample details.
    get_message_sample = function(status_message, sample_index) {
      sample_vector <- c("sample", as.character(sample_index))

      return(sprintf(status_message, paste(sample_vector, collapse = " ")))
    },

    #' @description
    #' Summarizes the simulation log and writes it to a text file.
    #'
    #' This method takes a nested list of simulation log entries generated by the `run` method and summarizes the log. It determines which simulations were successful, collects any warnings, and writes the summary to a text file in the results directory.
    #'
    #' @param simulation_log A nested list of simulation log entries.
    #' @return A list containing the summary, indices of failed simulations, indices of simulations with warnings, and the full log.
    #' @export
    log_simulation = function(simulation_log) {
      # Determine which simulations were successful and collect any warnings
      successful_array <- array(FALSE, length(simulation_log))
      warning_indices <- c()
      for (i in 1:length(simulation_log)) {
        if (is.null(simulation_log[[i]]$successful)) {
          simulation_log[[i]] <- list(message = as.character(simulation_log[[i]]), successful = FALSE)
        }
        successful_array[i] <- simulation_log[[i]]$successful
        if (!is.null(simulation_log[[i]]$warnings)) {
          warning_indices <- c(warning_indices, i)
        }
      }
      # Add a summary and failure & warning indices to the log
      simulation_log <- list(summary = sprintf("%s of %s sample models ran and saved results successfully",
                                               length(which(successful_array)), length(simulation_log)),
                             failed_indices = which(!successful_array),
                             warning_indices = warning_indices,
                             full_log = simulation_log)
      if (length(warning_indices)) {
        simulation_log$summary <- paste(simulation_log$summary, "with warnings")
      }
      # Write a log file
      log_file <- file.path(self$results_dir, "simulation_log.txt")
      suppressWarnings(try({
        file_con <- file(log_file, 'w')
        writeLines(c(simulation_log$summary), con = file_con)
        if (length(simulation_log$failed_indices)) {
          writeLines(c("", paste(length(simulation_log$failed_indices), "failed runs/errors:")), con = file_con)
          for (i in simulation_log$failed_indices) {
            writeLines(c("", paste("Sample", i, ":"), simulation_log$full_log[[i]]$message), con = file_con)
            if (!is.null(simulation_log$full_log[[i]]$errors)) {
              writeLines(simulation_log$full_log[[i]]$errors, con = file_con)
            }
          }
        }
        if (length(warning_indices)) {
          writeLines(c("", paste(length(warning_indices), "warnings:")), con = file_con)
          for (i in warning_indices) {
            writeLines(c("", paste("Sample", i, ":"), simulation_log$full_log[[i]]$message), con = file_con)
            writeLines(simulation_log$full_log[[i]]$warnings, con = file_con)
          }
        }
        close(file_con)
      }, silent = TRUE))
      return(simulation_log)
    },

    #' @description
    #' Sets the model sample attributes via the sample data frame and the 
    #' generators.
    #'
    #' This method sets the sample attributes of a SimulationModel object based
    #'  on the specified sample index.
    #' It uses the sample data frame and the generators to determine the 
    #' attribute values.
    #'
    #' @param simulation \code{\link{metaRangeSimulation}} object (clone) to 
    #' receive sample traits.
    #' @param sample_index Index of sample from data frame.
    #' @keywords internal
    #' @export
    set_model_sample = function(simulation, sample_index) {

      simulation$add_globals(
        results_dir = file.path(self$results_dir, paste0("simulation", sample_index))
      )

      sample_list <- as.list(self$sample_data[sample_index, ])
      names(sample_list) <- names(self$sample_data)

      already_assigned <- intersect(names(sample_list), names(simulation[[self$species_name]]$traits))

      if (length(already_assigned)) {
        cli_abort(c("Error: tried to assign traits that are already present in the simulation template.",
                  "i" = "These traits are {already_assigned}."))
      }

      rlang::inject(simulation$add_traits(
        species = self$species_name,
        population_level = FALSE,
        !!!sample_list
      ))

    if (!is.null(self$generators)) {
        for (i in seq_along(self$generators)) {
          generator <- self$generators[[i]]

          if (!is.null(self$generative_names[[i]])) {
            if ("DispersalGenerator" %in% class(generator)) {
              inputs <- intersect(generator$inputs, generator$get_attribute_aliases())

              if (any(names(simulation[[self$species_name]]$traits) %in% inputs)) {
                generator$set_attributes(params = setNames(lapply(inputs, 
                                                  function(input) simulation[[self$species_name]]$traits[[input]]), 
                                           inputs))
              } else if (any(names(self$sample_data) %in% inputs)) {
                generator$set_attributes(params = sample_list[inputs])
              }

              if (generator$generative_requirements_satisfied()$dispersal_data) {
                generator$calculate_dispersals(type = "matrix")
                new_input <- setNames(list(generator$dispersal_matrix), self$generative_names[[i]])
                if (length(generator$error_messages)) {
                  cli::cli_abort(c("Dispersal generator {name} produced errors:", "x" = "{generator$error_messages}"))
                }
                rlang::inject(simulation$add_traits(
                  species = self$species_name,
                  population_level = FALSE,
                  !!!new_input
                ))
              } else {
                cli::cli_abort(c("Errors produced when generating {self$generative_names[[i]]}.", "x" = "This generator requires {generator$inputs}."))
              }
            } else {
              inputs <- unique(c(generator$inputs, generator$get_attribute_aliases(params = generator$inputs)))

              input_values <- setNames(vector("list", length(inputs)), inputs)

              matching_attributes <- intersect(names(input_values), names(simulation[[self$species_name]]$traits))
              if (length(matching_attributes) == 0) {
                cli_abort(c("The input values for generator {generator$description}
                           could not be found in the species traits.",
                           "x" = "Input values {input_values} are missing."))
              } else {
                input_values[matching_attributes] <- lapply(matching_attributes, 
                                                            function(input) simulation[[self$species_name]]$traits[[input]])
              }

              matching_samples <- intersect(names(input_values), names(sample_list))
              input_values[matching_samples] <- sample_list[matching_samples]

              new_input <- generator$generate(input_values = input_values)
              if (grepl("abundance", self$generative_names[[i]], ignore.case = TRUE)) {
                rlang::inject(simulation$add_traits(
                  species = self$species_name,
                  population_level = TRUE,
                  !!!new_input
                ))
              } else {
                rlang::inject(simulation$add_traits(
                  species = self$species_name,
                  population_level = FALSE,
                  !!!new_input
                ))
              }

              if (length(generator$error_messages)) {
                cli::cli_abort(c("Generator {generator$description} produced errors:", "x" = "{generator$error_messages}"))
              }
            }
          }
        }
      }
    },


    #' @description
    #' Runs the multiple population simulations, stores the results, and 
    #' creates a simulation log.
    #'
    #' This method runs multiple population simulations using the 
    #' specified simulation template and sample data. It passes a results 
    #' directory to processes within the simulation template that save results,
    #'  and creates a simulation log. The simulation log contains information 
    #' about the success or failure of each simulation run.
    #'
    #' @return A list representing the simulation log. Each element of the list
    #'  corresponds to a simulation run and contains information about the 
    #' success or failure of the run, any error messages, and the path to the 
    #' saved results file (if applicable).
    #' @export
    run = function() {

      # Check for error messages
      if (!is.null(self$error_messages)) {
        error_messages <- self$error_messages
        self$error_messages <- NULL
        stop(error_messages, call. = FALSE)
      }

      # Check the completeness/consistency of the first sample only
      model <- self$simulation_template$new_clone()
      self$set_model_sample(model, 1)
      model <- NULL

      if (self$register_parallel == TRUE) {
        doParallel::registerDoParallel(cores = self$parallel_threads)
      }

      simulation_log <- foreach(i = 1:nrow(self$sample_data),
                          .packages = c("raster", "epizootic", "metaRange"),
                          .export = c("self"),
                          .errorhandling = c("pass")) %dopar% {

        # Clone the model
        model <- self$simulation_template$new_clone()
        self$set_model_sample(model, i)

        # Run the simulator
        run_status <- NULL
        run_status <- tryCatch(
          {
            suppressWarnings(
              withCallingHandlers(
                {
                  model$begin()
                },
                warning = function(w) {
                  self$warning_messages <- c(
                    self$warning_messages,
                    gsub("simpleWarning", "Warning",
                      gsub("\n", "", as.character(w), fixed = TRUE),
                      fixed = TRUE
                    )
                  )
                }
              )
            )
            if (!is.null(self$attached$warnings)) {
              list(
                successful = TRUE, message = "Model %s simulation ran successfully with warnings",
                warnings = self$warning_messages
              )
            } else {
              list(successful = TRUE, message = "Model %s simulation ran successfully")
            }
          },
          error = function(e) {
            list(
              successful = FALSE, message = "Model %s simulation ran unsuccessfully with errors",
              errors = c(as.character(e))
            )
          }
        )
        if (is.null(run_status)) {
          run_status <- list(successful = FALSE, message = "Model %s simulation had unknown failure without errors")
        }

        # Substitute sample details into the simulator run status message
        run_status$message <- self$get_message_sample(run_status$message, i)

        # Check results directories
        if (run_status$successful) {
          results_dir <- file.path(self$results_dir, paste0("simulation", i))
          if (length(list.files(results_dir)) > 0) {
            run_status$message <- paste0(run_status$message, " and the results were saved")
          } else {
            run_status$successful <- FALSE
            run_status$message <- paste0(run_status$message, ", but the results could not be saved in ", results_dir)
          }
        }

        return(run_status)
      }
      doParallel::stopImplicitCluster()

      # Summarize and write log to a file
      simulation_log <- self$log_simulation(simulation_log)
    }
  ),

  private = list(
    .detected_cores = NULL,
    .error_messages = NULL,
    .generative_names = NULL,
    .generators = NULL,
    .parallel_threads = 1,
    .register_parallel = TRUE,
    .results_dir = tempdir(),
    .sample_data = NULL,
    .seed = sample.int(1000000, 1),
    .simulation_template = NULL,
    .species_name = "species_1",
    .warning_messages = NULL
  ),
  active = list(
    
    # --------- // error_messages -------------
    #' @field error_messages A vector of error messages encountered when
    #' setting simulation traits.
    error_messages = function(value) {
      if (missing(value)) {
        private$.error_messages
      } else {
        checkmate::assert_character(value, null.ok = TRUE)
        private$.error_messages <- value
      }
    },
    
    # ---------- // generative_names ----------
    #' @field generative_names `<list>` List of names of outputs from 
    #' generators.
    generative_names = function(value) {
      if (missing(value)) {
        private$.generative_names
      } else {
        checkmate::assert_list(value, types = "character", null.ok = TRUE)
        private$.generative_names <- value
      }
    },

    # ---------- // generators ----------------
    #' @field generators A list of generators (\code{\link{Generator}} or 
    #' inherited class) objects for generating simulation model values.
    generators = function(value) {
      if (missing(value)) {
        private$.generators
      } else {
        validate_generator <- function(gen) {
          if (!inherits(gen, "Generator")) {
            cli::cli_abort("Generators must be Generator or inherited class objects.")
          }
        }
        
        if (is.null(value)) {
          private$.generators <- NULL
        } else if (inherits(value, "Generator")) {
          private$.generators <- list(value)
        } else if (is.list(value)) {
          lapply(value, validate_generator)
          private$.generators <- value
        } else {
          cli::cli_abort("Generators must be a Generator object or a list of Generator objects.")
        }
        
        self$generative_names <- lapply(seq_along(private$.generators), function(i) {
          generator <- private$.generators[[i]]
          if ("DispersalGenerator" %in% class(generator)) {
            paste0("dispersal", i)
          } else {
            generator$outputs
          }
        })
      }
    },


    # ---------- // parallel threads ----------
    #' @field parallel_threads `<integer>` number of parallel threads to run
    #' simulations.
    parallel_threads = function(value) {
      if (missing(value)) {
        private$.parallel_threads
      } else {
        if (is.null(private$.detected_cores)) {
          private$.detected_cores <- parallel::detectCores()
        }
        parallel_threads <- checkmate::assert_int(value, lower = 1L, upper = private$.detected_cores, null.ok = FALSE, coerce = TRUE)
        private$.parallel_threads <- parallel_threads
      }
    },

    # --------- // register_parallel ----------------
    #' @field register_parallel `<logical>` Should the metaRangeParallel object register parallel cores? If FALSE, the user must register parallel cores outside of this R6 object on their own. For the default value, TRUE, `metaRangeParallel` does it for you.
    register_parallel = function(value) {
      if (missing(value)) {
        private$.register_parallel
      } else {
        checkmate::assert_logical(value)
        private$.register_parallel <- value
      }
    },

    # --------- // results_dir ----------------
    #' @field results_dir `<character>` directory where results will be saved.
    results_dir = function(value) {
      if (missing(value)) {
        private$.results_dir
      } else {
        checkmate::assert_directory_exists(value, access = "w")
        private$.results_dir <- value
      }
    },
    
    # -------- // sample_data -----------------
    #' @field sample_data A data frame of sampled parameters for each simulation.
    sample_data = function(value) {
      if (missing(value)) {
        private$.sample_data
      } else {
        checkmate::assert_data_frame(value)
        private$.sample_data <- value
      }
    },

    # ---------- // seed ----------------------
    #' @field seed `<integer>` seed for the random number generator.
    seed = function(value) {
      if (missing(value)) {
        private$.seed
      } else {
        seed <- checkmate::assert_int(value, lower = 1L, null.ok = FALSE, 
                                      coerce = TRUE)
        private$.seed <- seed
      }
      set.seed(private$.seed)
      lockBinding(".seed", private)
    },
    
    # ---------- // simulation_template -------
    #' @field simulation_template A \code{\link{metaRangeSimulation}} object 
    #' with processes and traits common to all simulations.
    simulation_template = function(value) {
      if (missing(value)) {
        private$.simulation_template
      } else {
        checkmate::assert_class(value, "metaRangeSimulation")
        private$.simulation_template <- value

        if (inherits(private$.simulation_template, "metaRangeSimulation")) {
          species_names <- private$.simulation_template$species_names()
          if (length(species_names) > 0) {
            self$species_name <- species_names[1]
          }
        }
      }
    },

    
    # ---------- // species_name --------------
    #' @field species_name `<character>` name of species being modeled
    species_name = function(value) {
      if (missing(value)) {
        private$.species_name
      } else {
        checkmate::assert_character(value, len = 1)
        private$.species_name <- value

        if (inherits(self$simulation_template, "metaRangeSimulation")) {
          if (!(private$.species_name %in% self$simulation_template$species_names())) {
            self$simulation_template$add_species(private$.species_name)
          }
        }
      }
    },

    # ------- // warning_messages ------------
    #' @field warning_messages A vector of warning messages encountered when 
    #' setting model attributes.
    warning_messages = function(value) {
      if (missing(value)) {
        private$.warning_messages
      } else {
        checkmate::assert_character(value, null.ok = TRUE)
        private$.warning_messages <- value
      }
    }

  )
)