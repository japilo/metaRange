# Author: Stefan Fallert
# Date: 26.02.2023
# License: GPL-3 (See License.md)
#
# This file incorporates work covered by the following copyright and
# permission notice:
#
# ~~~~~~~~~~~~
# MIT License
# Copyright (c) 2018,2019,2020 mikefc@coolbutuseless.com
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# ~~~~~~~~~~~~~
# Compare the assignment of the correct `.__enclos_env__` to the process function to
# https://coolbutuseless.github.io/2021/02/19/modifying-r6-objects-after-creation/


#' @title metaRangeSimulation object
#'
#' @description Creates an simulation object in form of an
#' [R6][R6::R6Class] class that stores and handles all the individual parts
#' that are necessary to run a simulation.
#'
#' @return A `<metaRangeSimulation>` object
#' @export
metaRangeSimulation <- R6::R6Class("metaRangeSimulation",
    cloneable = FALSE,
    portable = TRUE,
    lock_objects = FALSE,
    public = list(
        # ---------- 1 public fields -------------
        # ---------- // ID -------------------
        #' @field ID simulation identification.
        ID = NULL,

        # ---------- // globals -------------------
        #' @field globals a place to store global variables.
        globals = NULL,

        # ---------- // environment --------------
        #' @field environment A [metaRangeEnvironment] that holds all the environmental
        #' values influencing the simulation.
        environment = NULL,

        # ---------- // time ----
        #' @field  number_time_steps number of timeseps in the simulation.
        number_time_steps = NULL,

        #' @field time_step_layer vector of layer IDs
        #' that describe which environmental layer to use at each time step.
        time_step_layer = NULL,

        #' @field current_time_step current time step.
        current_time_step = NULL,

        # ---------- // queue ----------
        #' @field queue The order in which the processes should be executed.
        queue = NULL,

        # ---------- // processes ----------
        #' @field processes global processes
        processes = NULL,

        # ---------- // seed ----------------------
        #' @field seed `<integer>` seed for the random number generator.
        seed = NULL,

        # ---------- 2 initialization ------------

        #' @description Creates a new [metaRangeSimulation] object.
        #' @param source_environment `<SpatRasterDataset>` created by [terra::sds()] that represents the environment.
        #' The individual data sets represent different environmental variables
        #' (e.g. temperature or habitat availability) and the different layer of the data sets
        #' represent the different timesteps of the simulation.
        #' The function [metaRangeSimulation]`$set_time_layer_mapping()` can be used
        #' to extend/ shorten the simulation timesteps and set the mapping between each time step and a corresponding
        #' environmental layer. This can be used e.g. to repeat the first (few) layer as a burn-in period.
        #' The number of layers must be the same for all data sets.
        #' @param ID `<string>` optional simulation identification string.
        #' Will be set automatically if none is specified.
        #' @param seed `<integer>` optional seed for the random number generator.
        #' Will be set automatically if none is specified.
        #' @return A `<metaRangeSimulation>` object.
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim
        initialize = function(source_environment, ID = NULL, seed = NULL) {
            if (!missing(source_environment)) {
                private$set_sim_environment(source_environment)
            }
            if (!is.null(ID)) {
                checkmate::assert_string(ID, min.chars = 1, max.chars = 64, null.ok = FALSE)
                self$ID <- ID
            } else {
                self$ID <- paste0("simulation_", as.hexmode(sample.int(100000000L, 1)))
            }
            lockBinding("ID", self)

            self$globals <- list()

            self$processes <- list()

            self$queue <- metaRangePriorityQueue$new()
            if (!is.null(seed)) {
                seed <- checkmate::assert_int(seed, lower = 1L, null.ok = FALSE, coerce = TRUE)
                self$seed <- seed
            } else {
                self$seed <- sample.int(1000, 1)
            }
            set.seed(self$seed)
            lockBinding("seed", self)
            if (getOption("metaRange.verbose", default = FALSE)) {
                message("created simulation: ", self$ID)
            }
        },


        # ---------- 3 public methods -----------

        # ---------- 3.1 low-level setup --------
        #' @description Add global variables to the simulation
        #' @param ... `<atomic>` (see [base::is.atomic()])
        #' Variables to add to the simulation. They will be saved and accesible
        #' through the 'globals' field.
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_globals(a = 1, b = 2)
        #' sim$globals$a
        #' #> [1] 1
        #' @return invisible self
        add_globals = function(...) {
            globals_to_add <- list(...)
            if (getOption("metaRange.verbose", default = FALSE) > 0L) {
                message("adding global variables: ")
                message(str(globals_to_add), appendLF = FALSE)
            }
            # TODO: prevent user acces?
            # list2env(globals_to_add, envir = self$globals) #TODO use env?
            self$globals <- c(self$globals, globals_to_add)
            return(invisible(self))
        },

        #' @description Set the time layer of the simulation.
        #' @param x `<integer>` vector of layer indices
        #' that describe which environmental layer to use at each time step.
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2, nlyr = 4))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$set_time_layer_mapping(1:2)
        #' stopifnot(identical(sim$time_step_layer, 1:2))
        #' @return invisible self
        set_time_layer_mapping = function(x) {
            x <- checkmate::assert_integerish(
                x,
                lower = 1L,
                upper = min(terra::nlyr(self$environment$sourceSDS)),
                null.ok = FALSE,
                any.missing = FALSE,
                all.missing = FALSE,
                coerce = TRUE,
                min.len = 1L
            )
            self$time_step_layer <- x
            self$number_time_steps <- length(x)
            private$set_current_time_step(1L)

            if (getOption("metaRange.verbose", default = FALSE)) {
                message("number of time steps: ", self$number_time_steps)
                message("time step layer mapping: ", paste(x, collapse = ", "))
            }
            return(invisible(self))
        },

        #' @description Get current time step
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$get_current_time_step()
        #' #> [1] 1
        #' @return invisible self
        get_current_time_step = function() {
            return(private$current_time_step)
        },

        #' @description Adds a new species to the simulation
        #' @param name `<string>` name or ID of the species.
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_species("species_1")
        #' sim$species_1
        #' @return `<invisible boolean>` `TRUE` on success `FALSE` on failure.
        add_species = function(name) {
            verbosity <- getOption("metaRange.verbose", default = FALSE)
            if (verbosity > 0L) message("adding species")
            if (!checkmate::test_string(name)) {
                message("failed to add species. Argument 'name' must be a string")
                return(invisible(FALSE))
            }
            if (is.null(self$environment) | !checkmate::test_class(self$environment, "metaRangeEnvironment")) {
                message("cannot add species to a simulation without environment. Please add environment first.")
                return(invisible(FALSE))
            }
            if (name %in% names(self)) {
                message("Species already present in simulation. Use a different name.")
                return(invisible(FALSE))
            }
            self[[name]] <- metaRangeSpecies$new(name = name, sim = self)
            if (verbosity > 0L) message("Name: ", name)
            return(invisible(TRUE))
        },

        #' @description Returns the names of all species in the simulation.
        #' @return `<character>` vector of species names
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_species("species_1")
        #' sim$add_species("species_2")
        #' sim$species_names()
        #' #> [1] "species_1" "species_2"
        species_names = function() {
            name_vec <- ls(envir = self, sorted = FALSE)
            res_vec <- vector("logical", length(name_vec))
            for (i in seq_along(name_vec)) {
                cond <- inherits(get0(name_vec[i], envir = self, inherits = FALSE), "metaRangeSpecies")
                if (cond) {
                    res_vec[i] <- TRUE
                }
            }
            return(name_vec[res_vec])
        },

        #' @description Adds a process to the simulation.
        #' @param species `<string>` Name of the species that the process should be added to.
        #' If `NULL` the process will be added to the simulationn object itself.
        #' @param process_name `<string>` Name of the process to add.
        #' @param process_fun `<named function>` The function to call when the process gets executed.
        #' @param execution_priority `<positive integer>` When this process should run within each time step.
        #' 1 == highest priority i.e. this function will be the executed first.
        #' @param queue `<boolean>` If `TRUE` the process will be added to the process execution queue directly.
        #' If `FALSE` the process will be added to the simulation but not to the queue,
        #' which means that in order to execute the process, it has to be added manually
        #' via the [metaRangePriorityQueue]`$enqueue()` method.
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_species("species_1")
        #' sim$add_process("species_1", "species_process_1", function() {message("process_1")}, 1)
        #' sim$species_1$processes$species_process_1
        #' sim$add_process(species = NULL, "global_process_2", function() {message("process_2")}, 2)
        #' sim$processes$global_process_2
        #' @return invisible(self).
        add_process = function(species = NULL, process_name, process_fun, execution_priority, queue = TRUE) {
            verbosity <- getOption("metaRange.verbose", default = FALSE)
            checkmate::assert_string(x = process_name, min.chars = 1, max.chars = 64)
            checkmate::assert_names(x = process_name, type = "strict")
            if (verbosity > 0L) message("adding process: ", process_name)
            if (!is.null(species)) {
                speciec_in_sim <- self$species_names()
                if (verbosity > 0L) message("to species: ", species)
                species_present <- species %in% speciec_in_sim
                if (!all(species_present)) {
                    stop("Species '", species[!species_present], "' not found in simulation. Unable to add process.")
                }

                for (sp in species) {
                    private$process_num <- private$process_num + 1L
                    self[[sp]]$processes[[process_name]] <- metaRangeProcess$new(
                        process_name = process_name,
                        id = as.character(private$process_num),
                        process_fun = process_fun,
                        execution_priority = execution_priority,
                        env = self[[sp]]$.__enclos_env__,
                        env_label = sp
                    )
                    if (queue) {
                        self$queue$enqueue(self[[sp]]$processes[[process_name]])
                    }
                }
            } else {
                private$process_num <- private$process_num + 1L
                self$processes[[process_name]] <- metaRangeProcess$new(
                    process_name = process_name,
                    id = as.character(private$process_num),
                    process_fun = process_fun,
                    execution_priority = execution_priority,
                    env = self$.__enclos_env__,
                    env_label = self$ID
                )
                if (queue) {
                    self$queue$enqueue(self$processes[[process_name]])
                }
            }
            return(invisible(self))
        },

        #' @description Adds traits to a species.
        #' @param species `<string>` Name of the species that the traits should be added to.
        #' @param population_level `<boolean>` If `TRUE` the traits will be added at the population level
        #' (i.e. as a matrix with same dimensions (nrow/ncol) as the environment with one value for each population).
        #' This means that the traits either need to be single values that will be extended
        #' to such a matrix via [base::matrix()] or they already need to be a matrix with these dimension.
        #' If `FALSE` the traits will be added without any conversion and may have any type and dimension.
        #' @param ... `<atomic>` (see [base::is.atomic()]) The traits to be added.
        #' @return invisible(self).
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_species("species_1")
        #' sim$add_traits("species_1", population_level = TRUE, a = 1)
        #' sim$add_traits("species_1", population_level = FALSE, b = 2, c = "c")
        #' sim$species_1$traits$a
        #' #>      [,1] [,2]
        #' #> [1,]    1    1
        #' #> [2,]    1    1
        #' sim$species_1$traits$b
        #' #> [1] 2
        #' sim$species_1$traits$c
        #' #> [1] "c"
        add_traits = function(species, population_level = TRUE, ...) {
            trait_list <- list(...)
            verbosity <- getOption("metaRange.verbose", default = FALSE)
            if (verbosity > 0L) {
                message("adding traits: ")
                message(show(names(trait_list)), appendLF = FALSE)
            }
            species_in_sim <- self$species_names()
            checkmate::assert_character(
                species,
                min.len = 1,
                max.len = length(species_in_sim),
                any.missing = FALSE,
                unique = TRUE
            )

            if (verbosity > 0L) {
                message("to species: ")
                message(show(species), appendLF = FALSE)
            }
            species_present <- species %in% species_in_sim
            if (!all(species_present)) {
                stop("Species '", species[!species_present], "' not found in simulation. Unable to add traits.")
                species <- species[species_present]
            }
            for (i in seq_along(trait_list)) {
                if (!checkmate::test_atomic(trait_list[[i]])) {
                    stop("trait '", names(trait_list)[i], "' is not atomic. Unable to add trait.")
                }
            }

            for (sp in species) {
                for (i in seq_along(trait_list)) {
                    dim_m <- dim(trait_list[[i]])
                    dim_r <- dim(self$environment$sourceSDS)[c(1, 2)]
                    if (!population_level || is.matrix(trait_list[[i]]) && all(dim_m == dim_r)) {
                        self[[sp]]$traits[[names(trait_list)[i]]] <- trait_list[[i]]
                    } else {
                        self[[sp]]$traits[[names(trait_list)[i]]] <- matrix(
                            trait_list[[i]],
                            nrow = dim_r[1],
                            ncol = dim_r[2]
                        )
                    }
                }
            }
            return(invisible(self))
        },



        # ---------- 3.3 simulation -------------
        #
        # some general thought on futures optimizations:
        # processes that have the same priority can/ should be able to be executed in parallel
        # as they are by definition not sequential (using the parallel or future packages)
        #
        # there might also be the possibility to do some paralell processing within the processes
        # themselves (at the c++ level).
        # Ref: https://doi.org/10.1002/wics.1515
        # "Parallel computing with R: A brief review"
        # Edelbuettel (2020)
        #
        # another possible option would be to use subprocesses / callr to do the
        # loading/ matrix conversion of the environment or the saving in an async worker thread
        #
        # TLDR: there is a lot of potential for optimization here

        #' @description When called, will end the simulation (prematurely) once the current process is finished.
        #' Usefull to e.g. end the simulation safely (i.e. without an error) when no species is alive anymore
        #' and there would be no benefit to continue the execution util the last time step.
        #' @examples
        #' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 4))
        #' names(sim_env) <- "env_var_name"
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_species("species_1")
        #' sim$add_process("species_1", "species_process_1", function() {self$sim$exit()}, 1)
        #' sim$begin()
        #' @return `invisible NULL`
        exit = function() {
            private$continue_execution <- FALSE
            return(invisible(NULL))
        },

        #' @description Begins the simulation
        #' @examples
        #' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 4))
        #' names(sim_env) <- "env_var_name"
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$add_process(
        #'      species = NULL,
        #'      "timestep_counter",
        #'      function() {
        #'          message("timestep: ", self$get_current_time_step())
        #'      },
        #'      1
        #' )
        #' sim$begin()
        #' @return The finished simulation (i.e. invisible self)
        begin = function() {
            verbosity <- getOption("metaRange.verbose", default = FALSE)
            if (verbosity > 0L) message("Starting simualtion.\n")
            start_time_sim <- Sys.time()
            private$continue_execution <- TRUE
            if (is.null(private$current_time_step)) {
                private$set_current_time_step(1L)
            } else {
                private$set_current_time_step(private$current_time_step) # seems redundant, but assures correctness
            }
            if (private$current_time_step >= self$number_time_steps) {
                message(
                    "current_time_step >= number_time_steps.\n",
                    "Looks like the simulation has already been simulated. \n",
                    "Reset the simulation to run it again"
                )
                return(invisible(self))
            }
            self$queue$update()
            if (self$queue$is_empty()) {
                message("Process queue is empty. Unable start the simulation.")
                return(invisible(self))
            }
            private$validate() # TODO call this every time step?
            if (verbosity > 1L) message("passed initial sanity checks.\n")

            for (i in private$current_time_step:self$number_time_steps) {
                self$queue$update()
                if (self$queue$is_empty()) {
                    message("Process queue is empty.")
                    break
                }
                if (verbosity > 0L) message("start of time step: ", private$current_time_step)
                time_step_start <- Sys.time()
                self$environment$set_current(self$time_step_layer[[private$current_time_step]])

                while (!self$queue$is_empty() && private$continue_execution) {
                    sucess <- self$queue$execute_next_process(verbosity > 1L)
                    if (!sucess) {
                        private$continue_execution <- FALSE
                    }
                }
                if (!private$continue_execution) break
                if (verbosity > 0L) {
                    message(
                        "end of time step: ", private$current_time_step, "\n",
                        format((Sys.time() - time_step_start) * (self$number_time_steps - private$current_time_step),
                            digits = 2
                        ), " remaining (estimate)\n",
                        round(private$current_time_step / self$number_time_steps * 100, digits = 2), " % done\n"
                    )
                }
                if (!private$next_time_step()) break
            }
            if (verbosity > 0L) {
                message("Simulation: '", self$ID, "' finished\n")
            }
            on.exit(
                if (verbosity > 0L) {
                    message("Exiting the Simulation\n")
                    message("Runtime: ", format(Sys.time() - start_time_sim, digits = 2), "\n")
                }
            )
            return(invisible(self))
        },

        #' @description Prints information about the simulation to the console
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$print()
        #' @return invisible self
        print = function() {
            # TODO: pretty print
            cat("metaRangeSimulation object\n")
            print_info <- self$species_names()
            cat("Fields: \n")
            cat("  $ID\n")
            cat("  $globals\n")
            cat("  $environment\n")
            cat("  $number_time_steps\n")
            cat("  $time_step_layer\n")
            cat("  $current_time_step\n")
            cat("  $queue\n")
            cat("  $processes\n")
            cat("  $seed\n")
            if (length(print_info) > 0) {
                cat("Species: \n")
                for (i in seq_along(print_info)) {
                    cat("  $", print_info[i], "\n", sep = "")
                }
            } else {
                cat("Species: none\n")
            }

            cat("Methods: \n")
            cat("  $species_names()\n")
            cat("  $add_globals()\n")
            cat("  $add_species()\n")
            cat("  $add_traits()\n")
            cat("  $add_process()\n")
            cat("  $begin()\n")
            cat("  $exit()\n")
            cat("  $set_current_time_step()\n")
            cat("  $set_time_layer_mapping()\n")
            cat("  $print()\n")
            cat("  $summary()\n")
            return(invisible(self))
        },
        #' @description Summarizes information about the simulation and outputs
        #' it to the console
        #' @examples
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sim$summary()
        #' @return invisible self
        summary = function() {
            # TODO: pretty print
            cat("ID:", self$ID, "\n")
            cat("Environment: \n")
            self$environment$print()
            cat("Time step layer mapping: ", self$time_step_layer, "\n")
            cat("Current time step: ", private$current_time_step, "\n")
            cat("Seed: ", self$seed, "\n")
            cat("Species:\n", paste(self$species_names(), collapse = ", "), "\n")
            cat("Queue:\n")
            self$queue$print()
            return(invisible(self))
        }
    ),
    private = list(
        # ---------- private fields ------------
        # @field continue_execution `<boolean>` if `TRUE` the simulation should continue
        # to run. If `FALSE` the simulation should stop.
        continue_execution = TRUE,

        # @field process_num `<integer>` counter for the number of added processes.
        # Will be appended to the process name to assure uniqueness in the odd case that the random number
        # generator produces the same number.
        process_num = 0L,

        # @description Set current time step
        # @param time_step `<integer>` The current time step of the simulation
        # @return invisible self
        set_current_time_step = function(time_step) {
            self$number_time_steps <- checkmate::assert_int(
                x = self$number_time_steps,
                lower = 1L,
                null.ok = FALSE,
                coerce = TRUE
            )
            if (self$number_time_steps != length(self$time_step_layer)) {
                stop(
                    "number_time_steps [",
                    self$number_time_steps,
                    "]  != length(time_step_layer)[",
                    length(self$time_step_layer),
                    "]."
                )
            }
            private$current_time_step <- checkmate::assert_int(
                x = time_step,
                lower = 1L,
                upper = self$number_time_steps,
                null.ok = FALSE,
                coerce = TRUE
            )
            return(invisible(self))
        },

        # @description Advance to the next time step
        # @return `TRUE` if advanced, `FALSE` if not
        # In that case the simulation is finished
        next_time_step = function() {
            if (private$current_time_step >= self$number_time_steps) {
                return(FALSE)
            }
            private$set_current_time_step(private$current_time_step + 1L)
            return(TRUE)
        },

        # @description Set the environment of a simulation
        # @param sds `<SpatRasterDataset>` created by [terra::sds()] that represents the environment.
        # The individual data set represent different environmental variables and the number of layers
        # represent the different timesteps of the simulation.
        # @return invisible self
        set_sim_environment = function(sds = NULL) {
            checkmate::assert_class(sds, "SpatRasterDataset")
            self$environment <- metaRangeEnvironment$new(sourceSDS = sds)
            self$set_time_layer_mapping(seq_len(min(terra::nlyr(self$environment$sourceSDS))))

            if (getOption("metaRange.verbose", default = FALSE)) {
                message("added environment")
                message(show(sds), appendLF = FALSE)
            }
            return(invisible(self))
        },
        validate = function() {
            for (sp_name in self$species_names()) {
                pr_names <- names(self[[sp_name]]$processes)
                for (pr_name in pr_names) {
                    checkmate::assert_class(self[[sp_name]]$processes[[pr_name]], "metaRangeProcess")
                }
                traits_names <- names(self[[sp_name]]$traits)
                for (t_name in traits_names) {
                    checkmate::assert_atomic(self[[sp_name]]$traits[[t_name]])
                }
            }
            for (pr_name in names(self$processes)) {
                checkmate::assert_class(self$processes[[pr_name]], "metaRangeProcess")
            }
            checkmate::assert_class(self$environment, "metaRangeEnvironment")
            checkmate::assert_class(self$queue, "metaRangePriorityQueue")
            checkmate::assert_class(self$globals, "list")
            for (g in names(self$globals)) {
                checkmate::assert_atomic(self$globals[[g]])
            }
            checkmate::assert_integerish(self$number_time_steps, lower = 1L)
            checkmate::assert_integerish(
                self$time_step_layer,
                lower = 1L,
                upper = min(terra::nlyr(self$environment$sourceSDS))
            )
            checkmate::assert_integerish(private$current_time_step, lower = 1L, upper = self$number_time_steps)
        }
    )
)