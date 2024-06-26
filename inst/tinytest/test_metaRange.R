simlength <- 3
n <- 5
# create test environment
temperature <- terra::rast(matrix(rep(1:n, each = n), n, n) + 273.15)
precipitation <- terra::rast(matrix(rep(1:n), n, n) * 100)
habitat <- matrix(1:n^2, n, n) * matrix(n^2:1, n, n)
habitat <- terra::rast(habitat / max(habitat))

temperature <- rep(temperature, simlength)
precipitation <- rep(precipitation, simlength)
habitat <- rep(habitat, simlength)
sim_env <- terra::sds(temperature, precipitation, habitat)
names(sim_env) <- c("temperature", "precipitation", "habitat")

testfun <- function(n, simlength, weighted, sim_env) {

    test_simulation <- create_simulation(sim_env)

    test_simulation$add_species("test_species")
    # Add traits that define the environmental limits
    test_simulation$add_traits(
        species = "test_species",
        population_level = FALSE,
        "temperature_maximum" = n * 1.3 + 273,
        "temperature_optimum" = n * 0.5 + 273,
        "temperature_minimum" = n * -0.3 + 273,
        "precipitation_maximum" = n * 1.3 * 100,
        "precipitation_optimum" = n * 0.5 * 100,
        "precipitation_minimum" = n * 0.2 * 100
    )
    test_simulation$add_traits(
        species = "test_species",
        "suitability" = NA_real_,
        "abundance" = 100,
        "reproduction_rate" = 0.5,
        "carrying_capacity" = 1000,
        "mass" = 1
    )
    # Add a process to calculate the suitability
    test_simulation$add_process(
        species = "test_species",
        process_name = "calculate_general_suitability",
        process_fun = function() {
            self$traits[["suitability"]] <- (
                calculate_suitability(
                    self$traits$temperature_maximum,
                    self$traits$temperature_optimum,
                    self$traits$temperature_minimum,
                    self$sim$environment$current[["temperature"]]) *
                calculate_suitability(
                    self$traits$precipitation_maximum,
                    self$traits$precipitation_optimum,
                    self$traits$precipitation_minimum,
                    self$sim$environment$current[["precipitation"]]) *
                self$sim$environment$current[["habitat"]])
        },
        execution_priority = 1
    )

    test_simulation$add_traits(
        species = "test_species",
        population_level = FALSE,
        "exponent_reproduction_rate" = -1 / 4,
        "exponent_carrying_capacity" = -3 / 4
    )

    test_simulation$add_globals(
        "E_rep" = -0.65,
        "E_carry_cap" = 0.65,
        "k" = 8.617333e-05
    )

    test_simulation$add_traits(
        species = "test_species",
        population_level = FALSE,
        "reproduction_rate_mte_constant" = calculate_normalization_constant(
            parameter_value =       test_simulation$test_species$traits[["reproduction_rate"]][[1]],
            scaling_exponent =      test_simulation$test_species$traits[["exponent_reproduction_rate"]],
            mass =                  test_simulation$test_species$traits[["mass"]][[1]],
            reference_temperature = test_simulation$test_species$traits[["temperature_optimum"]],
            E =                     test_simulation$globals$E_rep,
            k =                     test_simulation$globals$k),
        "carrying_capacity_mte_constant" = calculate_normalization_constant(
            parameter_value =       test_simulation$test_species$traits[["carrying_capacity"]][[1]],
            scaling_exponent =      test_simulation$test_species$traits[["exponent_carrying_capacity"]],
            mass =                  test_simulation$test_species$traits[["mass"]][[1]],
            reference_temperature = test_simulation$test_species$traits[["temperature_optimum"]],
            E =                     test_simulation$globals$E_carry_cap,
            k =                     test_simulation$globals$k)
    )

    # Add a process to apply the metabolic theory of ecology
    test_simulation$add_process(
        species = "test_species",
        process_name = "mte",
        process_fun = function() {
            self$traits[["reproduction_rate"]] <-
                metabolic_scaling(
                    normalization_constant = self$traits[["reproduction_rate_mte_constant"]],
                    scaling_exponent =       self$traits[["exponent_reproduction_rate"]],
                    mass =                   self$traits[["mass"]],
                    temperature =            self$sim$environment$current[["temperature"]],
                    E =                      self$sim$globals$E_rep,
                    k =                      self$sim$globals$k
                )

            self$traits[["carrying_capacity"]] <-
                metabolic_scaling(
                    normalization_constant = self$traits[["carrying_capacity_mte_constant"]],
                    scaling_exponent =       self$traits[["exponent_carrying_capacity"]],
                    mass =                   self$traits[["mass"]],
                    temperature =            self$sim$environment$current[["temperature"]],
                    E =                      self$sim$globals$E_carry_cap,
                    k =                      self$sim$globals$k
                )
        },
        execution_priority = 2
    )

    # Add a process to calculate the reproduction
    test_simulation$add_process(
        species = "test_species",
        process_name = "reproduction",
        process_fun = function() {
            self$traits[["abundance"]] <-
                ricker_reproduction_model(
                    self$traits[["abundance"]],
                    self$traits[["reproduction_rate"]] * self$traits[["suitability"]],
                    self$traits[["carrying_capacity"]] * self$traits[["suitability"]]
                )
        },
        execution_priority = 3
    )
    test_simulation$add_traits(
        species = "test_species",
        "offspring" = 0
    )
    test_simulation$add_traits(
        species = "test_species",
        population_level = FALSE,
        "dispersal_kernel" = calculate_dispersal_kernel(
            max_dispersal_dist = 1,
            kfun = negative_exponential_function,
            mean_dispersal_dist = 0.5)
    )
    # Add a process to calculate the dispersal
    if (weighted) {
        test_simulation$add_process(
            species = "test_species",
            process_name = "dispersal_process",
            process_fun = function() {
                self$traits[["abundance"]] <- dispersal(
                    abundance = self$traits[["abundance"]],
                    weights = self$traits[["suitability"]],
                    dispersal_kernel = self$traits[["dispersal_kernel"]])
            },
            execution_priority = 4
        )
    } else {
        test_simulation$add_process(
            species = "test_species",
            process_name = "dispersal_process",
            process_fun = function() {
                self$traits[["abundance"]] <- dispersal(
                    abundance = self$traits[["abundance"]],
                    dispersal_kernel = self$traits[["dispersal_kernel"]])
            },
            execution_priority = 4
        )
    }

    test_simulation$begin()
    return(ceiling(test_simulation[["test_species"]]$traits[["abundance"]]))
}

expect_equal(
    testfun(n = n, simlength = simlength, weighted = TRUE, sim_env = sim_env),
    matrix(
        c(
            0, 102, 130, 136, 120,
            0, 259, 290, 254, 178,
            0, 347, 361, 288, 185,
            0, 278, 270, 219, 151,
            0, 120, 105,  83,  51
        ),
        n, n
    ),
    info = "testing weighted dispersal"
)

expect_equal(
    testfun(n = n, simlength = simlength, weighted = FALSE, sim_env = sim_env),
    matrix(
        c(
            3, 120, 144, 147, 133,
            6, 230, 264, 234, 177,
            7, 312, 334, 267, 185,
            6, 247, 247, 194, 141,
            4, 128, 123, 104,  89
        ),
        n, n
    ),
    info = "testing unweighted dispersal"
)

testfun_deueue <- function(n, simlength, sim_env) {
    test_simulation <- create_simulation(sim_env)
    test_simulation$add_species(name = "s01")
    test_simulation$add_species(name = "s02")

    # Add traits that define the environmental limits
    test_simulation$add_traits(
        species = c("s01", "s02"),
        population_level = FALSE,
        "suitability" = NA_real_,
        "temperature_maximum" = n * 1.3 + 273,
        "temperature_optimum" = n * 0.5 + 273,
        "temperature_minimum" = n * -0.3 + 273
    )

    # Add a process to calculate the suitability
    test_simulation$add_process(
        species = c("s01", "s02"),
        process_name = "calculate_general_suitability",
        process_fun = function() {
            self$traits[["suitability"]] <- (
                calculate_suitability(
                    self$traits$temperature_maximum,
                    self$traits$temperature_optimum,
                    self$traits$temperature_minimum,
                    self$sim$environment$current[["temperature"]]))
        },
        execution_priority = 1
    )
    test_simulation$add_process(
        species = "s02",
        process_name = "deactivate",
        process_fun = function() {
            if (self$sim$get_current_time_step() == 2) {
                for (i in seq_along(self$processes)) {
                    self$sim$queue$dequeue(
                        PID = self$processes[[i]]$get_PID()
                    )
                }
            }
        },
        execution_priority = 2
    )



    pre_sim_current_queue_is_length_zero <- length(test_simulation$queue$get_queue()) == 0
    pre_sim_current_queue_is_empty <- test_simulation$queue$is_empty()
    pre_sim_future_queue_is_full <- length(test_simulation$queue$get_future_queue()) == 3
    test_simulation$begin()
    after_sim_queue_is_reduced <- length(test_simulation$queue$get_queue()) == 1

    success <- all(
        pre_sim_current_queue_is_length_zero,
        pre_sim_current_queue_is_empty,
        pre_sim_future_queue_is_full,
        after_sim_queue_is_reduced
    )
    return(success)
}
expect_true(testfun_deueue(n = n, simlength = simlength, sim_env = sim_env),
    info = "testing queue & dequeue")

## Test new_clone method 
sim <- create_simulation(sim_env)
sim$add_species("test_species")
copy <- sim$new_clone()

expect_true("test_species" %in% copy$species_names())
expect_inherits(copy, "metaRangeSimulation")

sim$add_globals(a = 1, b = 2) 
copy <- sim$new_clone()

expect_equal(copy$globals$a, 1) 
expect_equal(copy$globals$b, 2)

sim$add_globals(c = "c")

expect_null(copy$globals$c)

sim$add_process(
    process_name = "global_process",
    process_fun = function() {
        message("process_2")
    },
    execution_priority = 2
)
copy <- sim$new_clone()

expect_true("global_process" %in% names(copy$processes))

sim$add_process("test_species", 
                "species_process_1", 
                function() {
                    message("process_1")
                }, 
                1) 
copy <- sim$new_clone() 

expect_true("species_process_1" %in% names(copy$test_species$processes))

sim$add_traits("test_species", population_level = TRUE, a = 1) 
sim$add_traits("test_species", population_level = FALSE, b = 2, c = "c") 
copy <- sim$new_clone() 

expect_equal(copy$test_species$traits$a, matrix(1, nrow = 5, ncol = 5)) 
expect_equal(copy$test_species$traits$b, 2) 
expect_equal(copy$test_species$traits$c, "c")

sim$add_traits("test_species", population_level = FALSE, d = FALSE)

expect_null(copy$test_species$traits$d)

rm(testfun, testfun_deueue, sim_env, n, simlength, temperature, precipitation, habitat)
