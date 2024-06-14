sim_manager <- metaRangeParallel$new()
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
test_simulation <- create_simulation(sim_env)
test_simulation$add_species("test_species")
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
generator_a <- poems::Generator$new(
  description = "Test generator",
  decimals = 0,
  inputs = "seed_number",
  outputs = "abundance"
)
generator_a$add_generative_requirements(abundance = "function")

generator_a$add_function_template(
  "abundance",
  function_def = function(params) {
    matrix(params$seed_number, ncol = n, nrow = n)
  },
  call_params = c("seed_number")
)
sim_manager <- metaRangeParallel$new()
sim_manager$simulation_template <- test_simulation
sim_manager$sample_data <- data.frame(
  "temperature_maximum" = n * 1.3 + 273,
  "temperature_optimum" = n * 0.5 + 273,
  "temperature_minimum" = n * -0.3 + 273,
  "precipitation_maximum" = n * 1.3 * 100,
  "precipitation_optimum" = n * 0.5 * 100,
  "precipitation_minimum" = n * 0.2 * 100,
  seed_number = 1000,
  carrying_capacity = 1000,
  reproduction_rate = 0.01
)
sim_manager$generators <- generator_a

# Set model sample
model_clone <- sim_manager$simulation_template$clone()
expect_silent(sim_manager$set_model_sample(model_clone, 1))
expect_equal(model_clone$test_species$traits$abundance, matrix(1000, ncol = n, nrow = n))

# Run
sim_manager$results_dir <- tempdir()
sim_manager$run()
expect_true("simulation_log.txt" %in% list.files(sim_manager$results_dir))
