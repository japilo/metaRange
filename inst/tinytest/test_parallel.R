## Test initialization

sim_template <- metaRangeSimulation$new()
sample_data <- data.frame(
  "temperature_maximum" = c(1, 2, 3),
  "temperature_optimum" = c(4, 5, 6),
  "temperature_minimum" = c(7, 8, 9)
)
results_dir <- tempdir()

parallel_obj <- metaRangeParallel$new(
  simulation_template = sim_template,
  sample_data = sample_data,
  results_dir = results_dir
)

expect_equal(parallel_obj$simulation_template, sim_template)
expect_equal(parallel_obj$sample_data, sample_data)
expect_equal(parallel_obj$results_dir, results_dir)

## Test set_model_sample and run methods

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

simulator <- poems::SimulatorReference$new()
region <- poems::Region$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
conductance_raster <- raster::stack(replicate(10,+(region$region_raster > 0)))
conductance_raster[[2]][11] <- 0
dispersal_friction = poems::DispersalFriction$new(region = region,
                                            conductance = conductance_raster)
dispersal_gen1 <- poems::DispersalGenerator$new(
  dispersal_friction = dispersal_friction,
  dispersal_proportion = 0.6,
  dispersal_breadth = 110,
  inputs = c("dispersal_max_distance"),
  distance_scale = 1000,
  distance_classes = seq(100, 400, 20)
)
dispersal_gen1$calculate_distance_data()

dispersal_gen2 <- poems::DispersalGenerator$new(
  dispersal_friction = dispersal_friction,
  dispersal_proportion = 0.4,
  dispersal_breadth = 110,
  inputs = c("dispersal_max_distance"),
  distance_scale = 1000,
  distance_classes = seq(100, 400, 20)
)
dispersal_gen2$calculate_distance_data()

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
  reproduction_rate = 0.01,
  dispersal_max_distance = 500
)
sim_manager$generators <- list(dispersal_gen1, dispersal_gen2, generator_a)

# Set model sample
model_clone <- sim_manager$simulation_template$new_clone()
expect_silent(sim_manager$set_model_sample(model_clone, 1))
expect_equal(model_clone$test_species$traits$abundance, matrix(1000, ncol = n, nrow = n))
expect_equal(model_clone$globals$results_dir, file.path(sim_manager$results_dir, "simulation1"))
expect_equal(model_clone$test_species$traits$carrying_capacity, 1000)
expect_true("dispersal1" %in% names(model_clone$test_species$traits))

# Run
sim_manager$results_dir <- tempdir()
sim_manager$run()
expect_true("simulation_log.txt" %in% list.files(sim_manager$results_dir))


## Test get_message_sample method
parallel_obj <- metaRangeParallel$new()

status_message <- "Model %s simulation ran successfully"
sample_index <- 1

message <- parallel_obj$get_message_sample(status_message, sample_index)

expect_equal(message, "Model sample 1 simulation ran successfully")

## Test log_simulation method
parallel_obj <- metaRangeParallel$new()

simulation_log <- list(
  list(successful = TRUE, message = "Simulation 1 ran successfully"),
  list(successful = FALSE, message = "Simulation 2 ran unsuccessfully with errors", errors = c("Error 1", "Error 2")),
  list(successful = TRUE, message = "Simulation 3 ran successfully")
)

expected_log <- list(
  summary = "2 of 3 sample models ran and saved results successfully",
  failed_indices = c(2),
  warning_indices = NULL,
  full_log = simulation_log
)

log <- parallel_obj$log_simulation(simulation_log)

expect_equal(log$summary, expected_log$summary)
expect_equal(log$failed_indices, expected_log$failed_indices)
expect_equal(log$warning_indices, expected_log$warning_indices)
expect_equal(log$full_log, expected_log$full_log)