# Author: Stefan Fallert
# Date: 15.06.2023
# License: GPL-3 (See License.md)

#' Save function
#'
#' Saves the specified traits of a [metaRangeSpecies] object.
#'
#' @param x `<metaRangeSpecies>` [metaRangeSpecies] object.
#' @param traits `<character>` NULL or a character vector specifying the trait to save.
#' If NULL, all traits are saved.
#' @param prefix `<string>` prefix for the file names or NULL.
#' @param path `<string>`path to the directory where the files are saved.
#' @param overwrite `<boolean>` overwrite existing files.
#' @param ... additional arguments passed to [terra::writeRaster].
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' test_sim$add_traits("species_01", trait_01 = matrix(1, nrow = 2, ncol = 2))
#' res_path <- save_species(
#'     test_sim$species_01,
#'     traits = "trait_01",
#'     prefix = basename(tempfile()),
#'     path = tempdir()
#' )
#' # the following should be TRUE
#' # but might fail due to floating point errors (that's why we round the values)
#' identical(
#'     round(terra::as.matrix(terra::rast(res_path), wide = TRUE)),
#'     round(test_sim$species_01$traits[["trait_01"]])
#' )
#'
#' # cleanup
#' unlink(res_path)
#' stopifnot(!file.exists(res_path))
#' @return invisible the paths to the saved files.
#' @export
save_species <- function(x, traits = NULL, prefix = NULL, path, overwrite = FALSE, ...) {
    checkres <- checkmate::check_class(x, "metaRangeSpecies")
    if (!checkmate::test_true(checkres)) {
        warning("Can't save species. Argument 'x' is not a metaRangeSpecies object", call. = TRUE)
        return()
    }
    checkres <- checkmate::check_character(traits, null.ok = TRUE)
    if (!checkmate::test_true(checkres)) {
        warning("Can't save species. Argument 'traits' is not a character vector or NULL", call. = TRUE)
        return()
    }
    checkres <- checkmate::check_string(prefix, null.ok = TRUE)
    if (!checkmate::test_true(checkres)) {
        warning("Can't save species. Argument 'prefix' is not a character vector or NULL", call. = TRUE)
        prefix <- NULL
    }
    checkres <- checkmate::check_flag(overwrite)
    if (!checkmate::test_true(checkres)) {
        warning("Argument 'overwrite' is not a boolean. Assuming FALSE", call. = TRUE)
        overwrite <- FALSE
    }
    if (is.null(traits)) {
        traits <- names(x[["traits"]])
    }
    return_paths <- c()
    for (att in traits) {
        if (is.null(x$traits[[att]])) {
            warning(att, " is not an trait of species: ", x$name, call. = TRUE)
            next
        }
        if (inherits(x$traits[[att]], "SpatRaster")) {
            full_path <- file.path(path, paste0(prefix, x$name, "_", att, ".tif"))
            checkres <- checkmate::check_path_for_output(full_path, overwrite = overwrite)
            if (!checkmate::test_true(checkres)) {
                warning("Can't save ", att, ". ", checkres, call. = TRUE)
            }
            terra::writeRaster(x$traits[[att]], full_path, overwrite = overwrite, ...)
        } else if (inherits(x$traits[[att]], "matrix")) {
            dim_m <- dim(x$traits[[att]])[c(1, 2)]
            dim_r <- dim(x$sim$environment$sourceSDS)[c(1, 2)]

            if (!is.null(dim_m) && all(dim_m == dim_r)) {
                r <- terra::rast(
                    x$sim$environment$sourceSDS[[1]],
                    nlyrs = 1,
                    vals = x$traits[[att]]
                )
            } else {
                r <- terra::rast(x$traits[[att]])
            }
            full_path <- file.path(path, paste0(prefix, x$name, "_", att, ".tif"))
            checkres <- checkmate::check_path_for_output(full_path, overwrite = overwrite)
            if (!checkmate::test_true(checkres)) {
                warning("Can't save ", att, ". ", checkres, call. = TRUE)
            }
            terra::writeRaster(r, full_path, overwrite = overwrite, ...)
        } else {
            full_path <- file.path(path, paste0(prefix, x$name, "_", att, ".csv"))
            checkres <- checkmate::check_path_for_output(full_path, overwrite = overwrite)
            if (!checkmate::test_true(checkres)) {
                warning("Can't save ", att, ". ", checkres, call. = TRUE)
            }
            write.csv(x$traits[[att]], full_path, row.names = FALSE)
        }
        return_paths <- c(return_paths, full_path)
    }
    return(invisible(return_paths))
}