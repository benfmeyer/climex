#' Title
#'
#' @param year_list list of arrays for each year
#'
#' @returns a single array containing the finished data to be written to netcdf
#'
create_data <- function(year_list) {
    set.seed(13021994)
    random_sequence_for_years <- sample(1:5, 100, replace = T)
    list_of_years <- purrr::map(random_sequence_for_years, ~purrr::pluck(year_list, .x))
    complete_array_of_years <- do.call(abind::abind, list_of_years)
    return(complete_array_of_years)
}
