#' Title
#'
#' @param start vector of timesteps indicating the start of each year
#' @param end vector of timesteps indicating the end of each year
#' @param arl list of arrays containing the data where each list entry is a single timestep
#'
#' @returns a list of arrays where each list entry is a single year
#'
collect_years <- function(start, end, arl) {
    subset <- arl[start:end]
    yearly_list <- do.call(abind::abind, list(subset, along = 3))
    return(yearly_list)
}
