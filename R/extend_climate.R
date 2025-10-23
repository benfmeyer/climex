#' Title
#'
#' @param nc_file file for which data should be extended
#' @param var_name variable name to use
#' @param out_file file name for the resulting extended data
#' @param ext_period how long the data should be extended for
#' @param origin cf compliant unit for time dimension i.e. "days since 1979-01-01 12:00:00"
#'
#' @returns a netcdf file
#'
#' @export
extend_climate <- function(nc_file, var_name, out_file, ext_period = 100, origin = "months since 2100-01-01 12:00:00") {
    
  nc <- ncdf4::nc_open(nc_file)
  var <- ncdf4::ncvar_get(nc, var_name, start = c(1,1,1405))
  nc_template <- get_template(nc, ext_period, origin)
  ncdf4::nc_close(nc)
  
  list_of_single_timesteps <- asplit(var, 3)

  starts <- seq(1,49, by = 12)
  ends <- seq(12, 60, by = 12)

  yearly_list <- purrr::map2(starts, ends, collect_years, list_of_single_timesteps)
  ready_data <- create_data(yearly_list)

  out_nc <- ncdf4::nc_create(out_file, nc_template)
  ncdf4::ncvar_put(out_nc, varid = var_name, ready_data)
  ncdf4::nc_close(out_nc)

  return(out_file)

}
