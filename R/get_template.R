#' Title
#'
#' @param nc the netcdf file read in with nc_open
#' @param ext_period integer, years to extend data by
#' @param origin cf compliant unit for time dimension i.e. "days since 1979-01-01 12:00:00"
#'
#' @returns a variable template to create a new netcdf file with the same structure 
#'
get_template <- function(nc, ext_period, origin) {
  
  set_dims <- function(d) {
    ncdf4::ncdim_def(
      name = d$name,
      units = d$units,
      vals = d$vals,
      create_dimvar = TRUE
    )
  }

  set_vars <- function(v) {
    if(any(!is.finite(v$dimids)))
      return(NULL)
  
    ncdf4::ncvar_def(
      name = v$name,
      units = v$units,
      dim = dims[v$dimids + 1],   # +1 since R uses 1-based indexing
      missval = v$missval,
      longname = v$longname,
      prec = v$prec
    )
  }

  dims <- lapply(nc$dim, set_dims)
  dims$time$units <- origin
  timestep <- stringr::str_extract(origin, "months||days")
  ts_key <- c("months" = 12, "days" = 365)
  dims$time$len <- ext_period * ts_key[[timestep]]
  dims$time$vals <- 0:(dims$time$len - 1) 

  vars <- lapply(nc$var, set_vars) |> 
    purrr::compact()

  return(vars)
}
