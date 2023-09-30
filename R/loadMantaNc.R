#' @title Load MANTA NetCDF File
#'
#' @description Reads in hybrid millidecade data from a MANTA
#'   NetCDF output file and formats it into the dataframe format
#'   required for use in other PAMscapes functions
#'
#' @param x path to .nc file
#'
#' @return a dataframe with first column UTC and other columns
#'   named HMD_Frequency
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' # no sample NetCDF provided (too large)
#' \dontrun{
#' manta <- loadMantaNc('MANTA.nc')
#' }
#'
#' @export
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#'
loadMantaNc <- function(x) {
    nc <- nc_open(x)
    on.exit(nc_close(nc))
    dimNames <- c('time', 'frequency')
    if(!all(dimNames %in% names(nc$dim))) {
        stop('NetCDF file format not recognized, missing expected',
             ' "time" and "frequency" dimensions.')
    }
    if(!'psd' %in% names(nc$var)) {
        stop('NetCDF file format not recognized, missing expected',
             ' "psd" variable.')
    }
    hmd <- ncvar_get(nc, varid='psd', start=c(1, 1), count=c(-1, -1))
    UTC <- nc$dim$time$vals
    UTC <- ncTimeToPosix(UTC, units=nc$dim$time$units)
    hmd <- data.frame(t(hmd))
    colnames(hmd) <- paste0('HMD_', nc$dim$frequency$vals)
    hmd <- cbind(UTC, hmd)
    hmd
}

#' @importFrom lubridate parse_date_time
#'
ncTimeToPosix <- function(vals, units) {
    # if sending the whole dimension extract bits
    if(is.list(vals) &&
       all(c('units', 'vals') %in% names(vals))) {
        units <- vals$units
        vals <- vals$vals
    }
    # if(is.na(vals)) {
    #     return(vals)
    # }
    isNa <- is.na(vals)

    if(grepl('hours? since', units, ignore.case=TRUE)) {
        or <- gsub('hours? since ', '', units, ignore.case=TRUE)
        or <- gsub('\\s{0,1}UTC', '', or)
        or <- gsub('\\.0+$', '', or)
        or <- ymd_hms_fast(or)
        out <- as.POSIXct(vals * 3600, origin=or, tz='UTC')
        if(anyNA(out[!isNa])) {
            warning('Conversion failed for units ', units)
        }
        return(out)
    }
    # if(units == 'hours since 2000-01-01 00:00:00') {
    #     return(as.POSIXct(vals * 3600, origin = '2000-01-01 00:00:00', tz='UTC'))
    # }
    if(grepl('seconds? since', units, ignore.case=TRUE)) {
        or <- gsub('seconds? since ', '', units, ignore.case=TRUE)
        or <- gsub('\\s{0,1}UTC', '', or)
        or <- gsub('\\.0+$', '', or)
        or <- ymd_hms_fast(or)
        out <- as.POSIXct(vals, origin=or, tz='UTC')
        if(anyNA(out[!isNa])) {
            warning('Conversion failed for units ', units)
        }
        return(out)
    }
    # if(units == 'seconds since 1970-01-01T00:00:00Z') {
    #     return( as.POSIXct(vals, origin = '1970-01-01 00:00:00', tz='UTC'))
    # }
    # if(units == 'seconds since 1981-01-01 00:00:00') {
    #     return( as.POSIXct(vals, origin = '1981-01-01 00:00:00', tz='UTC'))
    # }
    if(units == 'count') {
        return(vals)
    }
    if(units == 'posix') {
        return(vals)
    }
    # if(units == 'hours since 1950-01-01') {
    #     return(as.POSIXct(vals * 3600, origin = '1950-01-01', tz='UTC'))
    # }
    stop('Dont know how to deal with time with units ', units)
}

ymd_hms_fast <- function(x) {
    ords <- c('%Y-%m-%d %H:%M:%S',
              '%Y-%m-%dT%H:%M:%SZ',
              '%Y/%m/%d %H:%M:%S',
              '%Y-%m-%dT%H:%M:%S')
    parse_date_time(x, orders=ords, truncated=3, exact=TRUE)
}