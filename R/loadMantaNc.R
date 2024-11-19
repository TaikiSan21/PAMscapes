#' @title Load MANTA NetCDF File
#'
#' @description Reads in hybrid millidecade data from a MANTA
#'   NetCDF output file and formats it into the dataframe format
#'   required for use in other PAMscapes functions
#'
#' @param x path to .nc file
#' @param keepQuals quality flag values to keep. Accepts vector of
#'   integers from (1, 2, 3, 4) corresponding to flag labels "Good",
#'   "Not evaluated/Unknown", "Compromised/Questionable", and "Unusable/Bad".
#'   HMD levels for points with data quality flags outside of \code{keepQuals}
#'   will be marked as \code{NA}.
#' @param keepEffort if \code{TRUE} or \code{FALSE}, a logical flag whether or
#'   not to keep the effort information with the outputs (number of seconds
#'   per minute). If a numeric value, then any minutes with an effort value
#'   less than \code{keepEffort} will be removed (e.g. \code{50} will remove
#'   minutes with less than 50 seconds of effort)
#'
#' @return a dataframe with first column UTC and other columns
#'   named HMD_Frequency
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' # no sample NetCDF provided (too large)
#' \donttest{
#' manta <- loadMantaNc('MANTA.nc')
#' }
#'
#' @export
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom sf st_coordinates st_as_sf
#'
loadMantaNc <- function(x, keepQuals=c(1), keepEffort=TRUE) {
    if(!file.exists(x)) {
        message('File ', x, ' does not exist.')
        return(NULL)
    }
    nc <- nc_open(x)
    on.exit(nc_close(nc))
    dimNames <- c('time', 'frequency')
    if(!all(dimNames %in% names(nc$dim))) {
        stop('NetCDF file format not recognized, missing expected',
             ' "time" and "frequency" dimensions.')
    }
    dataColOptions <- c('psd', 'sound_pressure_levels')
    hasData <- names(nc$var) %in% dataColOptions
    if(!any(hasData)) {
        stop('NetCDF file format not recognized, missing expected',
             ' "psd" or "sound_pressure_levels" variable.')
    }
    if(sum(hasData) > 1) {
        warning('Multiple data columns matched, defaulting to "psd"')
        dataCol <- 'psd'
    } else {
        dataCol <- names(nc$var)[hasData]
    }
    hmd <- ncvar_get(nc, varid=dataCol, start=c(1, 1), count=c(-1, -1))
    # if qual flags present, replace some HMD with NA
    if('quality_flag' %in% names(nc$var)) {
        quality <- ncvar_get(nc, varid='quality_flag', start=c(1, 1), count=c(-1, -1))
        qTypes <- unique(as.vector(quality))
        if(!all(keepQuals %in% c(1,2,3,4))) {
            warning('"keepQuals" expects values from (1, 2, 3, 4)')
            keepQuals <- keepQuals[keepQuals %in% c(1, 2, 3, 4)]
        }
        # manuscript https://cdn.ioos.noaa.gov/media/2017/12/QARTOD_PassiveAcousticsManual_Final_V1.0_signed.pdf
        # these should be
        # "Pass", "Not Evaulated", "Suspect or High Interest", "Fail", "Missing Data"=9
        dqLevels <- c('Good', 'Not evaluated/Unknown', 'Compromised/Questionable', 'Unusable/Bad')
        dqDrop <- qTypes[!qTypes %in% keepQuals]
        if(length(dqDrop) > 0) {
            message('Data quality flags ', paste0(dqLevels, collapse=', '),
                    ' found in data, corresponding levels marked as NA.')
        }
        dropIx <- matrix(!quality %in% keepQuals, nrow=nrow(quality))
        hmd[dropIx] <- NA
    }
    UTC <- nc$dim$time$vals
    UTC <- ncTimeToPosix(UTC, units=nc$dim$time$units)
    
    freqType <- checkFreqType(nc$dim$frequency$vals)
    hmd <- data.frame(t(hmd))
    # we double round the name to avoid mismatched round-to-even behavior
    labels <- nc$dim$frequency$val
    labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
    labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
    labels <- paste0(freqType, '_', labels)
    colnames(hmd) <- labels
    # colnames(hmd) <- paste0(freqType, '_', round(round(nc$dim$frequency$vals, 3), 1))
    hmLevs <- getHmdLevels(freqRange=range(nc$dim$frequency$vals))
    mismatch <- !names(hmd) %in% hmLevs$labels
    hmd <- cbind(UTC, hmd)
    if(!isFALSE(keepEffort) &&
       !'effort' %in% names(nc$var)) {
        warning('No effort data found in netCDF')
        keepEffort <- FALSE
    }
    if(!isFALSE(keepEffort)) {
        hmd$effortSeconds <- ncvar_get(nc, varid='effort', start=1, count=-1)
    }
    if(is.numeric(keepEffort)) {
        hmd <- hmd[hmd$effortSeconds >= keepEffort, ]
    }
    coords <- ncatt_get(nc, varid=0, attname='geospatial_bounds')
    if(isTRUE(coords$hasatt)) {
        coordVals <- st_coordinates(
            st_as_sf(
                data.frame(geometry=coords$value),
                wkt='geometry'
            )
        )
        if(length(coordVals) == 2) {
            hmd$Latitude <- coordVals[1]
            hmd$Longitude <- coordVals[2]
        }
    }
    platform <- ncatt_get(nc, varid=0, 'platform')
    if(isTRUE(platform$hasatt)) {
        hmd$platform <- platform$value
    }
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
