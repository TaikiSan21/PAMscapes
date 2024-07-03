#' @title Match GFS Environmental Data
#'
#' @description Downloads and matches wind and precipitation data
#'   from the Global Forecast System (GFS) weather model. Data is
#'   downloaded from the National Center for Atmospheric Research
#'   data server \url{https://rda.ucar.edu/datasets/ds084.1/}.
#'   The particular GFS dataset downloaded is the closest "forecast"
#'   dataset to the particular time (e.g. .f000 or .f003)
#'
#' @param x a dataframe with columns \code{UTC}, \code{Latitude} and
#'   \code{Longitude} to add environmental data to
#'
#' @return a dataframe with wind (m/s) and precipitation rate (kg/m^2/s) columns added
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' # API response may be slow for this example
#' \donttest{
#' gps <- data.frame(Latitude=c(33.2, 33.5,33.6),
#'                   Longitude=c(-118.1, -118.4, -119),
#'                   UTC=as.POSIXct(
#'                     c('2022-04-28 05:00:00',
#'                       '2022-04-28 10:00:00',
#'                       '2022-04-28 20:00:00'), tz='UTC'))
#' gps <- matchGFS(gps)
#' }
#'
#' @importFrom PAMmisc ncToData
#' @importFrom lubridate round_date
#'
#' @export
#'
matchGFS <- function(x) {
    needCols <- c('UTC', 'Latitude', 'Longitude')
    if(!all(needCols %in% colnames(x))) {
        stop('"x" must have columns "UTC", "Longitude", and "Latitude"')
    }
    # ranges of GFS data from UCAR
    minTime <- as.POSIXct('2015-01-15 00:00:00', tz='UTC')
    maxTime <- nowUTC() - 36*3600
    origCols <- colnames(x)
    splitDf <- split(x, round_date(x$UTC, unit='3hour'))

    splitDf <- lapply(splitDf, function(df) {
        if(round_date(df$UTC[1], unit='3hour') < minTime ||
           round_date(df$UTC[1], unit='3hour') > maxTime) {
            warning('Date ', df$UTC[1], ' out of range of UCAR dataset')
            df$windU <- NA
            df$windV <- NA
            df$precRate <- NA
            df$matchLong_mean <- NA
            df$matchLat_mean <- NA
            df$matchTime_mean <- NA
            return(df)
        }
        base <- 'https://thredds.rda.ucar.edu/thredds/ncss/grid/files/g/ds084.1/'
        url <- formatURL_GFS(df, base=base)
        vars <- url$vars
        file <- fileNameManager()
        dl <- GET(url$url, write_disk(file, overwrite = TRUE), progress())
        on.exit({
          tempCache <- getTempCacheDir(create=FALSE)
          # tempFiles <- list.files(tempCache, full.names=TRUE)
          # unlink(tempFiles, force=TRUE)
          unlink(tempCache, force=TRUE, recursive=TRUE)
        })
        if(dl$status_code != 200) {
            warning('URL ', url$url, ' is invalid, pasting this into a browser may give more information.')
            df$windU <- NA
            df$windV <- NA
            df$precRate <- NA
            df$matchLong_mean <- NA
            df$matchLat_mean <- NA
            df$matchTime_mean <- NA
            return(df)
        }
        df <- ncToData(df, file, var=vars, progress=FALSE, verbose=FALSE)
        vars <- paste0(vars, '_mean')
        df$windU <- df[[vars[1]]]
        df$windV <- df[[vars[2]]]
        df$precRate <- df[[vars[3]]]
        df[unique(c(origCols, 'windU', 'windV', 'precRate', 'matchLong_mean', 'matchLat_mean', 'matchTime_mean'))]
    })
    bind_rows(splitDf)
}

formatURL_GFS <- function(range, date=NULL, base='https://thredds.rda.ucar.edu/thredds/ncss/grid/files/g/ds084.1/') {
    if(is.data.frame(range)) {
        if(is.null(date)) {
            date <- range$UTC[1]
        }
        range <- list(Longitude=range(range[['Longitude']]),
                      Latitude=range(range[['Latitude']]))
    }
    for(c in c('Latitude', 'Longitude')) {
        if(range[[c]][1] == range[[c]][2]) {
            range[[c]] <- range[[c]] + c(-.001, .001)
        }
        range[[c]] <- round(range[[c]], 3)
    }
    range <- to180(range)

    date3 <- round_date(date, unit='3hour')
    # if(length(date3) == 1) {
    #     date3 <- rep(date3, 2)
    # }
    if(hour(date3) %in% c(3,9,15,21)) {
        date6 <- date3 - 3 * 3600
    } else {
        date6 <- date3
    }
    # date6 <- round_date(date, unit='6hour')
    f <- ifelse(hour(date3)[1] == hour(date6)[1], 'f000', 'f003')
    # isTP <- grepl('TP', base)
    datePart <-  paste0(format(date6[1], format='%Y/%Y%m%d/gfs.0p25.%Y%m%d%H.'),
                        f,
                        '.grib2?')
    vars <- c('u-component_of_wind_height_above_ground',
              'v-component_of_wind_height_above_ground')
    precAvg <-
        precVar <- ifelse(f == 'f000', 'Precipitation_rate_surface',
                          'Precipitation_rate_surface_3_Hour_Average')
    vars <- c(vars, precVar)
    varPart <- paste0('var=', vars, '&', collapse='')

    llPart <- paste0('north=', range[['Latitude']][[2]],
                     '&west=', range[['Longitude']][[1]],
                     '&east=', range[['Longitude']][[2]],
                     '&south=', range[['Latitude']][[1]],
                     '&horizStride=1&')
    endPart <- '&&&accept=netcdf4-classic'
    timePart <- paste0('time_start=',format(date3[1], '%Y-%m-%dT%H:%M:%SZ'),
                       '&time_end=', format(date3[1], '%Y-%m-%dT%H:%M:%SZ'))
    url <- paste0(base,
                  datePart,
                  varPart,
                  llPart,
                  timePart,
                  endPart)
    list(url=url, vars=vars)
}
