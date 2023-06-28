#gfs funcs
library(httr)
library(lubridate)
library(dplyr)
library(PAMmisc)

matchGFS <- function(df, base='https://rda.ucar.edu/thredds/ncss/grid/files/g/ds084.1/') {
    # ranges of GFS data from UCAR
    minTime <- as.POSIXct('2015-01-15 00:00:00', tz='UTC')
    maxTime <- PAMmisc:::nowUTC() - 36*3600
    origCols <- colnames(df)
    splitDf <- split(df, round_date(df$UTC, unit='3hour'))
    splitDf <- lapply(splitDf, function(x) {
        if(round_date(x$UTC[1], unit='3hour') < minTime ||
           round_date(x$UTC[1], unit='3hour') > maxTime) {
            warning('Date ', x$UTC[1], ' out of range of UCAR dataset')
            x$windU <- NA
            x$windV <- NA
            x$precRate <- NA
            x$matchLong_mean <- NA
            x$matchLat_mean <- NA
            x$matchTime_mean <- NA
            return(x)
        }
        url <- formatURL_GFS(x)
        vars <- url$vars
        file <- PAMmisc:::fileNameManager()
        dl <- GET(url$url, write_disk(file, overwrite = TRUE), progress())
        if(dl$status_code != 200) {
            warning('URL ', url$url, ' is invalid, pasting this into a browser may give more information.')
            x$windU <- NA
            x$windV <- NA
            x$precRate <- NA
            x$matchLong_mean <- NA
            x$matchLat_mean <- NA
            x$matchTime_mean <- NA
            return(x)
        }
        x <- ncToData(x, file, var=vars, progress=FALSE, verbose=FALSE)
        vars <- paste0(vars, '_mean')
        x$windU <- x[[vars[1]]]
        x$windV <- x[[vars[2]]]
        x$precRate <- x[[vars[3]]]
        x[c(origCols, 'windU', 'windV', 'precRate', 'matchLong_mean', 'matchLat_mean', 'matchTime_mean')]
    })
    bind_rows(splitDf)
}

formatURL_GFS <- function(range, date=NULL, base='https://rda.ucar.edu/thredds/ncss/grid/files/g/ds084.1/') {
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
    range <- PAMmisc:::to180(range)

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