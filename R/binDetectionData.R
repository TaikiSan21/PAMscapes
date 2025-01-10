#' @title Bin Detection Data to Time Bins
#' 
#' @description Transforms detection data to presence-type data with user
#'   specified time bin (e.g. hourly or daily presence). 
#'   
#' @param x dataframe of deteciton data
#' @param bin the amount time to bin by, must be a character of the form
#'   \code{"#unit"} or \code{"unit"} e.g. "2hour" or "day"
#' @param columns names of the columns in \code{x} that define which rows
#'   should still be considered distinct even if their times are in the same
#'   bin. For example, two calls from the same species in one hour should result
#'   in one row of hourly presence, but two calls from different species in one 
#'   hour should result in two separate rows of hourly presence.
#' @param rematchGPS logical flag, if \code{TRUE} then if columns Longitude
#'   and Latitude are present in \code{x} then they will be rematched to
#'   the outputs. Note that this is imprecise - the time used for rematching
#'   the outputs is the center of each output time bin.
#' @param gpsGroup the name of the column in \code{x} that denotes different
#'   GPS groupings within the data, usually something like "site" or
#'   "deployment." Not needed if all data are from the same location.
#' @param combineYears logical flag whether or not to combine all years to
#'   a single year. Year of the UTC column will be set to 2020 because this
#'   is a leap year, then end values will add the appropriate bin amount to
#'   that. This should allow for roughly accurate behavior around leap day 
#'   detections
#'   
#' @return a dataframe where each row represents detection presence of
#'   one time unit
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @examples 
#' dets <- data.frame(
#'    UTC = as.POSIXct(c('2020-04-04 12:20:00', '2020-04-04 12:40:00', '2020-04-04 13:20:00')),
#'    species = c('whale', 'whale', 'dolphin'),
#'    call = c('a', 'b', 'c'))
#' # two rows of outputs
#' binDetectionData(dets, bin='1hour', columns='species')
#' # adding "call" creates 3 rows of outputs
#' binDetectionData(dets, bin='1hour', columns=c('species', 'call'))
#' 
#' @importFrom lubridate floor_date `year<-`
#' @importFrom dplyr arrange select any_of
#' @importFrom stats approx
#' 
#' @export
#' 
binDetectionData <- function(x, 
                             bin,
                             columns=c('species', 'project'), 
                             rematchGPS=TRUE,
                             gpsGroup=NULL,
                             combineYears=FALSE) {
    columns <- columns[columns %in% colnames(x)]
    if(length(columns) == 0) {
        warning('None of the columns ', 
                paste0(columns, collapse=', '),
                ' were present in data')
        return(NULL)
    }
    if(!all(c('Latitude', 'Longitude') %in% colnames(x))) {
        rematchGPS <- FALSE
    }
    if(isTRUE(rematchGPS)) {
        gpsCols <- c('UTC', 'Latitude', 'Longitude')
        # need extra columns for grouping later e.g. by deployment
        if(!is.null(gpsGroup) && 
           !gpsGroup %in% columns) {
            warning('"gpsGroup" must also be included in "columns"')
            columns <- c(columns, gpsGroup)
        }
        gps <- distinct(x[c(gpsCols, gpsGroup)])
    }
    
    thisPeriod <- unitToPeriod(bin)
    x <- select(x, any_of(c('UTC', 'end', 'detectionType', columns)))
    x$UTC <- floor_date(x$UTC, unit=bin)
    # weekBin <- as.numeric(thisPeriod)/86400 == 7
    # if(isTRUE(weekBin)) {
    #     x$UTC <- lubridate::isoweek(x$UTC)
    # }
    # binSplitIx <- numeric(0)
    rowsIn <- nrow(x)
    if('end' %in% colnames(x) &&
       any(!is.na(x$end))) {
        # spread each row if cover multiple bins
        # if(old) {
        #     x <- bind_rows(lapply(1:nrow(x), function(i) {
        #         if(is.na(x$end[i])) {
        #             return(x[i, ])
        #         }
        #         floorEnd <- floor_date(x$end[i], unit=bin)
        #         # if((as.numeric(difftime(floorEnd, x$UTC[i], units='secs')) > thisPeriod) &&
        #         #    x$detectionType[i] == 'presence') {
        #         #     binSplitIx <<- c(binSplitIx, i)
        #         # }
        #         # if(isTRUE(weekBin)) {
        #         #     floorEnd <- lubridate::isoweek(floorEnd)
        #         # }
        #         if(floorEnd == x$end[i]) {
        #             floorEnd <- floorEnd - .01
        #         }
        #         dates <- seq(from=x$UTC[i], to=floorEnd, by=bin)
        #         # dates <- seq(from=x$UTC[i], to=floorEnd, by=as.numeric(thisPeriod))
        #         if(length(dates) > 1 &&
        #            x$detectionType[i] == 'presence') {
        #             binSplitIx <<- c(binSplitIx, i)
        #         }
        #         result <- as.list(x[i, ])
        #         result$UTC <- dates
        #         result
        #     }))
        # } else {
        naEnd <- is.na(x$end)
        floorEnd <- floor_date(x$end[!naEnd], unit=bin)
        sameFloor <- floorEnd == x$end[!naEnd]
        floorEnd[sameFloor] <- floorEnd[sameFloor] - .01
        dateSeq <- as.list(x$UTC)
        newSeqs <- mapply(function(x, y) {
            seq(from=x, to=y, by=bin)
        }, x$UTC[!naEnd], floorEnd, SIMPLIFY =FALSE)
        dateSeq[!naEnd] <- newSeqs
        nDates <- sapply(dateSeq, length)
        checkMislead <- (nDates > 1) & (x$detectionType == 'presence')
        dupeSeq <- unlist(sapply(seq_along(nDates), function(x) {
            rep(x, each=nDates[x])
        }))
        x <- x[dupeSeq, ]
        x$UTC <- as.POSIXct(unlist(dateSeq), origin='1970-01-01 00:00:00', tz='UTC')
        x$end <- NULL
        if(any(checkMislead)) {
            warning(sum(checkMislead), ' out of ', rowsIn,
                    ' input rows of type "presence" were spread across multiple output rows,',
                    ' results may be misleading.')
        }
    }
    x$end <- x$UTC + thisPeriod
    x$detectionType <- 'presence'
    x <- distinct(x)
    if(isTRUE(rematchGPS)) {
        #split by cols then go
        x$ORIGIX <- 1:nrow(x)
        if(is.null(gpsGroup)) {
            x$TEMPGPSGROUP <- 1
            gpsGroup <- 'TEMPGPSGROUP'
            gps$TEMPGPSGROUP <- 1
        }
        halfPer <- as.numeric(thisPeriod)/2
        x <- bind_rows(
            lapply(
                split(x, x[[gpsGroup]]), function(s) {
                    if(is.null(s) || nrow(s) == 0) {
                        return(s)
                    }
                    thisGps <- gps[gps[[gpsGroup]] == s[[gpsGroup]][1], ]
                    if(is.null(thisGps) || nrow(thisGps) == 0) {
                        return(s)
                    }
                    naLat <- is.na(thisGps$Latitude)
                    naLong <- is.na(thisGps$Longitude)
                    if(all(naLat) || all(naLong)) {
                        return(s)
                    }
                    thisGps <- distinct(thisGps)
                    if(length(unique(thisGps$UTC)) < nrow(thisGps)) {
                        # browser()
                    }
                    # match to center of bin
                    s$Longitude <- approx(x=thisGps$UTC, y=thisGps$Longitude, xout=s$UTC + halfPer)$y
                    s$Latitude <- approx(x=thisGps$UTC, y=thisGps$Latitude, xout=s$UTC + halfPer)$y
                    s
                }
            )
        )
        x <- arrange(x, x$ORIGIX)
        x$ORIGIX <- NULL
        if('TEMPGPSGROUP' %in% names(x)) {
            x$TEMPGPSGROUP <- NULL
        }
    }
    if(isTRUE(combineYears)) {
        year(x$UTC) <- 2020
        x$end <- x$UTC + thisPeriod
    }
    x
}
