#' @title Format Detection Effort
#' 
#' @description Format effort data for use in other acoustic detection plotting
#'   functions. Time ranges will be marked as either "on" or "off" effort
#'   
#' @param effort dataframe with columns \code{start} or \code{effortStart}
#'   and \code{end} or \code{effortEnd} describing on effort time ranges
#' @param range if not \code{NULL}, the full extent time ranges to consider for
#'   marking off effort times
#' @param resolution if not \code{NULL}, time resolution to round effort start
#'   and end times to. Start times will use \link[lubridate]{floor_date} and
#'   end times will use \link[lubridate]{ceiling_date}, must be a character
#'   that is valid for the \code{unit} argument of those functions
#' @param columns if not \code{NULL}, extra columns to use for differentiating
#'   different types of effort that should be tracked separately (e.g. different
#'   deployment sites or species with different effort)
#' @param combineYears logical flag to combine all years into a single "year"
#' 
#' @return a dataframe with columns \code{start}, \code{end}, and \code{status}
#'   which is either "on" or "off", as well as any columns listed in \code{columns}
#'   
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @export
#' 
#' @importFrom lubridate year floor_date ceiling_date interval union
#' @importFrom lubridate is.interval int_overlaps int_start int_end day month
#'
formatEffort <- function(effort, range=NULL, resolution=NULL, columns=NULL, combineYears=FALSE) {
    if(is.null(effort)) {
        return(NULL)
    }
    if(!is.null(columns)) {
        columns <- columns[columns %in% names(effort)]
        if(length(columns) == 0) {
            columns <- NULL
        }
    }
    if('effortStart' %in% names(effort)) {
        effort <- rename(effort, 'start'='effortStart')
    }
    if('effortEnd' %in% names(effort)) {
        effort <- rename(effort, 'end'='effortEnd')
    }
    if(!all(c('start', 'end') %in% names(effort))) {
        warning('Effort must have columns "start" and "end"')
        return(NULL)
    }
    if(!inherits(effort$start, 'POSIXct')) {
        effort$start <- parseToUTC(effort$start)
    }
    if(!inherits(effort$end, 'POSIXct')) {
        effort$end <- parseToUTC(effort$end)
    }
    # expecting input to only be on-effort times
    if('status' %in% names(effort)) {
        effort <- effort[effort$status == 'on', ]
        effort$status <- NULL
    }
    if(!is.null(resolution)) {
        effort$start <- floor_date(effort$start, unit=resolution)
        effort$end <- ceiling_date(effort$end, unit=resolution)
    }
    selectCols <- unique(c('start', 'end', columns))
    if(isTRUE(combineYears)) {
        diffs <- difftime(effort$end, effort$start, units='secs')
        yearDiff <- year(effort$end) - year(effort$start)
        year(effort$start) <- 2020
        year(effort$end) <- 2020 + yearDiff
        # effort$end <- effort$start + diffs
        fullYear <- year(effort$end) > 2021
        partialYear <- (year(effort$end) == 2021) &
            (effort$end != as.POSIXct('2021-01-01 00:00:00', tz='UTC'))
        if(any(fullYear)) {
            effort$start[fullYear] <- as.POSIXct('2020-01-01 00:00:00', tz='UTC')
            effort$end[fullYear] <- as.POSIXct('2021-01-01 00:00:00', tz='UTC') #- .1
        }
        if(any(partialYear)) {
            newEff <- data.frame(start=as.POSIXct('2020-01-01 00:00:00', tz='UTC'),
                                 end=effort$end[partialYear])
            year(newEff$end) <- 2020
            for(c in columns) {
                newEff[[c]] <- effort[[c]][partialYear]
            }
            effort$end[partialYear] <- as.POSIXct('2021-01-01 00:00:00', tz='UTC') #- .1
            effort <- bind_rows(effort, newEff)
        }
    }
    
    # need to account for NA vals in columns, e.g. so we can specify effort for one
    # species then all others use general effort
    for(c in columns) {
        effort[[c]][is.na(effort[[c]])] <- 'ALLVALUES'
    }
    effort <- distinct(
        select(effort, all_of(selectCols))
    )
    # effort <- arrange(effort, .data$start)
    effort$interval <- interval(effort$start, effort$end)
    # split by columns
    if(!is.null(columns)) {
        splitCols <- lapply(columns, function(x) effort[[x]])
    } else {
        splitCols <- rep(1, nrow(effort))
    }
    if(is.null(range)) {
        range <- c(min(effort$start), max(effort$end))
    }
    if(isTRUE(combineYears)) {
        range <-  as.POSIXct(c('2019-01-01 00:00:00', '2020-01-01 00:00:00'), tz='UTC')
    }
    nDrop <- 0
    nMod <- 0
    effort <- bind_rows(lapply(split(effort, splitCols), function(x) {
        # browser()
        if(nrow(x) == 0) {
            return(x)
        }
        ints <- collapseIntervals(x$interval)
        result <- data.frame(interval=ints)
        for(c in columns) {
            result[[c]] <- x[[c]][1]
        }
        result$start <- int_start(result$interval)
        result$end <- int_end(result$interval)
        result <- result[c('start', 'end', columns, 'interval')]
        result <- arrange(result, .data$start)
        if(isTRUE(combineYears)) {
            start229 <- is229(result$start)
            end229 <- is229(result$end)
            
            result$start[start229] <- as.POSIXct('2020-03-01 00:00:00', tz='UTC')
            result$end[end229] <- as.POSIXct('2020-03-01 00:00:00', tz='UTC')
            diffs <- as.numeric(difftime(result$end, result$start, units='secs'))
            dropBoth <-  diffs <= 0
            start229[dropBoth] <- FALSE
            end229[dropBoth] <- FALSE
            if(any(dropBoth)) {
                
                nDrop <<- nDrop + sum(dropBoth)
                result <- result[!dropBoth, ]
                diffs <- diffs[!dropBoth]
            }
            if(any(start229 | end229)) {
                nMod <<- nMod + sum(start229 | end229)
                
            }
            year(result$start) <- year(result$start) - 1
            year(result$end) <- year(result$end) - 1
            # result$end <- result$start + diffs
        }
        if(nrow(result) == 0) {
            return(NULL)
        }
        result$status <- 'on'
        offs <- NULL
        if(nrow(result) > 1) {
            thisOff <- data.frame(
                start = result$end[1:(nrow(result)-1)],
                end = result$start[2:nrow(result)]
            )
            thisOff$status <- 'off'
            offs <- rbind(offs, thisOff)
        }
        ### ADD CHECK FOR CUTTONG OFF ON EFFORT??
        if(range[1] < min(result$start)) {
            thisOff <- data.frame(start=range[1], end=min(result$start), status='off')
            offs <- rbind(offs, thisOff)
        }
        if(range[2] > max(result$end)) {
            thisOff <- data.frame(start=max(result$end), end=range[2], status='off')
            offs <- rbind(offs, thisOff)
        }
        if(!is.null(offs)) {
            for(c in columns) {
                offs[[c]] <- x[[c]][1]
            }
            offs$interval <- interval(offs$start, offs$end)
            result <- rbind(result, offs)
            result <- arrange(result, .data$start)
        }
        result
    }))
    if(nDrop > 0) {
        warning(nDrop, ' effort entries removed due to leap day (combineYears=TRUE)')
    }
    if(nMod > 0) {
        warning(nMod, ' effort entries modified due to leap day (combineYears=TRUE)')
    }
    effort$interval <- NULL
    effort
}

# combine overlapping intervals into a single using union
collapseIntervals <- function(x) {
    if(!is.interval(x) &&
       is.data.frame(x) &&
       'interval' %in% names(x)) {
        x <- x$interval
    }
    if(length(x) %in% c(0, 1)) {
        return(x)
    }
    for(i in seq_along(x)) {
        doesIntersect <- int_overlaps(x[i], x)
        doesIntersect[i] <- FALSE
        if(any(doesIntersect)) {
            for(j in which(doesIntersect)) {
                x[i] <- lubridate::union(x[i], x[j])
            }
            return(collapseIntervals(x[-which(doesIntersect)]))
        }
    }
    x
}

is229 <- function(x) {
    month(x) == 2 &
        day(x) == 29
}

# this is to deal with ALLVALUES effort assigning all the actual values
# for doing effort joins
# colVals is list(columnName = uniqueVals)
# effort[[columnName]] can be ALLVALUES, so spread to uniqueVals
# problem if have one with a specific value but not also an ALLVALUES
# - if theyve listed only a specific effort but not a general effort
spreadEffort <- function(effort, colVals=NULL, commas=NULL) {
    effort <- distinct(effort)
    for(c in commas) {
        whichCom <- grepl(',', effort[[c]])
        if(!any(whichCom)) {
            next
        }
        newEff <- vector('list', length=max(which(whichCom)))
        for(i in which(whichCom)) {
            spreadVals <- gsub(' ', '', strsplit(effort[[c]][i], ',')[[1]])
            thisEff <- effort[rep(i, length(spreadVals)), ]
            thisEff[[c]] <- spreadVals
            newEff[[i]] <- thisEff
        }
        newEff <- bind_rows(newEff)
        effort <- effort[-which(whichCom), ]
        effort <- bind_rows(effort, newEff)
        row.names(effort) <- NULL
    }
    if(is.null(colVals)) {
        return(effort)
    }
    # if column (e.g species) is missing, fill it with ALL before spreading
    for(n in names(colVals)) {
        if(!n %in% names(effort)) {
            effort[[n]] <- 'ALLVALUES'
        }
    }
    for(c in names(colVals)) {
        otherCols <- names(colVals)[names(colVals) != c]
        whichAll <- effort[[c]] == 'ALLVALUES'
        if(!any(whichAll)) {
            next
        }
        hasVals <- character(0)
        newEff <- vector('list', length=max(which(whichAll)))
        for(i in which(whichAll)) {
            sameGroup <- effort
            for(o in otherCols) {
               sameVal <- effort[[o]][i]
               # jank to make filter below match nothing
               if(sameVal == 'ALLVALUES') {
                   sameVal <- 'DONTMATCHME'
               }
               sameGroup <- sameGroup[sameGroup[[o]] == sameVal, ] 
            }#o
            hasVals <- unique(c(hasVals, sameGroup[[c]]))
            hasVals <- hasVals[hasVals != 'ALLVALUES']
            addVals <- colVals[[c]][!colVals[[c]] %in% hasVals]
            thisEff <- effort[rep(i, length(addVals)), ]
            thisEff[[c]] <- addVals
            newEff[[i]] <- thisEff
        }#i
        newEff <- bind_rows(newEff)
        effort <- effort[-which(whichAll), ]
        effort <- bind_rows(effort, newEff)
    }#c
    if(nrow(effort) == 0) {
        return(effort)
    }
    row.names(effort) <- NULL
    effort
}

# change x to have 0/1 detected or not based on effort
# missing dates are then off-effort
fillEffortZeroes <- function(x, effort=NULL, resolution, columns) {
    if(!'effortDetection' %in% names(x)) {
        x$effortDetection <- 1
    }
    # remove the non-detections before re-adding them later
    x <- x[x$effortDetection != 0, ]
    if(is.null(effort)) {
        return(x)
    }
    if(!'status' %in% names(effort)) {
        effort <- formatEffort(effort, resolution=resolution, columns=columns,
                               range=c(min(x$UTC), max(x$end)))
    }
    effort <- effort[effort$status == 'on', ]
    colVals <- lapply(columns, function(c) unique(x[[c]]))
    names(colVals) <- columns
    effort <- spreadEffort(effort, colVals=colVals)
    splitCols <- lapply(columns, function(c) x[[c]])
    bin <- binForSeq(resolution)
    period <- unitToPeriod(resolution)
    offDetections <- 0
    x <- bind_rows(lapply(split(x, splitCols), function(df) {
        if(nrow(df) == 0) {
            return(df)
        }
        thisEff <- effort
        for(col in columns) {
            thisEff <- thisEff[thisEff[[col]] == df[[col]][1], ]
        }
        if(nrow(thisEff) == 0) {
            return(df)
        }
        effSeq <- vector('list', length=nrow(thisEff))
        for(i in 1:nrow(thisEff)) {
            effSeq[[i]] <- seq(from=thisEff$start[i], to=thisEff$end[i]-.1, by=bin)
        }
        effSeq <- unlist(effSeq)
        effSeq <- as.POSIXct(effSeq, origin='1970-01-01 00:00:00', tz='UTC')
        startIn <- df$UTC %in% effSeq
        if(any(!startIn)) {
            offDetections <<- offDetections + sum(!startIn)
        }
        effSeq <- effSeq[!effSeq %in% df$UTC]
        nOff <- length(effSeq)
        effDf <- data.frame(UTC=effSeq, 
                            end=effSeq+period,
                            effortDetection=rep(0, nOff))
        for(col in columns) {
            effDf[[col]] <- rep(df[[col]][1], nOff)
        }
        
        bind_rows(df, effDf)
    }))
    if(offDetections > 0) {
        warning(offDetections, ' detections occurred during "off effort" times',
                call. = FALSE)
    }
    x
}