#' @title Add AIS Data to Dataframe
#'
#' @description Adds matching AIS data downloaded from Marine Cadastre
#'   to a dataframe containing location information
#'
#' @param x a dataframe with \code{UTC}, \code{Latitude}, and \code{Longitude}
#'   columns
#' @param ais AIS data created using the \link{readLocalAIS} function
#' @param interpType one of \code{c('all', 'close', 'none')}, the type
#'   of time interpolation to apply to \code{x}. Often the time scale of points
#'   in \code{x} is much longer than the points in \code{ais}, which can result
#'   in awkward looking AIS paths. \code{'all'} will interpolate all points in
#'   \code{x} to a smaller timescale. \code{'close'} will interpolate only
#'   time ranges in \code{ais} marked as \code{inDist} by \link{readLocalAIS}.
#'   \code{'none'} will apply no interpolation
#' @param interpTime time (seconds) between new \code{UTC} points. If
#'   \code{0} (default), no interpolation will be done
#' @param interpCols names of any extra columns to interpolate (other than
#'   \code{Latitude} and \code{Longitude})
#'
#' @return a dataframe with AIS data added, will contain more rows than \code{x}
#'   if \code{ais} has more than one vessel. If any interpolation is applied,
#'   any non-constant columns not specified to \code{interpCols} will be removed
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom utils tail
#' @export
#'
addAIS <- function(x, ais, interpType=c('all', 'close', 'none'), interpTime=0, interpCols=NULL) {
    interpType <- match.arg(interpType)
    # if(!all(c('Latitude', 'Longitude') %in% colnames(x))) {
    #     interpType <- 'none'
    #     interpTime <- 0
    # }
    if(interpType == 'all' &&
       interpTime > 0) {
        x <- interpLocations(x, diff=interpTime, includeEnd=TRUE, interpCols=interpCols)
    }
    bind_rows(
        lapply(
            # split(ais, list(ais$MMSI, ais$group)), function(x) {
            split(ais, ais$MMSI), function(oneAis) {
                # only do more if interp close
                if(interpType == 'none' ||
                   interpTime == 0 ||
                   interpType == 'all') {
                    return(oneAddAIS(x, oneAis))
                }
                byGroup <- split(oneAis, oneAis$group)
                interpData <- bind_rows(lapply(byGroup, function(oneGroup) {
                    tRange <- range(oneGroup$UTC) + c(-1, 1) * interpTime
                    if(isFALSE(oneGroup$inDist[1])) {
                        return(x[x$UTC >= tRange[1] & x$UTC <= tRange[2], ])
                    }
                    interpLocations(x, from=tRange[1],
                                    to=tRange[2],
                                    diff=interpTime,
                                    includeEnd=TRUE,
                                    interpCols=interpCols)
                }))
                interpData <- arrange(interpData, .data$UTC)
                oneAddAIS(interpData, oneAis)
                # result <- lapply(byGroup, function(oneGroup) {
                #     if(isFALSE(oneGroup$inDist[1])) {
                #         return(oneAddAIS(x, oneGroup))
                #     }
                #     # interpTime <- 60
                #     interpData <- interpLocations(x, from=min(oneGroup$UTC)-interpTime,
                #                                   to=max(oneGroup$UTC)+interpTime,
                #                                   diff=interpTime,
                #                                   includeEnd=TRUE,
                #                                   interpCols=interpCols)
                #     oneAddAIS(interpData, oneGroup)
                # })
                # bind_rows(result)
            }))
}

oneAddAIS <- function(gps, ais) {
    if(nrow(ais) <= 1) {
        return(gps)
    }
    gps$MMSI <- ais$MMSI[1]
    gps$vesselLength <- ais$vesselLength[1]
    gps$vesselType <- ais$vesselType[1]
    gps$SOG <- approx(x=ais$UTC, y=ais$SOG, xout=gps$UTC)$y
    gps$shipLat <- approx(x=ais$UTC, y=ais$Latitude, xout=gps$UTC)$y
    gps$shipLong <- approx(x=ais$UTC, y=ais$Longitude, xout=gps$UTC)$y
    if(all(c('Latitude', 'Longitude') %in% colnames(gps))) {
        gps$shipDist <- distGeo(matrix(c(gps$Longitude, gps$Latitude), ncol=2),
                                matrix(c(gps$shipLong, gps$shipLat), ncol=2))
    } else {
        gps$shipDist <- NA
    }
    gps
}
# interpolate lat/longs to smaller time scale
interpLocations <- function(x, from=NULL, to=NULL, diff=300, interpCols=NULL, includeEnd=TRUE) {
    if(is.null(from) ||
       from < min(x$UTC, na.rm=TRUE)) {
        from <- min(x$UTC, na.rm=TRUE)
    }
    if(is.null(to) ||
       to > max(x$UTC, na.rm=TRUE)) {
        to <- max(x$UTC, na.rm=TRUE)
    }
    newTimes <- seq(from=from, to=to, by=diff)
    # this can ensure doesnt cover less time, but not consistent diff
    if(isTRUE(includeEnd) &&
       tail(newTimes, 1) < to) {
        newTimes <- c(newTimes, to)
    }
    result <- data.frame(UTC=newTimes)
    interpCols <- c('Latitude', 'Longitude', interpCols)
    # result$Latitude <- approx(x=x$UTC, y=x$Latitude, xout=result$UTC)$y
    # result$Longitude <- approx(x=x$UTC, y=x$Longitude, xout=result$UTC)$y
    # if(!is.null(interpCols)) {
    for(e in interpCols) {
        if(!e %in% colnames(x)) {
            next
        }
        result[[e]] <- approx(x=x$UTC, y=x[[e]], xout=result$UTC)$y
    }
    # }
    uniqueVals <- lapply(x, function(x) unique(x))
    # browser()
    nVals <- sapply(uniqueVals, length)
    for(i in seq_along(uniqueVals)) {
        if(nVals[i] == 1) {
            result[[names(uniqueVals)[i]]] <- uniqueVals[[i]]
        }
    }
    result
}
