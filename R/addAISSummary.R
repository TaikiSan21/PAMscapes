#' @title Add AIS Data Summary to Dataframe
#'
#' @description Adds a summary of matching AIS data for nearby vessels
#'   to a data. Information added includes number of vessels, distance
#'   to nearby vessels, and average speed of nearby vessels
#'
#' @param x a dataframe with \code{UTC}, \code{Latitude}, and \code{Longitude}
#'   columns
#' @param ais AIS data created using the \link{readLocalAIS} function. Can also
#'   be a character listing the directory of AIS
#' @param distance distance (meters) within locations in \code{x} to mark
#'   as "nearby"
#'
#' @return a dataframe with AIS summary data added. Will contain new columns
#'   \describe{
#'    \item{nShips}{the number of ships within "distance" at this time}
#'    \item{meanDist}{average distance of nearby ships, NA if none}
#'    \item{meanSOG}{average speed over ground of nearby ships, NA if none}
#'    \item{closeDist}{distance of the closest ship, NA if none}
#'    \item{closeSOG}{speed over ground of closest ship, NA if none}
#'   }
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr distinct
#'
#' @export
#'
addAISSummary <- function(x, ais, distance=10e3) {
    x <- checkSimple(x, needCols=c('UTC', 'Latitude', 'Longitude'))
    aisCols <- c('MMSI', 'vesselLength', 'vesselType', 'SOG',
                 'shipLat', 'shipLong', 'shipDist')
    nonAisCols <- colnames(x)[!colnames(x) %in% aisCols]
    # if we do not already have AIS data in x
    if(!all(aisCols %in% colnames(x))) {
        if(is.character(ais)) {
            ais <- readLocalAIS(x, ais, distance=distance)
        }
        x <- addAIS(x, ais, interpType='none')
    }
    withAis <- bind_rows(lapply(split(x, x$UTC), function(time) {
        dists <- time$shipDist
        inDist <- dists < distance
        inDist[is.na(inDist)] <- FALSE
        nShips <- sum(inDist)
        meanDist <- mean(dists[inDist])
        meanSOG <- mean(time$SOG[inDist])
        closest <- which.min(dists[inDist])
        if(length(closest) != 0) {
            closeDist <- dists[inDist][closest]
            closeSOG <- time$SOG[inDist][closest]
        } else {
            closeDist <- NA
            closeSOG <- NA
        }

        list(UTC=time$UTC[1], nShips=nShips, meanDist=meanDist, meanSOG=meanSOG,
             closeDist=closeDist, closeSOG=closeSOG)
    }))
    x <- distinct(x[nonAisCols])
    x <- left_join(x, withAis, by=join_by('UTC'=='UTC'))
    x
}
