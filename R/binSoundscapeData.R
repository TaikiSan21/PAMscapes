#' @title Summarise Soundscape Data by Time Bin
#'
#' @description Bins soundscape measurements by a unit of time
#'   and summarises them using a function (usually the median)
#'
#' @param x a data.frame of soundscape metric data read in with 
#'   \link{loadSoundscapeData}
#' @param bin amount of time to bin data by, format can
#'   be "#Unit" e.g. \code{'2hour'} or \code{'1day'}
#' @param method summary function to apply to data in each time bin,
#'   must be one of "median" or "mean"
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a summarised version of the input data.frame \code{x}
#'
#' @export
#'
#' @importFrom data.table setDT setDF
#' @importFrom lubridate floor_date
#'
binSoundscapeData <- function(x, bin='1hour', method=c('median', 'mean')) {
    method <- match.arg(method)
    FUN <- switch(method,
                  'median' = function(x) median(x, na.rm=TRUE),
                  'mean' = function(x) mean(x, na.rm=TRUE)
    )
    cols <- colnames(x)

    if(isWide(cols)) {
        valCols <- cols[whichFreqCols(cols)]
        byCols <- c('UTC')
    }
    if(isLong(cols)) {
        valCols <- 'value'
        byCols <- c('frequency', 'type', 'UTC')
    }
    x$UTC <- floor_date(x[['UTC']], unit=bin)
    nonFreqCols <- getNonFreqCols(x)
    nonFreqData <- distinct(x[c('UTC', nonFreqCols)])
    setDT(x)
    x <- x[, lapply(.SD, FUN), .SDcols=valCols, by=byCols]
    setDF(x)
    if(length(nonFreqCols) > 0) {
        x <- left_join(x, nonFreqData, by='UTC')
    }
    x
}
