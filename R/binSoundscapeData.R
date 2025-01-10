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
#' @param binCount logical flag to return the number of times in
#'   each time bin as column "binCount"
#' @param extraCols Additional non-frequency columns in \code{x}
#'   to apply the binning to
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a summarised version of the input data.frame \code{x}
#'
#' @export
#'
#' @importFrom data.table setDT setDF .N
#' @importFrom lubridate floor_date
#'
binSoundscapeData <- function(x, 
                              bin='1hour',
                              method=c('median', 'mean'),
                              binCount=FALSE,
                              extraCols=NULL) {
    method <- match.arg(method)
    FUN <- switch(method,
                  'median' = function(x) median(x, na.rm=TRUE),
                  'mean' = function(x) mean(x, na.rm=TRUE)
    )
    cols <- colnames(x)

    if(isWide(cols)) {
        valCols <- cols[whichFreqCols(cols)]
        byCols <- c('UTC')
    } else if(isLong(cols)) {
        valCols <- 'value'
        byCols <- c('frequency', 'type', 'UTC')
    } else {
        valCols <- character(0)
        byCols <- c('UTC')
    }
    x$UTC <- floor_date(x[['UTC']], unit=bin)
    nonFreqCols <- getNonFreqCols(x)
    if(!is.null(extraCols)) {
        extraIn <- extraCols %in% nonFreqCols
        if(any(!extraIn)) {
            warning('Column(s) ', paste0(extraCols[!extraIn], collapse=', '),
                    ' are not in data')
            extraCols <- extraCols[extraIn]
        }
        valCols <- c(valCols, extraCols)
    }
    # cant bin effort column if its present bc wont work
    nonFreqCols <- nonFreqCols[!nonFreqCols %in% c('effortSeconds', extraCols)]
    nonFreqData <- distinct(x[c('UTC', nonFreqCols)])
    setDT(x)
    if(isTRUE(binCount)) {
        x <- x[, c(.N, lapply(.SD, FUN)), .SDcols=valCols, by=byCols]
        names(x)[names(x) == 'N'] <- 'binCount'
    } else {
        x <- x[, lapply(.SD, FUN), .SDcols=valCols, by=byCols]
    }
    setDF(x)
    if(length(nonFreqCols) > 0) {
        if(nrow(nonFreqData) != nrow(x)) {
            warning('Some additional columns ',
                    paste0(nonFreqCols, collapse=', '),
                    ' could not be kept with binned data, add to "extraCols"',
                    ' to keep them')
            return(x)
        }
        x <- left_join(x, nonFreqData, by='UTC')
    }
    x
}
