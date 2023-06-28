#' @title Mark NA Values by Time and Frequency
#'
#' @description Marks values within a soundscape dataframe as NA
#'   according to provided time and (optionally) frequency values
#'
#' @param x dataframe of soundscape data to mark NAs in
#' @param na dataframe listing areas to mark NA. Must have columns \code{start}
#'   and \code{end} in UTC listing time ranges. Can also have columns \code{freqMin}
#'   and \code{freqMax} to also have accompanying frequency ranges, otherwise
#'   all frequency values within the time range will be set to NA
#'
#' @return same dataframe as \code{x} but with some values replaced with \code{NA}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
markNA <- function(x, na) {
    na <- checkNaDf(na)
    startLong <- all(c('UTC', 'type', 'value', 'frequency') %in% colnames(x))
    x <- toLong(x)
    naFreq <- all(c('freqMin', 'freqMax') %in% colnames(na))
    for(i in 1:nrow(na)) {
        naIx <- x$UTC >= na$start[i] & x$UTC <= na$end[i]
        if(naFreq && !is.na(na$freqMin[i]) && !is.na(na$freqMax[i])) {
            freqIx <- x$frequency >= na$freqMin[i] & x$frequency <= na$freqMax[i]
            naIx <- naIx & freqIx
        }
        x$value[naIx] <- NA
    }
    #
    if(!startLong) {
        x <- toWide(x)
    }
    x
}

checkNaDf <- function(x) {
    if(!all(c('start', 'end') %in% colnames(x))) {
        stop('NA marks must have columns "start" and "end"')
    }
    x$start <- parseToUTC(x$start)
    x$end <- parseToUTC(x$end)
    x
}
