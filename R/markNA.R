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
#' @param by optional column name in both \code{x} and \code{na} if only certain
#'   rows of \code{na} should apply to certain rows of \code{x} (e.g. if these
#'   contain multiple deployments overlapping in time, a "DeploymentName" column
#'   can be used to only mark appropriate times)
#'
#' @return same dataframe as \code{x} but with some values replaced with \code{NA}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' manta <- checkSoundscapeInput(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
#' naDf <- data.frame(start=min(manta$UTC),
#'                    end=max(manta$UTC),
#'                    freqMin=100,
#'                    freqMax=500)
#' plotHourlyLevel(manta)
#' plotHourlyLevel(markNA(manta, na=naDf))
#'
#' @export
#'
markNA <- function(x, na, by=NULL) {
    na <- checkNaDf(na)
    x <- checkSoundscapeInput(x, 'UTC')
    if(!is.null(by)) {
        if(!by %in% colnames(x) ||
           !by %in% colnames(na)) {
            stop('"by" must be present in both dataframes')
        }
        return(bind_rows(lapply(split(x, x[[by]]), function(y) {
            thisNa <- na[na[[by]] == y[[by]][1], ]
            if(is.null(thisNa) || nrow(thisNa) == 0) {
                return(y)
            }
            y <- markNA(y, na=thisNa[c('start', 'end')], by=NULL)
            y
        })))
    }
    startLong <- isLong(x)
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
