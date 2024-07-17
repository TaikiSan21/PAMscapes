#' @title Create Octave Level Measurements
#'
#' @description Creates octave or third octave level measurements from finer
#'   resolution soundscape metrics, like Power Spectral Density (PSD) or
#'   Hybrid Millidecade (HMD) measures
#'
#' @param x dataframe of soundscape metrics
#' @param type either \code{'ol'} to create octave level or \code{'tol'} to create
#'   third octave level measures
#' @param freqRange a vector of the minimum and maximum center frequencies (Hz) desired
#'   for the output. If \code{NULL}, full available range of frequencies will be used.
#' @param method the summary method to apply to soundscape metrics within the octave band,
#'   one of \code{'sum'} or \code{'mean'}. The default \code{'sum'} should be used in
#'   almost all cases.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a dataframe with summarised octave level band measurements
#'
#' @export
#'
#' @examples
#' psd <- checkSoundscapeInput(system.file('extdata/PSDSmall.csv', package='PAMscapes'))
#' str(psd)
#' tol <- createOctaveLevel(psd, type='tol')
#' str(tol)
#' ol <- createOctaveLevel(tol, type='ol')
#' str(ol)
#'
#' @importFrom dplyr group_by summarise ungroup rename
#'
createOctaveLevel <- function(x, type=c('ol', 'tol'), freqRange=NULL, method=c('sum', 'mean')) {
    x <- checkSoundscapeInput(x)
    startLong <- isLong(x)
    x <- toLong(x)
    type <- match.arg(type)
    octLevels <- getOctaveLevels(type)
    if(is.null(freqRange)) {
        freqRange <- range(x$frequency)
    }
    lowCut <- min(which(freqRange[1] <= octLevels$freqs))
    highCut <- max(which(freqRange[2] >= octLevels$freqs))
    octLevels$freqs <- octLevels$freqs[lowCut:highCut]
    octLevels$limits <- octLevels$limits[lowCut:(highCut+1)]
    x$octave <- cut(x$frequency, octLevels$limits, labels=octLevels$freqs)
    x <- x[!is.na(x$octave), ]
    x$value <- 10^(x$value / 10)
    FUN <- switch(match.arg(method),
                  'sum' = sum,
                  'mean' = mean
    )
    x <- x %>%
        group_by(.data$UTC, .data$octave) %>%
        summarise(value = FUN(.data$value)) %>%
        ungroup()
    x$type <- toupper(type)
    x <- rename(x, frequency = 'octave')
    x$frequency <- as.numeric(levels(x$frequency))[as.numeric(x$frequency)]
    x$value <- 10 * log10(x$value)
    if(!startLong) {
        return(toWide(x))
    }
    x
}

getOctaveLevels <- function(type=c('ol', 'tol'), freqRange=NULL) {
    type <- match.arg(type)
    nominalFreqs <- c(1,
        1.25, 1.6, 2, 2.5, 3.15, 4, 5, 6.3, 8, 10,
        13, 16, 20, 25, 31.5, 40, 50, 63, 80, 100,
        125, 160, 200, 250, 315, 400, 500, 630, 800, 1000,
        1250, 1600, 2e3, 2500, 3150, 4e3, 5e3, 6300, 8e3, 10e3,
        12500, 16e3, 20e3, 25e3, 31500, 40e3, 50e3, 63e3, 80e3, 100e3,
        125e3, 160e3, 200e3, 250e3, 315e3, 400e3, 500e3)
    ix <- seq_along(nominalFreqs) - 1
    if(!is.null(freqRange) && length(freqRange) == 2) {
        freqRange <- sort(freqRange)
        inRange <- nominalFreqs >= freqRange[1] & nominalFreqs <= freqRange[2]
        nominalFreqs <- nominalFreqs[inRange]
        ix <- ix[inRange]
    }
    freqVals <- 10^(ix/10)
    switch(type,
           'ol' = {
               everyThird <- seq(from=1, to=length(nominalFreqs), by=3)
               nominalFreqs <- nominalFreqs[everyThird]
               freqVals <- freqVals[everyThird]
               freqLims <- c(freqVals[1] * 10^(-3/20), freqVals * 10^(3/20))
           },
           'tol' = {
               freqLims <- c(freqVals[1] * 10^(-1/20), freqVals * 10^(1/20))
           }
    )
    labels <- as.character(nominalFreqs)
    isSci <- grepl('e\\+', labels)
    labels[isSci] <- format(as.numeric(labels[isSci]), scientific=FALSE)
    labels <- paste0(toupper(type), '_', labels)
    # if(!is.null(freqRange) && length(freqRange) == 2) {
    #     freqRange <- sort(freqRange)
    #     inRange <- nominalFreqs >= freqRange[1] & nominalFreqs <= freqRange[2]
    #     nominalFreqss <- nominalFreqs[inRange]
    #     labels <- labels[inRange]
    #     limits <- limits[c(inRange, inRange[length(inRange)])]
    # }
    list(limits = freqLims, labels=labels, freqs=nominalFreqs)
}
