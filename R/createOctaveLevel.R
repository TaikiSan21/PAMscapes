#' @title Create Octave Level Measurements
#'
#' @description Creates octave or third octave level measurements from finer
#'   resolution soundscape metrics, like Power Spectral Density (PSD) or
#'   Hybrid Millidecade (HMD) measures
#'
#' @details Note that these measures are not as precise as they could be, mostly
#'   meant to be used for visualizations. Bands of the original data that do not
#'   fit entirely within a single octave band are not proportionately split between
#'   the two proper output bands. Instead an output band will contain all inputs where
#'   the center frequency falls between the limits of the output band. For higher
#'   frequencies this should result in negligible differences, but lower frequencies
#'   will be more imprecise.
#'
#' @param x dataframe of soundscape metrics
#' @param type either \code{'ol'} to create octave level or \code{'tol'} to create
#'   third octave level measures
#' @param freqRange a vector of the minimum and maximum center frequencies (Hz) desired
#'   for the output. If \code{NULL}, full available range of frequencies will be used.
#' @param method the summary method to apply to soundscape metrics within the octave band,
#'   one of \code{'sum'} or \code{'mean'}. The default \code{'sum'} should be used in
#'   almost all cases.
#' @param normalized logical flag to return values normalized by the bandwidth of
#'   each octave level band
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
#' @importFrom dplyr group_by summarise ungroup rename mutate
#'
createOctaveLevel <- function(x,
                              type=c('ol', 'tol'),
                              freqRange=NULL,
                              method=c('sum', 'mean', 'median'),
                              normalized=FALSE) {
    x <- checkSoundscapeInput(x)
    startLong <- isLong(x)
    x <- toLong(x)
    nonFreqCols <- getNonFreqCols(x)
    nonFreqData <- distinct(x[c('UTC', nonFreqCols)])
    inType <- x$type[1]
    if(inType == 'HMD') {
        # millidecade band parts are normalized by bandwidth, we need to un-norm them
        x <- correctHmdLevels(x)
    }
    type <- match.arg(type)
    octLevels <- getOctaveLevels(type)
    if(!is.null(freqRange)) {
        # freqRange <- range(x$frequency)
        lowCut <- min(which(freqRange[1] <= octLevels$freqs))
        highCut <- max(which(freqRange[2] >= octLevels$freqs))
        octLevels$freqs <- octLevels$freqs[lowCut:highCut]
        octLevels$limits <- octLevels$limits[lowCut:(highCut+1)]
    }
    x$octave <- cut(x$frequency, octLevels$limits, labels=octLevels$freqs)
    x <- x[!is.na(x$octave), ]
    x$value <- 10^(x$value / 10)
    FUN <- switch(match.arg(method),
                  'sum' = sum,
                  'mean' = mean,
                  'median' = median
    )
    setDT(x)
    x <- x[, lapply(.SD, FUN), .SDcols='value', by=c('UTC', 'octave')]
    setDF(x)
    if(length(nonFreqCols) > 0) {
        x <- left_join(x, nonFreqData, by='UTC')
    }

    x$type <- toupper(type)
    x <- rename(x, frequency = 'octave')
    x$frequency <- as.numeric(levels(x$frequency))[as.numeric(x$frequency)]
    x$value <- 10 * log10(x$value)
    if(isTRUE(normalized)) {
        levDf <- data.frame(frequency=octLevels$freqs, bw=diff(octLevels$limits))
        x <- mutate(
            left_join(x, levDf, by='frequency'),
            value = .data$value - 10*log10(.data$bw)
        )
        x$bw <- NULL
    }
    if(!startLong) {
        return(toWide(x))
    }
    x
}

getOctaveLevels <- function(type=c('ol', 'tol', 'hmd'), freqRange=NULL) {
    type <- match.arg(type)
    if(type == 'hmd') {
        return(getHmdLevels(freqRange))
    }
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
    list(limits = freqLims, labels=labels, freqs=nominalFreqs)
}

getHmdLevels <- function(freqRange=NULL) {
    n <- 1639:5000 # tail limit is 1e6
    lowCenter <- 0:434
    lowLims <- c(0, 0:434 + 0.5)
    highCenter <- c(10 * 10 ^ ((2*(n-1)+1) / (2*1000)))
    highLims <- c(highCenter*10^(1/2000))
    freqLims <- c(lowLims, highLims)
    nominalFreqs <- c(lowCenter, highCenter)
    if(!is.null(freqRange) && length(freqRange) == 2) {
        inRange <- nominalFreqs >= freqRange[1] &
            nominalFreqs <= freqRange[2]
        nominalFreqs <- nominalFreqs[inRange]
        freqLims <- freqLims[c(which(inRange), max(which(inRange))+1)]
    }
    nominalFreqs <- round(nominalFreqs, 1)
    labels <- paste0('HMD_', nominalFreqs)
    list(limits=freqLims, labels=labels, freqs=nominalFreqs)
}

# millidecade bands are normalized by bandwidth, need to multiple back
# the bandwidth before summing
correctHmdLevels <- function(x) {
    # changeFreq <- 434
    # lowHalf <- x[x$frequency <= changeFreq, ]
    # highHalf <- x[x$frequency > changeFreq, ]
    hmdLevels <- getHmdLevels(freqRange=range(x$frequency))
    levDf <- data.frame(frequencyJoin=round(hmdLevels$freqs, 1), bw=diff(hmdLevels$limits))
    x$frequencyJoin <- round(x$frequency, 1)
    x <- mutate(
        left_join(x, levDf, by='frequencyJoin'),
        value = .data$value + 10*log10(.data$bw)
    )
    x$bw <- NULL
    x$frequencyJoin <- NULL
    x
}
