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
#' psd <- loadSoundscapeData(system.file('extdata/PSDSmall.csv', package='PAMscapes'))
#' str(psd)
#' tol <- createOctaveLevel(psd, type='tol')
#' str(tol)
#' ol <- createOctaveLevel(tol, type='ol')
#' str(ol)
#'
#' @importFrom dplyr group_by summarise ungroup rename mutate
#' @importFrom data.table `:=`
#'
createOctaveLevel <- function(x,
                              type=c('ol', 'tol'),
                              freqRange=NULL,
                              normalized=FALSE) {
    # startLong <- isLong(x)
    # if(startLong) {
        # inType <- x$type[1]
    # } else {
        # whichFreq <- whichFreqCols(x)
        # inType <- gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq[1]])
    # }
    # if(inType == 'HMD') {
        # millidecade band parts are normalized by bandwidth, we need to un-norm them
        # x <- correctHmdLevels(x)
    # }
    # x <- toLong(x)
    # nonFreqCols <- getNonFreqCols(x)
    # nonFreqData <- distinct(x[c('UTC', nonFreqCols)])
# 
#     type <- match.arg(type)
#     octLevels <- getOctaveLevels(type)
#     if(!is.null(freqRange)) {
#         # freqRange <- range(x$frequency)
#         lowCut <- min(which(freqRange[1] <= octLevels$freqs))
#         highCut <- max(which(freqRange[2] >= octLevels$freqs))
#         octLevels$freqs <- octLevels$freqs[lowCut:highCut]
#         octLevels$limits <- octLevels$limits[lowCut:(highCut+1)]
#     }
#     x$octave <- cut(x$frequency, octLevels$limits, labels=octLevels$freqs)
#     if(anyNA(x$octave)) {
#         x <- x[!is.na(x$octave), ]
#     }
#     x$value <- 10^(x$value / 10)
#     FUN <- switch(match.arg(method),
#                   'sum' = function(x) sum(x, na.rm=TRUE),
#                   'mean' = function(x) mean(x, na.rm=TRUE),
#                   'median' = function(x) median(x, na.rm=TRUE)
#     )
#     setDT(x)
#     x <- x[, lapply(.SD, FUN), .SDcols='value', by=c('UTC', 'octave', nonFreqCols)]
#     setDF(x)
# 
#     x$type <- toupper(type)
#     x <- rename(x, frequency = 'octave')
#     x$frequency <- as.numeric(levels(x$frequency))[as.numeric(x$frequency)]
#     x$value <- 10 * log10(x$value)
    #######################
    # do prep band work
    inWide <- isWide(x)
    if(!inWide) {
        x <- toWide(x)
    }
    freqCols <- colnames(x)[whichFreqCols(x)]
    freqVals <- colsToFreqs(freqCols)
    inType <- gsub('([A-Z]*)_.*', '\\1', freqCols[1])
    if(inType == 'HMD') {
        hmdLevels <- getHmdLevels(freqRange=range(freqVals))
        bwList <- c(10*log10(diff(hmdLevels$limits)))
        names(bwList) <- hmdLevels$labels
        for(f in names(bwList)) {
            x[[f]] <- x[[f]] + bwList[f]
        }
    }
    if(is.null(freqRange)) {
        freqRange <- range(freqVals)
    }
    type <- match.arg(type)
    bandPlan <- planBandSum(inType, type, inRange=range(freqVals), outRange=freqRange)
    
    setDT(x)
    x[, (freqCols) := lapply(.SD, function(c) 10^(c/10)), .SDcols=freqCols]
    for(i in seq_along(bandPlan)) {
        thisBand <- bandPlan[[i]]
        # thisBand$factor <- round(thisBand$factor, 4)
        names(thisBand$factor) <- thisBand$labels
        x[, 
          (names(bandPlan)[i]) := 10*log10(rowSums(
              .SD[, lapply(names(.SD), function(c) .SD[[c]]*thisBand$factor[c])],
              # .SD*thisBand$factor[.SD],
              na.rm=TRUE)), 
          .SDcols=thisBand$labels]
    }
    x[, c(freqCols) := NULL]
    setDF(x)
    x <- checkInfinite(x, doWarn=FALSE)
    if(isTRUE(normalized)) {
        for(i in names(bandPlan)) {
            x[[i]] <- x[[i]] - bandPlan[[i]]$bw
        }
    }
    if(!inWide) {
        return(toLong(x))
    }
    x
    #########################
    # if all NA in a category they get set to 0 so then Inf'd on log
    # x <- checkInfinite(x, doWarn=FALSE)
    # if(isTRUE(normalized)) {
    #     levDf <- data.frame(frequency=octLevels$freqs, bw=10*log10(diff(octLevels$limits)))
    #     x <- mutate(
    #         left_join(x, levDf, by='frequency'),
    #         value = .data$value - .data$bw
    #     )
    #     x$bw <- NULL
    # }
    # if(!startLong) {
    #     return(toWide(x))
    # }
    # 
    # x
}

getOctaveLevels <- function(type=c('ol', 'tol', 'hmd', 'psd'), freqRange=NULL) {
    type <- match.arg(type)
    if(type == 'hmd') {
        return(getHmdLevels(freqRange))
    }
    if(type == 'psd') {
        return(getPsdLevels(freqRange))
    }
    nominalFreqs <- c(1,
        1.25, 1.6, 2, 2.5, 3.15, 4, 5, 6.3, 8, 10,
        13, 16, 20, 25, 31.5, 40, 50, 63, 80, 100,
        125, 160, 200, 250, 315, 400, 500, 630, 800, 1000,
        1250, 1600, 2e3, 2500, 3150, 4e3, 5e3, 6300, 8e3, 10e3,
        12500, 16e3, 20e3, 25e3, 31500, 40e3, 50e3, 63e3, 80e3, 100e3,
        125e3, 160e3, 200e3, 250e3, 315e3, 400e3, 500e3)
    ix <- seq_along(nominalFreqs) - 1
    if(type == 'ol') {
        olSeq <- seq(from=1, to=length(nominalFreqs), by=3)
        nominalFreqs <- nominalFreqs[olSeq]
        ix <- ix[olSeq]
    }
    if(!is.null(freqRange) && length(freqRange) == 2) {
        freqRange <- sort(freqRange)
        inRange <- nominalFreqs >= freqRange[1] & nominalFreqs <= freqRange[2]
        nominalFreqs <- nominalFreqs[inRange]
        ix <- ix[inRange]
    }
    freqVals <- 10^(ix/10)
    switch(type,
           'ol' = {
               # everyThird <- seq(from=1, to=length(nominalFreqs), by=3)
               # nominalFreqs <- nominalFreqs[everyThird]
               # freqVals <- freqVals[everyThird]
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

getPsdLevels <- function(freqRange=NULL) {
    nominalFreqs <- 0:500e3
    if(!is.null(freqRange)) {
        freqRange <- sort(freqRange)
        nominalFreqs <- nominalFreqs[nominalFreqs >= freqRange[1] &
                                         nominalFreqs <= freqRange[2]]
    }
    limits <- c(max(0, nominalFreqs[1]-0.5), nominalFreqs + 0.5)
    list(limits=limits, labels=paste0('PSD_', nominalFreqs), nominalFreqs)
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
    nominalFreqs <- round(nominalFreqs, 3)
    labels <- nominalFreqs
    labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
    labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
    labels <- paste0('HMD_', labels)
    # labels <- paste0('HMD_', round(nominalFreqs, 1))
    
    list(limits=freqLims, labels=labels, freqs=nominalFreqs)
}

# millidecade bands are normalized by bandwidth, need to multiple back
# the bandwidth before summing
correctHmdLevels <- function(x) {
    # changeFreq <- 434
    # lowHalf <- x[x$frequency <= changeFreq, ]
    # highHalf <- x[x$frequency > changeFreq, ]
    inLong <- FALSE
    if(isLong(x)) {
        inLong <- TRUE
        x <- toWide(x)
    }
    freqCols <- whichFreqCols(x)
    freqVals <- colsToFreqs(names(x)[freqCols])
    hmdLevels <- getHmdLevels(freqRange=range(freqVals))
    bwList <- c(10*log10(diff(hmdLevels$limits)))
    names(bwList) <- hmdLevels$labels
    # levDf <- data.frame(freq=round(hmdLevels$freqs, 1), bw=10*log10(diff(hmdLevels$limits)))
    # matchDf <- left_join(data.frame(freq=freqVals, ix=freqCols),
                         # levDf, by='freq')
    
    for(f in names(bwList)) {
        x[[f]] <- x[[f]] + bwList[f]
    }
    if(inLong) {
        x <- toLong(x)
    }
    x
}

planBandSum <- function(inBand, outBand, inRange=NULL, outRange=NULL) {
    inLevels <- getOctaveLevels(tolower(inBand), freqRange=inRange)
    outLevels <- getOctaveLevels(tolower(outBand), freqRange=outRange)
    outs <- vector('list', length=length(outLevels$labels))
    names(outs) <- outLevels$labels
    for(ix in seq_along(outs)) {
        thisLim <- outLevels$limits[ix:(ix+1)]
        if(thisLim[2] < inLevels$limits[1]) {
            next
        }
        if(thisLim[1] > inLevels$limits[length(inLevels$limits)]) {
            next
        }
        lowLim <- min(which(thisLim[1] <= inLevels$limits)) -1
        highLim <- max(which(thisLim[2] >= inLevels$limits)) + 1
        if(highLim > length(inLevels$limits)) {
            highLim <- length(inLevels$limits)
        }
        result <- list()
        result$lims <- thisLim
        inLim <- lowLim:highLim
        result$labels <- inLevels$labels[lowLim:(highLim-1)]
        result$hmd_lims <- inLevels$limits[inLim]
        nHmdLim <- length(result$hmd_lims)
        if(nHmdLim == 2) {
            result$factor <- diff(thisLim) / diff(result$hmd_lims)
            outs[[ix]] <- result
            next
        }
        lowFact <- (result$hmd_lims[2]-thisLim[1]) / diff(result$hmd_lims[1:2])
        if(lowFact < 0) {
            lowFact <- 1
        }
        highFact <- (thisLim[2]-result$hmd_lims[nHmdLim-1])/diff(result$hmd_lims[(nHmdLim-1):nHmdLim])
        if(highFact < 0) {
            highFact <- 1
        }
        result$factor <- c(lowFact, rep(1, max(nHmdLim-3, 0)), highFact)
        result$bw <- 10*log10(diff(thisLim))
        outs[[ix]] <- result
    }
    outs <- outs[sapply(outs, function(x) !is.null(x))]
    outs
}
