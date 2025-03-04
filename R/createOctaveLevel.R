#' @title Create Octave Level Measurements
#'
#' @description Creates (third) octave level or broadband measurements from finer
#'   resolution soundscape metrics, like Power Spectral Density (PSD) or
#'   Hybrid Millidecade (HMD) measures
#'
#' @details To create new measurements, finer resolution metrics are cast to 
#'   linear space, summed, and then re-logged. If input measurements are
#'   HMD values then they are assumed to be normalized per Hz, so levels are
#'   first corrected by the bandwidth before summing. In all other cases inputs
#'   are assumed to not be normalized per Hz measurements and are just summed.
#'
#' @param x dataframe of soundscape metrics
#' @param type one of \code{'ol'} to create octave level, \code{'tol'} to create
#'   third octave level measures, or \code{'broadband'} or \code{'bb'} to create
#'   an arbitrary broadband measure. For broadband measures, \code{freqRange} must
#'   be supplied to define the range
#' @param freqRange a vector of the minimum and maximum center frequencies (Hz) desired
#'   for the output. If \code{NULL}, full available range of frequencies will be used.
#'   If output \code{type} is broadband, this is used to define the lower and upper
#'   bounds of the desired output broadband level
#' @param normalized logical flag to return values normalized by the bandwidth of
#'   each octave level band (per Hz)
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
#' bb <- createOctaveLevel(psd, type='bb', freqRange=c(20, 150))
#' str(bb)
#'
#' @importFrom dplyr group_by summarise ungroup rename mutate
#' @importFrom data.table `:=`
#'
createOctaveLevel <- function(x,
                              type=c('ol', 'tol', 'broadband', 'bb'),
                              freqRange=NULL,
                              normalized=FALSE) {
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
    type <- match.arg(type)
    bandPlan <- planBandSum(inType, type, inRange=range(freqVals), outRange=freqRange)
    setDT(x)
    x[, (freqCols) := lapply(.SD, function(c) 10^(c/10)), .SDcols=freqCols]
    for(i in seq_along(bandPlan)) {
        thisBand <- bandPlan[[i]]
        names(thisBand$factor) <- thisBand$labels
        x[, 
          (names(bandPlan)[i]) := 10*log10(rowSums(
              .SD[, lapply(names(.SD), function(c) .SD[[c]]*thisBand$factor[c])],
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
}

getOctaveLevels <- function(type=c('ol', 'tol', 'hmd', 'psd', 'broadband', 'bb'), freqRange=NULL) {
    # limits n+1, labels n, freqs n
    type <- match.arg(type)
    if(type %in% c('broadband', 'bb')) {
        if(is.null(freqRange)) {
            stop('"freqRange" required to create broadband level')
        }
        result <- list(
            limits = freqRange,
            labels = paste0('BB_', freqRange[1], '-', freqRange[2]),
            freqs = mean(freqRange)
        )
        return(result)
    }
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
    list(limits=limits, labels=paste0('PSD_', nominalFreqs), freqs=nominalFreqs)
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
    list(limits=freqLims, labels=labels, freqs=nominalFreqs)
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
        result$freqs <- inLevels$freqs[lowLim:(highLim-1)]
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
