ncOrig <- '../Data/ncmod/original.nc'
ncMod <- '../Data/ncmod/modified.nc'

library(ncdf4)
nc <- nc_open(ncMod, write = TRUE)
# ok lets try making the DQ mat
# should be vars$quality_flag
inSize <- nc$var$psd$size
psd <- ncvar_get(nc, 'psd', start=c(1,1), count=c(-1, -1))
times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
freqs <- nc$dim$frequency$vals
dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
dqVar <-  ncvar_def(name='quality_flag', prec='byte', longname='Data quality flag', dim=nc$var$psd$dim, units='')
nc <- ncvar_add(nc, dqVar)
ncvar_put(nc, varid = dqVar, vals=dqMat)
nc_close(nc)

library(PAMscapes)
modData <- loadSoundscapeData(ncMod, keepQuals = c(1,2))
hm <- nc_open('tutorial/MANTAExample.nc')

addDataQuality <- function(ncFile, freqRange=NULL, timeRange=NULL, value=2, verbose=FALSE) {
    nc <- nc_open(ncFile)
    on.exit(nc_close(nc))
    if(!'quality_flag' %in% names(nc$var)) {
        if(verbose) {
            cat('No "quality_flag" variable found, creating a new one')
        }
        inSize <- nc$var$psd$size
        dqVar <- ncvar_def('quality_flag', prec='byte', longname='Data quality flag', dim=nc$var$psd$dim, units='')
        nc <- ncvar_add(nc, dqVar)
        dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
    } else {
        dqMat <- ncvar_get(nc, 'quality_flag', start=c(1, 1), count=c(-1, -1))
    }
    times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    type <- PAMscapes:::checkFreqType(freqs)
    dqMat <- markDQMatrix(dqMat, freqRange=freqRange, timeRange=timeRange, value=value,
                          times=times, freqs=freqs)
    
    ncvar_put(nc, varid='quality_flag', vals=dqMat)
}

markDQMatrix <- function(dq, freqRange=NULL, timeRange=NULL, value=2, times, freqs) {
    if(is.null(freqRange) &&
       is.null(timeRang)) {
        return(dq)
    }
    if(!inherits(freqRange, c('NULL', 'list', 'numeric'))) {
        stop('freqRange must be NULL, a list of numeric ranges, or a numeric vector')
    }
    if(!inherits(timeRange, c('NULL', 'list', 'POSIXct'))) {
        stop('timeRange must be NULL, a list of POSIXct ranges, or a POSIXct vector')
    }
    if(is.numeric(freqRange)) {
        if(length(freqRange) != 2) {
            stop('freqRange must be a numeric range of two frequency values (Hz)')
        }
    }
    if(inherits(timeRange, 'POSIXct')) {
        if(length(timeRange) != 2) {
            stop('timeRange must be a POSIXct vector of two time values')
        }
    }
    # cast NULL / non-list to list so we can a
    if(!is.list(freqRange)) {
        freqRange <- list(freqRange)
    }
    if(!is.list(timeRange)) {
        timeRange <- list(timeRange)
    }
    nFreq <- length(freqRange)
    nTime <- length(timeRange)
    nVal <- length(value)
    maxLen <- max(nFreq, nTime, nVal)
    if((nFreq > 1 & nFreq < maxLen) ||
       (nTime > 1 & nTime < maxLen) ||
       (nVal > 1 & nVal < maxLen)) {
        stop('Length of freqRange, timeRange, and value must match (or be 1)')
    }
    if(nVal == 1) {
        value <- rep(value, maxLen)
    }
    if(nFreq == 1) {
        freqRange <- rep(freqRange, maxLen)
    }
    if(nTime == 1) {
        timeRange <- rep(timeRange, maxLen)
    }
    lowFreq <- freqs[-length(freqs)]
    highFreq <- freqs[-1]
    lowTime <- times[-length(times)]
    highTime <- times[-1]
    for(i in seq_len(maxLen)) {
        if(is.null(freqRange[[i]])) {
            freqIx <- 1:nrow(dq)
        } else {
            freqIx <- highFreq > freqRange[[i]][1] &
                lowFreq < freqRange[[i]][2]
        }
        if(is.null(timeRange[[i]])) {
            timeIx <- 1:ncol(dq)
        } else {
            timeIx <- highTime > timeRange[[i]][1] &
                lowTime < timeRange[[i]][2]
        }
        dq[freqIx, timeIx] <- value[i]
    }
    dq
}

dq <- matrix(0, nrow=8, ncol=10)
testFreqs <- head(freqs, 9) - .5
testTimes <- head(times, 11)
markDQMatrix(dq, freqs=testFreqs, times=testTimes, 
             freqRange=list(c(13.5, 20), c(11, 11.1)),
             value=c(1, 2, 3),
             timeRange=list(testTimes[c(3,5)]+c(1, 0),
                            testTimes[c(6,8)]+c(1, 0)))

loadQuality <- function(nc) {
    if(is.character(nc)) {
        nc <- nc_open(nc)
    }
    if(!inherits(nc, 'ncdf4')) {
        warning('Not a NetCDF')
        return(NULL)
    }
    if(!'quality_flag' %in% names(nc$var)) {
        inSize <- nc$var$psd$size
        dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
    } else {
        dqMat <- ncvar_get(nc, 'quality_flag', start=c(1,1), count=c(-1, -1))
    }
    times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    list(dq=dqMat, time=times, freq=freqs)
}

longQuality <- function(x) {
    whichFreq <- PAMscapes:::whichFreqCols(x)
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq]))
    freqVals <- PAMscapes:::colsToFreqs(x)
    longSpec <- data.frame(.name=as.character(freqVals), .value='value')
    freqVals <- as.numeric(freqVals)
    longSpec$frequency <- freqVals
    longSpec$type <- type
    freqDiffs <- diff(freqVals)
    lowFreq <- freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
    
    freqDiffs <- c(lowFreq, freqDiffs)
    freqLows <- freqVals - freqDiffs
    longSpec$freqLow <- freqLows
    # the spec is to make it faster char->numeric conversion and add freqLow column
    # only need to specify here bc freqLow column
    x$UTCend <- x$UTC + unique(diff(as.numeric(x$UTC)))[1]
    x <- PAMscapes:::toLong(x, spec=longSpec)
    x
}

#### Plotters ####
ncMod <- '../Data/ncmod/modified.nc'
g <- plotLTSA(loadSoundscapeData(ncMod, keepQuals = 1:4), bin='1min', maxBins=2e3)
library(ggplot2)
dq <- loadQuality(ncMod)
labels <- dq$freq
labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
freqType <- PAMscapes:::checkFreqType(dq$freq)
labels <- paste0(freqType, '_', labels)
dqdf <- data.frame(t(dq$dq))
colnames(dqdf) <- labels
dqdf <- cbind(dq$time, dqdf)
names(dqdf)[1] <- 'UTC'
dqLong <- longQuality(dqdf)

g + 
    geom_rect(data=dqLong, aes(xmin=UTC,
                               xmax=UTCend,
                               ymin=freqLow,
                               ymax=frequency,
                               color=(value)), alpha=.5)
dq <- ggplot() +
    geom_rect(data=dqLong, aes(xmin=UTC,
                               xmax=UTCend,
                               ymin=freqLow,
                               ymax=frequency,
                               fill=as.character(value))) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_log10(expand=c(0,0))
ggplot() +
    geom_raster(data=dqLong, aes(x=UTC,
                               y=frequency,
                               fill=as.character(value)), interpolate=T) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_log10(expand=c(0,0)) +
    theme(legend.title = element_text(angle=90)) +
    guides(fill=guide_legend(title.position='right', title.hjust=.5))
library(patchwork)
g / dq
