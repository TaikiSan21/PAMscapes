#' @title Plot QAQC Data
#'
#' @description Simple plotting functions for various the various types of
#'   quality assurance / quality control (QAQC) data created by 
#'   \link{evaluateDeployment} and \link{evaluateRecordings}
#' 
#' @param x dataframe created by \link{evaluateDeployment} or
#'   \link{evaluateRecordings}
#' @param title optional title to add to plot
#' @param level which frequency band levels to display, one of "ol" 
#'   (octave level) or "tol" (third-octave level). 
#' @param dbRange range of dB (y-axis) values to plot
#' @param freqMin minimum frequency (Hz) to show on plot
#' 
#' @return a ggplot object
#' 
#' @rdname plotQAQC
#' 
#' @export
#' 
plotQAQCLevel <- function(x,
                        level=c('ol', 'tol'), 
                        dbRange=NULL,
                        freqMin=NULL,
                        title=NULL) {
    x <- toLong(x)
    if(!is.null(freqMin)) {
        x <- x[x$frequency >= freqMin, ]
    }
    level <- match.arg(level)
    plotLevels <- getOctaveLevels(level, freqRange=range(x$frequency))
    x <- x[x$frequency %in% plotLevels$freqs, ]
    x$frequency <- factor(x$frequency)
    tRange <- range(x$UTC)
    if(is.null(dbRange)) {
        dbRange <- range(x$value)
    }
    brks <- seq(from=floor(dbRange[1]/10)*10,
                to=ceiling(dbRange[2]/10)*10,
                by=10)
    g <- ggplot(x, aes(x=.data$UTC, y=.data$value, color=.data$frequency)) +
        geom_line(linewidth=0.5) +
        scale_x_datetime(limits=tRange) +
        scale_y_continuous(limits=dbRange, breaks=brks)
    if(!is.null(title)) {
        g <- g + ggtitle(title)
    }
    g
}

#' @rdname plotQAQC
#' @export
#' 
plotQAQCGap <- function(x, title=NULL) {
    x <- x[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
    names(x)[3:4] <- c('Wav End to Next File (s)',
                       'Time Between File Start (s)')
    x <- pivot_longer(x, cols=c('Wav End to Next File (s)',
                                       'Time Between File Start (s)'))
    # units opts?
    g <- ggplot(x, aes(x=.data$UTC, y=.data$value)) +
        geom_line() +
        facet_wrap(~name, scales='free', ncol=1) +
        theme(strip.text.x = element_text(size=12)) +
        ylab('Seconds')
    if(!is.null(title)) {
        g <- g + ggtitle(title)
    }
    g
}

#' @rdname plotQAQC
#' @export
#' 
plotQAQCTV <- function(x, title=NULL) {
    if(all(is.na(x$extBatt)) ||
       max(x$extBatt, na.rm=TRUE) == 0) {
        battCol1 <- 'intBatt'
        battCol2 <- NULL
    } else {
        battCol1 <- 'extBatt'
        battCol2 <- 'intBatt'
    }
    x <- rename(x, 'Temperature (C)'='temp')
    g <- plotScaledTimeseries(x, columns=c(battCol1, 'Temperature (C)', battCol2), color=c('darkblue', 'darkorange', 'blue'))
    g <- g + ylab('Battery (V)')
    if(!is.null(title)) {
        g <- g + ggtitle(title)
    }
    g
}

# #' @importFrom patchwork plot_layout plot_annotation
# #' 
# plotQuicklook <- function(x, dbRange=c(50, 140), freqMin=30, name=NULL) {
#     tol <- plotQAQCTol(x, freqMin=freqMin, dbRange=dbRange)
#     gap <- plotQAQCGap(x)
#     if(all(c('temp', 'extBatt') %in% names(x))) {
#         tv <- plotQAQCTV(x)
#     } else {
#         tv <- ggplot(data.frame(x=1, y=1, label='No Temp Volt Data')) +
#             geom_text(aes(x=x, y=y, label=label)) + 
#             theme_void()
#     }
#     layout <- '
#     AB
#     CB
#     '
#     if(is.null(name) && 'projectName' %in% names(x)) {
#         name <- x$projectName[1]
#     }
#     (tol + gap + tv) +
#         plot_layout(design=layout) +
#         plot_annotation(title=name,
#                         theme=theme(plot.title=element_text(hjust=.5)))
# }

createSpecImage <- function(clip, channel=1, wl=1024, hop=1, 
                            brightness=0, contrast=0, 
                            cmap=gray.colors(64, start=1, end=0),
                            title=NULL,
                            startTime=NULL,
                            panelLength=NULL,
                            file=NULL, ratio=2) {
    gram <- wavToSpecgram(clip[channel, ], sr=clip$rate, wl=wl, hop=hop, axes=TRUE)
    # if making one long plot
    if(is.null(panelLength)) {
        plotSpectrogram(gram, file=file, startTime=startTime, cmap=cmap, title=title)
        return()
    }
    # otherwise plan the breaking into subplots
    plotIx <- floor(gram$x / panelLength)
    nPlots <- length(unique(plotIx))
    oneWidth <- sum(plotIx == plotIx[1])
    if(!is.null(file)) {
        png(filename=file, width=oneWidth/ratio, height=nPlots * length(gram$y)/ratio, units='px')
        par(mgp=c(3, 2, 0))
        on.exit(dev.off())
    }
    oPar <- par()$mfrow
    par(mfrow=c(nPlots, 1))
    on.exit(par(mfrow=oPar), add=TRUE, after=FALSE)
    
    for(i in unique(plotIx)) {
        thisGram <- gram
        thisGram$mat <- thisGram$mat[plotIx == i, ]
        thisGram$x <- thisGram$x[plotIx == i]
        plotSpectrogram(thisGram, file=NULL, startTime=startTime, cmap=cmap, title=title, ratio=ratio)
        
    }
}

#' @importFrom graphics axis image par
#' @importFrom grDevices dev.off gray.colors png
#'
# handles rescaling, coloring, axis labeling, plotting
plotSpectrogram <- function(x, file=NULL, cmap=gray.colors(64, start=1, end=0), startTime=NULL, title=NULL, ratio=2) {
    q <- c(.01, .99)
    lim <- quantile(x$mat, q, na.rm=TRUE)
    x$mat[x$mat < lim[1]] <- lim[1]
    x$mat[x$mat > lim[2]] <- lim[2]
    x$mat <- x$mat / diff(range(x$mat, na.rm=TRUE)) * 255
    x$mat <- x$mat - min(x$mat, na.rm=TRUE)
    x$mat[x$mat > 255] <- 255
    x$mat[x$mat < 0] <- 0
    if(!is.null(file)) {
        png(filename=file, width=nrow(x$mat)/ratio, height=ncol(x$mat)/ratio, units='px')
        par(mgp=c(3, 2, 0))
        on.exit(dev.off())
    }
    image(x$mat, col = cmap, xaxt='n', yaxt='n',
          useRaster=TRUE,
          breaks=seq(from=0, to=255, length.out=length(cmap)+1),
          ylab='Frequency (kHz)')
    if(!is.null(title)) {
        title(title, cex.main=6/ratio)
    }
    tLabs <- seq(from=floor(min(x$x) / 60), to=ceiling(max(x$x) / 60), by=1)
    tLocs <- (tLabs - floor(min(x$x) / 60)) / ceiling(diff(range(x$x)) / 60)
    if(is.null(startTime)) {
        tLabs <- paste0(tLabs, 'min')
    } else {
        tLabs <- startTime + tLabs * 60
    }
    axis(1, at=tLocs, labels=tLabs, cex.axis=4/ratio)
    yPretty <- pretty(x$y, n=5)
    axis(2, at=(yPretty-min(x$y))/diff(range(x$y)), labels = yPretty, cex.axis=4/ratio)
}

#' @importFrom signal hanning
#' @importFrom PAMmisc pwelch
#' 
wavToSpecgram <- function(wav, sr=NULL, wl=1024, hop=.5, axes=FALSE, ...) {
    if(hop <= 1) {
        hop <- wl * hop
    }
    window <- hanning(wl)
    # planFF <- fftw::planFFT(wl)
    nSlices <- ceiling((length(wav) - wl)/(hop)) + 1
    slices <- seq(from=1, by=hop, length.out=nSlices)
    mat <- t(apply(as.matrix(slices), 1, function(s) {
        thisWav <- wav[s:(s+wl-1)]
        thisWav[is.na(thisWav)] <- 0
        thisWav <- thisWav - mean(thisWav)
        if(all(thisWav == 0)) {
            return(rep(NA, wl/2))
        }
        thisGram <- pwelch(thisWav,nfft=wl, noverlap=0, sr=sr, window=window, demean='none')
        10*log10(thisGram$spec)
    }))
    if(!axes) {
        return(mat)
    }
    yAxis <- (1:wl) / wl * sr / 1e3
    xAxis <- (slices-1) / sr
    list(mat=mat, x=xAxis, y=yAxis)
}
