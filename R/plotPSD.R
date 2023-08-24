#' @title Plot Power Spectral Density
#'
#' @description Plots the distribution of summarised sound levels
#'   across frequency, either as lines of quantile levels or a
#'   heatmap showing the full distribution
#'
#' @param x a dataframe
#' @param style character specifying plot style to create, either
#'   "quantile", "density", or a vector with both
#' @param scale scale to use for frequency axis, one of "log" or "linear"
#' @param q quantile to plot
#' @param color color for quantile
#' @param freqRange range of frequencies to plot
#' @param dbRange range of dB values to plot
#' @param dbInt bin interval size for density plot
#' @param units units for dB axis of plot
#' @param cmap color map to use for density plot
#' @param title optional title for plot
#'
#' @return a ggplot object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom graphics hist
#'
#' @export
#'
plotPSD <- function(x, style=c('quantile', 'density'),
                    scale=c('log', 'linear'),
                    q=.5, color='black',
                    freqRange=NULL,
                    dbRange=NULL, dbInt=1,
                    units='dB re: 1uPa^2/Hz',
                    cmap=viridis_pal()(25),
                    title=NULL) {
    x <- checkSoundscapeInput(x)
    if(isLong(x)) {
        x <- toWide(x)
    }
    scale <- switch(match.arg(scale),
                    'log' = 'log10',
                    'linear' = 'identity')
    freqs <- as.numeric(gsub('[A-z]+_', '', colnames(x)[2:ncol(x)]))
    if(!is.null(freqRange)) {
        goodFreqs <- freqs >= freqRange[1] & freqs <= freqRange[2]
        goodIx <- 1 + which(goodFreqs)
        x <- x[c(1, goodIx)]
        freqs <- as.numeric(gsub('[A-z]+_', '', colnames(x)[2:ncol(x)]))
    }
    if(is.null(freqRange)) {
        freqRange <- range(freqs)
    }
    dbMin <- floor(min(x[2:ncol(x)], na.rm=TRUE))
    dbMax <- ceiling(max(x[2:ncol(x)], na.rm=TRUE))
    if(is.null(dbRange)) {
        dbRange <- c(dbMin, dbMax)
    }
    g <- ggplot()
    if('density' %in% style) {

        dbVals <- seq(from=dbMin, to=dbMax, by=dbInt)
        counts <- apply(x[2:ncol(x)], 2, function(y) {
            hist(y, breaks=dbVals, plot=FALSE)$counts
        })
        freqDiffs <- diff(freqs)
        lowFreq <- switch(scale,
                          'log10' = {
                              freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
                          },
                          'identity' = freqDiffs[1]
        )
        freqDiffs <- c(lowFreq, freqDiffs)
        names(freqDiffs) <- as.character(freqs)
        minAllowed <- ifelse(scale=='log', 1.1, .1)
        freqLow <- freqs - freqDiffs
        freqLow <- ifelse(freqLow < minAllowed, minAllowed, freqLow)
        # browser()
        dbHigh <- dbVals[-1]
        dbLow <- dbHigh - dbInt
        densityData <- data.frame(dbLow = rep(dbLow, length(freqs)),
                                  dbHigh = rep(dbHigh, length(freqs)),
                                  frequency = rep(freqs, each=length(dbHigh)),
                                  freqLow = rep(freqLow, each=length(dbHigh)),
                                  count=as.vector(counts) / nrow(x))
        if(scale == 'log10' && any(freqs == 0)) {
            warning('Cannot plot "0" frequency on log scale, these are removed.')
            densityData <- densityData[densityData$frequency > 0, ]
        }
        densityData$count[densityData$count == 0] <- NA
        g <- g +
            geom_rect(data=densityData,
                      aes(xmin=.data$freqLow,
                          xmax=.data$frequency,
                          ymin=.data$dbLow,
                          ymax=.data$dbHigh,
                          fill=.data$count)) +
            scale_fill_gradientn(colors=cmap, na.value = 'transparent')
    }
    if('quantile' %in% style) {
        # quantiles <- apply(x[2:ncol(x)], 2, function(y) quantile(y, q))
        # qNames <- paste0(round(q*100, 0), '%')
        #
        # quantData <- data.frame(quantile=rep(qNames, length(freqs)),
        #                         frequency=rep(freqs, each=length(qNames)),
        #                         value=as.vector(quantiles))
        # if(scale == 'log10' && any(freqs == 0)) {
        #     warning('Cannot plot "0" frequency on log scale, these are removed.')
        #     quantData <- quantData[quantData$frequency > 0, ]
        # }
        # cpal <- checkCpal(cpal, length(qNames))
        # names(cpal) <- qNames
        # g <- g +
        #     geom_line(data=quantData,
        #               aes(x=.data$frequency,
        #                   y=.data$value,
        #                   color=.data$quantile), lwd=1) +
        #     scale_color_manual(values=cpal)
        #### q ribbon style
        if(q == 0) {
            q <- c(.5, .5, .5)
        }
        if(length(q) == 1) {
            q <- c(q, .5, 1-q)
        }
        if(length(q) == 2) {
            q <- c(q, .5)
        }
        if(length(q) != 3) {
            stop('"q" must be length 1 or 2')
        }
        q <- sort(q)
        quantiles <- apply(x[2:ncol(x)], 2, function(y) quantile(y, q, na.rm=TRUE))
        quantData <- data.frame(frequency = freqs,
                                qlow = quantiles[1, ],
                                qmed = quantiles[2, ],
                                qhigh = quantiles[3, ])
        if(scale == 'log10' && any(freqs == 0)) {
            warning('Cannot plot "0" frequency on log scale, these are removed.')
            quantData <- quantData[quantData$frequency > 0, ]
        }
        g <- g +
            geom_line(data=quantData, aes(x=.data$frequency, y=.data$qmed), color=color, lwd=1) +
            geom_ribbon(data=quantData, aes(x=.data$frequency, ymin=.data$qlow, ymax=.data$qhigh), fill=color, alpha=.1)
    }
    g <- g +
        scale_y_continuous(expand=c(0, 0), limits=dbRange) +
        ggtitle(title) +
        labs(x='Frequency (Hz)', color='Quantile', fill='Density', y=units)
    if(scale == 'log10') {
        freqs <- freqs[freqs != 0]
        g <- myLog10Scale(g, range(freqs), dim='x')
    } else {
        g <- g +
            scale_x_continuous(expand=c(0, 0))
    }
    g
}