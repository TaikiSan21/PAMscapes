#' @title Plot Power Spectral Density
#'
#' @description Plots the distribution of summarised sound levels
#'   across frequency, either as lines of quantile levels or a
#'   heatmap showing the full distribution. Multiple PSD sources
#'   can be combined and plotted as long as they have identical
#'   frequency levels.
#'
#' @details \code{prepPSDData} is called by the plotting code, and
#'   does not necessarily need to be called separately from
#'   \code{plotPSD}. Loading PSD data can be time consuming, so
#'   it may be useful to load the data first, then it is easier
#'   to spend time adjusting plot settings.
#'
#'   The output of \code{prepPSDData} is a list with 5 elements:
#'   \itemize{
#'      \item{frequency}{ - the frequency values of the input data}
#'      \item{freqRange}{ - the value of the "freqRange" parameter if
#'        it was supplied}
#'      \item{dbVals}{ - the dB values of breakpoints used for "density"
#'        plotting}
#'      \item{quantileData}{ - the data used for quantile plots. These
#'        are stored as "tidgest" objects serialized using
#'        \link[tdigest]{as.list.tdigest}, from which quantiles can
#'        be computed}
#'      \item{densityData}{ - the data used fro quantile plots. These
#'        are stored as a matrix of bin counts - each column corresponds
#'        to the "frequency" output, each row corresponds to bins defined
#'        using "dbVals" as boundaries}
#'   }
#'
#' @param x a dataframe or list of dataframes, or file path or vector
#'   of file paths, or the output from \code{prepPSDData}
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
#' @param progress logical flag to show progress bar
#'
#' @return a ggplot object for \code{plotPSD}, see details for \code{prepPSDData}
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
                    title=NULL,
                    progress=TRUE) {
    scale <- match.arg(scale)
    plotData <- prepPSDData(x, freqRange=freqRange, style=style, dbInt=dbInt, progress=progress)
    g <- ggplot()
    if('density' %in% style) {
        plotDensity <- formatDensityData(plotData,
                                         freqRange = freqRange,
                                         scale=scale)
        g <- g +
            geom_rect(data=plotDensity,
                      aes(xmin=.data$freqLow,
                          xmax=.data$frequency,
                          ymin=.data$dbLow,
                          ymax=.data$dbHigh,
                          fill=.data$count)) +
            scale_fill_gradientn(colors=cmap, na.value = 'transparent')
    }
    if('quantile' %in% style) {
        plotQuant <- formatQuantileData(plotData,
                                        q=q,
                                        freqRange=freqRange,
                                        scale=scale)
        g <- g +
            geom_line(
                data=plotQuant,
                aes(x=.data$frequency, y=.data$qmed), color=color, lwd=1) +
            geom_ribbon(
                data=plotQuant,
                aes(x=.data$frequency, ymin=.data$qlow, ymax=.data$qhigh), fill=color, alpha=.1)
    }

    g <- g +
        scale_y_continuous(expand=c(0, 0), limits=dbRange) +
        ggtitle(title) +
        labs(x='Frequency (Hz)', color='Quantile', fill='Density', y=units)
    if(is.null(freqRange)) {
        freqRange <- range(plotData$frequency)
        if('density' %in% style) {
            freqRange[1] <- min(plotDensity$freqLow)
        }
    }
    if(is.infinite(freqRange[2])) {
        freqRange[2] <- max(plotData$frequency)
    }
    if(scale == 'log') {
        if(freqRange[1] == 0) {
            freqRange[1] <- .9
        }
        g <- myLog10Scale(g, freqRange, dim='x')
    } else {
        g <- g +
            scale_x_continuous(expand=c(0, 0))
    }
    g
}

#' @importFrom tdigest tdigest td_merge as_tdigest
#' @importFrom purrr reduce
#' @rdname plotPSD
#' @export
#'
prepPSDData <- function(x, freqRange=NULL, style=c('density', 'quantile'),
                        dbInt=1, progress=TRUE) {
    # if input is already prepped data
    if(is.list(x) &&
       all(c('frequency', 'freqRange', 'quantileData', 'densityData') %in% names(x))) {
        return(x)
    }
    # jank to handle multiple file inputs, dataframe case
    if(is.data.frame(x)) {
        x <- list(x)
    }
    if(length(x) == 1) {
        progress <- FALSE
    }
    dbVals <- seq(from=0, to=200, by=dbInt)
    densityData <- NULL
    quantileData <- NULL
    if(progress) {
        cat('Prepping PSD data...\n')
        pb <- txtProgressBar(min=0, max=length(x), style=3)
    }
    for(f in seq_along(x)) {
        data <- checkSoundscapeInput(x[[f]])
        if(isLong(data)) {
            data <- toWide(data)
        }
        freqs <- as.numeric(gsub('[A-z]+_', '', colnames(data)[2:ncol(data)]))
        if(!is.null(freqRange)) {
            goodFreqs <- freqs >= freqRange[1] & freqs <= freqRange[2]
            goodIx <- 1 + which(goodFreqs)
            data <- data[c(1, goodIx)]
            freqs <- as.numeric(gsub('[A-z]+_', '', colnames(data)[2:ncol(data)]))
        }
        # checking compatibility of all file freq vals
        if(f == 1) {
            firstFreq <- freqs
        }
        # check for mismatching frequency levels - cant combine
        isDiff <- isFreqDiff(firstFreq, freqs, f)
        if(isDiff) {
            if(progress) {
                setTxtProgressBar(pb, value=f)
            }
            next
        }
        if('density' %in% style) {
            # hist count adding per
            # return counts @ frequency per each dbBin, then sum counts over all files
            thisDensity <- prepDensityData(data, dbVals)
            if(f == 1) {
                densityData <- thisDensity
            } else {
                densityData <- densityData + thisDensity
            }
        }
        if('quantile' %in% style) {
            #tdigest quantile adding per
            # return tdigest @ frequency for each, then td_merge them into one
            # then quantile of the merged tds
            thisQuantile <- prepQuantileData(data, q)
            if(f == 1) {
                quantileData <- thisQuantile
            } else {
                for(qFreq in seq_along(quantileData)) {
                    td_merge(
                        thisQuantile[[qFreq]],
                        quantileData[[qFreq]]
                    )
                }
            }
        }
        if(progress) {
            setTxtProgressBar(pb, value=f)
        }
    }
    if('quantile' %in% style) {
        # have to turn from pointer to list before saving
        for(q in seq_along(quantileData)) {
            quantileData[[q]] <- as.list(quantileData[[q]])
        }
    }
    rm(data)
    gc()
    list(frequency=firstFreq, freqRange=freqRange, dbVals=dbVals,
         quantileData=quantileData, densityData=densityData)
}

formatDensityData <- function(x, frequency=NULL, freqRange=NULL, dbVals=NULL, scale) {
    if(is.list(x)) {
        if(is.null(frequency)) {
            frequency <- x$frequency
        }
        if(is.null(freqRange)) {
            freqRange <- x$freqRange
        }
        if(is.null(dbVals)) {
            dbVals <- x$dbVals
        }
        x <- x$densityData
    }
    nCounts <- colSums(x)
    nCounts[nCounts == 0] <- 1
    for(i in 1:ncol(x)) {
        x[, i] <- x[, i] / nCounts[i]
    }
    freqDiffs <- diff(frequency)
    lowFreq <- switch(scale,
                      'log' = {
                          freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
                      },
                      'linear' = freqDiffs[1]
    )
    freqDiffs <- c(lowFreq, freqDiffs)
    freqLow <- frequency - freqDiffs
    minAllowed <- ifelse(scale=='log', .9, .1)
    freqLow <- ifelse(freqLow < minAllowed, minAllowed, freqLow)
    dbHigh <- dbVals[-1]
    dbLow <- dbHigh - diff(dbVals)[1]

    densityData <- data.frame(dbLow = rep(dbLow, length(frequency)),
                              dbHigh = rep(dbHigh, length(frequency)),
                              frequency = rep(frequency, each=length(dbHigh)),
                              freqLow = rep(freqLow, each=length(dbHigh)),
                              count=as.vector(x))
    if(!is.null(freqRange)) {
        densityData <- densityData[
            densityData$frequency >= freqRange[1] &
                densityData$frequency <= freqRange[2], ]
    }
    if(scale == 'log' && any(densityData$frequency == 0)) {
        warning('Cannot plot "0" frequency on log scale, these are removed.', call.=FALSE)
        densityData <- densityData[densityData$frequency > 0, ]
    }
    densityData <- densityData[densityData$count > 0, ]
    densityData
}

formatQuantileData <- function(x, q, frequency=NULL, freqRange=NULL, scale) {
    if(is.list(x)) {
        if(is.null(frequency)) {
            frequency <- x$frequency
        }
        if(is.null(freqRange)) {
            freqRange <- x$freqRange
        }
        x <- x$quantileData
    }
    q <- checkQuantile(q)
    # change back to tdigest from list we made earlier
    for(i in seq_along(x)) {
        x[[i]] <- as_tdigest(x[[i]])
    }
    quantiles <- sapply(x, function(x) {
        quantile(x, q)
    })

    quantileData <- data.frame(
        frequency = frequency,
        qlow = quantiles[1, ],
        qmed = quantiles[2, ],
        qhigh = quantiles[3, ])
    if(!is.null(freqRange)) {
        quantileData <- quantileData[
            quantileData$frequency >= freqRange[1] &
                quantileData$frequency <= freqRange[2], ]
    }
    if(scale == 'log' && any(quantileData$frequency == 0)) {
        warning('Cannot plot "0" frequency on log scale, these are removed.', call.=FALSE)
        quantileData <- quantileData[quantileData$frequency > 0, ]
    }
    quantileData
}

prepDensityData <- function(x, dbVals) {
    nOOB <- 0
    counts <- apply(x[2:ncol(x)], 2, function(y) {
        # findInterval will put stuff outside range into outer bins, so need to filter
        inBounds <- y >= dbVals[1] & y <= dbVals[length(dbVals)]
        naVals <- is.na(inBounds)
        inBounds[naVals] <- FALSE
        # dont count NA vals as OOB
        newOOB <- max(sum(!inBounds) - sum(naVals), 0)
        nOOB <<- nOOB + newOOB
        y <- y[inBounds]
        tabulate(findInterval(y, vec=dbVals), nbins=length(dbVals)-1)
    })
    if(nOOB > 0) {
        warning(nOOB, ' values were out of dbRange (',
                dbVals[1], '-', max(dbVals), ')')
    }
    counts
}

prepQuantileData <- function(x, q=.1) {
    qtd <- apply(x[2:ncol(x)], 2, tdigest)
    qtd
}

isFreqDiff <- function(x, y, ix=1) {
    if(ix == 1) {
        return(FALSE)
    }
    if(setequal(x, y)) {
        return(FALSE)
    }
    msg <- paste0('Frequency values in file ', ix, ' are not identical. Cannot be combined with others.\n')
    missing <- setdiff(x, y)
    extra <- setdiff(y, x)
    if(length(missing) > 0) {
        msg <- paste0(msg, 'Missing values ', paste0(missing, collapse=', '), '\n')
    }
    if(length(extra) > 0) {
        msg <- paste0(msg, 'Extra values ', paste0(extra, collapse=', '))
    }
    warning(msg, call.=FALSE)
    TRUE
}

checkQuantile <- function(q) {
    if(length(q) == 1) {
        if(q %in% c(0, 1)) {
            q <- c(.5, .5, .5)
        } else {
            q <- c(q, .5, 1-q)
        }
    }
    if(length(q) == 2) {
        q <- c(q, .5)
    }
    if(length(q) != 3) {
        stop('"q" must be length 1 or 2')
    }
    q <- sort(q)
    q
}
