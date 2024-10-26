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
#'   \describe{
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
#' @param densityRange optional range of values for density color scale
#' @param units units for dB axis of plot
#' @param cmap color map to use for density plot
#' @param by optional column to plot different quantile lines by, only affects
#'   \code{style='quantile'}. If \code{x} is a data.frame, \code{by} can also
#'   be one of \code{'hour'}, \code{'month'}, or \code{'year'} and that column
#'   will be created automatically if not present.
#' @param referenceLevel only used together with \code{by}. A value of the
#'   \code{by} column to use as a reference for all other levels. The plot
#'   will then show the difference between the other levels and the reference
#' @param facet optional column to facet the plots by
#' @param ncol number of columns to use when plotting with \code{facet}
#' @param compression compression factor for \link[tdigest]{tdigest}, lower
#'   values are less accurate but will compute faster. Only relevant for
#'   \code{style='quantile'} when loading and combining multiple datasets
#' @param title optional title for plot
#' @param returnData if \code{TRUE} then no plot will be generated, instead the
#'   dataframe that would normally be used to make the plot will be returned
#' @param progress logical flag to show progress bar
#'
#' @return a ggplot object for \code{plotPSD}, see details for \code{prepPSDData}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#'
#' psd <- loadSoundscapeData(system.file('extdata/PSDSmall.csv', package='PAMscapes'))
#' # Plotting only first 1000 columns for brevity
#' plotPSD(psd[1:1000], style='density')
#' plotPSD(psd[1:1000], style='quantile', q=.05)
#'
#' @importFrom graphics hist
#' @importFrom scales hue_pal squish
#' @importFrom lubridate hour month year
#'
#' @export
#'
plotPSD <- function(x, style=c('quantile', 'density'),
                    scale=c('log', 'linear'),
                    q=.5, color='black',
                    freqRange=NULL,
                    dbRange=NULL, dbInt=1,
                    densityRange=NULL,
                    units='dB re: 1uPa^2/Hz',
                    cmap=viridis_pal()(25),
                    by=NULL,
                    referenceLevel=NULL,
                    facet=NULL,
                    ncol=NULL,
                    title=NULL,
                    returnData=FALSE,
                    progress=TRUE) {
    scale <- match.arg(scale)
    if(!is.null(dbRange) &&
       length(dbRange) > 2) {
        dbRange <- range(dbRange, na.rm=TRUE)
    }
    if(isTRUE(returnData) &&
       length(style) != 1) {
        warning('Can only return data for a single "style", select ',
                'one of either "quantile" or "density".')
        return(NULL)
    }
    if(!is.null(by)) {
        if('density' %in% style) {
            warning('Plots with "by" can only show quantile, not density')
            style <- 'quantile'
        }
        # have builtin options for by without needing column
        if(is.data.frame(x) &&
           by %in% c('hour', 'month', 'year') &&
           !by %in% colnames(x)) {
            x[[by]] <- switch(by,
                              'hour' = hour(x$UTC),
                              'month' = month(x$UTC),
                              'year' = year(x$UTC)
            )
        }
        if(is.data.frame(x) &&
           !by %in% colnames(x)) {
            warning('"by" column not present in "x"')
            by <- NULL
        }
    }
    if(!is.null(facet) &&
       is.data.frame(x) &&
       !facet %in% colnames(x)) {
        warning('"facet" column not present in "x"')
        facet <- NULL
    }
    g <- ggplot()
    justOneDf <- is.data.frame(x)
    # way faster to skip the "prep" step if we take in a df so
    # that has a special case here. Avoids the problem of sending
    # copies of large "x" to other functions
    if(isTRUE(justOneDf)) {
        freqCols <- whichFreqCols(names(x))
        freqVals <- colsToFreqs(names(x)[freqCols])
        if(!is.null(freqRange)) {
            goodFreqs <- freqVals >= freqRange[1] & freqVals <= freqRange[2]
            goodIx <- which(goodFreqs)
            freqCols <- freqCols[goodIx]
            freqVals <- freqVals[goodIx]
        }
        if(is.null(freqRange)) {
            freqRange <- range(freqVals)
        }
        if(is.infinite(freqRange[2])) {
            freqRange[2] <- max(x$frequency)
        }
        
        if(!is.null(by) || !is.null(facet)) {
            splitCols <- list()
            if(!is.null(by)) {
                splitCols[[length(splitCols)+1]] <- x[[by]]
            }
            if(!is.null(facet)) {
                splitCols[[length(splitCols)+1]] <- x[[facet]]
            }
            x <- split(x, splitCols)
        }
        if('density' %in% style) {
            dbVals <- seq(from=0, to=200, by=dbInt)
            if(is.null(facet)) {
                denData <- prepDensityData(x[freqCols], dbVals=dbVals)
                denData <- formatDensityData(denData, 
                                             frequency=freqVals, 
                                             freqRange=freqRange, 
                                             dbVals=dbVals, 
                                             scale=scale)
            } else {
                denData <- bind_rows(lapply(x, function(b) {
                    thisDen <- prepDensityData(b[freqCols], dbVals=dbVals)
                    thisDen <- formatDensityData(thisDen, 
                                                 frequency=freqVals, 
                                                 freqRange=freqRange, 
                                                 dbVals=dbVals,
                                                 scale=scale)
                    thisDen$facet <- b[[facet]][1]
                    thisDen
                }))
            }
            if(isTRUE(returnData) && length(style) == 1) {
                return(denData)
            }
            g <- g +
                geom_rect(data=denData,
                          aes(xmin=.data$freqLow,
                              xmax=.data$frequency,
                              ymin=.data$dbLow,
                              ymax=.data$dbHigh,
                              fill=.data$count)) +
                labs(fill='Density')
            if(!is.null(densityRange) &&
               length(densityRange) != 2) {
                warning('"densityRange" must be 2 values')
                densityRange <- NULL
            }
            if(is.null(densityRange)) {
                g <- g +
                    scale_fill_gradientn(colors=cmap, na.value = 'transparent')
            } else {
                g <- g +
                    scale_fill_gradientn(colors=cmap, na.value='transparent',
                                         limits=densityRange,
                                         oob=squish)
            }
        }
        if('quantile' %in% style) {
            q <- checkQuantile(q)
            if(!is.null(by) || !is.null(facet)) {
                qData <- bind_rows(lapply(x, function(b) {
                    if(is.null(b) || nrow(b) == 0) {
                        return(NULL)
                    }
                    result <- bind_rows(lapply(b[freqCols], function(col) {
                        result <- quantile(col, q, na.rm=TRUE)
                        names(result) <- c('qlow', 'qmed', 'qhigh')
                        result
                    }))
                    if(!is.null(by)) {
                        result$by <- b[[by]][1]
                    }
                    if(!is.null(facet)) {
                        result$facet <- b[[facet]][1]
                    }
                    result$nBy <- nrow(b)
                    result$frequency <- freqVals
                    result
                }))
                
            } else {
                qData <- bind_rows(lapply(x[freqCols], function(col) {
                    result <- quantile(col, q, na.rm=TRUE)
                    names(result) <- c('qlow', 'qmed', 'qhigh')
                    result
                }))
                qData$frequency <- freqVals
            }
            if(!is.null(by) && !is.null(referenceLevel)) {
                if(!referenceLevel %in% qData$by) {
                    warning('Reference level "', referenceLevel, 
                            '" not found in column "', by, '"')
                } else{
                    qData <- bind_rows(lapply(split(qData, qData$by), function(d) {
                        d$qmed <- d$qmed - qData$qmed[qData$by == referenceLevel]
                        d$qlow <- d$qlow - qData$qmed[qData$by == referenceLevel]
                        d$qhigh <- d$qhigh - qData$qmed[qData$by == referenceLevel]
                        d
                    }))
                }
            }
            # x is frequency, qlow, qmed, qhigh
            if(isTRUE(returnData) && length(style) == 1) {
                return(qData)
            }
            if(isTRUE(returnData) && length(style) == 2) {
                return(
                    list(densityData=denData,
                         quantileData=qData)
                )
            }
            g <- addQuantilePlot(g, x=qData, by=by, color=color)
        }
    } # end justOneDf
    if(!is.list(x) ||
       !all(c('frequency', 'freqRange', 'quantileData', 'densityData') %in% names(x)) &&
       !justOneDf) {
        x <- prepPSDData(x, freqRange=freqRange, style=style, dbInt=dbInt, by=by, progress=progress)
    }
    
    if('density' %in% style &&
       !justOneDf) {
        x$densityData <- formatDensityData(x$densityData,
                                           frequency = x$frequency,
                                           freqRange = x$freqRange,
                                           dbVals=x$dbVals,
                                           scale=scale)
        if(isTRUE(returnData)) {
            return(x$densityData)
        }
        g <- g +
            geom_rect(data=x$densityData,
                      aes(xmin=.data$freqLow,
                          xmax=.data$frequency,
                          ymin=.data$dbLow,
                          ymax=.data$dbHigh,
                          fill=.data$count)) +
            labs(fill='Density')
        if(!is.null(densityRange) &&
           length(densityRange) != 2) {
            warning('"densityRange" must be 2 values')
            densityRange <- NULL
        }
        if(is.null(densityRange)) {
            g <- g +
                scale_fill_gradientn(colors=cmap, na.value = 'transparent')
        } else {
            g <- g +
                scale_fill_gradientn(colors=cmap, na.value='transparent',
                                     limits=densityRange,
                                     oob=squish)
        }
    }
    if('quantile' %in% style &&
       !justOneDf) {
        x$quantileData <- formatQuantileData(x$quantileData,
                                             frequency = x$frequency,
                                             q=q,
                                             freqRange=x$freqRange,
                                             scale=scale)
        if(isTRUE(returnData)) {
            return(x$quantileData)
        }
        g <- addQuantilePlot(g, x=x$quantileData, by=by, color=color)
    }
    
    g <- g +
        scale_y_continuous(expand=c(0, 0), limits=dbRange) +
        ggtitle(title) +
        labs(x='Frequency (Hz)', color='Quantile', y=units)
    if(is.null(freqRange)) {
        freqRange <- range(x$frequency)
        if('density' %in% style) {
            freqRange[1] <- min(x$densityData$freqLow)
        }
    }
    if(is.infinite(freqRange[2])) {
        freqRange[2] <- max(x$frequency)
    }
    if(scale == 'log') {
        if(freqRange[1] == 0) {
            freqRange[1] <- .9
        }
        g <- myLog10Scale(g, freqRange, dim='x')
    } else {
        g <- g +
            scale_x_continuous(expand=c(0, 0), limits=freqRange)
    }
    if(!is.null(facet)) {
        g <- g +
            facet_wrap(~facet, ncol=ncol)
    }
    g
}

#' @importFrom tdigest tdigest td_merge as_tdigest
#' @importFrom purrr reduce
#' @rdname plotPSD
#' @export
#'
prepPSDData <- function(x, freqRange=NULL, style=c('density', 'quantile'),
                        by=NULL, dbInt=1, compression=10e3, progress=TRUE) {
    # if input is already prepped data
    if(is.list(x) &&
       all(c('frequency', 'freqRange', 'quantileData', 'densityData') %in% names(x))) {
        return(x)
    }
    # jank to handle multiple file inputs, dataframe case
    justDf <- FALSE
    if(is.data.frame(x)) {
        justDf <- TRUE
        progress <- FALSE
    }
    if(is.list(x) && length(x) == 1) {
        progress <- FALSE
    }
    dbVals <- seq(from=0, to=200, by=dbInt)
    densityData <- NULL
    quantileData <- NULL
    if(progress) {
        cat('Prepping PSD data...\n')
        pb <- txtProgressBar(min=0, max=length(x), style=3)
    }
    # need a way to deal with problem that midway through files
    # the "by" column disappears and our grouping is janked
    resetBy <- FALSE
    nSeq <- ifelse(justDf, 1, length(x))
    for(f in seq_len(nSeq)) {
        if(justDf) {
            data <- x
        } else {
            data <- loadSoundscapeData(x[[f]])
        }
        if(isLong(colnames(data))) {
            data <- toWide(data)
        }
        if(!is.null(by) &&
           !by %in% colnames(data)) {
            warning('"by" column not present in data index ', f,
                    ', "by" grouping will be removed.')
            by <- NULL
            resetBy <- TRUE
        }
        if(!is.null(by)) {
            byVals <- data[[by]]
        }
        freqCols <- whichFreqCols(colnames(data))
        freqs <- as.numeric(gsub('[A-z]+_', '', colnames(data)[freqCols]))
        # data <- data[c(1, freqCols)]
        data <- data[freqCols]
        
        if(!is.null(freqRange)) {
            goodFreqs <- freqs >= freqRange[1] & freqs <= freqRange[2]
            goodIx <- which(goodFreqs)
            data <- data[c(goodIx)]
            freqCols <- whichFreqCols(colnames(data))
            # freqs <- as.numeric(gsub('[A-z]+_', '', colnames(data)))
            freqs <- colsToFreqs(colnames(data))
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
        if(!is.null(by)) {
            data <- split(data, byVals)
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
            thisQuantile <- prepQuantileData(data, by=!is.null(by), compression=compression)
            if(f == 1) {
                quantileData <- thisQuantile
                next
            }
            # output will be list(by=list(tdigest(freq)))
            if(!is.null(by)) {
                for(b in names(thisQuantile)) {
                    # if new level its baseline
                    if(!b %in% names(quantileData)) {
                        quantileData[[b]] <- thisQuantile[[b]]
                        next
                    }
                    # otherwise smash
                    for(qFreq in seq_along(quantileData[[b]])) {
                        td_merge(
                            thisQuantile[[b]][[qFreq]],
                            quantileData[[b]][[qFreq]]
                        )
                    }
                }
            } else { # no "by"
                # need to reset qd from list(by(freq)) to list(freq)
                if(isTRUE(resetBy)) {
                    resetBy <- FALSE
                    for(b in seq_along(quantileData)) {
                        if(b == 1) {
                            next
                        }
                        # merge all into [[1]]
                        for(qFreq in seq_along(quantileData[[1]])) {
                            td_merge(
                                quantileData[[b]][[qFreq]],
                                quantileData[[1]][[qFreq]]
                            )
                        }
                    }
                    quantileData <- quantileData[[1]]
                }
                
                for(qFreq in seq_along(quantileData)) {
                    td_merge(
                        thisQuantile[[qFreq]],
                        quantileData[[qFreq]]
                    )
                }
            }
        }
        # just forcing these bc PSD can be large
        rm(data)
        gc()
        if(progress) {
            setTxtProgressBar(pb, value=f)
        }
    }
    if('quantile' %in% style) {
        # have to turn from pointer to list before saving
        for(q in seq_along(quantileData)) {
            if(is.null(by)) {
                quantileData[[q]] <- as.list(quantileData[[q]])
            } else {
                for(b in seq_along(quantileData[[q]])) {
                    quantileData[[q]][[b]] <- as.list(quantileData[[q]][[b]])
                }
            }
        }
    }
    
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
    # this is input format from prepQuantileData
    if(is.list(x) &&
       all(c('frequency', 'freqRange', 'quantileData') %in% names(x))) {
        if(is.null(frequency)) {
            frequency <- x$frequency
        }
        if(is.null(freqRange)) {
            freqRange <- x$freqRange
        }
        x <- x$quantileData
    }
    # case when by
    if(!inherits(x[[1]], 'tdigest_list')) {
        return(
            bind_rows(
                lapply(x,
                       function(b) {
                           formatQuantileData(b, q=q, frequency=frequency, freqRange=freqRange, scale=scale)
                       }
                ), .id='by'
            )
        )
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
# input here is only frequency columns
prepDensityData <- function(x, dbVals) {
    nOOB <- 0
    # counts <- apply(x[2:ncol(x)], 2, function(y) {
    counts <- sapply(x, function(y) {
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

prepQuantileData <- function(x, by=FALSE, compression=10e3) {
    # if splitting "by" this will come in as
    # result of split(x, by) instead of DF x
    if(isTRUE(by)) {
        return(
            lapply(x, function(b) {
                lapply(b, tdigest, compression=compression)
            })
        )
    }
    lapply(x, tdigest, compression=compression)
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

addQuantilePlot <- function(g=NULL, x, by=NULL, color='black') {
    if(is.null(g)) {
        g <- ggplot()
    }
    if(!is.null(by)) {
        if(is.numeric(x$by)) {
            x$by <- factor(x$by, levels=sort(unique(x$by)))
        }
        if(is.character(x$by) ||
           is.logical(x$by)) {
            x$by <- as.factor(x$by)
        }
        if('nBy' %in% colnames(x)) {
            nLevs <- ungroup(
                summarise(
                    group_by(
                        distinct(x[c('by', 'nBy')]), .data$by
                    ),
                    nBy = sum(.data$nBy)
                )
            )
            nLevs <- left_join(data.frame(by=levels(x$by)),
                               nLevs,
                               by='by')
            levels(x$by) <- paste0(nLevs$by, ' (', nLevs$nBy, ')')
        }
    }
    nBy <- ifelse(is.null(by), 0, length(unique(x$by)))
    if(is.character(color) &&
       length(color) < nBy) {
        if(length(color) > 1) {
            warning('Not enough colors supplied for levels of "by", defaulting to "scales::hue_pal"')
        }
        color <- hue_pal()
    }
    if(is.function(color)) {
        color <- color(nBy)
    }
    if(is.null(by)) {
        g <- g +
            geom_line(
                data=x,
                aes(x=.data$frequency, y=.data$qmed), color=color, lwd=1) +
            geom_ribbon(
                data=x,
                aes(x=.data$frequency, ymin=.data$qlow, ymax=.data$qhigh), fill=color, alpha=.1)
    } else {
        g <- g +
            geom_line(
                data=x,
                aes(x=.data$frequency, y=.data$qmed, color=.data$by), lwd=1) +
            geom_ribbon(
                data=x,
                aes(x=.data$frequency, ymin=.data$qlow, ymax=.data$qhigh, fill=.data$by), alpha=.1) +
            scale_color_manual(values=color, name=paste0(by, ' (nObs)')) +
            scale_fill_manual(values=color) +
            guides(fill='none')
    }
    g
}
