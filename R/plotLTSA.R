#' @title Plot Long-Term Spectral Average (LTSA)
#'
#' @description Creates a long-term spectral average (LTSA) style plot
#'   of the data, a plot where the x-axis is time and the y-axis is frequency.
#'   Color represents the magnitude of sound. In order to compress the
#'   time axis, data are binned into time chunks and the median value
#'   within that time bin is displayed
#'
#' @param x a soundscape metric file that can be read in with
#'   \link{checkSoundscapeInput}, or a dataframe with \code{UTC},
#'   \code{frequency}, and \code{value}
#' @param bin amount of time to bin for each LTSA slice, format can
#'   be "#Unit" e.g. \code{'2hour'} or \code{'1day'}
#' @param scale scaling for frequency axis, one of \code{log} or \code{linear}
#' @param title optional title for plot
#' @param freqRange if not \code{NULL}, a vector of two numbers specifying the
#'   range of frequencies (Hz) to plot. Providing \code{NA} for either value will
#'   use the max/min frequency present in the dataset
#' @param dbRange if not \code{NULL}, a fixed limit to use for the color
#'   scaling of dB values in the plot
#' @param units units for plot labeling, will attempt to read them from the input
#' @param cmap color palette map to use for plot, default is \link[scales]{viridis_pal}
#' @param toTz timezone to use for the time axis (input data must be UTC).
#'   Specification must be from \link{OlsonNames}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return ggplot object of the LTSA plot
#'
#' @examples
#' hmd <- checkSoundscapeInput(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
#' # time range is too small for nice plots
#' plotLTSA(hmd, bin='1min', title='Every Minute')
#' plotLTSA(hmd, bin='2min', title='2 Minute Bins')
#'
#'
#' @importFrom scales squish
#' @importFrom data.table setDT setkeyv setDF .SD
#'
#' @export
#'
plotLTSA <- function(x, bin='1hour', scale=c('log', 'linear'),
                     title=NULL, freqRange=NULL, dbRange=NULL, units=NULL,
                     cmap=viridis_pal()(25), toTz='UTC') {
    x <- checkSoundscapeInput(x, needCols='UTC')
    x <- toLong(x)
    if(!is.null(freqRange)) {
        if(length(freqRange) != 2) {
            freqRange <- range(x$frequency)
            warning('"freqRange" must be two numbers specifying upper and lower bounds')
        }
        if(is.na(freqRange[1])) {
            freqRange[1] <- min(x$frequency)
        }
        if(is.na(freqRange[2])) {
            freqRange[2] <- max(x$frequency)
        }
        freqRange <- sort(freqRange)
        x <- dplyr::filter(x,
                           .data$frequency <= freqRange[2],
                           .data$frequency >= freqRange[1])
    }
    if(is.null(units)) {
        units <- typeToUnits(x$type[1])
    }
    scale <- match.arg(scale)
    x$UTC <- with_tz(x$UTC, tzone=toTz)
    x$plotTime <- floor_date(x[['UTC']], unit=bin)
    setDT(x)
    # setkeyv(x, c('frequency', 'plotTime'))
    x <- x[, .('value'=median(.SD$value)), by=c('frequency', 'plotTime')]
    setDF(x)
    # x <- group_by(x, frequency, plotTime) %>%
    #     summarise(value = median(value)) %>%
    #     ungroup()
    freqVals <- sort(unique(x$frequency))
    freqDiffs <- diff(freqVals)
    lowFreq <- switch(scale,
                      'log' = {
                          freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
                      },
                      'linear' = freqDiffs[1]
    )
    freqDiffs <- c(lowFreq, freqDiffs)
    names(freqDiffs) <- as.character(freqVals)
    # making geom_rect endpoints for x and y
    x$freq_low <- x$frequency - freqDiffs[as.character(x$frequency)]
    x$UTCend <- x$plotTime + unitToPeriod(bin)
    if(is.function(cmap)) {
        cmap <- cmap(25)
    }
    if(is.null(dbRange)) {
        dbRange <- range(x$value)
    }
    if(scale == 'log') {
        x <- dplyr::filter(x, .data$freq_low > 0)
    }
    ggplot(x) +
        # geom_raster(aes(x=.data$plotTime,
        #               # xmax=.data$UTCend,
        #               # ymin=.data$freq_low,
        #               y=.data$frequency,
        #               fill=.data$value)) +
        geom_rect(aes(xmin=.data$plotTime,
                      xmax=.data$UTCend,
                      ymin=.data$freq_low,
                      ymax=.data$frequency,
                      fill=.data$value)) +
        scale_fill_gradientn(colors=cmap,
                             limits=dbRange,
                             oob=squish) +
        scale_x_datetime(expand=c(0,0)) +
        scale_y_log10(expand=c(0,0)) +
        labs(fill=units) +
        ggtitle(title)
}

globalVariables('.')
