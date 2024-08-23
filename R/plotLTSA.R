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
#' @param alpha alpha to use for the plot fill
#' @param returnData if \code{TRUE} then no plot will be generated, instead the
#'   dataframe that would normally be used to make the plot will be returned
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
                     cmap=viridis_pal()(25), toTz='UTC', alpha=1,
                     returnData=FALSE) {
    x <- checkSoundscapeInput(x, needCols='UTC')
    scale <- match.arg(scale)
    # we need a freqLow column later for the geom_, this block
    # is just making this faster by passing pivot_longer a
    # spec df for how to do the long-ing
    whichFreq <- whichFreqCols(x)
    x$UTC <- with_tz(x$UTC, tzone=toTz)
    x$UTC <- floor_date(x[['UTC']], unit=bin)
    setDT(x)
    x <- x[, lapply(.SD, median), .SDcols=colnames(x)[whichFreq], by=c('UTC')]
    setDF(x)
    # x <- binSoundscapeData(x, bin=bin, FUN=median)
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq]))
    freqVals <- gsub('[A-z]+_', '', colnames(x)[whichFreq])
    longSpec <- data.frame(.name=freqVals, .value='value')
    freqVals <- as.numeric(freqVals)
    longSpec$frequency <- freqVals
    longSpec$type <- type
    freqDiffs <- diff(freqVals)
    lowFreq <- switch(scale,
                      'log' = {
                          freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
                      },
                      'linear' = freqDiffs[1]
    )
    freqDiffs <- c(lowFreq, freqDiffs)
    freqLows <- freqVals - freqDiffs
    longSpec$freqLow <- freqLows
    # the spec is to make it faster char->numeric conversion and add freqLow column
    # only need to specify here bc freqLow column
    x <- toLong(x, spec=longSpec)
    # we could move this earlier but it doesnt actually take much time
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
    x$UTCend <- x$UTC + unitToPeriod(bin)
    if(is.function(cmap)) {
        cmap <- cmap(25)
    }
    if(is.null(dbRange)) {
        dbRange <- range(x$value)
    }
    if(scale == 'log') {
        x <- dplyr::filter(x, .data$freqLow > 0)
    }
    if(isTRUE(returnData)) {
        return(x)
    }
    ggplot(x) +
        geom_rect(aes(xmin=.data$UTC,
                      xmax=.data$UTCend,
                      ymin=.data$freqLow,
                      ymax=.data$frequency,
                      fill=.data$value),
                  alpha=alpha) +
        scale_fill_gradientn(colors=cmap,
                             limits=dbRange,
                             oob=squish) +
        scale_x_datetime(expand=c(0,0)) +
        scale_y_log10(expand=c(0,0)) +
        labs(fill=units) +
        ggtitle(title)
}
