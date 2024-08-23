#' @title Plot Hourly Sound Level
#'
#' @description Plots a heatmap of summarised sound levels. Y-axis is hour
#'   of the day, X-axis is frequency bin. Plotted values are the median of
#'   the \code{value} column for each hour/frequency pairing across the dataset.
#'   This function is designed to work with sound level outputs with consistent
#'   frequency bins measured across time
#'
#' @param x a dataframe with columns \code{UTC}, \code{frequency}, and
#'   \code{value}
#' @param title title for the plot. If \code{NULL} (default) it will use the
#'   first value in the \code{type} column of \code{x} (if present)
#' @param units name of units for plot labeling, default is taken from
#'   common soundscape units
#' @param scale one of \code{'log'} or \code{'linear'} for the scale of
#'   the frequency axis
#' @param freqMin minimum frequency for the plot range, if desired to be different
#'   than the minimum frequency of the data
#' @param toTz timezone to use for the time axis (input data must be UTC).
#'   Specification must be from \link{OlsonNames}
#' @param cmap color palette map to use for plot, default is \link[scales]{viridis_pal}
#' @param returnData if \code{TRUE} then no plot will be generated, instead the
#'   dataframe that would normally be used to make the plot will be returned
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' plotHourlyLevel(system.file('extdata/OLSmall.csv', package='PAMscapes'))
#'
#' @return a ggplot object
#'
#' @importFrom scales viridis_pal
#' @importFrom lubridate with_tz hour
#' @importFrom dplyr group_by summarise
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats median
#' @importFrom magrittr %>%
#'
#' @export
#'
plotHourlyLevel <- function(x, title=NULL, units=NULL,
                            scale=c('log', 'linear'), freqMin=NULL, toTz='UTC',
                            cmap=viridis_pal()(25),
                            returnData=FALSE) {
    scale <- match.arg(scale)
    x <- checkSoundscapeInput(x, needCols='UTC')
    x <- toLong(x)
    if('type' %in% colnames(x)) {
        if(x$type[1] == 'BB') {
            stop('Broadband data inappropriate for this plot.')
        }
        if(is.null(units)) {
            units <- typeToUnits(x$type[1])
        }
    }
    x$UTC <- with_tz(x$UTC, tzone=toTz)
    x$hour <- hour(x$UTC)
    summByHour <- group_by(x, .data$hour, .data$frequency) %>%
        summarise(value = median(.data$value, na.rm=TRUE), .groups='drop')
    summByHour$hour_end <- summByHour$hour + 1 # hour ranges 0-23
    freqVals <- sort(unique(summByHour$frequency))
    freqDiffs <- diff(freqVals)
    lowFreq <- switch(scale,
                      'log' = {
                          freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
                      },
                      'linear' = freqDiffs[1]
    )
    freqDiffs <- c(lowFreq, freqDiffs)
    names(freqDiffs) <- as.character(freqVals)
    summByHour$freq_low <- summByHour$frequency - freqDiffs[as.character(summByHour$frequency)]
    minAllowed <- ifelse(scale=='log', 1.0001, .0001)
    summByHour$freq_low <- ifelse(summByHour$freq_low < minAllowed, minAllowed, summByHour$freq_low)
    if(is.null(title)) {
        title <- x$type[1]
    }
    if(is.null(freqMin)) {
        freqMin <- min(summByHour$freq_low)
    }
    if(freqMin < 1 && scale == 'log') {
        freqMin <- 1
    }
    summByHour <- dplyr::filter(summByHour, .data$freq_low >= freqMin)
    scale <- switch(match.arg(scale),
                    'log' = 'log10',
                    'identity'
    )
    if(isTRUE(returnData)) {
        return(summByHour)
    }
    g <- ggplot(summByHour) +
        geom_rect(aes(ymin=.data$hour,
                      ymax=.data$hour_end,
                      xmin=.data$freq_low,
                      xmax=.data$frequency,
                      fill=.data$value)) +
        # scale_x_continuous(trans=scale, expand=c(0,0), limits=c(freqMin, max(freqVals))) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_gradientn(colors=cmap)
    if(scale == 'log10') {
        g <- myLog10Scale(g, range=c(freqMin, max(freqVals)), dim='x')
    } else {
        g <- g + scale_x_continuous(expand=c(0, 0), limits=c(freqMin, max(freqVals)))
    }
    g <- g +
        ggtitle(title) +
        labs(x='Frequency (Hz)',
             y=paste0('Hour (', toTz, ')'),
             fill = units) +
        theme(legend.title = element_text(angle=90)) +
        guides(fill=guide_colorbar(title.position='right', barheight=10, title.hjust=.5))
    g
}
