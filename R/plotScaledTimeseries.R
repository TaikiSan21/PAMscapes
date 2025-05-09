#' @title Plot Rescaled Timeseries
#'
#' @description Plot timeseries of different values, rescaled so
#'   that multiple types of data are visible on the same plot
#'
#' @param x a dataframe with column \code{UTC}
#' @param columns the names of the columns to plot. Values of columns will
#'   be rescaled to appear similar to range of the first column
#' @param title title for the plot
#' @param units name of units for plot labeling, default is taken from
#'   common soundscape units
#' @param color colors to use for different lines, can either be a color
#'   palette function or a vector of color names
#' @param cpal Deprecated in favor of \code{color} for naming consistency
#' @param lwd line width, either a single value or a vector of widths
#'   matching the length of \code{columns}
#' @param minVals minimum value for each of \code{columns} to use for rescaling,
#'   either a single value to use for all or a vector matching the length of
#'   \code{columns}. A value of \code{NA} will use the minimum value present
#'   in the data. See Details for more info
#' @param relMax the percentage of the maximum value for all rescaled columns
#'   relative to the first column. See Details for more info
#' @param toTz timezone to use for the time axis (input data must be UTC).
#'   Specification must be from \link{OlsonNames}
#'
#' @details The data in the different \code{columns} of \code{x} may have
#'   very different ranges, so they must be rescaled in order to create a
#'   useful comparison plot. The default behavior is to rescale all columns
#'   to have the same min/max range as the first column in \code{columns}.
#'   This means that the Y-axis values will only be accurate for the first
#'   column, and all lines will have their minimum value at the bottom edge
#'   of the plot and their maximum value at the top edge of the plot.
#'
#'   There are some cases where this full-range rescaling is not desirable.
#'   One case is when one of the variables should have a minimum value of
#'   zero, but the lowest value present in your data is larger than zero.
#'   For example, wind speed might in your data might range from values of
#'   0.5 to 3, so by default this 0.5 value would appear at the bottom of the
#'   plot. However, it would make much more sense if the values were plotted
#'   relative to a minimum of zero. The \code{minVals} argument lets you control
#'   this. The default \code{NA} value uses the minimum of your data range, but
#'   you can provide a value of zero (or anything else) to control the displayed
#'   minimum.
#'
#'   It can also be distracting or busy to display all lines at the same relative
#'   height, especially as the number of columns displayed grows. There are two
#'   ways to help this. First, the \code{lwd} parameter can be used to display
#'   certain lines more prominently, making it easier to keep track of more
#'   important information. Second, the \code{relMax} can be used to control the
#'   maximum relative height of each line plot. The default value of 1 makes each
#'   line the same maximum height as the first column, reducing this to a value of
#'   0.75 would make it so that all lines other than the first will not go higher than
#'   75% of the Y-axis
#'
#' @return a ggplot object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' manta <- loadSoundscapeData(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
#' plotScaledTimeseries(manta, columns=c('HMD_50', 'HMD_100', 'HMD_200'))
#'
#' @importFrom scales hue_pal
#' @importFrom dplyr all_of select
#'
#' @export
#'
plotScaledTimeseries <- function(x, 
                                 columns, 
                                 title=NULL, 
                                 units=NULL,
                                 color=hue_pal(),
                                 cpal, 
                                 lwd=.5, 
                                 minVals=NA, 
                                 relMax=1,
                                 toTz='UTC') {
    x <- checkSimple(x, needCols='UTC')
    x$UTC <- with_tz(x$UTC, tzone=toTz)
    if(length(minVals) == 1) {
        minVals <- rep(minVals, length(columns))
    }
    if(length(lwd) == 1) {
        lwd <- rep(lwd, length(columns))
    }
    if(is.null(units)) {
        units <- 'Value'
    }
    # check if using old arg name and reassign
    if(!missing(cpal) && missing(color)) {
        warning('Argument "cpal" is deprecated, please use "color" instead')
        color <- cpal
    }
    cpal <- checkCpal(color, length(columns))
    names(cpal) <- columns
    if(length(columns) > 1) {
        for(i in 2:length(columns)) {
            if(i == 2) {
                min2 <- min(c(x[[columns[2]]], minVals[2]), na.rm=TRUE)
                min1 <- min(c(x[[columns[1]]], minVals[1]), na.rm=TRUE)
                diff2 <- diff(range(c(x[[columns[2]]], minVals[2]), na.rm=TRUE))
                diff2 <- ifelse(diff2 == 0, 1, diff2)
                diff1 <- diff(range(c(x[[columns[1]]], minVals[1]), na.rm=TRUE))

                col2Trans <- function(y) {
                    (y - min1) / diff1 * diff2 / relMax + min2
                }
            }
            x[[columns[i]]] <- doRescale(x[[columns[i]]], target=c(minVals[1], x[[columns[1]]]), min=minVals[i], relMax=relMax)
        }
    }
    # x <- select(x, all_of(c('UTC', columns))) %>%
    #     pivot_longer(cols=columns,
    #                         names_to='Type', values_to='value')
    x <- pivot_longer(
        select(x, all_of(c('UTC', columns))),
        cols=columns,
        names_to='Type',
        values_to='value'
    )
    x$Type <- factor(x$Type, levels=columns)
    g <- ggplot(data=x, aes(x=.data$UTC, y=.data$value, col=.data$Type, lwd=.data$Type)) +
        geom_line() +
        scale_color_manual(values=cpal, breaks=columns) +
        scale_linewidth_manual(values=lwd)
    g <- g + ggtitle(title) +
        labs(y=units, x=paste0('Date (', toTz, ')'))
    if(length(columns) > 1) {
        g <- g +
            scale_y_continuous(limits=range(c(minVals[1], x$value[x$Type == columns[1]]), na.rm=TRUE),
                               sec.axis = sec_axis(name=columns[2], transform=col2Trans))
    }
    g
}

doRescale <- function(x, target, min=NA, relMax=1) {
    x <- c(min, x)
    # want to rescale to 0-1
    xRange <- diff(range(x, na.rm=TRUE))
    xRange <- ifelse(xRange == 0, 1, xRange) # if all same value dont div 0
    x <- (x - min(x, na.rm=TRUE)) / xRange
    # then cast to range of "target"
    x <- x * diff(range(target, na.rm=TRUE)) * relMax + min(target, na.rm=TRUE)
    x[-1]
}
