#' @title Plot Timeseries
#'
#' @description Plot simple timeseries of values
#'
#' @param x a dataframe with column \code{UTC}
#' @param bin time bin for summarising data. The median of values
#'   within the same time bin will be plotted
#' @param column the name of the column to plot
#' @param title title for the plot, if left as default \code{NULL} it
#'   will use the \code{column} name
#' @param units name of units for plot labeling, default is taken from
#'   common soundscape units
#' @param style one of \code{'line'} or \code{'heatmap'}. \code{'line'}
#'   will create a simple line time series plot, \code{'heatmap'} will create
#'   a grid plot with hour of day as X-axis and Date as y-axis where the
#'   value of \code{column} is the color
#' @param q only valid for \code{style='line'}, quantile level for plotting,
#'   between 0 and 1.
#'   If left as \code{0}, none will be plotted. If a single value, then
#'   levels \code{q} and \code{1-q} will be plotted. Users can also
#'   specify both values for non-symmettric intervals.
#' @param by only valid for \code{style='line'}, optional categorical
#'   column to plot separate lines for
#' @param cmap only valid for \code{style='heatmap'}, the color palette to
#'   use for plotting values
#' @param toTz timezone to use for the time axis (input data must be UTC).
#'   Specification must be from \link{OlsonNames}
#'
#' @return a ggplot object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' manta <- checkSoundscapeInput(system.file('extdata/MANTAExampleSmall2.csv', package='PAMscapes'))
#' plotTimeseries(manta, bin='1minute', column='HMD_150')
#'
#' @importFrom lubridate floor_date hour minute
#' @importFrom stats quantile
#' @importFrom scales viridis_pal
#'
#' @export
#'
plotTimeseries <- function(x, bin='1hour', column, title=NULL, units=NULL,
                           style=c('line', 'heatmap'), q=0, by=NULL,
                           cmap=viridis_pal()(25), toTz='UTC') {
    x <- checkSimple(x, needCols='UTC')
    x$UTC <- with_tz(x$UTC, tzone=toTz)
    x$timeBin <- floor_date(x$UTC, unit=bin)
    if(is.null(units)) {
        units <- colToUnits(column)
    }
    switch(match.arg(style),
           'line' = {
               if(length(q) == 1) {
                   q <- c(q, ifelse(q==0, 0, 1-q))
               }
               q <- sort(q)
               if(is.null(by)) {
                   plotData <- group_by(x, .data$timeBin)
               } else {
                   x[[by]] <- factor(x[[by]])
                   plotData <- group_by(x, .data$timeBin, .data[[by]])
               }
               if(!all(q == 0)) {
                   plotData <- plotData %>%
                       summarise(med = median(.data[[column]], na.rm=TRUE),
                                 qlow = quantile(.data[[column]], probs=q[1], na.rm=TRUE),
                                 qhigh = quantile(.data[[column]], probs=q[2], na.rm=TRUE),
                                 .groups='drop')
               } else {
                   plotData <- plotData %>%
                       summarise(med = median(.data[[column]], na.rm=TRUE),
                                 .groups='drop')
               }
               if(is.null(by)) {
                   g <- ggplot(data=plotData, aes(x=.data$timeBin))
               } else {
                   g <- ggplot(data=plotData, aes(x=.data$timeBin, col=.data[[by]], fill=.data[[by]]))
               }
               g <- g + geom_line(aes(y=.data$med))
               if(!all(q == 0)) {
                   g <- g +
                       geom_ribbon(aes(ymin=.data$qlow, ymax=.data$qhigh), alpha=.1)
               }

               g <- g + labs(x=paste0('Date (', toTz, ')'), y=units)
           },
           'heatmap' = {
               x$day <- floor_date(x$UTC, unit='1day')
               plotData <- group_by(x, .data$day, .data$timeBin) %>%
                   summarise(med = median(.data[[column]], na.rm=TRUE), .groups='drop')
               binHours <- as.numeric(unitToPeriod(bin)) / 3600
               plotData$hour <- hour(plotData$timeBin) + minute(plotData$timeBin) / 60
               g <- ggplot(plotData) +
                   geom_rect(aes(xmin=.data$hour,
                                 xmax = .data$hour + binHours,
                                 ymin = .data$day,
                                 ymax = .data$day + 3600*24,
                                 fill = .data$med)) +
                   scale_fill_gradientn(colors=cmap) +
                   scale_x_continuous(limits=c(0,24), expand=c(0,0)) +
                   scale_y_datetime()
               g <- g +
                   labs(x=paste0('Hour (', toTz, ')'),
                        y='Date',
                        fill = units) +
                   theme(legend.title = element_text(angle=90)) +
                   guides(fill=guide_colorbar(title.position='right', barheight=10, title.hjust=.5))
           }
    )
    if(is.null(title)) {
        title <- column
    }
    g <- g + ggtitle(title)
    g
}

# try to convert column name to metric type and then unit
colToUnits <- function(x) {
    x <- strsplit(x, '_')[[1]]
    if(length(x) == 1) {
        return(NULL)
    }
    # assume if type of XXX__### then XXX is metric type
    if(length(x) == 2 &&
       !is.na(suppressWarnings(as.numeric(x[2])))) {
        return(typeToUnits(x[1]))
    }
    NULL
}
