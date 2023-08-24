#' @title Plot Acoustic Scene
#'
#' @description Plots a representation of the acoustic scene using
#'   detections in data. Frequency ranges for detections are taken
#'   from user input and displayed as different colored bars
#'
#' @param x dataframe of detections, must have column \code{UTC} and
#'   a column to connect detection types to the frequency type map
#' @param freqMap a dataframe listing frequency ranges to use for
#'   various detection types in \code{x}. Must have columns \code{type},
#'   \code{freqMin} (Hz), \code{freqMax} (Hz), and optionally \code{color}
#'   (color to use for this type of detection on plot)
#' @param typeCol column name in \code{x} that matches names in \code{type}
#'   column in \code{freqMap}
#' @param title optional title to use for the plot
#' @param bin time bin to use for plotting time axis. Each detection will
#'   be displayed as covering this amount of time
#' @param scale one of \code{log} or \code{linear}, the frequency scale for
#'   the plot
#' @param freqMin optional minimum frequency for plot, useful for log scale
#' @param alpha transparency percentage for plotting, values less than 1
#'   will allow multiple overlapping colors to be seen
#'
#' @return a ggplot object
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr left_join join_by distinct
#'
#' @export
#'
plotAcousticScene <- function(x, freqMap, typeCol='species',
                              title=NULL, bin='1day', scale=c('log', 'linear'), freqMin=NULL,
                              alpha=1) {
    x <- checkSimple(x, needCols=c('UTC', typeCol))
    x$plotStart <- floor_date(x$UTC, unit=bin)
    thisPeriod <- unitToPeriod(bin)
    x$plotEnd <- x$plotStart + thisPeriod
    scale <- switch(match.arg(scale),
                    'log' = 'log10',
                    'identity'
    )
    missType <- unique(x[[typeCol]])[!unique(x[[typeCol]]) %in% freqMap$type]
    if(length(missType) > 0) {
        warning(length(missType), ' types in "x$', typeCol, '" were not in "freqMap$type" (',
                paste0(missType, collapse=', '), '), they will not be plotted.',
                call.=FALSE)
        x <- x[!x[[typeCol]] %in% missType, ]
    }
    # jank-ish work around to work with NSE for join_by, {{ }} didnt work
    x$TEMPJOINCOLUMN <- x[[typeCol]]
    x <- left_join(x, freqMap, by=join_by('TEMPJOINCOLUMN' =='type'))
    x$TEMPJOINCOLUMN <- NULL

    x <- x[!is.na(x[['freqMin']]), ]
    x <- distinct(x[c('plotStart', 'plotEnd', 'freqMin', 'freqMax', typeCol)])
    if(is.null(freqMin)) {
        freqMin <- min(x[['freqMin']])
    }
    if(freqMin < 1 && scale == 'log10') {
        freqMin <- 1
    }
    g <- ggplot(x) +
        geom_rect(aes(xmin=.data$plotStart,
                      xmax=.data$plotEnd,
                      ymin=.data$freqMin,
                      ymax=.data$freqMax,
                      fill=.data[[typeCol]]),
                  alpha=alpha)
        # scale_y_continuous(trans=scale)
    if(scale == 'log10') {
        g <- myLog10Scale(g, range=c(freqMin, max(x$freqMax)), dim='y')
    }
    g <- g +
        ggtitle(title) +
        labs(y='Frequency (Hz)',
             x='Date',
             fill='Sound Type')
    if('color' %in% colnames(freqMap)) {
        colNames <- freqMap[['color']]
        names(colNames) <- freqMap[['type']]
        g <- g +
            scale_fill_manual(values=colNames)
    }
    g
}
