#' @title Plot Detections in a Polar Plot
#' 
#' @description Plots detection data in a polar plot where the
#'   circular (angular) axis is either the hour of day or month of the year
#'   
#' @param x dataframe of data loaded with \link{loadDetectionData}
#' @param bin character specification of the radial and circular (angular) 
#'   dimensions of the plot in the form "bin1/bin2", where "bin1" is one of 
#'   "detection", "hour", or "day", and "bin2" is one of "hour" or "month". "bin1"
#'   is the units of the radial axis, and "bin2" is the unit of the 
#'   circular axis. If "bin1" is "detection", then each row is treated as a distinct
#'   instantaneous detection, otherwise calls are binned using 
#'   \link{binDetectionData}.
#' @param quantity character indicating what type of quantity to plot. "count"
#'   plots total detections, "mean" plots average detections across \code{group}s,
#'   "effort" plots amount of total effort, "percentTotal" plots number of detections
#'   as a percent of total detections, "percentEffort" plots percent of total
#'   effort with detections.
#' @param group a vector of name(s) of columns in \code{x} indicating which rows
#'   are distinct from eachother, used for binning data and accounting for effort.
#'   Typically something like "species", "site", or both
#' @param facet optional name of column to facet output plot by
#' @param effort Not relevant for \code{quantity} "count or "percentTotal", or
#'   \code{bin} with "detection". If not \code{NULL}, a dataframe describing on 
#'   effort times to be formatted with \link{formatEffort}. If effort data is not 
#'   provided then times with zero detections will not be properly accounted for. 
#'   Alternatively, if columns "effortStart" and "effortEnd" are present in
#'   \code{x}, then these values will be used for start and end of effort
#' @param title optional title for plot
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return a ggplot object
#' 
#' @importFrom dplyr n
#'
#' @export
#' 
plotPolarDetections <- function(x, 
                               bin=c('days/month'), 
                               quantity=c('count', 'mean', 'effort', 'percentEffort', 'percentTotal'),
                               group='species',
                               facet=NULL, 
                               effort=NULL, 
                               title=NULL) {
    binSplit <- strsplit(bin, '/')[[1]]
    if(length(binSplit) != 2) {
        stop('"bin" must be of of format "bin1/bin2"')
    }
    binSplit <- gsub('s$', '', binSplit)
    smallBin <- binSplit[1]
    bigBin <- binSplit[2]
    quantity <- match.arg(quantity)
    if(!bigBin %in% c('hour', 'month')) {
        stop('Denominator bin must be "hour" or "month"')
    }
    switch(bigBin,
           'hour' = {
               if(!smallBin %in% c('detection', 'hour')) {
                   stop('Numerator bin must be "detection" or "hour"')
               }
           },
           'month' = {
               if(!smallBin %in% c('detection', 'hour', 'day')) {
                   stop('Numerator bin must be "detection", "hour", "day", or "month"')
               }
           }
    )
    group <- unique(c(group, facet))
    if(is.null(effort) &&
       all(c('effortStart', 'effortEnd') %in% names(x))) {
        effort <- distinct(select(x, all_of(c('effortStart', 'effortEnd', group))))
    }
    for(col in group) {
        if(!col %in% names(effort)) {
            next
        }
        effort <- effort[effort[[col]] %in% unique(x[[col]]), ]
    }
    
    if(smallBin == 'detection') {
        # each row is a unique call, but spread if they span multiple
        # big bin groups. May not be necessary
        # wait no. dont spread a call across bins 1 != 2
        # x$CALLIX <- 1:nrow(x)
        # x <- binDetectionData(x, bin=bigBin, columns=c(group, 'CALLIX'))
        # x$CALLIX <- NULL
        # smallBin <- 'hour'
        effort <- NULL
    } else {
        x <- binDetectionData(x, bin=smallBin, columns=c(group))
    }
    x <- fillEffortZeroes(x, effort=effort, resolution=smallBin, columns=group)
    switch(bigBin, 
           'hour' = {
               x$COUNTBIN <- floor_date(x$UTC, unit='hour')
               x$PLOTBIN <- hour(x$UTC)
               lims <- c(-.5, 23.5)
               breaks <- seq(from=0, to=24, by=3)
               pStart <- -(.5/24)*2*pi
               xlab <- 'Hour'
           },
           'month' = {
               x$COUNTBIN <- floor_date(x$UTC, unit='month')
               x$PLOTBIN <- month(x$UTC)
               lims <- c(.5, 12.5)
               breaks <- seq(from=1, to=12, by=1)
               pStart <- -(.5/12)*2*pi
               xlab <- 'Month'
           }
    )
    plotData <- summarise(
        group_by(x, across(all_of(c('PLOTBIN', 'COUNTBIN', facet)))),
        nDetections = sum(.data$effortDetection),
        nTotal = n(),
        .groups='drop_last'
        )
    plotData <- summarise(
        group_by(plotData, across(all_of(c('PLOTBIN', facet)))),
        meanDetections = mean(.data$nDetections),
        nDetections = sum(.data$nDetections),
        nEffort = sum(.data$nTotal),
        nGroups = n(),
        pctEffort = .data$nDetections/.data$nEffort)
    
    if(is.null(facet)) {
        plotData$pctDetections <- plotData$nDetections / sum(plotData$nDetections)
    } else {
        plotData <- mutate(
            group_by(plotData, across(all_of(facet))),
            pctDetections = .data$nDetections / sum(.data$nDetections)
        )
    }
    switch(quantity,
           'count' = {
               plotCol <- 'nDetections'
               minY <- 0
               yLab <- paste0(oneUp(smallBin), 's')
               fillLab <- paste0(oneUp(smallBin), 's')
           },
           'mean' = {
               plotCol <- 'meanDetections'
               minY <- 0
               yLab <- paste0('Average ', oneUp(smallBin), 's')
               fillLab <- paste0(oneUp(smallBin), 's')
           },
           'percentEffort' = {
               plotCol <- 'pctEffort'
               minY <- 0
               yLab <- paste0('Perecent of Available ', oneUp(smallBin), 's')
               fillLab <- 'Perecent'
           },
           'percentTotal' = {
               plotCol <- 'pctDetections'
               minY <- 0
               yLab <- paste0('Percent of Total ', oneUp(smallBin), 's')
               fillLab <- 'Percent'
           },
           'effort' = {
               plotCol <- 'nEffort'
               minY <- 0
               yLab <- paste0(oneUp(smallBin), 's', ' of Effort')
               fillLab <- paste0(oneUp(smallBin), 's')
           }
    )

    g <- ggplot(plotData) +
        geom_bar(aes(x=.data$PLOTBIN, y=.data[[plotCol]], fill=.data[[plotCol]]), stat='identity') +
        coord_polar(start=pStart) +
        scale_fill_viridis_c(name=fillLab) +
        scale_x_continuous(breaks=breaks, limits=lims, expand=c(0, 0)) + 
        scale_y_continuous(limits=c(minY, NA), name=yLab) +
        ggtitle(title) +
        labs(x=xlab)
    if(!is.null(facet)) {
        g <- g +
            facet_wrap(~.data[[facet]])
    }
    g
}
