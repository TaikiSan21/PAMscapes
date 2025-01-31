#' @title Plot Detection Boxplot
#' 
#' @description Plots time series of boxplots showing detection data across time
#' 
#' @details The combination of \code{group}, \code{facet}, and 
#'   \code{combineYears} determine the data points that make up each boxplot.
#'   If \code{combineYears=TRUE}, then there will be a different point for 
#'   each year. There will additionally be separate points for each different
#'   value of the columns in \code{group}, excluding the column used for 
#'   \code{facet} (since these points are instead split out to different
#'   facetted plots). 
#'   
#'   For example, if you have data from a single location, then settings of
#'   \code{combineYears=FALSE}, \code{group='species'}, and \code{facet=NULL}
#'   will create a plot where each point in a boxplot represents the number
#'   of detections for a species. If you change to \code{facet='species'},
#'   then the result will show a multi panel plot where each boxplot is just
#'   a single point. Then changing to \code{combineYears=TRUE} will show
#'   a multi panel plot where each point in a boxplot is the number of
#'   detections for that panel's species in different years.
#'
#' @param x dataframe of detection data read in with \link{loadDetectionData}
#' @param group name(s) of columns indicating which rows of \code{x} are 
#'   distinct from each other, typically something like "site" or "species"
#'   or both. These are used to define the different data points that go 
#'   in to each boxplot, see Details below for more information.
#' @param facet if not \code{NULL}, name of the column in \code{x} to facet
#'   the plot by
#' @param color only used if \code{facet} is not \code{NULL}, colors to use
#'   for each separate facet. Can either be a color palette function or a
#'   character vector of color names. If a vector, it can be named by the
#'   levels in \code{facet} that each color should correspond to
#' @param bin time bins to use for generating plot, must be a character of 
#'   format "time1/time2" where "time1" will be the y-axis of the plot and
#'   "time2" will be the x-axis of the plot. Times are one of "hour", "day", 
#'   "week", or "month" (e.g. \code{"day/week"}). 
#' @param combineYears logical flag to combine all observations to display
#'   as a single "year"
#' @param effort if not \code{NULL}, a dataframe describing on effort times
#'   to be formatted with \link{formatEffort}. If effort data is not provided
#'   then times with zero detections will not be properly accounted for.
#' @param dropZeroes logical flag to remove boxplots where all observations
#'   are zero (these would normally appear as a flat line at zero)
#' @param returnData if \code{TRUE} then no plot will be generated, instead the
#'   dataframe that would normally be used to make the plot will be returned
#'   
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return a ggplot object
#' 
#' @export
#' 
#' @importFrom dplyr group_by summarise across all_of mutate filter ungroup
#' @importFrom lubridate month isoweek yday
#' 
plotDetectionBoxplot <- function(x, 
                                 group='species', 
                                 facet=NULL,
                                 color=hue_pal(),
                                 bin='day/week', 
                                 combineYears=FALSE, 
                                 effort=NULL,
                                 dropZeroes=FALSE, 
                                 returnData=FALSE) {
    binChoice <- c('hour', 'day', 'week', 'month')
    binSplit <- strsplit(bin, '/')[[1]]
    if(length(binSplit) != 2) {
        stop('"bin" must be of of format "bin1/bin2"')
    }
    binSplit <- gsub('s$', '', binSplit)
    
    bigBin <- match.arg(binSplit[2], binChoice)
    smallBin <- match.arg(binSplit[1], binChoice)
    
    if(!is.null(facet) &&
       !facet %in% group) {
        # warning('"facet" must be included in "group"')
        group <- c(group, facet)
    }
    missCol <- group[!group %in% names(x)]
    if(any(missCol)) {
        stop('Column(s) ', paste0(missCol, collapse=', '), ' are not in "x"')
    }
    for(col in group) {
        if(!col %in% names(effort)) {
            next
        }
        effort <- effort[effort[[col]] %in% unique(x[[col]]), ]
    }
    # e.g. bin to days (of day/week)
    x <- binDetectionData(x, bin=smallBin, columns=group, rematchGPS=FALSE)
    x <- fillEffortZeroes(x, effort=effort, resolution=smallBin, columns=group)
    x$SMALLBIN <- x$UTC
    # e.g. binto weeks (of day/week)
    x <- binDetectionData(x, bin=bigBin, columns=c(group, 'SMALLBIN', 'effortDetection'))
    
    timeRange <- c(min(x$UTC, na.rm=TRUE), max(x$end, na.rm=TRUE))
    
    for(g in facet) {
        if(is.character(x[[g]]) || is.factor(x[[g]])) {
            next
        }
        x[[g]] <- as.character(x[[g]])
    }
    x <- summarise(
        group_by(x, across(all_of(c('UTC', group)))),
        nPres = sum(.data$effortDetection),
        .groups='drop'
    )
    
    sumAcross <- character(0)
    sumAcross <- c(sumAcross, group[!group %in% facet])
    
    if(isTRUE(combineYears)) {
        sumAcross <- c(sumAcross, 'year')
        switch(bigBin,
               'week' = {
                   FUN <- isoweek
                   levs <- 1:53
                   xLabs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
                   xAt <- 1 + c(0, 4, 8, 12, 17, 21, 25, 30, 34, 39, 43, 48)
                   switch(smallBin,
                          'day' = {
                              yLim <- c(0, 7)
                              yAt <- c(1, 3, 5, 7)
                              yMinor <- 0:7
                          },
                          'hour' = {
                              yLim <- c(0, 7) * 24
                              yAt <- c(1, 3, 5, 7) * 24
                              yMinor <- yAt
                          }
                   )
               },
               'month' = {
                   FUN <- month
                   levs <- 1:12
                   xLabs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
                   xAt <- 1:12
                   switch(smallBin,
                          'day' = {
                              yLim <- c(0, 31)
                              yAt <- c(7, 14, 21, 28)
                              yMinor <- c(0, yAt, 35)
                          },
                          'hour' = {
                              yLim <- c(0, 31) * 24
                              yAt <- c(7, 14, 21, 28) * 24
                              yMinor <- c(0, yAt, 35*24)
                          },
                          'week' = {
                              yLim <- c(0, 5)
                              yAt <- c(1, 2, 3, 4, 5)
                              yMinor <- c(0, yAt, 5)
                          }
                   )
               },
               'day' = {
                   FUN <- yday
                   levs <- 1:366
                   xLabs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
                   xAt <- 1 + c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
                   switch(smallBin,
                          'hour' = {
                              yLim <- c(0, 24)
                              yAt <- c(0, 6, 12, 18, 24)
                              yMinor <- yAt
                          }
                   )
               }
        )
        x$UTC <- FUN(x$UTC)
        x$UTC <- factor(x$UTC, levels=levs)
    } else {
        switch(bigBin,
               'week' = {
                   switch(smallBin,
                          'day' = {
                              yLim <- c(0, 7)
                              yAt <- c(1, 3, 5, 7)
                              yMinor <- yAt
                          },
                          'hour' = {
                              yLim <- c(0, 7) * 24
                              yAt <- c(1, 3, 5, 7) * 24
                              yMinor <- yAt
                          }
                   )
               },
               'month' = {
                   switch(smallBin,
                          'day' = {
                              yLim <- c(0, 31)
                              yAt <- c(7, 14, 21, 28)
                              yMinor <- c(0, yAt, 35)
                          },
                          'hour' = {
                              yLim <- c(0, 31) * 24
                              yAt <- c(7, 14, 21, 28) * 24
                              yMinor <- c(0, yAt, 35*24)
                          },
                          'week' = {
                              yLim <- c(0, 5)
                              yAt <- c(1, 2, 3, 4, 5)
                              yMinor <- c(0, yAt, 5)
                          }
                   )
               },
               'day' = {
                   switch(smallBin,
                          'hour' = {
                              yLim <- c(0, 24)
                              yAt <- c(0, 6, 12, 18, 24)
                              yMinor <- yAt
                          }
                   )
               }
        )
    }
    if(isTRUE(dropZeroes)) {
        x <- filter(
            mutate(
                group_by(x, across(all_of(c('UTC', facet)))),
                nMax = max(.data$nPres)
            ),
            .data$nMax > 0
        )
        x <- ungroup(x)
    }
    if(returnData) {
        return(x)
    }
    
    if(!is.null(facet)) {
        nFacet <- length(unique(x[[facet]]))
        color <- checkCpal(color, nFacet)
        if(is.factor(x[[facet]]) && is.null(names(color))) {
            names(color) <- levels(x[[facet]])
        }
        g <- ggplot() +
            geom_boxplot(data=x, aes(x=.data$UTC, y=.data$nPres, fill=.data[[facet[1]]], group=.data$UTC)) +
            facet_wrap(~.data[[facet]], ncol=1) +
            scale_fill_manual(values=color)
    } else {
        g <- ggplot() +
            geom_boxplot(data=x, aes(x=.data$UTC, y=.data$nPres, group=.data$UTC), fill='gray')
    }
    g <- g +
        scale_y_continuous(breaks=yAt, minor_breaks=yMinor, name=paste0(oneUp(smallBin), 's')) +
        coord_cartesian(ylim=yLim)
    if(isTRUE(combineYears)) {
        g <- g +
            scale_x_discrete(breaks=xAt, labels=xLabs, name=oneUp(bigBin))
    } else {
        g <- g +
            scale_x_datetime(date_labels = '%b-%Y', name='Date')
    }
    if(!is.null(effort)) {
        for(col in c(facet)) {
            if(!col %in% names(effort)) {
                next
            }
            effort <- effort[effort[[col]] %in% unique(x[[col]]), ]
        }
        effort <- formatEffort(effort, range=timeRange, 
                               resolution=bigBin, combineYears = combineYears, columns=c(facet))
        if(isTRUE(combineYears)) {
            effort$start <- factor(FUN(effort$start), levels=levs)
            effort$end <- factor(FUN(effort$end), levels=levs)
        }
        colVals <- lapply(c(facet), function(c) unique(x[[c]]))
        names(colVals) <- c(facet)
        effort <- spreadEffort(effort, colVals=colVals)
        if(!is.null(facet) && is.factor(x[[facet]])) {
            effort[[facet]] <- factor(effort[[facet]], levels=levels(x[[facet]]))
        }
        # rename to same names as original plot for easy adding
        effort$UTC <- effort$start
        effort <- effort[effort$status == 'off', ]
        
        g <- g +
            geom_rect(data=effort, 
                      aes(xmin=.data$UTC,
                          xmax=.data$end,
                          ymin=-10,
                          ymax=100),
                      fill='gray',
                      alpha=0.5)
    }
    if(length(sumAcross) == 0) {
        sumAcross <- '(nothing)'
    }
    groupTitle <- paste0(sumAcross, collapse=', ')
    groupTitle <- paste0('grouped across: ', groupTitle)
    binTitle <- paste0(oneUp(smallBin), 's per ', oneUp(bigBin))
    title <- paste0(binTitle, ', ', groupTitle)
    g <- g + ggtitle(title)
    g
}

oneUp <- function(s) {
    paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}
