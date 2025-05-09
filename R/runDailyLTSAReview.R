#' @title Run Daily Soundscape LTSA Data Review Shiny App
#' 
#' @details Launches a Shiny app that allows users to manually review LTSA
#'   style plots of soundscape NetCDF data. Users can draw annotations
#'   on the LTSA plot and export these annotations as a CSV file. Users can
#'   additionally mark selected areas with the 1-4 standard quality flags.
#'   
#' @param file either a folder containing NetCDF soundscape files or a
#'   single file
#' @param plotQuality logical flag whether or not to plot the matrix of
#'   data quality flags. This greatly increases the load time for each file.
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return nothing, just runs the app
#' 
#' @importFrom shiny isolate invalidateLater icon
#' @export
#' 
runDailyLTSAReview <- function(file, plotQuality=FALSE) {
    if(length(file) == 1 &&
       dir.exists(file)) {
        file <- list.files(file, pattern='nc$', full.names=TRUE)
    }
    if(length(file) == 0 ||
       !any(file.exists(file))) {
        stop('No files found, cannot run app')
    }
    names(file) <- basename(file)
    ui <- navbarPage(
        id='Main',
        'Daily LTSA Reviewer',
        # UI PLOT ####
        tabPanel(
            'Plot',
            fluidRow(
                column(1,
                       actionButton('left', label='', icon=icon('chevron-left', lib='glyphicon')),
                       actionButton('right', label='', icon=icon('chevron-right', lib='glyphicon')),
                ),
                column(5,
                       selectInput('fileSelect', label='File', choices=as.list(file), multiple=FALSE)
                ),
            ),
            tags$head(tags$style(HTML('#left {position: absolute; top: 25px; left: 0%}'))),
            tags$head(tags$style(HTML('#right {position: absolute; top: 25px; left: 50px}'))),
            uiOutput('plotHead'),
            fluidRow(
                plotOutput(outputId = 'ltsaPlot', brush=brushOpts(id='ltsaBrush'))
            ),
            fluidRow(
                column(2, actionButton('addButton', label='Add Annotation')),
                column(2, selectizeInput('dqValue', label='Quality Flag',
                                         choices=list('(1) Good'=1,
                                                      '(2) Not Evaluated'=2,
                                                      '(3) Compromised'=3,
                                                      '(4) Bad'=4), selected=2)),
                column(3, textAreaInput('comment', label='Comment')),
                column(5, verbatimTextOutput('currentRange')),
            ),
            fluidRow(
                plotOutput(outputId = 'dqPlot', height='360px')
            )
        ),
        # UI ANNOTATIONS ####
        tabPanel(
            'Annotations',
            tags$h4(paste0('Click to select row, double click to select and edit value in a cell'),
                    style='text-align:center;'),
            DTOutput('annoTable'),
            actionButton('removeAnno', label='Remove Annotation'),
            downloadButton('downloadAnno', label='Download Annotations')
        )
    )
    server <- function(input, output, session) {
        # Data Loading ####
        nc <- file[1]
        if(isTRUE(plotQuality)) {
            # tic('Initial load and format of DQ data')
            dqInit <- loadQuality(nc)
            dqPlotBase <- prepQualityPlot(dqInit)
            # toc()
        } else {
            dqInit <- list(dq=NA, time=NA, freq=NA)
            dqPlotBase <- NA
        }
        # tic('Initial load of soundscape')
        ncData <- loadSoundscapeData(nc, keepQuals = 1:4)
        # toc()
        # tic('LTSA data format')
        ltsaData <- fastLTSAData(ncData)
        # toc()
        vals <- reactiveValues(
            file=file[1],
            data=ncData,
            timeRange = NA,
            freqRange = NA,
            dq = dqInit$dq,
            dqPlotBase = dqPlotBase,
            dqTime=dqInit$time,
            dqFreq=dqInit$freq,
            annots=NULL,
            ltsaData=ltsaData
        )
        skipPlot <- reactiveVal(1)
        skipDq <- reactiveVal(1)
        # Header ####
        output$plotHead <- renderUI({
            tags$h4(paste0('LTSA of file: "', basename(vals$file), '"'),
                    style='font-weight:bold; text-align:center;')
        })
        plotColors <- c('1'='darkgreen', '2'='steelblue', '3'='yellow', '4'='red')
        # Change File ####
        observeEvent(input$fileSelect, {
            skipPlot(1) # allows for prepping the header
            vals$file <- input$fileSelect
            vals$data <- loadSoundscapeData(vals$file, keepQuals=1:4)
            vals$timeRange <- NA
            vals$freqRange <- NA
            vals$ltsaData <- fastLTSAData(vals$data)
            if(isTRUE(plotQuality)) {
                skipDq(1)
                newDq <- loadQuality(vals$file)
                vals$dq <- newDq$dq
                vals$dqTime <- newDq$time
                vals$dqFreq <- newDq$freq
                vals$dqBasePlot <- prepQualityPlot(newDq)
            }
        })
        # File bttons ####
        observeEvent(input$left, {
            curIx <- which(file == input$fileSelect)
            if(curIx > 1) {
                nextFile <- file[curIx - 1]
                updateSelectInput(inputId='fileSelect', selected=nextFile)
            } else{
                showNotification('Already at first file!')
            }
        })
        observeEvent(input$right, {
            curIx <- which(file == input$fileSelect)
            if(curIx < length(file)) {
                nextFile <- file[curIx + 1]
                updateSelectInput(inputId='fileSelect', selected=nextFile)
            } else {
                showNotification('Already at last file!')
            }
        })
        # Plot LTSA ####
        output$ltsaPlot <- renderPlot({
            if(isolate(skipPlot() == 1)) {
                skipPlot(0)
                invalidateLater(1, session)
                return()
            }
            # tic('Plot LTSA')
            g <- ggplot(vals$ltsaData) +
                geom_rect(aes(xmin=.data$UTC,
                              xmax=.data$UTCend,
                              ymin=.data$freqLow,
                              ymax=.data$frequency,
                              fill=.data$value)) +
                scale_fill_gradientn(colors=viridis_pal()(25),
                                     limits=range(vals$ltsaData$value),
                                     oob=squish) +
                scale_x_datetime(expand=c(0,0)) +
                scale_y_log10(expand=c(0,0), guide=guide_axis(position='left')) +
                theme(legend.title = element_text(angle=90)) +
                guides(fill=guide_colorbar(title.position='right', barheight=unit(1, 'null'), title.hjust=.5))
            # toc()
            g
        })
        # Plot DQ ####
        output$dqPlot <- renderPlot({
            if(isFALSE(plotQuality)) {
                return()
            }
            if(isolate(skipDq() == 1)) {
                skipDq(0)
                invalidateLater(1, session)
                return()
            }
            # tic('Plot DQ')
            dqPlot <- ggplot() +
                geom_rect(data=vals$dqPlotBase,
                          aes(xmin=.data$UTC,
                              xmax=.data$UTCend,
                              ymin=.data$freqLow,
                              ymax=.data$frequency,
                              fill=.data$value),
                          show.legend=TRUE) +
                scale_x_datetime(expand=c(0,0)) +
                scale_y_log10(expand=c(0,0)) +
                theme(legend.title = element_text(angle=90)) +
                guides(fill=guide_legend(title.position='right', title.hjust=.5)) +
                scale_fill_manual(values=plotColors, drop=FALSE)
            for(i in seq_along(vals$annots)) {
                dqPlot <- dqPlot +
                    annotate('rect',
                             xmin=vals$annots[[i]]$start,
                             xmax=vals$annots[[i]]$end,
                             ymin=vals$annots[[i]]$freqMin,
                             ymax=vals$annots[[i]]$freqMax,
                             fill=plotColors[vals$annots[[i]]$quality])
            }
            # toc()
            dqPlot
        })
        # LTSA Brush ####
        observeEvent(input$ltsaBrush, {
            # tic('Brush input')
            # plotData <- fastLTSAData(vals$data)
            brush <- input$ltsaBrush
            vals$timeRange <- as.POSIXct(round(c(brush$xmin, brush$xmax), 0), origin='1970-01-01 00:00:00', tz='UTC')
            vals$freqRange <- round(c(brush$ymin, brush$ymax), 1)
            # toc()
        })
        # Add Anno ####
        observeEvent(input$addButton, {
            # tic('Add button')
            if(isTRUE(plotQuality)) {
                vals$dq <- markDQMatrix(vals$dq,
                                        freqRange=vals$freqRange,
                                        timeRange=vals$timeRange,
                                        value=input$dqValue,
                                        times=vals$dqTime,
                                        freqs=vals$dqFreq)
            }
            newAnnot <- list(
                freqMin=vals$freqRange[1],
                freqMax=vals$freqRange[2],
                start=vals$timeRange[1],
                end=vals$timeRange[2],
                quality=input$dqValue,
                comment=input$comment,
                file=vals$file
            )
            if(is.null(vals$annots)) {
                vals$annots <- list(newAnnot)
            } else {
                vals$annots[[length(vals$annots)+1]] <- newAnnot
            }
            # toc()
        })
        # Anno Text ####
        output$currentRange <- renderText({
            paste0(
                'Selected frequency: ',
                vals$freqRange[1], ' to ',
                vals$freqRange[2],
                '\nSelected time: ',
                vals$timeRange[1], ' to ',
                vals$timeRange[2]
            )
        })
        # Anno Table ####
        output$annoTable <- renderDT({
            # tic('Render Table')
            annoTable <- bind_rows(vals$annots)
            # toc()
            annoTable
        },
        server=FALSE,
        options=list(dom='rtip'),
        editable=TRUE)
        # Table Editors ####
        observeEvent(input$annoTable_cell_edit, {
            edit <- input$annoTable_cell_edit
            valAdd <- edit$value
            print(valAdd)
            if(edit$col %in% c(3, 4)) {
                valAdd <- as.POSIXct(valAdd, format='%Y-%m-%dT%H:%M:%S', tz='UTC')
            }
            print(valAdd)
            print(vals$annots[[edit$row]][edit$col])
            vals$annots[[edit$row]][[edit$col]] <- valAdd
        })
        observeEvent(input$removeAnno, {
            dropIx <- input$annoTable_rows_selected
            if(is.null(dropIx)) {
                showNotification('No rows selected')
                return()
            }
            vals$annots <- vals$annots[-dropIx]
        })
        output$downloadAnno <- downloadHandler(
            filename = function() {
                'DailyAnnotations.csv'
            },
            content = function(file) {
                out <- bind_rows(vals$annots)
                out$start <- psxTo8601(out$start)
                out$end <- psxTo8601(out$end)
                write.csv(out, file, row.names=FALSE)
            }
        )
    }
    runApp(shinyApp(ui=ui, server=server))
}

markDQMatrix <- function(dq, freqRange=NULL, timeRange=NULL, value=2, times, freqs) {
    if(is.null(freqRange) &&
       is.null(timeRange)) {
        return(dq)
    }
    if(!inherits(freqRange, c('NULL', 'list', 'numeric'))) {
        stop('freqRange must be NULL, a list of numeric ranges, or a numeric vector')
    }
    if(!inherits(timeRange, c('NULL', 'list', 'POSIXct'))) {
        stop('timeRange must be NULL, a list of POSIXct ranges, or a POSIXct vector')
    }
    if(is.numeric(freqRange)) {
        if(length(freqRange) != 2) {
            stop('freqRange must be a numeric range of two frequency values (Hz)')
        }
    }
    if(inherits(timeRange, 'POSIXct')) {
        if(length(timeRange) != 2) {
            stop('timeRange must be a POSIXct vector of two time values')
        }
    }
    # cast NULL / non-list to list so we can a
    if(!is.list(freqRange)) {
        freqRange <- list(freqRange)
    }
    if(!is.list(timeRange)) {
        timeRange <- list(timeRange)
    }
    nFreq <- length(freqRange)
    nTime <- length(timeRange)
    nVal <- length(value)
    maxLen <- max(nFreq, nTime, nVal)
    if((nFreq > 1 & nFreq < maxLen) ||
       (nTime > 1 & nTime < maxLen) ||
       (nVal > 1 & nVal < maxLen)) {
        stop('Length of freqRange, timeRange, and value must match (or be 1)')
    }
    if(nVal == 1) {
        value <- rep(value, maxLen)
    }
    if(nFreq == 1) {
        freqRange <- rep(freqRange, maxLen)
    }
    if(nTime == 1) {
        timeRange <- rep(timeRange, maxLen)
    }
    lowFreq <- freqs[-length(freqs)]
    highFreq <- freqs[-1]
    lowTime <- times[-length(times)]
    highTime <- times[-1]
    for(i in seq_len(maxLen)) {
        if(is.null(freqRange[[i]])) {
            freqIx <- 1:nrow(dq)
        } else {
            freqIx <- highFreq > freqRange[[i]][1] &
                lowFreq < freqRange[[i]][2]
        }
        if(is.null(timeRange[[i]])) {
            timeIx <- 1:ncol(dq)
        } else {
            timeIx <- highTime > timeRange[[i]][1] &
                lowTime < timeRange[[i]][2]
        }
        dq[freqIx, timeIx] <- value[i]
    }
    dq
}

compressDQData <- function(dqLong, doFreq=TRUE) {
    dqLong <- arrange(dqLong, .data$UTC)
    dqLong <- bind_rows(lapply(split(dqLong, list(dqLong$value, dqLong$frequency)), function(x) {
        if(nrow(x) <= 1) {
            return(x)
        }
        change <- c(FALSE, x$UTCend[1:(nrow(x)-1)] != x$UTC[2:nrow(x)])
        x$GROUP <- cumsum(change)
        x <- bind_rows(lapply(split(x, x$GROUP), function(y) {
            result <- y[1, ]
            result$UTCend <- y$UTCend[nrow(y)]
            result
        }))
        x$GROUP <- NULL
        x
    }))
    if(isFALSE(doFreq)) {
        return(dqLong)
    }
    dqLong <- arrange(dqLong, .data$freqLow)
    dqLong <- bind_rows(lapply(split(dqLong, list(dqLong$value, dqLong$UTC, dqLong$UTCend)), function(x) {
        # browser()
        if(nrow(x) <= 1) {
            return(x)
        }
        change <- c(FALSE, x$frequency[1:(nrow(x)-1)] != x$freqLow[2:nrow(x)])
        x$GROUP <- cumsum(change)
        x <- bind_rows(lapply(split(x, x$GROUP), function(y) {
            result <- y[1, ]
            result$frequency <- y$frequency[nrow(y)]
            result
        }))
        x$GROUP <- NULL
        x
    }))
    dqLong
}

loadQuality <- function(nc) {
    if(is.character(nc)) {
        nc <- nc_open(nc)
    }
    if(!inherits(nc, 'ncdf4')) {
        warning('Not a NetCDF')
        return(NULL)
    }
    if(!'quality_flag' %in% names(nc$var)) {
        inSize <- nc$var$psd$size
        dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
    } else {
        dqMat <- ncvar_get(nc, 'quality_flag', start=c(1,1), count=c(-1, -1))
    }
    times <- ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    list(dq=dqMat, time=times, freq=freqs)
}

# dq from loadQuality
prepQualityPlot <- function(dq) {
    labels <- dq$freq
    labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
    labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
    freqType <- checkFreqType(dq$freq)
    labels <- paste0(freqType, '_', labels)
    dqdf <- data.frame(t(dq$dq))
    colnames(dqdf) <- labels
    dqdf <- cbind(dq$time, dqdf)
    names(dqdf)[1] <- 'UTC'
    dqPlotBase <- longQuality(dqdf)
    dqPlotBase <- filter(dqPlotBase, .data$freqLow > 0)
    dqPlotBase$value <- factor(as.character(dqPlotBase$value), levels=as.character(1:4))
    
    dqPlotBase <- compressDQData(dqPlotBase, TRUE)
    dqPlotBase
}

longQuality <- function(x) {
    whichFreq <- whichFreqCols(x)
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq]))
    freqVals <- colsToFreqs(x)
    longSpec <- data.frame(.name=as.character(freqVals), .value='value')
    freqVals <- as.numeric(freqVals)
    longSpec$frequency <- freqVals
    longSpec$type <- type
    freqDiffs <- diff(freqVals)
    lowFreq <- freqDiffs[1] / (freqDiffs[2]/freqDiffs[1])
    
    freqDiffs <- c(lowFreq, freqDiffs)
    freqLows <- freqVals - freqDiffs
    longSpec$freqLow <- freqLows
    # the spec is to make it faster char->numeric conversion and add freqLow column
    # only need to specify here bc freqLow column
    x$UTCend <- x$UTC + unique(diff(as.numeric(x$UTC)))[1]
    x <- toLong(x, spec=longSpec)
    x
}

fastLTSAData <- function(x, 
                         freqRange=NULL, 
                         scale=c('log', 'linear'),
                         toTz='UTC') {
    x <- loadSoundscapeData(x, needCols='UTC')
    scale <- match.arg(scale)
    # we need a freqLow column later for the geom_, this block
    # is just making this faster by passing pivot_longer a
    # spec df for how to do the long-ing
    whichFreq <- whichFreqCols(x)
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq]))
    freqVals <- gsub('[A-z]+_', '', colnames(x)[whichFreq])
    x$UTC <- with_tz(x$UTC, tzone=toTz)
    bin <- unique(as.numeric(diff(x$UTC, units='secs')))
    
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
        x <- filter(x,
                    .data$frequency <= freqRange[2],
                    .data$frequency >= freqRange[1])
    }
    x$UTCend <- x$UTC + (bin)
    if(scale == 'log') {
        x <- filter(x, .data$freqLow > 0)
    }
    x
}

# runDailyQualityReview for name
# ncDir <- 'testData/NRS11'
# ncFile <- 'testData/NRS11/NRS11_20200101.nc'
# # ncData <- loadSoundscapeData(ncFile)
# hm <- runDataQualityReview(ncDir, plotQuality=T)
# 
# # TODO ####
# dq plotting and prepping is gross :(
# but it makes seeing marks very fast
