# shiny app for lookin et soond scoop qoolitys
library(shiny)
library(ncdf4)
library(PAMscapes)
library(ggplot2)
library(DT)
runDataQualityReview <- function(nc, outFile=NULL) {
    ui <- navbarPage(
        id='Main',
        'Data Quality Reviewer',
        tabPanel(
            'Plots',
            paste0('LTSA of file ', basename(nc)),
            fluidRow(
                plotOutput(outputId = 'ltsaPlot', brush=brushOpts(id='ltsaBrush'))
            ),
            fluidRow(
                plotOutput(outputId = 'dqPlot')
            ),
            fluidRow(
                column(2, actionButton('addButton', label='Add Annotation')),
                column(3, selectizeInput('dqValue', label='Quality Flag',
                                         choices=list('(1) Good'=1,
                                                      '(2) Not Evaluated'=2,
                                                      '(3) Compromised'=3,
                                                      '(4) Bad'=4), selected=2)),
                column(6, verbatimTextOutput('currentRange'))
            )
        ),
        tabPanel(
            'Annotations',
            DTOutput('annoTable'),
            actionButton('removeAnno', label='Remove Annotation')
        )
    )
    server <- function(input, output, session) {
        dqInit <- loadQuality(nc)
        labels <- dqInit$freq
        labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
        labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
        freqType <- PAMscapes:::checkFreqType(dqInit$freq)
        labels <- paste0(freqType, '_', labels)
        dqdf <- data.frame(t(dqInit$dq))
        colnames(dqdf) <- labels
        dqdf <- cbind(dqInit$time, dqdf)
        names(dqdf)[1] <- 'UTC'
        dqPlotBase <- longQuality(dqdf)
        dqPlotBase$value <- factor(as.character(dqPlotBase$value), levels=as.character(1:4))
        dqPlotBase <- smollify(dqPlotBase, TRUE)
        vals <- reactiveValues(
            data=loadSoundscapeData(nc, keepQuals = 1:4),
            timeRange = NA,
            freqRange = NA,
            dq = dqInit$dq,
            dqBasePlot = NA,
            dqTime=dqInit$time,
            dqFreq=dqInit$freq,
            annots=NULL
        )
        plotColors <- c('1'='darkgreen', '2'='steelblue', '3'='yellow', '4'='red')
        output$ltsaPlot <- renderPlot({
            plotLTSA(vals$data, bin='30min', maxBins=2e3)
        })
        output$dqPlot <- renderPlot({
            dqPlot <- ggplot() +
                geom_rect(data=dqPlotBase, aes(xmin=UTC,
                                               xmax=UTCend,
                                               ymin=freqLow,
                                               ymax=frequency,
                                               fill=value),
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
                             fill=plotColors[vals$annots[[i]]$value])
            }
            dqPlot
        })
        observeEvent(input$ltsaBrush, {
            plotData <- plotLTSA(vals$data, returnData = TRUE)
            brush <- input$ltsaBrush
            vals$timeRange <- as.POSIXct(round(c(brush$xmin, brush$xmax), 0), origin='1970-01-01 00:00:00', tz='UTC')
            vals$freqRange <- round(c(brush$ymin, brush$ymax), 1)
        })
        observeEvent(input$addButton, {
            vals$dq <- markDQMatrix(vals$dq,
                                    freqRange=vals$freqRange,
                                    timeRange=vals$timeRange,
                                    value=input$dqValue,
                                    times=vals$dqTime,
                                    freqs=vals$dqFreq)
            newAnnot <- list(
                freqMin=vals$freqRange[1],
                freqMax=vals$freqRange[2],
                start=vals$timeRange[1],
                end=vals$timeRange[2],
                value=input$dqValue
            )
            if(is.null(vals$annots)) {
                vals$annots <- list(newAnnot)
            } else {
                vals$annots[[length(vals$annots)+1]] <- newAnnot
            }
        })
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
        output$annoTable <- renderDT({
            annoTable <- bind_rows(vals$annots)
            annoTable
        },
        server=FALSE,
        options=list(dom='rtip'))
        observeEvent(input$removeAnno, {
            dropIx <- input$annoTable_rows_selected
            if(is.null(dropIx)) {
                showNotification('No rows selected')
                return()
            }
            vals$annots <- vals$annots[-dropIx]
        })
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

smollify <- function(dqLong, doFreq=TRUE) {
    dqLong <- arrange(dqLong, UTC)
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
    dqLong <- arrange(dqLong, freqLow)
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
    times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    list(dq=dqMat, time=times, freq=freqs)
}

longQuality <- function(x) {
    whichFreq <- PAMscapes:::whichFreqCols(x)
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq]))
    freqVals <- PAMscapes:::colsToFreqs(x)
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
    x <- PAMscapes:::toLong(x, spec=longSpec)
    x
}

runDataQualityReview('../Data/ncmod/modified.nc')



