# shiny app for lookin et soond scoop qoolitys
library(shiny)
library(ncdf4)
library(PAMscapes)
runDataQualityReview <- function(nc, outFile=NULL) {
    ui <- fluidPage(
        paste0('LTSA of file ', basename(nc)),
        fluidRow(
            plotOutput(outputId = 'ltsaPlot', brush=brushOpts(id='ltsaBrush')),
            actionButton('test', label='Clickyboi')
        ),
        fluidRow(
            plotOutput(outputId = 'dqPlot')
        ),
        fluidRow(
            verbatimTextOutput('currentRange')
        )
    )
    server <- function(input, output, session) {
        vals <- reactiveValues(
            data=loadSoundscapeData(nc, keepQuals = 1:4),
            timeRange = NA,
            freqRange = NA,
            dq = NULL,
        )
        
        dq <- loadQuality(nc)
        labels <- dq$freq
        labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
        labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
        freqType <- PAMscapes:::checkFreqType(dq$freq)
        labels <- paste0(freqType, '_', labels)
        dqdf <- data.frame(t(dq$dq))
        colnames(dqdf) <- labels
        dqdf <- cbind(dq$time, dqdf)
        names(dqdf)[1] <- 'UTC'
        dqLong <- longQuality(dqdf)
        
        vals$dq <- dqLong
        
        output$ltsaPlot <- renderPlot({
            plotLTSA(vals$data, bin='10min', maxBins=2e3)
        })
        output$dqPlot <- renderPlot({
            if(is.null(vals$dq)) {
                return(NA)
            }
            dq <- ggplot() +
                geom_rect(data=vals$dq, aes(xmin=UTC,
                                            xmax=UTCend,
                                            ymin=freqLow,
                                            ymax=frequency,
                                            fill=as.character(value))) +
                scale_x_datetime(expand=c(0,0)) +
                scale_y_log10(expand=c(0,0)) +
                theme(legend.title = element_text(angle=90)) +
                guides(fill=guide_legend(title.position='right', title.hjust=.5))
            dq
        })
        observeEvent(input$ltsaBrush, {
            plotData <- plotLTSA(vals$data, returnData = TRUE)
            brush <- input$ltsaBrush
            vals$timeRange <- as.POSIXct(round(c(brush$xmin, brush$xmax), 0), origin='1970-01-01 00:00:00', tz='UTC')
            vals$freqRange <- round(c(brush$ymin, brush$ymax), 1)
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
        observeEvent(input$test, {
            print(vals$timeRange)
            print(vals$freqRange)
        })
    }
    
    runApp(shinyApp(ui=ui, server=server))
}
runDataQualityReview('../Data/ncmod/modified.nc')



