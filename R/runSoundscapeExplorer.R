#' @title Run Soundscape Explorer App
#'
#' @description Launches a shiny app that allows users to browse
#'   the various plotting functions available to visualize
#'   soundscape data
#'
#' @param data file path to soundscape data or data that has been loaded with
#'   \link{loadSoundscapeData}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @returns invisible TRUE
#'
#' @examples
#' if(interactive()) {
#'   hmd <- loadSoundscapeData(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
#'   runSoundscapeExplorer(hmd)
#' }
#'
#' @export
#'
#' @importFrom shiny addResourcePath removeResourcePath tabPanel navbarPage
#' @importFrom shiny fileInput textOutput verbatimTextOutput navbarMenu
#' @importFrom shiny h4 em updateNavbarPage renderText renderPrint
#' @importFrom shiny runApp selectInput updateSelectizeInput fluidRow column
#' @importFrom shiny renderPlot plotOutput reactiveValues observeEvent
#' @importFrom shiny tags sliderInput updateSelectInput shinyApp
#' @importFrom utils str
#'
runSoundscapeExplorer <- function(data=NULL) {
    # Data Prep and pre-App section ####
    if(!is.null(data)) {
        data <- loadSoundscapeData(data)
        freqCols <- colnames(data)[whichFreqCols(data)]
        freqVals <- colsToFreqs(freqCols)
        freqType <- checkFreqType(freqVals)
        otherCols <- colnames(data)[!colnames(data) %in% c('UTC', freqCols)]
    } else {
        freqCols <- ''
        freqVals <- NA
        freqType <- 'NA'
        otherCols <- character(0)
    }
    if(is.data.frame(data)) {
        DFNAME <- deparse(sys.call()[[2]])
    } else {
        DFNAME <- NULL
    }
    addResourcePath(prefix='scapex-images', system.file('images', package='PAMscapes'))
    on.exit(removeResourcePath('scapex-images'))
    # UI Begins ####
    buttonHeight <- '250px'
    ui <- navbarPage(
        ## Main Button Page ####
        id='main',
        'Soundscape Explorer',
        tabPanel(
            'Home',
            fluidRow(column(1), column(10, 'Click on a sample plot to try')),
            tags$button(id='btn_psd_q',
                        class='btn action-button',
                        tags$img(src='scapex-images/psd-q-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_psd_den',
                        class='btn action-button',
                        tags$img(src='scapex-images/psd-den-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_timeseries_line',
                        class='btn action-button',
                        tags$img(src='scapex-images/ts-line-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_timeseries_heat',
                        class='btn action-button',
                        tags$img(src='scapex-images/ts-heat-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_mts',
                        class='btn action-button',
                        tags$img(src='scapex-images/mts-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_hourlev',
                        class='btn action-button',
                        tags$img(src='scapex-images/hourlev-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_ltsa',
                        class='btn action-button',
                        tags$img(src='scapex-images/ltsa-ex.png',
                                 height=buttonHeight)
            )
        ),
        ## Data Page ####
        tabPanel(
            'Data',
            fileInput('dataLoad',
                      label='Select file(s) to load',
                      multiple=TRUE,
                      accept=c('csv', 'nc', 'rds')),
            textOutput('dataFreqSummary'),
            textOutput('dataNonFreq'),
            verbatimTextOutput('dataStr')
        ),
        ## Plot Section #####
        navbarMenu(
            'Plots',
            ### plotPSD ####
            tabPanel(
                'plotPSD',
                h4(em('plotPSD')),
                'PSD Style Plot',
                plotOutput('plot_psd'),
                fluidRow(
                    column(2,
                           selectInput('psd_style',
                                       label='Style',
                                       choices=c('quantile', 'density'),
                                       selected='quantile')
                    ),
                    column(3, sliderInput('psd_q',
                                          label='Quantile',
                                          min=0,
                                          max=.5,
                                          step=.01,
                                          value=0)),
                    column(2, selectInput('psd_by',
                                          label='By',
                                          choices=c('none', 'hour', 'month', 'year'),
                                          selected='none')),
                    column(2, selectInput('psd_facet',
                                          label='Facet',
                                          choices=c('none', 'hour', 'month', 'year'),
                                          selected='none'))
                ),
                'Copy/paste this code to recreate this plot:',
                verbatimTextOutput('code_psd')
            ),
            ### plotHourlyLevel ####
            tabPanel(
                'plotHourlyLevel',
                h4(em('plotHourlyLevel')),
                'Frequency vs. Hour of Day',
                plotOutput('plot_hourlev'),
                fluidRow(
                    
                ),
                'Copy/paste this code to recreate this plot:',
                verbatimTextOutput('code_hourlev')
            ),
            ### plotTimeseries ####
            tabPanel(
                'plotTimeseries',
                h4(em('plotTimeseries')),
                'Data Across Time',
                plotOutput('plot_timeseries'),
                fluidRow(
                    column(2,
                           selectInput('ts_style',
                                       label='Style',
                                       choices=c('line', 'heatmap'),
                                       selected='line')),
                    column(4,
                           selectInput('ts_column',
                                       label='Column to Plot',
                                       choices=NULL)),
                    column(4,
                           sliderInput('ts_q',
                                       label='Quantile',
                                       min=0,
                                       max=.5,
                                       step=.01,
                                       value=0)),
                    column(3,
                           selectInput('ts_by',
                                       label='By',
                                       choices='No Other Column'))
                ),
                'Copy/paste this code to recreate this plot:',
                verbatimTextOutput('code_timeseries')
                
            ),
            ### plotLTSA ####
            tabPanel(
                'plotLTSA',
                h4(em('plotLTSA')),
                'LTSA Style Plot',
                plotOutput('plot_ltsa'),
                fluidRow( # possibly add time bin
                    
                ),
                'Copy/paste this code to recreate this plot:',
                verbatimTextOutput('code_ltsa')
            ),
            ### plotScaledTimeseries ####
            tabPanel(
                'plotScaledTimeseries',
                h4(em('plotScaledTimeseries')),
                'Multiple Timeseries Plot',
                plotOutput('plot_multiseries'),
                fluidRow(
                    column(3,
                           selectInput('mts_freq',
                                       label='Frequency Column',
                                       choices=NULL)),
                    column(3,
                           selectInput('mts_other',
                                       label='Other Column',
                                       choices='No Other Columns'))
                ),
                'Copy/paste this code to recreate this plot:',
                verbatimTextOutput('code_multiseries')
            )
        )
    )
    # Server Begins ####
    server <- function(input, output, session) {
        # Intial column selection updates ####
        options(shiny.maxRequestSize = 30 * 1024^2)
        appData <- reactiveValues(data=data,
                                  freqCols=freqCols,
                                  freqVals=freqVals,
                                  freqType=freqType,
                                  otherCols=otherCols)
        # Setup reactives ####
        observeEvent(appData$data, {
            appData$freqCols <- colnames(appData$data)[whichFreqCols(appData$data)]
            appData$freqVals <- colsToFreqs(appData$freqCols)
            appData$freqType <- checkFreqType(appData$freqVals)
            appData$otherCols <- colnames(appData$data)[!colnames(appData$data) %in% c('UTC', appData$freqCols)]
            updateSelectizeInput(session, 'ts_column',
                                 choices=appData$freqCols,
                                 selected=appData$freqCols[1],
                                 server=TRUE)
            updateSelectizeInput(session, 'mts_freq',
                                 choices=appData$freqCols,
                                 selected=appData$freqCols[1],
                                 server=TRUE)
            
            otherPlotCols <- appData$otherCols
            # remove non-informative columns for coords
            otherPlotCols <- otherPlotCols[!otherPlotCols %in% c('Longitude', 'Latitude', 'matchLat', 'matchLong', 'matchTime')]
            numericCols <- sapply(appData$data[otherPlotCols], is.numeric)
            categoryCols <- sapply(appData$data[otherPlotCols], function(x) {
                is.character(x) | is.factor(x) | is.logical(x)
            })
            if(length(numericCols) > 0 && sum(numericCols) > 0) {
                updateSelectizeInput(session, 'mts_other',
                                     choices=otherPlotCols[numericCols],
                                     selected=otherPlotCols[numericCols][1])
            } else {
                updateSelectizeInput(session, 'mts_other',
                                     choices='No Other Columns',
                                     selected='No Other Columns')
            }
            if(length(categoryCols) > 0 && sum(categoryCols) > 0) {
                updateSelectizeInput(session, 'psd_by',
                                     choices=c('none', 'hour', 'month', 'year', otherPlotCols[categoryCols]),
                                     selected='none')
                updateSelectizeInput(session, 'psd_facet',
                                     choices=c('none', 'hour', 'month', 'year', otherPlotCols[categoryCols]),
                                     selected='none')
                updateSelectizeInput(session, 'ts_by',
                                     choices=c('none', otherPlotCols[categoryCols]),
                                     selected='none')
            } else {
                updateSelectizeInput(session, 'psd_by',
                                     choices=c('none', 'hour', 'month', 'year'),
                                     selected='none')
                updateSelectizeInput(session, 'psd_facet',
                                     choices=c('none', 'hour', 'month', 'year'),
                                     selected='none')
                updateSelectizeInput(session, 'ts_by',
                                     choices='none',
                                     selected='none')
            }
        })
        # Image grid navigation ####
        observeEvent(input$btn_psd_q, {
            updateSelectInput(session, 'psd_style', selected='quantile')
            updateNavbarPage(session, 'main', 'plotPSD')
        })
        observeEvent(input$btn_psd_den, {
            updateSelectInput(session, 'psd_style', selected='density')
            updateNavbarPage(session, 'main', 'plotPSD')
        })
        observeEvent(input$btn_timeseries_line, {
            updateSelectInput(session, 'ts_style', selected='line')
            updateNavbarPage(session, 'main', 'plotTimeseries')
        })
        observeEvent(input$btn_timeseries_heat, {
            updateSelectInput(session, 'ts_style', selected='heatmap')
            updateNavbarPage(session, 'main', 'plotTimeseries')
        })
        observeEvent(input$btn_hourlev, {
            updateNavbarPage(session, 'main', 'plotHourlyLevel')
        })
        observeEvent(input$btn_ltsa, {
            updateNavbarPage(session, 'main', 'plotLTSA')
        })
        observeEvent(input$btn_mts, {
            updateNavbarPage(session, 'main', 'plotScaledTimeseries')
        })
        # Data Loading ####
        observeEvent(input$dataLoad, {
            inFile <- input$dataLoad$datapath
            if(all(grepl('rds$', inFile))) {
                inFile <- do.call(rbind, lapply(inFile, readRDS))
            }
            inFile <- loadSoundscapeData(inFile)
            appData$data <- inFile
        })
        # Data Render ####
        output$dataFreqSummary <- renderText(paste0(
            nrow(appData$data), ' ',
            switch(appData$freqType,
                   'OL' = 'Ocatave Level',
                   'TOL' = 'Third Octave Level',
                   'PSD' = 'Power Spectral Density',
                   'HMD' = 'Hybrid Millidecade',
                   'FREQ' = 'Other'),
            ' measurements at ',
            length(appData$freqVals),
            ' frequency values ranging from ',
            round(min(appData$freqVals), 2), ' to ', round(max(appData$freqVals), 2)
        ))
        
        output$dataNonFreq <- renderText(paste0(
            '\nAlso contains column(s): ',
            paste0(appData$otherCols, collapse=', ')
        ))
        output$dataStr <- renderPrint(str(appData$data, list.len=10))
        # Plot Rendering ####
        output$plot_timeseries <- renderPlot({
            if(input$ts_by == 'none' || input$ts_style=='heatmap') {
                tsBy <- NULL
            } else {
                tsBy <- input$ts_by
            }
            plotTimeseries(appData$data,
                           column=input$ts_column,
                           q=input$ts_q,
                           style=input$ts_style,
                           by=tsBy)
        })
        output$code_timeseries <- renderPrint({
            argList <- list(x=data, 
                            column=input$ts_column,
                            style=input$ts_style)
            if(input$ts_by != 'none' && input$ts_style != 'heatmap') {
                argList$by <- input$ts_by
            }
            if(input$ts_style != 'heatmap') {
                argList$q <- input$ts_q
            }
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotTimeseries(', argText, ')')
            cat(argText)
            # cat('plotTimeseries(data',
            #     ', column="', input$ts_column, '"',
            #     ifelse(input$ts_style=='heatmap', '', paste0(', q=', input$ts_q)),
            #     ', style="', input$ts_style, '"',
            #     ifelse(is.null(tsBy), '', paste0(', by="', tsBy, '"')),
            #     ')', sep='')
        })
        output$plot_psd <- renderPlot({
            if(input$psd_by == 'none' ||
               input$psd_style == 'density') {
                psdBy <- NULL
            } else {
                psdBy <- input$psd_by
            }
            if(input$psd_facet == 'none') {
                psdFacet <- NULL
            } else {
                psdFacet <- input$psd_facet
            }
            plotPSD(appData$data, style=input$psd_style, q=input$psd_q,
                    by=psdBy, facet=psdFacet)
        })
        output$code_psd <- renderPrint({
            argList <- list(x=data,
                            style=input$psd_style)
            if(input$psd_style != 'density') {
                argList$q <- input$psd_q
            }
            if(input$psd_by != 'none' && input$psd_style != 'density') {
                argList$by <- input$psd_by
            }
            if(input$psd_facet != 'none') {
                argList$facet <- input$psd_facet
            }
                            
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotPSD(', argText, ')')
            cat(argText)
            # cat('plotPSD(data',
            #     ', style="', input$psd_style, '"',
            #     ifelse(input$psd_by == 'none' || input$psd_style=='density',
            #            '',
            #            paste0(', by="', input$psd_by, '"')),
            #     ifelse(input$psd_facet == 'none',
            #            '',
            #            paste0(', facet="', input$psd_facet, '"')),
            #     ifelse(input$psd_style=='density',
            #            '',
            #            paste0(', q=', input$psd_q)),
            #     ')', sep='')
            
        })
        output$plot_hourlev <- renderPlot({
            plotHourlyLevel(appData$data)
        })
        output$code_hourlev <- renderPrint({
            argList <- list(x=data)
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotHourlyLevel(', argText, ')')
            cat(argText)
            # cat('plotHourlyLevel(data)')
        })
        output$plot_ltsa <- renderPlot({
            plotLTSA(appData$data)
        })
        output$code_ltsa <- renderPrint({
            argList <- list(x=data)
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotLTSA(', argText, ')')
            cat(argText)
            # cat('plotLTSA(data)')
        })
        output$plot_multiseries <- renderPlot({
            mtsCols <- if(input$mts_other == 'No Other Columns') {
                input$mts_freq
            } else {
                c(input$mts_freq, input$mts_other)
            }
            plotScaledTimeseries(appData$data,
                                 columns=mtsCols,
                                 lwd=rev(c(.5, 1)[1:length(mtsCols)]))
        })
        output$code_multiseries <- renderPrint({
            argList <- list(x=data)
            # mtsCols <- if(input$mts_other == 'No Other Columns') {
            #     input$mts_freq
            # } else {
            #     c(input$mts_freq, input$mts_other)
            # }
            if(input$mts_other == 'No Other Columns') {
                argList$columns <- input$mts_freq
                argList$lwd <- c(.5)
            } else {
                argList$columns <- c(input$mts_freq, input$mts_other)
                argList$lwd <- c(1, .5)
            }
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotScaledTimeseries(', argText, ')')
            cat(argText)
            # cat('plotScaledTimeseries(data',
            #     ', columns=c(',
            #     paste0('"', mtsCols, '"', collapse=', '), ')',
            #     ', lwd=',
            #     ifelse(length(mtsCols)==1, '.5', 'c(1, .5)'),
            #     ')', sep='')
        })
    }
    runApp(shinyApp(ui=ui, server=server))
    invisible(TRUE)
}

# possible next to do - adding more options
# possibly time bins for LTSA / timeseries
# text output of function call? IDK if thats actually better
# than linking to documentation page
# real next step will be linking to a documentation page that actually
# lists out all the possible options, no need to try and show everything here

# runSoundscapeExplorer()

