#' @title Run QAQC Review Shiny App
#' 
#' @description Launches a shiny app that allows users to review QAQC outputs
#'   created by \link{evaluateDeployment} and interactively mark potential
#'   problems to investigate. Potential problems can be saved in an "Issues Log"
#'   that can be downloaded as a CSV file.
#'
#' @param data output from \link{evaluateDeployment} either as a dataframe
#'   or path to a CSV file containing the same data
#' @param issue if not \code{NULL}, a dataframe or path to CSV file of
#'   issue logs previously created with this function
#' @param freqLims frequency limits (Hz) for plotting
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return invisibly \code{data}
#' 
#' @importFrom DT DTOutput renderDT
#' @importFrom shiny fluidPage actionButton textInput downloadButton
#' @importFrom shiny brushOpts textAreaInput checkboxGroupInput
#' @importFrom shiny updateSliderInput updateCheckboxGroupInput HTML
#' @importFrom shiny showNotification downloadHandler stopApp brushedPoints
#' 
#' @export
#' 
runQAQCReview <- function(data, issue=NULL, freqLims=c(30, Inf)) {
    # check input type
    INCHAR <- is.character(data)
    if(INCHAR && !file.exists(data)) {
        stop('File or directory ', data, ' does not exist.')
    }
    # if folder input expecting folder with qLog and issLog
    INDIR <- INCHAR && dir.exists(data)
    if(INDIR) {
        # INPATH <- data
        # data <- readQLog(INPATH)
        # if(INDIR && is.null(issue)) {
        #     issue <- readIssueLog(INPATH)
        # }
    }
    if(INCHAR && !INDIR) {
        data <- read.csv(data, stringsAsFactors = FALSE)
        data$UTC <- parse_date_time(data$UTC, orders='%Y-%m-%d %H:%M:%S', tz='UTC')
    }
    if(isQaqcLog(data)) {
        INLOG <- TRUE
    # check for single QAQC input
    } else if(any(grepl('TOL_', colnames(data))) &&
              all(c('UTC', 'wavLength') %in% colnames(data))) {
        INLOG <- FALSE
    } else {
        stop('Unknown input - not QAQC log or single deployment output')
    }
    if(is.null(issue)) {
        # template of issue log
        issueLog <- data.frame(
            UTC=NA,
            file=NA, #wav file
            value=NA, #freq for TOL, gap amt for GAP?
            projectName=NA,
            source=NA, #TOL, GAP
            comment=NA, # box
            issueChecked=NA,
            qaqcDate=NA # keep track of when we marked this?
        )
        issue <- issueLog[FALSE, ]
    }
    ui <- fluidPage(
        style = "padding: 0px;", # no gap in navbar
        navbarPage(
            id='main',
            'QAQC Time',
            # Data Page ####
            tabPanel(
                'Home',
                DTOutput('inData'),
                checkboxGroupInput('statusCheck', 
                                   choices=QAQC_STATUS, 
                                   selected=c('QAQCRun', 'QAQCReviewed'),
                                   label=NULL,
                                   inline=TRUE),
                fluidRow(
                    column(2,
                           actionButton('loadSelected', label='Load Selected',
                                        style='margin-top:24px;')
                    ),
                    column(4,
                           downloadButton('downloadLog', label='Download Log',
                                          style='margin-top:24px;')
                    ),
                    column(4,
                           selectInput('statusValue', label='Change QAQC Status',
                                       choices=QAQC_STATUS)
                    ),
                    column(2,
                           actionButton('changeStatus', label='Change',
                                        style='margin-top:24px;')
                    )
                ),
                h4('Current QAQC directory'),
                verbatimTextOutput('qaqcDir'),
                textInput('changeQaqcDir', label='Change base directory', value='', width='100%'),
                fluidRow(
                    column(10,
                           actionButton('changeDirButton', label='Change')
                    )
                )
            ),
            
            # Plot TOL ####
            tabPanel(
                'TOL',
                plotOutput('tolPlot',
                           brush = brushOpts(id = "tolBrush", fill = "#ccc", direction = "x")),
                sliderInput('timeSliderTol', label='Time Range', 
                            min=0, max=1, value=c(0, 1),
                            width='100%'),
                checkboxGroupInput('tolFreqs', choices=NULL, label='Frequency to Show',
                                   inline=TRUE),
                fluidRow(
                    column(10,
                           textAreaInput('tolComment', label='Comment', width='100%')
                    ),
                    column(2,
                           actionButton('tolLogIssue', label='Log Issue', width='100%', height='100%',
                                        style='font-weight:bold;margin-top:35px;')
                    )
                ),
                verbatimTextOutput('tolBrushOut')
            ),
            # Plot Gaps ####
            tabPanel(
                'Data Gaps',
                plotOutput('gapPlot',
                           brush = brushOpts(id='gapBrush')),
                sliderInput('timeSliderGap', label='Time Range',
                            min=0, max=1, value=c(0, 1),
                            width='100%'),
                fluidRow(
                    column(10,
                           textAreaInput('gapComment', label='Comment', width='100%')
                    ),
                    column(2,
                           actionButton('gapLogIssue', label='Log Issue', width='100%', height='100%',
                                        style='font-weight:bold;margin-top:35px;')
                    )
                ),
                verbatimTextOutput('gapBrushOut')
            ),
            # Plot TV ####
            tabPanel(
                'Temp Volt',
                plotOutput('tvPlot',
                           brush=brushOpts(id='tvBrush', direction='x')),
                fluidRow(
                    column(10,
                           textAreaInput('tvComment', label='Comment', width='100%')
                    ),
                    column(2,
                           actionButton('tvLogIssue', label='Log Issue', width='100%', height='100%',
                                        style='font-weight:bold;margin-top:35px;')
                    )
                ),
                verbatimTextOutput('tvBrushOut')
            ),
            # Issue Log ####
            tabPanel(
                'Issue Log',
                h4('Issue Log'),
                DTOutput('issueData'),
                fluidRow(
                    column(3,
                           actionButton('removeIssue',
                                        label='Remove Selected Row(s)')
                    ),
                    column(4,
                           downloadButton('downloadIssues', label='Download Issues')
                    )
                ),
                fluidRow(column(3, em('(only rows added during this session will be successfully removed)')))
            ),
            # Stop Page ####
            tabPanel(
                'Save and Exit',
                h4('Save any updates to Log & Issues before hitting "Stop App"'),
                fluidRow(
                    column(2, 
                           actionButton('saveLog', label='Save Log Data')
                    ),
                    column(5,
                           verbatimTextOutput('logSaveTime')
                    )
                ),
                fluidRow(
                    column(2, 
                           actionButton('saveIssues', label='Save Issues Data')
                    ),
                    column(5, verbatimTextOutput('issueSaveTime')
                    )
                ),
                actionButton('stopApp', label='Stop App', style='font-weight:bold;')
            )
        )
    )
    server <- function(input, output, session) {
        
        # Setup Reactives ####
        if(isFALSE(INLOG)) {
            appData <- reactiveValues(
                log=NULL,
                data=data,
                freqLevs=1,
                project='',
                issue=issue,
                rowLoaded=NULL,
                logSaveTime=NULL,
                issueSaveTime=NULL
            )
        } else {
            # if('QAQCRun' %in% data$qaqcStatus) {
            #     loadIx <- which(data$qaqcStatus == 'QAQCRun')[1]
            # } else if('QAQCReviewed' %in% data$qaqcStatus) {
            #     loadIx <- which(data$qaqcStatus == 'QAQCReviewed')[1]
            # } else if('QAQCComplete' %in% data$qaqcStatus) {
            #     loadIx <- which(data$qaqcStatus == 'QAQCComplete')[1]
            # } else {
            #     loadIx <- 1
            # }
            # appData <- reactiveValues(
            #     log=data,
            #     data=readQData(file.path(data$qaqcBaseDir[loadIx], data$qaqcDir[loadIx]), 
            #                    pattern=data$deviceId[loadIx]),
            #     freqLevs=1,
            #     project=data$projectName[loadIx],
            #     issue=issue,
            #     rowLoaded=loadIx,
            #     logSaveTime=NULL,
            #     issueSaveTime=NULL
            # )
        }
        observeEvent(appData$data, {
            freqCols <- colsToFreqs(names(appData$data)[whichFreqCols(appData$data)])
            freqCols <- freqCols[freqCols >= freqLims[1] & freqCols <= freqLims[2]]
            olLevels <- getOctaveLevels('ol', freqRange=range(freqCols))
            freqCols <- freqCols[freqCols %in% olLevels$freqs]
            appData$freqLevs <- freqCols
            timeRange <- range(appData$data$UTC)
            timeFormat <- ifelse(as.numeric(difftime(timeRange[2], timeRange[1], units='days')) > 2,
                                 '%F',
                                 '%m-%d %H:%M')
            updateSliderInput(inputId='timeSliderTol',
                              min=timeRange[1],
                              max=timeRange[2],
                              value=timeRange,
                              timeFormat=timeFormat,
                              timezone='UTC')
            updateSliderInput(inputId='timeSliderGap',
                              min=timeRange[1],
                              max=timeRange[2],
                              value=timeRange,
                              timeFormat=timeFormat,
                              timezone='UTC')
            freqText <- makeHtmlColors(freqCols)
            updateCheckboxGroupInput(inputId='tolFreqs',
                                     choiceNames=lapply(freqText, HTML),
                                     choiceValues=freqCols,
                                     selected = freqCols, inline = TRUE)
        })
        observeEvent(input$loadSelected, {
            if(isFALSE(INLOG)) {
                showNotification('Button only active for QAQC logs')
                return(NULL)
            }
            # logIx <- input$inData_rows_selected
            # if(is.null(logIx)) {
            #     showNotification('No row selected')
            #     return(NULL)
            # }
            # projLoad <- appData$log$projectName[
            #     appData$log$qaqcStatus %in% input$statusCheck
            # ][logIx]
            # projIx <- which(appData$log$projectName == projLoad)
            # appData$data <- readQData(
            #     file.path(appData$log$qaqcBaseDir[projIx],
            #               appData$log$qaqcDir[projIx]),
            #     pattern=appData$log$deviceId[projIx]
            # )
            # appData$project <- appData$log$projectName[projIx]
            # appData$rowLoaded <- projIx
            # showNotification('Data load complete!')
        })
        # Render Table ####
        output$inData <- renderDT({
            if(INLOG) {
                out <- appData$log[c('projectName', 'qaqcStatus')]
                out <- out[out$qaqcStatus %in% input$statusCheck, ]
            } else {
                # out <- appData$data[c('UTC', 'file')]
                out <- data.frame(projectName = appData$data$projectName[1],
                                  nFiles = nrow(appData$data))
            }
            out
        },
        selection=list(mode='single'),
        options=list(dom='tip')
        )
        output$downloadLog <- downloadHandler(
            filename = function() {
                'QAQC_Log.csv'
            },
            content = function(file) {
                write.csv(appData$log, file, row.names=FALSE)
            }
        )
        observeEvent(input$stopApp, {
            stopApp()
        })
        observeEvent(input$changeStatus, {
            if(isFALSE(INLOG)) {
                showNotification('Only active for log data')
                return()
            }
            logIx <- input$inData_rows_selected
            if(is.null(logIx)) {
                showNotification('No row selected')
                return()
            }
            projLoad <- appData$log$projectName[
                appData$log$qaqcStatus %in% input$statusCheck
            ][logIx]
            projIx <- which(appData$log$projectName == projLoad)
            appData$log$qaqcStatus[projIx] <- input$statusValue
        })
        output$qaqcDir <- renderText({
            text <- ''
            if(isFALSE(INLOG)) {
                return(text)
            }
            if(is.null(appData$data)) {
                text <- paste0(text, 'No QAQC data found in directory:\n')
            }
            text <- paste0(text,
                           'Project: ', appData$project)
            text <- paste0(text,
                           '\nBase: ', appData$log$qaqcBaseDir[appData$rowLoaded])
            text <- paste0(text,
                           '\nFolder: ', appData$log$qaqcDir[appData$rowLoaded])
            text
        })
        observeEvent(input$changeDirButton, {
            if(isFALSE(INLOG)) {
                showNotification('Only active for log data')
                return()
            }
            # changeIx <- appData$log$qaqcBaseDir == appData$log$qaqcBaseDir[appData$rowLoaded]
            # appData$log$qaqcBaseDir[changeIx] <- input$changeQaqcDir
            # appData$data <- readQData(
            #     file.path(appData$log$qaqcBaseDir[appData$rowLoaded],
            #               appData$log$qaqcDir[appData$rowLoaded]),
            #     pattern=appData$log$deviceId[appData$rowLoaded]
            # )
            # showNotification('Base directory changed!')
        })
        # Plot - TOL ####
        output$tolPlot <- renderPlot({
            if(is.null(appData$data)) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='No data loaded')
                return()
            }
            plotData <- toLong(appData$data)
            if(!is.null(input$tolFreqs)) {
                plotData <- filter(plotData, .data$frequency %in% input$tolFreqs)
            }
            if(inherits(input$timeSliderTol[1], 'POSIXct')) {
                # plotData <- filter(plotData, .data$UTC >= input$timeSliderTol[1] &
                #                               .data$UTC <= input$timeSliderTol[2])
                tRange <- c(input$timeSliderTol[1], input$timeSliderTol[2])
            } else {
                tRange <- range(plotData$UTC)
            }
            plotLevels <- getOctaveLevels('ol', freqRange=range(plotData$frequency))
            plotData <- plotData[plotData$frequency %in% plotLevels$freqs, ]
            plotData$frequency <- factor(plotData$frequency, levels=appData$freqLevs)
            ggplot(plotData, aes(x=.data$UTC, y=.data$value, color=.data$frequency)) +
                geom_line() +
                scale_color_manual(values=hue_pal()(length(appData$freqLevs)), 
                                   breaks=appData$freqLevs) +
                scale_x_datetime(expand=c(0, 0)) +
                coord_cartesian(xlim=tRange) +
                ggtitle(appData$project)
        })
        # Brush - TOL ####
        output$tolBrushOut <- renderPrint({
            if(is.null(appData$data)) {
                return('No data loaded')
            }
            plotData <- toLong(appData$data)
            if(!is.null(input$tolFreqs)) {
                plotData <- filter(plotData, .data$frequency %in% input$tolFreqs)
            }
            plotLevels <- getOctaveLevels('ol', freqRange=range(plotData$frequency))
            plotData <- plotData[plotData$frequency %in% plotLevels$freqs, ]
            plotData$frequency <- factor(plotData$frequency, levels=appData$freqLevs)
            brushData <- toWide(brushedPoints(plotData, brush=input$tolBrush, xvar='UTC'))
            # if(nrow(brushData) > 0) {
            #     brushFreqs <- names(brushData)[PAMscapes:::whichFreqCols(brushData)]
            # } else {
            #     brushFreqs <- NULL
            # }
            brushData <- brushData[c('UTC', 'file')]
            if(nrow(brushData) > 0) {
                brushData <- brushData[brushData$UTC %in% range(brushData$UTC), ]
            }
            freqVals <- paste0(input$tolFreqs, collapse=',')
            brushData$frequency <- rep(freqVals, nrow(brushData))
            brushData$comment <- rep(input$tolComment, nrow(brushData))
            brushData
        })
        observeEvent(input$tolLogIssue, {
            plotData <- toLong(appData$data)
            if(!is.null(input$tolFreqs)) {
                plotData <- filter(plotData, .data$frequency %in% input$tolFreqs)
            }
            plotLevels <- getOctaveLevels('ol', freqRange=range(plotData$frequency))
            plotData <- plotData[plotData$frequency %in% plotLevels$freqs, ]
            plotData$frequency <- factor(plotData$frequency, levels=appData$freqLevs)
            brushData <- toWide(brushedPoints(plotData, brush=input$tolBrush, xvar='UTC'))
            if(nrow(brushData) == 0) {
                showNotification('No data selected.')
                return(NULL)
            }
            # if(nrow(brushData) > 0) {
            #     brushFreqs <- names(brushData)[PAMscapes:::whichFreqCols(brushData)]
            # } else {
            #     brushFreqs <- NULL
            # }
            brushData <- brushData[c('UTC', 'file')]
            brushData <- brushData[brushData$UTC %in% range(brushData$UTC), ]
            freqVals <- paste0(input$tolFreqs, collapse=',')
            brushData$value <- rep(freqVals, nrow(brushData))
            brushData$projectName <- appData$project
            brushData$source <- 'TOL'
            brushData$comment <- rep(input$tolComment, nrow(brushData))
            brushData$issueChecked <- 'no'
            nowUTC <- Sys.time()
            attr(nowUTC, 'tzone') <- 'UTC'
            brushData$qaqcDate <- nowUTC
            if(nrow(appData$issue) == 0) {
                appData$issue <- brushData
            } else {
                appData$issue <- bind_rows(appData$issue, brushData)
            }
            showNotification('Issue Logged')
        })
        # Plot - Gap ####
        output$gapPlot <- renderPlot({
            if(is.null(appData$data)) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='No data loaded')
                return()
            }
            gapData <- appData$data[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
            names(gapData)[3:4] <- c('Wav End to Next File (s)',
                                     'Time Between File Start (s)')
            gapData <- pivot_longer(gapData, cols=c('Wav End to Next File (s)',
                                                           'Time Between File Start (s)'))
            if(inherits(input$timeSliderGap[1], 'POSIXct')) {
                # gapData <- filter(gapData, .data$UTC >= input$timeSliderGap[1] &
                #                              .data$UTC <= input$timeSliderGap[2])
                timeRange <- c(input$timeSliderGap[1], input$timeSliderGap[2])
            } else {
                timeRange <- range(gapData$UTC)
            }
            # units opts?
            ggplot(gapData, aes(x=.data$UTC, y=.data$value)) +
                geom_line() +
                scale_x_datetime(expand=c(0,0)) +
                coord_cartesian(xlim=timeRange) +
                facet_wrap(~name, scales='free', ncol=1) +
                theme(strip.text.x = element_text(size=12)) +
                ggtitle(appData$project)
        })
        # Brush - Gap ####
        output$gapBrushOut <- renderPrint({
            if(is.null(appData$data)) {
                return('No data loaded')
            }
            
            gapData <- appData$data[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
            names(gapData)[3:4] <- c('Wav End to Next File (s)',
                                     'Time Between File Start (s)')
            gapData <- pivot_longer(gapData, cols=c('Wav End to Next File (s)',
                                                           'Time Between File Start (s)'))
            gapBrushData <- brushedPoints(gapData, brush=input$gapBrush, xvar='UTC', yvar='value')
            gapBrushData$comment <- rep(input$gapComment, nrow(gapBrushData))
            gapBrushData
        })
        observeEvent(input$gapLogIssue, {
            gapData <- appData$data[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
            names(gapData)[3:4] <- c('Wav End to Next File (s)',
                                     'Time Between File Start (s)')
            gapData <- pivot_longer(gapData, cols=c('Wav End to Next File (s)',
                                                           'Time Between File Start (s)'))
            gapBrushData <- brushedPoints(gapData, brush=input$gapBrush, xvar='UTC', yvar='value')
            if(nrow(gapBrushData) == 0) {
                showNotification('No data selected.')
                return(NULL)
            }
            gapBrushData <- gapBrushData[c('UTC', 'file', 'value', 'name')]
            names(gapBrushData) <- c('UTC', 'file', 'value', 'source')
            gapBrushData$value <- as.character(round(gapBrushData$value, 3))
            gapBrushData$projectName <- appData$project
            gapBrushData$source <- paste0('GapPlot-', gapBrushData$source)
            gapBrushData$comment <- rep(input$gapComment, nrow(gapBrushData))
            gapBrushData$issueChecked <- 'no'
            nowUTC <- Sys.time()
            attr(nowUTC, 'tzone') <- 'UTC'
            gapBrushData$qaqcDate <- nowUTC
            gapBrushData <- gapBrushData[
                c('UTC', 'file', 'value', 'projectName', 'source', 'comment', 'issueChecked', 'qaqcDate')]
            if(nrow(appData$issue) == 0) {
                appData$issue <- gapBrushData
            } else {
                appData$issue <- bind_rows(appData$issue, gapBrushData)
            }
            showNotification('Issue Logged')
        })
        # Plot - TV ####
        output$tvPlot <- renderPlot({
            if(is.null(appData$data)) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='No data loaded')
                return()
            }
            if(!all(c('intBatt', 'temp') %in% colnames(appData$data))) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='No temperature / voltage data found')
                return()
            }
            tvCols <- c('UTC', 'intBatt', 'extBatt', 'temp')
            tvCols <- tvCols[tvCols %in% names(appData$data)]
            tvData <- appData$data[tvCols]
            plotQAQCTV(tvData, title=appData$projectName)
        })
        # Brush - TV ####
        output$tvBrushOut <- renderPrint({
            if(is.null(appData$data)) {
                return('No data loaded')
            }
            if(!all(c('intBatt', 'temp') %in% colnames(appData$data))) {
                return('No temperature / voltage data found')
            }
            tvCols <- c('UTC', 'file', 'intBatt', 'extBatt', 'temp')
            tvCols <- tvCols[tvCols %in% names(appData$data)]
            tvData <- appData$data[tvCols]
            tvBrushData <- brushedPoints(tvData, brush=input$tvBrush, xvar='UTC', yvar='temp')
            if(nrow(tvBrushData) > 0) {
                tvBrushData <- tvBrushData[tvBrushData$UTC %in% range(tvBrushData$UTC), ]
            }
            tvBrushData
        })
        observeEvent(input$tvLogIssue, {
            if(is.null(appData$data)) {
                return()
            }
            if(!all(c('intBatt', 'temp') %in% colnames(appData$data))) {
                return()
            }
            tvCols <- c('UTC', 'file', 'intBatt', 'extBatt', 'temp')
            tvCols <- tvCols[tvCols %in% names(appData$data)]
            tvData <- appData$data[tvCols]
            tvBrushData <- brushedPoints(tvData, brush=input$tvBrush, xvar='UTC', yvar='temp')
            if(nrow(tvBrushData) == 0) {
                showNotification('No data selected')
                return()
            }
            tvBrushData <- tvBrushData[tvBrushData$UTC %in% range(tvBrushData$UTC), ]
            tvVals <- character(nrow(tvBrushData))
            if('intBatt' %in% names(tvBrushData)) {
                intVals <- paste0('IntBatt: ', tvBrushData[['intBatt']])
                tvVals <- paste(tvVals, intVals)
            }
            if('extBatt' %in% names(tvBrushData)) {
                extVals <- paste0('ExtBatt: ', tvBrushData[['extBatt']])
                tvVals <- paste(tvVals, extVals)
            }
            if('temp' %in% names(tvBrushData)) {
                tempVals <- paste0('Temp: ', tvBrushData[['temp']])
                tvVals <- paste(tvVals, tempVals)
            }
            tvBrushData <- tvBrushData[c('UTC', 'file')]
            tvBrushData$value <- tvVals
            tvBrushData$projectName <- appData$project
            tvBrushData$source <- 'TempVoltPlot'
            tvBrushData$comment <- rep(input$tvComment, nrow(tvBrushData))
            tvBrushData$issueChecked <- 'no'
            nowUTC <- Sys.time()
            attr(nowUTC, 'tzone') <- 'UTC'
            tvBrushData$qaqcDate <- nowUTC
            if(nrow(appData$issue) == 0) {
                appData$issue <- tvBrushData
            } else {
                appData$issue <- bind_rows(appData$issue, tvBrushData)
            }
            showNotification('Issue Logged')
            
        })
        # Issue Log ####
        output$issueData <- renderDT({
            arrange(appData$issue, appData$issue$qaqcDate)
        }, 
        server=FALSE,
        options=list(dom='rtip',
                     order=list(8, 'desc'))
        )
        observeEvent(input$removeIssue, {
            dropIx <- input$issueData_rows_selected
            if(is.null(dropIx)) {
                showNotification('No rows selected')
                return(NULL)
            }
            appData$issue <- appData$issue[-dropIx, ]
        })
        output$downloadIssues <- downloadHandler(
            filename = function() {
                'QAQC_Issues.csv'
            },
            content = function(file) {
                write.csv(appData$issue, file, row.names=FALSE)
            }
        )
        # Stop Page ####
        observeEvent(input$saveLog, {
            if(isFALSE(INDIR)) {
                showNotification('Saving only works when input is QAQC folder.')
                showNotification('Use Download button on "Home" page instead.')
                return()
            }
            # saveQLog(appData$log, dir=INPATH, update='new')
            # appData$saveLogTime <- Sys.time()
        })
        observeEvent(input$saveIssues, {
            if(isFALSE(INDIR)) {
                showNotification('Saving only works when input is QAQC folder.')
                showNotification('Use Download button on "Issues" page instead.')
                return()
            }
            # saveIssueLog(appData$issue, dir=INPATH)
            # appData$saveIssueTime <- Sys.time()
        })
        output$logSaveTime <- renderText({
            if(isFALSE(INLOG)) {
                out <- 'Feature not active when reviewing a single deployment'
            } else if(is.null(appData$saveLogTime)) {
                out <- 'Log has not been saved this session'
            } else {
                out <- paste0('Log last saved ', appData$saveLogTime)
            }
            out
        })
        output$issueSaveTime <- renderText({
            if(isFALSE(INLOG)) {
                out <- 'Use Download button on "Issues" page instead'
            } else if(is.null(appData$saveIssueTime)) {
                out <- 'Issues have not been saved this session'
            } else {
                out <- paste0('Issues last saved ', appData$saveIssueTime)
            }
            out
        })
    }
    runApp(shinyApp(ui=ui, server=server))
    invisible(data)
}

QAQC_STATUS <- c('NoData', 'NoQAQC', 'TimeChecked', 'ClipOnly', 'QAQCRun', 'QAQCReviewed', 'QAQCComplete')

makeHtmlColors <- function(values) {
    cols <- hue_pal()(length(values))
    text <- paste0('<div style="display:flex"><div style="color:', cols, ';font-weight:bold;">', values,'</div></div>')
    text
}

isQaqcLog <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    all(c('projectBaseDir',
          'projectDir',
          'projectName',
          'sensitivity',
          'calibration',
          'qaqcStatus',
          'qaqcBaseDir',
          'qaqcDir') %in%
            x)
}
