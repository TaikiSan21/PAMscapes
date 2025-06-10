#' @title Run Detection Data Explorer App
#'
#' @description Runs a Shiny app that allows users to interactively
#'   find the proper parameters to load a detection dataset using
#'   \link{loadDetectionData} and explore plots for detection data
#'
#' @param data file path to a CSV file containing detection data or
#'   a dataframe
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' if(interactive()) {
#'   detFile <- system.file('extdata/detectionExample.csv', package='PAMscapes')
#'   runDetectionExplorer(detFile)
#' }
#'
#' @importFrom shiny uiOutput renderUI reactive reactiveVal selectizeInput
#' @importFrom shiny tabsetPanel renderTable observe
#' @importFrom utils head
#' @importFrom graphics text
#'
#' @export
#'
runDetectionExplorer <- function(data=NULL) {
    if(is.null(data)) {
        data <- file.choose()
    }
    if(is.data.frame(data)) {
        DFNAME <- deparse(sys.call()[[2]])
    } else {
        DFNAME <- NULL
    }
    tzVals <- c(
        'UTC',
        paste0('UTC+', 1:12),
        paste0('UTC-', 1:12)
    )
    # UI Start ####
    addResourcePath(prefix='scapex-images', system.file('images', package='PAMscapes'))
    on.exit(removeResourcePath('scapex-images'))
    buttonHeight <- '250px'
    ui <- navbarPage(
        id='main',
        'Detection Explorer',
        ## ui buttons ----
        tabPanel(
            'Home',
            fluidRow(column(1), column(10, 'Click on an image to navigate')),
            tags$button(id='btn_data_load',
                        class='btn action-button',
                        tags$img(src='scapex-images/data-loading.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_boxplot',
                        class='btn action-button',
                        tags$img(src='scapex-images/boxplot-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_scene',
                        class='btn action-button',
                        tags$img(src='scapex-images/scene-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_freq_scene',
                        class='btn action-button',
                        tags$img(src='scapex-images/freq-scene-ex.png',
                                 height=buttonHeight)
            ),
            tags$button(id='btn_polar',
                        class='btn action-button',
                        tags$img(src='scapex-images/polar-ex.png',
                                 height=buttonHeight)
            ),
        ),
        ## ui data ----
        tabPanel(
            'loadDetectionData',
            fluidRow(
                h4('Code snippet - copy me!'),
                verbatimTextOutput('codeSample')
            ),
            fluidRow(
                h4('Data Preview - If this shows a table, load was successful!'),
                uiOutput('dataHeader')
            ),
            tabsetPanel(type='pills',
                        tabPanel(
                            'Basic Options',
                            fluidRow(
                                column(6,
                                       selectInput('source', label='source', choices=c('makara', 'csv')),
                                       selectInput('detectionType', label='detectionType',
                                                   choices=c('auto', 'presence', 'detection')),
                                       textInput('presenceDuration', label='presenceDuration', value='')
                                ),
                                column(6,
                                       selectizeInput('tz', label='tz', choices=tzVals, options=list(create=TRUE)),
                                       selectInput('wide', label='wide',
                                                   choices=list('FALSE'=FALSE, 'TRUE'=TRUE)),
                                       selectInput('extraCols', label='extraCols', choices=character(0),
                                                   multiple=TRUE)
                                )
                            )
                        ),
                        tabPanel(
                            'columnMap',
                            uiOutput('colMap')
                        ),
                        tabPanel(
                            'If wide=TRUE',
                            uiOutput('wideBucket')
                        ),
                        tabPanel(
                            'Raw Data',
                            uiOutput('rawData')
                        )
            )
        ),
        ## ui plots ----
        navbarMenu(
            'Plot',
            tabPanel(
                'plotAcousticScene',
                h4(em('plotAcousticScene')),
                'Acoustic Presence / Acoustic Scene',
                plotOutput('plot_scene'),
                verbatimTextOutput('code_scene'),
                fluidRow(
                    column(2, selectInput(
                        'scene_by',
                        label='by',
                        choices=c('none'),
                        selected='none'
                    )),
                    column(2, selectInput(
                        'scene_combine',
                        label='combineYears',
                        choices=c(TRUE, FALSE),
                        selected=FALSE
                    )),
                    column(2, selectInput(
                        'scene_usefreq',
                        label='Use Frequency',
                        choices=c(TRUE, FALSE),
                        selected=FALSE
                    ))
                ),
                DTOutput('scene_freqmap')
            ),
            tabPanel(
                'plotPolarDetections',
                h4(em('plotPolarDetections')),
                'Polar Plot of Detections',
                plotOutput('plot_polar'),
                fluidRow(
                    column(2, selectInput(
                        'polar_bin1',
                        label='bin1',
                        choices=c('detection', 'hour', 'day'),
                        selected='detection'
                    )),
                    column(2, selectInput(
                        'polar_bin2',
                        label='bin2',
                        choices=c('hour', 'month'),
                        selected='hour'
                    )),
                    column(3, selectInput(
                        'polar_quantity',
                        label='quantity',
                        choices=c('count', 'mean', 'effort', 'percentEffort', 'percentTotal'),
                        selected='count'
                    )),
                    column(3, selectizeInput(
                        'polar_group',
                        label='group',
                        choices='species',
                        selected='species',
                        multiple=TRUE
                    )),
                    column(2, selectInput(
                        'polar_facet',
                        label='facet',
                        choices='none',
                        selected='none'
                    ))
                ),
                verbatimTextOutput('code_polar')
            ),
            tabPanel(
                'plotDetectionBoxplot',
                h4(em('plotDetectionBoxplot')),
                'Summarized Detection Boxplot',
                plotOutput('plot_boxplot'),
                fluidRow(
                    column(2, selectInput(
                        'box_bin1',
                        label='bin1',
                        choices=c('hour', 'day', 'week'),
                        selected='day'
                    )),
                    column(2, selectInput(
                        'box_bin2',
                        label='bin2',
                        choices=c('day', 'week', 'month'),
                        selected='week'
                    )),
                    column(3, selectizeInput(
                        'box_group',
                        label='group',
                        choices='species',
                        selected='species',
                        multiple=TRUE
                    )),
                    column(3, selectInput(
                        'box_facet',
                        label='facet',
                        choices='none',
                        selected='none'
                    )),
                    column(2, selectInput(
                        'box_combine',
                        label='combineYears',
                        choices=c(TRUE, FALSE),
                        selected=FALSE
                    ))
                ),
                'Copy/paste this code to recreate this plot:',
                verbatimTextOutput('code_boxplot')
            )
        )
    )
    server <- function(input, output, session) {
        if(is.character(data)) {
            # df <- read.csv(data, nrows = 10, stringsAsFactors = FALSE)
            df <- read.csv(data, stringsAsFactors = FALSE)
        } else if(is.data.frame(data)) {
            # df <- head(data, 10)
            df <- data
        }
        origNames <- reactiveVal(colnames(df))
        funArgs <- reactiveVal(list(x=data))
        plotData <- reactiveVal('Loading...')
        categoryCols <- reactiveVal(character(0))
        freqMap <- reactiveVal(data.frame())
        # update plot columns ####
        observeEvent(plotData(), {
            data <- plotData()
            if(is.character(data)) {
                categoryCols(character(0))
            } else {
                notCategory <- c('detectionType', 'detectedFlag')
                otherCols <- names(data)[!names(data) %in% notCategory]
                newCats <- sapply(data[otherCols], function(x) {
                    is.character(x) | is.factor(x) | is.logical(x)
                })
                categoryCols(otherCols[newCats])
                # do species vals and freq map
                spVals <- unique(data$species)
                oldMap <- freqMap()
                newMap <- data.frame(species=spVals, freqMin=NA, freqMax=NA)
                if(nrow(oldMap) == 0) {
                    freqMap(newMap)
                } else {
                    oldMap <- bind_rows(
                        oldMap,
                        newMap[!newMap$species %in% oldMap$species, ]
                    )
                    freqMap(oldMap)
                }
            }
        })
        observeEvent(categoryCols(), {
            cats <- categoryCols()
            if(length(cats) > 0) {
                updateSelectizeInput(
                    session,
                    'box_group',
                    choices=cats,
                    selected=input$box_group
                )
                updateSelectInput(
                    session,
                    'box_facet',
                    choices=c('none', cats),
                    selected=input$box_facet
                )
                updateSelectInput(
                    session,
                    'scene_by',
                    choices=c('none', cats[cats != 'species']),
                    selected=input$scene_by
                )
                updateSelectInput(
                    session,
                    'polar_facet',
                    choices=c('none', cats),
                    selected=input$polar_facet
                )
                updateSelectizeInput(
                    session,
                    'polar_group',
                    choices=cats,
                    selected=input$polar_group
                )
            } else {
                updateSelectInput(
                    session,
                    'box_group',
                    choices='species',
                    selected='species'
                )
                updateSelectInput(
                    session,
                    'box_facet',
                    choices='none',
                    selected='none'
                )
                updateSelectInput(
                    session,
                    'scene_by',
                    choices='none',
                    selected='none'
                )
                updateSelectInput(
                    session,
                    'polar_facet',
                    choices='none',
                    selected='none'
                )
                updateSelectizeInput(
                    session,
                    'polar_group',
                    choices='species',
                    selected='species'
                )
            }
        })
        updateSelectInput(inputId='extraCols', choices=names(df))
        # Update args ####
        observeEvent(input$source, {
            newArg <- funArgs()
            newArg$source <- input$source
            funArgs(newArg)
        })
        observeEvent(input$detectionType, {
            newArg <- funArgs()
            newArg$detectionType <- input$detectionType
            funArgs(newArg)
        })
        observeEvent(input$wide, {
            newArg <- funArgs()
            newArg$wide <- input$wide == 'TRUE'
            funArgs(newArg)
        })
        observeEvent(input$wideDetVals, {
            newArg <- funArgs()
            newArg$detectedValues <- input$wideDetVals
            funArgs(newArg)
        }, ignoreNULL=FALSE)
        observeEvent(input$speciesCols, {
            newArg <- funArgs()
            newArg$speciesCols <- input$speciesCols
            funArgs(newArg)
        }, ignoreNULL=FALSE)
        observeEvent(input$extraCols, {
            newArg <- funArgs()
            newArg$extraCols <- input$extraCols
            funArgs(newArg)
        }, ignoreNULL=FALSE)
        observeEvent(input$presenceDuration, {
            newArg <- funArgs()
            newVal <- input$presenceDuration
            if(!is.na(suppressWarnings(as.numeric(newVal)))) {
                newVal <- as.numeric(newVal)
            }
            if(is.null(newVal) || length(newVal) == 0 || newVal == '') {
                newArg$presenceDuration <- NULL
            } else {
                newArg$presenceDuration <- newVal
            }
            funArgs(newArg)
        }, ignoreNULL=FALSE)
        colListen <- reactive({
            list(input$selectUTC, input$selectEnd, input$selectSpecies,
                 input$selectEffortStart, input$selectEffortEnd,
                 input$selectLat, input$selectLong)
        })
        observeEvent(colListen(), {
            utc <- input$selectUTC
            end <- input$selectEnd
            species <- input$selectSpecies
            effStart <- input$selectEffortStart
            effEnd <- input$selectEffortEnd
            lat <- input$selectLat
            long <- input$selectLong
            colMap <- list(UTC=NA, end=NA, species=NA, effortStart=NA, effortEnd=NA,
                           Latitude=NA, Longitude=NA)
            if(is.null(utc) || length(utc) == 0 || utc == '') {
                colMap$UTC <- NULL
            } else {
                colMap$UTC <- utc
            }
            if(is.null(end) || length(end) == 0 || end == '') {
                colMap$end <- NULL
            } else {
                colMap$end <- end
            }
            if(is.null(species) || length(species) == 0 || species == '') {
                colMap$species <- NULL
            } else {
                colMap$species <- species
            }
            if(is.null(effStart) || length(effStart) == 0 || effStart == '') {
                colMap$effortStart <- NULL
            } else {
                colMap$effortStart <- effStart
            }
            if(is.null(effEnd) || length(effEnd) == 0 || effEnd == '') {
                colMap$effortEnd <- NULL
            } else {
                colMap$effortEnd <- effEnd
            }
            if(is.null(lat) || length(lat) == 0 || lat == '') {
                colMap$Latitude <- NULL
            } else {
                colMap$Latitude <- lat
            }
            if(is.null(long) || length(long) == 0 || long == '') {
                colMap$Longitude <- NULL
            } else {
                colMap$Longitude <- long
            }
            
            newArg <- funArgs()
            if(length(colMap) == 0) {
                newArg$columnMap <- NULL
            } else {
                newArg$columnMap <- colMap
            }
            funArgs(newArg)
        })
        observeEvent(input$tz, {
            newArg <- funArgs()
            newArg$tz <- utcToOlson(input$tz)
            funArgs(newArg)
        })
        # columnMap page ####
        output$colMap <- renderUI({
            fluidPage(
                fluidRow(
                    column(4,
                           selectizeInput('selectUTC', '"UTC" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    ),
                    column(4,
                           selectizeInput('selectEnd', '"end" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    ),
                    column(4,
                           selectizeInput('selectSpecies', '"species" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    )
                ),
                fluidRow(
                    column(4,
                           selectizeInput('selectEffortStart', '"effortStart" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    ),
                    column(4,
                           selectizeInput('selectEffortEnd', '"effortEnd" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    )
                ),
                fluidRow(
                    column(4, 
                           selectizeInput('selectLat', '"Latitude" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    ),
                    column(4,
                           selectizeInput('selectLong', '"Longitude" column', choices=origNames(),
                                          multiple=TRUE, options = list(maxItems=1))
                    )
                )
            )
        })
        
        # Wide page ####
        output$wideBucket <- renderUI({
            fluidRow(
                column(6,
                       selectizeInput('speciesCols',
                                      label='Species columns',
                                      choices=origNames(),
                                      selected=NULL,
                                      multiple=TRUE)
                ),
                column(6,
                       selectInput('wideDetVals',
                                   label='Positive Detection',
                                   choices=character(0),
                                   multiple=TRUE)
                )
            )
        })
        observeEvent(input$speciesCols, {
            oldSel <- input$wideDetVals
            possVals <- unique(unlist(df[input$speciesCols]))
            possVals <- possVals[!is.na(possVals)]
            updateSelectInput(inputId = 'wideDetVals',
                              choices=possVals,
                              selected=oldSel[oldSel %in% possVals])
        })
        # data page ----
        output$codeSample <- renderPrint({
            argList <- funArgs()
            argText <- argToText(argList, dfName=DFNAME)
            if(grepl('columnMap', argText)) {
                argText <- gsub('columnMap', '\ncolumnMap', argText)
            }
            if(grepl('speciesCols', argText)) {
                argText <- gsub('speciesCols', '\nspeciesCols', argText)
            }
            argText <- paste0('loadDetectionData(', argText, ')')
            cat(argText)
        })
        observe({
            tryLoad <- tryCatch({
                do.call(loadDetectionData, funArgs())
            },
            warning = function(w) {
                w
            },
            error = function(e) {
                e
            })
            noLoad <- FALSE
            if(inherits(tryLoad, 'error')) {
                noLoad <- TRUE
                badReason <- 'Error: '
            }
            if(inherits(tryLoad, 'warning')) {
                noLoad <- TRUE
                badReason <- 'Warning: '
            }
            if(isTRUE(noLoad)) {
                badMessage <- paste0(badReason, tryLoad$message)
                plotData(badMessage)
            } else {
                plotData(tryLoad)
            }
        })
        output$dataHeader <- renderUI({
            tryLoad <- plotData()
            if(is.character(tryLoad)) {
                return(renderPrint({
                    print('Not able to load data with these settings')
                    print(tryLoad)
                }))
            }
            tryLoad$UTC <- format(tryLoad$UTC, format='%Y-%m-%d %H:%M:%S')
            tryLoad$end <- format(tryLoad$end, format='%Y-%m-%d %H:%M:%S')
            if('effortStart' %in% names(tryLoad)) {
                tryLoad$effortStart <- format(tryLoad$effortStart, format='%Y-%m-%d %H:%M:%S')
            }
            if('effortEnd' %in% names(tryLoad)) {
                tryLoad$effortEnd <- format(tryLoad$effortEnd, format='%Y-%m-%d %H:%M:%S')
            }
            renderTable(head(tryLoad))
        })
        output$rawData <- renderUI({
            renderTable(head(df, 10))
        })
        # buttons ----
        observeEvent(input$btn_data_load, {
            updateNavbarPage(session, 'main', 'loadDetectionData')
        })
        observeEvent(input$btn_boxplot, {
            updateNavbarPage(session, 'main', 'plotDetectionBoxplot')
        })
        observeEvent(input$btn_scene, {
            updateSelectInput(session, 'scene_usefreq', selected='FALSE')
            updateNavbarPage(session, 'main', 'plotAcousticScene')
        })
        observeEvent(input$btn_freq_scene, {
            updateSelectInput(session, 'scene_usefreq', selected='TRUE')
            updateNavbarPage(session, 'main', 'plotAcousticScene')
        })
        observeEvent(input$btn_polar, {
            updateNavbarPage(session, 'main', 'plotPolarDetections')
        })
        # plots ----
        output$plot_scene <- renderPlot({
            data <- plotData()
            if(is.character(data)) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='Data not properly loaded\nUse loadDetectionData panel')
                return()
            }
            if(input$scene_by == 'none') {
                by <- NULL
            } else {
                by <- input$scene_by
            }
            if(input$scene_usefreq == 'FALSE') {
                map <- NULL
            } else {
                spVals <- unique(data$species)
                map <- freqMap()
                spIn <- map$species %in% spVals
                if(anyNA(map$freqMin[spIn]) ||
                   anyNA(map$freqMax[spIn])) {
                    map <- NULL
                }
            }
            combine <- input$scene_combine == 'TRUE'
            plotAcousticScene(data, by = by, freqMap=map, combineYears=combine)
        })
        output$code_scene <- renderPrint({
            argList <- list(x=data)
            if(input$scene_by != 'none') {
                argList$by <- input$scene_by
            }
            argList$combineYears <- input$scene_combine == 'TRUE'
            if(input$scene_usefreq == 'TRUE') {
                argList$freqMap <- as.list(freqMap())
            }
            argText <- argToText(argList, dfName=DFNAME, listName='data.frame')
            argText <- paste0('plotAcousticScene(', argText, ')')
            cat(argText)
        })
        output$scene_freqmap <- renderDT({
            if(input$scene_usefreq == 'FALSE' || is.character(plotData())) {
                return(data.frame())
            }
            spVals <- unique(plotData()$species)
            map <- freqMap()
            map[map$species %in% spVals, ]
            freqMap()
        },
        server=FALSE,
        options=list(dom='t'),
        editable=TRUE,
        selection=list(target='cell')
        )
        observeEvent(input$scene_freqmap_cell_edit, {
            info <- input$scene_freqmap_cell_edit
            oldMap <- freqMap()
            val <- suppressWarnings(as.numeric(info$value))
            if(info$col != 1 && !is.na(val)) {
                oldMap[info$row, info$col] <- val
                freqMap(oldMap)
            }
        })
        output$plot_boxplot <- renderPlot({
            data <- plotData()
            if(is.character(data)) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='Data not properly loaded\nUse loadDetectionData panel')
                return()
            }
            if(input$box_facet == 'none') {
                facet <- NULL
            } else {
                facet <- input$box_facet
            }
            group <- input$box_group
            combine <- input$box_combine == 'TRUE'
            bin <- paste0(input$box_bin1, '/', input$box_bin2)
            plotDetectionBoxplot(data, group=group, bin=bin, facet=facet, combineYears=combine)
        })
        output$code_boxplot <- renderPrint({
            argList <- list(x=data)
            argList$group <- input$box_group
            if(input$box_facet == 'none') {
                facet <- NULL
            } else {
                argList$facet <- input$box_facet
            }
            argList$bin <- paste0(input$box_bin1, '/', input$box_bin2)
            argList$combineYears <- input$box_combine == 'TRUE'
            
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotDetectionBoxplot(', argText, ')')
            cat(argText)
        })
        output$plot_polar <- renderPlot({
            data <- plotData()
            if(is.character(data)) {
                plot(x=1, y=1, type='n')
                text(x=1, y=1, label='Data not properly loaded\nUse loadDetectionData panel')
                return()
            }
            if(input$polar_facet == 'none') {
                facet <- NULL
            } else {
                facet <- input$polar_facet
            }
            bin <- paste0(input$polar_bin1, '/', input$polar_bin2)
            plotPolarDetections(data, bin=bin, facet=facet, group=input$polar_group,
                                quantity=input$polar_quantity)
        })
        output$code_polar <- renderPrint({
            argList <- list(x=data)
            argList$group <- input$polar_group
            if(input$polar_facet == 'none') {
                facet <- NULL
            } else {
                argList$facet <- input$polar_facet
            }
            argList$bin <- paste0(input$polar_bin1, '/', input$polar_bin2)
            argList$quantity <- input$polar_quantity
            argText <- argToText(argList, dfName=DFNAME)
            argText <- paste0('plotPolarDetections(', argText, ')')
            cat(argText)
        })
    }
    runApp(shinyApp(ui=ui, server=server))
}

utcToOlson <- function(tz) {
    if(grepl('UTC\\-', tz)) {
        tz <- gsub('UTC\\-', 'Etc/GMT+', tz)
    }
    if(grepl('UTC\\+', tz)) {
        tz <- gsub('UTC\\+', 'Etc/GMT-', tz)
    }
    tz
}

argToText <- function(x, type='c', max=5, dfName='x', listName='list') {
    for(i in seq_along(x)) {
        val <- x[[i]]
        if(is.character(val)) {
            val <- paste0("'", val, "'")
        }
        if(is.data.frame(val)) {
            val <- dfName
        }
        if(!is.list(val) && length(val) > 1) {
            val <- paste0(val, collapse=', ')
            val <- paste0(type, '(', val, ')')
        }
        if(is.list(val)) {
            val <- argToText(val, max=1e3)
            val <- paste0(listName, '(', val, ')')
        }
        x[[i]] <- val
    }
    x <- paste0(names(x), '=', unlist(x))
    nArgs <- length(x)
    if(nArgs > max) {
        atMax <- seq(from=max, to=nArgs-1, by=max)
        for(ix  in atMax) {
            x[ix] <- paste0('\n', x[ix])
        }
    }
    x <- paste0(x, collapse=', ')
    x
}
