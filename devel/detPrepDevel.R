library(shiny)
library(PAMscapes)

runDetPrep <- function(data) {
    # UI Start ####
    ui <- navbarPage(
        id='Main',
        'DetLoader Helper',
        tabPanel(
            'Data Prep',
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
                                       textInput('tz', label='tz', value='UTC'),
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
                        )
            )
        )
    )
    server <- function(input, output, session) {
        df <- read.csv(data, nrows = 10, stringsAsFactors = FALSE)
        origNames <- reactiveVal(colnames(df))
        funArgs <- reactiveVal(list(x=data))
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
            if(is.null(newVal) || length(newVal) == 0 || newVal == '') {
                newArg$presenceDuration <- NULL
            } else {
                newArg$presenceDuration <- input$presenceDuration
            }
            funArgs(newArg)
        }, ignoreNULL=FALSE)
        colListen <- reactive({
            list(input$selectUTC, input$selectEnd, input$selectSpecies)
        })
        observeEvent(colListen(), {
            utc <- input$selectUTC
            end <- input$selectEnd
            species <- input$selectSpecies
            colMap <- list(UTC=NA, end=NA, species=NA)
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
            newArg$tz <- input$tz
            funArgs(newArg)
        })
        # Render columnMap page ####
        output$colMap <- renderUI({
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
            )
        })

        # Render Wide page ####
        output$wideBucket <- renderUI({
            if(!isTRUE(funArgs()$wide)) {
                return(renderText('"wide" is not set to "TRUE"'))
            }
            fluidRow(
                column(6,
                       selectizeInput('speciesCols',
                                      label='Species columns',
                                      choices=origNames(),
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
        output$codeSample <- renderPrint({
            argList <- funArgs()
            argText <- argToText(argList)
            argText <- paste0('loadDetectionData(', argText, ')')
            cat(argText)
        })
        output$dataHeader <- renderUI({
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
                return(renderPrint({
                    print('Not able to load data with these settings')
                    print(badMessage)
                }))
            }
            tryLoad$UTC <- format(tryLoad$UTC, format='%Y-%m-%d %H:%M:%S')
            tryLoad$end <- format(tryLoad$end, format='%Y-%m-%d %H:%M:%S')
            renderTable(head(tryLoad))
        })

    }
    runApp(shinyApp(ui=ui, server=server))
}

argToText <- function(x, type='c', max=5) {
    for(i in seq_along(x)) {
        val <- x[[i]]
        if(is.character(val)) {
            val <- paste0("'", val, "'")
        }
        if(!is.list(val) && length(val) > 1) {
            val <- paste0(val, collapse=', ')
            val <- paste0(type, '(', val, ')')
        }
        if(is.list(val)) {
            val <- argToText(val, max=1e3)
            val <- paste0('list(', val, ')')
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

wideFile <- '../Data/Acousdet/all_sp_dataframe.csv'
runDetPrep(wideFile)
