library(shiny)
library(sortable)

instructions <- '../Data/MakBal/Makara - Template Instructions and Definitions.xlsx'
fields <- readxl::read_excel(instructions, sheet='Data Table Fields')

runMakaraHelper <- function(data) {
    ui <- navbarPage(
        id='Main',
        'Maino',
        tabPanel(
            'Detections',
            tableOutput('detTable'),
            uiOutput('detMain')
        ),
        tabPanel(
            'Analyses',
            uiOutput('analysesMain')
        ),
        
    )
    server <- function(input, output, session) {
        output$detTable <- renderTable({
            plotDat <- head(data)
            if('notes' %in% names(plotDat)) {
                charMax <- 20
                charHas <- nchar(plotDat$notes)
                charHas[is.na(charHas)] <- 0
                plotDat$notes <- substr(plotDat$notes, 1, charMax)
                plotDat$notes[charHas > charMax] <- paste0(plotDat$notes[charHas > charMax], '...')
            }
            plotDat
        },
        striped=TRUE)
        detFields <- filter(fields, Table == 'detections')
        output$detMain <- renderUI({
            detRanks <- lapply(detFields$Field, function(x) {
                this <- detFields[detFields$Field == x, ]
                if(!is.na(this$Required) && this$Required == 'Always') {
                    # if(x %in% required) {
                    lab <- HTML(paste0('<b>', x, '</b>'))
                } else {
                    lab <- x
                }
                add_rank_list(text=lab, labels=NULL, input_id=paste0('det_', x))
            })
            fluidRow(
                tags$style(HTML('
            .bucket-list-container {min-height: 150px;}
            p.rank-list-title {margin-bottom: 3px;}
            .default-sortable.rank-list-container {padding: 2px; flex: 1 0 0;}'
                )),
                column(4,
                       bucket_list(header='Inputs',
                                   group_name = 'detBucket',
                                   add_rank_list(text='cols', labels=names(data))
                       )
                ),
                column(8,
                       do.call('bucket_list', c(header='DetBucket', group_name='detBucket',detRanks))
                )
            )
        })
        anaFields <- filter(fields, Table == 'analyses')
        output$analysesMain <- renderUI({
            anaRanks <- lapply(anaFields$Field, function(x) {
                this <- anaFields[anaFields$Field == x, ]
                if(!is.na(this$Required) && this$Required == 'Always') {
                    lab <- HTML(paste0('<b>', x, '</b>'))
                } else {
                    lab <- x
                }
                add_rank_list(text=lab, labels=NULL, input_id=paste0('ana_', x))
            })
            fluidRow(
                tags$style(HTML('
            .bucket-list-container {min-height: 150px;}
            p.rank-list-title {margin-bottom: 3px;}
            .default-sortable.rank-list-container {padding: 2px; flex: 1 0 0;}
            .default-sortable .rank-list {display: flex; flex-wrap: wrap;}'
                )),
                column(4,
                       bucket_list(header='Inputs',
                                   group_name = 'anaBucket',
                                   add_rank_list(text='cols', labels=names(data))
                       )
                ),
                column(8,
                       do.call('bucket_list', c(header='AnaBucket', group_name='anaBucket',anaRanks))
                )
            )
            
        })
        observeEvent(input$detBucket, {
            # print(unlist(input$detBucket))
        })
    }
    
    runApp(shinyApp(ui=ui, server=server))
}

# runMakaraHelper(combOld[[1]])
required <- c('organization_code',
              'deployment_code',
              'analysis_code',
              'detection_start_datetime',
              'detection_end_datetime',
              'detection_effort_secs',
              'detection_sound_source_code',
              'detection_call_type_code',
              'detection_result_code')

finalDetCols <- list(
    'organization_code' = 'external', #*
    'deployment_code' = 'external', #*
    'analysis_code' = 'external', #*
    'detection_start_datetime' = 'pasters', #*
    'detection_end_datetime' = 'pasters', #*
    'detection_effort_secs' = 'IDK', #* how often is this different from range(time)?
    'detection_sound_source_code' = 'species', #*
    'detection_call_type_code' = 'calltype', #*
    'detection_behavior_code' = 'probably not',
    'detection_demographic_code' = 'probably not',
    'detection_n_validated' = 'probably',
    'detection_n_total' = 'moreprobably',
    'detection_result_code' = 'yes/no', #*,
    'detection_event_type' = 'pamguard?eventType',
    'detection_event_id' = 'pamguard!eventId',
    'detection_latitude' = 'probably',
    'detection_longitude' = 'probably',
    'detection_received_level_db' = 'probably not',
    'detection_quality_code' = 'idk',
    'detection_n_animals' = 'probably not',
    'detection_n_animals_min' = 'probably not',
    'detection_n_animals_max' = 'probably not',
    'detection_json' = 'other garbo', # detector settings for LFDCS
    'detection_comments' = 'some more garbo', # generic paste-shit
    'lots of localization shit' = 'pamgaurd??'
)