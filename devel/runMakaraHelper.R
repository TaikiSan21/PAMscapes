library(shiny)
library(sortable)
runMakaraHelper <- function(data) {
    ui <- navbarPage(
        id='Main',
        'Maino',
        tabPanel(
            'Detections',
            uiOutput('detMain')
        ),
        tabPanel(
            'Analyses',
        ),

    )
    server <- function(input, output, session) {
        output$detMain <- renderUI({
            detRanks <- lapply(names(finalDetCols), function(x) {
                add_rank_list(text=x, labels=NULL, input_id=paste0('det_', x))
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
        observeEvent(input$detBucket, {
            print(unlist(input$detBucket))
        })
    }

    runApp(shinyApp(ui=ui, server=server))
}

runMakaraHelper(allOld[[1]])
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