# makara notes

# analyses table has INTERVAL, EVENT_ENCOUNTER, and EVENT_CALL granularity codes
# that tell us presence, event, single call types of detections
# then thats how you interpret the duration of detections in det table

# recording_intervals tables seems to be only problems? one I read is marked
# with unusable with the only entry

# recordings table has usable start/end for effort
det <- read.csv('../Data/Acousdet/makara/NEFSC_GOM_202202_LUBEC_HAPO_ANALYSIS.csv', stringsAsFactors = F)
rec <- read.csv('../Data/Acousdet/makara/Makara - Phase 2 Review - Organization-Specific Data - 20241121 - recordings.csv', stringsAsFactors = F)
ana <- read.csv('../Data/Acousdet/makara/Makara - Phase 2 Review - Organization-Specific Data - 20241121 - analyses.csv', stringsAsFactors = F)
dep <- read.csv('../Data/Acousdet/makara/Makara - Phase 2 Review - Organization-Specific Data - 20241121 - deployments.csv', stringsAsFactors = F)

issues <- read.csv('../Data/Acousdet/makara/NEFSC_GOM_202202_LUBEC_RECORDING.csv', stringsAsFactors = FALSE)

kog <- read.csv('../Data/Acousdet/makara/NEFSC_NE-OFFSHORE_HB1603_LEG4_KOGIA_ANALYSIS.csv', stringsAsFactors = F)


makAnaCols <- c('deployment_code', #this_below.csv is csv name, for join
                'analysis_code', # above_this.csv is detection csv file name
                'analysis_sound_source_codes', #species?
                'analysis_granularity_code', #interval
                'analysis_timezone') # might need?

anaToEffort <- list('deployment_code' = 'project',
                    'analysis_sound_source_codes' = 'species',
                    'analysis_start_datetime' = 'start',
                    'analysis_end_datetime' = 'end')
effort <- ana[names(anaToEffort)]
names(effort) <- unlist(anaToEffort)
effort$start <- as.POSIXct(effort$start, '%Y-%m-%d %H:%M:%S', tz='UTC')
effort$end <- as.POSIXct(effort$end, '%Y-%m-%d %H:%M:%S', tz='UTC')

makRecCols <- c('deployment_code', # for join
                'recording_duration_secs', # using this and below for duty cycle?
                'recording_interval_secs',
                'recording_usable_start_datetime', # on effort stuff
                'recording_usable_end_datetime')

# pull [deployment_code]_[analysis_code]
makDetCols <- c('detection_start_datetime', # infer presence from duration
                'detection_end_datetime',
                'detection_effort_secs', # ?
                'detection_call_type_code', #calltype
                'detection_sound_source_code', #species
                'detection_type_code', # detected/not_detected
                'detection_latitude', # if exists
                'detection_longitude')

detFileToCode <- function(x, analysis=FALSE) {
    x <- basename(x)
    x <- gsub('\\.csv$', '', x)
    pattern <- '(.*)_([A-z]*_ANALYSIS$)'
    if(isTRUE(analysis)) {
        code <- gsub(pattern, '\\2', x)
    } else {
        code <- gsub(pattern, '\\1', x)
    }
    if(code == x) {
        warning('Could not properly parse file name ', x)
        return(NA)
    }
    code
}

detFiles <- list.files('../Data/Acousdet/makara/', full.names=TRUE)
detFiles <- detFiles[grepl('ANALYSIS', detFiles)]
codes <- sapply(detFiles, function(x) detFileToCode(x)[[1]])
codes %in% rec$deployment_code
detFileToCode('../Data/Acousdet/makara/NEFSC_GOM_202202_LUBEC_HAPO_ANALYSIS.csv')
library(dplyr)

        
formatMakaraDets <- function(detection, analysis=NULL) {
    makDetMap <- tribble(
        ~old, ~new,
        'detection_start_datetime', 'UTC',
        'detection_end_datetime', 'end',
        'detection_sound_source_code', 'species',
        'detection_call_type_code', 'call',
        'detection_type_code', 'detectedFlag',
        'detection_latitude', 'Latitude',
        'detection_longitude', 'Longitude'
    )
    makDetCols <- c('detection_start_datetime', # infer presence from duration
                    'detection_end_datetime',
                    'detection_effort_secs', # ?
                    'detection_sound_source_code', # species
                    'detection_call_type_code', #call type
                    'detection_type_code', # detected/not_detected
                    'detection_latitude', # if exists
                    'detection_longitude')
    if(is.character(detection)) {
        result <- bind_rows(
            lapply(detection, function(x) {
                if(!file.exists(x)) {
                    warning('Detection file ', x, ' does not exist')
                    return(NULL)
                }
                oneDet <- read.csv(x, stringsAsFactors = FALSE)
                oneDet <- oneDet[makDetCols]
                # names(oneDet) <- c('UTC', 'end', 'effort', 'species', 'call', 'detectedFlag', 'Latitude', 'Longitude')
                depCode <- detFileToCode(x, analysis = FALSE)
                anaPart <- detFileToCode(x, analysis = TRUE)
                oneDet$project <- depCode 
                oneDet$ANAJOIN <- paste0(depCode, '_', anaPart)
                oneDet
            })
        )
        result <- distinct(result)
    } else {
        result <- detection
    }
    #### OTHER RENAMER? 
    names(result) <- renameToMap(names(result), makDetMap)
    result$detection_effort_secs <- NULL # not using but maybe?
    ####
    result$UTC <- as.POSIXct(result$UTC, '%Y-%m-%dT%H:%M:%S+0000', tz='UTC')
    result$end <- as.POSIXct(result$end, '%Y-%m-%dT%H:%M:%S+0000', tz='UTC')
    result <- result[result$detectedFlag %in% 'DETECTED', ]
    # result$effort <- NULL
    result$detectedFlag <- NULL
    if(is.null(analysis)) {
        result$ANAJOIN <- NULL
        result$detectionType <- inferDetType(result$UTC, result$end)
        return(result)
    }
    
    if(is.character(analysis)) {
        if(!file.exists(analysis)) {
            warning('Analysis file ', analysis, ' does not exist')
            return(NULL)
        }
        analysis <- read.csv(analysis, stringsAsFactors = FALSE)
    }
    
    makAnaCols <- c('deployment_code', #this_below.csv is csv name, for join
                    'analysis_code', # above_this.csv is detection csv file name
                    'analysis_sound_source_codes', #species?
                    'analysis_granularity_code', #interval
                    'analysis_timezone') # might need?
    if(!all(makAnaCols %in% colnames(analysis))) {
        warning('Analysis data did not have all expected columns')
        return(NULL)
    }
    analysis <- analysis[makAnaCols]
    granMatch <- data.frame(analysis_granularity_code = c('EVENT_ENCOUNTER', 'EVENT_DETECTION', 'INTERVAL'),
                            detectionType = c('detection', 'detection', 'presence'))
    analysis <- left_join(analysis, granMatch, by='analysis_granularity_code')
    analysis$analysis_granularity_code <- NULL
    analysis$ANAJOIN <- paste0(analysis$deployment_code, '_', analysis$analysis_code)
    result <- left_join(result, analysis[c('ANAJOIN', 'detectionType')], by='ANAJOIN')
    naJoin <- is.na(result$detectionType)
    result$ANAJOIN <- NULL
    if(any(naJoin)) {
        warning('Could not match deployment code ', 
                unique(result$project[naJoin]),
                ' in analysis table')
    }
    result
}

detFiles <- list.files('../Data/Acousdet/SEFSC/', full.names=T)
hm <- formatMakaraDets(detFiles)
str(hm)

