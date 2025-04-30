library(dplyr)
library(lubridate)
# oh dear theres metadata at the head of CSVs
readOldDetection <- function(x) {
    startVals <- c('start date', 'Start_Date', 'Start_date', 'StartDate', 'Date', 'UNIT_ID') # OR Start_Date
    startPat <- paste0(startVals, collapse='|')
    startPat <- paste0('^(', startPat, ')')
    # findHeader <- readr::read_csv(x, n_max=40, show_col_types = FALSE, col_names=FALSE)
    # headRow <- findHeader[[1]] %in% startVals
    findHeader <- readLines(x, n=1, warn=FALSE)
    headRow <- grepl(startPat, findHeader)
    if(headRow) {
        headIx <- 1
    } else {
        findHeader <- readLines(x, n=40, warn=FALSE)
        findHeader <- substr(findHeader, 1, 20)
        headRow <- grepl(startPat, findHeader)
    }
    if(!any(headRow)) {
        warning('Could not find "start date" column in file ', basename(x))
        return(NULL)
    }
    headIx <- which(headRow)
    if(length(headIx) > 1) {
        warning('More than one "start date" appears in file ', basename(x))
        return(NULL)
    }
    data <- readr::read_csv(x, skip=headIx-1, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
    data <- janitor::clean_names(data)
    data <- myRenamer(data, map=colRename)
    unnamedCols <- grepl('^x[0-9]{1,2}$', names(data))
    if(any(unnamedCols)) {
        vals <- lapply(data[unnamedCols], function(x) {
            x[!is.na(x)]
        })
        vals <- unlist(vals)
        warning('Unnamed columns in file ', basename(x),
                ' with ',length(vals), ' values ', PAMscapes:::printN(vals),
                call.=FALSE)
    }
    # ix 106, 272
    if('x14' %in% names(data)) {
        data$unknown_timestamp <- paste0(data$unknown_timestamp, data$x14, sep=';')
        data$x14 <- NULL
    }
    if(any(c('x15', 'x16', 'x17', 'x18') %in% names(data))) {
        data$notes <- paste0(data$notes, data$x17, data$x18, sep=';')
        data$x15 <- NULL
        data$x16 <- NULL
        data$x17 <- NULL
        data$x18 <- NULL
    }
    if(any(c('x20', 'x21') %in% names(data))) {
        data$x20 <- NULL
        data$x21 <- NULL
    }
    if(any(grepl('call_type', names(data)))) {
        data <- pivot_longer(data, cols=grep('call_type', names(data)), names_to='call_type', values_to='call_count')
    }
    date$detection_start_datetime <- paste(data$start_date, data$start_time)
    data$detection_end_datetime <- paste(data$end_date, data$end_time)
    data <- PAMpal:::dropCols(data, c('start_date', 'start_time', 'end_date', 'end_time'))
    data$file <- x
    data
}

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

# Notes for X## columns ----
# x14 is 106, 272. Appears to be "unknown_timestamp", v few entries and not obvious.
# x15-18 is only 1548. Appears to be notes
# x19 DNE
# x20 x21 is only 1060. Appears to be nothing - extra commas in column spec and no data there
# LMAO oh feck I don't care about the new baleen whale data
# baleen old is diffrant
# rename columns like so
# OLD = NEW
colRename <- list(
    'manial_review' = 'manual_review',
    'manaual_review' = 'manual_review',
    'manual_reivew' = 'manual_review',
    'manaul_review' = 'manual_review',
    'manual_species' = 'manual_review',
    'manual' = 'manual_review',
    'true_timestam' = 'true_timestamp',
    'true_time_stamp' = 'true_timestamp',
    'true_times' = 'true_timestamp',
    'true_tally_time' = 'true_timestamp',
    'true_tally_timestamp' = 'true_timestamp',
    'true_tally_timestamps' = 'true_timestamp',
    'true_timestamp' = 'true_timestamp',
    'true_timestamps' = 'true_timestamp',
    'true_tally_timestmap' = 'true_timestamp',
    'unknown_timestam_p' = 'unknown_timestamp',
    'unknown_time' = 'unknown_timestamp',
    'unknown_times' = 'unknown_timestamp',
    'unkown_times' = 'unknown_timestamp',
    'unknown_time_stamps' = 'unknown_timestamp',
    'unknown_timestamps' = 'unknown_timestamp',
    'suggeste_presence' = 'suggested_daily_presence',
    'suggested_presence' = 'suggested_daily_presence',
    'singlet_timestamp' = 'singles',
    'singles_timestamp' = 'singles',
    'single_timestamp' = 'singles'
)

# map analyst shorts like
anaRename <- list(
    'MM' = 'MMARTIN',
    'GD' = 'GDAVIS',
    'SW' = 'SWEISS',
    'NP' = 'NPEGG',
    'LT' = 'LTRANSUE',
    'RB' = 'RBRINER',
    'AS' = 'ASCOTT',
    'HK' = 'HKOILPILLAI',
    'ST' = 'STENNANT',
    'TC' = 'TCOLEMAN',
    'SZ' = 'SZOLL',
    'PH' = 'PHANSON'
)

myRenamer <- function(x, map) {
    if(is.data.frame(x)) {
        names(x) <- myRenamer(names(x), map)
        return(x)
    }
    for(val in names(map)) {
        if(val %in% x) {
            x[x == val] <- map[[val]]
        }
    }
    x
}
