# unnest expands the list-df column into a dataframe and copies others columns
# to make on big DF (like how I probably would have kept it reading in)

# nest_by re-created that, with colname = .key

# seems like we always dropping call_type in favor of manual call_type_code

library(dplyr)

baseDataLoader <- function(x) {
    if(length(x) > 1) {
        x <- lapply(x, baseDataLoader)
        return(x)
    }
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
        if(length(vals) == 0) {
            data <- data[!unnamedCols]
        } else {
            warning('Unnamed columns in file ', basename(x),
                    ' with ',length(vals), ' values ', PAMscapes:::printN(vals),
                    call.=FALSE)
        }
    }
    if(any(grepl('call_type', names(data)))) {
        data <- tidyr::pivot_longer(data, cols=grep('call_type', names(data)), names_to='call_type', values_to='call_count')
    }
    if(all(c('start_date', 'start_time') %in% names(data))) {
        data$detection_start_datetime <- paste(data$start_date, data$start_time)
    }
    if(all(c('end_date', 'end_time') %in% names(data))) {  
        data$detection_end_datetime <- paste(data$end_date, data$end_time)
    }
    data$file <- file.path(basename(dirname(x)), basename(x))
    data
}
# x is either list of already read data or file paths
sameNameCombiner <- function(x) {
    if(is.character(x)) {
        x <- lapply(x, baseDataLoader)
    }
    allNames <- lapply(x, colnames)
    ixVec <- rep(0, length(allNames))
    distNames <- list()
    for(i in seq_along(allNames)) {
        if(i==1) {
            distNames <- list(allNames[[i]])
        }
        isIn <- unlist(lapply(distNames, function(y) identical(allNames[[i]], y)))
        if(!any(isIn)) {
            distNames[[length(distNames)+1]] <- allNames[[i]]
            ixVec[i] <- length(distNames)
        } else {
            ixVec[i] <- which(isIn)
        }
    }
    distNames
    combOld <- vector('list', length=max(ixVec))
    for(i in seq_along(combOld)) {
        combOld[[i]] <- bind_rows(x[ixVec == i])
    }
    combOld
}

colRename <- list(
    'manial_review' = 'manual_review',
    'manaual_review' = 'manual_review',
    'manual_reivew' = 'manual_review',
    'manaul_review' = 'manual_review',
    'manual_species' = 'manual_review',
    'maual_review' = 'manual_review',
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
    'single_timestamp' = 'singles',
    'singlest_timestamp' = 'singles'
)

