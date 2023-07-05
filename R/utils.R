# util functions

#' @importFrom readr read_csv
#'
checkSoundscapeInput <- function(x, needCols=c('UTC')) {
    if(is.character(x)) {
        if(!file.exists(x)) {
            stop('File ', x, ' does not exist.')
        }
        x <- read_csv(x, show_col_types=FALSE)
    }
    tritonTime <- "yyyy-mm-ddTHH:MM:SSZ"
    if(tritonTime %in% colnames(x)) {
        colnames(x)[colnames(x) == tritonTime] <- 'UTC'
    }
    missingCols <- needCols[!needCols %in% colnames(x)]
    if(length(missingCols) > 0) {
        stop('Required columns ', paste0(missingCols, collapse=', '),
             ' are missing.')
    }
    if(is.character(x$UTC)) {
        x$UTC <- parseToUTC(x$UTC)
    }
    x
}

#' @importFrom hoardr hoard
#'
fileNameManager <- function(fileName=NULL, suffix=NULL) {
    if(is.null(fileName)) {
        tempDir <- hoard()$cache_path_set('PAMmisc')
        if(!dir.exists(tempDir)) {
            dir.create(tempDir, recursive = TRUE)
        }
        fileName <- paste0(tempDir, '/TEMPFILE.nc')
    }
    if(!is.null(suffix)) {
        fileName <- gsub('(.*)(\\.nc$)', paste0('\\1_', suffix, '\\2'), fileName)
    }
    if(!dir.exists(dirname(fileName))) {
        dir.create(dirname(fileName), recursive = TRUE)
    }
    fileName
}

to180 <- function(x, inverse = FALSE) {
    if(inverse) {
        return(to360(x, inverse = FALSE))
    }
    if(is.data.frame(x) ||
       is.list(x)) {
        tmp <- x$Longitude %% 360
        tmp <- ifelse(tmp > 180, tmp - 360, tmp)
        x$Longitude <- tmp
        return(x)
    }
    tmp <- x %% 360
    tmp <- ifelse(tmp > 180, tmp - 360, tmp)
    tmp
}

to360 <- function(x, inverse = FALSE) {
    if(inverse) {
        return(to180(x, inverse = FALSE))
    }
    if(is.data.frame(x) ||
       is.list(x)) {
        x$Longitude <- x$Longitude %% 360
        return(x)
    }
    x %% 360
}

#' @importFrom lubridate parse_date_time with_tz
#'
parseToUTC <- function(x,
                       format = c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                  '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'),
                       tz='UTC') {
    tryCatch({
        testTz <- parse_date_time('10-10-2020 12:00:05', orders = '%m/%d/%Y %H:%M:%S', tz=tz)
    },
    error = function(e) {
        msg <- e$message
        if(grepl('CCTZ: Unrecognized output timezone', msg)) {
            stop('Timezone not recognized, see function OlsonNames() for accepted options', call.=FALSE)
        }
    })
    if(!inherits(x, 'POSIXct')) {
        origTz <- parse_date_time(x, orders=format, tz=tz, exact=TRUE, truncated=3)
        if(!inherits(origTz, 'POSIXct')) {
            stop('Unable to convert to POSIXct time.', call.=FALSE)
        }
    } else {
        origTz <- x
    }
    with_tz(origTz, tzone='UTC')
}

#' @importFrom tidyr pivot_longer
#'
toLong <- function(x) {
    longCols <- c('UTC', 'frequency', 'value')
    # already long'd
    if(all(longCols %in% colnames(x))) {
        return(x)
    }
    type <- unique(gsub('_[0-9-]+', '', colnames(x)[2:ncol(x)]))
    if(length(type) != 1) {
        stop('"x" must be in long format with columns "UTC", "frequency", and "value" OR',
             ' columns 2:n must named in format TYPE_FREQUENCY.')
    }
    if(type != 'BB') {
        x <- pivot_longer(x, cols=2:ncol(x), names_to='type', values_to='value')
        x$frequency <- as.numeric(gsub('[A-z]+_', '', x$type))
        x$type <- gsub('_[0-9-]+', '', x$type)
    }
    if(type == 'BB') {
        freqs <- as.numeric(strsplit(gsub('BB_', '', colnames(x)[2]), '-')[[1]])
        x$type <- 'BB'
        colnames(x)[grepl('BB_', colnames(x))] <- 'value'
        x$freqMin <- freqs[1]
        x$freqMax <- freqs[2]
    }
    x
}

#' @importFrom tidyr pivot_wider
#'
toWide <- function(x) {
    needCols <- c('UTC', 'frequency', 'value', 'type')
    if(!all(needCols %in% colnames(x))) {
        return(x)
    }
    x <- pivot_wider(x, names_from=c('type', 'frequency'), names_sep='_', values_from='value')
    x
}

nowUTC <- function() {
    now <- Sys.time()
    attr(now, 'tzone') <- 'UTC'
    now
}
