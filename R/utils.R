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
    x <- checkTriton(x)
    x <- checkManta(x)
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

checkTriton <- function(x) {
    tritonTime <- "yyyy-mm-ddTHH:MM:SSZ"
    if(tritonTime %in% colnames(x)) {
        colnames(x)[colnames(x) == tritonTime] <- 'UTC'
    }
    x
}

# colnames are d-m-y h:m:s, 0, 0-freq end
checkManta <- function(x) {
    if(all(grepl('^X', colnames(x)))) {
        colnames(x) <- gsub('^X', '', colnames(x))
    }
    dateCol <- colnames(x)[1]
    mantaFormat <- c('%d-%b-%Y %H:%M:%S', '%m/%d/%Y %H:%M:%S',
                     '%d.%b.%Y.%H.%M.%S', '%m.%d.%Y.%H.%M.%S')
    tryConvert <- suppressWarnings(parse_date_time(dateCol, orders=mantaFormat, tz='UTC', truncated=2))
    # manta has the date as first column name, if we couldnt convert
    # then this isnt manta
    if(is.na(tryConvert)) {
        return(x)
    }
    # manta second col is seconds? only sometimes
    checkSeconds <- all(x[[2]] <= 60)
    secondCol <- grepl('^0\\.{3}[0-9]{1}$', colnames(x)[2]) ||
        (colnames(x)[2] == '0' & colnames(x)[3] == '0.1')
    checkSeconds <- checkSeconds & secondCol
    if(isTRUE(checkSeconds )) {
        x[[2]] <- NULL
        colnames(x)[2] <- '0'
    }

    # manta should have columns named just frequency for 2:ncol
    # if we cant convert w/o NA, then its not manta
    freqCols <- colnames(x)[2:ncol(x)]
    tryFreq <- suppressWarnings(as.numeric(freqCols))
    if(anyNA(tryFreq)) {
        return(x)
    }
    colnames(x)[1] <- 'UTC'
    if(is.character(x$UTC)) {
        x$UTC <- parse_date_time(x$UTC, orders=mantaFormat, tz='UTC', truncated=2)
    }
    colnames(x)[2:ncol(x)] <- paste0('HMD_', colnames(x)[2:ncol(x)])
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
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[2:ncol(x)]))
    if(length(type) != 1) {
        stop('"x" must be in long format with columns "UTC", "frequency", and "value" OR',
             ' columns 2:n must named in format TYPE_FREQUENCY.')
    }
    if(type != 'BB') {
        colnames(x)[2:ncol(x)] <- gsub('[A-z]+_', '', colnames(x)[2:ncol(x)])
        x <- pivot_longer(x, cols=2:ncol(x), names_to='frequency', values_to='value')
        x$frequency <- as.numeric(x$frequency)
        # x$type <- gsub('_[0-9-]+', '', x$type)
        x$type <- type
    }
    if(type == 'BB') {
        freqRange <- gsub('BB_', '', colnames(x)[2])
        # freqs <- as.numeric(strsplit(freqRange, '-')[[1]])
        x$type <- 'BB'
        colnames(x)[grepl('BB_', colnames(x))] <- 'value'
        # x$freqMin <- freqs[1]
        # x$freqMax <- freqs[2]
        x$frequency <- freqRange
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


#' @importFrom lubridate period
#'
unitToPeriod <- function(x) {
    x <- gsub('([0-9]*)(.*)', '\\1_\\2', x)
    x <- strsplit(x, '_')[[1]]
    if(x[1] == '') {
        x[1] <- '1'
    }
    period(as.numeric(x[1]), units=x[2])
}

isLong <- function(x) {
    all(c('UTC', 'type', 'value', 'frequency') %in% colnames(x))
}