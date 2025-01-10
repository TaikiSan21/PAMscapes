# util functions

#' @importFrom tools R_user_dir
#'
getTempCacheDir <- function(create=TRUE) {
    tempDir <- R_user_dir("PAMscapes", which = "cache")
    if(create &&
       !dir.exists(tempDir)) {
        dir.create(tempDir, recursive=TRUE)
    }
    tempDir
}

fileNameManager <- function(fileName=NULL, suffix=NULL) {
    if(is.null(fileName)) {
        tempDir <- getTempCacheDir(create=TRUE)
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

#' @importFrom tidyr pivot_longer_spec pivot_longer
#'
toLong <- function(x, spec=NULL) {
    if(isLong(x)) {
        return(x)
    }
    whichFreq <- whichFreqCols(x)
    type <- unique(gsub('_[0-9\\.-]+', '', colnames(x)[whichFreq]))
    if(length(type) != 1) {
        stop('"x" must be in long format with columns "UTC", "frequency", and "value" OR',
             ' columns 2:n must named in format TYPE_FREQUENCY.')
    }
    if(type != 'BB') {
        freqCols <- gsub('[A-z]+_', '', colnames(x)[whichFreq])
        colnames(x)[whichFreq] <- freqCols
        if(is.null(spec)) {
            spec <- data.frame(.name=freqCols, .value='value', frequency=as.numeric(freqCols))
        }
        # x <- pivot_longer(x, cols=all_of(whichFreq), names_to='frequency', values_to='value',
        #                   names_transform = as.numeric)
        x <- pivot_longer_spec(x, spec=spec)
        x$type <- type
    }
    if(type == 'BB') {
        freqRange <- gsub('BB_', '', colnames(x)[whichFreq[1]])
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
    if(isWide(x)) {
        return(x)
    }
    x <- pivot_wider(x, names_from=c('type', 'frequency'), names_sep='_', values_from='value')
    x
}

isLong <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    all(c('UTC', 'type', 'value', 'frequency') %in% x)
}

isWide <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    freqCols <- whichFreqCols(x)
    # all(grepl('^[A-z]+_[0-9\\.\\-]+$', freqCols))
    if(length(freqCols) == 1 &&
       grepl('BB', x[freqCols])) {
        return(TRUE)
    }
    length(freqCols) > 1
}

nowUTC <- function() {
    now <- Sys.time()
    attr(now, 'tzone') <- 'UTC'
    now
}


#' @importFrom lubridate period period_to_seconds seconds_to_period is.period
#'
unitToPeriod <- function(x) {
    if(is.period(x)) {
        return(x)
    }
    x <- gsub('([0-9]*)(.*)', '\\1_\\2', x)
    x <- strsplit(x, '_')[[1]]
    if(x[1] == '') {
        x[1] <- '1'
    }
    if(x[1] == '1') {
        return(period(1, units=x[2]))
    }
    # doing this to convert to roundest unit
    # e.g. 720 seconds -> 12 minutes
    seconds_to_period(
        period_to_seconds(
            period(as.numeric(x[1]), units=x[2])
        )
    )
}

whichFreqCols <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    isFreq <- grepl('^[A-z]+_[0-9\\.\\-]+$', x)
    colSplit <- split(x[isFreq], '_')
    type <- colSplit[[1]][1]
    sameBase <- sapply(colSplit, function(x) {
        x[1] == type
    })
    which(isFreq)[sameBase]
}

getNonFreqCols <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    if(isLong(x)) {
        longCols <- c('UTC', 'frequency', 'type', 'value')
        return(x[!x %in% longCols])
    }
    freqCols <- whichFreqCols(x)
    nonFreq <- x[!(1:length(x)) %in% freqCols]
    return(nonFreq[nonFreq != 'UTC'])
}

myLog10Scale <- function(g, range, dim=c('x', 'y')) {
    major <- logSeq(c(1,5))
    useMajor <- which(major >= range[1] & major <= range[2])
    maxMajor <- 6
    if(length(useMajor) > maxMajor) {
        major <- logSeq(1)
    }
    minor <- logSeq(1:9)
    maxMinor <- maxMajor * 10
    useMinor <- which(minor >= range[1] & minor <= range[2])
    if(length(useMinor) > maxMinor) {
        minor <- logSeq(c(1,5))
    }
    dim <- match.arg(dim)
    if(dim == 'x') {
        g +
            scale_x_continuous(trans='log10',
                               breaks = major,
                               minor_breaks = minor,
                               labels = scientific_10,
                               limits = range,
                               expand = c(0, 0)) +
            annotation_logticks(sides='b')
    } else {
        g +
            scale_y_continuous(trans='log10',
                               breaks = major,
                               minor_breaks = minor,
                               labels = scientific_10,
                               limits = range,
                               expand = c(0, 0)) +
            annotation_logticks(sides='l')
    }
}

#' @importFrom scales scientific_format
#'
scientific_10 <- function(x) {
    parse(text=gsub("e\\+{0,1}", " %*% 10^", scientific_format()(x)))
}

logSeq <- function(x) {
    min <- -1
    max <- 8
    exps <- seq(from=min, to=max, by=1)
    as.vector(sapply(exps, function(e) {
        x * 10 ^ e
    }))
}

checkSimple <- function(x, needCols='UTC') {
    tritonTime <- "yyyy-mm-ddTHH:MM:SSZ"
    if(tritonTime %in% colnames(x)) {
        colnames(x)[colnames(x) == tritonTime] <- 'UTC'
    }
    x
    if(!all(needCols %in% colnames(x))) {
        stop('"x" must have columns ', paste0(needCols, collapse=', '))
    }
    if(is.character(x$UTC)) {
        x$UTC <- parseToUTC(x$UTC)
    }
    x
}

checkCpal <- function(cpal, n) {
    if(is.null(cpal)) {
        cpal <- hue_pal()
    }
    if(is.character(cpal)) {
        if(length(cpal) == 1) {
            cpal <- rep(cpal, n)
        }
        if(length(cpal) < n) {
            stop('Must specify enough colors for each different item.')
        }
        plotColors <- cpal
    }
    if(is.function(cpal)) {
        plotColors <- cpal(n)
    }
    plotColors
}

# convert TOL, PSD, etc. to db re whatever units
typeToUnits <- function(type) {
    switch(type,
           'OL' = 'dB re: 1uPa',
           'TOL' = 'dB re: 1uPa',
           'PSD' = 'dB re: 1uPa^2/Hz',
           'HMD' = 'dB re: 1uPa^2/Hz',
           NULL
    )
}

colsToFreqs <- function(x) {
    if(is.data.frame(x)) {
        whichFreq <- whichFreqCols(colnames(x))
        x <- colnames(x)[whichFreq]
    }
    as.numeric(gsub('[A-z]+_', '', x))
}

checkFreqType <- function(freq) {
    if(is.character(freq)) {
        nc <- nc_open(freq)
        on.exit(nc_close(nc))
        freq <- nc$dim$frequency$vals
        if(is.null(freq)) {
            warning('No frequency dimension')
            return(NULL)
        }
    }
    regDiff <- round(diff(freq), 1)
    isOne <- regDiff == 1
    if(all(isOne)) {
        return('PSD')
    }
    # possible weirdness of others having 0, 1, 2? but not a lot
    if(sum(isOne) > 2) {
        return('HMD')
    }

    multDiff <- round(freq[2:length(freq)] / freq[1:(length(freq)-1)], 1)
    if(all(multDiff == 2)) {
        return('OL')
    }
    thirds <- seq(from=1, to=length(freq), by=3)
    freq <- freq[thirds]
    multDiff <- round(freq[2:length(freq)] / freq[1:(length(freq)-1)], 1)
    if(all(multDiff == 2)) {
        return('TOL')
    }
    warning('Could not parse frequency type')
    'FREQ'
}

calcSliceLength <- function(dates, maxSlice) {
    secDiff <- as.numeric(difftime(max(dates), min(dates), units='secs')) / maxSlice
    # second, minute, hour, day are valid options
    if(secDiff > 86400) {
        value <- secDiff / 86400
        unit <- 'day'
    } else if(secDiff > 3600) {
        value <- secDiff / 3600
        unit <- 'hour'
    } else if(secDiff > 60) {
        value <- secDiff / 60
        unit <- 'minute'
    } else if(secDiff <= 60) {
        value <- secDiff
        unit <- 'second'
    }

    secBin <- ceiling(value)
    binString <- paste0(secBin, unit)
    binPer <- unitToPeriod(binString)
    binPer
}
