#' @title Load Soundscape Data
#'
#' @description Reads and checks data to ensure formatting will work
#'   for other \code{PAMscapes} functions. Will read and check the
#'   formatting of CSV files, or check the formatting of dataframes.
#'   Can also read in MANTA NetCDF files and format the data
#'   appropriately.
#'
#' @param x a dataframe, path to a CSV file, or path to a MANTA
#'   NetCDF file, or folder containing these. If \code{x} is a vector
#'   of file paths then all will be read in and combined. If \code{x}
#'   is a folder, then all files with extension \code{extension} will
#'   be loaded. Note this will not load files within subfolders, only
#'   the main folder.
#' @param needCols names of columns that must be present in \code{x},
#'   if any are missing will trigger an error
#' @param skipCheck logical flag to skip some data checking, recommended
#'   to keep as \code{FALSE}
#' @param timeBin amount of time to bin data by, format can
#'   be "#Unit" e.g. \code{'2hour'} or \code{'1day'}
#' @param binFunction summary function to apply to data in each time bin,
#'   default is "median"
#' @param binCount logical flag to return the number of times in
#'   each time bin as column "binCount"
#' @param octave one of "original", "tol", or "ol". If "original" then
#'   nothing happens, otherwise data are converted to Octave-leve ("ol")
#'   or Third-Octave-Level ("tol") measurements using
#'   \link{createOctaveLevel}
#' @param label optional, if not \code{NULL} then this value will be
#'   added as an additional column "label" to the output
#' @param keepEffort if \code{TRUE} or \code{FALSE}, a logical flag whether or
#'   not to keep the effort information with the outputs (number of seconds
#'   per minute). If a numeric value, then any minutes with an effort value
#'   less than \code{keepEffort} will be removed (e.g. \code{50} will remove
#'   minutes with less than 50 seconds of effort)
#' @param dropNonHmd logical flag to drop non-standard hybrid millidecade
#'   bands, only applies to HMD type data. Some datasets have frequency
#'   values that are not part of the standard HMD bands (e.g. at exactly
#'   the Nyquist rate), if \code{TRUE} these will be removed.
#' @param tz timezone of the data being loaded, will be converted to UTC
#'   after load
#' @param extension only used if \code{x} is a folder, the file extension
#'   to load. Must be one of "nc" or "csv"
#'
#' @details Files created by MANTA and Triton software will be
#'   reformatted to have consisitent formatting. The first column
#'   will be renamed to "UTC", and columns containing soundscape
#'   metrics will be named using the convention "TYPE_FREQUENCY",
#'   e.g. "HMD_1", "HMD_2" for Manta hybrid millidecade mesaurements.
#'
#'   Inputs from sources other than MANTA or Triton can be accepted
#'   in either "wide" or "long" format. Wide format must follow
#'   the conventions above - first column "UTC", other columns
#'   named by "TYPE_FREQUENCY" where TYPE is consistent across all
#'   columns and FREQUENCY is in Hertz. Long format data must have
#'   the following columns:
#'   \describe{
#'     \item{"UTC"}{ - time of the measurement, in UTC timezone}
#'     \item{"type"}{ - the type of soundscape measurement e.g.
#'       PSD or OL, must be the same for all}
#'     \item{"frequency"}{ - the frequency of the measurement, in Hertz}
#'     \item{"value"}{ - the soundscape measurement value, usually dB}
#'   }
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a dataframe
#'
#' @examples
#'
#' manta <- loadSoundscapeData(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
#' str(manta)
#' ol <- loadSoundscapeData(system.file('extdata/OLSmall.csv', package='PAMscapes'))
#' str(ol)
#' psd <- loadSoundscapeData(system.file('extdata/PSDSmall.csv', package='PAMscapes'))
#' str(psd)
#'
#' @export
#'
#' @importFrom data.table fread setDF
#' @importFrom lubridate force_tz with_tz
#' @importFrom future.apply future_lapply
#'
loadSoundscapeData <- function(x,
                               needCols=c('UTC'),
                               skipCheck=FALSE,
                               timeBin=NULL,
                               binFunction='median',
                               binCount=FALSE,
                               octave=c('original', 'tol', 'ol'),
                               label=NULL,
                               keepEffort=TRUE,
                               dropNonHmd=TRUE,
                               tz='UTC',
                               extension=c('nc', 'csv')) {
    if(is.character(x) &&
       length(x) == 1 &&
       dir.exists(x)) {
        ext <- switch(match.arg(extension),
                      'nc' = '\\.nc$',
                      'csv' = '\\.csv$'
        )
        x <- list.files(x, pattern=ext, full.names=TRUE)
    }
    octave <- match.arg(octave)
    allowedExt <- '\\.nc$|\\.csv$'
    if(is.character(x)) {
        x <- x[grepl(allowedExt, x)]
        if(length(x) == 0) {
            warning('No files of appropriate type provided.')
            return(NULL)
        }
    }
    # combine if multiple files
    if(is.character(x) &&
       length(x) > 1) {
        x <- bind_rows(future_lapply(x, function(f) {
            loadSoundscapeData(f, needCols=needCols, skipCheck=skipCheck,
                               timeBin=timeBin, binFunction=binFunction,
                               binCount=binCount,
                               octave=octave, label=label, keepEffort=keepEffort,
                               dropNonHmd = FALSE,
                               tz=tz)
        }, future.seed=NULL))
        freqCols <- whichFreqCols(x)
        freqVals <- colsToFreqs(colnames(x)[freqCols])
        type <- gsub('([A-z]*)_.*', '\\1', colnames(x)[freqCols][1])
        # standardizing to round to integer on all HMD columns
        if(type == 'HMD') {
            standardHmd <- paste0('HMD_', round(freqVals, 0))
            colnames(x)[freqCols] <- standardHmd
            hmdLevels <- getHmdLevels(freqRange=range(freqVals)+c(-1, 1))
            nonStandard <- !standardHmd %in% hmdLevels$labels
            newLabs <- fixHmdLabels(freqVals[nonStandard], hmdLevels=hmdLevels)
            colnames(x)[freqCols][nonStandard][!is.na(newLabs)] <- newLabs[!is.na(newLabs)]
            if(anyNA(newLabs) &&
               isTRUE(dropNonHmd)) {
                warning('Found ', sum(is.na(newLabs)), ' non-standard ',
                        'hybrid millidecade frequencies (',
                        paste0(standardHmd[nonStandard][is.na(newLabs)], collapse=', '),
                        ') these will be removed. Run with "dropNonHmd=FALSE"',
                        ' to keep them.')
                for(col in standardHmd[nonStandard[is.na(newLabs)]]) {
                    x[[col]] <- NULL
                }
            }
        }
        return(x)
    }
    if(is.character(x)) {
        if(!file.exists(x)) {
            warning('File ', x, ' does not exist.')
            return(NULL)
        }
        if(grepl('csv$', x, ignore.case=TRUE)) {
            # head <- strsplit(read_lines(x, n_max = 1), ', ')[[1]]
            # first <- strsplit(read_lines(x, n_max=1, skip=1), ', ')[[1]]
            readTop <- strsplit(readLines(x, n=2), ',')
            if(length(readTop[[1]]) < length(readTop[[2]])) {
                warning('File ', x, ' has more data columns than column headers. Cannot load.')
                return(NULL)
            }
            x <- fread(x, header=TRUE)
            setDF(x)
        } else if(grepl('nc$', x, ignore.case=TRUE)) {
            x <- loadMantaNc(x, keepEffort=keepEffort)
        }
    }
    colnames(x) <- checkTimeName(colnames(x))
    x <- checkManta(x, keepEffort=keepEffort)
    if(isFALSE(skipCheck)) {
        # x <- checkTriton(x)
        x <- checkInfinite(x)
    }
    missingCols <- needCols[!needCols %in% colnames(x)]
    if(length(missingCols) > 0) {
        warning('Required columns ', paste0(missingCols, collapse=', '),
                ' are missing.')
        return(NULL)
    }
    if(is.character(x$UTC)) {
        x$UTC <- parseToUTC(x$UTC)
    }
    if(tz != 'UTC') {
        x$UTC <- force_tz(x$UTC, tzone=tz)
        x$UTC <- with_tz(x$UTC, tzone='UTC')
    }
    if(!isWide(colnames(x)) && !isLong(colnames(x))) {
        warning('Input "x" could not be formatted properly.')
        return(NULL)
    }
    # for now this check is just fixing 31_5 to 31.5
    colnames(x) <- checkFreqNames(colnames(x))
    freqCols <- whichFreqCols(x)
    freqVals <- colsToFreqs(colnames(x)[freqCols])
    type <- gsub('([A-z]*)_.*', '\\1', colnames(x)[freqCols][1])
    # standardizing to round to integer on all HMD columns
    if(type == 'HMD') {
        standardHmd <- paste0('HMD_', round(freqVals, 0))
        colnames(x)[freqCols] <- standardHmd
        hmdLevels <- getHmdLevels(freqRange=range(freqVals)+c(-1, 1))
        nonStandard <- !standardHmd %in% hmdLevels$labels
        newLabs <- fixHmdLabels(freqVals[nonStandard], hmdLevels=hmdLevels)
        colnames(x)[freqCols][nonStandard][!is.na(newLabs)] <- newLabs[!is.na(newLabs)]
        if(anyNA(newLabs) &&
           isTRUE(dropNonHmd)) {
            warning('Found ', sum(is.na(newLabs)), ' non-standard ',
                    'hybrid millidecade frequencies (',
                    paste0(standardHmd[nonStandard][is.na(newLabs)], collapse=', '),
                    ') these will be removed. Run with "dropNonHmd=FALSE"',
                    ' to keep them.')
            for(col in standardHmd[nonStandard[is.na(newLabs)]]) {
                x[[col]] <- NULL
            }
        }
    }
    if(!is.null(timeBin)) {
        x <- binSoundscapeData(x, bin=timeBin, method=binFunction, binCount=binCount)
    }
    if(octave != 'original') {
        x <- createOctaveLevel(x, type=octave)
    }
    if(!is.null(label)) {
        x$label <- label
    }
    x
}


# i hate this we cant round to the same level because of REASONS
# so fix by matching closest in round(0) cases
fixHmdLabels <- function(freqVals, hmdLevels=NULL) {
    if(is.null(hmdLevels)) {
        hmdLevels <- getHmdLevels(freqRange=range(freqVals) + c(-1, 1))
    }
    newLabels <- rep(NA, length(freqVals))
    for(i in seq_along(freqVals)) {
        diffs <- abs(hmdLevels$freqs - freqVals[i])
        whichMin <- which.min(diffs)
        minDiff <- diffs[whichMin]
        if(minDiff <= 1) {
            newLabels[i] <- hmdLevels$labels[whichMin]
        }
    }
    newLabels
}

checkInfinite <- function(x, doWarn=TRUE) {
    infCols <- sapply(x, function(c) any(is.infinite(c)))
    if(!any(infCols)) {
        return(x)
    }
    infIx <- which(infCols)
    if(doWarn) {
        warning('Found infinite values in "x", they will be replaced with NA.')
    }
    for(i in infIx) {
        x[[i]][is.infinite(x[[i]])] <- NA
    }
    x
}

checkTriton <- function(x) {
    tritonTime <- "yyyy-mm-ddTHH:MM:SSZ"
    if(tritonTime %in% colnames(x)) {
        colnames(x)[colnames(x) == tritonTime] <- 'UTC'
    }
    alternate <- 'yyyy.mm.ddTHH.MM.SSZ'
    if(alternate %in% colnames(x)) {
        colnames(x)[colnames(x) == alternate] <- 'UTC'
    }
    alternate <- 'yyyy_mm_ddTHH_MM_SSZ'
    if(alternate %in% colnames(x)) {
        colnames(x)[colnames(x) == alternate] <- 'UTC'
    }
    x
}

checkTimeName <- function(x) {
    if(is.data.frame(x)) {
        names <- checkTimeName(names(x))
        names(x) <- names
        return(x)
    }
    tritonTime <- "yyyy-mm-ddTHH:MM:SSZ"
    if(tritonTime %in% x) {
        x[x == tritonTime] <- 'UTC'
    }
    alternate <- 'yyyy.mm.ddTHH.MM.SSZ'
    if(alternate %in% x) {
        x[x == alternate] <- 'UTC'
    }
    alternate <- 'yyyy_mm_ddTHH_MM_SSZ'
    if(alternate %in% x) {
        x[x == alternate] <- 'UTC'
    }
    x
}

# colnames are d-m-y h:m:s, 0, 0-freq end
checkManta <- function(x, keepEffort=FALSE) {
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
        (colnames(x)[2] == '0' & colnames(x)[3] %in% c('0', '0.1'))
    checkSeconds <- checkSeconds & secondCol
    freqIx <- 2:ncol(x)
    if(isTRUE(checkSeconds)) {
        if(isFALSE(keepEffort)) {
            x[[2]] <- NULL
            colnames(x)[2] <- '0'
            freqIx <- 2:ncol(x)
        } else if(isTRUE(keepEffort)) {
            colnames(x)[2:3] <- c('effortSeconds', '0')
            freqIx <- 3:ncol(x)
        }
    }
    # manta should have columns named just frequency for 2:ncol
    # if we cant convert w/o NA, then its not manta
    freqCols <- colnames(x)[freqIx]
    tryFreq <- suppressWarnings(as.numeric(freqCols))
    # sometimes written as 31_5 instead of 31.5?
    if(anyNA(tryFreq)) {
        freqCols <- gsub('_', '.', freqCols)
        tryFreq <- suppressWarnings(as.numeric(freqCols))
    }
    if(anyNA(tryFreq)) {
        return(x)
    }
    colnames(x)[1] <- 'UTC'
    if(is.character(x$UTC)) {
        x$UTC <- parse_date_time(x$UTC, orders=mantaFormat, tz='UTC', truncated=2)
    }
    colnames(x)[freqIx] <- paste0('HMD_', freqCols)
    x
}

checkFreqNames <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    # grepl('[0-9]+_[0-9]+', x)
    x <- gsub('(.*)([0-9]+)_([0-9]+)', '\\1\\2.\\3', x)
    x
}
