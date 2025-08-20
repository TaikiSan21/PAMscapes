#' @title Evaluate Recording Files for Issues
#'
#' @description Evaluates recording files for potential problems. Sound
#'   levels are calculated for a small section of each recording file, this
#'   is typically done to check for recorder malfunction. Additionally
#'   times between the starts and ends of files are calculated, this is
#'   typically done to check for gaps in data.
#'
#' @param wavFiles file paths to wav files to evaluate, or the directory
#'   containing the wav files
#' @param sampleWindow start and end (in seconds) of the time window to use
#'   for analysis, e.g. \code{c(40, 100)} will use a 60 second window starting
#'   40 seconds into the file
#' @param octave type of sound level to calculate, either \code{'tol'} for
#'   third octave level or \code{'ol'} for octave level
#' @param channel channel of the file to use for analysis
#' @param freqRange if not \code{NULL}, a vector of two numbers giving the
#'   range of frequencies to use for analysis (\code{NULL} will use the full
#'   available range)
#' @param calibration if not \code{NULL}, the frequency dependent calibration
#'   to apply. Must have "frequency" and "gain" (in dB), can either be a .tf
#'   file, a CSV file with columns for frequency and gain, a dataframe with
#'   columns frequency and gain, or a NetCDF with "frequency" dimension and
#'   "senstivity" or "gain" variable
#' @param sensitivity the sensitivity of the recording device in dB, this
#'   is typically a large negative number
#' @param progress logical flag to show a progress bar
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a dataframe containing the sound level and data gap measurements
#'   for each file
#'
#' @importFrom PAMmisc pwelch
#' @importFrom signal interp1 hamming
#' @importFrom tuneR readWave
#'
#' @export
#'
evaluateRecordings <- function(wavFiles,
                             sampleWindow=c(60, 120),
                             octave=c('tol', 'ol'),
                             channel=1,
                             freqRange=NULL,
                             calibration=NULL,
                             sensitivity=0,
                             progress=TRUE) {
    if(length(wavFiles) == 1 &&
       dir.exists(wavFiles)) {
        wavFiles <- list.files(wavFiles, recursive=FALSE, full.names=TRUE, pattern='\\.wav$')
    }
    octave <- match.arg(octave)
    octaves <- getOctaveLevels(match.arg(octave), freqRange=freqRange)
    if(progress) {
        pb <- txtProgressBar(min=0, max=length(wavFiles), style=3)
        ix <- 0
    }
    calibration <- checkCalibration(calibration)
    maxTries <- 3
    WINDOW <- hamming(1)
    PLANFREQ <- 0
    OCTPLAN <- NULL
    badTimes <- character(0)
    tol <- lapply(wavFiles, function(x) {
        fileTime <- fileToTime(x)
        if(is.na(fileTime)) {
            badTimes <<- c(badTimes, x)
            if(progress) {
                ix <<- ix + 1
                setTxtProgressBar(pb, value=ix)
            }
            return(NULL)
        }
        # implement retry on failed read
        for(i in 1:maxTries) {
            readTry <- try({
                wavHdr <- fastReadWave(x, header = TRUE)
                # wavHdr <- tuneR::readWave(x, header=TRUE)
                nfft <- wavHdr$sample.rate
                wavLength <- wavHdr$samples / nfft
                # fix if wav not long enough
                if(wavLength < sampleWindow[2]) {
                    from <- max(wavLength - diff(sampleWindow), 0)
                    to <- min(from + diff(sampleWindow), wavLength)
                } else {
                    from <- sampleWindow[1]
                    to <- sampleWindow[2]
                }
                wavClip <- fastReadWave(x, from=from, to=to)

            })
            if(length(wavClip) == 0) {
                warning('File ', x, ' appears to be corrupt (length of 0)')
                if(progress) {
                    ix <<- ix + 1
                    setTxtProgressBar(pb, value=ix)
                }
                return(
                    list(UTC=fileToTime(x),
                         wavLength=0,
                         file=basename(x))
                )
            }
            # if no error break
            if(!inherits(readTry, 'try-error')) {
                break
            }
            # if errored this many times, its bad
            if(i == maxTries) {
                warning('File ', x, ' could not be read after ', maxTries, ' attempts')
                if(progress) {
                    ix <<- ix + 1
                    setTxtProgressBar(pb, value=ix)
                }
                return(list(UTC=fileToTime(x),
                            file=basename(x)))
            }
        }
        # trying to avoid recalc when possible
        if(length(WINDOW) != nfft) {
            WINDOW <<- hamming(nfft)
        }
        # this is list of $freq(Hz) $spec (linear)
        welch <- pwelch(wavClip, nfft=nfft, noverlap=0, window=WINDOW, demean='long', channel=channel)
        # drop 0 freq part
        welch$freq <- welch$freq[-1]
        welch$spec <- welch$spec[-1]
        # apply calibration - sens only, or sens + transfer function
        calValues <- sensitivity
        if(!is.null(calibration)) {
            calValues <- calValues +
                interp1(calibration$frequency, calibration$gain, xi=welch$freq, method='pchip')
        }
        # calibration is in log space
        welch$spec <- welch$spec * 10^(-calValues / 10)

        if(is.null(OCTPLAN) ||
           max(welch$freq) != PLANFREQ) {
            PLANFREQ <<- max(welch$freq)
            OCTPLAN <<- planBandSum('psd', octave, inRange=c(1, PLANFREQ), outRange=c(1, PLANFREQ))
        }
        tolVals <- vector('list', length=length(OCTPLAN))
        # names(welch$spec) <- paste0('PSD_', format(round(welch$freq, 0), scientific=FALSE, trim=TRUE))
        names(tolVals) <- names(OCTPLAN)
        for(i in seq_along(tolVals)) {
            tolVals[[i]] <- 10*log10(sum(OCTPLAN[[i]]$factor * welch$spec[OCTPLAN[[i]]$freqs]))
        }

        # tolBins <- cut(welch$freq, octaves$limits, octaves$labels)
        # tolVals <- lapply(split(welch$spec, tolBins, drop=TRUE), function(p) {
        #     10*log10(sum(p))
        # })
        tolVals$UTC <- fileToTime(x)
        tolVals$wavLength <- wavLength
        tolVals$file <- basename(x)
        if(progress) {
            ix <<- ix + 1
            setTxtProgressBar(pb, value=ix)
        }
        tolVals
    })
    if(length(badTimes) > 0) {
        warning('Could not parse files times for ', length(badTimes), 
                ' out of ', length(wavFiles), ' files, these are excluded',
                ' from analysis.')
    }
    if(!is.null(calibration) &&
       PLANFREQ > max(calibration$frequency)) {
        warning('Calibration function did not cover range of frequencies in data,',
                ' results outside calibration range will be NA')
    }
    tol <- bind_rows(tol)
    tol <- arrange(tol, .data$UTC)
    tol$timeToNext <- 0
    tol$timeToNext[1:(nrow(tol)-1)] <- as.numeric(
        difftime(
            tol$UTC[2:nrow(tol)],
            tol$UTC[1:(nrow(tol)-1)],
            units='secs'
        )
    )
    tol$diffBetweenLength <- tol$timeToNext - tol$wavLength
    tol$diffBetweenLength[nrow(tol)] <- 0
    tol
}

#' @importFrom utils read.fwf
#'
readHarpTf <- function(x) {
    if(!grepl('tf$', x)) {
        stop('Not a HARP transfer function .tf file')
    }
    tf <- read.fwf(x, widths=c(6, -3, 6))
    colnames(tf) <- c('frequency', 'gain')
    tf
}

readNcTf <- function(x) {
    if(!grepl('nc$', x)) {
        stop('Not a NetCDF calibration file')
    }
    nc <- nc_open(x)
    on.exit(nc_close(nc))
    gainVar <- c('gain', 'sensitivity') %in% names(nc$var)
    if(!'frequency' %in% names(nc$dim) ||
       !any(gainVar)) {
        stop('Could not find variable "sensitivity" and dim "frequency" in .nc file')
    }
    gainCol <- c('gain', 'sensitivity')[gainVar][1]
    freq <- nc$dim$frequency$vals
    ## may need to adjust HMD kine
    # we double round the name to avoid mismatched round-to-even behavior
    # labels <- nc$dim$frequency$val
    # labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
    # labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
    # labels <- paste0(freqType, '_', labels)
    ##
    sens <- ncvar_get(nc, gainCol)
    data.frame(frequency=freq, gain=sens)
}

checkCalibration <- function(x) {
    if(is.null(x) || is.na(x)) {
        return(NULL)
    }
    if(is.character(x)) {
        if(!file.exists(x)) {
            stop('Calibration file ', x, ' does not exist')
        }
        if(grepl('tf$', x)) {
            x <- readHarpTf(x)
            return(x)
        }
        if(grepl('csv$', x)) {
            x <- read.csv(x, stringsAsFactors = FALSE)
        }
        if(grepl('nc$', x)) {
            x <- readNcTf(x)
            return(x)
        }
    }
    if(!is.data.frame(x)) {
        stop('Calibration must be a dataframe, .tf, or .csv file')
    }
    calMapper <- data.frame(old=c('freq', 'f', 'gain db', 'gain.db'),
                            new=c('frequency', 'frequency', 'gain', 'gain'))
    names(x) <- tolower(names(x))
    for(i in 1:nrow(calMapper)) {
        if(!calMapper$old[i] %in% names(x)) {
            next
        }
        names(x)[names(x) == calMapper$old[i]] <- calMapper$new[i]
    }
    if(!all(c('frequency', 'gain') %in% names(x))) {
        stop('Could not parse "frequency" and "gain" columns from input')
    }
    x
}
