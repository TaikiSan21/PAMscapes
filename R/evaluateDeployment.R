#' @title Evaluate deployment of recording files for potential problems
#'
#' @description Runs a number of quality assurance / quality control (QAQC)
#'   checks on a folder of recording files to identify potential problems.
#'   These include checking the start and end times of files for consistency
#'   to identify potential data gaps, measuring sound levels in each file
#'   to identify potential recorder issues, and (if applicable) checking
#'   battery and temperature data to identify potential instrument failure.
#'   Can also create spectrogram images throughout the deployment to aid in
#'   visually checking for problems or noise.
#'
#' @param dir folder or folders containing recordings and optionally Soundtrap
#'   .log.xml files. All .wav and .log.xml files within \code{dir} will be
#'   analysed, as well as all files in each subfolder of \code{dir} (only
#'   going down one level).
#' @param excludeDirs the names of any subfolders within \code{dir} that
#'   should be excluded (e.g. if \code{dir} contains folders "Recordings" and
#'   "Clips" then \code{excludeDirs="Clips"} would result in only the "Recordings"
#'   folder being analysed
#' @param sampleWindow start and end (in seconds) of the time window to use
#'   for analysis, e.g. \code{c(40, 100)} will use a 60 second window starting
#'   40 seconds into the file
#' @param channel channel number of recording files to use for analysis
#' @param sensitivity the sensitivity of the recording device in dB, this
#'   is typically a large negative number
#' @param calibration if not \code{NULL}, the frequency dependent calibration
#'   to apply. Must have "frequency" and "gain" (in dB), can either be a .tf
#'   file, a CSV file with columns for frequency and gain, or a dataframe with
#'   columns frequency and gain
#' @param timeRange if not \code{NULL}, a vector of two POSIXct times identifying
#'   the expected start and end times of the deployment. If the actual start and
#'   end times of of the recording files are earlier or later than these, then
#'   a warning will be issued and no calculations will be done, returning
#'   \code{NULL}
#' @param name a name to assign for this deployment, used for plot labeling,
#'   logging, and stored as \code{projectName} with the output dataframe. If
#'   left as \code{NULL}, then the basename of \code{dir} will be used.
#' @param subDirPattern if not \code{NULL}, a pattern to use for selecting
#'   which subfolders of \code{dir} to use for analysis. E.g. if folders
#'   "Site1_Recordings" and "Site2_Recordings" both exist in \code{dir}, then
#'   \code{subDirPattern="^Site1"} would result in analysing only the first
#'   folder of recordings
#' @param outDir if not \code{NULL}, a directory to store outputs. Outputs include
#'   plots, a CSV of the calculated metrics, and a .txt log file if \code{log=TRUE}
#' @param nSpectrograms number of spectrogram images to generate. Recording files
#'   for creating the spectrograms will be approximately evenly spaced throughout
#'   the deployment - if \code{nSpectrograms=5}, then the first recording will be
#'   randomly chosen from the first 20% of files, the second from the second 20%,
#'   etc.
#' @param specLength length of spectrograms to create, in seconds
#' @param panelLength length of each panel of the spectrogram plot, in seconds.
#'   This must be less than \code{specLength}, and is used to avoid creating
#'   awkwardly long plots. If \code{specLength=360} and \code{panelLength=60},
#'   then the result will be a 6 panel plot where each section is 60 seconds long
#' @param log if \code{TRUE} and \code{outDir} is not \code{NULL}, then a text
#'   file named "(name)_EvaluateRecorder_LogFile.txt" will be created in
#'   \code{outDir} logging progress and warning messages
#' @param progress logical flag to show a progress bar
#' @param verbose logical flag to show some messages
#'
#' @returns a dataframe of the QAQC metric outputs for each recording file
#'
#' @importFrom PAMmisc fastReadWave processSoundtrapLogs
#' @importFrom scales label_log
#' @importFrom utils write.csv
#'
#' @export
#'
evaluateDeployment <- function(dir,
                               excludeDirs=c('Post_Retrieval_Data', 'Pre_Deployment_Data'),
                               sampleWindow=c(60, 120),
                               channel=1,
                               sensitivity=NA,
                               calibration=NULL,
                               timeRange=NULL,
                               name=NULL,
                               subDirPattern=NULL,
                               outDir=NULL,
                               nSpectrograms=0,
                               specLength=1800,
                               panelLength=300,
                               log=FALSE,
                               progress=TRUE,
                               verbose=TRUE) {
    # subDirPattern is used when multiple device IDs were prsent in same main
    # folder so that they can be processed separately

    # add option that dir can be multiple if XML and wav not same main folder

    # wav, log.xml, these are only file types we currently allow
    exts <- '\\.wav|\\.log\\.xml'
    if(any(!dir.exists(dir))) {
        warning('Folder ', printN(dir[!dir.exists(dir)]), ' does not exist')
        return(NULL)
    }
    if(!is.null(outDir) && !dir.exists(outDir)) {
        dir.create(outDir)
    }
    if(isTRUE(log) && is.null(outDir)) {
        warning('Cannot create log file without specifying "outDir"')
        return(NULL)
    }
    if(is.null(name)) {
        name <- basename(dir[1])
    }
    procStart <- Sys.time()
    on.exit({
        procEnd <- Sys.time()
        timeMin <- as.numeric(difftime(procEnd, procStart, units='mins'))
        if(isTRUE(verbose)) {
            cat('Project', name, 'processed in', round(timeMin, 1), 'minutes\n')
        }
    })
    if(isTRUE(log)) {
        logName <- paste0(name, '_EvaluateRecorder_LogFile.txt')
        logCon <- file(file.path(outDir, logName), open='wt')
        sink(file=logCon, type='message')
        sink(file=logCon, type='output')
        ow <- getOption('warn')
        options(warn=1)
        on.exit({
            options(warn=ow)
            sink()
            sink(type='message')
            close(logCon)
        },
        add=TRUE, after=TRUE)
        cat('------------------------------------------\n')
        cat('Starting deployment evaluation at',
            format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
        cat('------------------------------------------\n')
        cat('Analysis files saved to', outDir, '\n---\n')
        cat('Analyzing files in', dir, '\n---\n')
        cat('Analyzing project', name, '\n---\n')
    }

    # only going down one subfolder
    # subDirs <- list.dirs(dir, full.names=TRUE, recursive=FALSE)
    subDirs <- unlist(lapply(dir, list.dirs, full.names=TRUE, recursive=FALSE))
    # sometimes we split into FPOD/ST directories first
    hasST <- any(grepl('_ST$', subDirs))
    hasFPOD <- any(grepl('_FPOD$', subDirs))
    if(hasST && hasFPOD) {
        dir <- grep('_ST$', subDirs, value=TRUE)
        subDirs <- list.dirs(dir, full.names=TRUE, recursive=FALSE)
    }
    if(length(subDirs) == 1) {
        oneMore <- list.dirs(subDirs, full.names=TRUE, recursive=FALSE)
        if(length(oneMore) != 0) {
            subDirs <- oneMore
        }
    }
    keepDirs <- rep(TRUE, length(subDirs))
    for(e in excludeDirs) {
        keepDirs <- keepDirs & !grepl(e, basename(subDirs), ignore.case=TRUE)
    }
    subDirs <- subDirs[keepDirs]
    if(!is.null(subDirPattern)) {
        keepSub <- grepl(subDirPattern, basename(subDirs))
        if(!any(keepSub)) {
            warning('subDirPattern ', subDirPattern, ' did not match any subfolders',
                    ' in folder ', dir, immediate.=TRUE)
            keepSub <- rep(TRUE, length(keepSub))
        }
        subDirs <- subDirs[keepSub]
    }
    # subDirs <- subDirs[!basename(subDirs) %in% excludeDirs]
    # list files from both base and sub dirs
    allFiles <- unlist(lapply(c(dir, subDirs), function(x) {
        list.files(x, pattern=exts, full.names=TRUE, recursive=FALSE)
    }))
    isWav <- grepl('\\.wav$', allFiles)
    if(!any(isWav)) {
        warning('No wav files found in folder ', dir, immediate. = TRUE)
        return(NULL)
    }
    wavFiles <- allFiles[isWav]
    if(!is.null(subDirPattern)) {
        wavBase <- unique(dirname(wavFiles))
        if(length(wavBase) > 1) {
            warning('More than 1 folder of wav files was found within folder ',
                    dir, ' using subDirPattern ', subDirPattern, immediate.=TRUE)
        }
    }
    isLog <- grepl('\\.log\\.xml', allFiles)
    logFiles <- allFiles[isLog]

    wavTimes <- fileToTime(wavFiles)
    if(!is.null(timeRange)) {
        startWav <- which.min(wavTimes)
        diffStart <- as.numeric(difftime(wavTimes[startWav], timeRange[1], units='secs'))
        if(diffStart < -1) {
            warning('Appears Clipping has not happened - first wav file (',
                    basename(wavFiles[startWav]), ') is ', abs(diffStart),
                    ' seconds before expected start time', immediate. = TRUE)
            return(NULL)
        }
        endWav <- which.max(wavTimes)
        endHeader <- fastReadWave(wavFiles[endWav], header=TRUE)
        endTime <- wavTimes[endWav] + endHeader$samples / endHeader$sample.rate
        diffEnd <- as.numeric(difftime(wavTimes[endWav], endTime, units='secs'))
        if(diffEnd > 1) {
            warning('Appears Clipping has not happened - last wav file (',
                    basename(wavFiles[endWav]), ') is ', abs(diffEnd),
                    ' seconds after expected end time', immediate. = TRUE)
            return(NULL)
        }
    }
    if(!is.null(calibration) &&
       is.character(calibration) &&
       !file.exists(calibration)) {
        isCal <- grepl(basename(calibration), allFiles)
        if(!any(isCal)) {
            warning('Calibration file ', calibration, ' could not be found')
            return(NULL)
        }
        calibration <- allFiles[isCal][1]
    }
    if(isTRUE(log)) {
        cat('Calculating TOLs from', sampleWindow[1], 'to', sampleWindow[2],
            'for', length(wavFiles), 'files', '\n')
        cat('---\n')
        cat('Total calibration correction of recorder and hydrophone entered as',
            sensitivity, 'dB', '\n---\n')
        if(is.null(calibration) || is.na(calibration)) {
            cat('No frequency-dependent calibration applied', '\n---\n')
        } else if(is.character(calibration)) {
            cat('Applying frequency-dependent calibration from file',
                calibration, '\n')
        } else {
            cat('Applying frequency-dependent calibration\n')
        }
    }
    wavQaqc <- evaluateRecordings(wavFiles,
                                  sampleWindow=sampleWindow,
                                  octave='tol',
                                  channel=channel,
                                  calibration=calibration,
                                  sensitivity=sensitivity,
                                  progress=progress
    )
    if(!is.null(name)) {
        wavQaqc$projectName <- name
    }
    if(!is.null(outDir)) {
        tolPlot <- plotQAQCLevel(wavQaqc, dbRange=c(50, 140), freqMin=30, title=name) +
            theme(plot.title = element_text(hjust = 0.5))
        ggsave(filename=file.path(outDir, paste0(name, '_TOLPlot.png')),
               plot=tolPlot, width=4e3, height=3e3, units='px')

        gapPlot <- plotQAQCGap(wavQaqc, title=name) +
            theme(plot.title = element_text(hjust = 0.5))
        ggsave(filename=file.path(outDir, paste0(name, '_GapPlot.png')),
               plot=gapPlot, width=3e3, height=4e3, units='px')
        if(!is.null(calibration)) {
            calPoints <- checkCalibration(calibration)
            freqs <- seq(from=min(calPoints$frequency), to=max(calPoints$frequency), by=1)
            calSmooth <- data.frame(frequency=freqs,
                                    gain=interp1(calPoints$frequency, calPoints$gain, xi=freqs, method='pchip'))
            logFreq <- floor(range(log10(freqs)))
            logBreaks <- 10^((logFreq[1]):(logFreq[2]))
            calPlot <- ggplot() +
                geom_point(data=calPoints, aes(x=.data$frequency, y=.data$gain)) +
                geom_line(data=calSmooth, aes(x=.data$frequency, y=.data$gain)) +
                scale_x_log10(breaks=logBreaks, labels=label_log()) +
                ggtitle(name) +
                labs(y='Transfer Function (dB)', x='Frequency (Hz)')
            ggsave(filename=file.path(outDir, paste0(name, '_CalibrationPlot.png')),
                   plot=calPlot, width=3e3, height=2e3, units='px')
        }
    }

    # IF st log files exist add the batt/temp data
    if(any(isLog)) {
        if(isTRUE(log)) {
            cat('---', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
            cat('Analyzing Soundtrap log files for Temperature and Voltage\n')
        }
        logQaqc <- processSoundtrapLogs(logFiles)
        wavQaqc <- timeJoin(wavQaqc,
                            rename(logQaqc, 'UTC' = 'startUTC')[c('UTC', 'intBatt', 'extBatt', 'temp')],
                            interpolate=FALSE)
        # logs may not match first and last wav files bc they can be chopped, correct
        firstIn <- wavQaqc$UTC[1] >= logQaqc$startUTC & wavQaqc$UTC[1] <= logQaqc$endUTC
        lastIn <- wavQaqc$UTC[nrow(wavQaqc)] >= logQaqc$startUTC & wavQaqc$UTC[nrow(wavQaqc)] <= logQaqc$endUTC
        firstIn <- max(which(firstIn))
        lastIn <- max(which(lastIn))
        wavQaqc$intBatt[1] <- logQaqc$intBatt[firstIn]
        wavQaqc$intBatt[nrow(wavQaqc)] <- logQaqc$intBatt[lastIn]
        wavQaqc$extBatt[1] <- logQaqc$extBatt[firstIn]
        wavQaqc$extBatt[nrow(wavQaqc)] <- logQaqc$extBatt[lastIn]
        wavQaqc$temp[1] <- logQaqc$temp[firstIn]
        wavQaqc$temp[nrow(wavQaqc)] <- logQaqc$temp[lastIn]

        if(!is.null(outDir)) {
            tvPlot <- plotQAQCTV(wavQaqc, title=name) +
                theme(plot.title = element_text(hjust = 0.5))
            ggsave(filename=file.path(outDir, paste0(name, '_TempVoltPlot.png')),
                   plot=tvPlot, width=3e3, height=2e3, units='px')
        }
    }
    if(!is.null(outDir)) {
        wavQaqc$UTC <- psxTo8601(wavQaqc$UTC)
        write.csv(wavQaqc,
                  file=file.path(outDir, paste0(name, '_QAQCData.csv')),
                  row.names=FALSE)
        wavQaqc$UTC <- as.POSIXct(wavQaqc$UTC, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC')
    }
    if(isTRUE(log)) {
        cat('---', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
        cat('Generating', nSpectrograms, 'spectrograms\n')
    }
    if(!is.null(outDir) &&
       nSpectrograms > 0) {
        specDir <- file.path(outDir, paste0(name, '_Spectrograms'))
        if(!dir.exists(specDir)) {
            dir.create(specDir)
        }
        toSpec <- rep(NA, nSpectrograms)
        nWavs <- length(wavFiles)
        brks <- ceiling(seq(from=1, to=nWavs+1, length.out=nSpectrograms+1))
        for(i in seq_along(toSpec)) {
            thisSet <- seq(from=brks[i], to=brks[i+1]-1, by=1)
            toSpec[i] <- sample(thisSet, size=1)
        }
        if(toSpec[nSpectrograms] > length(nWavs)) {
            toSpec[nSpectrograms] <- nWavs
        }
        for(i in seq_along(toSpec)) {
            thisWav <- fastReadWave(wavFiles[toSpec[i]], from=0, to=specLength)
            baseWav <- gsub('\\.[A-z]{1,4}$', '', basename(wavFiles[toSpec[i]]))
            thisFile <- paste0(name, '_',
                               'Spectrogram', specLength, 'sec_',
                               baseWav, '.png')
            trySpec <- try(createSpecImage(thisWav, channel=1, wl=2048, hop=1,
                                           title=paste0(name, '_', baseWav),
                                           startTime=fileToTime(wavFiles[toSpec[i]]),
                                           panelLength=panelLength, ratio=1,
                                           file=file.path(specDir, thisFile)))
            if(inherits(trySpec, 'try-error')) {
                warning('Problem with spectrogram for ', baseWav)
            }
        }
    }

    if(isTRUE(log)) {
        cat('------------------------------------------\n')
        cat('Evaluation finished without issue on',
            format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
        cat('------------------------------------------\n')
    }
    wavQaqc
}

psxTo8601 <- function(x) {
    if(is.character(x)) {
        return(x)
    }
    format(x, format='%Y-%m-%dT%H:%M:%SZ')
}

#' @importFrom tools file_path_sans_ext
#'
fileToTime <- function(x) {
    if(length(x) > 1) {
        result <- lapply(x, fileToTime)
        result <- as.POSIXct(unlist(result), origin='1970-01-01 00:00:00', tz='UTC')
        return(result)
    }
    x <- basename(x)
    x <- file_path_sans_ext(x)
    format <- c('pamguard', 'pampal', 'soundtrap', 'sm3', 'icListens1', 'icListens2', 
                'AMAR', 'WISPR', 'PMAR')
    for(f in format) {
        switch(
            f,
            'pamguard' = {
                date <- gsub('.*([0-9]{8}_[0-9]{6}_[0-9]{3})$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 15), tz = 'UTC', format = '%Y%m%d_%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 17, 19)) / 1e3
                if(!is.na(posix)) {
                    break
                }
            },
            'pampal' = {
                date <- gsub('.*([0-9]{14}_[0-9]{3})$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 14), tz = 'UTC', format = '%Y%m%d%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 16, 18)) / 1e3
                if(!is.na(posix)) {
                    break
                }
            },
            'soundtrap' = {
                date <- gsub('.*\\.([0-9]{12})$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'sm3' = {
                date <- gsub('.*\\_([0-9]{8}_[0-9]{6})Z?$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d_%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'icListens1' = {
                date <- gsub('.*_([0-9]{8}-[0-9]{6})$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'icListens2' = {
                date <- gsub('.*_([0-9]{6}-[0-9]{6})$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'AMAR' = {
                # 'AMAR668.9.20210823T231318Z.wav' example
                date <- gsub('.*([0-9]{8}T[0-9]{6}Z)$', '\\1', x)
                posix <- as.POSIXct(date, format='%Y%m%dT%H%M%SZ', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    break
                }
            },
            'WISPR' = {
              # 'WISPR_230504_195202.wav' example
              date <- gsub('.*_([0-9]{6}_[0-9]{6})$', '\\1', x)
              posix <- as.POSIXct(date, format='%y%m%d_%H%M%S', tz='UTC')
              millis <- 0
              if(!is.na(posix)) {
                break
              }
            },
            'PMAR' = {
              # 'template_230411-221029.038.wav' example
              date <- gsub('.*_([0-9]{6}-[0-9]{6}.[0-9]{3})$', '\\1', x)
              posix <- as.POSIXct(substr(date, 1, 13), tz = 'UTC', format = '%y%m%d-%H%M%S')
              if(is.na(posix)) next
              millis <- as.numeric(substr(date, 15, 17)) / 1e3
              if(!is.na(posix)) {
                break
              }
            }
        )
    }
    posix + millis
}

timeJoin <- function(x, y, thresh=Inf, interpolate=TRUE, replace=FALSE, keepDiff=FALSE) {
    if(is.null(x) ||
       nrow(x) == 0) {
        return(x)
    }
    x[['timeDiff']] <- NULL
    # reservedCols <- c('eventId', 'db', 'UID', 'eventLabel', 'BinaryFile', 'species', 'Channel')
    if(isFALSE(replace)) {
        addCols <- colnames(y)[!colnames(y) %in% colnames(x)]
        oldCols <- colnames(x)
    } else if(isTRUE(replace)) {
        addCols <- colnames(y)[colnames(y) != 'UTC']
        oldCols <- colnames(x)[!colnames(x) %in% colnames(y)]
        oldCols <- c('UTC', oldCols)
    } else if(is.na(replace)) {
        overlaps <- (colnames(y) %in% colnames(x)) &
            colnames(y) != 'UTC'
        if(any(overlaps)) {
            overlaps <- colnames(y)[overlaps]
            hasNa <- sapply(overlaps, function(c) {
                anyNA(x[[c]])
            })
            hasNa <- overlaps[hasNa]
            for(n in hasNa) {
                isNa <- is.na(x[[n]])
                newMatch <- timeJoin(x[c('UTC', n)], y[c('UTC', n)], thresh=thresh, interpolate=interpolate, replace=TRUE)
                x[[n]][isNa] <- newMatch[[n]][isNa]
            }
        }
        addCols <- colnames(y)[!colnames(y) %in% colnames(x)]
        oldCols <- colnames(x)
    }
    # addCols <- addCols[!addCols %in% reservedCols]
    if(length(addCols) == 0) {
        return(x)
    }
    y <- y[c('UTC', addCols)]
    x <- x[oldCols]
    x$xTime <- x$UTC
    y$yTime <- y$UTC
    setDT(x)
    setDT(y)
    setkeyv(x, 'UTC')
    setkeyv(y, 'UTC')
    result <- y[x, roll='nearest']
    result$timeDiff <- abs(as.numeric(result$xTime) - as.numeric(result$yTime))
    tooFar <- result$timeDiff > thresh
    result[tooFar, (addCols) := NA]
    if(nrow(y) == 1) {
        interpolate <- FALSE
    }
    if(interpolate) {
        for(c in addCols) {
            interpVal <- approx(x=y$UTC, y=y[[c]], xout=result$xTime[!tooFar], rule=2)$y
            result[[c]][!tooFar] <- interpVal
        }
    }
    result[, c('xTime', 'yTime') := NULL]
    setDF(x)
    setDF(y)
    setDF(result)
    outCols <- c(oldCols, addCols)
    if(isTRUE(keepDiff)) {
        outCols <- c(outCols, 'timeDiff')
    }
    result[outCols]
}
