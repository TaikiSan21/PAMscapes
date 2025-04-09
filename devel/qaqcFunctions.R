library(tuneR) #misc - readWave only
library(fftw) #misc - spec images
library(PAMmisc) #both
library(ggplot2) #both
# library(future.apply)
library(PAMscapes) #octave, tLong, freqs (shiny app mostly)
library(signal) #misc - interp1 and hanning
library(lubridate) #both
library(shiny) #scapes
library(ggplot2) #both
library(dplyr) # both
library(DT) # neither
# library(patchwork)
library(readxl) # neither, only for NEFSC specific
library(xml2) #misc, can export function
library(PAMpal)# just timeJoin


# subPattern is used when multiple device IDs were prsent in same main
# folder so that they can be processed separately

# add option that dir can be multiple if XML and wav not same main folder
# evaluateDeployment <- function(dir,
#                                excludeDirs=c('Post_Retrieval_Data', 'Pre_Deployment_Data'),
#                                sampleWindow=c(60, 120),
#                                channel=1,
#                                sensitivity=-174.2,
#                                calibration=NA,
#                                timeRange=NULL,
#                                name=NULL,
#                                subPattern=NULL,
#                                outDir=NULL,
#                                nSpectrograms=0,
#                                specLength=30,
#                                panelLength=5,
#                                log=FALSE,
#                                progress=TRUE) {
#     if(!dir.exists(dir)) {
#         warning('Folder ', dir, ' does not exist')
#         return(NULL)
#     }
#     if(!dir.exists(outDir)) {
#         dir.create(outDir)
#     }
#     procStart <- Sys.time()
#     on.exit({
#         procEnd <- Sys.time()
#         timeMin <- as.numeric(difftime(procEnd, procStart, units='mins'))
#         cat('Project', name, 'processed in', round(timeMin, 1), 'minutes\n')
#     })
#     if(log) {
#         logName <- paste0(name, '_EvaluateRecorder_LogFile.txt')
#         logCon <- file(file.path(outDir, logName), open='wt')
#         sink(file=logCon, type='message')
#         sink(file=logCon, type='output')
#         ow <- getOption('warn')
#         options(warn=1)
#         on.exit({
#             options(warn=ow)
#             sink()
#             sink(type='message')
#             close(logCon)
#         },
#         add=TRUE, after=TRUE)
#         cat('------------------------------------------\n')
#         cat('Starting deployment evaluation at',
#             format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
#         cat('------------------------------------------\n')
#         cat('Analysis files saved to', outDir, '\n---\n')
#         cat('Analyzing files in', dir, '\n---\n')
#         cat('Analyzing project', name, '\n---\n')
#     }
#     dirName <- basename(dir)
#     # wav, log.xml, csv&tf are for maybe calibration
#     exts <- '\\.wav|\\.log\\.xml'
#     # only going down one subfolder
#     subDirs <- list.dirs(dir, full.names=TRUE, recursive=FALSE)
#     # sometimes we split into FPOD/ST directories first
#     hasST <- any(grepl('_ST$', subDirs))
#     hasFPOD <- any(grepl('_FPOD$', subDirs))
#     if(hasST && hasFPOD) {
#         dir <- grep('_ST$', subDirs, value=TRUE)
#         subDirs <- list.dirs(dir, full.names=TRUE, recursive=FALSE)
#     }
#     keepDirs <- rep(TRUE, length(subDirs))
#     for(e in excludeDirs) {
#         keepDirs <- keepDirs & !grepl(e, basename(subDirs), ignore.case=TRUE)
#     }
#     subDirs <- subDirs[keepDirs]
#     if(!is.null(subPattern)) {
#         keepSub <- grepl(subPattern, subDirs)
#         if(!any(keepSub)) {
#             warning('subPattern ', subPattern, ' did not match any subfolders',
#                     ' in folder ', dir, immediate.=TRUE)
#             keepSub <- rep(TRUE, length(keepSub))
#         }
#         subDirs <- subDirs[keepSub]
#     }
#     # subDirs <- subDirs[!basename(subDirs) %in% excludeDirs]
#     # list files from both base and sub dirs
#     allFiles <- unlist(lapply(c(dir, subDirs), function(x) {
#         list.files(x, pattern=exts, full.names=TRUE, recursive=FALSE)
#     }))
#     isWav <- grepl('\\.wav$', allFiles)
#     if(!any(isWav)) {
#         warning('No wav files found in folder ', dir, immediate. = TRUE)
#         return(NULL)
#     }
#     wavFiles <- allFiles[isWav]
#     if(!is.null(subPattern)) {
#         wavBase <- unique(dirname(wavFiles))
#         if(length(wavBase) > 1) {
#             warning('More than 1 folder of wav files was found within folder ',
#                     dir, ' using subPattern ', subPattern, immediate.=TRUE)
#         }
#     }
#     isLog <- grepl('\\.log\\.xml', allFiles)
#     logFiles <- allFiles[isLog]
#
#     wavTimes <- wavToTime(wavFiles)
#     if(!is.null(timeRange)) {
#         startWav <- which.min(wavTimes)
#         diffStart <- as.numeric(difftime(wavTimes[startWav], timeRange[1], units='secs'))
#         if(diffStart < -1) {
#             warning('Appears Clipping has not happened - first wav file (',
#                     basename(wavFiles[startWav]), ') is ', abs(diffStart),
#                     ' seconds before expected start time', immediate. = TRUE)
#             return(NULL)
#         }
#         endWav <- which.max(wavTimes)
#         endHeader <- tuneR::readWave(wavFiles[endWav], header=TRUE)
#         endTime <- wavTimes[endWav] + endHeader$samples / endHeader$sample.rate
#         diffEnd <- as.numeric(difftime(wavTimes[endWav], endTime, units='secs'))
#         if(diffEnd > 1) {
#             warning('Appears Clipping has not happened - last wav file (',
#                     basename(wavFiles[endWav]), ') is ', abs(diffEnd),
#                     ' seconds after expected end time', immediate. = TRUE)
#             return(NULL)
#         }
#     }
#
#     if(log) {
#         cat('Calculating TOLs from', sampleWindow[1], 'to', sampleWindow[2],
#             'for', length(wavFiles), 'files', '\n')
#         cat('---\n')
#         cat('Total calibration correction of recorder and hydrophone entered as',
#             sensitivity, 'dB', '\n---\n')
#         if(is.null(calibration) || is.na(calibration)) {
#             cat('No frequency-dependent calibration applied', '\n---\n')
#         } else {
#             cat('Applying frequency-dependent calibration from file',
#                 calibration, '\n')
#         }
#     }
#     wavQaqc <- evaluateWavFiles(wavFiles,
#                                 sampleWindow=sampleWindow,
#                                 octave='tol',
#                                 channel=channel,
#                                 plot=FALSE,
#                                 calibration=calibration,
#                                 sensitivity=sensitivity,
#                                 progress=progress
#     )
#     if(!is.null(name)) {
#         wavQaqc$projectName <- name
#     }
#     if(!is.null(outDir)) {
#         tolPlot <- plotQAQCTol(wavQaqc, dbRange=c(50, 140), freqMin=30, title=name) +
#             theme(plot.title = element_text(hjust = 0.5))
#         ggsave(filename=file.path(outDir, paste0(name, '_TOLPlot.png')),
#                plot=tolPlot, width=4e3, height=3e3, units='px')
#
#         gapPlot <- plotQAQCGap(wavQaqc, title=name) +
#             theme(plot.title = element_text(hjust = 0.5))
#         ggsave(filename=file.path(outDir, paste0(name, '_GapPlot.png')),
#                plot=gapPlot, width=3e3, height=4e3, units='px')
#         if(!is.na(calibration)) {
#             calPoints <- checkCalibration(calibration)
#             freqs <- seq(from=min(calPoints$frequency), to=max(calPoints$frequency), by=1)
#             calSmooth <- data.frame(frequency=freqs,
#                                     gain=signal::interp1(calPoints$frequency, calPoints$gain, xi=freqs, method='pchip'))
#             logFreq <- floor(range(log10(freqs)))
#             logBreaks <- 10^((logFreq[1]):(logFreq[2]))
#             calPlot <- ggplot() +
#                 geom_point(data=calPoints, aes(x=frequency, y=gain)) +
#                 geom_line(data=calSmooth, aes(x=frequency, y=gain)) +
#                 scale_x_log10(breaks=logBreaks, labels=scales::trans_format('log10',scales::math_format(10^.x))) +
#                 ggtitle(name) +
#                 labs(y='Transfer Function (dB)', x='Frequency (Hz)')
#             ggsave(filename=file.path(outDir, paste0(name, '_CalibrationPlot.png')),
#                    plot=calPlot, width=3e3, height=2e3, units='px')
#         }
#     }
#
#     # IF st log files exist add the batt/temp data
#     if(any(isLog)) {
#         if(log) {
#             cat('---', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
#             cat('Analyzing Soundtrap log files for Temperature and Voltage\n')
#         }
#         logQaqc <- processSoundtrapLogs(logFiles)
#         wavQaqc <- PAMpal::timeJoin(wavQaqc,
#                                     rename(logQaqc, 'UTC' = 'startUTC')[c('UTC', 'intBatt', 'extBatt', 'temp')],
#                                     interpolate=FALSE)
#         # logs may not match first and last wav files bc they can be chopped, correct
#         firstIn <- wavQaqc$UTC[1] >= logQaqc$startUTC & wavQaqc$UTC[1] <= logQaqc$endUTC
#         lastIn <- wavQaqc$UTC[nrow(wavQaqc)] >= logQaqc$startUTC & wavQaqc$UTC[nrow(wavQaqc)] <= logQaqc$endUTC
#         firstIn <- max(which(firstIn))
#         lastIn <- max(which(lastIn))
#         wavQaqc$intBatt[1] <- logQaqc$intBatt[firstIn]
#         wavQaqc$intBatt[nrow(wavQaqc)] <- logQaqc$intBatt[lastIn]
#         wavQaqc$extBatt[1] <- logQaqc$extBatt[firstIn]
#         wavQaqc$extBatt[nrow(wavQaqc)] <- logQaqc$extBatt[lastIn]
#         wavQaqc$temp[1] <- logQaqc$temp[firstIn]
#         wavQaqc$temp[nrow(wavQaqc)] <- logQaqc$temp[lastIn]
#
#         if(!is.null(outDir)) {
#             tvPlot <- plotQAQCTV(wavQaqc, title=name) +
#                 theme(plot.title = element_text(hjust = 0.5))
#             ggsave(filename=file.path(outDir, paste0(name, '_TempVoltPlot.png')),
#                    plot=tvPlot, width=3e3, height=2e3, units='px')
#         }
#     }
#     if(!is.null(outDir)) {
#         wavQaqc$UTC <- psxTo8601(wavQaqc$UTC)
#         write.csv(wavQaqc,
#                   file=file.path(outDir, paste0(name, '_QAQCData.csv')),
#                   row.names=FALSE)
#         wavQaqc$UTC <- as.POSIXct(wavQaqc$UTC, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC')
#     }
#     if(log) {
#         cat('---', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
#         cat('Generating', nSpectrograms, 'spectrograms\n')
#     }
#     if(!is.null(outDir) &&
#        nSpectrograms > 0) {
#         specDir <- file.path(outDir, paste0(name, '_Spectrograms'))
#         if(!dir.exists(specDir)) {
#             dir.create(specDir)
#         }
#         toSpec <- rep(NA, nSpectrograms)
#         nWavs <- length(wavFiles)
#         brks <- ceiling(seq(from=1, to=nWavs+1, length.out=nSpectrograms+1))
#         for(i in seq_along(toSpec)) {
#             thisSet <- seq(from=brks[i], to=brks[i+1]-1, by=1)
#             toSpec[i] <- sample(thisSet, size=1)
#         }
#         if(toSpec[nSpectrograms] > length(nWavs)) {
#             toSpec[nSpectrograms] <- nWavs
#         }
#         for(i in seq_along(toSpec)) {
#             thisWav <- fastReadWave(wavFiles[toSpec[i]], from=0, to=specLength * 60)
#             baseWav <- gsub('\\.[A-z]{1,4}$', '', basename(wavFiles[toSpec[i]]))
#             thisFile <- paste0(name, '_',
#                                'Spectrogram', specLength, 'min_',
#                                baseWav, '.png')
#             trySpec <- try(createSpecImage(thisWav, channel=1, wl=2048, hop=1,
#                                            title=paste0(name, '_', baseWav),
#                                            startTime=wavToTime(wavFiles[toSpec[i]]),
#                                            panelLength=panelLength, ratio=1,
#                                            file=file.path(specDir, thisFile)))
#             if(inherits(trySpec, 'try-error')) {
#                 warning('Problem with spectrogram for ', baseWav)
#             }
#         }
#     }
#
#     if(log) {
#         cat('------------------------------------------\n')
#         cat('Evaluation finished without issue on',
#             format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\n')
#         cat('------------------------------------------\n')
#     }
#     wavQaqc
# }

# plotQAQCTol <- function(x,
#                         levels=c('ol', 'tol'),
#                         tBuffer=0,
#                         dbRange=NULL,
#                         freqMin=NULL,
#                         title=NULL) {
#     x <- PAMscapes:::toLong(x)
#     if(!is.null(freqMin)) {
#         x <- x[x$frequency >= freqMin, ]
#     }
#     levels <- match.arg(levels)
#     plotLevels <- PAMscapes:::getOctaveLevels(levels, freqRange=range(x$frequency))
#     x <- x[x$frequency %in% plotLevels$freqs, ]
#     x$frequency <- factor(x$frequency)
#     tRange <- range(x$UTC) + c(-1, 1) * tBuffer
#     if(is.null(dbRange)) {
#         dbRange <- range(x$value)
#     }
#     brks <- seq(from=floor(dbRange[1]/10)*10,
#                 to=ceiling(dbRange[2]/10)*10,
#                 by=10)
#     g <- ggplot(x, aes(x=UTC, y=value, color=frequency)) +
#         geom_line(linewidth=0.5) +
#         scale_x_datetime(limits=tRange) +
#         scale_y_continuous(limits=dbRange, breaks=brks)
#     if(!is.null(title)) {
#         g <- g + ggtitle(title)
#     }
#     g
# }
#
# plotQAQCGap <- function(x, title=NULL) {
#     x <- x[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
#     names(x)[3:4] <- c('Wav End to Next File (s)',
#                        'Time Between File Start (s)')
#     x <- tidyr::pivot_longer(x, cols=c('Wav End to Next File (s)',
#                                        'Time Between File Start (s)'))
#     # units opts?
#     g <- ggplot(x, aes(x=UTC, y=.data$value)) +
#         geom_line() +
#         facet_wrap(~name, scales='free', ncol=1) +
#         theme(strip.text.x = element_text(size=12)) +
#         ylab('Seconds')
#     if(!is.null(title)) {
#         g <- g + ggtitle(title)
#     }
#     g
# }
#
# plotQAQCTV <- function(x, title=NULL) {
#     if(all(is.na(x$extBatt)) ||
#        max(x$extBatt, na.rm=TRUE) == 0) {
#         battCol1 <- 'intBatt'
#         battCol2 <- NULL
#     } else {
#         battCol1 <- 'extBatt'
#         battCol2 <- 'intBatt'
#     }
#     x <- rename(x, 'Temperature (C)'='temp')
#     g <- plotScaledTimeseries(x, columns=c(battCol1, 'Temperature (C)', battCol2), cpal=c('darkblue', 'darkorange', 'blue'))
#     g <- g + ylab('Battery (V)')
#     if(!is.null(title)) {
#         g <- g + ggtitle(title)
#     }
#     g
# }

# plotQuicklook <- function(qData, dbRange=c(50, 140), freqMin=30, name=NULL) {
#     tol <- plotQAQCTol(qData, freqMin=freqMin, dbRange=dbRange)
#     gap <- plotQAQCGap(qData)
#     if(all(c('temp', 'extBatt') %in% names(qData))) {
#         tv <- plotQAQCTV(qData)
#     } else {
#         tv <- ggplot(data.frame(x=1, y=1, label='No Temp Volt Data')) +
#             geom_text(aes(x=x, y=y, label=label)) +
#             theme_void()
#     }
#     layout <- '
#     AB
#     CB
#     '
#     if(is.null(name)) {
#         name <- qData$projectName[1]
#     }
#     (tol + gap + tv) +
#         plot_layout(design=layout) +
#         plot_annotation(title=name,
#                         theme=theme(plot.title=element_text(hjust=.5)))
# }

# createSpecImage <- function(clip, channel=1, wl=1024, hop=1,
#                             brightness=0, contrast=0,
#                             cmap=gray.colors(64, start=1, end=0),
#                             title=NULL,
#                             startTime=NULL,
#                             panelLength=NULL,
#                             file=NULL, ratio=2) {
#     gram <- wavToSpecgram(clip[channel, ], sr=clip$rate, wl=wl, hop=hop, axes=TRUE)
#     # if making one long plot
#     if(is.null(panelLength)) {
#         gramToPlot(gram, file=file, startTime=startTime, cmap=cmap, title=title)
#         return()
#     }
#     # otherwise plan the breaking into subplots
#     plotIx <- floor(gram$x / panelLength / 60)
#     nPlots <- length(unique(plotIx))
#     oneWidth <- sum(plotIx == plotIx[1])
#     if(!is.null(file)) {
#         png(filename=file, width=oneWidth/ratio, height=nPlots * length(gram$y)/ratio, units='px')
#         par(mgp=c(3, 2, 0))
#         on.exit(dev.off())
#     }
#     oPar <- par()$mfrow
#     par(mfrow=c(nPlots, 1))
#     on.exit(par(mfrow=oPar), add=TRUE, after=FALSE)
#
#     for(i in unique(plotIx)) {
#         thisGram <- gram
#         thisGram$mat <- thisGram$mat[plotIx == i, ]
#         thisGram$x <- thisGram$x[plotIx == i]
#         gramToPlot(thisGram, file=NULL, startTime=startTime, cmap=cmap, title=title, ratio=ratio)
#
#     }
# }

# gramToPlot <- function(x, file=NULL, cmap=gray.colors(64, start=1, end=0), startTime=NULL, title=NULL, ratio=2) {
#     q <- c(.01, .99)
#     lim <- quantile(x$mat, q, na.rm=TRUE)
#     x$mat[x$mat < lim[1]] <- lim[1]
#     x$mat[x$mat > lim[2]] <- lim[2]
#     x$mat <- x$mat / diff(range(x$mat, na.rm=TRUE)) * 255
#     x$mat <- x$mat - min(x$mat, na.rm=TRUE)
#     x$mat[x$mat > 255] <- 255
#     x$mat[x$mat < 0] <- 0
#     if(!is.null(file)) {
#         png(filename=file, width=nrow(x$mat)/ratio, height=ncol(x$mat)/ratio, units='px')
#         par(mgp=c(3, 2, 0))
#         on.exit(dev.off())
#     }
#     image(x$mat, col = cmap, xaxt='n', yaxt='n',
#           useRaster=TRUE,
#           breaks=seq(from=0, to=255, length.out=length(cmap)+1),
#           ylab='Frequency (kHz)')
#     if(!is.null(title)) {
#         title(title, cex.main=6/ratio)
#     }
#     tLabs <- seq(from=floor(min(x$x) / 60), to=ceiling(max(x$x) / 60), by=1)
#     tLocs <- (tLabs - floor(min(x$x) / 60)) / ceiling(diff(range(x$x)) / 60)
#     if(is.null(startTime)) {
#         tLabs <- paste0(tLabs, 'min')
#     } else {
#         tLabs <- startTime + tLabs * 60
#     }
#     axis(1, at=tLocs, labels=tLabs, cex.axis=4/ratio)
#     yPretty <- pretty(x$y, n=5)
#     axis(2, at=(yPretty-min(x$y))/diff(range(x$y)), labels = yPretty, cex.axis=4/ratio)
# }

# wavToSpecgram <- function(wav, sr=NULL, wl=1024, hop=.5, axes=FALSE, ...) {
#     if(hop <= 1) {
#         hop <- wl * hop
#     }
#     window <- signal::hanning(wl)
#     planFF <- fftw::planFFT(wl)
#     nSlices <- ceiling((length(wav) - wl)/(hop)) + 1
#     slices <- seq(from=1, by=hop, length.out=nSlices)
#     mat <- t(apply(as.matrix(slices), 1, function(s) {
#         thisWav <- wav[s:(s+wl-1)]
#         thisWav[is.na(thisWav)] <- 0
#         thisWav <- thisWav - mean(thisWav)
#         if(all(thisWav == 0)) {
#             return(rep(NA, wl/2))
#         }
#         thisGram <- calcSpec(thisWav,sr=sr, window=window, plan=planFF)
#         thisGram[, 2]
#     }))
#     if(!axes) {
#         return(mat)
#     }
#     yAxis <- (1:wl) / wl * sr / 1e3
#     xAxis <- (slices-1) / sr
#     list(mat=mat, x=xAxis, y=yAxis)
# }
#
# calcSpec <- function(wave, window = TRUE, sr, plan=NULL) {
#     len <- length(wave)
#     halfLen <- len %/% 2
#     if(isTRUE(window)) {
#         w <- signal::hanning(len)
#     } else if(isFALSE(window)) {
#         w <- rep(1, length(wave))
#     } else if(is.numeric(window)) {
#         w <- window
#     }
#     wave <- wave * w
#     if(is.null(plan)) {
#         plan <- fftw::planFFT(len)
#     }
#     FUN <- function(x) {
#         result <- Mod(fftw::FFT(x, plan=plan))^2
#         result <- 2 * result[1:halfLen]
#         result <- result / sum(w)^2
#         10*log10(result)
#     }
#     y <- (1:(len)) / len * sr
#     ans <- matrix(NA, nrow=halfLen, ncol=2)
#     ans[, 1] <- y[1:halfLen]
#     dB <- FUN(wave)
#     isInf <- is.infinite(dB)
#     if(all(isInf)) {
#         return(ans)
#     }
#     if(any(isInf)) {
#         dB[dB == Inf] <- max(dB[!isInf], na.rm=TRUE)
#         dB[dB == -Inf] <- min(dB[!isInf], na.rm=TRUE)
#     }
#     ans[, 2] <- dB[1:halfLen]
#     ans
# }

processQAQCLog <- function(x, tolWindow=c(60, 120), nSpectrograms=0, rerun=FALSE, autosave=TRUE, log=TRUE) {
    baseDir <- NA
    if(is.character(x)) {
        if(dir.exists(x)) {
            baseDir <- x
        } else if(file.exists(x)) {
            baseDir <- dirname(x)
        } else {
            stop('Path ', x, ' does not exist')
        }
        if(isTRUE(autosave)) {
            on.exit(saveQLog(x, baseDir, update = 'new'))
        }
        x <- readQLog(x)
    }
    if(!isQaqcLog(x)) {
        stop('This is not in the expected QAQC Log format')
    }
    x$qaqcStatus <- checkValidStatus(x$qaqcStatus)
    toRun <- x$qaqcStatus %in% c('NoQAQC', 'TimeChecked', 'ClipOnly')
    if(!any(toRun)) {
        message('No more projects to run QAQC on!')
        return(x)
    }
    if(!all(dir.exists(unique(x$projectBaseDir[toRun])))) {
        stop('Base recording directories do not exist - check path or remap')
    }
    if(!all(dir.exists(unique(x$qaqcBaseDir[toRun])))) {
        stop('Base QAQC directories do no exist - check path or remap')
    }

    pb <- txtProgressBar(min=0, max=sum(toRun), style=3)
    ix <- 0
    for(i in which(toRun)) {
        # First check conditions for skipping ####
        if(is.na(x$qaqcDir[i])) {
            warning('Output directory for project ', x$projectName[i],
                    ' does not exist')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }
        outPath <- file.path(x$qaqcBaseDir[i], x$qaqcDir[i])
        if(!dir.exists(outPath)) {
            warning('Output directory for project ', x$projectName[i],
                    ' does not exist (check base directory)')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }
        if(is.na(x$sensitivity[i])) {
            warning('No sensitivity value for project ', x$projectName[i],
                    ' could not run QAQC')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }
        if(grepl('[Ss]ound[Tt]rap\\s*500', x$deviceName[i]) &&
           abs(x$sensitivity[i]) < 10) {
            warning('Soundtrap500 appears to be missing hydrophone sensitivity ',
                    'in project ', x$projectName[i], ', could not run QAQC')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }
        if(is.na(x$usableStart[i]) || is.na(x$usableEnd[i])) {
            warning('Usable start or end times are not entered for project ',
                    x$projectName[i], ', could not run QAQC')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }
        if(is.na(parseQDate(x$usableStart[i])) || is.na(parseQDate(x$usableEnd[i]))) {
            warning('Usable start or end times could not be parsed for project ',
                    x$projectName[i], ', could not run QAQC')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }
        thisName <- paste0(x$projectName[i], '_', x$deviceId[i])
        if(isFALSE(rerun) &&
           dir.exists(file.path(outPath, 'QAQC_Output')) &&
           file.exists(file.path(outPath, 'QAQC_Output', paste0(thisName, '_QAQCData.csv')))) {
            warning('QAQC outputs for project ', x$projectName[i], ' already exist and ',
                    'rerun=FALSE, they will not be created again.')
            ix <- ix + 1
            setTxtProgressBar(pb, value=ix)
            next
        }

        # Then try running the eval code ####
        # This saves outputs in outDir if its not NULL
        tryEvalDep <- try(evaluateDeployment(
            dir=file.path(x$projectBaseDir[i], x$projectDir[i]),
            excludeDirs=c('Post_Retrieval_Data', 'Pre_Deployment_Data'),
            sensitivity=x$sensitivity[i],
            calibration=x$calibration[i],
            sampleWindow=tolWindow,
            timeRange=parseQDate(c(x$usableStart[i], x$usableEnd[i])),
            name=thisName,
            subPattern=ifelse(is.na(x$deviceId[i]), NULL, paste0('^', x$deviceId[i])),
            log=log,
            nSpectrograms=nSpectrograms,
            outDir=file.path(outPath, 'QAQC_Output'),
            progress=FALSE
        ))
        if(isTRUE(log)) {
            logFile <- file.path(outPath, 'QAQC_Output', paste0(thisName, '_EvaluateRecorder_LogFile.txt'))
            f <- file(logFile)
            logLines <- readLines(f, warn=FALSE)
            close(f)
            hasWarn <- grepl('^Warning in', logLines)
            if(any(hasWarn)) {
                warning('Warning occurred when running project ', x$projectName[i],
                        ', check the log file for details.')
            }
        }
        if(is.null(tryEvalDep) || inherits(tryEvalDep, 'try-error')) {
            warning('Error running project ', x$projectName[i],
                    ': ', attr(tryEvalDep, 'condition')$message)
            ix <- ix +1
            setTxtProgressBar(pb, value=ix)
            next
        }
        x$qaqcStatus[i] <- 'QAQCRun'
        ix <- ix +1
        setTxtProgressBar(pb, value=ix)
    }
    x
}

isQaqcLog <- function(x) {
    if(is.data.frame(x)) {
        x <- colnames(x)
    }
    all(c('projectBaseDir',
          'projectDir',
          'projectName',
          'sensitivity',
          'calibration',
          'qaqcStatus',
          'qaqcBaseDir',
          'qaqcDir') %in%
            x)
}

readQData <- function(dir, pattern=NULL) {
    subDirs <- list.dirs(dir, recursive=FALSE, full.names=TRUE)
    if('QAQC_Output' %in% basename(subDirs)) {
        dir <- grep('QAQC_Output', subDirs, value=TRUE)
    }
    files <- list.files(dir, recursive=FALSE, full.names=TRUE)
    whichQdata <- grepl('QAQCData.csv', files)
    if(!any(whichQdata)) {
        warning('Cant find QAQCData.csv file')
        return(NULL)
    }
    qFile <- files[whichQdata]
    if(length(qFile) > 1 && !is.null(pattern)) {
        pattMatch <- grepl(pattern, basename(qFile))
        if(sum(pattMatch) == 1) {
            qFile <- qFile[pattMatch]
        }
    }
    if(length(qFile) > 1) {
        warning('Multiple QAQCData.csv files found, loading the first')
        qFile <- qFile[1]
    }
    # readr::read_csv(qFile)
    result <- read.csv(qFile, stringsAsFactors = FALSE)
    result$UTC <- parse_date_time(result$UTC, orders='%Y-%m-%d %H:%M:%S', tz='UTC')
    result
}

# evaluateWavFiles <- function(wavFiles,
#                              sampleWindow=c(60, 120),
#                              octave=c('tol', 'ol'),
#                              plot=TRUE,
#                              channel=1,
#                              freqRange=NULL,
#                              calibration=NULL,
#                              sensitivity=0,
#                              n=NULL,
#                              progress=TRUE) {
#     if(is.null(n)) {
#         n <- length(wavFiles)
#     }
#     if(n < length(wavFiles)) {
#         wavFiles <- wavFiles[1:n]
#     }
#     # need to parse wav file times and track
#     octaves <- PAMscapes:::getOctaveLevels(match.arg(octave), freqRange=freqRange)
#     if(progress) {
#         pb <- txtProgressBar(min=0, max=length(wavFiles), style=3)
#         ix <- 0
#     }
#
#     calibration <- checkCalibration(calibration)
#
#     maxTries <- 3
#     # tol <- future.apply::future_lapply(wavFiles, function(x) {
#     tol <- lapply(wavFiles, function(x) {
#         # implement retry on failed read
#         for(i in 1:maxTries) {
#             readTry <- try({
#                 if(packageVersion('PAMmisc') >= '1.12.2') {
#                     # wavHdr <- PAMmisc::fastReadWave(x, header = TRUE)
#                     # nfft <- wavHdr[[1]] # sample rate
#                     # wavLength <- wavHdr[[4]] / nfft # sample length/sample rate
#                     # BUG in fastread's header=TRUE leaving conns open
#                     wavHdr <- tuneR::readWave(x, header=TRUE)
#                     nfft <- wavHdr$sample.rate
#                     wavLength <- wavHdr$samples / nfft
#                     # fix if wav not long enough
#                     if(wavLength < sampleWindow[2]) {
#                         from <- max(wavLength - diff(sampleWindow), 0)
#                         to <- min(from + diff(sampleWindow), wavLength)
#                     } else {
#                         from <- sampleWindow[1]
#                         to <- sampleWindow[2]
#                     }
#                     wavClip <- PAMmisc::fastReadWave(x, from=from, to=to)
#                 } else {
#                     wavHdr <- tuneR::readWave(x, header=TRUE)
#                     nfft <- wavHdr$sample.rate
#                     wavLength <- wavHdr$samples / nfft
#                     if(wavLength < sampleWindow[2]) {
#                         from <- max(wavLength - diff(sampleWindow), 0)
#                         to <- min(from + diff(sampleWindow), wavLength)
#                     } else {
#                         from <- sampleWindow[1]
#                         to <- sampleWindow[2]
#                     }
#                     wavClip <- tuneR::readWave(x, from=from, to=to, units='seconds', toWaveMC = TRUE)
#                 }
#             })
#             # if no error break
#             if(!inherits(readTry, 'try-error')) {
#                 break
#             }
#             # if errored this many times, its bad
#             if(i == maxTries) {
#                 stop('File ', x, ' could not be read after ', maxTries, ' attempts')
#             }
#         }
#         # this is list of $freq(Hz) $spec (linear)
#         welch <- pwelch(wavClip, nfft=nfft, noverlap=0, demean='long', channel=channel)
#         # drop 0 freq part
#         welch$freq <- welch$freq[-1]
#         welch$spec <- welch$spec[-1]
#         # apply calibration - sens only, or sens + transfer function
#         calValues <- sensitivity
#         if(!is.null(calibration)) {
#             calValues <- calValues +
#                 signal::interp1(calibration$frequency, calibration$gain, xi=welch$freq, method='pchip')
#         }
#         welch$spec <- 10*log10(welch$spec) - calValues
#         welch$spec <- 10^(welch$spec / 10)
#
#         tolBins <- cut(welch$freq, octaves$limits, octaves$labels)
#         tolVals <- lapply(split(welch$spec, tolBins, drop=TRUE), function(p) {
#             10*log10(sum(p))
#         })
#         time <- wavToTime(x)
#         tolVals$UTC <- time
#         tolVals$wavLength <- wavLength
#         tolVals$file <- basename(x)
#         if(progress) {
#             ix <<- ix + 1
#             setTxtProgressBar(pb, value=ix)
#         }
#         tolVals
#     })
#     tol <- bind_rows(tol)
#     tol$timeToNext <- 0
#     tol$timeToNext[1:(nrow(tol)-1)] <- as.numeric(
#         difftime(
#             tol$UTC[2:nrow(tol)],
#             tol$UTC[1:(nrow(tol)-1)],
#             units='secs'
#         )
#     )
#     tol$diffBetweenLength <- tol$timeToNext - tol$wavLength
#     tol$diffBetweenLength[nrow(tol)] <- 0
#     if(plot) {
#         tolPlot <- plotQAQCTol(tol)
#         print(tolPlot)
#     }
#     tol
# }

# readHarpTf <- function(x) {
#     if(!grepl('tf$', x)) {
#         stop('Not a HARP transfer function .tf file')
#     }
#     tf <- read.fwf(x, widths=c(6, -3, 6))
#     colnames(tf) <- c('frequency', 'gain')
#     tf
# }
#
# checkCalibration <- function(x) {
#     if(is.null(x) || is.na(x)) {
#         return(NULL)
#     }
#     if(is.character(x)) {
#         if(!file.exists(x)) {
#             stop('Calibration file ', x, ' does not exist')
#         }
#         if(grepl('tf$', x)) {
#             x <- readHarpTf(x)
#             return(x)
#         }
#         if(grepl('csv$', x)) {
#             x <- read.csv(x, stringsAsFactors = FALSE)
#         }
#     }
#     if(!is.data.frame(x)) {
#         stop('Calibration must be a dataframe, .tf, or .csv file')
#     }
#     calMapper <- data.frame(old=c('freq', 'f', 'gain db', 'gain.db'),
#                             new=c('frequency', 'frequency', 'gain', 'gain'))
#     names(x) <- tolower(names(x))
#     for(i in 1:nrow(calMapper)) {
#         if(!calMapper$old[i] %in% names(x)) {
#             next
#         }
#         names(x)[names(x) == calMapper$old[i]] <- calMapper$new[i]
#     }
#     if(!all(c('frequency', 'gain') %in% names(x))) {
#         stop('Could not parse "frequency" and "gain" columns from input')
#     }
#     x
# }



#' wavToTime <- function(x) {
#'     if(length(x) > 1) {
#'         result <- lapply(x, wavToTime)
#'         result <- as.POSIXct(unlist(result), origin='1970-01-01 00:00:00', tz='UTC')
#'         return(result)
#'     }
#'     x <- basename(x)
#'     format <- c('pamguard', 'pampal', 'soundtrap', 'sm3', 'icListens1', 'icListens2', 'AMAR')
#'     for(f in format) {
#'         switch(
#'             f,
#'             'pamguard' = {
#'                 date <- gsub('.*([0-9]{8}_[0-9]{6}_[0-9]{3})\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(substr(date, 1, 15), tz = 'UTC', format = '%Y%m%d_%H%M%S')
#'                 if(is.na(posix)) next
#'                 millis <- as.numeric(substr(date, 17, 19)) / 1e3
#'                 if(!is.na(posix)) {
#'                     # FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             },
#'             'pampal' = {
#'                 date <- gsub('.*([0-9]{14}_[0-9]{3})\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(substr(date, 1, 14), tz = 'UTC', format = '%Y%m%d%H%M%S')
#'                 if(is.na(posix)) next
#'                 millis <- as.numeric(substr(date, 16, 18)) / 1e3
#'                 if(!is.na(posix)) {
#'                     # FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             },
#'             'soundtrap' = {
#'                 date <- gsub('.*\\.([0-9]{12})\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(date, format = '%y%m%d%H%M%S', tz='UTC')
#'                 millis <- 0
#'                 if(!is.na(posix)) {
#'                     # FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             },
#'             'sm3' = {
#'                 date <- gsub('.*\\_([0-9]{8}_[0-9]{6})Z?\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(date, format = '%Y%m%d_%H%M%S', tz='UTC')
#'                 millis <- 0
#'                 if(!is.na(posix)) {
#'                     # FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             },
#'             'icListens1' = {
#'                 date <- gsub('.*_([0-9]{8}-[0-9]{6})\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(date, format = '%Y%m%d-%H%M%S', tz='UTC')
#'                 millis <- 0
#'                 if(!is.na(posix)) {
#'                     # FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             },
#'             'icListens2' = {
#'                 date <- gsub('.*_([0-9]{6}-[0-9]{6})\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(date, format = '%y%m%d-%H%M%S', tz='UTC')
#'                 millis <- 0
#'                 if(!is.na(posix)) {
#'                     # FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             },
#'             'AMAR' = {
#'                 #'AMAR668.9.20210823T231318Z.wav' example
#'                 date <- gsub('.*([0-9]{8}T[0-9]{6}Z)\\.wav$', '\\1', x)
#'                 posix <- as.POSIXct(date, format='%Y%m%dT%H%M%SZ', tz='UTC')
#'                 millis <- 0
#'                 if(!is.na(posix)) {
#'                     FOUNDFORMAT <<- f
#'                     break
#'                 }
#'             }
#'         )
#'     }
#'     posix + millis
#' }




# levels controls how far down we can go
mapProjectDir <- function(project, dir, levels=4, verbose=TRUE) {
    if(is.null(project) || length(project) == 0) {
        character(0)
    }
    projMatch <- rep(NA, length(project))
    curDir <- dir
    subDirs <- dir
    while(levels > 0) {
        subBase <- basename(subDirs)
        thisMatch <- sapply(project, function(x) {
            any(x == subBase)
        })
        if(any(thisMatch)) {
            projMatch[thisMatch] <- sapply(project[thisMatch], function(x) {
                match <- subDirs[subBase == x]
                if(length(match) > 1) {
                    warning('Project ', x, ' matched multiple folders')
                    match <- NA
                }
                match
            })
        }
        # are we done? yay!
        toDo <- is.na(projMatch)
        if(!any(toDo)) {
            break
        }
        # if not, try to be smart about which subdirs to search next
        partialMatch <- sapply(project[toDo], function(x) {
            # parks aus does not follow same naming strucutre in
            # proj names and subfoldering
            if(grepl('PARKSAUSTRALIA', x) &&
               'PARKS_AUSTRALIA' %in% subBase) {
                return('PARKS_AUSTRALIA')
            }
            hasSub <- sapply(subBase, function(y) grepl(y, x))
            if(any(hasSub)) {
                return(subBase[hasSub][1])
            }
            NA
        })
        # if we found partials for all we can subset
        # otherwise gotta subdir all
        if(!anyNA(partialMatch)) {
            curDir <- subDirs[subBase %in% partialMatch]
        } else {
            curDir <- subDirs
        }
        # QAQC side of parks is org'd differently, not just within
        # the base folder, within base/rec perf
        isParksAus <- subBase == 'PARKS_AUSTRALIA'
        if(any(isParksAus) &&
           grepl('PROJECT_ADMIN_ACCDATA', curDir[isParksAus])) {
            curDir[isParksAus] <- paste0(curDir[isParksAus], '/Recorder Performance')
        }

        levels <- levels - 1
        if(levels == 0) {
            if(verbose) {
                cat('Did not find matching folder.\n')
            }
            break
        }
        if(verbose) {
            cat('Listing subfolders for ', length(curDir), ' folders...\n')
        }
        subDirs <- unlist(lapply(curDir, function(x) list.dirs(x, recursive=FALSE, full.names=TRUE)))
    }
    projMatch <- gsub(dir, '', projMatch)
    projMatch <- gsub('^/', '', projMatch)
    projMatch
}

parseDeviceId <- function(x) {
    x <- tolower(x)
    if(!grepl('soundtrap', x)) {
        return(NA)
    }
    x <- gsub(' ', '', x)
    x <- strsplit(x, '-')[[1]]
    if(length(x) != 2) {
        return(NA)
    }
    x <- x[2]
    x <- gsub('\\(copy\\)', '', x)
    x
}

addNefscDirs <- function(log, recBase, qaqcBase,levels=4, verbose=TRUE) {
    # only try to add dirs if they are missing or if base is same
    # e.g. don't try if new BOTTOM_MOUNTED but old was not
    if(is.null(log) || nrow(log) == 0) {
        warning('Log data is empty! Check project name spelling if you tried to subset.')
        return(log)
    }
    pToChange <- sapply(log$projectBaseDir, function(x) {
        is.na(x) || (basename(x) == basename(recBase))
    })
    qToChange <- sapply(log$qaqcBaseDir, function(x) {
        is.na(x) || (basename(x) == basename(qaqcBase))
    })
    log$projectBaseDir[pToChange] <- recBase
    log$qaqcBaseDir[qToChange] <- qaqcBase
    # check which projects are supposed to have data
    hasData <- log$qaqcStatus != 'NoData'
    # check which do not yet have existing directory
    # noProjLog <- is.na(log$projectDir) | !dir.exists(log$projectDir)
    noProjLog <- sapply(file.path(log$projectBaseDir, log$projectDir), function(x) is.na(x) || !dir.exists(x))
    noQaqcLog <- sapply(file.path(log$qaqcBaseDir, log$qaqcDir), function(x) is.na(x) || !dir.exists(x))
    # noQaqcLog <- is.na(log$qaqcDir) | !dir.exists(log$qaqcDir)
    projCheck <- hasData & noProjLog
    qaqcCheck <- hasData & noQaqcLog
    if(any(projCheck)) {
        log$projectDir[projCheck] <- mapProjectDir(log$projectName[projCheck],
                                                   dir=recBase,
                                                   levels=levels,
                                                   verbose=verbose)
    } else {
        cat('All project directories already entered!\n')
    }
    if(any(qaqcCheck)) {
        log$qaqcDir[qaqcCheck] <- mapProjectDir(log$projectName[qaqcCheck],
                                                dir=qaqcBase,
                                                levels=levels,
                                                verbose=verbose)
    } else {
        cat('All QAQC directores already entered!\n')
    }
    projBad <- is.na(log$projectDir[projCheck])
    qaqcBad <- is.na(log$qaqcDir[qaqcCheck])
    if(any(projBad)) {
        warning('Could not find wav folders for ',
                sum(projBad),
                ' projects:\n',
                paste0(log$projectName[projCheck][projBad], collapse=', '))
    }
    if(any(qaqcBad)) {
        warning('Could not find QAQC folders for ',
                sum(qaqcBad),
                ' projects:\n',
                paste0(log$projectName[qaqcCheck][qaqcBad], collapse=', '),
                '\nThese must be created manually.')
    }
    calChecked <- 0
    calFound <- 0
    for(i in 1:nrow(log)) {
        if(!dir.exists(file.path(log$qaqcBaseDir[i], log$qaqcDir[i]))) {
            next
        }
        if(!is.na(log$calibration[i]) &&
           file.exists(log$calibration[i])) {
            next
        }
        # dont check for calibration unless we might run it
        if(!log$qaqcStatus[i] %in% c('NoQAQC', 'TimeChecked', 'ClipOnly')) {
            next
        }
        log$calibration[i] <- findCalibration(log$qaqcDir[i])
        calChecked <- calChecked + 1
        calFound <- calFound + !is.na(log$calibration[i])
    }
    cat('Found', calFound, 'calibration files in', calChecked, 'project folders\n')
    log
}

readPaUpload <- function(x) {
    if(!file.exists(x)) {
        stop('PA Data Upload file does not exist')
    }
    if(grepl('csv$', x)) {
        status <- read.csv(x, skip=4, stringsAsFactors = FALSE, na.strings=c('NA', ''))
    } else if(grepl('xlsx{0,1}$', x)) {
        sheetList <- readxl::excel_sheets(x)
        if('PA Data - SoundTrap, HARU, HARP' %in% sheetList) {
            return(readPaUploadSmartsheets(x))
        }
        status <- data.frame(readxl::read_excel(x, skip=4))
        # status[[1]][is.na(status[[1]])] <- ''
    }

    statHeaders <- seq(from=1, to=nrow(status), by=8)
    statHeaders <- statHeaders[!is.na(status[[1]][statHeaders])]
    status <- split(status[1:(tail(statHeaders, 1)+7), ], rep(statHeaders, each=8))
    status <- bind_rows(
        lapply(
            status, function(x) {
                proj <- list(
                    projectName = x[1, 3],
                    deviceName = x[1, 1],
                    deploymentMakara = x[3, 4],
                    recoveryMakara = x[4, 4],
                    tempUpload = x[5, 4],
                    dataExtracted = x[6, 4],
                    TOLComplete = x[7, 4],
                    useableMakara = x[8, 4]
                )
                proj$deviceId <- parseDeviceId(proj$deviceName)
                proj
            }
        )
    )
}

readPaUploadSmartsheets <- function(x) {
    status <- read_excel(x, sheet = 1)
    status <- status[status$Status != 'Done', ]
    status <- list(
        projectName = status[['Project Name']],
        deviceName = status[['Item']],
        deploymentMakara = status[['Status: Deployment data entered into Makara']],
        recoveryMakara = status[['Status: Recovery data entered into Makara']],
        tempUpload = status[['Status: Temperature logger data uploaded']],
        dataExtracted = status[['Status: Data files extracted on Stellwagen server']],
        TOLComplete = status[['Status: (QAQC #1) TOL completed']],
        useableMakara = status[['Status: (QAQC #2) Usable dates entered in Makara']],
        usableStartTime = status[['Usable Data Timeline - Start Time']],
        usableEndTime = status[['Usable Data Timeline - End Time']],
        usableStartDate = status[['Usable Data Timeline - Start Date']],
        usableEndDate = status[['Usable Data Timeline - End Date']]
    )
    hasTime <- !is.na(status$usableStartTime) &
        !is.na(status$usableEndTime) &
        !is.na(status$usableStartDate) &
        !is.na(status$usableEndDate)
    status$usableStart <- NA
    status$usableEnd <- NA
    status$usableStart[hasTime] <- paste0(status$usableStartDate[hasTime],
                                          'T',
                                          status$usableStartTime[hasTime],
                                          'Z')
    status$usableEnd[hasTime] <- paste0(status$usableEndDate[hasTime],
                                        'T',
                                        status$usableEndTime[hasTime],
                                        'Z')
    status$deviceId <- sapply(status$deviceName, parseDeviceId)
    for(c in c('deploymentMakara', 'recoveryMakara')) {
        # do I need to change certian text to NA? i dont think so
    }
    data.frame(status)
}

readRecPerf <- function(x) {
    if(!file.exists(x)) {
        stop('Recorder Performance file does not exist')
    }
    if(grepl('Instrument Tracking', x)) {
        recorder <- data.frame(readxl::read_excel(x, sheet=1))
        recorder <- recorder[c('Item.ID', 'Sensitivity')]
        names(recorder) <- c('deviceId', 'sensitivity')
        recorder <- recorder[!is.na(recorder$sensitivity), ]
        recorder$deviceId <- as.character(round(as.numeric(recorder$deviceId), 0))
        # recorder <- distinct(recorder)
    } else if(grepl('csv$', x)) {
        recorder <- read.csv(x, skip=4, stringsAsFactors = FALSE, na.strings=c('NA', ''))
        recorder <- recorder[c('Serial.Number', 'Sensitivity')]
    } else if(grepl('xlsx{0,1}$', x)) {
        recorder <- data.frame(readxl::read_excel(x, skip=4, .name_repair = 'minimal'))
        recorder <- recorder[c('Serial.Number', 'Sensitivity')]
    }
    names(recorder) <- c('deviceId', 'sensitivity')
    recorder$deviceId <- as.character(recorder$deviceId)
    recorder$sensitivity <- as.numeric(recorder$sensitivity)
    recorder$sensitivity <- recorder$sensitivity * -1
    distinct(recorder)
}

readDataQaqc <- function(x) {
    if(!file.exists(x)) {
        stop('QAQC Data spreadsheet does not exist')
    }
    sheets <- readxl::excel_sheets(x)
    result <- vector('list', length=length(sheets))
    names(result) <- sheets
    for(s in sheets) {
        thisSheet <- readxl::read_excel(x, sheet=s, skip=5, .name_repair = 'minimal', col_types='list')
        startCol <- grep('USABLE_START', names(thisSheet), value=TRUE)
        endCol <- grep('USABLE_END', names(thisSheet), value=TRUE)
        projCol <- grep('PROJECT NAME', names(thisSheet), value=TRUE)
        if(length(startCol) == 0 ||
           length(endCol) == 0 ||
           length(projCol) == 0) {
            next
        }
        thisSheet <- thisSheet[c(projCol, startCol, endCol)]
        # thisSheet[[2]] <- as.character(thisSheet[[2]])
        # thisSheet[[3]] <- as.character(thisSheet[[3]])
        names(thisSheet) <- c('projectName', 'usableStart', 'usableEnd')
        thisSheet[['projectName']] <- unlist(thisSheet[['projectName']])
        for(col in c('usableStart', 'usableEnd')) {
            timeVals <- unlist(thisSheet[[col]])
            for(i in seq_along(thisSheet[[col]])) {
                if(inherits(thisSheet[[col]][[i]], 'POSIXct')) {
                    timeVals[i] <- psxTo8601(thisSheet[[col]][[i]])
                }
            }
            thisSheet[[col]] <- timeVals
        }
        result[[s]] <- thisSheet
    }
    result <- bind_rows(result)
    result
}

nefscMondayToLog <- function(dataUpload, recPerf=NULL, qaqcSheet=NULL) {
    status <- readPaUpload(dataUpload)
    if(!is.null(recPerf)) {
        recorder <- readRecPerf(recPerf)
        status <- left_join(status, recorder, by='deviceId')
        noSens <- is.na(status$sensitivity)
        if(any(noSens)) {
            warning('\nCould not find matching sensitivity values for ', sum(noSens),
                    ' recording devices:\n',
                    paste0(status$deviceName[noSens], collapse=', '))
        }
    } else {
        status$sensitivity <- NA
        warning('No recorder information provided, all sensitivity values are NA')
    }
    if(!is.null(qaqcSheet)) {
        qSheet <- readDataQaqc(qaqcSheet)
        status <- left_join(status, qSheet, by='projectName')
    } else if(!all(c('usableStart', 'usableEnd') %in% names(status))) {
        status$usableStart <- NA
        status$usableEnd <- NA
    }
    noData <- tolower(status$dataExtracted) %in% c(NA, 'not applicable')
    noData[!is.na(status$usableStart) & !is.na(status$usableEnd)] <- FALSE
    noStart <- !noData & is.na(status$usableStart)
    noEnd <- !noData & is.na(status$usableEnd)
    if(any(noStart | noEnd)) {
        noClipProj <- unique(status$projectName[noStart | noEnd])
        warning(sum(noStart), ' Start and ',
                sum(noEnd), ' End times were missing for project(s):\n',
                paste0(noClipProj, collapse=', '))
    }
    # if its not NA and we cant parse it, need to fix
    badStart <- !is.na(status$usableStart) & is.na(parseQDate(status$usableStart))
    badEnd <- !is.na(status$usableEnd) & is.na(parseQDate(status$usableEnd))

    if(any(badStart | badEnd)) {
        warning(sum(badStart), ' Start and ',
                sum(badEnd), ' End times could not be automatically',
                ' parsed to datetimes, they must be manually changed to',
                ' YYYY-MM-DDTHH:MM:SSZ format')
    }
    notRun <- !noData & is.na(status$TOLComplete)
    isRun <- !notRun & status$TOLComplete == 'Done'
    status$qaqcStatus <- 'NoData'
    status$qaqcStatus[noData] <- 'NoData'
    status$qaqcStatus[notRun] <- 'NoQAQC'
    status$qaqcStatus[isRun] <- 'QAQCRun'

    log <- data.frame(
        projectBaseDir = NA,
        projectDir = NA,
        projectName = status$projectName,
        deviceName = status$deviceName,
        deviceId = status$deviceId,
        sensitivity = status$sensitivity,
        calibration = NA,
        usableStart = status$usableStart,
        usableEnd = status$usableEnd,
        qaqcStatus = status$qaqcStatus,
        qaqcBaseDir = NA,
        qaqcDir = NA
    )
    log
}

# this tries to parse dates from an excel sheet
parseQDate <- function(x) {
    if(length(x) > 1) {
        result <- lapply(x, parseQDate)
        result <- as.POSIXct(unlist(result), origin='1970-01-01 00:00:00', tz='UTC')
        return(result)
    }
    if(is.na(x)) {
        return(NA_POSIXct_)
    }
    if(x == '') {
        return(NA_POSIXct_)
    }
    x <- gsub('^\\s*', '', x)
    x <- gsub('\\s*$', '', x)
    if(x == '') {
        return(NA_POSIXct_)
    }
    tryNum <- suppressWarnings(as.numeric(x))
    if(!is.na(tryNum)) {
        # this is the excel numeric date - so dumb
        return(as.POSIXct((tryNum - 2)*3600*24, origin='1900-01-01', tz='UTC'))
    }
    result <- suppressWarnings(
        parse_date_time(x, orders=c('%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S'), tz='UTC')
    )
    if(is.na(result)) {
        return(NA_POSIXct_)
    }
    result
}

# just trying converting any .tf or .csv files to a calibration
findCalibration <- function(dir) {
    files <- list.files(dir, full.names=TRUE, recursive=FALSE)
    hasTf <- grepl('tf$', files)
    hasCsv <- grepl('csv$', files)
    possCal <- hasTf | hasCsv
    if(!any(possCal)) {
        return(NA)
    }
    for(f in files[possCal]) {
        tryCal <- try(checkCalibration(f), silent=TRUE)
        # if this is NULL or returned an error then it bad
        if(!is.null(tryCal) && !inherits(tryCal, 'try-error')) {
            return(f)
        }
    }
    NA
}

readIssueLog <- function(x) {
    if(dir.exists(x)) {
        files <- list.files(x, full.names=TRUE, recursive=FALSE)
        iLog <- grepl('QAQC_Issues\\.csv$', files)
        if(!any(iLog)) {
            warning('Could not find QAQC_Issues.csv in folder ', x)
            return(NULL)
        }
        x <- files[iLog][1]
    }
    if(!file.exists(x)) {
        stop('File ', x, ' does not exist')
    }
    data <- read.csv(x, stringsAsFactors = FALSE)
    issueCols <- c('UTC', 'file', 'value', 'projectName',
                   'source', 'comment', 'issueChecked', 'qaqcDate')
    if(!all(issueCols %in% names(data))) {
        warning(x, ' does not appear to be an issue log file')
        return(NULL)
    }
    data$UTC <- parse_date_time(data$UTC, orders=c('%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S'), tz='UTC')
    data$qaqcDate <- parse_date_time(data$qaqcDate, orders=c('%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S'), tz='UTC')
    data$value <- as.character(data$value)
    data
}

# FECK okay this is harder to keep track of
# what if we remove from existing we want to keep track
# of that
# but for all new we want to append all
saveIssueLog <- function(data, dir) {
    if(!dir.exists(dir)) {
        stop('Folder ', dir, ' does not exist')
    }
    outfile <- file.path(dir, 'QAQC_Issues.csv')
    old <- try(readIssueLog(dir), silent=TRUE)
    # if we didnt find any old data
    if(is.null(old) || inherits(old, 'try-error') || nrow(old) == 0) {
        data$UTC <- psxTo8601(data$UTC)
        data$qaqcDate <- psxTo8601(data$qaqcDate)
        write.csv(data, file=outfile, row.names=FALSE)
        return(invisible(outfile))
    }
    data <- bind_rows(old, data)
    data$UTC <- psxTo8601(data$UTC)
    data$qaqcDate <- psxTo8601(data$qaqcDate)
    data <- distinct(data)
    write.csv(data, file=outfile, row.names=FALSE)
    invisible(outfile)
}

# psxTo8601 <- function(x) {
#     if(is.character(x)) {
#         return(x)
#     }
#     format(x, format='%Y-%m-%dT%H:%M:%SZ')
# }

readQLog <- function(x) {
    if(dir.exists(x)) {
        files <- list.files(x, full.names=TRUE, recursive=FALSE)
        qlog <- grepl('QAQC_Log\\.csv$', files)
        if(!any(qlog)) {
            stop('Could not find QAQC_Log.csv in folder ', x)
        }
        x <- files[qlog][1]
    }
    if(!file.exists(x)) {
        stop('File ', x, ' does not exist')
    }
    data <- read.csv(x, stringsAsFactors = FALSE)
    if(!isQaqcLog(data)) {
        stop(x, ' does not appear to be a QAQC log file')
    }
    data$deviceId <- as.character(data$deviceId)
    data
}

# update tells this how to handle projects that are in both the
# old and new log data
# "new"  means only fill in missing data from the old columns,
#        and only update the QAQC status to one further down
#        the processing chain
# "all"  means fully replace the old data with the new data
# "none" means don't change any old data
saveQLog <- function(data, dir, update=c('new', 'all', 'none')) {
    if(!dir.exists(dir)) {
        stop('Folder ', dir, ' does not exist')
    }
    old <- try(readQLog(dir), silent=TRUE)
    # if we didnt find any old data
    outfile <- file.path(dir, 'QAQC_Log.csv')
    data$usableStart <- psxTo8601(data$usableStart)
    data$usableEnd <- psxTo8601(data$usableEnd)
    if(is.null(old) || inherits(old, 'try-error') || nrow(old) == 0) {
        write.csv(data, file=outfile, row.names=FALSE)
        return(invisible(outfile))
    }
    # make sure both are chars to combine later
    old$usableStart <- psxTo8601(old$usableStart)
    old$usableEnd <- psxTo8601(old$usableEnd)
    # setting up directory fixing
    oldP <- unique(old$projectBaseDir)
    oldP <- oldP[!is.na(oldP)]
    oldQ <- unique(old$qaqcBaseDir)
    oldQ <- oldQ[!is.na(oldQ)]
    newP <- unique(data$projectBaseDir)
    newP <- newP[!is.na(newP)]
    newP <- newP[dir.exists(newP)]
    newQ <- unique(data$qaqcBaseDir)
    newQ <- newQ[!is.na(newQ)]
    newQ <- newQ[dir.exists(newQ)]
    old$PROJIX <- paste0(old$projectName, '-', old$deviceId)
    data$PROJIX <- paste0(data$projectName, '-', data$deviceId)
    switch(match.arg(update),
           'new' = {
               oldInNew <- old$PROJIX[old$PROJIX %in% data$PROJIX]
               # possible columns we want to update - others are directory folders
               # that get handled with addNefscDir
               updateCols <- c('deviceName', 'deviceId', 'sensitivity',
                               'calibration', 'usableStart', 'usableEnd',
                               'qaqcDir', 'projectDir')
               # check if status is further along the line, others are NA
               projUpdated <- character(0)
               for(proj in oldInNew) {
                   oldIx <- which(old$PROJIX == proj)
                   newIx <- data$PROJIX == proj
                   for(c in updateCols) {
                       if(is.na(old[[c]][oldIx]) &&
                          !is.na(data[[c]][newIx])) {
                           old[[c]][oldIx] <- data[[c]][newIx]
                           projUpdated <- c(projUpdated, proj)
                       }
                   }
                   # only update status if it is further down the line -
                   # don't undo progress
                   oldStatus <- which(QAQC_STATUS == old$qaqcStatus[oldIx])
                   newStatus <- which(QAQC_STATUS == data$qaqcStatus[newIx])
                   if(newStatus > oldStatus) {
                       old$qaqcStatus[oldIx] <- data$qaqcStatus[newIx]
                       projUpdated <- c(projUpdated, proj)
                   }
               }
               nUpdated <- length(projUpdated)
               projUpdated <- unique(projUpdated)
               # remove duplicates bc we only want to update specific values
               newToDrop <- data$PROJIX %in% old$PROJIX
               data <- data[!newToDrop, ]
               cat('Updated', nUpdated, 'log entries in', length(projUpdated),
                   'different projects (update="new")\n')
           },
           'all' = {
               oldToDrop <- old$PROJIX %in% data$PROJIX
               old <- old[!oldToDrop, ]
               cat('Updated log entries for', sum(oldToDrop), 'projects (update="all")\n')
           },
           'none' = {
               newToDrop <- data$PROJIX %in% old$PROJIX
               data <- data[!newToDrop, ]
               cat('Did not update log entries for', sum(newToDrop), 'projects in',
                   'new log data (update="none")\n')
           }
    )
    data <- bind_rows(old, data)
    data$PROJIX <- NULL
    pDNE <- !dir.exists(oldP)
    if(any(pDNE)) {
        for(p in oldP[pDNE]) {
            newMatch <- basename(newP) == basename(p)
            if(any(newMatch)) {
                data$projectBaseDir[data$projectBaseDir == p] <- newP[newMatch][1]
            }
        }
    }
    qDNE <- !dir.exists(oldQ)
    if(any(qDNE)) {
        for(q in oldQ[qDNE]) {
            newMatch <- basename(newQ) == basename(q)
            if(any(newMatch)) {
                data$qaqcBaseDir[data$qaqcBaseDir == q] <- newQ[newMatch][1]
            }
        }
    }
    write.csv(data, file=outfile, row.names=FALSE)
    invisible(outfile)
}

QAQC_STATUS <- c('NoData', 'NoQAQC', 'TimeChecked', 'ClipOnly', 'QAQCRun', 'QAQCReviewed', 'QAQCComplete')

checkValidStatus <- function(x) {
    if(is.data.frame(x)) {
        x$qaqcStatus <- checkValidStatus(x$qaqcStatus)
        return(x)
    }
    isMatch <- x %in% QAQC_STATUS
    if(all(isMatch)) {
        return(x)
    }
    isLowMatch <- tolower(x) %in% tolower(QAQC_STATUS)
    if(all(isLowMatch)) {
        for(i in which(!isMatch & isLowMatch)) {
            x[i] <- QAQC_STATUS[tolower(QAQC_STATUS) == tolower(x[i])]
        }
        return(x)
    }
    badVals <- x[!isLowMatch]
    warning(length(badVals), ' "qaqcStatus" values did not match standard levels (',
            paste0(QAQC_STATUS, collapse=','),
            ')\nProjects with non-standard levels will be skipped')
    x
}

# runQAQCReview <- function(data, issue=NULL, freqLims=c(30, Inf)) {
#     # check input type
#     INCHAR <- is.character(data)
#     if(INCHAR && !file.exists(data)) {
#         stop('File or directory ', data, ' does not exist.')
#     }
#     INDIR <- INCHAR && dir.exists(data)
#     if(INCHAR) {
#         INPATH <- data
#         data <- readQLog(INPATH)
#         if(INDIR && is.null(issue)) {
#             issue <- readIssueLog(INPATH)
#         }
#     }
#     if(isQaqcLog(data)) {
#         INLOG <- TRUE
#     } else if(any(grepl('TOL_', colnames(data))) &&
#               all(c('UTC', 'wavLength') %in% colnames(data))) {
#         INLOG <- FALSE
#     } else {
#         stop('Unknown input - not QAQC log or single deployment output')
#     }
#     if(is.null(issue)) {
#         # template of issue log
#         issueLog <- data.frame(
#             UTC=NA,
#             file=NA, #wav file
#             value=NA, #freq for TOL, gap amt for GAP?
#             projectName=NA,
#             source=NA, #TOL, GAP
#             comment=NA, # box
#             issueChecked=NA,
#             qaqcDate=NA # keep track of when we marked this?
#         )
#         issue <- issueLog[FALSE, ]
#     }
#     ui <- fluidPage(
#         style = "padding: 0px;", # no gap in navbar
#         navbarPage(
#             id='main',
#             'QAQC Time',
#             # Data Page ####
#             tabPanel(
#                 'Home',
#                 DTOutput('inData'),
#                 checkboxGroupInput('statusCheck',
#                                    choices=QAQC_STATUS,
#                                    selected=c('QAQCRun', 'QAQCReviewed'),
#                                    label=NULL,
#                                    inline=TRUE),
#                 fluidRow(
#                     column(2,
#                            actionButton('loadSelected', label='Load Selected',
#                                         style='margin-top:24px;')
#                     ),
#                     column(4,
#                            downloadButton('downloadLog', label='Download Log',
#                                           style='margin-top:24px;')
#                     ),
#                     column(4,
#                            selectInput('statusValue', label='Change QAQC Status',
#                                        choices=QAQC_STATUS)
#                     ),
#                     column(2,
#                            actionButton('changeStatus', label='Change',
#                                         style='margin-top:24px;')
#                     )
#                 ),
#                 h4('Current QAQC directory'),
#                 verbatimTextOutput('qaqcDir'),
#                 textInput('changeQaqcDir', label='Change base directory', value='', width='100%'),
#                 fluidRow(
#                     column(10,
#                            actionButton('changeDirButton', label='Change')
#                     )
#                 )
#             ),
#
#             # Plot TOL ####
#             tabPanel(
#                 'TOL',
#                 plotOutput('tolPlot',
#                            brush = brushOpts(id = "tolBrush", fill = "#ccc", direction = "x")),
#                 sliderInput('timeSliderTol', label='Time Range',
#                             min=0, max=1, value=c(0, 1),
#                             width='100%'),
#                 checkboxGroupInput('tolFreqs', choices=NULL, label='Frequency to Show',
#                                    inline=TRUE),
#                 fluidRow(
#                     column(10,
#                            textAreaInput('tolComment', label='Comment', width='100%')
#                     ),
#                     column(2,
#                            actionButton('tolLogIssue', label='Log Issue', width='100%', height='100%',
#                                         style='font-weight:bold;margin-top:35px;')
#                     )
#                 ),
#                 verbatimTextOutput('tolBrushOut')
#             ),
#             # Plot Gaps ####
#             tabPanel(
#                 'Data Gaps',
#                 plotOutput('gapPlot',
#                            brush = brushOpts(id='gapBrush')),
#                 sliderInput('timeSliderGap', label='Time Range',
#                             min=0, max=1, value=c(0, 1),
#                             width='100%'),
#                 fluidRow(
#                     column(10,
#                            textAreaInput('gapComment', label='Comment', width='100%')
#                     ),
#                     column(2,
#                            actionButton('gapLogIssue', label='Log Issue', width='100%', height='100%',
#                                         style='font-weight:bold;margin-top:35px;')
#                     )
#                 ),
#                 verbatimTextOutput('gapBrushOut')
#             ),
#             # Plot TV ####
#             tabPanel(
#                 'Temp Volt',
#                 plotOutput('tvPlot',
#                            brush=brushOpts(id='tvBrush', direction='x')),
#                 fluidRow(
#                     column(10,
#                            textAreaInput('tvComment', label='Comment', width='100%')
#                     ),
#                     column(2,
#                            actionButton('tvLogIssue', label='Log Issue', width='100%', height='100%',
#                                         style='font-weight:bold;margin-top:35px;')
#                     )
#                 ),
#                 verbatimTextOutput('tvBrushOut')
#             ),
#             # Issue Log ####
#             tabPanel(
#                 'Issue Log',
#                 h4('Issue Log'),
#                 DTOutput('issueData'),
#                 fluidRow(
#                     column(3,
#                            actionButton('removeIssue',
#                                         label='Remove Selected Row(s)')
#                     ),
#                     column(4,
#                            downloadButton('downloadIssues', label='Download Issues')
#                     )
#                 ),
#                 fluidRow(column(3, em('(only rows added during this session will be successfully removed)')))
#             ),
#             # Stop Page ####
#             tabPanel(
#                 'Save and Exit',
#                 h4('Save any updates to Log & Issues before hitting "Stop App"'),
#                 fluidRow(
#                     column(2,
#                            actionButton('saveLog', label='Save Log Data')
#                     ),
#                     column(5,
#                            verbatimTextOutput('logSaveTime')
#                     )
#                 ),
#                 fluidRow(
#                     column(2,
#                            actionButton('saveIssues', label='Save Issues Data')
#                     ),
#                     column(5, verbatimTextOutput('issueSaveTime')
#                     )
#                 ),
#                 actionButton('stopApp', label='Stop App', style='font-weight:bold;')
#             )
#         )
#     )
#     server <- function(input, output, session) {
#
#         # Setup Reactives ####
#         if(isFALSE(INLOG)) {
#             appData <- reactiveValues(
#                 log=NULL,
#                 data=data,
#                 freqLevs=1,
#                 project='',
#                 issue=issue,
#                 rowLoaded=NULL,
#                 logSaveTime=NULL,
#                 issueSaveTime=NULL
#             )
#         } else {
#             if('QAQCRun' %in% data$qaqcStatus) {
#                 loadIx <- which(data$qaqcStatus == 'QAQCRun')[1]
#             } else if('QAQCReviewed' %in% data$qaqcStatus) {
#                 loadIx <- which(data$qaqcStatus == 'QAQCReviewed')[1]
#             } else if('QAQCComplete' %in% data$qaqcStatus) {
#                 loadIx <- which(data$qaqcStatus == 'QAQCComplete')[1]
#             } else {
#                 loadIx <- 1
#             }
#             appData <- reactiveValues(
#                 log=data,
#                 data=readQData(file.path(data$qaqcBaseDir[loadIx], data$qaqcDir[loadIx]),
#                                pattern=data$deviceId[loadIx]),
#                 freqLevs=1,
#                 project=data$projectName[loadIx],
#                 issue=issue,
#                 rowLoaded=loadIx,
#                 logSaveTime=NULL,
#                 issueSaveTime=NULL
#             )
#         }
#         observeEvent(appData$data, {
#             freqCols <- PAMscapes:::colsToFreqs(names(appData$data)[PAMscapes:::whichFreqCols(appData$data)])
#             freqCols <- freqCols[freqCols >= freqLims[1] & freqCols <= freqLims[2]]
#             olLevels <- PAMscapes:::getOctaveLevels('ol', freqRange=range(freqCols))
#             freqCols <- freqCols[freqCols %in% olLevels$freqs]
#             appData$freqLevs <- freqCols
#             updateSliderInput(inputId='timeSliderTol',
#                               min=min(appData$data$UTC),
#                               max=max(appData$data$UTC),
#                               value=range(appData$data$UTC),
#                               timeFormat='%F')
#             updateSliderInput(inputId='timeSliderGap',
#                               min=min(appData$data$UTC),
#                               max=max(appData$data$UTC),
#                               value=range(appData$data$UTC),
#                               timeFormat='%F')
#             freqText <- makeHtmlColors(freqCols)
#             updateCheckboxGroupInput(inputId='tolFreqs',
#                                      choiceNames=lapply(freqText, HTML),
#                                      choiceValues=freqCols,
#                                      selected = freqCols, inline = TRUE)
#         })
#         observeEvent(input$loadSelected, {
#             if(isFALSE(INLOG)) {
#                 showNotification('Button only active for QAQC logs')
#                 return(NULL)
#             }
#             logIx <- input$inData_rows_selected
#             if(is.null(logIx)) {
#                 showNotification('No row selected')
#                 return(NULL)
#             }
#             projLoad <- appData$log$projectName[
#                 appData$log$qaqcStatus %in% input$statusCheck
#             ][logIx]
#             projIx <- which(appData$log$projectName == projLoad)
#             appData$data <- readQData(
#                 file.path(appData$log$qaqcBaseDir[projIx],
#                           appData$log$qaqcDir[projIx]),
#                 pattern=appData$log$deviceId[projIx]
#             )
#             appData$project <- appData$log$projectName[projIx]
#             appData$rowLoaded <- projIx
#             showNotification('Data load complete!')
#         })
#         # Render Table ####
#         output$inData <- DT::renderDT({
#             if(INLOG) {
#                 out <- appData$log[c('projectName', 'qaqcStatus')]
#                 out <- out[out$qaqcStatus %in% input$statusCheck, ]
#             } else {
#                 out <- appData$log[c('UTC', 'file')]
#             }
#             out
#         },
#         selection=list(mode='single'),
#         options=list(dom='tip')
#         )
#         output$downloadLog <- downloadHandler(
#             filename = function() {
#                 'QAQC_Log.csv'
#             },
#             content = function(file) {
#                 write.csv(appData$log, file, row.names=FALSE)
#             }
#         )
#         observeEvent(input$stopApp, {
#             stopApp()
#         })
#         observeEvent(input$changeStatus, {
#             if(isFALSE(INLOG)) {
#                 showNotification('Only active for log data')
#                 return()
#             }
#             logIx <- input$inData_rows_selected
#             if(is.null(logIx)) {
#                 showNotification('No row selected')
#                 return()
#             }
#             projLoad <- appData$log$projectName[
#                 appData$log$qaqcStatus %in% input$statusCheck
#             ][logIx]
#             projIx <- which(appData$log$projectName == projLoad)
#             appData$log$qaqcStatus[projIx] <- input$statusValue
#         })
#         output$qaqcDir <- renderText({
#             text <- ''
#             if(isFALSE(INLOG)) {
#                 return(text)
#             }
#             if(is.null(appData$data)) {
#                 text <- paste0(text, 'No QAQC data found in directory:\n')
#             }
#             text <- paste0(text,
#                            'Project: ', appData$project)
#             text <- paste0(text,
#                            '\nBase: ', appData$log$qaqcBaseDir[appData$rowLoaded])
#             text <- paste0(text,
#                            '\nFolder: ', appData$log$qaqcDir[appData$rowLoaded])
#             text
#         })
#         observeEvent(input$changeDirButton, {
#             changeIx <- appData$log$qaqcBaseDir == appData$log$qaqcBaseDir[appData$rowLoaded]
#             appData$log$qaqcBaseDir[changeIx] <- input$changeQaqcDir
#             appData$data <- readQData(
#                 file.path(appData$log$qaqcBaseDir[appData$rowLoaded],
#                           appData$log$qaqcDir[appData$rowLoaded]),
#                 pattern=appData$log$deviceId[appData$rowLoaded]
#             )
#             showNotification('Base directory changed!')
#         })
#         # Plot - TOL ####
#         output$tolPlot <- renderPlot({
#             if(is.null(appData$data)) {
#                 plot(x=1, y=1, type='n')
#                 text(x=1, y=1, label='No data loaded')
#                 return()
#             }
#             plotData <- PAMscapes:::toLong(appData$data)
#             if(!is.null(input$tolFreqs)) {
#                 plotData <- dplyr::filter(plotData, frequency %in% input$tolFreqs)
#             }
#             if(inherits(input$timeSliderTol[1], 'POSIXct')) {
#                 plotData <- dplyr::filter(plotData, .data$UTC >= input$timeSliderTol[1] &
#                                               .data$UTC <= input$timeSliderTol[2])
#             }
#
#             tRange <- range(plotData$UTC)
#             plotLevels <- PAMscapes:::getOctaveLevels('ol', freqRange=range(plotData$frequency))
#             plotData <- plotData[plotData$frequency %in% plotLevels$freqs, ]
#             plotData$frequency <- factor(plotData$frequency, levels=appData$freqLevs)
#             ggplot(plotData, aes(x=UTC, y=value, color=frequency)) +
#                 geom_line() +
#                 scale_color_manual(values=scales::hue_pal()(length(appData$freqLevs)),
#                                    breaks=appData$freqLevs) +
#                 scale_x_datetime(limits=tRange, expand=c(0, 0)) +
#                 ggtitle(appData$project)
#         })
#         # Brush - TOL ####
#         output$tolBrushOut <- renderPrint({
#             if(is.null(appData$data)) {
#                 return('No data loaded')
#             }
#             plotData <- PAMscapes:::toLong(appData$data)
#             if(!is.null(input$tolFreqs)) {
#                 plotData <- dplyr::filter(plotData, frequency %in% input$tolFreqs)
#             }
#             plotLevels <- PAMscapes:::getOctaveLevels('ol', freqRange=range(plotData$frequency))
#             plotData <- plotData[plotData$frequency %in% plotLevels$freqs, ]
#             plotData$frequency <- factor(plotData$frequency, levels=appData$freqLevs)
#             brushData <- PAMscapes:::toWide(brushedPoints(plotData, brush=input$tolBrush))
#             # if(nrow(brushData) > 0) {
#             #     brushFreqs <- names(brushData)[PAMscapes:::whichFreqCols(brushData)]
#             # } else {
#             #     brushFreqs <- NULL
#             # }
#             brushData <- brushData[c('UTC', 'file')]
#             if(nrow(brushData) > 0) {
#                 brushData <- brushData[brushData$UTC %in% range(brushData$UTC), ]
#             }
#             freqVals <- paste0(input$tolFreqs, collapse=',')
#             brushData$frequency <- rep(freqVals, nrow(brushData))
#             brushData$comment <- rep(input$tolComment, nrow(brushData))
#             brushData
#         })
#         observeEvent(input$tolLogIssue, {
#             plotData <- PAMscapes:::toLong(appData$data)
#             if(!is.null(input$tolFreqs)) {
#                 plotData <- dplyr::filter(plotData, frequency %in% input$tolFreqs)
#             }
#             plotLevels <- PAMscapes:::getOctaveLevels('ol', freqRange=range(plotData$frequency))
#             plotData <- plotData[plotData$frequency %in% plotLevels$freqs, ]
#             plotData$frequency <- factor(plotData$frequency, levels=appData$freqLevs)
#             brushData <- PAMscapes:::toWide(brushedPoints(plotData, brush=input$tolBrush))
#             if(nrow(brushData) == 0) {
#                 showNotification('No data selected.')
#                 return(NULL)
#             }
#             # if(nrow(brushData) > 0) {
#             #     brushFreqs <- names(brushData)[PAMscapes:::whichFreqCols(brushData)]
#             # } else {
#             #     brushFreqs <- NULL
#             # }
#             brushData <- brushData[c('UTC', 'file')]
#             brushData <- brushData[brushData$UTC %in% range(brushData$UTC), ]
#             freqVals <- paste0(input$tolFreqs, collapse=',')
#             brushData$value <- rep(freqVals, nrow(brushData))
#             brushData$projectName <- appData$project
#             brushData$source <- 'TOL'
#             brushData$comment <- rep(input$tolComment, nrow(brushData))
#             brushData$issueChecked <- 'no'
#             nowUTC <- Sys.time()
#             attr(nowUTC, 'tzone') <- 'UTC'
#             brushData$qaqcDate <- nowUTC
#             if(nrow(appData$issue) == 0) {
#                 appData$issue <- brushData
#             } else {
#                 appData$issue <- bind_rows(appData$issue, brushData)
#             }
#             showNotification('Issue Logged')
#         })
#         # Plot - Gap ####
#         output$gapPlot <- renderPlot({
#             if(is.null(appData$data)) {
#                 plot(x=1, y=1, type='n')
#                 text(x=1, y=1, label='No data loaded')
#                 return()
#             }
#             gapData <- appData$data[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
#             names(gapData)[3:4] <- c('Wav End to Next File (s)',
#                                      'Time Between File Start (s)')
#             gapData <- tidyr::pivot_longer(gapData, cols=c('Wav End to Next File (s)',
#                                                            'Time Between File Start (s)'))
#             if(inherits(input$timeSliderGap[1], 'POSIXct')) {
#                 gapData <- dplyr::filter(gapData, .data$UTC >= input$timeSliderGap[1] &
#                                              .data$UTC <= input$timeSliderGap[2])
#             }
#             # units opts?
#             ggplot(gapData, aes(x=UTC, y=.data$value)) +
#                 geom_line() +
#                 facet_wrap(~name, scales='free', ncol=1) +
#                 theme(strip.text.x = element_text(size=12)) +
#                 ggtitle(appData$project)
#         })
#         # Brush - Gap ####
#         output$gapBrushOut <- renderPrint({
#             if(is.null(appData$data)) {
#                 return('No data loaded')
#             }
#
#             gapData <- appData$data[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
#             names(gapData)[3:4] <- c('Wav End to Next File (s)',
#                                      'Time Between File Start (s)')
#             gapData <- tidyr::pivot_longer(gapData, cols=c('Wav End to Next File (s)',
#                                                            'Time Between File Start (s)'))
#             gapBrushData <- brushedPoints(gapData, brush=input$gapBrush, xvar='UTC', yvar='value')
#             gapBrushData$comment <- rep(input$gapComment, nrow(gapBrushData))
#             gapBrushData
#         })
#         observeEvent(input$gapLogIssue, {
#             gapData <- appData$data[c('UTC', 'file', 'diffBetweenLength', 'timeToNext')]
#             names(gapData)[3:4] <- c('Wav End to Next File (s)',
#                                      'Time Between File Start (s)')
#             gapData <- tidyr::pivot_longer(gapData, cols=c('Wav End to Next File (s)',
#                                                            'Time Between File Start (s)'))
#             gapBrushData <- brushedPoints(gapData, brush=input$gapBrush, xvar='UTC', yvar='value')
#             if(nrow(gapBrushData) == 0) {
#                 showNotification('No data selected.')
#                 return(NULL)
#             }
#             gapBrushData <- gapBrushData[c('UTC', 'file', 'value', 'name')]
#             names(gapBrushData) <- c('UTC', 'file', 'value', 'source')
#             gapBrushData$value <- as.character(round(gapBrushData$value, 3))
#             gapBrushData$projectName <- appData$project
#             gapBrushData$source <- paste0('GapPlot-', gapBrushData$source)
#             gapBrushData$comment <- rep(input$gapComment, nrow(gapBrushData))
#             gapBrushData$issueChecked <- 'no'
#             nowUTC <- Sys.time()
#             attr(nowUTC, 'tzone') <- 'UTC'
#             gapBrushData$qaqcDate <- nowUTC
#             gapBrushData <- gapBrushData[
#                 c('UTC', 'file', 'value', 'projectName', 'source', 'comment', 'issueChecked', 'qaqcDate')]
#             if(nrow(appData$issue) == 0) {
#                 appData$issue <- gapBrushData
#             } else {
#                 appData$issue <- bind_rows(appData$issue, gapBrushData)
#             }
#             showNotification('Issue Logged')
#         })
#         # Plot - TV ####
#         output$tvPlot <- renderPlot({
#             if(is.null(appData$data)) {
#                 plot(x=1, y=1, type='n')
#                 text(x=1, y=1, label='No data loaded')
#                 return()
#             }
#             if(!all(c('intBatt', 'temp') %in% colnames(appData$data))) {
#                 plot(x=1, y=1, type='n')
#                 text(x=1, y=1, label='No temperature / voltage data found')
#                 return()
#             }
#             tvCols <- c('UTC', 'intBatt', 'extBatt', 'temp')
#             tvCols <- tvCols[tvCols %in% names(appData$data)]
#             tvData <- appData$data[tvCols]
#             plotQAQCTV(tvData, title=appData$projectName)
#         })
#         # Brush - TV ####
#         output$tvBrushOut <- renderPrint({
#             if(is.null(appData$data)) {
#                 return('No data loaded')
#             }
#             if(!all(c('intBatt', 'temp') %in% colnames(appData$data))) {
#                 return('No temperature / voltage data found')
#             }
#             tvCols <- c('UTC', 'file', 'intBatt', 'extBatt', 'temp')
#             tvCols <- tvCols[tvCols %in% names(appData$data)]
#             tvData <- appData$data[tvCols]
#             tvBrushData <- brushedPoints(tvData, brush=input$tvBrush, xvar='UTC', yvar='temp')
#             if(nrow(tvBrushData) > 0) {
#                 tvBrushData <- tvBrushData[tvBrushData$UTC %in% range(tvBrushData$UTC), ]
#             }
#             tvBrushData
#         })
#         observeEvent(input$tvLogIssue, {
#             if(is.null(appData$data)) {
#                 return()
#             }
#             if(!all(c('intBatt', 'temp') %in% colnames(appData$data))) {
#                 return()
#             }
#             tvCols <- c('UTC', 'file', 'intBatt', 'extBatt', 'temp')
#             tvCols <- tvCols[tvCols %in% names(appData$data)]
#             tvData <- appData$data[tvCols]
#             tvBrushData <- brushedPoints(tvData, brush=input$tvBrush, xvar='UTC', yvar='temp')
#             if(nrow(tvBrushData) == 0) {
#                 showNotification('No data selected')
#                 return()
#             }
#             tvBrushData <- tvBrushData[tvBrushData$UTC %in% range(tvBrushData$UTC), ]
#             tvVals <- character(nrow(tvBrushData))
#             if('intBatt' %in% names(tvBrushData)) {
#                 intVals <- paste0('IntBatt: ', tvBrushData[['intBatt']])
#                 tvVals <- paste(tvVals, intVals)
#             }
#             if('extBatt' %in% names(tvBrushData)) {
#                 extVals <- paste0('ExtBatt: ', tvBrushData[['extBatt']])
#                 tvVals <- paste(tvVals, extVals)
#             }
#             if('temp' %in% names(tvBrushData)) {
#                 tempVals <- paste0('Temp: ', tvBrushData[['temp']])
#                 tvVals <- paste(tvVals, tempVals)
#             }
#             tvBrushData <- tvBrushData[c('UTC', 'file')]
#             tvBrushData$value <- tvVals
#             tvBrushData$projectName <- appData$project
#             tvBrushData$source <- 'TempVoltPlot'
#             tvBrushData$comment <- rep(input$tvComment, nrow(tvBrushData))
#             tvBrushData$issueChecked <- 'no'
#             nowUTC <- Sys.time()
#             attr(nowUTC, 'tzone') <- 'UTC'
#             tvBrushData$qaqcDate <- nowUTC
#             if(nrow(appData$issue) == 0) {
#                 appData$issue <- tvBrushData
#             } else {
#                 appData$issue <- bind_rows(appData$issue, tvBrushData)
#             }
#             showNotification('Issue Logged')
#
#         })
#         # Issue Log ####
#         output$issueData <- renderDT({
#             arrange(appData$issue, appData$issue$qaqcDate)
#         },
#         server=FALSE,
#         options=list(dom='rtip',
#                      order=list(8, 'desc'))
#         )
#         observeEvent(input$removeIssue, {
#             dropIx <- input$issueData_rows_selected
#             if(is.null(dropIx)) {
#                 showNotification('No rows selected')
#                 return(NULL)
#             }
#             appData$issue <- appData$issue[-dropIx, ]
#         })
#         output$downloadIssues <- downloadHandler(
#             filename = function() {
#                 'QAQC_Issues.csv'
#             },
#             content = function(file) {
#                 write.csv(appData$issue, file, row.names=FALSE)
#             }
#         )
#         # Stop Page ####
#         observeEvent(input$saveLog, {
#             if(isFALSE(INDIR)) {
#                 showNotification('Saving only works when input is QAQC folder.')
#                 showNotification('Use Download button on "Home" page instead.')
#                 return()
#             }
#             saveQLog(appData$log, dir=INPATH, update='new')
#             appData$saveLogTime <- Sys.time()
#         })
#         observeEvent(input$saveIssues, {
#             if(isFALSE(INDIR)) {
#                 showNotification('Saving only works when input is QAQC folder.')
#                 showNotification('Use Download button on "Issues" page instead.')
#                 return()
#             }
#             saveIssueLog(appData$issue, dir=INPATH)
#             appData$saveIssueTime <- Sys.time()
#         })
#         output$logSaveTime <- renderText({
#             if(is.null(appData$saveLogTime)) {
#                 out <- 'Log has not been saved this session'
#             } else {
#                 out <- paste0('Log last saved ', appData$saveLogTime)
#             }
#             out
#         })
#         output$issueSaveTime <- renderText({
#             if(is.null(appData$saveIssueTime)) {
#                 out <- 'Issues have not been saved this session'
#             } else {
#                 out <- paste0('Issues last saved ', appData$saveIssueTime)
#             }
#             out
#         })
#     }
#
#     runApp(shinyApp(ui=ui, server=server))
# }

# makeHtmlColors <- function(values) {
#     cols <- hue_pal()(length(values))
#     text <- paste0('<div style="display:flex"><div style="color:', cols, ';font-weight:bold;">', values,'</div></div>')
#     text
# }