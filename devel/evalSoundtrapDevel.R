library(tuneR)
library(dplyr)
library(fftw)
library(PAMmisc)
library(ggplot2)
evaluateSoundtrapPerformance <- function(dir, sampleWindow=c(60, 60),
                                         octave=c('tol', 'ol'),
                                         plot=TRUE, channel=1, freqRange=NULL,
                                         cal=-174.2,
                                         n=100,
                                         progress=TRUE) {
    wavFiles <- list.files(dir, pattern='wav$', full.names=TRUE)
    if(n < length(wavFiles)) {
        wavFiles <- wavFiles[1:n]
    }
    # need to parse wav file times and track
    octaves <- PAMscapes:::getOctaveLevels(match.arg(octave), freqRange=freqRange)
    if(progress) {
        pb <- txtProgressBar(min=0, max=length(wavFiles), style=3)
        ix <- 0
    }
    tol <- lapply(wavFiles, function(x) {
        if(packageVersion('PAMmisc') >= '1.12.2') {
            wavClip <- PAMmisc::fastReadWave(x, from=sampleWindow[1], to=sum(sampleWindow))
            nfft <- wavClip$rate
        } else {
            wavClip <- tuneR::readWave(x, from=sampleWindow[1], to=sum(sampleWindow), units='seconds', toWaveMC = TRUE)
            nfft <- wavClip@samp.rate
        }
        welch <- pwelch(wavClip, nfft=nfft, noverlap=0, demean='long', channel=1)
        tolBins <- cut(welch$freq, octaves$limits, octaves$labels)
        tolVals <- lapply(split(welch$spec, tolBins), function(p) {
            10*log10(sum(p)) - cal
        })
        time <- wavToTime(x)
        tolVals$UTC <- time
        if(progress) {
            ix <<- ix + 1
            setTxtProgressBar(pb, value=ix)
        }
        tolVals
    })
    tol <- bind_rows(tol)
    tol$file <- basename(wavFiles)
    tolLong <- PAMscapes:::toLong(tol)
    if(plot) {
        tolLong$frequency <- factor(tolLong$frequency)
        tolPlot <- ggplot(tolLong, aes(x=UTC, y=value, color=frequency)) +
            geom_line()
        print(tolPlot)
    }
    tol
}

wavToTime <- function(x) {
    x <- basename(x)
    format <- c('pamguard', 'pampal', 'soundtrap', 'sm3', 'icListens1', 'icListens2')
    for(f in format) {
        switch(
            f,
            'pamguard' = {
                date <- gsub('.*([0-9]{8}_[0-9]{6}_[0-9]{3})\\.wav$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 15), tz = 'UTC', format = '%Y%m%d_%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 17, 19)) / 1e3
                if(!is.na(posix)) {
                    # FOUNDFORMAT <<- f
                    break
                }
            },
            'pampal' = {
                date <- gsub('.*([0-9]{14}_[0-9]{3})\\.wav$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 14), tz = 'UTC', format = '%Y%m%d%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 16, 18)) / 1e3
                if(!is.na(posix)) {
                    # FOUNDFORMAT <<- f
                    break
                }
            },
            'soundtrap' = {
                date <- gsub('.*\\.([0-9]{12})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    # FOUNDFORMAT <<- f
                    break
                }
            },
            'sm3' = {
                date <- gsub('.*\\_([0-9]{8}_[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d_%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    # FOUNDFORMAT <<- f
                    break
                }
            },
            'icListens1' = {
                date <- gsub('.*_([0-9]{8}-[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    # FOUNDFORMAT <<- f
                    break
                }
            },
            'icListens2' = {
                date <- gsub('.*_([0-9]{6}-[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    # FOUNDFORMAT <<- f
                    break
                }
            }
        )
    }
    posix + millis
}
