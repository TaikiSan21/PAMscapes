library(ncdf4)
library(dplyr)
library(lubridate)
library(PAMscapes)
addDataQuality <- function(ncFile, 
                           annotation=NULL, 
                           freqRange=NULL, 
                           timeRange=NULL, 
                           quality=2, 
                           updateUnknown=FALSE, # change 2s to something else
                           verbose=FALSE,
                           test=TRUE,
                           reset=FALSE) {
    nc <- nc_open(ncFile, write=TRUE)
    on.exit(nc_close(nc))
    if(!'quality_flag' %in% names(nc$var)) {
        if(verbose) {
            cat('No "quality_flag" variable found, creating a new one')
        }
        dataColOptions <- c('psd', 'sound_pressure_levels')
        hasData <- names(nc$var) %in% dataColOptions
        if(!any(hasData)) {
            stop('NetCDF file format not recognized, missing expected',
                 ' "psd" or "sound_pressure_levels" variable.')
        }
        if(sum(hasData) > 1) {
            warning('Multiple data columns matched, defaulting to "psd"')
            dataCol <- 'psd'
        } else {
            dataCol <- names(nc$var)[hasData]
        }
        inSize <- nc$var[[dataCol]]$size
        dqVar <- ncvar_def('quality_flag', prec='byte', longname='Data quality flag', dim=nc$var[[dataCol]]$dim, units='')
        nc <- ncvar_add(nc, dqVar)
        dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
    } else {
        dqMat <- ncvar_get(nc, 'quality_flag', start=c(1, 1), count=c(-1, -1))
    }
    if(reset) {
        dqMat[,] <- 2
    }
    checkStandard <- ncatt_get(nc, 'quality_flag', 'standard_name')
    if(isFALSE(checkStandard$hasatt)) {
        ncatt_put(nc, 'quality_flag', attname='standard_name', attval='quality_flag')
    }
    checkComment <- ncatt_get(nc, 'quality_flag', 'comment')
    if(isFALSE(checkComment$hasatt)) {
        ncatt_put(nc, 'quality_flag', attname='comment', attval='1 = Good, 2 = Not evaluated/Unknown, 3 =  Compromised/Questionable , 4 = Unusable / Bad')
    }
    checkContentType <- ncatt_get(nc, 'quality_flag', 'coverage_content_type')
    if(isFALSE(checkContentType$hasatt)) {
        ncatt_put(nc, 'quality_flag', attname='coverage_content_type', attval='qualityInformation')
    }
    times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    type <- PAMscapes:::checkFreqType(freqs)
    dqAnno <- formatDQAnnotation(x=annotation,
                                 freqRange=freqRange,
                                 timeRange=timeRange, 
                                 quality=quality
    )
    if(!is.null(dqAnno)) {
        dqMat <- PAMscapes:::markDQMatrix(dqMat, 
                              freqRange=dqAnno$freqRange,
                              timeRange=dqAnno$timeRange, 
                              value=dqAnno$quality,
                              times=times, 
                              freqs=freqs)
    }
    if(!isFALSE(updateUnknown) &&
       !updateUnknown %in% c(1,3,4)) {
        warning('updateUnknown must be a quality flag of 1, 3, or 4')
        updateUnknown <- FALSE
    }
    if(!isFALSE(updateUnknown)) {
        dqMat[dqMat == 2] <- updateUnknown
    }
    if(test) {
        return(dqMat)
    }
    ncvar_put(nc, varid='quality_flag', vals=dqMat)
    pamscapesNote <- ncatt_get(nc, varid=0, attname='pamscapes_note')
    if(isTRUE(pamscapesNote$hasatt)) {
        # note already exists
    }
    ncatt_put(nc, 
              varid=0, 
              attname='pamscapes_note', 
              attval=paste0('Modified by PAMscapes R Package on ', Sys.time())
              )
    invisible(dqMat)
}

formatDQAnnotation <- function(x=NULL, freqRange=NULL, timeRange=NULL, quality=NULL) {
    if(is.null(x) &&
       is.null(freqRange) &&
       is.null(timeRange)) {
        return(NULL)
    }
    if(!is.null(x)) {
        result <- formatDQDf(x) 
        result <- dfToAnnoList(result)
        return(result)
    }
    list('freqRange'=freqRange,
         'timeRange'=timeRange,
         'quality'=as.numeric(quality))
}

formatDQDf <- function(x) {
    makaraMap <- list('recording_interval_start_datetime' = 'start',
                      'recording_interval_end_datetime' = 'end',
                      'recording_interval_min_frequency_khz' = 'freqMin',
                      'recording_interval_max_frequency_khz' = 'freqMax',
                      'recording_interval_quality_type_code' = 'quality')
    if(all(names(makaraMap) %in% names(x))) {
        names(x) <- myRenamer(names(x), makaraMap)
        codeMap <- list('COMPROMISED' = 3,
                        'UNUSABLE' = 4)
        x$freqMin <- x$freqMin * 1000
        x$freqMax <- x$freqMax * 1000
        x$quality <- myRenamer(x$quality, codeMap)
        # x <- x[unlist(makaraMap)]
        # return(x)
    }
    hasTime <- all(c('start', 'end') %in% names(x))
    hasFreq <- all(c('freqMin', 'freqMax') %in% names(x))
    if(!hasTime && !hasFreq) {
        warning('Annotation input does not have time columns "start" and "end"',
                ' or frequency columns "freqMin" and "freqMax"')
    }
    hasQuality <- 'quality' %in% names(x)
    if(hasTime) {
        x$start <- ymd_hms(x$start)
        x$end <- ymd_hms(x$end)
    }
    if(hasQuality) {
        x$quality <- as.numeric(x$quality)
    }
    x
}

dfToAnnoList <- function(x) {
    hasTime <- all(c('start', 'end') %in% names(x))
    hasFreq <- all(c('freqMin', 'freqMax') %in% names(x))
    timeRange <- freqRange <- vector('list', length=nrow(x))
    for(i in seq_len(nrow(x))) {
        if(hasTime) {
            timeRange[[i]] <- c(x$start[i], x$end[i])
        }
        if(hasFreq) {
            freqRange[[i]] <- c(x$freqMin[i], x$freqMax[i])
        }
        # quality[[i]] <- x$quality[i]
    }
    list('freqRange' = freqRange,
         'timeRange' = timeRange,
         'quality' = x$quality
    )
}

myRenamer <- function(x, map) {
    if(is.data.frame(x)) {
        names(x) <- myRenamer(names(x), map)
        return(x)
    }
    # check if map is obviously backwards and fix it
    if(!any(x %in% names(map)) &&
       any(x %in% unlist(map))) {
        newMap <- as.list(names(map))
        names(newMap) <- unlist(map)
        map <- newMap
    }
    for(val in names(map)) {
        if(val %in% x) {
            x[x == val] <- map[[val]]
        }
    }
    x
}
