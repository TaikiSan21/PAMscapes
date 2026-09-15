#' @title Add Data Quality Matrix to PyPam NetCDF
#' 
#' @description Add or modify the existing data quality matrix data
#'   in a PyPam (or similar) hybrid millidecade NetCDF output. This
#'   data adds or modifies the "quality_flag" variable of a NetCDF 
#'   file, which is a matrix of numbers 1,2,3, or 4 that is the same
#'   dimensions as the hybrid millidecade outputs. The input NetCDF files
#'   are modified unless \code{preview=TRUE}
#'   
#' @param x one or more NetCDF files or a folder containing NetCDF files.
#'   If \code{x} is a folder, all files with extension ".nc" will be
#'   listed from the first level of the folder (non-recursive)
#' @param annotation a dataframe of annotations specifying the time-frequency
#'   bounds and quality flags to be changed. Columns for time bounds must 
#'   be called \code{start} and \code{end}, must be in UTC timezone, and 
#'   either POSIXct format or in YYYY-MM-DD HH:MM:SS. Columns for frequency
#'   bounds must be called \code{freqMin} and \code{freqMax} and have numeric
#'   frequency values in Hz. Column for quality code must be called 
#'   \code{quality} and have numeric values from 1,2,3,4. Can also have
#'   additional column \code{file} specifying that a given annotation only
#'   applies to a specific file, or a column to match the "platform" attribute
#'   of the NetCDF file (see \code{platformColumn})
#' @param platformColumn NetCDF files should contain a "platform" global attribute,
#'   this can be used to specify that certain rows of \code{annotation} only apply
#'   to certain files in \code{x}. This value specifies the name of the column in
#'   \code{annotation} should be used (e.g. "deployment_code" means
#'   \code{annotation$deployment_code} contains values that will match the
#'   "platform" global attribute)
#' @param updateUnknown if \code{FALSE}, then existing quality flags of "2"
#'   will not be changed, otherwise flags of "2" will be updated to this value
#'   (must be one of 1, 3, or 4)
#' @param preview if \code{TRUE}, then no changes are applied to the NetCDF
#'   files, instead only plots are shown. It is recommended to run with
#'   \code{preview=TRUE} first to check that annotations appear to be applied
#'   properly
#' @param plot if \code{TRUE}, a plot of the quality matrix is shown. If
#'   \code{"new"}, the plot is only shown if any annotations have been
#'   applied to the file. If \code{FALSE} no plot is created
#' @param reset if \code{TRUE}, all quality flag flags are reset to a
#'   value of "2" before applying further annotations. Can be useful if
#'   incorrect annotations were previously applied. If \code{reset=TRUE}
#'   and \code{annotation=NULL}, then all quality code values will be "2"
#' @param verbose logical flag to show or hide some messages
#'
#' @return \code{TRUE} or \code{FALSE} to indicate whether or not any
#'   annotations were applied. If \code{x} is multiple files or a folder of
#'   files, the returned vector has names equal to the file names
#'   
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @importFrom scales viridis_pal
#' @importFrom ncdf4 ncatt_put ncvar_def ncvar_add ncvar_put
#' @importFrom lubridate ymd_hms
#' @importFrom graphics legend
#'
#' @export
#' 
addDataQuality <- function(x, 
                           annotation=NULL, 
                           # freqRange=NULL, 
                           # timeRange=NULL, 
                           # quality=2, 
                           platformColumn=NULL,
                           updateUnknown=FALSE, # change 2s to something else
                           preview=FALSE,
                           plot=FALSE,
                           reset=FALSE,
                           verbose=FALSE) {
    if(length(x) == 1 &&
       dir.exists(x)) {
        x <- list.files(x, pattern='nc$', full.names=TRUE, recursive=FALSE)
    }
    if(isTRUE(preview) &&
       isFALSE(plot)) {
        plot <- 'new'
    }
    if(length(x) > 1) {
        result <- sapply(x, function(file) {
            addDataQuality(file, 
                           annotation=annotation,
                           # freqRange = freqRange,
                           # timeRange = timeRange,
                           # quality = quality,
                           updateUnknown = updateUnknown,
                           verbose = verbose,
                           preview=preview,
                           platformColumn = platformColumn,
                           plot = plot,
                           reset = reset)
        })
        names(result) <- basename(x)
        return(invisible(result))
    }
    nc <- nc_open(x, write=TRUE)
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
    platform <- ncatt_get(nc, 0, 'platform')
    if(isTRUE(platform$hasatt)) {
        platform <- platform$value
    } else {
        platform <- NULL
    }
    times <- ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    type <- checkFreqType(freqs)
    makaraCols <- c(
        'deployment_code',
        'recording_interval_start_datetime',
        'recording_interval_end_datetime',
        'recording_interval_min_frequency_khz',
        'recording_interval_max_frequency_khz',
        'recording_interval_quality_type_code')
    if(all(makaraCols %in% names(annotation))) {
        platformColumn <- 'deployment_code'
    }
    dqAnno <- formatDQAnnotation(x=annotation)
    # dqAnno <- formatDQAnnotation(x=annotation,
    #                              freqRange=freqRange,
    #                              timeRange=timeRange, 
    #                              quality=quality
    # )
    if(!is.null(platformColumn) &&
       !is.null(dqAnno) &&
       !platformColumn %in% names(dqAnno)) {
        warning('Column ', platformColumn, ' is not present in annotation data')
        platformColumn <- NULL
        
    }
    if(!is.null(platformColumn) &&
       is.null(platform)) {
        warning('NetCDF file ', basename(nc), ' does not have a platform attribute')
    }
    if(!is.null(platformColumn) &&
       !is.null(platform) &&
       !is.null(dqAnno)) {
        dqAnno <- dqAnno[dqAnno[[platformColumn]] == platform, ]
    }
    if('file' %in% names(dqAnno)) {
        dqAnno <- dqAnno[basename(dqAnno$file) == basename(x), ]
    }
    if(!isFALSE(updateUnknown) &&
       !updateUnknown %in% c(1,3,4)) {
        warning('updateUnknown must be a quality flag of 1, 3, or 4 or FALSE')
        updateUnknown <- FALSE
    }
    if(!isFALSE(updateUnknown)) {
        dqMat[!dqMat %in% c(1,3,4)] <- updateUnknown
    }
    changedVals <- FALSE
    if(!is.null(dqAnno) &&
       nrow(dqAnno) > 0) {
        dqAnno$interval <- interval(dqAnno$start, dqAnno$end)
        thisInterval <- interval(times[1], times[length(times)])
        doesOverlap <- int_overlaps(dqAnno$interval, thisInterval)
        if(anyNA(doesOverlap) || any(doesOverlap)) {
            dqMat <- markDQMatrix(dqMat, 
                                  freqRange=dqAnno,
                                  times=times, 
                                  freqs=freqs)
            changedVals <- TRUE
        }
    }
    # only plot new if vals changed
    if(plot == 'new') {
        plot <- changedVals
    }
    
    if(isTRUE(plot)) {
        pal <- viridis_pal(option='H')(25)[6:25]
        # pal <- rep(c('darkgreen', 'steelblue', 'yellow', 'red'), each=5)
        image(t(dqMat), x=times, y=seq_along(freqs), 
              axes=FALSE, xlab='Date',ylab='Freq',useRaster=TRUE,
              zlim=c(1,4),
              col=pal,
              main=basename(x))
        
        # legend(grconvertX(1, "ndc"), grconvertY(1, "ndc"), 
        legend(x=as.numeric(times[1]),y=length(freqs), 
               xjust=0, yjust=0,horiz = T, cex=0.75,
               c("1",'2', "3","4"), fill = pal[c(1, 7, 14, 20)], xpd = NA)
        
        prettyDates <- pretty(times, n=5)
        prettyFreqs <- pretty(seq_along(freqs), n=5)
        prettyFreqs[prettyFreqs == 0] <- 1
        axis(1, at=prettyDates, labels=as.character(prettyDates))
        axis(2, at=prettyFreqs, labels=round(freqs[prettyFreqs], 0))
    }
    if(isTRUE(preview)) {
        return(changedVals)
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
    invisible(changedVals)
}

formatDQAnnotation <- function(x=NULL, freqRange=NULL, timeRange=NULL, quality=NULL) {
    if(is.null(x) &&
       is.null(freqRange) &&
       is.null(timeRange)) {
        return(NULL)
    }
    if(!is.null(x)) {
        result <- formatDQDf(x) 
        # result <- dfToAnnoList(result)
        return(result)
    }
    result <- list(quality=as.numeric(quality))
    if(!is.null(freqRange)) {
        result$freqMin <- freqRange[1]
        result$freqMax <- freqRange[2]
    }
    if(!is.null(timeRange)) {
        if(is.character(timeRange)) {
            timeRange <- ymd_hms(timeRange)
        }
        result$start <- timeRange[1]
        result$end <- timeRange[2]
    }
    result <- bind_rows(result)
    result <- dropNADQ(result)
    result
}

dropNADQ <- function(x) {
    timeNA <- is.na(x$start) | is.na(x$end)
    freqNA <- is.na(x$freqMin) | is.na(x$freqMax)
    bothNA <- timeNA & freqNA
    x[!bothNA, ]
}

#' @export
#' @rdname addDataQuality
#' 
formatDQDf <- function(annotation) {
    makaraMap <- list('recording_interval_start_datetime' = 'start',
                      'recording_interval_end_datetime' = 'end',
                      'recording_interval_min_frequency_khz' = 'freqMin',
                      'recording_interval_max_frequency_khz' = 'freqMax',
                      'recording_interval_quality_type_code' = 'quality')
    if(all(names(makaraMap) %in% names(annotation))) {
        names(annotation) <- myRenamer(names(annotation), makaraMap)
        codeMap <- list('COMPROMISED' = 3,
                        'UNUSABLE' = 4)
        annotation$freqMin <- annotation$freqMin * 1000
        annotation$freqMax <- annotation$freqMax * 1000
        annotation$quality <- myRenamer(annotation$quality, codeMap)
    }
    hasTime <- all(c('start', 'end') %in% names(annotation))
    hasFreq <- all(c('freqMin', 'freqMax') %in% names(annotation))
    if(!hasTime && !hasFreq) {
        warning('Annotation input does not have time columns "start" and "end"',
                ' or frequency columns "freqMin" and "freqMax"')
    }
    hasQuality <- 'quality' %in% names(annotation)
    if(hasTime) {
        annotation$start <- ymd_hms(annotation$start)
        annotation$end <- ymd_hms(annotation$end)
    }
    if(hasQuality) {
        annotation$quality <- as.numeric(annotation$quality)
    }
    annotation <- dropNADQ(annotation)
    annotation
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
