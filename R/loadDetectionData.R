#' @title Load Detection Data
#' 
#' @description Loads and formats detection data into a common format for 
#'   use in other PAMscapes functions
#'   
#' @param x dataframe or path to CSV file containing detection data
#' @param source source of the detection data, choices other than "csv"
#'   just specify specific formatting options
#' @param columnMap a list or data.frame specifying how to map the input
#'   column names to the required standard names of "UTC", "end", and "species".
#'   If a list, must be a named list where the names are the existing 
#'   column names and the values are the standardized names, e.g. 
#'   \code{list('start'='UTC', 'SpeciesName'='species')}. If a data.frame,
#'   must have columns "old" with the existing column names and "new" with
#'   the standardized name to change it to. All columns successfully changed
#'   will be kept with the output
#' @param detectionType one of "auto", "presence", or "detection" specifying
#'   the type of detection in the data. "presence" means hourly or daily presence
#'   style of detections - the duration of the detection is used for the time 
#'   unit (e.g. hourly presence might have "UTC" value 2020-01-01 12:00:00 and
#'   "end" value 2020-01-01 13:00:00 for a detection). "detection" means the data
#'   refer to specific detections or bouts of detections rather than just presence.
#'   "auto" means that the type of detection will be inferred from the start and 
#'   end time of each detection - any detections with a duration of exactly one 
#'   hour or exactly one day will be marked as "presence", any other duration
#'   will be marked as "detection"
#' @param presenceDuration if \code{detectionType='presence'}, the duration in
#'   seconds, e.g. 86400 for daily presence
#' @param dateFormat format string of dates, see \link{strptime}. Can be a 
#'   vector of multiple formats
#' @param tz time zone of input data
#' @param wide logical flag indicating whether the input data has species 
#'   detection information in wide (instead of long) format. If \code{TRUE},
#'   then this means that there are multiple columns representing multiple
#'   kinds of detections, e.g. one column for each different species present.
#'   If \code{FALSE}, then there is a single column that indicates what kind
#'   of detection it is. 
#' @param speciesCols only used if \code{wide=TRUE}, the names of the columns
#'   containing the different types of detections
#' @param detectedValues only used if \code{wide=TRUE}, the values in each
#'   \code{speciesCols} column that indicate a positive detection. e.g. if
#'   "0" represents no detection and "1" represents a detection, then this
#'   should be "1". Note that all values will be converted to characters,
#'   so the string \code{"1"} must be used instead of the numeric \code{1}
#' @param extraCols (optional) any additional columns to keep with the output
#' @param \dots additional arguments used for certain \code{source} values
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @return a dataframe with columns UTC, end, species, and detectionType, where
#'   each row represents a single detection event. May have additional columns 
#'   depending on other parameters
#'   
#' @export
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate parse_date_time
#' @importFrom utils read.csv
#'
loadDetectionData <- function(x,
                              source=c('csv', 'makara'),
                              columnMap=NULL,
                              detectionType=c('auto', 'presence', 'detection'),
                              presenceDuration=NULL,
                              dateFormat=c('%Y-%m-%dT%H:%M:%S+0000',
                                           '%Y-%m-%d %H:%M:%S',
                                           '%m-%d-%Y %H:%M:%S',
                                           '%Y/%m/%d %H:%M:%S',
                                           '%m/%d/%Y %H:%M:%S'),
                              tz='UTC',
                              wide=FALSE, 
                              speciesCols=NULL,
                              detectedValues=NULL,
                              extraCols=NULL,
                              ...) {
    if(is.character(x) && length(x) > 1) {
        result <- bind_rows(lapply(x, function(d) {
            loadDetectionData(d, 
                              source=source,
                              columnMap=columnMap,
                              detectionType='detection',
                              presenceDuration = presenceDuration,
                              dateFormat=dateFormat,
                              tz=tz,
                              wide=wide,
                              speciesCols=speciesCols,
                              detectedValues=detectedValues,
                              extraCols=extraCols,
                              ...)
        }))
        switch(match.arg(detectionType),
               'auto' = {
                   if(!'end' %in% names(result)) {
                       warning('Must have an "end" column for detections to use "auto" detectionType')
                       return(NULL)
                   }
                   result$detectionType <- inferDetType(result$UTC, result$end)
               },
               'presence' = {
                   if(is.null(presenceDuration)) {
                       warning('Must provide duration of presence as "presenceDuration" (in seconds)')
                       return(NULL)
                   }
                   result$detectionType <- 'presence'
                   result$end <- result$UTC + presenceDuration
               },
               'detection' = {
                   if(!'end' %in% names(result)) {
                       result$end <- NA
                   }
                   result$detectionType <- 'detection'
               }
        )
        return(result)
    }
    reqCols <- c('UTC', 'end', 'species', 'detectionType')
    source <- match.arg(source)
    if(is.character(x) && !file.exists(x)) {
        warning('File ', x, ' does not exist')
        return(NULL)
    }
    if(is.character(x)) {
        # result <- read.csv(x, stringsAsFactors = FALSE)
        result <- fread(x, header=TRUE)
        setDF(result)
    }
    if(is.data.frame(x)) {
        result <- x
    }
    if(source == 'makara') {
        columnMap <- getColMaps('makara')
        result$project <- detFileToCode(x)
        extraCols <- c(extraCols, 'call', 'project')
        if(is.null(detectedValues)) {
            detectedValues <- 'DETECTED'
        }
    }
    if(is.null(columnMap)) {
        columnMap <- getColMaps('standard')
    }
    columnMap <- checkMap(columnMap)
    names(result) <- renameToMap(names(result), columnMap)
    extraCols <- c(extraCols, names(result)[names(result) %in% columnMap$new])
    if(!'UTC' %in% names(result)) {
        warning('Could not find "UTC" column, adjust "columnMap" and try again')
        return(NULL)
    }
    if(!tz %in% OlsonNames()) {
        warning('Time zone ', tz, ' is invalid, must be present in "OlsonNames()"')
        return(NULL)
    }
    result$UTC <- parse_date_time(result$UTC, 
                                  orders=dateFormat, 
                                  truncated = 3,
                                  tz=tz, 
                                  quiet=TRUE,
                                  exact=TRUE)
    if(tz != 'UTC') {
        result$UTC <- with_tz(result$UTC, tzone='UTC')
    }
    naDate <- is.na(result$UTC)
    if(all(naDate)) {
        warning('No dates could be parsed correctly, adjust "dateFormat" and try again')
        return(NULL)
    }
    if(any(naDate)) {
        warning(sum(naDate), ' dates could not be parsed correctly')
    }
    if('end' %in% names(result)) {
        result$end <- parse_date_time(result$end, 
                                      orders=dateFormat, 
                                      truncated = 3,
                                      tz=tz, 
                                      quiet=TRUE,
                                      exact=TRUE)
    }
    if('duration' %in% names(result) &&
       !'end' %in% names(result)) {
        result$end <- result$UTC + result$duration
        extraCols <- c(extraCols, 'duration')
    }
    
    detectionType <- match.arg(detectionType)
    switch(detectionType,
           'auto' = {
               if(!'end' %in% names(result)) {
                   warning('Must have an "end" column for detections to use "auto" detectionType')
                   return(NULL)
               }
               result$detectionType <- inferDetType(result$UTC, result$end)
           },
           'presence' = {
               if(is.null(presenceDuration)) {
                   warning('Must provide duration of presence as "presenceDuration" (in seconds)')
                   return(NULL)
               }
               result$detectionType <- 'presence'
               result$end <- result$UTC + presenceDuration
           },
           'detection' = {
               if(!'end' %in% names(result)) {
                   result$end <- NA
               }
               result$detectionType <- 'detection'
           }
    )
    if(isTRUE(wide)) {
        if(is.null(detectedValues)) {
            warning('Must specify which values indicate a positive detection',
                    ' with "detectedValues"')
            return(NULL)
        }
        if(is.null(speciesCols)) {
            warning('Must specify which columns contain detection data with',
                    ' "speciesCols"')
        }
        result <- pivot_longer(result, 
                               cols=all_of(speciesCols), 
                               values_transform=as.character,
                               names_to='species',
                               values_to='detectedFlag')
    }
    if('detectedFlag' %in% names(result) &&
       !is.null(detectedValues)) {
        result <- result[result$detectedFlag %in% detectedValues, ]
        extraCols <- c(extraCols, 'detectedFlag')
        # result$detectedFlag <- NULL
    }
    result <- select(result, all_of(unique(c(reqCols, extraCols))))
    result
}

getColMaps <- function(which=NULL) {
    maps <- list(
        'makara' = list('detection_start_datetime' = 'UTC',
                        'detection_end_datetime' = 'end',
                        'detection_sound_source_code' = 'species',
                        'detection_call_type_code' = 'call',
                        'detection_type_code' = 'detectedFlag',
                        'detection_latitude' = 'Latitude',
                        'detection_longitude' = 'Longitude'
        ),
        'gen' = list('StartDate' = 'UTC',
                     'PROJECT_DESCRIPTION' = 'project',
                     'SITE_NAME' = 'site',
                     'LATITUDE_DDG_DEPLOYMENT' = 'Latitude',
                     'LONGITUDE_DDG_DEPLOYMENT' = 'Longitude'
        ),
        'standard' = list('utc' = 'UTC',
                          'end' = 'end',
                          'lat' = 'Latitude',
                          'lon' = 'Longitude',
                          'long' = 'Longitude',
                          'latitude' = 'Latitude',
                          'longitude' = 'Longitude',
                          'start' = 'UTC',
                          'duration' = 'duration',
                          'species' = 'species'
        )
    )
    if(is.null(which)) {
        return(maps)
    }
    maps[[which]]
}

inferDetType <- function(start, end, verbose=TRUE) {
    diff <- as.numeric(difftime(end, start, units='secs'))
    isNa <- is.na(diff)
    hour <- diff %in% 3600
    day <- diff %in% 86400
    result <- rep('detection', length(start))
    result[hour] <- 'presence'
    result[day] <- 'presence'
    result[isNa] <- NA_character_
    if(verbose) {
        nHour <- sum(hour)
        nDay <- sum(day)
        nNa <- sum(isNa)
        nDet <- length(result) - nHour - nDay - nNa
        text <- 'Detection types found:'
        if(nHour > 0) {
            text <- paste0(text, '\n  ', nHour, ' hourly presence')
        }
        if(nDay > 0) {
            text <- paste0(text, '\n  ', nDay, ' daily presence')
        }
        if(nDet > 0) {
            text <- paste0(text, '\n  ', nDet, ' detection')
        }
        if(nNa > 0) {
            text <- paste0(text, '\n  ', nNa, ' could not be automatically inferred (no end time)')
        }
        cat(text)
    }
    result
}

renameToMap <- function(names, map) {
    map <- checkMap(map)
    lowNames <- tolower(names)
    map$old <- tolower(map$old)
    for(i in which(lowNames %in% map$old)) {
        names[i] <- map[['new']][map$old == lowNames[i]]
    }
    names
}

detFileToCode <- function(x, analysis=FALSE) {
    x <- basename(x)
    x <- gsub('\\.csv$', '', x)
    pattern <- '(.*)_([A-z]*_ANALYSIS$)'
    if(isTRUE(analysis)) {
        code <- gsub(pattern, '\\2', x)
    } else {
        code <- gsub(pattern, '\\1', x)
    }
    if(code == x) {
        warning('Could not properly parse file name ', x)
        return(NA)
    }
    code
}

checkMap <- function(map) {
    if(is.null(map)) {
        return(map)
    }
    if(is.list(map) &&
       !is.data.frame(map)) {
        map <- data.frame(old=names(map), new=unlist(map, use.names=FALSE))
    }
    if(is.character(map) &&
       !is.null(names(map))) {
        map <- data.frame(old=names(map), new=unname(map))
    }
    if(!is.data.frame(map) ||
       !all(c('old', 'new') %in% names(map))) {
        warning('Column map must be a named list, named vector, or',
                'dataframe with "old" and "new" columns')
        return(NULL)
    }
    map
}
