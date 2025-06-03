#' @title Match Detections to Other Data
#' 
#' @description Matches detection data from \link{loadDetectionData} to
#'    other data types. A new column is created in the input data to
#'    store detection data, and values in that column are filled in
#'    based on whether or not their times overlap with times in
#'    the detection data.
#' 
#' @param x a dataframe with column "UTC" and optionally "end"
#' @param detection dataframe of detection data loaded with
#'    \link{loadDetectionData}
#' @param name name of the column in \code{x} to add detection information
#'    to, will be created if it does not exist
#' @param value value to use for marking detections. If \code{value} is the
#'    name of a column in \code{detection}, then the corresponding value of 
#'    that column will be used. If not, then \code{value} will be used 
#'    for all detections
#' @param fillNA value to fill in for times that do not have detections,
#'    default is \code{NA}
#' @param by if not \code{NULL}, a specification for pairing distinct groups
#'    in \code{x} and \code{detection}. Can be a single character of the
#'    column name shared by both (e.g. \code{"site"}), a named character where the
#'    name is the column in \code{x} and the value is the column in 
#'    \code{detection} (e.g. \code{c("site"="deployment")}), or an equality
#'    statement where the left is the column in \code{x} and the right is the 
#'    column in \code{detection} (e.g. \code{"site" == "deployment"})
#' @param mode one of "replace" or "add", this sets how to deal with times
#'    that have multiple detections or where column \code{name} has an existing
#'    value. "replace" always replaces with the most recent value in \code{detection},
#'    "add" concatenates all values with ","
#' 
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#' 
#' @returns a dataframe
#' 
#' @examples
#' 
#' time <- as.POSIXct('2020-01-01 12:00:00', tz='UTC')
#' data <- data.frame(UTC=time+c(0, 30, 60, 90, 120),
#'                    end=time+c(30, 60, 90, 120, 150))
#' detection <- data.frame(UTC=time+c(25, 90),
#'                         end=time+c(40, 95),
#'                         species=c('A', 'B'))
#'                         
#' # Create a new "presence" column and fill with TRUE                       
#' matchDetectionData(data, detection, name='presence', value=TRUE)
#' # Fill non-matching times with FALSE (instead of default NA)
#' matchDetectionData(data, detection, name='presence', value=TRUE, fillNA=FALSE)
#' # Instead fill with value from "species" column
#' matchDetectionData(data, detection, name='presence', value='species')
#' 
#' detection <- data.frame(UTC=time+c(25, 90, 18, 30),
#'                         end=time+c(40, 95, 28, 40),
#'                         site=c('east', 'east', 'west', 'west'),
#'                         species=c('A', 'B', 'A', 'B'))
#'                         
#' data <- data.frame(UTC=time+c(0, 30, 60, 90, 120, 0, 30),
#'                    end=time+c(30, 60, 90, 120, 150, 30, 60),
#'                    deployment=c(rep('east', 5), rep('west', 2)))
#'                    
#' # detection now has overlapping times, so this will trigger a warning
#' matchDetectionData(data, detection, name='presence', value='species')
#' # mode='add' will change this to concatenate the labels for overlaps
#' matchDetectionData(data, detection, name='presence', value='species', mode='add')
#' # but really these correspond to different locations, so we can use "by" for that
#' matchDetectionData(data, detection, name='presence', value='species', by=c('deployment'='site'))
#' # this is another way to specify "by"
#' matchDetectionData(data, detection, name='presence', value='species', by='deployment'=='site')
#' 
#' @importFrom purrr reduce
#'
#' @export
#' 
matchDetectionData <- function(x, detection, name, value, fillNA=NA, by=NULL,
                               mode=c('replace', 'add')) {
    # check by. either c('a'='b') or 'a', or 'a'=='b'
    if(!is.null(by)) {
        if(is.character(by) &&
           is.null(names(by))) {
            by <- c(rep(by, 2))
        }
        if(is.character(by) &&
           !is.null(names(by))) {
            by <- c(names(by), by)
        }
        if(is.logical(by)) {
            by <- as.list(match.call())$by
            by <- strsplit(gsub(' |"', '', deparse(by)), '==')[[1]]
        }
        if(!is.character(by) ||
           length(by) != 2) {
            warning('Improper specification of "by" argument, data not matched')
            return(x)
        }
        if(!by[1] %in% names(x) ||
           !by[2] %in% names(detection)) {
            warning('"by" columns are missing from "x" or "detection", data not matched')
            return(x)
        }
    }
    if(!all(c('UTC', 'end') %in% names(detection))) {
        warning('"detection" data does not appear to be standard data loaded with ',
                '"loadDetectionData", cannot match')
        return(x)
    }
    inLong <- FALSE
    if(isLong(x)) {
        x <- toWide(x)
        inLong <- TRUE
    }
    dropEnd <- FALSE
    if(isWide(x)) {
        dropEnd <- TRUE
        # have to assume same time diff for sound
        tDiff <- median(as.numeric(difftime(
            x$UTC[2:nrow(x)],
            x$UTC[1:(nrow(x)-1)],
            units='secs')), na.rm=TRUE)
        x$end <- x$UTC + tDiff
    }
    
    if(!'end' %in% names(x)) {
        # 0 int makes same comparison as withinLHS
        x$interval <- interval(x$UTC, x$UTC) 
    } else {
        x$interval <- interval(x$UTC, x$end)
    }
    if(is.null(by)) {
        x <- split(x, 1) # jank to make rest work the tsame
    } else {
        x <- split(x, x[[by[1]]])
    }
    if(anyNA(detection$end)) {
        detection$end[is.na(detection$end)] <- detection$UTC[is.na(detection$end)]
    }
    detection$interval <- interval(detection$UTC, detection$end)
    mode <- match.arg(mode)
    x <- bind_rows(lapply(x, function(df) {
        if(!is.null(by)) {
            thisDet <- filter(detection, .data[[by[2]]] == df[[by[1]]][1])
            if(is.null(thisDet) ||
               nrow(thisDet) == 0) {
                warning('No detections for "', by[1], '" level ', df[[by[1]]][1], call.=FALSE)
                return(df)
            }
        } else {
            thisDet <- detection
        }
        result <- rep(NA, nrow(df))
        # ok if when adding new its not NA then yell if mode='replace'
        overlaps <- lapply(thisDet$interval, function(y) {
            overlapLHS(df$interval, y)
        })
        # case when using values in detection column
        if(value %in% names(thisDet)) {
            vals <- unique(thisDet[[value]])
            for(v in vals) {
                thisOverlap <- reduce(overlaps[thisDet[[value]] == v], `|`)
                newVal <- is.na(result[thisOverlap])
                if(mode == 'replace') {
                    if(any(!newVal)) {
                        warning('Some detections have overlapping times, only',
                                ' one will be used with mode="replace"', call.=FALSE)
                    }
                    result[thisOverlap] <- v
                }
                if(mode == 'add') {
                    result[thisOverlap][newVal] <- v
                    result[thisOverlap][!newVal] <- paste0(result[thisOverlap][!newVal], ',', v)
                }
            }
        } else {
            #case when single value
            overlaps <- reduce(overlaps, `|`)
            result[overlaps] <- value
        }
        if(!name %in% names(df)) {
            df[[name]] <- NA
        }
        if(mode == 'replace') {
            df[[name]] <- result
        }
        if(mode == 'add') {
            newVal <- is.na(df[[name]])
            df[[name]][newVal] <- result[newVal]
            df[[name]][!newVal] <- paste0(df[[name]][!newVal], ',', result[!newVal])
        }
        df
    }))
    if(!is.null(fillNA)) {
        isNA <- is.na(x[[name]])
        x[[name]][isNA] <- fillNA
    }
    if(isTRUE(dropEnd)) {
        x$end <- NULL
    }
    if(isTRUE(inLong)) {
        x <- toLong(x)
    }
    x$interval <- NULL
    x
}

overlapLHS <- function(int1, int2) {
    int1@start <= int2@start + int2@.Data & 
        int2@start < int1@start + int1@.Data
}

psx <- as.POSIXct('2020-01-01 12:00:00')
testX <- data.frame(
    UTC = psx + c(0, 30, 60, 90, 120, 0, 30),
    deployment = c('a', 'a', 'a', 'a', 'a', 'd', 'b'),
    'BB_10-100' = 1:7 #req to make it recognize as sound data and fill end
)
testDet <- data.frame(
    UTC = psx + c(115, 60, 61, 15, 31),
    end = psx + c(130, 62, 110, 25, 35),
    end = NA,
    deployment = c('a', 'a', 'a', 'b', 'c'),
    species = c('b', 'b', 'c', 'b', 'a')
)

# matchDetectionData(testX, testDet, name='SPORKS', value=TRUE, mode='replace')
# matchDetectionData(testX, testDet, name='SPORKS', value=TRUE, mode='replace', fillNA=FALSE)
# matchDetectionData(testX, testDet, name='SPORKS', value='species', mode='replace')
# matchDetectionData(testX, testDet, name='SPORKS', value='species', mode='add')
# matchDetectionData(testX, testDet, name='SPORKS', value='species', mode='add', by='deployment')
# matchDetectionData(hm, testDet, name='SPORKS', value='species', mode='add', by='deployment')
