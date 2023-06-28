# ais functions
library(data.table)
library(sf)
library(readr)
library(ggplot2)
library(httr)
library(lubridate)
library(dplyr)

# make marine cadstre URL
dayToAisURL <- function(x) {
    dayChar <- format(x, format='%Y/AIS_%Y_%m_%d')
    urlBase <- 'https://coast.noaa.gov/htdata/CMSP/AISDataHandler/'
    paste0(urlBase, dayChar, '.zip')
}

# make date sequence to cover range
getDaySequence <- function(x) {
    tRange <- range(x)
    tRange <- floor_date(tRange, unit='day')
    seq(from=tRange[1], to=tRange[2], by='day')
}

# download set of marine cadastre data covering
# date range in "x"
downloadAis <- function(x, outDir='.', overwrite=TRUE, unzip=TRUE) {
    allDays <- getDaySequence(x$UTC)
    years <- year(allDays)
    tooEarly <- years < 2015
    if(any(tooEarly)) {
        warning('AIS data before 2015 has a different format and cannot be downloaded')
    }
    tooLate <- years > 2022
    if(any(tooLate)) {
        warning('AIS data after 2022 is not yet available and cannot be downloaded')
    }
    allDays <- allDays[!tooEarly & !tooLate]
    if(length(allDays) == 0) {
        warning('No valid years present')
        return(NULL)
    }

    urls <- dayToAisURL(allDays)
    # urls
    if(!dir.exists(outDir)) {
        dir.create(outDir)
    }
    outZip <- file.path(outDir, basename(urls))
    outCsv <- gsub('zip$', 'csv', outZip)
    for(i in seq_along(urls)) {
        cat(paste0('\nDownloading AIS file ', i, ' out of ', length(urls), '...\n'))
        if(isFALSE(overwrite) &&
           (file.exists(outZip[i]) ||
            file.exists(outCsv[i]))) {
            # outZip[i] <- NA
            cat('File', i, 'exists')
        } else {
            dl <- GET(urls[i],
                      progress(),
                      write_disk(outZip[i], overwrite = TRUE))
            if(dl$status_code != 200) {
                warning('URL ', urls[i], ' failed to download properly')
                outZip[i] <- NA
            }
        }
        if(isTRUE(unzip) &&
           file.exists(outZip[i])) {
            cat('Unzipping...')
            unzip(outZip[i], exdir=outDir)
        }
    }
    invisible(outZip)
}

# subset full marine cadastre files to only bounding box range so easier to work with
subsetMarCadAIS <- function(inDir, outDir, latRange=c(20, 50), lonRange=c(-140, -110), name='West_') {
    if(!dir.exists(outDir)) {
        dir.create(outDir)
    }
    aisFiles <- list.files(inDir, pattern='AIS_[0-9]{4}_[0-9]{1,2}_[0-9]{1,2}\\.csv$', full.names=TRUE)
    pb <- txtProgressBar(min=0, max=length(aisFiles), style=3)
    for(i in seq_along(aisFiles)) {
        thisAis <- fread(aisFiles[i], select=c(LAT='numeric', LON='numeric', MMSI='integer',BaseDateTime='POSIXct',
                                               Length='integer', VesselType='integer', SOG='numeric'), quote='')

        colnames(thisAis) <- c('Latitude', 'Longitude', 'MMSI', 'UTC', 'vesselLength', 'vesselType', 'SOG')
        thisAis <- thisAis[Longitude >= lonRange[1] &
                               Longitude <= lonRange[2] &
                               Latitude >= latRange[1] &
                               Latitude <= latRange[2], ]
        thisAis$vesselType <- vessTypeConvert(thisAis$vesselType)
        setkeyv(thisAis, 'UTC')
        thisAis[, tDiff := c(0, diff(as.numeric(UTC))), MMSI]
        thisAis <- dropBySpeed(thisAis, knots=150)
        thisAis$knots <- NULL
        fwrite(thisAis, file=file.path(outDir, paste0(name, basename(aisFiles[i]))))
        setTxtProgressBar(pb, value=i)
    }
}

vessTypeConvert <- function(x) {
    tug <- c(21, 22, 31, 32, 52)
    fishing <- 30

    passenger <- c(60:69)
    cargo <- 70:79
    tanker <- 80:89
    out <- rep('other', length(x))
    out[x %in% tug] <- 'tug'
    out[x %in% fishing] <- 'fishing'
    out[x %in% passenger] <- 'passenger'
    out[x %in% cargo] <- 'cargo'
    out[x %in% tanker] <- 'tanker'
    out
}

gpsToAis <- function(gps, aisDir, buffer=10e3, timeBuff=3600) {
    daySeq <- PAMscapes:::getDaySequence(gps$UTC)
    dayChar <- format(daySeq, format='%Y_%m_%d')
    aisFiles <- list.files(aisDir, pattern='.*AIS.*csv$', full.names=TRUE)
    aisDay <- gsub('.*([0-9]{4}_[0-9]{2}_[0-9]{2})\\.csv$', '\\1', basename(aisFiles))
    hasDay <- dayChar %in% aisDay
    if(!all(hasDay)) {
        stop(sum(!hasDay), ' days (', paste0(dayChar[!hasDay], collapse=', '),
             ') did not have matching AIS files. Download using "downloadMarCadAIS"')
    }
    aisUse <- aisFiles[aisDay %in% dayChar]
    ais <- rbindlist(lapply(aisUse, function(a) {
        tmp <- fread(a)
        tmp <- tmp[UTC >= (min(gps$UTC) - timeBuff) &
                       UTC <= (max(gps$UTC) + timeBuff), ]
        tmp
    }))
    bufferBound <- st_as_sf(gps[,c('Longitude', 'Latitude', 'UTC')],
                            coords=c('Longitude', 'Latitude'), crs=4326) %>%
        st_bbox() %>%
        st_as_sfc() %>%
        st_buffer(dist=buffer) %>%
        st_bbox()
    mmsiNear <- unique(ais[Longitude >= bufferBound['xmin'] &
                               Longitude <= bufferBound['xmax'] &
                               Latitude >= bufferBound['ymin'] &
                               Latitude <= bufferBound['ymax'], 'MMSI'][[1]])
    ais <- ais[MMSI %in% mmsiNear, ]
    setkeyv(ais, 'UTC')
    ais$buoyLat <- approx(x=gps$UTC, y=gps$Latitude, xout=ais$UTC)$y
    ais$buoyLon <- approx(x=gps$UTC, y=gps$Longitude, xout=ais$UTC)$y
    ais$buoyDist <- geosphere::distGeo(matrix(c(ais$Longitude, ais$Latitude), ncol=2),
                                       matrix(c(ais$buoyLon, ais$buoyLat), ncol=2))
    ais$inDist <- ais$buoyDist <= buffer
    mmsiIn <- unique(ais$MMSI[ais$inDist])
    ais <- ais[MMSI %in% mmsiIn, ]
    # create groups in case a single path cuts in and out of our bounds
    ais <- rbindlist(lapply(split(ais, ais$MMSI), function(x) {
        x <- arrange(x, UTC)
        x$alternate <- FALSE
        if(nrow(x) == 1) {
            return(x)
        }
        x$alternate[2:nrow(x)] <- x$inDist[2:nrow(x)] != x$inDist[1:(nrow(x)-1)]
        x$alternate[is.na(x$alternate)] <- FALSE
        x$group <- cumsum(x$alternate)
        # x$alternate <- NULL
        x
    }))
    ais
}

# interp AIS coords & speed to new time
oneAisToGps <- function(gps, ais) {
    if(nrow(ais) <= 1) {
        return(gps)
    }
    gps$MMSI <- ais$MMSI[1]
    gps$vesselLength <- ais$vesselLength[1]
    gps$vesselType <- ais$vesselType[1]
    gps$SOG <- approx(x=ais$UTC, y=ais$SOG, xout=gps$UTC)$y
    gps$shipLat <- approx(x=ais$UTC, y=ais$Latitude, xout=gps$UTC)$y
    gps$shipLong <- approx(x=ais$UTC, y=ais$Longitude, xout=gps$UTC)$y
    gps$shipDist <- geosphere::distGeo(matrix(c(gps$Longitude, gps$Latitude), ncol=2),
                                       matrix(c(gps$shipLong, gps$shipLat), ncol=2))
    gps
}

aisToGps <- function(gps, ais) {
    bind_rows(
        lapply(
            # split(ais, list(ais$MMSI, ais$group)), function(x) {
            split(ais, ais$MMSI), function(x) {
                tryCatch(oneAisToGps(gps, x),
                         error = function(e) {
                             browser()
                         },
                         warning = function(w) {
                             browser()
                         })
            }))
}

calcKnots <- function(x) {
    if(nrow(x) == 1) {
        return(NA)
    }
    ix1 <- 1:(nrow(x)-1)
    ix2 <- 2:(nrow(x))
    # diff <- sqrt((x$Latitude[ix1] - x$Latitude[ix2])^2 +
    # (x$Longitude[ix1] - x$Longitude[ix2])^2)
    diff <- distGeo(
        matrix(c(x$Longitude[ix1], x$Latitude[ix1]), ncol=2),
        matrix(c(x$Longitude[ix2], x$Latitude[ix2]), ncol=2))
    # tDiff <- abs(as.numeric(x$UTC[ix1]) - as.numeric(x$UTC[ix2]))
    tDiff <- abs(as.numeric(difftime(x$UTC[ix1], x$UTC[ix2], units='secs')))
    diff <- ifelse(tDiff == 0, 0, diff / tDiff)
    # convert from dec.deg/s to ~km/h 111km/dd
    # c(NA, diff) * 111 * 3600
    # convert m/s to knots * 1.94
    c(NA, diff) * 1.94
}

dropBySpeed <- function(x, knots=200) {
    if(nrow(x) == 1) {
        return(x)
    }
    if(length(unique(x$MMSI)) > 1) {
        return(
            bind_rows(lapply(split(x, x$MMSI), function(d) {
                dropBySpeed(d, knots)
            }))
        )
    }
    x <- arrange(x, UTC)
    timeDiff <- as.numeric(difftime(x$UTC[2:nrow(x)], x$UTC[1:(nrow(x)-1)], units='secs'))
    x <- x[c(TRUE, abs(timeDiff) > 5),]
    x$knots <- calcKnots(x)
    whichHigh <- which(x$knots > knots)
    if(length(whichHigh) == 0) {
        # x$knots <- NULL
        return(x)
    }
    # check for consecutives and drop them
    consec <- union(intersect(whichHigh, whichHigh+1),
                    intersect(whichHigh, whichHigh-1))
    if(length(consec) > 0) {
        return(dropBySpeed(x[-consec, ], knots))
    }
    # if(nrow(x) %in% whichHigh) {
    #     return(dropBySpeed(x[-nrow(x), ], knots))
    # }
    # non-consec ones are ambiguous which of 2 points are the outlier, so drop by
    # which has the higher z-score
    zLat <- abs((x$Latitude - mean(x$Latitude, na.rm=TRUE)) / sd(x$Latitude, na.rm=TRUE))
    zLon <- abs((x$Longitude - mean(x$Longitude, na.rm=TRUE)) / sd(x$Longitude, na.rm=TRUE))
    drop <- numeric()
    for(i in seq_along(whichHigh)) {
        z_curr <- max(zLat[whichHigh[i]], zLon[whichHigh[i]])
        z_bef <- max(zLat[whichHigh[i]-1], zLon[whichHigh[i]-1])
        drop[i] <- ifelse(z_curr > z_bef, whichHigh[i], whichHigh[i]-1)
    }
    x <- x[-drop, ]
    dropBySpeed(x, knots)
}