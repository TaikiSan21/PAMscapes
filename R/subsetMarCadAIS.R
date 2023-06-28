#' @title Subset Marine Cadastre AIS Data to Region
#'
#' @description Subsets the full download files from Marine Cadastre to a
#'   smaller region so that they are easier to work with
#'
#' @param inDir directory containing Marine Cadastre AIS CSV files to subset
#' @param outDir directory to write subsetted files to
#' @param latRange range of desired latitudes (decimal degrees)
#' @param lonRange range of desired longitudes (decimal degrees)
#' @param name prefix to append to new filenames
#' @param progress logical flag to show progress bar
#'
#' @return invisibly return new file names
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom data.table fread setkeyv fwrite
#' @importFrom utils txtProgressBar setTxtProgressBar globalVariables
#' @importFrom stats sd
#'
#' @export
#'
subsetMarCadAIS <- function(inDir, outDir, latRange=c(20, 50), lonRange=c(-140, -110), name='West_', progress=TRUE) {
    if(!dir.exists(outDir)) {
        dir.create(outDir)
    }
    aisFiles <- list.files(inDir, pattern='AIS_[0-9]{4}_[0-9]{1,2}_[0-9]{1,2}\\.csv$', full.names=TRUE)
    if(progress) {
        pb <- txtProgressBar(min=0, max=length(aisFiles), style=3)
    }
    outFiles <- character(length(aisFiles))
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
        # thisAis[, tDiff := c(0, diff(as.numeric(UTC))), MMSI]
        thisAis <- dropBySpeed(thisAis, knots=150)
        thisAis$knots <- NULL
        outName <- file.path(outDir, paste0(name, basename(aisFiles[i])))
        fwrite(thisAis, file=outName)
        outFiles[i] <- outName
        if(progress) {
            setTxtProgressBar(pb, value=i)
        }
    }
    invisible(outFiles)
}

globalVariables(c('Longitude', 'Latitude'))

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

#' @importFrom geosphere distGeo
#'
calcKnots <- function(x) {
    if(nrow(x) == 1) {
        return(NA)
    }
    ix1 <- 1:(nrow(x)-1)
    ix2 <- 2:(nrow(x))
    diff <- distGeo(
        matrix(c(x$Longitude[ix1], x$Latitude[ix1]), ncol=2),
        matrix(c(x$Longitude[ix2], x$Latitude[ix2]), ncol=2))
    tDiff <- abs(as.numeric(difftime(x$UTC[ix1], x$UTC[ix2], units='secs')))
    diff <- ifelse(tDiff == 0, 0, diff / tDiff)
    # convert m/s to knots * 1.94
    c(NA, diff) * 1.94
}

#' @importFrom dplyr arrange bind_rows
#'
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
    x <- arrange(x, .data$UTC)
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
