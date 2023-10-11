#' @title Read AIS Data Near GPS Track
#'
#' @description Reads in AIS data downloaded from Marine Cadastre of
#'   ship tracks that come within a certain distance of a given GPS
#'   track. Also calculates the distance to the GPS track for each
#'   AIS point
#'
#' @param gps a dataframe with columns \code{UTC}, \code{Latitude},
#'   and \code{Longitude} to get nearby AIS data for
#' @param aisDir directory of AIS CSV files to read from
#' @param distance distance in meters around the GPS track to read AIS
#'   data for
#' @param timeBuff extra time (seconds) before and after the GPS points
#'   to read AIS data for. This can help create a better picture of ship
#'   activity surrounding the GPS
#'
#' @return a dataframe of AIS data, with additional columns related to distance
#'   to provided buoy GPS track
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' gps <- data.frame(Latitude=c(33.2, 33.5,33.6),
#'                   Longitude=c(-118.1, -118.4, -119),
#'                   UTC=as.POSIXct(
#'                     c('2022-04-28 05:00:00',
#'                       '2022-04-28 10:00:00',
#'                       '2022-04-28 20:00:00'),
#'                     tz='UTC'))
#' ais <- readLocalAIS(gps, aisDir=system.file('extdata/ais', package='PAMscapes'), distance=20e3)
#' str(ais)
#'
#' @importFrom data.table rbindlist
#' @importFrom sf st_as_sf st_bbox st_as_sfc st_buffer st_bbox
#' @importFrom dplyr arrange
#' @importFrom stats approx
#'
#' @export
#'
readLocalAIS <- function(gps, aisDir, distance=10e3, timeBuff=0) {
    if(nrow(gps) == 1 &&
       timeBuff == 0) {
        stop('Cannot match AIS to a single point in time, either ',
             'increase "timeBuff" or add more "UTC" rows.')
    }
    daySeq <- getDaySequence(gps$UTC)
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
        # st_bbox() %>%
        # st_as_sfc() %>%
        st_buffer(dist=distance) %>%
        st_bbox()
    mmsiNear <- unique(ais[Longitude >= bufferBound['xmin'] &
                               Longitude <= bufferBound['xmax'] &
                               Latitude >= bufferBound['ymin'] &
                               Latitude <= bufferBound['ymax'], 'MMSI'][[1]])
    ais <- ais[ais$MMSI %in% mmsiNear, ]
    setkeyv(ais, 'UTC')
    if(nrow(gps) == 1) {
        approxMethod <- 'constant'
        approxRule <- 2
    } else {
        approxMethod <- 'linear'
        approxRule <- 1
    }
    ais$buoyLat <- approx(x=gps$UTC, y=gps$Latitude, xout=ais$UTC, method=approxMethod, rule=approxRule)$y
    ais$buoyLon <- approx(x=gps$UTC, y=gps$Longitude, xout=ais$UTC, method=approxMethod, rule=approxRule)$y
    ais$buoyDist <- distGeo(matrix(c(ais$Longitude, ais$Latitude), ncol=2),
                            matrix(c(ais$buoyLon, ais$buoyLat), ncol=2))
    ais$inDist <- ais$buoyDist <= distance
    ais$inDist[is.na(ais$inDist)] <- FALSE
    mmsiIn <- unique(ais$MMSI[ais$inDist])
    ais <- ais[ais$MMSI %in% mmsiIn, ]
    # create groups in case a single path cuts in and out of our bounds
    ais <- rbindlist(lapply(split(ais, ais$MMSI), function(x) {
        if(nrow(x) == 1) {
            return(x)
        }
        x <- arrange(x, .data$UTC)
        x$alternate <- FALSE
        x$alternate[2:nrow(x)] <- x$inDist[2:nrow(x)] != x$inDist[1:(nrow(x)-1)]
        x$alternate[is.na(x$alternate)] <- FALSE
        x$group <- cumsum(x$alternate)
        x$alternate <- NULL
        x
    }))
    ais
}

globalVariables('UTC')
