# this works from 2015-2022
# 2009-2014 have different formats by Zone by Month. We should be zone 10/11 (maybe 9 if offshore)

# could easily write script that downloads this shit for all of our deployments to date, truncates
# ais to relevant area, and stores somewhere



gps <- readr::read_csv('../DriftWatch/GPS_CSV/ADRIFT_006_GPS.csv')
gps <- ad6
gps <- gps[-(1:2),]
gps <- readr::read_csv('testData/ADRIFT_017_GPS.csv')
aisFiles <- downloadMarCadAIS(gps, outDir = 'AIS', overwrite = FALSE, unzip=TRUE)

library(data.table)


subsetMarCadAIS('AIS', 'AIS_West', name='West_', overwrite = FALSE,
                latRange=c(20,50),
                lonRange=c(-140, -110))
# ais <- gpsToAis(gps, 'AIS_West', buffer=10e3)
ais <- readLocalAIS(gps, 'AIS_West', distance=10e3)
bb <- readr::read_csv('testData/ADRIFT_017_BB_mean_2min.csv')
colnames(bb)[1] <- 'UTC'
# range(bb$UTC)
bb <- PAMpal::addGps(bb, gps)
bb_ais <- addAIS(bb, ais, interpType='none')
gps_ais <- addAIS(gps, ais, interpType='none')
hm <- lapply(split(gps_ais, gps_ais$UTC), function(time) {
    dists <- time$shipDist
    inDist <- dists < 10e3
    inDist[is.na(inDist)] <- FALSE
    nShips <- sum(inDist)
    meanDist <- mean(dists[inDist])
    meanSOG <- mean(time$SOG[inDist])
    closest <- which.min(dists[inDist])
    if(length(closest) != 0) {
        closeDist <- dists[inDist][closest]
        closeSOG <- time$SOG[inDist][closest]
    } else {
        closeDist <- NA
        closeSOG <- NA
    }

    list(UTC=time$UTC[1], nShips=nShips, meanDist=meanDist, meanSOG=meanSOG,
         closeDist=closeDist, closeSOG=closeSOG)
})
hm <- bind_rows(hm)
doRescale <- function(x, target) {
    x <- (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))
    x * diff(range(target, na.rm=TRUE)) + min(target, na.rm=TRUE)
}

hm <- bind_rows(hm)
bb <- left_join(bb, hm, by=join_by('UTC' == 'UTC'))
bb$plotVal <- bb$`BB_100-24000`
bb$shipScale <- doRescale(bb$nShips, bb$plotVal)
bb$sogScale <- doRescale(c(0, bb$closeSOG), bb$plotVal)[-1]
bb$distScale <- doRescale(bb$meanDist, bb$plotVal)
library(patchwork)
(ggplot(bb, aes(x=UTC)) +
    geom_line(aes(y=plotVal)) +
    geom_line(aes(y=shipScale), col='red') +
    geom_line(aes(y=sogScale), col='blue') )/

ggplot(ais) +
    geom_path(aes(x=UTC, y=buoyDist, group=MMSI, col=vesselType)) +
    ylim(0, 10e3) +
    xlim(min(bb$UTC), max(bb$UTC))

hm <- matchGFS(gps[1:2,])
# this looks fucked because GPS are far apart relative to time ships near by
# so matching to gps coords makes very few ship path points v far apart
# gps_wais <- aisToGps(gps, ais)
# gps_wais <- addAIS(gps, ais, doInterp = FALSE)
# gps_interpais <- addAIS(gps, ais, doInterp = TRUE)
ga_no <- addAIS(gps, ais, interpType = 'none')
ga_close <- addAIS(gps, ais, interpType = 'close', interpTime = 120)
ga_all <- addAIS(gps, ais, interpType = 'all', interpTime = 120)

baseGps <- ggplot(data=gps, aes(x=Longitude, y=Latitude), col='black') +
    geom_path() +
    xlim(min(gps$Longitude), max(gps$Longitude)) +
    ylim(min(gps$Latitude), max(gps$Latitude))
library(patchwork)
# paths col by inDist
(baseGps +
    geom_path(data=ga_no, aes(x=shipLong, y=shipLat, group=factor(MMSI), color=shipDist < 10e3)) +
        geom_point(data=ga_no, aes(x=shipLong, y=shipLat, color=shipDist < 10e3)) +
    ggtitle('NoInterp')) /
(baseGps +
    geom_path(data=ga_close, aes(x=shipLong, y=shipLat, group=factor(MMSI), color=shipDist < 10e3)) +
     geom_point(data=ga_close, aes(x=shipLong, y=shipLat, color=shipDist < 10e3)) +
    ggtitle('CloseInterp')) /
(baseGps +
    geom_path(data=ga_all, aes(x=shipLong, y=shipLat, group=factor(MMSI), color=shipDist < 10e3)) +
     geom_point(data=ga_all, aes(x=shipLong, y=shipLat, color=shipDist < 10e3)) +
    ggtitle('AllInterp'))
# points same
(baseGps +
        geom_point(data=ga_no, aes(x=shipLong, y=shipLat, color=shipDist < 10e3)) +
        ggtitle('NoInterp')) /
    (baseGps +
         geom_point(data=ga_close, aes(x=shipLong, y=shipLat, color=shipDist < 10e3)) +
         ggtitle('CloseInterp')) /
    (baseGps +
         geom_point(data=ga_all, aes(x=shipLong, y=shipLat, color=shipDist < 10e3)) +
         ggtitle('AllInterp'))

aisSummary <- ais %>%
    filter(!is.na(inDist)) %>%
    group_by(MMSI, group, inDist, vesselType) %>%
    summarise(activeHours = difftime(max(UTC), min(UTC), units='hours'),
              meanSOG = mean(SOG),
              length = mean(vesselLength)) %>%
    filter(inDist) %>%
    ungroup()
# check time between points for potential gaps
filter(ais, inDist) %>%
    mutate(ix = paste0(MMSI, '_', group)) %>%
    split(.$ix) %>%
    lapply(function(x) {
        x$tDiff <- 0
        if(nrow(x) == 1) return(x)
        x$tDiff[2:nrow(x)] <- as.numeric(x$UTC[2:nrow(x)]) -
            as.numeric(x$UTC[1:(nrow(x)-1)])
        x
    }) %>%
    rbindlist %>%
    ggplot(aes(x=UTC, y=tDiff)) + geom_point()
# plot of vessel dist over time
ggplot(ais) +
    geom_path(aes(x=UTC, y= buoyDist, group=MMSI, color=vesselType)) +
    ylim(0, 10e3)
bbox <- st_as_sf(gps[,c('Longitude', 'Latitude', 'UTC')],
         coords=c('Longitude', 'Latitude'), crs=4326) %>%
    st_bbox()
# bbox <- st_bbox(gps)
library(patchwork)
# plot of vessel paths over drift colored by inDist
byIndist <- ggplot() +
        geom_path(data=ais, aes(x=Longitude, y=Latitude, col=factor(inDist), group=MMSI)) +
        geom_path(data=filter(gps, UTC <= max(ais$UTC)), aes(x=Longitude, y=Latitude)) +
        lims(x=c(bbox['xmin'], bbox['xmax']), y=c(bbox['ymin'], bbox['ymax']))
byAis <- ggplot() +
    geom_path(data=ais, aes(x=Longitude, y=Latitude, col=factor(MMSI), group=MMSI)) +
    geom_path(data=filter(gps, UTC <= max(ais$UTC)), aes(x=Longitude, y=Latitude)) +
    lims(x=c(bbox['xmin'], bbox['xmax']), y=c(bbox['ymin'], bbox['ymax']))
byIndist/byAis
# dist 2 buoy across time U-plot
ggplot() +
    geom_path(data=ais, aes(x=UTC, y=buoyDist, col=factor(MMSI))) +
    lims(y=c(0, 10e3))

