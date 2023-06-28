# this works from 2015-2022
# 2009-2014 have different formats by Zone by Month. We should be zone 10/11 (maybe 9 if offshore)

# could easily write script that downloads this shit for all of our deployments to date, truncates
# ais to relevant area, and stores somewhere



ad6 <- readr::read_csv('../DriftWatch/GPS_CSV/ADRIFT_006_GPS.csv')
hm <- downloadAis(ad6, outDir = 'AISTest', overwrite = F, unzip=T)
gps <- ad6
gps <- gps[-(1:2),]
range(ad6$UTC)

library(data.table)


subsetAis('AISTest', 'AIS_West', name='West_')
# ais <- gpsToAis(gps, 'AIS_West', buffer=10e3)
ais <- readLocalAIS(gps, 'AIS_West', distance=10e3)
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

ggplot() +
    geom_path(data=gps, aes(x=Longitude, y=Latitude), col='black') +
    geom_path(data=ga_all, aes(x=shipLong, y=shipLat, group=factor(MMSI), color=factor(MMSI)), lwd=1) +
    # geom_point(data=ais, aes(x=Longitude, y=Latitude, shape=inDist)) +
    geom_segment(data=ga_all, aes(x=Longitude, xend=shipLong,
                                         y=Latitude, yend=shipLat), alpha=.1) +
    xlim(min(gps$Longitude), max(gps$Longitude)) +
    ylim(min(gps$Latitude), max(gps$Latitude))
# not sure whats goig wrong, but something is very bad with the times
waisPlot <- ggplot() +
    geom_path(data=gps, aes(x=Longitude, y=Latitude), col='black') +
    geom_path(data=filter(ga_no, shipDist < 10e3),
              aes(x=shipLong, y=shipLat, col=factor(MMSI)))
aisSummary <- ais %>%
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
    geom_path(aes(x=UTC, y= buoyDist, group=MMSI, color=type)) +
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

