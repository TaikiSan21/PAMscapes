# soundscape import
bb <- readr::read_csv('../Data/Soundscape/ADRIFT_017_BB_mean_2min.csv')
names(bb)[1] <- 'UTC'
bb$UTC <- bb$UTC + 7 * 3600
gps <- readr::read_csv('../Data/Soundscape/ADRIFT_017_GPS.csv')
ol <- readr::read_csv('../Data/Soundscape/ADRIFT_017_OL_mean_2min.csv')
names(ol)[1] <- 'UTC'
ol$UTC <- ol$UTC + 7 * 3600
tol <- readr::read_csv('../Data/Soundscape/ADRIFT_017_TOL_mean_2min.csv')
names(tol)[1] <- 'UTC'
tol$UTC <- tol$UTC + 7 * 3600
psd <- readr::read_csv('../Data/Soundscape/ADRIFT_017_PSD_mean_2min.csv')



wtf <- readr::read_delim('~/../Downloads/ADRIFT_017_PSD_pct01_2min.csv', delim=',', skip=0, n_max=2, col_names=T)

# not sure which mets are relevant / how to store so much nonsense
# first test - download wind/etc data to gps track, compare this to noise levels
source('devel/GFSFunctions.R')

gps <- matchGFS(gps)
write.csv(gps, file='../Data/Soundscape/ADRIFT_017_GPS.csv', row.names = FALSE)
library(dplyr)

hm <- left_join(gps, bb, join_by(closest(UTC >= UTC)))

joinSoundscape <- function(x, ss) {
    if('yyyy-mm-ddTHH:MM:SSZ' %in% colnames(ss)) {
        ss <- rename(ss, 'sUTC' = 'yyyy-mm-ddTHH:MM:SSZ')
    }
    if('UTC' %in% colnames(ss)) {
        ss <- rename(ss, 'sUTC' = 'UTC')
    }
    xRange <- range(x$UTC)
    sRange <- range(ss$sUTC)
    x$isGreater <- x$UTC >= sRange[1]
    x <- bind_rows(lapply(split(x, x$isGreater), function(d) {
        if(d$isGreater[1]) {
            d <- left_join(d, ss, join_by(closest(UTC >= sUTC)))
        } else {
            d <- left_join(d, ss, join_by(closest(UTC <= sUTC)))
        }
        d
    }))
    x$sUTC <- NULL
    x$isGreater <- NULL
    x
}

hm <- joinSoundscape(gps, bb)
hm <- joinSoundscape(hm, ol)
hm <- joinSoundscape(hm, tol)
hm$windMag <- sqrt(hm$windU^2 + hm$windV^2)
hm <- tidyr::gather(hm, 'Measure', 'Level', `BB_100-24000`:`TOL_20000`)
hm$UTC <- hm$UTC + 7 * 3600

library(ggplot2)
ggplot(hm, aes(x=windMag, y=Level, col=Measure)) +
    geom_line()

ggplot(hm, aes(x=precRate, y=Level, col=Measure)) +
    geom_line()

dbPlot <- ggplot(hm, aes(x=UTC, y=Level, col=Measure)) +
    geom_line() +
    xlim(range(hm$UTC))

library(patchwork)
windPlot <- ggplot(hm, aes(x=UTC, y=windMag)) +
    geom_line()

dbPlot/windPlot
hm$precRate <- hm$precRate * 3600 # converting from kg/m2/s to kg/m2/h == 1L per m2 per hr
coeff <- mean(hm$precRate) / mean(hm$windMag)

envPlot <- ggplot(hm, aes(x=UTC + 3 * 3600)) +
    geom_line(aes(y=windMag), col='darkgray') +
    geom_line(aes(y=precRate / coeff), col='blue') +
    scale_y_continuous(
        name = 'Wind Speed (m/s)',
        sec.axis = sec_axis(~.*coeff, name='Precipitation Rate (kg/m2/hr)')
    ) +
    theme(
        axis.title.y = element_text(color = 'darkgray', size=13),
        axis.title.y.right = element_text(color = 'blue', size=13)
    ) +
    xlim(range(hm$UTC))
dbPlot / envPlot
# not really sure what to do with PSDrange

ggplot(gps, aes(x=Longitude, y=Latitude, col=UTC)) +
    geom_point()

##### sanctsound nc files ####
data <- readRDS('../Data/Soundscape/adrift17study.rds')
library(PAMpal)
# SANCTSOUND nc file notes
# # bb OK, ships bad NC with useless dim, tol has frequency dim, psd has frequency dim, ol has frequnecy dim

library(sf)
gps <- gps(data)
# stgps <- st_as_sf(gps[,c('Longitude', 'Latitude', 'UTC')], coords=c('Longitude', 'Latitude'), crs=4326) %>%
#     st_buffer(dist=10e3)
# stgps
# plot(stgps)
# stgps <- st_buffer(stgps, dist=10e3) # dist is meters
# bbox <- st_bbox(stgps)
# bbox <- c(xmin=-140, xmax=-115, ymin=20, ymax=50)
library(data.table)
aisFiles <- list.files('../Data/Soundscape/', pattern='^AIS_', full.names=TRUE)
ais <- rbindlist(lapply(aisFiles, function(x) {
    tmp <- fread(x, select=c(LAT='numeric', LON='numeric', MMSI='integer',BaseDateTime='POSIXct',
                      Length='integer', VesselType='integer', SOG='numeric'), quote='')
    # print(str(tmp))
    colnames(tmp) <- c('Latitude', 'Longitude', 'MMSI', 'UTC', 'Length', 'VesselType', 'SOG')
    bbox <- c(xmin=-140, xmax=-115, ymin=20, ymax=50)
    tmp[Longitude >= bbox['xmin'] &
            Longitude <= bbox['xmax'] &
            Latitude >= bbox['ymin'] &
            Latitude <= bbox['ymax'] &
            UTC >= min(gps$UTC) &
            UTC <= max(gps$UTC), ]
}))

# get only tracks nearby
bbox <- st_as_sf(gps[,c('Longitude', 'Latitude', 'UTC')], coords=c('Longitude', 'Latitude'), crs=4326) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_buffer(dist=10e3) %>%
    st_bbox()
# bbox <- st_bbox(stgps)
mmsiNear <- unique(ais[Longitude >= bbox['xmin'] &
                           Longitude <= bbox['xmax'] &
                           Latitude >= bbox['ymin'] &
                           Latitude <= bbox['ymax'], 'MMSI'][[1]])

ais <- ais[MMSI %in% mmsiNear, ]
library(ggplot2)
setkeyv(ais, 'UTC')

# ggplot() +
#     geom_path(data=ais, aes(x=LON, y=LAT, col=factor(MMSI))) +
#     geom_path(data=gps, aes(x=Longitude, y=Latitude))
# QUESTIONS FOR THIS
# How do we deal with time component? V
ais$buoyLat <- approx(x=gps$UTC, y=gps$Latitude, xout=ais$UTC)$y
ais$buoyLon <- approx(x=gps$UTC, y=gps$Longitude, xout=ais$UTC)$y
ais$buoyDist <- geosphere::distGeo(matrix(c(ais$Longitude, ais$Latitude), ncol=2),
                                   matrix(c(ais$buoyLon, ais$buoyLat), ncol=2))
ais$inDist <- ais$buoyDist <= 10e3
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
    x$group <- cumsum(x$alternate)
    # x$alternate <- NULL
    x
}))

aisSummary <- ais %>%
    group_by(MMSI, group, inDist, type) %>%
    summarise(activeHours = difftime(max(UTC), min(UTC), units='hours'),
              meanSOG = mean(SOG),
              length = mean(Length)) %>%
    filter(inDist)
# plot of vessel dist over time
ggplot(ais) +
    geom_path(aes(x=UTC, y= buoyDist, group=MMSI, color=type)) +
    ylim(0, 10e3)
library(patchwork)
# plot of vessel paths over drift colored by inDist
(ggplot() +
    geom_path(data=ais, aes(x=Longitude, y=Latitude, col=factor(inDist), group=MMSI)) +
    geom_path(data=filter(gps, UTC <= max(ais$UTC)), aes(x=Longitude, y=Latitude)) +
    lims(x=c(bbox['xmin'], bbox['xmax']), y=c(bbox['ymin'], bbox['ymax']))) /
    ggplot() +
    geom_path(data=ais, aes(x=Longitude, y=Latitude, col=factor(MMSI), group=MMSI)) +
    geom_path(data=filter(gps, UTC <= max(ais$UTC)), aes(x=Longitude, y=Latitude)) +
    lims(x=c(bbox['xmin'], bbox['xmax']), y=c(bbox['ymin'], bbox['ymax']))
ggplot() +
    geom_path(data=ais, aes(x=UTC, y=buoyDist, col=factor(MMSI)))
