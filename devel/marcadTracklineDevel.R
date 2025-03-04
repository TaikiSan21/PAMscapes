# marcad trackline devel
library(RSQLite)
library(sf)
gpkg <- '../Data/trackline/AISVesselTracks2022.gpkg'
con <- dbConnect(gpkg, drv=SQLite())
dbListTables(con)
dbReadTable(con, 'gpkg_contents') # just row AISVesselTracks2023
dbReadTable(con, 'gpkg_data_columns') # nothing
dbReadTable(con, 'gpkg_geometry_columns')
dbReadTable(con, 'rtree_AISVesselTracks2022_Shape')
dbGetQuery(con, statement='SELECT * FROM AISVesselTracks2022 WHERE TrackStartTime <= "2022-01-02T00:00:12Z" LIMIT 5')
dbGetQuery(con, statement='SELECT * FROM rtree_AISVesselTracks2022_Shape LIMIT 5')



hm <- st_read('../Data/trackline/AISVesselTracks2023.gpkg', query = 'SELECT * FROM AISVesselTracks2023 WHERE MMSI == 1056261 LIMIT 20')
# hm$Length <- as.numeric(st_length(hm))
hm$Knots <- as.numeric(st_length(hm)) / hm$DurationMinutes / 60 * 1.944

# 19 points in 1119 minute track
# 9 points in 100 minute track
gps <- read_csv(here('tutorial/ADRIFT_017_GPS.csv'), show_col_types = FALSE)
range(gps$Longitude)
range(gps$Latitude)
range(gps$UTC)

wat <- st_read(gpkg, query =
                   '
SELECT * FROM AISVesselTracks2022 WHERE OBJECTID IN 
(SELECT id FROM rtree_AISVesselTracks2022_Shape WHERE minx < -124.53 AND maxx > -124.48 AND miny < 41.06 AND maxy > 40.69)
AND TrackStartTime <= "2022-04-29T00:00:12Z" AND TrackEndTime >= "2022-04-25T00:00:12Z"
')
wat$Distance <- st_length(wat)
wat$Knots <- as.numeric(wat$Distance) / wat$DurationMinutes / 60 * 1.944
attr(wat$TrackEndTime, 'tzone') <- 'UTC'
attr(wat$TrackStartTime, 'tzone') <- 'UTC'

plot(wat[c('Knots')], axes=T, xlim=range(gps$Longitude), ylim=range(gps$Latitude), key.pos=4, reset=FALSE, breaks=0:18)
lines(x=gps$Longitude, y=gps$Latitude, col='black')
plot(x=gps$Longitude, y=gps$Latitude, col='black', type='l')
plot(wat[1], axes=T, xlim=range(gps$Longitude), ylim=range(gps$Latitude), key.pos=4,add=T)

#ths works to cut multilines down to a buffered area
# o wait thats bad start time is no longer accurate
smol <- st_crop(wat, st_buffer(st_as_sfc(st_bbox(st_as_sf(gps, coords=c('Longitude', 'Latitude'), crs=st_crs(wat)))), dist=20e3))
plot(smol[1], axes=T)

readGpkgAIS <- function(gpkg, latRange, lonRange, timeRange) {
    if(inherits(timeRange, 'POSIXct')) {
        timeRange <- PAMscapes:::psxTo8601(timeRange)
    }
    con <- dbConnect(gpkg, drv=SQLite())
    on.exit(dbDisconnect(con))
    name <- dbReadTable(con, 'gpkg_contents')$identifier
    dataTable <- name
    shapeTable <- paste0('rtree_', name, '_Shape')
    dataQ <- paste0(
        'SELECT * FROM ', dataTable, ' WHERE OBJECTID IN',
        '(SELECT id FROM ', shapeTable, ' WHERE ',
        'minx <= ', lonRange[2], ' AND ',
        'maxx >= ', lonRange[1], ' AND ',
        'miny <= ', latRange[2], ' AND ',
        'maxy >= ', latRange[1], ') AND ',
        'TrackStartTime <= "', timeRange[2], '" AND ',
        'TrackEndTime >= "', timeRange[1], '"')
    result <- st_read(gpkg, query=dataQ)
    result$Distance <- st_length(result)
    result$Knots <- as.numeric(result$Distance) / result$DurationMinutes / 60 * 1.944
    attr(result$TrackStartTime, 'tzone') <- 'UTC'
    attr(result$TrackEndTime, 'tzone') <- 'UTC'
    result
}

estTrackTimes <- function(track, start, end) {
    points <- st_cast(track, 'POINT')
    dists <- st_distance(points[-1], points[-length(points)], by_element=TRUE)
    times <- approx(x=c(0, st_length(track)),
                    y=c(start, end),
                    xout=c(0, cumsum(dists)))
    as.POSIXct(times$y, origin='1970-01-01 00:00:00', tz='UTC')
}

torks <- readGpkgAIS(gpkg, range(gps$Latitude), range(gps$Longitude), range(gps$UTC))
plot(diff(estTrackTimes(torks$Shape[1], torks$TrackStartTime[1], torks$TrackEndTime[1])))
