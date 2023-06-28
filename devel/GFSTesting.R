# testing GFS data

ncFiles <- list.files('../Data/GFSTest/Data/', full.names=TRUE)
# -127 to -117, 32 to 40, 2023-01-16 00:00:00 to 2023-01-17 12:00:00
library(lubridate)
df <- data.frame(Latitude = rep(seq(from=32, to=40, length.out=20), 20),
                 Longitude = rep(seq(from=-121, to=-117, length.out=20), each=20))
df$UTC <- rep(seq(from=as.POSIXct('2023-01-01 03:00:00', tz='UTC'),
                  to=as.POSIXct('2023-01-03 03:00:00', tz='UTC'),
                  length.out=20), 20)
library(PAMmisc)
ncToData(df, nc=ncFiles[7])

# only for 2015+
# YYYYMMDD/YYYYMMDD[00,06,12,18].f000-384 by3
file <- '../Data/GFSTest/gfs.0p25.2023010300.f003.grib2.nc'
url <- paste0('https://rda.ucar.edu/thredds/ncss/grid/files/g/ds084.1/',
              '2023/20230103/gfs.0p25.2023010300.f003.grib2?',
              'var=u-component_of_wind_height_above_ground&',
              'var=v-component_of_wind_height_above_ground&',
              'var=Precipitation_rate_surface_3_Hour_Average&',
              'var=Total_precipitation_surface_3_Hour_Accumulation',
              '&north=40&west=-121&east=-117&south=32&horizStride=1&',
              'time_start=2023-01-03T03:00:00Z&time_end=2023-01-03T03:00:00Z',
              '&&&accept=netcdf4-classic')
wind <- ncToData(df, file)
names(wind)[c(5, 6, 7, 8)] <- c('PrecRate', 'TotPrec', 'WindU', 'WindV')
wind$WindMag <- sqrt(wind$WindU^2 + wind$WindV^2)
ggplot(wind) +
    geom_tile(aes(x=Longitude, y=Latitude, fill=WindMag))

ggplot(wind) +
    geom_tile(aes(x=Longitude, y=Latitude, fill=PrecRate))

ggplot(wind) +
    geom_tile(aes(x=Longitude, y=Latitude, fill=TotPrec))

date <- df$UTC[1:3]
library(lubridate)
format(date[1], format='%Y/%Y%m%d/gfs.0p25.%Y%m%d')

library(httr)



hm <- matchGFS(df[1:2,])
tries <- seq(df$UTC[1], by=3600*3, length.out=15)
dateToGFS(tries)
for(i in seq_along(tries)) {
    dl <- GET(dateToGFS(tries[i]),
              write_disk(file.path('../Data/GFSTest/Newdl/',
                                   paste0(format(tries[i], '%Y%m%d_%H%M%S'), 'gfs.nc')),
                         overwrite = TRUE
              )
    )
    
        
}
tryFiles <- file.path('../Data/GFSTest/Newdl/', 
                      paste0(format(tries, '%Y%m%d_%H%M%S'), 'gfs.nc'))
tryDfs <- vector('list', length=length(tries))
library(profvis)
for(i in seq_along(tryDfs)) {
# profvis({
# for(i in 2){
    this <- df
    this$UTC <- tries[i]
    # this <- ncToData(this, nc=tryFiles[i], var=c('v-component_of_wind_height_above_ground',
    #                                              'u-component_of_wind_height_above_ground',
    #                                              'Precipitation_rate_surface'))
    this <- ncToData(this, nc=tryFiles[i],verbose = F)
    this$windMag <- sqrt(this$`u-component_of_wind_height_above_ground_mean`^2 +
                             this$`v-component_of_wind_height_above_ground_mean`^2)
    this$precRate <- this$Precipitation
    tryDfs[[i]] <- this
}
# })
View(tryDfs)
names(tryDfs) <- basename(tryFiles)
all <- bind_rows(lapply(tryDfs, function(x) {
    x[c('UTC', 'Longitude', 'Latitude', 'precRate', 'windMag')]
}), .id='file')
all$par <- as.character(rep(1:length(tryDfs), each=nrow(this)) %% 2)
ggplot(all, aes(x=precRate, col=par)) + geom_density() + xlim(0, .00005)
ggplot(all, aes(x=windMag, col=par)) + geom_density()

ggplot(all, aes(x=Longitude, y=Latitude, fill=windMag)) +
    geom_tile() +
    facet_wrap(~file)

ggplot(all, aes(x=Longitude, y=Latitude, fill=precRate)) +
    geom_tile() +
    facet_wrap(~file)

# NOTES data seems reasonable, access is still annoying bc of the time of day
# will need to break up drift dfs by 3 hour block and dl separately, wont
# fit in to matchEnvData neatly i think


# TP SET TP.nc4. everything is NaN except the average...i think is
# fukt with long time range
tpMax <- as.POSIXct('2021-07-20 12:00:00', tz='UTC')
paste0('https://rda.ucar.edu/thredds/ncss/grid/aggregations/g/ds084.1/1/TP?',
       'var=u-component_of_wind_height_above_ground&var=v-component_of_wind_height_above_ground&',
       'var=Precipitation_rate_surface&var=Precipitation_rate_surface_Mixed_intervals_Average',
       '&north=40&west=-121&east=-117&south=32&horizStride=1&',
       'time_start=2021-01-15T00:00:00Z&time_end=2021-01-16T12:00:00Z&&&accept=netcdf4-classic')
nc <- nc_open('../Data/GFSTest/TP.nc4')
tpDf <- df
tpDf$UTC <- rep(seq(from=as.POSIXct('2021-01-15 00:00:00', tz='UTC'),
                    to=as.POSIXct('2021-01-16 12:00:00', tz='UTC'),
                    length.out=20), 20)
tpDf <- ncToData(tpDf, '../Data/GFSTest/TP.nc4')

# TODO: buffer amt for this? not sure if spot est is best. Res is .25
# TODO: need to know if ok to DL 50 things at once or whatever
# not sure how this works with soundscape stuff - we dont have detections
# right so pair this so start/end times of something? drift track not really
# in PAMpal in a real connected way
