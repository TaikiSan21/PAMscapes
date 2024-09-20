data <- checkSoundscapeInput(c('testData/NRS11/NRS11_20200201.nc',
                               'testData/NRS11/NRS11_20200202.nc',
                               'testData/NRS11/NRS11_20200203.nc'))
data$day <- lubridate::day(data$UTC)
data$day <- NULL
olData <- createOctaveLevel(data, 'tol')
gpsdq <-plotPSD(data, style='quantile', title='plotPSD', q=.05)
gpsdq
gpsdden <-plotPSD(data, style='density', title='plotPSD')
gpsdden

ghl <- plotHourlyLevel(data, title='plotHourlyLevel')
ghl
gltsa <- plotLTSA(data, title='plotLTSA')
gltsa
olData <- checkSoundscapeInput('testData/ADRIFT_017_OL_sim.csv')
gps <- read_csv(here('tutorial/ADRIFT_017_GPS.csv'), show_col_types = FALSE)
olData$OL_250[2163:2182] <- 95 + runif(20, min=-5, max=5)
olData <- PAMpal::addGps(olData, gps, thresh = 4*3600)
olData <- matchEnvData(olData, nc='jplMURSST41', var='analysed_sst')
olData <- dplyr::rename(olData, 'SST'='analysed_sst_mean')
gmts <- plotScaledTimeseries(olData, columns = c('OL_250', 'SST'), title='plotScaledTimeseries',
                             lwd=c(.3, .5))

gtsline <- plotTimeseries(olData, column='OL_250', q=.05, title='plotTimeseries', style='line')
gtsline
gtsheat <- plotTimeseries(olData, column='OL_250', q=.05, title='plotTimeseries', style='heatmap')
gtsheat
ggplot2::ggsave(gtsheat, file='inst/images/ts-heat-ex.png', width=500, height=400, units='px', dpi=100)
ggplot2::ggsave(gmts, file='inst/images/mts-ex.png', width=500, height=400, units='px', dpi=100)
ggplot2::ggsave(gtsline, file='inst/images/ts-line-ex.png', width=500, height=400, units='px', dpi=100)
ggplot2::ggsave(gltsa, file='inst/images/ltsa-ex.png', width=500, height=400, units='px', dpi=100)
ggplot2::ggsave(gpsdq, file='inst/images/psd-q-ex.png', width=500, height=400, units='px', dpi=100)
ggplot2::ggsave(gpsdden, file='inst/images/psd-den-ex.png', width=500, height=400, units='px', dpi=100)
ggplot2::ggsave(ghl, file='inst/images/hourlev-ex.png', width=500, height=400, units='px', dpi=100)


maxPix <- 6666
secDiff <- as.numeric(difftime(max(data$UTC), min(data$UTC), units='secs')) / maxPix
# second, minute, hour, day are valid options
if(secDiff > 86400) {
    value <- secDiff / 86400
    unit <- 'day'
} else if(secDiff > 3600) {
    value <- secDiff / 3600
    unit <- 'hour'
} else if(secDiff > 60) {
    value <- secDiff / 60
    unit <- 'minute'
} else if(secDiff <= 60) {
    value <- secDiff
    unit <- 'second'
}


secBin <- ceiling(value)
binString <- paste0(secBin, unit)
binPer <- PAMscapes:::unitToPeriod(binString)
binPer
binned <- lubridate::floor_date(data$UTC, unit=binPer)

lt800 <- plotLTSA(data, bin='1min')
lt1200 <- plotLTSA(data, bin='1min', maxBins=1200)
lt1200
l600 <- plotLTSA(data, bin='1min', maxBins=60)
l600
