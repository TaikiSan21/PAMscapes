# soundscapes ----
data <- loadSoundscapeData(c('testData/NRS11/NRS11_20200201.nc',
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
olData <- loadSoundscapeData('testData/ADRIFT_017_OL_sim.csv')
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

psd_by <- plotPSD(data, by='day', q=.05, title='plotPSD')
psd_by
ggplot2::ggsave(psd_by, file='inst/images/psd-by-ex.png', width=500, height=400, units='px', dpi=100)
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

# detections ----
data <- read.csv('../Data/Acousdet/all_sp_dataframe.csv', stringsAsFactors = F)

data <- loadDetectionData(x=data, source='csv', detectionType='presence', wide=TRUE, 
                         tz='UTC', presenceDuration='1day', extraCols=c('PROJECT_DESCRIPTION'), 
                         columnMap=list(UTC='StartDate',
                                        effortStart='DataStart',
                                        effortEnd='DataEnd'), 
                         speciesCols=c('NARW_Occur', 'Fin_Occur', 'Humpback_Occur', 'Blue_Occur',
                                       'Minke_Occur', 'Sei_Occur', 'Sperm_Occur', 'Dolphin_Occur', 'Porpoise_Occur'),
                         detectedValues='1')
data$species <- gsub('_Occur', '', data$species)
data$site <- gsub('_[0-9]{6}', '', data$PROJECT_DESCRIPTION)
data$effortStart[is.na(data$effortStart)] <- ymd('2022-11-03')
data$effortEnd[is.na(data$effortEnd)] <- ymd('2023-04-15')
data$effortEnd <- data$effortEnd + 86400

gboxplot <- data %>% 
    filter(species %in% c('Sei', 'Humpback', 'Dolphin', 'NARW'),
           year(UTC) >= 2021) %>% 
    plotDetectionBoxplot(group='site',facet='species', combineYears = T,
                         title='plotDetectionBoxplot') 
gboxplot
ggplot2::ggsave(gboxplot, file='inst/images/boxplot-ex.png', width=500, height=400, units='px', dpi=80)

freqMap <- dplyr::tribble(
    ~species, ~freqMin, ~freqMax,
    'Porpoise', 40e3, 100e3,
    'Dolphin', 3000, 20e3,
    'Minke', 1000, 2000,
    'Sperm', 100, 2000,
    'Humpback', 200, 1000,
    'Sei', 120, 300,
    'NARW', 35, 80,
    'Fin', 20, 40,
    'Blue', 10, 30,
)
gscene <- data %>% 
    filter(
    # species %in% c('Sei', 'Humpback', 'Dolphin', 'NARW'),
        year(UTC) >= 2021) %>% 
    plotAcousticScene(freqMap=select(freqMap, -freqMin, -freqMax),
                      title='plotAcousticScene')
gscene
ggplot2::ggsave(gscene, file='inst/images/scene-ex.png', width=500, height=400, units='px', dpi=100)


gfscene <- data %>% 
    select(-effortStart) %>% 
    filter(
        year(UTC) >= 2021) %>% 
    plotAcousticScene(freqMap=freqMap, title='plotAcousticScene')
gfscene
ggplot2::ggsave(gfscene, file='inst/images/freq-scene-ex.png', width=500, height=400, units='px', dpi=100)
