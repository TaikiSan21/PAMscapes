# HMD with Detections plotting devel
# TOL with detections examples??
tolDir <- '../Data/FinTOL'
tolFiles <- list.files(tolDir, recursive=T, full.names=T)
detFile <- '../ADRIFT_Report/data/humpback/humpbackDataFinal.rds'
detData <- readRDS(detFile)
str(detData)
library(lubridate)
binPerUnits <- function(x, presence='1hour', time='1day') {
    x$presBin <- floor_date(x$UTC, unit=presence)
    x$timeBin <- floor_date(x$UTC, unit=time)
    x$UTC <- NULL
    distinct(x) %>%
        group_by(timeBin, species, call, DriftName) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        mutate(units=paste0(presence, '/', time))
}

hm <- binPerUnits(detData)
str(hm)
max(table(hm$DriftName)) #ad5, ad48,
table(hm$DriftName)[gsub('^([A-z]*_[0-9]{1,3}).*$', '\\1', basename(tolFiles))]
# 49 and 52 have 6, thats #8, 11

##### testing dets per hour vs TOL on scaled timesries plot ####
tolData <- checkSoundscapeInput(grep('ADRIFT_049', tolFiles, value=T))
plotLTSA(tolData, bin='2min')
bin <- '1hour'
plotData <- filter(detData, DriftName == 'ADRIFT_049') %>%
    mutate(binTime = floor_date(UTC, unit=bin)) %>%
    group_by(binTime) %>%
    mutate(nDet = n()) %>%
    select(-UTC, -end, -call) %>%
    ungroup() %>%
    distinct()

tolData %>%
    mutate(binTime=floor_date(UTC, unit=bin)) %>%
    left_join(plotData) %>%
    mutate(nDet = ifelse(is.na(nDet), 0, nDet)) %>%
    plotScaledTimeseries(columns=c('TOL_500', 'nDet'), relMax=1,
                         title='Humpback Detections Per Hour')

plotLTSA(tolData, bin='2min')
#### testing scene on LTSA plot ####
freqMap <- tribble(
    ~type, ~freqMin, ~freqMax, ~color,
    'beaked whale', 25e3, 60e3,'darkgreen',
    'blue', 15, 25,'black',
    'dolphin', 10e3, 25e3,'blue',
    'fin', 20, 50,'red',
    'gray', 100, 2e3,'gray',
    'humpback', 100, 2e3, 'black',
    'minke', 1e3, 2e3,'black',
    'NBHF', 80e3, 120e3,'black',
    'sei', 50, 500,'black',
    'ship', 100, 1e3,'red',
    'sperm', 1e3, 20e3,'black'
)
plotData <- filter(detData, DriftName == 'ADRIFT_049')
allDets <- data.frame(data.table::fread('../ADRIFT_Report/data/AllDetections_wGPS.csv'))
plotData <- filter(allDets, DriftName == 'ADRIFT_049')
detRename <- tribble(
    ~old, ~new,
    'Anthro', 'ship',
    'Mn', 'humpback',
    'Bp', 'fin',
    'Bm', 'blue',
    'B. borealis', 'sei',
    'Lo', 'dolphin',
    'Gg', 'dolphin',
    'UO', 'dolphin',
    'Ba', 'minke',
    'Pm', 'sperm',
    'BB', 'beaked whale',
    'BW', 'beaked whale',
    'BW43', 'beaked whale',
    'ZC', 'beaked whale',
    'MS', 'beaked whale',
    'MC', 'beaked whale',
    'BWC', 'beaked whale',
    'Er', 'gray'
)
for(i in 1:nrow(detRename)) {
    plotData$species[plotData$species == detRename$old[i]] <- detRename$new[i]
}
table(plotData$species)
g <- plotData %>%
    filter(!species %in% c('fin', 'beaked whale')) %>%
    plotAcousticScene(freqMap=freqMap, bin='30min',
                      title='Combined LTSA/Acoustic Scene',
                      add = plotLTSA(tolData, bin='2min', alpha=.9))
ggsave(g, filename='LTSASceneAD49.png', width=10, height=6)
# ok how to pair different timescales of detections and HMD
# probably want to have granular for HMD and wider for binning
# detections? idk

##### ltsa dets
tolDir <- '../Data/FinTOL'
tolFiles <- list.files(tolDir, recursive=T, full.names=T)
tolData <- checkSoundscapeInput(grep('ADRIFT_049', tolFiles, value=T))
allDets <- data.frame(data.table::fread('../ADRIFT_Report/data/AllDetections_wGPS.csv'))
plotData <- filter(allDets, DriftName == 'ADRIFT_049')
detRename <- tibble::tribble(
    ~old, ~new,
    'Anthro', 'ship',
    'Mn', 'humpback',
    'Bp', 'fin',
    'Bm', 'blue',
    'B. borealis', 'sei',
    'Lo', 'dolphin',
    'Gg', 'dolphin',
    'UO', 'dolphin',
    'Ba', 'minke',
    'Pm', 'sperm',
    'BB', 'beaked whale',
    'BW', 'beaked whale',
    'BW43', 'beaked whale',
    'ZC', 'beaked whale',
    'MS', 'beaked whale',
    'MC', 'beaked whale',
    'BWC', 'beaked whale',
    'Er', 'gray'
)

for(i in 1:nrow(detRename)) {
    plotData$species[plotData$species == detRename$old[i]] <- detRename$new[i]
}
freqMap <- tribble(
    ~type, ~freqMin, ~freqMax, ~color,
    'dolphin', 10e3, 25e3,'blue',
    'humpback', 100, 2e3, 'white',
    'ship', 100, 1e3,'red'
)
library(PAMscapes)
library(dplyr)
ltsaPlot <- plotLTSA(tolData, bin='2min', title='Combined LTSA/Acoustic Scene', alpha=.9)
ltsaPlot
ltsaDets <- plotData %>%
    filter(species %in% c('dolphin', 'humpback', 'ship')) %>%
    plotAcousticScene(freqMap=freqMap, bin='30min',
                      add = ltsaPlot)
ltsaDets
