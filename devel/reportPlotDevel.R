# PAMscapes PSD plots
pascalDir <- 'X:/ANALYSIS/ADRIFT/Soundscape/metrics/'
tolFiles <- list.files(pascalDir, recursive=TRUE, full.names=TRUE, pattern='TOL_2min')
noiseLog <- 'X:/ANALYSIS/ADRIFT/Soundscape/Noise Logs/ADRIFTNoiseLogs.xlsx'
library(readxl)
library(patchwork)
library(RSQLite)
library(ggplot2)
library(PAMscapes)
library(lubridate)
library(dplyr)
library(purrr)


adriftTol <- bind_rows(lapply(tolFiles, function(x) {
    data <- checkSoundscapeInput(x)
    driftName <- basename(dirname(x))
    if(grepl('pst', driftName)) {
        data$UTC <- data$UTC + 7 * 24 * 3600
        driftName <- gsub('_pst', '', driftName)
    }
    data$DriftName <- driftName
    data
}))
saveRDS(adriftTol, 'ADRIFT_TOL.rds')
adriftTol <- readRDS('../Data/ReportPlots/ADRIFT_TOL.rds')
adriftTol <- rename(adriftTol, DriftName = driftName)
noiseData <- read_xlsx(noiseLog)
noiseData <- readRDS('../Data/ReportPlots/ADRIFT_NoiseLog.rds')
noiseData <- rename(noiseData, start='Start time', end = 'End time')
formatNumber <- function(x) {
    outs <- as.character(x)
    outs[x < 10] <- paste0('0', outs[x < 10])
    outs[x < 100] <- paste0('0', outs[x < 100])
    outs
}
noiseData$DriftName <- paste0('ADRIFT_', formatNumber(noiseData$Drift))
adriftTol <- bind_rows(lapply(split(adriftTol, adriftTol$DriftName), function(x) {
    thisLog <- filter(noiseData, DriftName == x$DriftName[1])
    if(is.null(thisLog) || nrow(thisLog) == 0) {
        return(x)
    }
    # cat('Do be noise')
    x <- markNA(x[-25], na=thisLog[c('start', 'end')])
    x$DriftName <- thisLog$DriftName[1]
    x
}))
getDepDetails <- function(x) {
    con <- dbConnect(x, drv=SQLite())
    on.exit(dbDisconnect(con))
    data <- dbReadTable(con, 'deploymentData')
    for(col in c('Start', 'End', 'DataStart', 'DataEnd')) {
        data[[col]] <- as.POSIXct(data[[col]], format='%Y-%m-%d %H:%M:%S', tz='UTC')
    }
    data
}
depDb <- '../DriftWatch/SPOTGPS_Logger.sqlite3'
depDet <- getDepDetails(depDb)
saveRDS(depDet, file='DeployDetails.rds')
depDet <- readRDS('../Data/ReportPlots/DeployDetails.rds')
# table(depDet$DeploymentSite[depDet$DriftName %in% adriftTol$driftName])
adriftTol <- left_join(adriftTol, depDet[c('DriftName', 'DeploymentSite')],
                       by=c('DriftName' = 'DriftName'))

markInData <- function(x, depDet) {
    x <- split(x, x$DriftName)
    bind_rows(lapply(x, function(d) {
        # browser()
        thisDep <- depDet[depDet$DriftName %in% d$DriftName[1], ]
        start <- ifelse(is.na(thisDep$DataStart), thisDep$Start, thisDep$DataStart)
        end <- ifelse(is.na(thisDep$DataEnd), thisDep$End, thisDep$DataEnd)
        d$inData <- d$UTC >= start &
            d$UTC <= end
        d
    }))
}

adriftTol <- markInData(adriftTol, depDet)
markRegion <- function(x) {
    # mby <- c('MBY', 'MOB', 'SF', 'SFB')
    # hum <- c('HUM', 'Crescent City')
    # ore <- c('ORE')
    # x[x %in% mby] <- 'MBY'
    # x[x %in% hum] <- 'HUM'
    # x[x %in% ore] <- 'ORE'
    # annes codes
    MorroBay<-c('MBY','MOB')
    Humboldt<-c('Crescent City','HUM')
    Oregon<-c('ORE')
    SanFrancisco<-c('HMB','SF','SFB')
    x[x %in% MorroBay] <- 'MorroBay'
    x[x %in% Humboldt] <- 'Humboldt'
    x[x %in% Oregon] <- 'Oregon'
    x[x %in% SanFrancisco] <- 'SanFrancisco'
    factor(x, levels=c('Oregon', 'Humboldt', 'SanFrancisco','MorroBay'))
}

adriftTol$Region <- markRegion(adriftTol$DeploymentSite)
adriftTol$DeploymentSite <- NULL
markSeason <- function(x) {
    season <- c(rep('Winter', 2),
                rep('Upwelling', 4),
                rep('Post-Upwelling', 5),
                'Winter')
    factor(season[month(x)], levels=c('Upwelling', 'Post-Upwelling', 'Winter'))
}


# bySeason <- split(adriftTol, markSeason(adriftTol$UTC))

byRegion <- split(filter(adriftTol, inData), filter(adriftTol, inData)$Region)
byRegion <- split(adriftTol, adriftTol$Region)

sPlots <- vector('list', length=length(byRegion))
names(sPlots) <- names(byRegion)

for(i in seq_along(byRegion)) {
    bySeason <- split(byRegion[[i]], markSeason(byRegion[[i]]$UTC))
    thisPlots <- vector('list', length=length(byRegion))
    for(j in seq_along(bySeason)) {
        title <- paste0(names(byRegion)[i], '-', names(bySeason)[j])
        # if(nro
        thisPlots[[j]] <- plotPSD(bySeason[[j]], style='density',
                            title = title, q=.05) +
            coord_cartesian(ylim=c(30, 145),
                            xlim=c(100, 20e3))
    }
    sPlots[[i]] <- reduce(thisPlots, `|`)
}
withFilter <- reduce(sPlots, `/`) + plot_layout(nrow=length(sPlots), ncol=1)
ggsave('WithFilter.png', plot=withFilter, width=12, height=16)
noFilter <- reduce(sPlots, `/`) + plot_layout(nrow=length(sPlots), ncol=1)
ggsave('NoFilter.png', plot=noFilter, width=12, height=16)
plotPSD(adriftTol, by='DriftName')

whyBad <- filter(adriftTol, Region == 'Humboldt', markSeason(UTC) == 'Winter')
quiets <- c('ADRIFT_039', 'ADRIFT_040', 'ADRIFT_041')
whyBad <- filter(adriftTol, DriftName %in% quiets)
plotPSD(whyBad, by='DriftName', q=.05)
plotTimeseries(whyBad, column='TOL_250', by = 'DriftName')
bads <- lapply(split(whyBad, whyBad$DriftName), function(x) {
    plotPSD(x,, style='density', title=x$DriftName[1])
})
reduce(bads, `|`) + plot_layout(nrow=2, ncol=2)
