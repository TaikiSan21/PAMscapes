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
    data$driftName <- driftName
    data
}))
saveRDS(adriftTol, 'ADRIFT_TOL.rds')
adriftTol <- readRDS('ADRIFT_TOL.rds')
noiseData <- read_xlsx(noiseLog)
noiseData <- rename(noiseData, start='Start time', end = 'End time')
formatNumber <- function(x) {
    outs <- as.character(x)
    outs[x < 10] <- paste0('0', outs[x < 10])
    outs[x < 100] <- paste0('0', outs[x < 100])
    outs
}
noiseData$DriftName <- paste0('ADRIFT_', formatNumber(noiseData$Drift))
adriftTol <- bind_rows(lapply(split(adriftTol, adriftTol$driftName), function(x) {
    thisLog <- filter(noiseData, DriftName == x$driftName[1])
    if(is.null(thisLog) || nrow(thisLog) == 0) {
        return(x)
    }
    # cat('Do be noise')
    x <- markNA(x[-25], na=thisLog[c('start', 'end')])
    x$driftName <- thisLog$DriftName[1]
    x
}))
getDepDetails <- function(x) {
    con <- dbConnect(x, drv=SQLite())
    on.exit(dbDisconnect(con))
    data <- dbReadTable(con, 'deploymentData')
    data
}
depDet <- getDepDetails('../DriftWatch/SPOTGPS_Logger.sqlite3')
# table(depDet$DeploymentSite[depDet$DriftName %in% adriftTol$driftName])
adriftTol <- left_join(adriftTol, depDet[c('DriftName', 'DeploymentSite')],
                       by=c('driftName' = 'DriftName'))
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
    factor(x, levels=c('MorroBay', 'Humboldt', 'Oregon', 'SanFrancisco'))
}

adriftTol$Region <- markRegion(adriftTol$DeploymentSite)
adriftTol$DeploymentSite <- NULL
markSeason <- function(x) {
    season <- c(rep('Winter', 2),
                rep('Upwelling', 4), 
                rep('Post-Upwelling', 5),
                'Winter')
    factor(season[month(x)], levels=c('Winter', 'Upwelling', 'Post-Upwelling'))
}


# bySeason <- split(adriftTol, markSeason(adriftTol$UTC))
byRegion <- split(adriftTol, adriftTol$Region)

sPlots <- vector('list', length=length(byRegion))
names(sPlots) <- names(byRegion)

for(i in seq_along(byRegion)) {
    bySeason <- split(byRegion[[i]], markSeason(byRegion[[i]]$UTC))
    thisPlots <- vector('list', length=length(byRegion))
    for(j in seq_along(bySeason)) {
        title <- paste0(names(byRegion)[i], '-', names(bySeason)[j])
        # if(nro
        thisPlots[[j]] <- plotPSD(bySeason[[j]][-c(25, 26)], style='density',
                            title = title, q=.05) +
            coord_cartesian(ylim=c(30, 145))
    }
    sPlots[[i]] <- reduce(thisPlots, `|`)
}

reduce(sPlots, `/`) + plot_layout(nrow=length(sPlots), ncol=1)

plotPSD(adriftTol, by='driftName')

whyBad <- filter(adriftTol, Region == 'Humboldt', markSeason(UTC) == 'Winter')
quiets <- c('ADRIFT_039', 'ADRIFT_040', 'ADRIFT_041')
whyBad <- filter(whyBad, driftName %in% quiets)
plotPSD(whyBad[-26], by='driftName', q=.05)
plotTimeseries(whyBad[-c(26)], column='TOL_250', by = 'driftName')
bads <- lapply(split(whyBad[-26], whyBad$driftName), function(x) {
    plotPSD(x[-25],, style='density', title=x$driftName[1])
})
reduce(bads, `|`) + plot_layout(nrow=2, ncol=2)
