preds24 <- readr::read_csv('../../beaker-wigner-class/adrift_test576/ADRIFT024_Wigner288_pred_orig.csv')
data24 <- readRDS('../Data/MTC_CV/ADRIFT_024Study.rds')

data24 <- addASMeasures(data24, doSpec=TRUE, duration < 1000, BW_10dB < 35, angle > 1.5)
saveRDS(data24, '../Data/MTC_CV/ADRIFT_024Study_SpecFilt.rds')
data24 <- readRDS('../Data/MTC_CV/ADRIFT_024Study_SpecFilt.rds')

bntModel <- readRDS('../Data/MTC_CV/ADRIFT_banter_oldSelBest.rds')
bntModelAngle <- readRDS('../Data/MTC_CV/ADRIFT_banter_oldSelBestAngle.rds')

bntData <- makeBanterData(data24, preds24)
bntPreds <- predict(bntModel, new.data=bntData)

bntDataAngle <- makeBanterData(data24, preds24, angleMin = pi/2)
bntPredsAngle <- predict(bntModelAngle, new.data=bntDataAngle)

preds24_813 <- readr::read_csv('../../beaker-wigner-class/adrift_test576/ADRIFT024_Wigner288_pred_813.csv')
bntData813 <- makeBanterData(data24, preds24)
bntPreds813 <- predict(bntBest, new.data=bntData813)

table(bntPreds$predict.df$predicted)
table(bntPredsAngle$predict.df$predicted)

library(RSQLite)
con <- dbConnect('../Data/MTC_CV/Databases/ADRIFT_024 - Copy.sqlite3', drv=SQLite())
ev <- dbReadTable(con, 'Click_Detector_OfflineEvents')
# This had 22 manual events accidentally left in at the beginning, so need to
# remove them to match up IDs to the MTC database I created
ev$eventId <- paste0('ADRIFT_024_MTC.OE', ev$Id - 22)
ev <- ev[-(1:22),]
dbDisconnect(con)
ev <- ev[c('Id', 'UID', 'UTC', 'eventType', 'nClicks', 'eventId')]
ev$eventType <- gsub(' ', '', ev$eventType)
ev <- left_join(ev, 
                getMaxPred(bntPreds813$predict.df)[c('id', 'predicted', 'maxPred', 'diff12')],
                by=join_by('eventId' == 'id'))
ev$predicted[is.na(ev$predicted)] <- 'NotBW'
ev$eventType[ev$eventType == 'FP'] <- 'NotBW'
ev$correct <- ev$eventType == ev$predicted
table(list('Manual'=ev$eventType, 'Banter'=ev$predicted))
ggplot(ev, aes(x=diff12, fill=correct)) +
    geom_histogram(binwidth=0.025) +
    facet_wrap(~predicted, scales='free') +
    xlim(-.1, 1.1) +
    geom_vline(xintercept=c(0.25, 0.5))

clicks <- getClickData(data_wGPS)
clicks %>% 
    select(species, eventId, UID) %>% 
    distinct() %>% 
    filter(species %in% c('ZC', 'MS', 'BB', 'NotBW', 'BW43', 'BW39V')) %>% 
    group_by(eventId, species) %>% 
    summarise(n=n()) %>% 
    ggplot(aes(x=n, fill=species)) +
    geom_histogram() +
    scale_x_log10()
