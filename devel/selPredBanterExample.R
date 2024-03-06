#### CREATING NEW MODELS ####
source('devel/selPredBanterFunctions.R')
newPreds <- readr::read_csv('../../beaker-wigner-class/preds/adrift_bads3pct/ADRIFT_Wigner_FixedCombined_pred11.csv')
preds <- readr::read_csv('../../beaker-wigner-class/adrift_test576/ADRIFT_Wigner_FixedCombined_pred.csv')
preds5_15 <- readr::read_csv('../../beaker-wigner-class/preds/adrift_bads5pct/ADRIFT_Wigner_FixedCombined_pred15.csv')
preds8_13 <- readr::read_csv('../../beaker-wigner-class/preds/adrift_bads8pct/ADRIFT_Wigner_FixedCombined_pred13.csv')
data_wGPS <- readRDS('../Data/MTC_CV/ADRIFT_studyFinal220.rds')
data_wGPS <- setSpecies(data_wGPS, 'pamguard')

library(ggplot2)
library(patchwork)
#### Looking at distribution of sel scores and num dropped ####
goodSp <- c('ZC', 'MS', 'NotBW', 'BW43', 'BB', 'BW39V')
(newPreds %>% 
        filter(eventLabel %in% goodSp) %>% 
        ggplot() +
        geom_density(aes(x=sel_prob, col=eventLabel)) +
        facet_wrap(~eventLabel, scales='free')) /
    
    (preds5_15 %>% 
         filter(eventLabel %in% goodSp) %>% 
         ggplot() +
         geom_density(aes(x=sel_prob, col=eventLabel)) +
         facet_wrap(~eventLabel, scales='free'))

selThresh <- 0.3
preds5_15 %>% 
    group_by(eventId, eventLabel) %>% 
    summarise(n=n(),
              nThresh = sum(sel_prob > selThresh)) %>% 
    View

##### new sel net model event level ####
newBantNS <- fitSelPredBanter(data_wGPS,
                              preds=preds8_13,
                              selMin=0,
                              angleMin=0,
                              ntree=5e3,
                              importance=TRUE)
newBantNS@model

##### sel filter ####
newBantSelEv <- fitSelPredBanter(data_wGPS,
                                 preds=preds5_15,
                                 selMin=0.3,
                                 angleMin=0,
                                 ntree=5e3,
                                 importance=TRUE)
newBantSelEv@model
##### try with angle cutoffs too i guess ####
bantNSAngle8_13 <- newBantNSAngle
newBantNSAngle <- fitSelPredBanter(data_wGPS,
                                   preds = preds,
                                   angleMin = pi/2,
                                   selMin = 0,
                                   ntree=5e3,
                                   importance=TRUE)
newBantNSAngle@model
newBantNSAngleSel <- fitSelPredBanter(data_wGPS,
                                      preds=preds5_15,
                                      angleMin = pi/2,
                                      selMin = 0.3,
                                      ntree=5e3, 
                                      importance=TRUE)
newBantNSAngleSel@model

newBantSelPredMin <- fitSelPredBanter(data_wGPS,
                                      preds=preds5_15,
                                      angleMin=0,
                                      selMin=0,
                                      ntree=5e3,
                                      importance=TRUE,
                                      selPredMin = .15)
newBantSelPredMin@model
plotBanterScores(newBantSelPredMin)
#### looking at predictions ####
ns <- plotBanterScores(newBantNS)

nSummary <- newPreds %>% 
    group_by(eventId) %>% 
    summarise(n=n(),
              nThresh = sum(sel_prob > selThresh))
cvEvNS <- makeSelEv(newPreds, angleMin=0, selMin=0)
cvEvNS <- left_join(cvEvNS, cp, by=join_by('eventId' == 'id')) %>% 
    left_join(nSummary, join_by('eventId' == 'eventId'))
View(cvEvNS)
ggplot(cvEvNS) +
    geom_histogram(aes(x=nThresh, fill=is.correct)) +
    facet_wrap(~predicted, scales='free')

#### PREDICTING ON NEW DATA ####
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
