library(banter)
library(rfPermute)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(readr)
library(PAMpal)
# whole process of making banter data and fitting the model
# so that stuff can be easily run
fitSelPredBanter <- function(study, preds, angleMin = 0, selMin = 0, 
                             selPredMin=0, ntree=5e3, importance=importance) {
    bntData <- makeBanterData(study, preds, 
                              angleMin=angleMin, 
                              selMin=selMin, 
                              selPredMin=selPredMin)
    bntMdl <- initBanterModel(bntData$events)
    bntMdl <- addBanterDetector(bntMdl, bntData$detectors, ntree=ntree, sampsize=2, importance=importance)
    bntMdl <- runBanterModel(bntMdl, ntree=ntree, sampsize=5)
    bntMdl
}
# create event level CV measures, optionally with angle/selection filters
makeSelEv <- function(preds, angleMin = 0, selMin = 0, selPredMin=0) {
    if(is.character(preds)) {
        preds <- readr::read_csv(preds)
    }
    if(selPredMin > 0) {
        preds <- filter(preds,
                        maxSelPred > selPredMin)
    }
    preds %>% 
        filter(angle > angleMin,
               sel_prob > selMin) %>% 
        group_by(eventId, eventLabel) %>% 
        summarise(mean0 = mean(p0),
                  mean1 = mean(p1),
                  mean2 = mean(p2),
                  mean3 = mean(p3),
                  mean4 = mean(p4),
                  sd0 = sd(p0),
                  sd1 = sd(p1),
                  sd2 = sd(p2),
                  sd3 = sd(p3),
                  sd4 = sd(p4),
                  meanSel = mean(sel_prob),
                  selmean0 = mean(p0 * sel_prob),
                  selmean1 = mean(p1 * sel_prob),
                  selmean2 = mean(p2 * sel_prob),
                  selmean3 = mean(p3 * sel_prob),
                  selmean4 = mean(p4 * sel_prob)
        )%>% 
        ungroup()
}
# read preds data and create common new vars and banter-style call.id for joins
formatPreds <- function(preds, extraCols=NULL, maxSel=FALSE) {
    if(is.character(preds)) {
        preds <- readr::read_csv(preds)
    }
    predCols <- c('p0', 'p1', 'p2', 'p3', 'p4', 'sel_prob', 
                  'UID', 'Channel', 'eventId', 'eventLabel', 
                  'angle', extraCols)
    preds <- preds[predCols] %>% 
        mutate(call.id = paste0('C', Channel, eventId, UID),
               selp0 = sel_prob * p0,
               selp1 = sel_prob * p1,
               selp2 = sel_prob * p2,
               selp3 = sel_prob * p3,
               selp4 = sel_prob * p4)
    # preds <- PAMpal:::dropCols(preds, c('UID', 'Channel', 'eventId'))
    if(maxSel) {
        preds <- preds %>% 
            pivot_longer(cols=paste0('selp', 0:4)) %>%
            group_by(call.id) %>% 
            mutate(maxSelPred = value[which.max(value)]) %>% 
            ungroup() %>% 
            pivot_wider(names_from=name, values_from=value)
    }
    preds
}
# create the export_banter output with additional CV measures attached
# and apply any angle/selection filtering
makeBanterData <- function(study, preds, angleMin=0, selMin=0, channel=c('1'),
                           selPredMin=0) {
    study <- filter(study, Channel %in% channel)
    origSp <- species(study)
    if(angleMin != 0) {
        study <- filter(study, angle > angleMin)
    }
    preds <- formatPreds(preds, maxSel=selPredMin > 0)
    predEv <- makeSelEv(preds, angleMin=angleMin, selMin=selMin, selPredMin = selPredMin)
    predEv$eventLabel <- NULL
    bntData <- export_banter(study, 
                             dropSpecies = c('?BW', 'BW', 'PM', 'NBHF'),
                             dropVars = c('Latitude', 'Longitude', 'eventLabel', 'sst_mean',
                                          'sea_floor_depth_mean', 'magnitude_gradient_mean'),
                             verbose = FALSE)
    
    preds <- PAMpal:::dropCols(preds, c('UID', 'Channel', 'eventId', 'eventLabel'))
    for(d in seq_along(bntData$detectors)) {
        bntData$detectors[[d]] <- left_join(bntData$detectors[[d]], preds, by='call.id')
        bntData$detectors[[d]] <- bntData$detectors[[d]][!is.na(bntData$detectors[[d]]$sel_prob), ]
        bntData$detectors[[d]] <- filter(bntData$detectors[[d]], sel_prob > selMin)
        if(selPredMin > 0) {
            bntData$detectors[[d]] <- filter(bntData$detectors[[d]], maxSelPred > selPredMin)
        }
    }
    allEvs <- unique(unlist(sapply(bntData$detectors, function(x) unique(x$event.id))))
    lostEvs <- names(origSp)[!names(origSp) %in% allEvs]
    cat('Dropped species:\n')
    print(table(origSp[lostEvs]))
    bntData$events <- filter(bntData$events, event.id %in% allEvs)
    bntData$events <- repPredNa(
        left_join(bntData$events, predEv, by=join_by('event.id' == 'eventId'))
    )
    bntData
}
# some ev level params were not calculated for all events (filtering)
# so we need to replace with dummy vals. x is a df.
repPredNa <- function(x) {
    withZero <- c(paste0('sd', 0:4), 'meanSel', paste0('selmean', 0:4))
    with20 <- paste0('mean', 0:4)
    for(n in withZero) {
        if(!n %in% colnames(x)) {
            next
        }
        x[[n]][is.na(x[[n]])] <- 0
    }
    for(n in with20) {
        if(!n %in% colnames(x)) {
            next
        }
        x[[n]][is.na(x[[n]])] <- 0.2
    }
    x
}
# from banter model or pred df get the max
# prediction across classes and diff12 value
getMaxPred <- function(bnt) {
    spCols <- c('BB', 'BW43', 'BW39V', 'MS', 'NotBW', 'ZC')
    if(inherits(bnt, 'banter_model')) {
        cp <- casePredictions(bnt@model)
    } else if(is.data.frame(bnt) &&
              all(c('event.id', 'predicted', spCols) %in% colnames(bnt))) {
        cp <- bnt
        cp <- rename(cp, id=event.id, is.correct=correct)
    }
    cpLong <- pivot_longer(cp, cols=spCols[spCols %in% colnames(cp)])
    cpMax <- group_by(cpLong, id) %>% 
        mutate(maxPred = max(value),
               diff12 = diff(sort(value, decreasing=TRUE)[2:1])) %>% 
        ungroup() %>% 
        select(all_of(c('id', 'original', 'predicted', 'is.correct', 'maxPred', 'diff12'))) %>% 
        distinct()
    cpMax
}
# plots prediction scores colored by correctness
plotBanterScores <- function(bnt) {
    cpMax <- getMaxPred(bnt)
    (ggplot(cpMax, aes(x=maxPred, fill=is.correct)) + 
            geom_histogram(binwidth=.02) +
            # geom_density(alpha=.5) +
            facet_wrap(~predicted, scales='free') +
            xlim(0,1)) /
        (ggplot(cpMax, aes(x=diff12, fill=is.correct)) +
             geom_histogram(binwidth=.02) +
             # geom_density(alpha=.5) +
             facet_wrap(~predicted, scales='free') +
             xlim(0,1))
}
# prints num events reviewed/ accepted at given diff12 threshold
reviewSummary <- function(bnt, thresh=c(0.5, .25, .25), verbose=TRUE) {
    # thresh is BW, ZC, NotBW
    if(inherits(bnt, 'banter_model')) {
        predVals <- getMaxPred(bnt)
    } else if(is.data.frame(bnt) &&
              all(c('predicted', 'diff12') %in% colnames(bnt))) {
        predVals <- bnt
    }
    zcReview <- table(filter(predVals, predicted == 'ZC')$diff12 > thresh[2])
    notbwReview <- table(filter(predVals, predicted == 'NotBW')$diff12 > thresh[3])
    bwReview <- table(filter(predVals, !predicted %in% c('ZC', 'NotBW'))$diff12 > thresh[1])
    predVals$review <- FALSE
    predVals$review[predVals$predicted == 'ZC'] <- predVals$diff12[predVals$predicted == 'ZC'] < thresh[2]
    predVals$review[predVals$predicted == 'NotBW'] <- predVals$diff12[predVals$predicted == 'NotBW'] < thresh[3]
    predVals$review[!predVals$predicted %in% c('ZC', 'NotBW')] <- predVals$diff12[!predVals$predicted %in% c('ZC', 'NotBW')] < thresh[1]
    if(verbose) {
        print(table(predVals$review, predVals$predicted))
    }
    invisible(predVals)
}
# adds Anne Simonis' event level measures to acousticstudy
# doSpec is flag to do avg spec based params
# ... is filters for filtering down data before calculating params
# data is study
addASMeasures <- function(data, doSpec=TRUE, ...) {
    datafilt <- filter(data, ...)
    #Keep click detections with 10dB bandwidth <35 kHz and duration<1000
    clicks <- getClickData(datafilt)
    EventSum <- clicks %>%
        group_by(eventId) %>%
        reframe(PeakVar = var(peak),
                AngleVar = var(angle), 
                Angle10 = as.numeric(quantile(angle, .1)),
                BWVar = var(BW_3dB),
                ICI10 =  as.numeric(quantile(All_ici, .1)))
    
    TroughSum <- clicks %>%
        group_by(eventId) %>%
        filter(trough > 0) %>%
        reframe(Trough10 = as.numeric(quantile(trough, .1)))
    EventSum <- left_join(EventSum, TroughSum)
    EventSum$Trough10[is.na(EventSum$Trough10)] <- 0
    data <- data[EventSum$eventId]
    data <- addMeasures(data, EventSum, replace=TRUE)
    slope20_30 <- NA
    slope30_40 <- NA
    eventId < NA
    eventLabel <- NA
    if(isTRUE(doSpec)) {
        pb <- txtProgressBar(min=0, max=length(events(datafilt)), style=3)
        for(e in 1:length(datafilt@events)) {
            Spec <- calculateAverageSpectra(datafilt,e,sr=288e3, wl=720,filterfrom_khz=10, filterto_khz=NULL,plot=FALSE)
            Ind20kHz <- which(Spec$freq==20000)
            Ind30kHz <- which(Spec$freq==30000)
            Ind40kHz <- which(Spec$freq==40000)
            
            #Average Slope between 20 and 40 kHz
            rise <- diff(Spec$avgSpec[Ind30kHz:Ind20kHz])
            run <- diff(Spec$freq[Ind30kHz:Ind20kHz])
            slope20_30[e] <- 1000*mean(rise/run)
            
            #Average Slope between 30 and 40 kHz
            rise <- diff(Spec$avgSpec[Ind40kHz:Ind30kHz])
            run <- diff(Spec$freq[Ind40kHz:Ind30kHz])
            slope30_40[e] <- 1000*mean(rise/run)
            
            eventId[e] <- datafilt@events[[e]]@id
            eventLabel[e] <- datafilt@events[[e]]@species$id[1]
            setTxtProgressBar(pb, value=e)
        }
        
        EventSpec <- data.frame(eventId,slope20_30,slope30_40)
        data <- data[EventSpec$eventId]
        data <- addMeasures(data, EventSpec, replace=TRUE)
    }
    data
}