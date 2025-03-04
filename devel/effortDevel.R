library(dplyr)
library(lubridate)
library(PAMscapes)
library(ggplot2)

ana <- read.csv('../Data/Acousdet/makara/Makara - Phase 2 Review - Organization-Specific Data - 20241121 - analyses.csv', stringsAsFactors = F)
dep <- read.csv('../Data/Acousdet/makara/Makara - Phase 2 Review - Organization-Specific Data - 20241121 - deployments.csv', stringsAsFactors = F)

anaToEffort <- list('deployment_code' = 'project',
                    'analysis_sound_source_codes' = 'species',
                    'analysis_start_datetime' = 'start',
                    'analysis_end_datetime' = 'end')

effort <- ana[names(anaToEffort)]
effort <- left_join(effort, distinct(dep[c('deployment_code', 'site_code')]), by='deployment_code')
names(effort) <- c(unlist(anaToEffort), 'site')
effort$start <- as.POSIXct(effort$start, '%Y-%m-%d %H:%M:%S', tz='UTC')
effort$end <- as.POSIXct(effort$end, '%Y-%m-%d %H:%M:%S', tz='UTC')
# off effort markings devel

### Step 1 - off effort boxers for plotters


library(lubridate)
eff <- data.frame(
    start=ymd(c('2021-03-05', '2021-03-06')),
    end = ymd(c('2021-04-05', '2021-03-21')),
    species=c(NA, 'whale'))
formatEffort(eff, columns='species')
sites <- unique(effort$site[effort$project %in% data$project])

thisEff <- formatEffort(filter(effort, site %in% sites), columns=c('site', 'species'),
                              range=c(min(data$UTC), max(data$end, na.rm=TRUE)), resolution='day')

as <- plotAcousticScene(filter(data, site %in% c('COX01', 'COX02', 'NS01', 'NS02', 'NS03', 'NS04', 'NS05')), by='site')
plotAcousticScene(filter(data, site %in% c('COX01', 'COX02', 'NS01', 'NS02', 'NS03', 'NS04', 'NS05')), by='site', combineYears = T) /
    plotAcousticScene(filter(data, site %in% c('COX01', 'COX02', 'NS01', 'NS02', 'NS03', 'NS04', 'NS05')), by='site', combineYears = F)
library(ggplot2)

as / (as + 
    geom_rect(data=filter(thisEff[-4], status=='off'), aes(xmin=start, xmax=end, ymin=0, ymax=10), color='grey'))
