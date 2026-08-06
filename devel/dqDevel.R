ncOrig <- '../Data/ncmod/original.nc'
ncMod <- '../Data/ncmod/modified.nc'

ncOrig <- '../Data/ncmod/NEFSC_GOM_202510_USTR09_20251026.nc'
library(ncdf4)
nc <- nc_open(ncOrig, write = TRUE)
# ok lets try making the DQ mat
# should be vars$quality_flag
inSize <- nc$var$psd$size
psd <- ncvar_get(nc, 'psd', start=c(1,1), count=c(-1, -1))
times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
freqs <- nc$dim$frequency$vals
dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
dqVar <-  ncvar_def(name='quality_flag', prec='byte', longname='Data quality flag', dim=nc$var$psd$dim, units='')
# nc <- ncvar_add(nc, dqVar)
# ncvar_put(nc, varid = dqVar, vals=dqMat)
nc_close(nc)

library(PAMscapes)
modData <- loadSoundscapeData(ncMod, keepQuals = c(1,2))
hm <- nc_open('tutorial/MANTAExample.nc')





dq <- matrix(0, nrow=8, ncol=10)
testFreqs <- head(freqs, 9) - .5
testTimes <- head(times, 11)
markDQMatrix(dq, freqs=testFreqs, times=testTimes,
             freqRange=list(c(13.5, 20), c(11, 11.1)),
             value=c(1, 2),
             timeRange=list(testTimes[c(3,5)]+c(1, 0),
                            testTimes[c(6,8)]+c(1, 0)))



#### Plotters ####
ncMod <- '../Data/ncmod/modified.nc'
g <- plotLTSA(loadSoundscapeData(ncMod, keepQuals = 1:4), bin='1min', maxBins=2e3)
library(ggplot2)
dq <- loadQuality(ncMod)
labels <- dq$freq
labels[labels < 1e3] <- round(labels[labels < 1e3], 0)
labels[labels >= 1e3] <- round(labels[labels >= 1e3], 0)
freqType <- PAMscapes:::checkFreqType(dq$freq)
labels <- paste0(freqType, '_', labels)
dqdf <- data.frame(t(dq$dq))
colnames(dqdf) <- labels
dqdf <- cbind(dq$time, dqdf)
names(dqdf)[1] <- 'UTC'
dqLong <- longQuality(dqdf)

g <- ggplot() +
    geom_rect(data=dqLong, aes(xmin=UTC,
                               xmax=UTCend,
                               ymin=freqLow,
                               ymax=frequency,
                               color=(value)), alpha=.5)
dq <- ggplot() +
    geom_rect(data=dqLong, aes(xmin=UTC,
                               xmax=UTCend,
                               ymin=freqLow,
                               ymax=frequency,
                               fill=as.character(value))) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_log10(expand=c(0,0))
ggplot() +
    geom_raster(data=dqLong, aes(x=UTC,
                               y=frequency,
                               fill=as.character(value)), interpolate=T) +
    scale_x_datetime(expand=c(0,0)) +
    scale_y_log10(expand=c(0,0)) +
    theme(legend.title = element_text(angle=90)) +
    guides(fill=guide_legend(title.position='right', title.hjust=.5))
library(patchwork)
g / dq

## we back in 2026 ----
ncOrig <- '../Data/DQ/NEFSC_GOM_202309_USTR01_20231029.nc'
data <- loadSoundscapeData(ncOrig)
annoFile <- '../Data/DQ/DailyAnnotations.csv'
anno <- readr::read_csv(annoFile)
intervals <- readr::read_csv('../Data/DQ/makara_intervals.csv') %>% 
    filter(deployment_code == 'NEFSC_GOM_202309_USTR01')
qs <- addDataQuality(ncOrig, annotation = intervals, reset=TRUE, test=F, updateUnknown = 1)
table(qs)
image(t(qs), useRaster = TRUE)
loadSoundscapeData(ncOrig, keepQuals = c(1)) %>% 
    plotLTSA(bin='30min')

nc <- nc_open(ncOrig)
nc
nc_close(nc)
# need to go directly from annotainos -> update NC file
# could be  button in the app, could happen later (if later, may need way to update paths)
# Need to make Makara -> Annotations, but do we also need Annotations -> Makara?
# Need to add some sort of metadata comment like "we messed with thsi file in PAMscapes"
# Add a plot quality check box to the app 
# Update all 2 flags to some other value
# More general global att editor/updater side of tool not just for this

ncDir <- '../Data/DQ'
library(PAMscapes)
anno <- readr::read_csv('../Data/DQ/DailyAnnotations.csv')
intervals <- readr::read_csv('../Data/DQ/makara_intervals.csv')
runDailyLTSAReview(ncDir)

# ncatt_get(nc, 0) lists all global atts
# ncatt_put(nc, 0, 'name', value)

# markDQ expects 3 lists of equal length (or 1, to repeat)

