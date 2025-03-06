ncOrig <- '../Data/ncmod/original.nc'
ncMod <- '../Data/ncmod/modified.nc'

library(ncdf4)
nc <- nc_open(ncMod, write = TRUE)
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

addDataQuality <- function(ncFile, freqRange=NULL, timeRange=NULL, value=2, verbose=FALSE) {
    nc <- nc_open(ncFile)
    on.exit(nc_close(nc))
    if(!'quality_flag' %in% names(nc$var)) {
        if(verbose) {
            cat('No "quality_flag" variable found, creating a new one')
        }
        inSize <- nc$var$psd$size
        dqVar <- ncvar_def('quality_flag', prec='byte', longname='Data quality flag', dim=nc$var$psd$dim, units='')
        nc <- ncvar_add(nc, dqVar)
        dqMat <- matrix(2, nrow=inSize[1], ncol=inSize[2])
    } else {
        dqMat <- ncvar_get(nc, 'quality_flag', start=c(1, 1), count=c(-1, -1))
    }
    times <- PAMscapes:::ncTimeToPosix(nc$dim$time)
    freqs <- nc$dim$frequency$vals
    type <- PAMscapes:::checkFreqType(freqs)
    dqMat <- markDQMatrix(dqMat, freqRange=freqRange, timeRange=timeRange, value=value,
                          times=times, freqs=freqs)

    ncvar_put(nc, varid='quality_flag', vals=dqMat)
}



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

g +
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

# okaaaaay dq plotters devel
