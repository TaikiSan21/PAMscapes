# oh boy itsa-me, makara templates
detDir <- '../nefsc-makara-data-migration/data/datasets/baleen-old/data/detectiondata/'
detFiles <- list.files(detDir, full.names=TRUE, recursive=TRUE)
library(dplyr)

detData <- (lapply(detFiles, read.csv, stringsAsFactors=FALSE))
str(detData)

allOld <- (lapply(detFiles, readOldDetection))
# allOld <- vector('list', length=length(detFiles))
# for(i in seq_along(allOld)) {
    # cat(i)
    # allOld[[i]] <- readOldDetection(detFiles[i])
# }


readOldDetection(detFiles[probIx])

# templates
tempDir <- '../nefsc-makara-data-migration/data/templates/templates/'
list.files(tempDir)
read.csv(file.path(tempDir, 'detections.csv'))

names(bind_rows(allOld))

allNames <- lapply(allOld, names)
ixVec <- rep(0, length(allNames))
distNames <- list()
for(i in seq_along(allNames)) {
    if(i==1) {
        distNames <- list(allNames[[i]])
    }
    isIn <- unlist(lapply(distNames, function(y) identical(allNames[[i]], y)))
    if(!any(isIn)) {
        distNames[[length(distNames)+1]] <- allNames[[i]]
        ixVec[i] <- length(distNames)
    } else {
        ixVec[i] <- which(isIn)
    }
}
distNames
combOld <- vector('list', length=max(ixVec))
for(i in seq_along(combOld)) {
    combOld[[i]] <- bind_rows(allOld[ixVec == i])
}
saveRDS(combOld, file='../Data/MakBal/CombOld.rds')
combOld <- lapply(combOld, function(x) {
    x$species <- basename(dirname(x$file))
    x
})

# general thoughts:
# check through different colname combos and map
# Should some categories print a sumary? Like here are your analysts, here are your species, whatever else
# appears to sometimes be an extra column after notes that contains notes?
# clean_names is from janitor package

# Hm... 4-30 next steps
# read in metadata sheets folder and fix up
# parse file name for what we can
# idk if app is helpful at this point

# okay now the plan is to mostly copy how jeffs baleen-new code works lmao and
# try it on the server data. Lots just hard-coded by species and thats fine

# anaylsis start/end is calc'd as min(det$start), max(det$end)
