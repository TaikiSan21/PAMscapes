# oh boy itsa-me, makara templates
detDir <- '../nefsc-makara-data-migration/data/datasets/baleen-old/data/detectiondata/'
detFiles <- list.files(detDir, full.names=TRUE, recursive=TRUE)
library(dplyr)

detData <- bind_rows(lapply(detFiles, read.csv, stringsAsFactors=FALSE))
str(detData)

allOld <- (lapply(detFiles, readOldDetection))
checkers <- sapply(allOld, function(x) 'x23' %in% names(x))
which(checkers)
probIx <- 106
allOld[[probIx]]$file[1]
View(allOld[[probIx]])

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
# general thoughts:
# check through different colname combos and map
# Should some categories print a sumary? Like here are your analysts, here are your species, whatever else
# appears to sometimes be an extra column after notes that contains notes?
# clean_names is from janitor package



