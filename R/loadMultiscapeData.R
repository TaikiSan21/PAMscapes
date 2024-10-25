#' @title Load Multiple Folders of Soundscape Data
#'
#' @description Loads soundscape data just like \link{loadSoundscapeData},
#'   but is designed to load multiple soundscape datasets from 
#'   multiple folders. This is identical to loading each folder of
#'   data individually with the same bin and label parameters.
#'
#' @param x a vector of folder names to load
#' @param timeBin amount of time to bin data by, format can
#'   be "#Unit" e.g. \code{'2hour'} or \code{'1day'}. Unlike
#'   \link{loadSoundscapeData} this argument is now mandatory
#'   to reduce data size
#' @param binFunction summary function to apply to data in each time bin,
#'   default is median
#' @param octave one of "original", "tol", or "ol". If "original" then
#'   nothing happens, otherwise data are converted to Octave-leve ("ol")
#'   or Third-Octave-Level ("tol") measurements using
#'   \link{createOctaveLevel}
#' @param label if not \code{NUL}, then must be of equal length to \code{x}
#' @param dropNonHmd logical flag to drop non-standard hybrid millidecade
#'   bands, only applies to HMD type data. Some datasets have frequency
#'   values that are not part of the standard HMD bands (e.g. at exactly
#'   the Nyquist rate), if \code{TRUE} these will be removed.
#' @param tz timezone of the data being loaded, will be converted to UTC
#'   after load
#' @param extension only required if both netCDF and CSV files exist in
#'   the folders to load, in which case only one type will be loaded. 
#'   Must be one of "nc" or "csv"
#'   
#' @details This function is equivalent to loading each folder of data
#'   separately with the same time and octave-level aggregation options
#'   applied, and is meant as a convenient wrapper for loading multiple
#'   years or sites of data for comparison person. The expectation is that
#'   this function will be primarily used for large scale comparisons, hence
#'   why \code{timeBin} is a required argument to reduce data resolution. 
#'   
#'   The only other difference is that if no labels are supplied for the folders,
#'   then one will be generated either from the names of \code{x} if it is a
#'   named vector, or the name of the folder using \link{basename}. This is to
#'   ensure that each separate folder can be identified once read in.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return a dataframe
#'
#' @examples
#'
#' x <- 1
#' 
#' @export
#' @importFrom dplyr bind_rows
#'
#'
loadMultiscapeData <- function(x, 
                               timeBin=NULL, 
                               binFunction=median, 
                               octave=c('original', 'tol', 'ol'),
                               label=NULL,
                               dropNonHmd=TRUE,
                               tz='UTC',
                               extension=c('nc', 'csv')) {
    if(!is.character(x)) {
        warning('"x" must be a vector of folder paths.')
        return(NULL)
    }
    isDir <- sapply(x, dir.exists)
    if(!any(isDir)) {
        warning('None of the folder(s) in "x" exist, check file paths')
        return(NULL)
    }
    if(any(!isDir)) {
        warning(sum(!isDir), ' of ', length(isDir), ' folders do not exist')
        x <- x[isDir]
        label <- label[isDir]
    }
    if(is.null(timeBin)) {
        warning('Argument "timeBin" is required for loading multiple soundscapes',
                ' in order to reduce data size.')
        return(NULL)
    }
    if(!is.null(label) && length(label) != length(x)) {
        warning('Number of labels must be equal to number of folders')
        return(NULL)
    }
    if(is.null(label) && !is.null(names(x))) {
        label <- names(x)
    }
    # last case use folder name
    if(is.null(label)) {
        label <- basename(x)
    }
    result <- vector('list', length=length(x))
    for(i in seq_along(x)) {
        result[[i]] <- loadSoundscapeData(x[i], 
                                          timeBin=timeBin,
                                          binFunction=binFunction,
                                          octave=octave,
                                          label=label[i],
                                          tz=tz,
                                          dropNonHmd=FALSE,
                                          extension=extension)
    }
    result <- bind_rows(result)
    freqCols <- whichFreqCols(result)
    freqVals <- colsToFreqs(colnames(result)[freqCols])
    type <- gsub('([A-z]*)_.*', '\\1', colnames(result)[freqCols][1])
    # standardizing to round to integer on all HMD columns
    if(type == 'HMD') {
        standardHmd <- paste0('HMD_', round(freqVals, 0))
        colnames(result)[freqCols] <- standardHmd
        hmdLevels <- getHmdLevels(freqRange=range(freqVals)+c(-1, 1))
        nonStandard <- !standardHmd %in% hmdLevels$labels
        newLabs <- fixHmdLabels(freqVals[nonStandard], hmdLevels=hmdLevels)
        colnames(result)[freqCols][nonStandard][!is.na(newLabs)] <- newLabs[!is.na(newLabs)]
        if(anyNA(newLabs) &&
           isTRUE(dropNonHmd)) {
            warning('Found ', sum(is.na(newLabs)), ' non-standard ',
                    'hybrid millidecade frequencies (',
                    paste0(standardHmd[nonStandard][is.na(newLabs)], collapse=', '),
                    ') these will be removed. Run with "dropNonHmd=FALSE"',
                    ' to keep them.')
            for(col in standardHmd[nonStandard[is.na(newLabs)]]) {
                result[[col]] <- NULL
            }
        }
    }
    result
}