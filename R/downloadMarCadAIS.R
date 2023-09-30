#' @title Download AIS Data from Marine Cadastre
#'
#' @description Downloads daily AIS files from \url{https://marinecadastre.gov/ais/}
#'   covering the date range present in input data
#'
#' @param x a dataframe with column \code{UTC} in POSIXct format
#' @param outDir directory to save the downloaded files
#' @param overwrite logical flag to overwrite existing data. Recommended
#'   to be \code{FALSE} to avoid re-downloading large files unnecessarily
#' @param unzip logical flag to unzip downloaded files. Original downloads
#'   from Marine Cadastre come as large .zip
#' @param verbose logical flag to print messages about download progress
#'
#' @return a vector of the paths to the downloaded .zip files, any days
#'   that were unable to download will be \code{NA}
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @examples
#' # dont run example because causes large download
#' \dontrun{
#' gps <- data.frame(Latitude=c(33.2, 33.5,33.6),
#'                   Longitude=c(-118.1, -118.4, -119),
#'                   UTC=as.POSIXct(
#'                     c('2022-04-28 05:00:00',
#'                       '2022-04-28 10:00:00',
#'                       '2022-04-28 20:00:00'),
#'                     tz='UTC'))
#' marcadFiles <- downloadMarCadAIS(gps, outDir='AISData')
#' }
#'
#' @importFrom httr GET progress write_disk
#' @importFrom lubridate year
#'
#' @export
#'
downloadMarCadAIS <- function(x, outDir='.', overwrite=FALSE, unzip=TRUE,
                              verbose=TRUE) {
    allDays <- getDaySequence(x$UTC)
    years <- year(allDays)
    tooEarly <- years < 2015
    if(any(tooEarly)) {
        warning('AIS data before 2015 has a different format and cannot be downloaded')
    }
    tooLate <- years > 2023
    if(any(tooLate)) {
        warning('AIS data after 2023 is not yet available and cannot be downloaded')
    }
    allDays <- allDays[!tooEarly & !tooLate]
    if(length(allDays) == 0) {
        warning('No valid years present')
        return(NULL)
    }

    urls <- dayToAisURL(allDays)
    # urls
    if(!dir.exists(outDir)) {
        dir.create(outDir)
    }
    outZip <- file.path(outDir, basename(urls))
    outCsv <- gsub('zip$', 'csv', outZip)
    for(i in seq_along(urls)) {
        if(verbose) {
            cat(paste0('Downloading AIS file ', i, ' out of ', length(urls), '...\n'))
        }
        if(isFALSE(overwrite) &&
           (file.exists(outZip[i]) ||
            file.exists(outCsv[i]))) {
            # outZip[i] <- NA
            if(verbose) {
                cat('File for', urls[i], 'exists\n')
            }
        } else {
            dl <- GET(urls[i],
                      progress(),
                      write_disk(outZip[i], overwrite = TRUE))
            if(dl$status_code != 200) {
                # warning('URL ', urls[i], ' failed to download properly')
                outZip[i] <- NA
            }
        }
        if(isTRUE(unzip) &&
           file.exists(outZip[i])) {
            if(isFALSE(overwrite) &&
               file.exists(outCsv[i])) {
                next
            }
            if(verbose) {
                cat('Unzipping...\n')
            }
            unzip(outZip[i], exdir=outDir)
        }
    }
    dlFail <- is.na(outZip)
    if(any(dlFail)) {
        warning(sum(dlFail), ' URL(s) ', paste0(urls[dlFail], collapse=', '),
                ' failed to download properly.')
    }
    invisible(outZip)
}

dayToAisURL <- function(x) {
    dayChar <- format(x, format='%Y/AIS_%Y_%m_%d')
    urlBase <- 'https://coast.noaa.gov/htdata/CMSP/AISDataHandler/'
    paste0(urlBase, dayChar, '.zip')
}

# make date sequence to cover range
getDaySequence <- function(x) {
    tRange <- range(x)
    tRange <- floor_date(tRange, unit='day')
    seq(from=tRange[1], to=tRange[2], by='day')
}
