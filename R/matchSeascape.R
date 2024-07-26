#' @title Match Seascape Class to Data
#'
#' @description Downloads and matches relevant Seascape class
#'   data from the ERDDAP (Environmental Research Division's
#'   Data Access Program) server at
#'   \url{https://cwcgom.aoml.noaa.gov/erddap/index.html}. More
#'   information on theclasses can be found on the help page for
#'   the seascapeR package
#'   \url{https://marinebon.github.io/seascapeR/index.html}.
#'
#' @param x a dataframe with columns \code{UTC}, \code{Latitude} and
#'   \code{Longitude} to add environmental data to
#' @param type the type of seascape data to download, one of
#'    "monthly" or "8day"
#' @param progress logical flag whether or not to show download progress
#'
#' @return the same dataframe as \code{x}, but with new columns
#'   \code{seascapeClass} and \code{seascapeProb} representing the
#'   "CLASS" and "P" variables from the dataset
#'
#' @details This function is just a wrapper around \link[PAMmisc]{matchEnvData}
#'   pointing to the specific base URL and dataset ID relevant for seascape
#'   data
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
#' @importFrom PAMmisc erddapToEdinfo matchEnvData
#' @importFrom dplyr rename
#'
matchSeascape <- function(x, type=c('monthly', '8day'), progress=TRUE) {
    dataset <- switch(match.arg(type),
                      'monthly' = 'noaa_aoml_4729_9ee6_ab54',
                      '8day' = 'noaa_aoml_seascapes_8day'
    )
    baseurl <- 'https://cwcgom.aoml.noaa.gov/erddap/'
    edi <- erddapToEdinfo(dataset=dataset,
                          baseurl=baseurl,
                          chooseVars=c('CLASS', 'P'))
    x <- matchEnvData(x, nc=edi, progress=progress)
    x <- rename(x,
                'seascapeClass'='CLASS_mean',
                'seascapeProb' = 'P_mean')
    x
}
