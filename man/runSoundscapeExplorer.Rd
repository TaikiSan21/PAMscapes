% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runSoundscapeExplorer.R
\name{runSoundscapeExplorer}
\alias{runSoundscapeExplorer}
\title{Run Soundscape Explorer App}
\usage{
runSoundscapeExplorer(data = NULL)
}
\arguments{
\item{data}{file path to soundscape data or data that has been loaded with
\link{loadSoundscapeData}}
}
\value{
invisible TRUE
}
\description{
Launches a shiny app that allows users to browse
the various plotting functions available to visualize
soundscape data
}
\examples{
if(interactive()) {
  hmd <- loadSoundscapeData(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
  runSoundscapeExplorer(hmd)
}

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
