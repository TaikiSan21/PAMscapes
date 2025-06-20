% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotAcousticScene.R
\name{plotAcousticScene}
\alias{plotAcousticScene}
\title{Plot Acoustic Scene}
\usage{
plotAcousticScene(
  x,
  freqMap = NULL,
  typeCol = "species",
  title = NULL,
  bin = "1day",
  by = NULL,
  combineYears = FALSE,
  effort = NULL,
  scale = c("log", "linear"),
  freqMin = NULL,
  freqMax = NULL,
  fill = TRUE,
  alpha = 1,
  returnData = FALSE,
  add = FALSE
)
}
\arguments{
\item{x}{dataframe of detections, must have column \code{UTC} and
a column to connect detection types to the frequency type map}

\item{freqMap}{a dataframe listing frequency ranges to use for
various detection types in \code{x}. Must have columns \code{type},
\code{freqMin} (Hz), \code{freqMax} (Hz), and optionally \code{color}
(color to use for this type of detection on plot)}

\item{typeCol}{column name in \code{x} that matches names in \code{type}
column in \code{freqMap}}

\item{title}{optional title to use for the plot}

\item{bin}{time bin to use for plotting time axis. Each detection will
be displayed as covering this amount of time}

\item{by}{if not \code{NULL}, column name to facet plot by (e.g. site)}

\item{combineYears}{logical flag to combine all observations to display
as a single "year". The year will be set to 2019, and detections falling
on leap days (February 29th) will be removed}

\item{effort}{if not \code{NULL}, a dataframe decribing effort data to be
be formatted with \link{formatEffort}. Alternatively, if columns
"effortStart" and "effortEnd" are present in \code{x}, then these will
be used.}

\item{scale}{one of \code{log} or \code{linear}, the frequency scale for
the plot}

\item{freqMin}{optional minimum frequency for plot, useful for log scale}

\item{freqMax}{optional maximum frequency for plot}

\item{fill}{logical flag if \code{TRUE} then filled boxes will be plotted,
if \code{FALSE} then only outlines will be plotted}

\item{alpha}{transparency percentage for plotting, values less than 1
will allow multiple overlapping colors to be seen}

\item{returnData}{if \code{TRUE} then no plot will be generated, instead the
dataframe that would normally be used to make the plot will be returned}

\item{add}{logical flag if \code{FALSE} plots normally if \code{TRUE}
then the output can be (maybe) added to an existing ggplot object}
}
\value{
a ggplot object
}
\description{
Plots a representation of the acoustic scene using
detections in data. Frequency ranges for detections are taken
from user input and displayed as different colored bars
}
\examples{
detDf <- data.frame(
   UTC=as.POSIXct(c('2023-01-01 00:00:00',
                    '2023-01-03 00:00:00',
                    '2023-01-02 12:00:00',
                    '2023-01-04 00:00:00'),
                  tz='UTC'),
  species = c('Dolphin', 'Whale', 'Whale', 'Dolphin'))
freqMap <- data.frame(type=c('Dolphin', 'Whale'),
                      freqMin=c(10e3, 100),
                      freqMax=c(30e3, 400),
                      color=c('darkgreen', 'blue'))
plotAcousticScene(detDf, freqMap=freqMap, typeCol='species', bin='1day')

detFile <- system.file('extdata/detectionExample.csv', package='PAMscapes')
detData <- loadDetectionData(detFile, source='csv',
                     columnMap=list(UTC='start', end='end', species='sound_code'))
plotAcousticScene(detData)

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
