% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markNA.R
\name{markNA}
\alias{markNA}
\title{Mark NA Values by Time and Frequency}
\usage{
markNA(x, na)
}
\arguments{
\item{x}{dataframe of soundscape data to mark NAs in}

\item{na}{dataframe listing areas to mark NA. Must have columns \code{start}
and \code{end} in UTC listing time ranges. Can also have columns \code{freqMin}
and \code{freqMax} to also have accompanying frequency ranges, otherwise
all frequency values within the time range will be set to NA}
}
\value{
same dataframe as \code{x} but with some values replaced with \code{NA}
}
\description{
Marks values within a soundscape dataframe as NA
according to provided time and (optionally) frequency values
}
\examples{
manta <- checkSoundscapeInput(system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes'))
naDf <- data.frame(start=min(manta$UTC),
                   end=max(manta$UTC),
                   freqMin=100,
                   freqMax=500)
plotHourlyLevel(manta)
plotHourlyLevel(markNA(manta, na=naDf))

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
