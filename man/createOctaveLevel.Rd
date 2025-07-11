% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createOctaveLevel.R
\name{createOctaveLevel}
\alias{createOctaveLevel}
\title{Create Octave Level Measurements}
\usage{
createOctaveLevel(
  x,
  type = c("ol", "tol", "broadband", "bb", "hmd"),
  freqRange = NULL,
  normalized = FALSE
)
}
\arguments{
\item{x}{dataframe of soundscape metrics}

\item{type}{one of \code{'ol'} to create octave level, \code{'tol'} to create
third octave level measures, or \code{'broadband'} or \code{'bb'} to create
an arbitrary broadband measure. For broadband measures, \code{freqRange} must
be supplied to define the range}

\item{freqRange}{a vector of the minimum and maximum center frequencies (Hz) desired
for the output. If \code{NULL}, full available range of frequencies will be used.
If output \code{type} is broadband, this is used to define the lower and upper
bounds of the desired output broadband level}

\item{normalized}{logical flag to return values normalized by the bandwidth of
each octave level band (per Hz)}
}
\value{
a dataframe with summarised octave level band measurements
}
\description{
Creates (third) octave level or broadband measurements from finer
resolution soundscape metrics, like Power Spectral Density (PSD) or
Hybrid Millidecade (HMD) measures
}
\details{
To create new measurements, finer resolution metrics are cast to
linear space, summed, and then re-logged. If input measurements are
HMD values then they are assumed to be normalized per Hz, so levels are
first corrected by the bandwidth before summing. In all other cases inputs
are assumed to not be normalized per Hz measurements and are just summed.
}
\examples{
psd <- loadSoundscapeData(system.file('extdata/PSDSmall.csv', package='PAMscapes'))
str(psd)
tol <- createOctaveLevel(psd, type='tol')
str(tol)
ol <- createOctaveLevel(tol, type='ol')
str(ol)
bb <- createOctaveLevel(psd, type='bb', freqRange=c(20, 150))
str(bb)

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
