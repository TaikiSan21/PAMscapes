% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchDetectionData.R
\name{matchDetectionData}
\alias{matchDetectionData}
\title{Match Detections to Other Data}
\usage{
matchDetectionData(
  x,
  detection,
  name,
  value,
  fillNA = NA,
  by = NULL,
  mode = c("replace", "add")
)
}
\arguments{
\item{x}{a dataframe with column "UTC" and optionally "end"}

\item{detection}{dataframe of detection data loaded with
\link{loadDetectionData}}

\item{name}{name of the column in \code{x} to add detection information
to, will be created if it does not exist}

\item{value}{value to use for marking detections. If \code{value} is the
name of a column in \code{detection}, then the corresponding value of
that column will be used. If not, then \code{value} will be used
for all detections}

\item{fillNA}{value to fill in for times that do not have detections,
default is \code{NA}}

\item{by}{if not \code{NULL}, a specification for pairing distinct groups
in \code{x} and \code{detection}. Can be a single character of the
column name shared by both (e.g. \code{"site"}), a named character where the
name is the column in \code{x} and the value is the column in
\code{detection} (e.g. \code{c("site"="deployment")}), or an equality
statement where the left is the column in \code{x} and the right is the
column in \code{detection} (e.g. \code{"site" == "deployment"})}

\item{mode}{one of "replace" or "add", this sets how to deal with times
that have multiple detections or where column \code{name} has an existing
value. "replace" always replaces with the most recent value in \code{detection},
"add" concatenates all values with ","}
}
\value{
a dataframe
}
\description{
Matches detection data from \link{loadDetectionData} to
other data types. A new column is created in the input data to
store detection data, and values in that column are filled in
based on whether or not their times overlap with times in
the detection data.
}
\examples{

time <- as.POSIXct('2020-01-01 12:00:00', tz='UTC')
data <- data.frame(UTC=time+c(0, 30, 60, 90, 120),
                   end=time+c(30, 60, 90, 120, 150))
detection <- data.frame(UTC=time+c(25, 90),
                        end=time+c(40, 95),
                        species=c('A', 'B'))
                        
# Create a new "presence" column and fill with TRUE                       
matchDetectionData(data, detection, name='presence', value=TRUE)
# Fill non-matching times with FALSE (instead of default NA)
matchDetectionData(data, detection, name='presence', value=TRUE, fillNA=FALSE)
# Instead fill with value from "species" column
matchDetectionData(data, detection, name='presence', value='species')

detection <- data.frame(UTC=time+c(25, 90, 18, 30),
                        end=time+c(40, 95, 28, 40),
                        site=c('east', 'east', 'west', 'west'),
                        species=c('A', 'B', 'A', 'B'))
                        
data <- data.frame(UTC=time+c(0, 30, 60, 90, 120, 0, 30),
                   end=time+c(30, 60, 90, 120, 150, 30, 60),
                   deployment=c(rep('east', 5), rep('west', 2)))
                   
# detection now has overlapping times, so this will trigger a warning
matchDetectionData(data, detection, name='presence', value='species')
# mode='add' will change this to concatenate the labels for overlaps
matchDetectionData(data, detection, name='presence', value='species', mode='add')
# but really these correspond to different locations, so we can use "by" for that
matchDetectionData(data, detection, name='presence', value='species', by=c('deployment'='site'))
# this is another way to specify "by"
matchDetectionData(data, detection, name='presence', value='species', by='deployment'=='site')

}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
