# PAMscapes 0.10.1

- Added `keepQuals` argument to `loadSoundscapeData` and `loadMultiscapeData`

# PAMscapes 0.10.0

- Adding `formatEffort` and related internal helper functions

- Adding `effort` to `plotAcousticScene` to plot off-effort times

- Adding some tests for detection data processing

- Adding `plotDetectionBoxplot` plotting function

- Deprecating `cpal` argument in `plotScaledTimeseries` in favor of `color`
for better naming consistency

# PAMscapes 0.9.1

- Bug fix for bad Lat/Long WKT in some nc files

- Reducing columns of PSD example file to 3kHz instead of 24kHz because
it was causing UBSAN errors (I think)

# PAMscapes 0.9.0

- Adding `binDetectionData` function for converting detection data to
presence data (e.g. hourly/daily presence)

- Adding `by` option to `plotAcousticScene` to allow for faceting the plot

- Adding ability for `plotAcousticScene` to work without frequency ranges,
just plotting equal size bars for each detection type present

# PAMscapes 0.8.2

- Adding `binCount` to `loadSoundscapeData`, `binSoundscapeData`, and
`loadMultiscapeData` to return number of times in each bin

- Adding `keepEffort` to `loadSoundscapeData` and `loadMultiscapeData`
to return effort column. Can also be used to set a threshold for
an amount of effort to keep in results

# PAMscapes 0.8.1

- `plotLTSA` with `facet` changed labeling scheme

# PAMscapes 0.8.0

- HMD type input standardized to always use same rounded column
names (why was this such a headache). Also new option for
all data loaders `dropNonHmd` to auto-remove any columns in
HMD data that are not standard HMD levels - most commonly 
exactly Nyquist in some datasets

- Deprecating `checkSoundscapeInput` in favor of `loadSoundscapeData`
because the old name was stupid 

- Adding new function `loadMultiscapeData` to load multiple folders of
soundscape data to better allow for large scale comparisons

- Adding parallelization ability to `loadSoundscapeData` using
`future.apply` package

- Adding `facet` options to `plotLTSA` and `plotPSD` to create faceted
plots by a column in the data. Also added to `runSoundscapeExplorer`
for `plotPSD`

- `createOctaveLevel` updated to accurately handle HMD data at lower
frequencies by summing partial bands. Also dropped the method
argument because we are always summing (`normalize` argument handles
what people might want from a mean/median solution??)

- `binSoundscapeData` changed to use character function argument instead
of actual function (to better incorporate `na.rm=TRUE`)

- Adding `referenceLevel` option to `plotPSD` that plots different 
values of `by` column as difference between the selected reference
column instead of absolutes

# PAMscapes 0.7.3

- Bug in `plotPSD` for log scale frequency axis if frequency of 0 was present

# PAMscapes 0.7.2

- Updating `createOctaveLevel` to work properly for hybrid-millidecade data.
Was previously not correcting for binwidth correction. Also added a
`normalized` option to allow for bin width-normalized outputs

- `createOctaveLevel` also now properly keeps track of any added columns
in addition to the standard soundscape metric data (e.g. GPS)

- Updating `matchGFS` with new `progress` and `keepMatch` options, and
adding `windMag` to the standard output

- Updating tutorial with PyPam examples and data and to keep up with 
more recent developments

- Fixed problem in `plotScaledTimeseries` if second plotted column was
all the same value

- Added "Copy to create this plot" text output to all `runSoundscapeExplorer`
plots

- Updated `loadMantaNc` (and thus `checkSoundscapeInput`) to use a more
standard naming scheme for HMD data from netCDF files - will now always
be rounded to a single decimal point

# PAMscapes 0.7.1

- `loadMantaNc` (and thus `checkSoundscapeInput`) now knows to look for
coordinates and platform from NC files to create `Latitude` `Longitude`
and `platform` columns in the loaded data

# PAMscapes 0.7.0

- Adding new `runSoundscapeExplorer` function that launches a `shiny` 
app to explore the various visualization options for soundscape data

# PAMscapes 0.6.3

- Adding `tz` option for `checkSoundscapeInput` if data are not already in
UTC timezone

- `checkSoundscapeInput` combines multiple files by default

# PAMscapes 0.6.2

- Adding `by` option for `plotPSD`

- Improving performance for large datasets in `plotPSD` when they are provided
as a single dataframe

- Adding `binSoundscapeData` function to summarise data by a smaller
unit of time

- Added binning options directly to `checkSoundscapeInput`

- Adding `returnData` option for most plotting functions to return the
plotted data instead of a plot

# PAMscapes 0.6.1

- Adding `matchSeascape` function

# PAMscapes 0.6.0

- Fixing CRAN checking issue with `matchGFS` example

- Added `plotLTSA` function

- Speed improvements for many functions

# PAMscapes 0.5.8

- dropping `hoardr` and trying to fix cache path on MacOS for CRAN

# PAMscapes 0.5.7

- ggplot 3.5.0 broke some auto date scaling so I ahve to add it back

# PAMscapes 0.5.6

- Adding `by` option for `markNA`

# PAMscapes 0.5.5

- Adding `by` option for `plotPSD`

- Adding `keepQuals` option for `loadMantaNc` to honor
data quality flags

# PAMscapes 0.5.4

- Updates for `readLocalAIS` to work better with stationary deployments

# PAMscapes 0.5.3

- Added examples for all functions and a small AIS file for use in examples

# PAMscapes 0.5.2

- Exporting `prepPSDData` so people can use to make summarised data
separate from the plotting portion

# PAMscapes 0.5.1 

- Bug fix for `addAIS` for `interptype='all'` and `interpType='close'` where
it was accidentally creating extra duplicate NA valued rows

# PAMscapes 0.5.0

- Adding `plotPSD` function
- Exporting `checkSoundscapeInput` so that others can use it to load data
- Adding both of these updates to the tutorial doc

# PAMscapes 0.4.1

- Added `loadMantaNc` function to support MANTA NetCDF file input

# PAMscapes 0.4.0

- Added `createOctaveLevel` function and updated tutorial doc to include it
- Added tests

# PAMscapes 0.3.2

- Added `alpha` param to `plotAcousticScene`

# PAMscapes 0.3.1

- Some bugfixes from creating tutorial
- Uploading `PAMscapesTutorial` RMarkdown doc

# PAMscapes 0.3.0

- Adding `plotScaledTimeseries` function
- Adding `addAISSumamry` function

# PAMscapes 0.2.0

- Adding plotting functions
- Adding GHActions

# PAMscapes 0.1.1

- Removing slide# from exports

# PAMscapes 0.1.0

- First commit! AIS functions, GFS download functions

