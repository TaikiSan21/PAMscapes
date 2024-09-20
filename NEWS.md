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

