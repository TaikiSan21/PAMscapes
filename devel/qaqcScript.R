# Run QAQC Example Script 
# eventually this we be replaced with library(PAMscapes) or something
source('//nefscdata/PassiveAcoustics/Stellwagen_Old/MANUALS_SOFTWARE_CODE LIBRARY/CODE_LIBRARY/R/R_QAQC_TS/qaqcFunctions.R')

# Folder where QAQC log data get stored
qaqcDir <- '//nefscdata/PassiveAcoustics/Stellwagen_Old/PROJECT_ADMIN_ACCDATA/QAQC'

# Names of PA Data Upload and Recorder Performance CSV files
# paDataUpload <- file.path(qaqcDir, 'PA_Data_Upload_1732560244.xlsx')
# Changing to Smartsheets file
paDataUpload <- file.path(qaqcDir, 'PA Data - SoundTrap_ HARU_ HARP_ Towed Array.xlsx')
# recPerf <- file.path(qaqcDir, 'Recorder_Performance_1736370569.xlsx')
recPerf <- file.path(qaqcDir, 'Instrument Tracking.xlsx')
# dataQaqc <- file.path(qaqcDir, 'Data QAQC - MA-RI, GOM, SBNMS, PAu, Mid-Atl.xlsx')

# Generate new log file, this may warn about missing sensitivity
qLog <- nefscMondayToLog(dataUpload=paDataUpload, 
                         recPerf=recPerf)

# Map server directories to projects, this may warn about missing directories
# Will search for matching folders within the provided base directories
# based on the project name
qLog <- addNefscDirs(
    qLog, 
    recBase = '//nefscdata/PassiveAcoustics_Soundfiles/BOTTOM_MOUNTED/', 
    qaqcBase = '//nefscdata/PassiveAcoustics/Stellwagen_Old/PROJECT_ADMIN_ACCDATA/BOTTOM_MOUNTED/')

# Now save these new deployments back to the QAQC directory, this function
# will append the new data to the existing QAQC_Log.csv file
# update controls how situations are handled when a project is in both
# the existing QAQC_Log.csv and the new log data in "qLog"
# "new" - only replace values in the CSV that were previously NA, or
#         increase the QAQC Status (e.g. "NoData" to "NotRun" is allowed
#         but not vice versa. This is usually what you want.
# "none" - make no changes to existing projects
# "all" - replace all values with new data in existing projects
saveQLog(qLog, dir=qaqcDir, update='new')

# Now we are ready to run!

# This generates QAQC outputs for all projects that need them - any with 
# missing directories or sensitivity values will be skipped with warnings
# When autosave=TRUE it will also automatically save the updated log file,
# updating the QAQC Status of any projects that were processed
# nSpectrograms controls how many 30 minute spectrogram images will be
# created for each 
qLog <- processQAQCLog(qaqcDir, nSpectrograms=10, autosave=TRUE)

# Now you are ready to review! This launches the Shiny app - it will read the
# QAQC_Log and QAQC_Issues files from the QAQC folder
runQAQCReview(qaqcDir)

#### OPTIONAL WORKFLOW ####
# If you only want to run a few specific projects, the easiest way is to
# read in the log file, subset that dataframe to only the row(s) you want,
# and then use that to run the processQAQCLog function
qLog <- readQLog(qaqcDir)

projNames <- c('NEFSC_VA_202405_CB01','NEFSC_VA_202405_CB02','NEFSC_VA_202405_CB03','NEFSC_VA_202405_CB04')

qSubset <- qLog[qLog$projectName %in% projNames, ]
qSubset <- addNefscDirs(
    qSubset, 
    recBase = '//nefscdata/PassiveAcoustics_Soundfiles/BOTTOM_MOUNTED/', 
    qaqcBase = '//nefscdata/PassiveAcoustics/Stellwagen_Old/PROJECT_ADMIN_ACCDATA/BOTTOM_MOUNTED/')

qSubset <- processQAQCLog(qSubset, nSpectrograms=10)

# Note that in this case you will need to manually save the new log results back
# with overwrite=TRUE to update the QAQC status of the processed projects
saveQLog(qSubset, qaqcDir, update='new')
