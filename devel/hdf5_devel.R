#hdf5?
library(hdf5r)
file1 <- '../Data/HDF5/sg680_CalCurCEAS_Sep2024_20241017.h5'
file2 <- '../Data/HDF5/WHICEAS2020_ch01.h5'

?h5file

wat <- h5file(file1, 'r')
wat2 <- h5file(file2, 'r')
hmd <- wat[[names(wat)[1]]]
data <- hmd[['hybridMiliDecLevels']][,]

str(data)

list.datasets(wat)

readDataSet(wat[[, "CalCurCEAS_2024/DateTime")
readDataSet(wat[["CalCurCEAS_2024/Parameters"]])

pars <- wat[["CalCurCEAS_2024/Parameters"]]
hdf5r::h5attributes(pars)

nms <- names(wat)
allData <-wat[[nms]]
times <- readDataSet(allData[['DateTime']])
freqs <- readDataSet(allData[['hybridDecFreqHz']])
hmd <- readDataSet(allData[['hybridMiliDecLevels']])


bro <- wat2[[names(wat2)[1]]]
f2 <- readDataSet(bro[['hybridDecFreqHz']])
