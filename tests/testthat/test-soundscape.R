context('Test soundscape data loading and formatting')

test_that('Test MANTA load', {
    mfile1 <- system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes')
    mfile2 <- system.file('extdata/MANTAExampleSmall2.csv', package='PAMscapes')
    expect_warning(loadSoundscapeData(mfile1), 'Found 1 non-standard')
    base1 <- read.csv(mfile1, stringsAsFactors = FALSE)
    base2 <- read.csv(mfile2, stringsAsFactors = FALSE)

    base1 <- checkManta(base1)
    base2 <- checkManta(base2)

    expect_true(all(c('UTC', 'HMD_0', 'HMD_1', 'HMD_2') %in% colnames(base1)[1:4]))
    expect_true(all(c('UTC', 'HMD_0', 'HMD_1', 'HMD_2') %in% colnames(base2)[1:4]))

    base1 <- loadSoundscapeData(base1, dropNonHmd = FALSE)
    base2 <- expect_warning(loadSoundscapeData(base2, dropNonHmd = FALSE),
                          'Input does not appear to be standard')
    expect_warning(checkSoundscapeInput(base1), 'This function has been renamed')

    base1[3, 10] <- Inf
    expect_warning(base1 <- loadSoundscapeData(base1, dropNonHmd = FALSE))
    expect_true(is.na(base1[3, 10, drop=TRUE]))
})

test_that('Test octave', {
    mantaFile <- system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes')
    manta <- expect_warning(loadSoundscapeData(mantaFile, dropNonHmd = TRUE, keepEffort = FALSE),
                            'Found 1 non-standard')
    ol <- createOctaveLevel(manta, type='ol')
    olDirect <- expect_warning(loadSoundscapeData(mantaFile, dropNonHmd=TRUE, octave='ol', keepEffort = FALSE),
                               'Found 1 non-standard')
    expect_identical(ol, olDirect)
    tol <- createOctaveLevel(manta, type='tol')
    expect_true(all(grepl('^OL_', colnames(ol[2:ncol(ol)]))))
    expect_true(all(grepl('^TOL_', colnames(tol[2:ncol(tol)]))))

    filtOl <- createOctaveLevel(manta, type='ol', freqRange = c(250, 8000))
    expect_true(colnames(filtOl)[2] == 'OL_250')
    expect_true(colnames(filtOl)[ncol(filtOl)] == 'OL_8000')
    filtDiff <- createOctaveLevel(manta, type='ol', freqRange = c(200, 8000))
    expect_identical(filtOl[['OL_250']], filtDiff[['OL_250']])
})

test_that('Test long wide', {
    manta <- system.file('extdata/MANTAExampleSmall1.csv', package='PAMscapes')
    manta <- expect_warning(loadSoundscapeData(manta, dropNonHmd = TRUE),
                            'Found 1 non-standard')
    ol <- createOctaveLevel(manta, type='ol')
    expect_identical(ol, toWide(ol))
    expect_identical(ol, data.frame(toWide(toLong(ol))))
    olLong <- data.frame(toLong(ol))
    expect_identical(olLong, toLong(olLong))
    expect_identical(olLong, data.frame(toLong(toWide(olLong))))
    ol$ADDSTUFF <- runif(nrow(ol))
    expect_identical(ol, data.frame(toWide(toLong(ol))[colnames(ol)]))
})
