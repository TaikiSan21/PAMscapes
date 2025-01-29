context('Test detection data loading and formatting')

test_that('Test effort formatting', {
          effort <- 
              data.frame(
                  start=c('2023-01-04', '2024-02-29','2020-02-29', '2023-04-01'),
                  end = c('2023-03-02', '2024-04-05', '2020-03-01', '2024-01-01'),
                  site = c('a', 'a', 'b', 'a')
              )
          effort$start <- lubridate::ymd(effort$start)
          effort$end <- lubridate::ymd(effort$end)
          allEff <- formatEffort(effort[-3], combineYears=TRUE)
          expect_equal(allEff$status, c('off', 'on'))
          expect_equal(as.character(allEff$end), c('2019-01-04', '2020-01-01'))
          expect_warning(siteEff <- formatEffort(effort, columns='site', combineYears=TRUE),
                         '1 effort entries removed')
          expect_identical(allEff, siteEff[-3])
          effort <- 
              data.frame(
                  start=c('2023-01-04', '2024-02-29','2020-02-29', '2023-04-01'),
                  end = c('2023-03-02', '2024-04-05', '2020-03-01', '2024-01-04'),
                  site = c('a', 'b', 'a', 'a')
              )
          effort$start <- lubridate::ymd(effort$start)
          effort$end <- lubridate::ymd(effort$end)
          expect_warning(siteEff <- formatEffort(effort, columns='site', combineYears=TRUE),
                         '1 effort entries modified')
})
