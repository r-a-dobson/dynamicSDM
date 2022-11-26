
save.directory=tempdir()
projection.type<-"binary"

test_that("Stop if no dates", {
  expect_error(dynamic_proj_GIF(projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                       save.directory=tempdir()))})

test_that("Stop if no drive folder or local directory", {
  expect_error( dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,
                                       save.directory=tempdir()))})

test_that("Stop if no save.drive.folder or save.directory", {
  expect_error( dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                      ))})

test_that("Stop if local directory does not exist", {
  expect_error( dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="notrealdirectory",
                                       save.directory=tempdir()))})

test_that("Stop if save directory does not exist", {
  expect_error( dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                       save.directory="notrealsavedirectory"))})

test_that("Successfully write GIF: binary", {
  save.directory=tempdir()
  projection.type<-"binary"
  filenames<-paste0(projection.type,".gif")
  dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                         save.directory=tempdir())
expect_equal(file.exists(paste0(save.directory,"/",filenames[1])),TRUE)})



test_that("Successfully write GIF: abundance", {
  save.directory=tempdir()
  projection.type<-"abundance"
  filenames<-paste0(projection.type,".gif")
  dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                         save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[1])),TRUE)})


test_that("Successfully write GIF: stacked", {
  save.directory=tempdir()
  projection.type<-"stacked"
  filenames<-paste0(projection.type,".gif")
  dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                         save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[1])),TRUE)})


test_that("Successfully write GIF: proportional", {
  save.directory=tempdir()
  projection.type<-"proportional"
  filenames<-paste0(projection.type,".gif")
  dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                         save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[1])),TRUE)})


