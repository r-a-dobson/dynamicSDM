
# test dynamic_proj
data("sample_model_data")
sample_model_data_test<-dplyr::sample_n(sample_model_data,100)
sample_model_data_train<-dplyr::sample_n(sample_model_data,100)

results1<-brt_fit(occ.data = sample_model_data_train,response.col = "presence.absence",block.col="blockno",distribution="bernoulli",varnames=colnames(sample_model_data_train)[9:12])

results1_eval<-list(c(0.6,0.7,0.6,0.4,0.5,0.6))


sample_model_data_train_abund<-sample_model_data[sample_model_data$individualCount>0 ,]
sample_model_data_train_abund<-sample_model_data_train_abund[!is.na(sample_model_data_train_abund$individualCount),]
sample_model_data_train_abund$individualCount<-log10(1+sample_model_data_train_abund$individualCount)
sample_model_data_train_abund_test.data<-dplyr::sample_n(sample_model_data_train_abund,415)
sample_model_data_train_abund<-dplyr::sample_n(sample_model_data_train_abund,415)

results3<-brt_fit(occ.data = sample_model_data_train_abund,response.col = "individualCount",block.col="blockno",distribution="gaussian",varnames=colnames(sample_model_data_train)[9:12])


results3_eval<-list(c(0.70,0.56,0.60,0.7,0.7,0.90))

user.email<-as.character(gargle::gargle_oauth_sitrep()$email)

#dynamic_proj_covariates

dates=c("2010-01-01","2010-04-01")
Extent<-raster::extent(c(12,36,-35,-12))

test_that("Works if spatial.ext = polygon", {
  dates=c("2010-01-01","2010-04-01")
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-dynamic_proj_covariates(dates=dates,varnames=c("Temperaturemean","TemperatureEightmean","Precipitationsum","Precipitation8Wsum"),user.email=user.email,drive.folder="temporary_folder_buffered_extraction",
                                   spatial.ext=polygon,spatial.res.degrees=0.05,resample.method="bilinear",
                                   save.drive.folder = "testfiles")

  expect_equal(length(results),length(dates))})




### dynamic_proj
test_that("Success if projection.method = all", {
  dates=c("2010-01-01","2010-04-01")
  save.directory=tempdir()
  filenames<-paste0(dates,"_stacked.tif")
  dynamic_proj(dates=c("2010-01-01","2010-04-01"),drive.folder = "testfiles",user.email = user.email,
                              projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                              sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.drive.folder = "temporarysavedrivefolder", save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[2])),TRUE)})



#dynamic_proj_GIF

projection.type<-"binary"

test_that("Successfully write GIF: binary", {
  save.directory=tempdir()
  projection.type<-c("binary")
  filenames<-paste0(projection.type,".gif")
  dynamic_proj_GIF(dates=c("2010-01-01","2010-04-01"),projection.type=projection.type,drive.folder="temporarysavedrivefolder",
                   save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email,save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[1])),TRUE)})

