data("sample_explan_data")
random_cat_layer <- terra::rast(sample_extent_data)
random_cat_layer <- terra::setValues(random_cat_layer,
                                     sample(0:10, terra::ncell(random_cat_layer),
                                          replace = TRUE))

test_that("Works if static rasters and buffering", {
results<-extract_static_coords(occ.data = sample_explan_data,
                      varnames = "random_cat_layer",
                      static.rasters = random_cat_layer)
  expect_equal(ncol(results), ncol(sample_explan_data)+1)
})


test_that("Works if static rastersx2 and buffering", {
  results<-extract_static_coords(occ.data = sample_explan_data,
                                 varnames = c("random_cat_layer","random_cat_layer2"),
                                 static.rasters = c(random_cat_layer,random_cat_layer))
  expect_equal(ncol(results), ncol(sample_explan_data)+2)
})


