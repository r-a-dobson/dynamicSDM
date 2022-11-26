## extract_xy_min_max Extracts xmin,xmax,ymin,ymax from spatial extent object 
#' @param x spatial extent object. Object of class "Extent", "raster" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order.
#' @noRd

extract_xy_min_max<-function(x){
  
if(class(x)=="numeric" && length(x)==4){
  xmin<-x[1]
  xmax<-x[2]
  ymin<-x[3]
  ymax<-x[4]}

### Extent object to co-ords

if(class(x)=="Extent"){
  xmin<-sp::bbox(x)[1,1]
  xmax<-sp::bbox(x)[1,2]
  ymin<-sp::bbox(x)[2,1]
  ymax<-sp::bbox(x)[2,2]}


### RasterLayer object to co-ords

if(class(x)=="RasterLayer"){
  xmin<-sp::bbox(raster::extent(x))[1,1]
  xmax<-sp::bbox(raster::extent(x))[1,2]
  ymin<-sp::bbox(raster::extent(x))[2,1]
  ymax<-sp::bbox(raster::extent(x))[2,2]}


### Polygon object to co-ords

if(class(x)=="Polygon"){
  xmin<-sp::bbox(x)[1,1]
  xmax<-sp::bbox(x)[1,2]
  ymin<-sp::bbox(x)[2,1]
  ymax<-sp::bbox(x)[2,2]}
  
  return(c(xmin,xmax,ymin,ymax))}