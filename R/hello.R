#' @export
hello <- function(x=2,y=2) {
  print("Hello, world!",x+y)
}
##' @export bm
bm <- function(){
   devtools::document()
   system(paste0('cd ',rprojroot::find_rstudio_root_file(),'; R CMD INSTALL --preclean --no-multiarch --with-keep.source .'))
}


