#' load demo data
#'
#' @include Session-Class.R
#'
#' @import gdalcubes
#' @export
loadDemoData = function() {

  path = Session$getConfig()$data.path
  path.demo = paste(path,"L8_demo", sep = "/")


  if (! is.element("L8.db",list.files(path))) {

    if (! is.element("L8_demo", list.files(path))) {

      zipfile = paste(path, "L8_demo.zip", sep = "/")
      download.file("https://uni-muenster.sciebo.de/s/e5yUZmYGX0bo4u9/download",
                    destfile = zipfile, mode="wb")
      unzip(zipfile=zipfile, exdir = path.demo)
      file.remove(zipfile)
    }

    L8.files = list.files(path.demo, pattern = ".tif",
                          recursive = TRUE, full.names = TRUE)

    create_image_collection(L8.files, format = "L8_SR", out_file = paste(path,"L8.db",sep = "/"))
  }
  path.db = paste(path,"L8.db", sep = "/")
  ic =image_collection(path.db)
#browser()

  L8 = Collection$new(id = "L8", title = "Landsat8", description = "Landsat")
  L8$setCollection(ic)
  Session$assignData(L8)
}


