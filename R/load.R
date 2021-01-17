#' load demo data
#'
#' @include Session-Class.R
#'
#' @import gdalcubes
#' @export
loadDemoData = function() {

  demo.path = Session$getConfig()$demo.path
  file.path = paste(demo.path,"L8_demo", sep = "/")

  if (! is.element("L8.db",list.files(demo.path))) {

    if (! is.element("L8_demo", list.files(demo.path))) {

      zipfile = paste(demo.path, "L8_demo.zip", sep = "/")
      download.file("https://uni-muenster.sciebo.de/s/e5yUZmYGX0bo4u9/download",
                    destfile = zipfile, mode="wb")
      unzip(zipfile=zipfile, exdir = file.path)
      file.remove(zipfile)
    }

    L8.files = list.files(file.path, pattern = ".tif",
                          recursive = TRUE, full.names = TRUE)

    create_image_collection(L8.files, format = "L8_SR", out_file = paste(demo.path,"L8.db",sep = "/"))
  }
  path.db = paste(demo.path,"L8.db", sep = "/")
  ic =image_collection(path.db)

  L8 = Collection$new(id = "L8", title = "Landsat8", description = "Landsat")
  L8$setCollection(ic)
  Session$assignData(L8)
}
