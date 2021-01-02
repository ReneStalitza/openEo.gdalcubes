if (!require(openEo.gdalcubes)) {
  library(devtools)
  install_github(repo="ReneStalitza/openEo.gdalcubes",ref="main")
}

library(openEO.gdalcubes)

config = SessionConfig()

config$workspace.path = "/var/openeo/workspace"

createSessionInstance(config)
Session$startSession(port = 8000, host = "0.0.0.0")
