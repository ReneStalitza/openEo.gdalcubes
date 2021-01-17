# openEo.gdalcubes

**openEo.gdalcubes** is an R package operating as a proof-of-concept [openEO](http://openeo.org/) webservice. It is based on the API version 1.0.0 and using 'plumber' for the web API. This package includes the [gdalcubes](https://github.com/appelmar/gdalcubes) library for on-demand processing of data cubes from satellite image collections. Be aware that the webservice is not production-ready and unexpected errors might occur.

## System requirements:
* gdal
* libgdal
* libproj
* netcdf4

# Installation
## Install as an R package
Due to temporary compliance issues between the used packages, the installation of package 'sf' needs to be isolated and prior to 'openEo.gdalcubes'.
```
library(devtools)
install.packages("sf")
install_github(repo="ReneStalitza/openEo.gdalcubes",ref="main")
```

## Getting started
After starting the session, Landsat-8 demo files will be downloaded and an image collection created.
```
library(openEo.gdalcubes)
config = SessionConfig(api.port = 8000, host = "127.0.0.1")

## The change the default workspace path use:
## config$workspace.path = "<path/to/your/folder>"

createSessionInstance(config)
Session$startSession()

```

## Installation via docker
Alternatively you can run the webservice on a docker machine. The provided Dockerfile and starting R-Script will install and start the webservice. Simply build an image and run the container out of the directory containing this package:
```
docker build -t <IMAGENAME> .
docker run -p 8000:8000 <IMAGENAME>
```

# Running examples
The 'Example' folder provides some examples for the following clients:
* [openEo R-Client](https://github.com/Open-EO/openeo-r-client)
* [JS-Client](https://github.com/Open-EO/openeo-js-client) using Node.js


# Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://api.openeo.org/)
