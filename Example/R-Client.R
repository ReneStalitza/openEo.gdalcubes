library(openeo)

# Connect to the backend and login with the default credentials
con = connect("http://127.0.0.1:8000")
login(user = "user",
      password = "password",
      login_type = "basic")

# Set the process builder
p = processes()

# Load the collection
datacube =  p$load_collection(id = "L8",
                       spatial_extent = list(west = -58,south = -5.5,east = -54.5,north = -2.9),
                       temporal_extent = c("2014-06-01", "2017-09-01"))

# Optionally filter the datacube for the desired bands
datacubeFiltered = p$filter_bands(data = datacube, bands = c("B04", "B05"))

# Create a function for a normalized difference calculation
normDiff = function(data, context) {
  red = data[1]
  nir = data[2]
  return((nir-red)/(nir+red))
}

# Reduce the datacube to a datacube including only the band of the normalized difference calculation
ndvi = p$reduce_dimension(data = datacubeFiltered, dimension = "bands", reducer = normDiff)

# Create a datacube including an evi calculation with the following function
evi = p$reduce_dimension(data = datacube, dimension = "bands", reducer = function(data ,context) {
  nir = p$array_element(data = data, label = "B05")
  red = p$array_element(data = data, label = "B04")
  blue = p$array_element(data = data, label = "B02")
  return(2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue+ 1)))
})

# Set a function for the following minimal ndvi calculation
min = function(data, context) {
  return(p$min(data))
}

# Reduce the datacube to the minimum values of the ndvi
minNdvi = p$reduce_dimension(data = ndvi, dimension = "t", reducer = min)


# Rename the band names
renamedNdvi = p$rename_labels(data = ndvi, dimension = "bands", target = "Ndvi")
renamedEvi = p$rename_labels(data = evi, dimension = "bands", target = "Evi", source = "band1")
renamedMinNdvi = p$rename_labels(data = minNdvi, dimension = "bands", target = "Min_Ndvi", source = 1)

# Merge Ndvi- and Evi-Datacube into one, merging with the minNdvi is not possible due to different dimensions
merged = p$merge_cubes(renamedNdvi, renamedEvi)

# Save result as last process including the offered file formats
formats = list_file_formats()

saveMerged = p$save_result(data = merged, format = formats$output$NetCDF)
saveRenamedMinNdvi = p$save_result(data = renamedMinNdvi, format = formats$output$GTiff)


# Process and download data synchronously
compute_result(graph = saveMerged, output_file = "merged_ndvi_evi.nc")

# Create and start a new batch job
job = create_job(graph = saveRenamedMinNdvi, title = "minNdvi", description = "Minimum NDVI calculation")
start_job(job = job)

# Get an overview of the job
describe_job(job = job)

# Get an overview of the created files and download them to the desired folder
list_results(job = job)
download_results(job = job, folder = "path/to/folder") 
