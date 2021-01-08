// Make the client available to the Node.js script
// Also include the Formula library for simple math expressions
const { OpenEO, Formula } = require('@openeo/js-client');

async function example() {
  // Connect to the back-end
  var con = await OpenEO.connect("http://127.0.0.1:8000");

  // Authenticate via Basic authentication with default credentials
  await con.authenticateBasic("user", "password");

  // Create a process builder
  var builder = await con.buildProcess();

  // Load the collection
  var datacube = builder.load_collection(
    "L8",
    {west: -58, south: -5.5, east: -54.5, north: -2.9},
    ["2014-01-01", "2016-01-01"]
  );

//var normDiff = new Formula('(B05-B04)/(B05+B04)')
  // Create a function for a normalized difference calculation
  var normDiff = function(data, context) {
    var B04 = this.array_element(data, 5);
    var B05 = this.array_element(data, 6);
    return this.divide(
             this.subtract(B05, B04),
             this.add(B05, B04));
  };

  // Reduce the datacube to a datacube including only the band of the normalized difference calculation
  var  ndvi = builder.reduce_dimension(datacube, normDiff, "bands");


  // Create a datacube including an evi calculation with the following function
  var  evi = builder.reduce_dimension(datacube, function(data, context) {
    var nir = this.array_element(data, 6);
    var red = this.array_element(data, 5);
    var blue = this.array_element(data, 3);
    return this.multiply(
      2.5,
      this.divide(
        this.subtract(nir, red),
        this.subtract(
          this.add(
            nir,
            this.multiply(
              6,
              red
            )
          ),
          this.add(
            1,
            this.multiply(
              7.5,
              blue
        )))));
  }, "bands");

  // Set a function for the following minimal ndvi calculation
  var min = function(data, context) {
    return(this.min(data))
  };

  // Reduce the datacube to the minimum values of the ndvi
  var minNdvi = builder.reduce_dimension(ndvi, min, "t");

  // Rename the band names
  var renamedNdvi = builder.rename_labels(ndvi, "bands", "Ndvi");
  var renamedEvi = builder.rename_labels(evi, "bands", "Evi", "band1");
  var renamedMinNdvi = builder.rename_labels(minNdvi, "bands", "Min_Ndvi", 1);

  // Merge Ndvi- and Evi-Datacube into one, merging with the minNdvi is not possible due to different dimensions
  var merged = builder.merge_cubes(renamedNdvi, renamedEvi);

  // Save result as last process including the offered file formats
  var formats = await con.listFileTypes();

  var saveMerged = builder.save_result(merged, formats.data.output.NETCDF);
  var saveRenamedMinNdvi = builder.save_result(renamedMinNdvi, formats.data.output.GTIFF);

  // Process and download data synchronously
   await con.downloadResult(saveMerged, "merged_ndvi_evi.nc");

  // Create and start a new batch job
  var job = await con.createJob(saveRenamedMinNdvi, "minNdvi");
  await job.startJob();

  // Get an overview of the created files and download them to the desired folder
  x = await job.listResults();
 console.log(x);
  await job.downloadResults("C:/Users/ReneS/project/output/");

}
example().catch(error => console.error(error));
