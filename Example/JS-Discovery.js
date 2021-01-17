// Make the client available to the Node.js script
// Also include the Formula library for simple math expressions
const { OpenEO, Formula } = require('@openeo/js-client');

async function example() {
  // Connect to the back-end
  var con = await OpenEO.connect("http://127.0.0.1:8000");

  // Show implemented API version of the back-end
  var capabilities = con.capabilities();
  console.log("Server API version: " + capabilities.apiVersion());

  // List collection names
  var collections = await con.listCollections();
  console.log("Collections: " + collections.collections.map(c => c.id).join(', '));

  // List process ids
  var processes = await con.listProcesses();
  console.log("Processes: " + processes.processes.map(p => p.id).join(', '));

  // List supported file types
  var types = await con.listFileTypes();
  console.log("Output file formats: " + Object.keys(types.getOutputTypes()).join(', '));

  // List Bands of provided collection
  var collection = await con.describeCollection("L8");
  console.log("Bands:" + collection['cube:dimensions'].bands.values.map(p => p).join(', '));
}
example().catch(error => console.error(error));
