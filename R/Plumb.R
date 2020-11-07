
# plumber.R

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Lookup a user
#* @get /users/<id>
function(id){
  subset(users, uid %in% id)
}

#* Test
#* @get /test
function(){
  barABC()
}
