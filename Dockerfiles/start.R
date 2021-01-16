
library(openEo.gdalcubes)


config = SessionConfig(api.port = 8000, host = "0.0.0.0")

config$workspace.path = "/var/openeo/workspace"


r_server_app_port_param = "R_SERVER_PORT"
r_server_user_workspace_param = "R_SERVER_WORKSPACE_PATH"
r_server_data_path_param = "R_SERVER_DATA_PATH"

env_variables = names(Sys.getenv())

if (r_server_user_workspace_param %in% env_variables) {
  config$workspace.path = Sys.getenv(r_server_user_workspace_param)
}

if (r_server_data_path_param %in% env_variables) {
  config$data.path = Sys.getenv(r_server_data_path_param)
}

createSessionInstance(config)


if (r_server_app_port_param %in% env_variables) {
  port = Sys.getenv(r_server_app_port_param)
} else {
  port = 8000
}

Session$startSession()
