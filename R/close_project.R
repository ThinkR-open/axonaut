# curl -X PATCH "https://axonaut.com/api/v2/projects/1310" 
# -H  "accept: application/json" 
# -H  "userApiKey: 1508d8181637c2af807470c69a0043017f7c1508" 
# -H  "Content-Type: application/json" 
# -d "{ \"actual_end_ts\": 1583933107}"
# 
# project_id <- 1787
# userApiKey=readLines("~/.Axonaut2")


#' Title
#'
#' @param project_id 
#' @param actual_end_ts 
#' @param userApiKey 
#'
#' @return
#' @export
#' @importFrom glue glue
#' @importFrom httr PATCH
close_project <- function(project_id, actual_end_ts=Sys.time(), userApiKey = getOption("userApiKey")){
  
  if (is.null(userApiKey)){
    stop("missing userApiKey")
  }
  actual_end_ts <- unclass(actual_end_ts)
  
  PATCH(
    glue::glue("https://axonaut.com/api/v2/projects/{project_id}"),
    add_headers(
      "Content-Type" = "application/json",
      accept = "application/json",
      userApiKey = userApiKey
    ),body = 
      glue::glue("{
    \"actual_end_ts\": <actual_end_ts>}"
                 ,.open ="<",.close=">")) -> k
  
content(k)
  
}