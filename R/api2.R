
#' Title
#'
#' @param what 
#' @param url 
#' @param userApiKey 
#'
#' @return
#' @export
#' @import httr 
#' @import magrittr
#' @import purrr
get_all <- function(what,url = glue::glue("https://axonaut.com/api/v2/{what}"),userApiKey = getOption("userApiKey")){
  
  
  nb_page <- content(GET(
    url,
    add_headers(
      accept = "application/json",
      userApiKey = userApiKey
    )
  ))$`error`$pages
  
  tt <-  seq_len(nb_page) %>% 
    map(~
          GET(url,
            add_headers(
              accept = "application/json",page = .x,
              userApiKey = userApiKey
            )
          )
        
    )
  tt %>% map(content) %>% flatten()
  
}



#' Title
#'
#' @param userApiKey 
#'
#' @return
#' @export
#'
get_all_companies <- function(userApiKey = getOption("userApiKey")){
  get_all(what="companies",userApiKey = userApiKey)
}

#' Title
#'
#' @param userApiKey 
#'
#' @return
#' @export
#'
get_all_project <- function(userApiKey = getOption("userApiKey")){
  get_all(what="projects",userApiKey = userApiKey)  %>%
    # content() %>%
    map(~.x[c("id","name","number","company_id")]) %>%
    map(as.character ) %>%
    do.call("rbind",.) %>%
    as.data.frame() %>%
    as.tbl() %>%
    set_names(c("id","name","number","company_id"))
  
  
}

get_company_id <- function(company_name,
                           all_companies =get_all_companies(userApiKey = userApiKey),
                           userApiKey = getOption("userApiKey")){
  
  all_companies %>% 
    keep(~.x$name == company_name) %>% 
    map_chr("id") 
  
}




#' Title
#'
#' @param number 
#' @param company_name 
#' @param name 
#' @param commentaire 
#' @param company_id 
#' @param userApiKey 
#'
#' @return
#' @export
#'
#' @import httr
create_project <- function(
  number,
  company_name,
  name = number, 
  commentaire = name,
  company_id = get_company_id(company_name),
  userApiKey = getOption("userApiKey")
){
  if (is.null(company_id)){
    stop("company_id is null")
    
  }
  httr::POST(
    "https://axonaut.com/api/v2/projects",
    add_headers(
      accept = "application/json",
      userApiKey = userApiKey,
      `Content-Type` = "application/json"
    ),body = 
      glue::glue("{  \"number\": \"<number>\",
    \"name\": \"<name>\",
    \"comments\": \"<commentaire>\",
    \"company_id\": <company_id>}"
                 ,.open ="<",.close=">")) %>% 
    content()
  
  
  
  
  
}
