
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
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  nb_page <- 1
  try(
  nb_page <- content(GET(
    url,
    add_headers(
      accept = "application/json",
      userApiKey = userApiKey
    )
  ))$`error`$pages,silent = TRUE
  )
  if (is.null(nb_page)){
    nb_page <- 1
  }
  
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
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
    get_all(what="companies",userApiKey = userApiKey)
}


#' Title
#'
#' @param userApiKey 
#'
#' @return
#' @export
#'
get_all_contracts<- function(userApiKey = getOption("userApiKey")){
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
  get_all(what="contracts",userApiKey = userApiKey)
}


#' Title
#'
#' @param userApiKey 
#'
#' @return
#' @export
#'
get_all_quotations<- function(userApiKey = getOption("userApiKey")){
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
  get_all(what="quotations",userApiKey = userApiKey)
}



#' Title
#'
#' @param userApiKey 
#'
#' @return
#' @export
#'
get_all_invoices<- function(userApiKey = getOption("userApiKey")){
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
  get_all(what="invoices",userApiKey = userApiKey)
}


#' Title
#'
#' @param userApiKey 
#'
#' @return
#' @export
#'
get_all_project <- function(userApiKey = getOption("userApiKey")){
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
    get_all(what="projects",userApiKey = userApiKey)  %>%
    # content() %>%
    map(~.x[c("id","name","number","company_id")]) %>%
    map(as.character ) %>%
    do.call("rbind",.) %>%
    as.data.frame() %>%
    as.tbl() %>%
    set_names(c("id","name","number","company_id"))
  
  
}

get_project_id <- function(number,
  all_project = get_all_project(userApiKey = userApiKey),
  userApiKey = getOption("userApiKey")){
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
 out <-  all_project %>% 
    filter(number == !!number) %>% 
    pull("id") %>% as.character()
  if (length(out)>1){
    
    message(glue::glue("attention plusieurs project avec le meme number : {number}"))
  }
 
 out
 }


get_company_id <- function(company_name,
                           all_companies =get_all_companies(userApiKey = userApiKey),
                           userApiKey = getOption("userApiKey")){
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
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
  company_name= NULL,
  name = number, 
  commentaire = name,
  company_id = NULL,
  all_project = get_all_project(userApiKey = userApiKey),
  userApiKey = getOption("userApiKey")
){
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
  if (is.null(company_id) & is.null(company_name)){
    return(create_project_without_company(number = number,
                                          name = name,
                                          commentaire=commentaire,
                                          all_project = all_project,
                                          userApiKey = userApiKey))
  }
  
  
  if (is.null(company_id)){  
  company_id = get_company_id(company_name,userApiKey = userApiKey)
  }
  

  

  
  
  
  # on va vierie que le projet n'existe pas déja
  if (  exist_project(number = number,all_project = all_project,userApiKey = userApiKey)){
    message(glue::glue(" le projet {number} exist deja"))
    return(get_project_id(number = number,all_project =all_project,userApiKey =  userApiKey))
    
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




create_project_without_company <- function(
  number,
  name = number, 
  commentaire = name,
  all_project = get_all_project(userApiKey = userApiKey),
  userApiKey = getOption("userApiKey")
){
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
  if (  exist_project(number = number,all_project = all_project,userApiKey = userApiKey)){
    message(glue::glue(" le projet {number} exist deja"))
    return(get_project_id(number = number,all_project =all_project,userApiKey =  userApiKey))
    
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
    \"comments\": \"<commentaire>\"}"
                 ,.open ="<",.close=">")) %>% 
    content()
  
}


#' Title
#'
#' @param name 
#' @param all_companies 
#' @param userApiKey 
#'
#' @return
#' @export
#'
create_companie <- function(name,
                            address_street="",
                            address_zip_code="",
                            address_city="",
                            address_country="",
                            comments="",
                             all_companies =get_all_companies(userApiKey = userApiKey),
                             userApiKey = getOption("userApiKey")){
  
 if (is.null(userApiKey) || userApiKey == "" ){
   stop("missing userApiKey")
 }
  
  
  if ( is.null(name) || name == ""  ){
    message("no companie created")
    return(NULL)
    
  }
  
  
   # all_companies %>% 
    # keep(~.x$name == company_name)
  
  
  if (exist_companie(name = name,all_companies = all_companies,userApiKey = userApiKey)){
    message(glue::glue("the companie '{name}' already exist"))
    return(get_company_id(name,all_companies = all_companies,userApiKey = userApiKey))
  }
  
  httr::POST(
    "https://axonaut.com/api/v2/companies",
    add_headers(
      accept = "application/json",
      userApiKey = userApiKey,
      `Content-Type` = "application/json"
    ),body = 
      glue::glue("{
    \"name\": \"<name>\",
    \"address_street\": \"<address_street>\",
    \"address_zip_code\": \"<address_zip_code>\",
    \"address_city\": \"<address_city>\",
    \"address_country\": \"<address_country>\",
    \"comments\": \"<comments>\"}"
                 ,.open ="<",.close=">")) %>% 
    content()
  
  
  
  
}


exist_companie <- function(name,
                           all_companies =get_all_companies(userApiKey = userApiKey),
                           userApiKey = getOption("userApiKey")){
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
  
  length(intersect(name,  all_companies %>% map_chr("name")  ))> 0
 
  
}


exist_project <- function(number,
                          all_project = get_all_project(userApiKey = userApiKey),
                          userApiKey = getOption("userApiKey")
                          ){
  length(intersect(all_project$number,  number)) > 0
  }
# 
# -d "{  \"name\": \"string\",  \"address_street\": \"string\", 
# \"address_zip_code\": \"string\",  \"address_city\": \"string\", 
# \"address_country\": \"string\",  \"comments\": \"string\", 
# \"custom_fields\": {}, 
# \"categories\": [    \"string\"  ],  \"internal_id\": \"string\"}"
# 

