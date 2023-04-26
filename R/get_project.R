#' Title
#'
#' @param accountApiKey accountApiKey 
#'
#' @export
get_arrayProjects  <-
    function(accountApiKey = getOption("accountApiKey")) {
      message("deprecated use XXX instead")
      stop("deprecated")
}

#' Title
#'
#' @param userApiKey userApiKey 
#'
#' @export
#'
get_projects <-  function(userApiKey = getOption("userApiKey")){
  
  if (is.null(userApiKey)  || userApiKey == ""){
    stop("missing userApiKey")
  }
 pp <- get_all(what="projects",userApiKey = userApiKey) 
  
 
 pp %>% 
   map(compact) %>%
   # map(~select(.x,- workforce))
   # discard(names("workforce")
   map_dfr(as.data.frame) %>%
   select(-starts_with("workforce")) %>% 
   select(-starts_with("project_nature")) %>% 
   select(-starts_with("parent_project")) %>% 
   select(-starts_with("son_projects")) %>% 
   select(id = id,project_number = number     , name,comments,idCompany=company_id,estimatedStart =estimated_start ,
          estimatedEnd = estimated_end ,estimatedNbHours =estimated_hours,estimatedCost=estimated_cost ,
          
          estimatedRevenue = estimated_revenue ,actualStartDate=actual_start ,actualEndDate=actual_end ,
          actualExpensesCost =  actual_expenses_cost 
          ) %>% 
   mutate_at(c("estimatedCost" ,"estimatedRevenue" ,"estimatedNbHours"),thinkr::as_mon_numeric) 
  # 
  # # arrayProjects %>% map(~purrr::discard(.x,is.list))
  #          # %>% data.frame()
  # # arrayProjects%>% 
  #   # map(~purrr::discard(.x,is.list))
  # arrayProjects %>% 
  #   map(as.matrix) %>%
  #   map(t) %>% 
  #   map(as.data.frame) %>%
  #   map(~select(.x,-workforces,-parentProject,-projectNature,-sonProjects)) %>% 
  #   map(~mutate(.x,projectStatus = projectStatus %>% unlist() %>% .[2] %>% unname())) %>% 
  #   # map(  ~mutate_all(.x,~list(unlist(.)))) %>% # ptet pas le faire sur idCompany
  #   map(  ~mutate_all(.x,~unlist(.))) %>% # ptet pas le faire sur idCompany
  #   map(~mutate(.x,idCompany = as.character(idCompany))) %>%
  #   bind_rows()
  
}




