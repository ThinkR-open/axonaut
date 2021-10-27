#' Title
#'
#' @param accountApiKey accountApiKey 
#'
#' @export
get_arrayProjects  <-
    function(accountApiKey = getOption("accountApiKey")) {
      POST(
        "https://axonaut.com/api/post/project/list",
        body = list(
          "accountApiKey" = accountApiKey#,
          # "startDateTS" = startDateTS,
          # "endDateTS " = endDateTS
        )
      ) %>% content()     %>%
        pluck("arrayProjects")
}

# arrayProjects[[149]] %>% names()
# arrayProjects[[149]] %>% pluck("id")
# arrayProjects[[149]] %>% pluck("name")
# arrayProjects[[149]] %>% pluck("number")
# arrayProjects[[149]] %>% select("number")
# arrayProjects[[149]] %>% discard(~is.list) %>% data.frame()
# arrayProjects[149:151] %>% map_df(~discard(.x,~is.list)%>% data.frame() )

# %>% data.frame()



# arrayProjects[[149]] %>% map("id")
# arrayProjects %>% map("id")
# arrayProjects %>% keep( ~names(.x) %in% "id")
# arrayProjects
#' Title
#'
#' @param arrayProjects arrayProjects 
#'
#' @export
#'
get_projects <- function(arrayProjects = get_arrayProjects()){
  # arrayProjects %>% map(~purrr::discard(.x,is.list))
           # %>% data.frame()
  # arrayProjects%>% 
    # map(~purrr::discard(.x,is.list))
  arrayProjects %>% 
    map(as.matrix) %>%
    map(t) %>% 
    map(as.data.frame) %>%
    map(~select(.x,-workforces,-parentProject,-projectNature,-sonProjects)) %>% 
    map(~mutate(.x,projectStatus = projectStatus %>% unlist() %>% .[2] %>% unname())) %>% 
    # map(  ~mutate_all(.x,~list(unlist(.)))) %>% # ptet pas le faire sur idCompany
    map(  ~mutate_all(.x,~unlist(.))) %>% # ptet pas le faire sur idCompany
    map(~mutate(.x,idCompany = as.character(idCompany))) %>%
    bind_rows()
  
}
# 
# 
# arrayProjects[[4]] %>% purrr::discard(is.list)
# arrayProjects%>% map(~purrr::discard(.x,is.list)) %>% map(names) %>% bind_rows(.id = "cou")
# 
# arrayProjects[1:3] %>% 
#   map(~purrr::discard(.x,is.list)) %>%
#   map(names) %>% 
#   rbind_list()
# 
# as.data.frame(t(as.matrix(l[[1]])))
# as.data.frame(t(as.matrix(l[1:3])))
# 
# l %>% map(as.matrix) %>% map(t) %>% map(as.data.frame) %>% rbind_list() %>% select(-comments) %>%  mutate_all(unlist)
# l %>% map(as.matrix) %>% map(t) %>% map(as.data.frame) %>%
#   map(  ~mutate_all(.x,unlist)) %>% map(~mutate(.x,idCompany = as.character(idCompany))) %>% bind_rows()
#   
#   
#   rbind_list() %>% select(-comments) %>%  mutate_all(unlist)
# l %>% map(as.matrix) %>% map(t) %>% map(as.data.frame) -> kk
# kk[[207]] %>% mutate_all(unlist)
