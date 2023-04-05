
#' Title
#'
#' @param accountApiKey token api
#'
#' @import httr
#' @import purrr
get_arrayquotations <-
  function(accountApiKey = getOption("accountApiKey")) {
    message("deprecated use XXX instead")
    stop("deprecated")
    
  }

#' Title
#'
#' @param arrayquotations 
#'
#' @return
#' @export
#' @importFrom  purrr map_df  discard
get_quotations <- function(quotations = get_arrayquotations()){
  
  quotations %>%
    map_df(~discard(.x,~is.list)%>% data.frame() )
  
 
}
# quotations[1:4] %>% map(~pluck(.x,"number"))
# quotations[quotations %>% map_lgl(~pluck(.x,"number") == "190129003") ]
# 
# quotations %>% keep(~map_lgl(.x,~pluck(.x,"number") == "190129003") )
# 190129003
# 
# 
# arrayProjects = get_arrayProjects()
# 
# arrayProjects[1]
# arrayProjects[arrayProjects %>% map_lgl(~pluck(.x,"number") == "form_edf_sgmcc") ]
# 14856
# 
# quotations[1:4] %>% length()
# 
# 190129003 