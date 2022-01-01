to_matrix = function(tbl, rownames = NULL){
  
  tbl %>%
    {
      if(
        !tbl %>% 
        { if(!is.null(rownames)) (.) %>% dplyr::select(- contains(rownames)) else (.) } %>%
        dplyr::summarise_all(class) %>% 
        tidyr::gather(variable, class) %>%
        pull(class) %>% unique %>% identical("numeric")
      ) warning("to_matrix says: there are NON-numerical columns, the matrix will NOT be numerical")
      
      (.)
      
    } %>%
    as.data.frame() %>%
    { 
      if(!is.null(rownames)) 
        (.) %>% 
        magrittr::set_rownames(tbl %>% pull(!!rownames)) %>%
        dplyr::select(- !!rownames)
      else (.) 
    } %>%
    as.matrix()
}