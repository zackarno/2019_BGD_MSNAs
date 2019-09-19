
ms <- function(ms) {
  hh_data %>% select(starts_with(ms)) %>% colnames() %>% dput()
  
}