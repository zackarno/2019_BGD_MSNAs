
ms <- function(data,ms) {
  data %>% select(starts_with(ms)) %>% colnames() %>% dput()
  
}
