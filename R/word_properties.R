word_type_property_summary <- function() {
  
  x <- read.csv("data-raw/stimuli.csv") %>%
    select(-id) %>%
    tidyr::gather(word_type, word)
  
  y <- read.csv("data-raw/word_properties.csv") 
  
  z <- left_join(x, y, by = c("word" = "Word"))
  
  S <- filter(z, !is.na(Length)) %>%
    group_by(word_type) %>%
    summarise_at(c("SUBTLWF", "Length"),
                 c("mean", "sd", "min", "max")
                 )
  return(S)
}


