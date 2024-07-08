library(tidyverse)




# read puf ---------------------------------------------------------------------
puf <- read.csv(
  
  "data/puf.csv", 
  header = TRUE
  
)

# filter thymoma patients using ICD9=164.0 & 212.6; ICD10=C37 --------------------------------
thymoma <- puf %>%
  
  select(
    
    -operyr
    
  ) %>%
  
  pivot_longer(
    
    cols = c(
      podiag, 
      podiag10
    ),
    names_to = "icd",
    values_to = "code"
    
  ) %>%
  
  filter(
    
    code != ""
    
  ) %>%
  
  select(
    
    caseid,
    code
    
  )


thymoma <- thymoma[grepl("^(C37|164.0|212.6)", thymoma$code), ]


thymoma <- distinct(thymoma, caseid, .keep_all = TRUE)




# create thymoma indicator column ---------------------------------------------------
thymoma$thymoma <- 1




# remove temporary code column -------------------------------------------------
thymoma <- thymoma %>%
  
  select(
    
    -code
    
  )




# write to .csv ----------------------------------------------------------------
write.csv(
  
  thymoma, 
  "data/thymoma.csv", 
  row.names = FALSE
  
)
