library(tidyverse)




# read puf ---------------------------------------------------------------------
puf <- read.csv(
  
  "data/puf.csv", 
  header = TRUE
  
  )

# filter mg patients using icd9=358 & icd10=G70 --------------------------------
mg <- puf %>%
  
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


mg <- mg[grepl("^(G70|358)", mg$code), ]


mg <- distinct(mg, caseid, .keep_all = TRUE)




# create mg indicator column ---------------------------------------------------
mg$mg <- 1




# remove temporary code column -------------------------------------------------
mg <- mg %>%
  
  select(
    
    -code
    
    )




# write to .csv ----------------------------------------------------------------
write.csv(
  
  mg, 
  "data/mg.csv", 
  row.names = FALSE
  
  )
