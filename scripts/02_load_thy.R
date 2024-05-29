library(RSQLite)
library(tidyverse)




# connect db -------------------------------------------------------------------
conn <- dbConnect(
  
  RSQLite::SQLite(), 
  "nsqip.db"
  
  )




# query puf --------------------------------------------------------------------
thy <- dbGetQuery(
  
    conn, 
    "SELECT * 
    FROM cpt"
    
    )




# create thymectomy cpt code vector --------------------------------------------
code <- c(
  
  # thymectomy cpt codes
  60520, # transcervical
  60521, # transsternal without radical mediastinal dissection
  60522, # transsternal with radical mediastinal dissection
  32673  # minimally invasive
  
  )




# filter thymectomy cpt codes --------------------------------------------------
thy <- thy %>%
  
  select(
    
    caseid,
    cpt
    
    ) %>%
  
  filter(
    
    cpt %in% code
    
    )




# create thy indicator column --------------------------------------------------
thy$thy <- 1




# write to .csv ----------------------------------------------------------------
write.csv(
  
  thy, 
  "data/thy.csv", 
  row.names = FALSE
  
  )
