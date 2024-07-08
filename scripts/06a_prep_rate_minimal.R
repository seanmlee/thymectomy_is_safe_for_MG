library(tidyverse)




# read puf, thy, mg ------------------------------------------------------------
puf <- read.csv(
  
  "data/puf.csv", 
  header = TRUE
  
) %>% 
  
  select(
    
    caseid, 
    operyr
    
  )


thy <- read.csv(
  
  "data/thy.csv", 
  header = TRUE
  
)  %>%
  
  filter(
    
    cpt == 32673
    
    )


mg <- read.csv(
  
  "data/mg.csv", 
  header = TRUE
  
  )


thymoma <- read.csv(
  
  "data/thymoma.csv", 
  header = TRUE
  
  )



# format -----------------------------------------------------------------------
# join
rate_summary_minimal <- left_join(
  
  puf, 
  thy
  
  )


rate_summary_minimal <- left_join(
  
  rate_summary_minimal, 
  mg
  
  )


rate_summary_minimal <- left_join(
  
  rate_summary_minimal, 
  thymoma
  
  )


# replace na
rate_summary_minimal["thy"][is.na(rate_summary_minimal["thy"])] <- 0 
rate_summary_minimal["mg"][is.na(rate_summary_minimal["mg"])] <- 0 
rate_summary_minimal["thymoma"][is.na(rate_summary_minimal["thymoma"])] <- 0 


# exclude thymoma
rate_summary_minimal <- rate_summary_minimal %>%
  
  filter(
    
    thymoma != 1
    
    )



# create mgthy indicator column (which is 1 when mg = 1 and thy = 1) -----------
rate_summary_minimal <- rate_summary_minimal %>% 
  
  mutate(
    
    mgthy = case_when(
      (mg == 1 & thy == 1) ~ 1,
      TRUE ~ 0
    )
    
  ) %>%
  
  group_by(
    
    operyr
    
  ) %>% 
  
  summarize(
    
    n = n(), 
    
    thy_total = sum(
      thy == 1
    ),
    
    mg_total = sum(
      mg == 1
    ),
    
    mgthy_total = sum(
      mgthy == 1
    ),
    
    thy_rate = sum(
      (thy == 1) / n),
    
    mg_rate = sum(
      (mg == 1) / n),
    
    mgthy_rate = (mgthy_total / mg_total)
    
  )




# write .csv -------------------------------------------------------------------
write.csv(
  
  rate_summary_minimal, 
  "data/rate_summary_minimal.csv", 
  row.names = FALSE
  
)
