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
rate_summary <- left_join(
  
  puf, 
  thy
  
  )


rate_summary <- left_join(
  
  rate_summary, 
  mg
  
  )


rate_summary <- left_join(
  
  rate_summary, 
  thymoma
  
  )


# replace na
rate_summary["thy"][is.na(rate_summary["thy"])] <- 0 
rate_summary["mg"][is.na(rate_summary["mg"])] <- 0 
rate_summary["thymoma"][is.na(rate_summary["thymoma"])] <- 0 


# exclude thymoma
rate_summary <- rate_summary %>%
  
  filter(
    
    thymoma != 1
    
    )


# create mgthy indicator column (which is 1 when mg = 1 and thy = 1) -----------
rate_summary <- rate_summary %>% 
  
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
  
  rate_summary, 
  "data/rate_summary.csv", 
  row.names = FALSE
  
  )
