

# install {nih.joinpoint} ------------------------------------------------------
# per https://github.com/DanChaltiel/nih.joinpoint, 
# "As for now, you can only install the development version of this package from GitHub:"
devtools::install_github("DanChaltiel/nih.joinpoint")


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(nih.joinpoint)


# load data --------------------------------------------------------------------
nsqip_jp <- read.csv("out/nsqip.csv", header = TRUE)


# format -----------------------------------------------------------------------
nsqip_jp <- nsqip_jp %>%
  
  dplyr::select(
    
    caseid, 
    cpt,
    operyr,
    thy,
    mg
    
  ) %>%
  
  mutate(
    
    operyr = as.character(operyr),
    
    operyr = case_when(
      operyr <= 2012 ~ "2005-2012",
      operyr > 2012 ~ operyr
    )
    
  )


# calculate rates --------------------------------------------------------------
nsqip_jp <- nsqip_jp %>% 
  
  mutate(
    
    thy_for_mg = case_when(
      (mg == 1 & thy == 1) ~ 1,
      TRUE ~ 0
    )
    
  ) %>%
  
  group_by(
    
    operyr
    
  ) %>% 
  
  summarize(
    
    n_thy = n(), 
    
    n_mg = sum(mg == 1),
    
    n_thy_for_mg = sum(thy_for_mg == 1),
    
    rate_thy_for_mg = (n_thy_for_mg / n_mg)
    
  ) %>%
  
  mutate(
    year = seq(1:12)
  )


# fit joinpoint model ----------------------------------------------------------
run_opt = run_options(
  model = "ln", 
  max_joinpoints = 3, 
  n_cores = 3
)

export_opt = export_options()

jp = joinpoint(
  nsqip_jp, 
  x = year, 
  y = rate_thy_for_mg,
  run_opts = run_opt,
  export_opts = export_opt
)

jp_apc <- jp$apc
jp_p <- jp$report$slope_chg_p_value
jp_plot(jp)


# plot joinpoint model predictions ---------------------------------------------
jp_plot(jp) + 
  
  geom_point(size = 2) +
  
  scale_x_continuous(
    limits = c(1, 12),
    breaks = 1:12,
    labels = c("2005-2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.01),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = scales::percent
  ) +
  
  geom_vline(
    xintercept = 5.5,
    alpha = 0.4,
    color = "black",
    linetype = "dashed"
  ) +
  
  ggtitle("a)") +
  
  xlab("Year") +
  
  ylab("Thymectomy for MG") +
  
  theme_bw() +
  
  theme(
    
    legend.position = "none",
    plot.title = element_text(size = 15),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
    
  )


# out --------------------------------------------------------------------------
ggsave(
  "out/joinpoint.png",
  width = 5,
  height = 4
  )



# rates for table --------------------------------------------------------------
tbl_nsqip_jp <- read.csv("out/nsqip.csv", header = TRUE) %>%
  
  dplyr::select(
    
    caseid, 
    operyr,
    thy,
    mg
    
  ) %>%
  
  mutate(
    
    thy_for_mg = case_when(
      (mg == 1 & thy == 1) ~ 1,
      TRUE ~ 0
    )
    
  ) %>%
  
  group_by(
    
    operyr
    
  ) %>% 
  
  summarize(
    
    n_thy = n(), 
    
    n_mg = sum(mg == 1),
    
    n_thy_for_mg = sum(thy_for_mg == 1),
    
    rate_thy_for_mg = (n_thy_for_mg / n_mg)
    
  )


# write ------------------------------------------------------------------------
write.csv(tbl_nsqip_jp, "out/tbl_nsqip_jp.csv", row.names = FALSE)
