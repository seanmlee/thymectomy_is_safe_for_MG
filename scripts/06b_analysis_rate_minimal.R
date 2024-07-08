library(tidyverse)




# read data --------------------------------------------------------------------
rate_summary_minimal <- read.csv(
  
  "data/rate_summary_minimal.csv", 
  header = TRUE
  
)



# scale year -------------------------------------------------------------------
rate_summary_minimal <- rate_summary_minimal %>%
  
  mutate(
    
    period = ifelse(
      operyr <= 2016,
      "pre",
      "post"
    )
    
  ) %>%
  
  filter(
    
    operyr >= 2012
    
  ) %>%
  
  mutate(
    
    year = operyr - 2011
    
  ) %>%
  
  mutate(
    
    difference = mg_total - mgthy_total
    
  )

rate_summary_minimal$period <- factor(
  
  rate_summary_minimal$period, 
  levels = c("pre", "post")
  
)




# fit model --------------------------------------------------------------------
model <- lm(
  
  mgthy_rate*100 ~ # multiply by 100 to put coefficient estimates on the percentage point scale
    year *
    period +
    mg_total,
  data = rate_summary_minimal
  
  )

summary(
  
  model
  
  )

car::vif(
  
  model, 
  type = "predictor"
  
  )




# plot -------------------------------------------------------------------------
critval <- 1.96


# predicted1 -------------------------------------------------------------------
newdata1 <- data.frame(
  
  year = seq(1, 5.5, length.out = 100),
  period = rep("pre", 100),
  mg_total = rep(30.5, 100)
  
)

preds1 <- predict(
  
  model, 
  newdata = newdata1, 
  type = "response", 
  se.fit = TRUE
  
)


fit_response1 <- preds1$fit

upr_response1 <- preds1$fit + (critval * preds1$se.fit)

lwr_response1 <- preds1$fit - (critval * preds1$se.fit)

newdata1 <- as.data.frame(
  
  cbind(
    newdata1,
    fit_response1,
    upr_response1,
    lwr_response1
  )
  
)




# predicted2 -------------------------------------------------------------------
newdata2 <- data.frame(
  
  year = seq(5.5, 10, length.out = 100),
  period = rep("post", 100),
  mg_total = rep(30.5, 100)
  
)

preds2 <- predict(
  
  model, 
  newdata = newdata2, 
  type = "response", 
  se.fit = TRUE
  
)

fit_response2 <- preds2$fit

upr_response2 <- preds2$fit + (critval * preds2$se.fit)

lwr_response2 <-preds2$fit - (critval * preds2$se.fit)

newdata2 <- as.data.frame(
  
  cbind(
    newdata2,
    fit_response2,
    upr_response2,
    lwr_response2
  )
  
)


rate_summary_minimal %>%
  
  ggplot(
    
    aes(
      x = year, 
      y = mgthy_rate
    )
    
  ) +
  
  scale_x_continuous(
    
    limits = c(1, 10),
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
    
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
  
  geom_point(
    
    size = 5,
    alpha = 0.4,
    aes(color = period)
    
  ) +
  
  geom_line(
    
    data = newdata1, 
    mapping = aes(
      x = year, 
      y = fit_response1 / 100
    ),
    color = "#FF5744"
    
  ) +
  
  geom_ribbon(
    
    data = newdata1,
    mapping = 
      aes(
        x = year,
        y = fit_response1 / 100,
        ymin = lwr_response1 / 100,
        ymax = upr_response1 / 100
      ),
    alpha = 0.1,
    fill = "#FF5744",
    color = NA
    
  ) +
  
  geom_line(
    
    data = newdata2, 
    mapping = aes(
      x = year, 
      y = fit_response2 / 100
    ),
    color = "#00BFC4"
    
  ) +
  
  geom_ribbon(
    
    data = newdata2,
    mapping = 
      aes(
        x = year,
        y = fit_response2 / 100,
        ymin = lwr_response2 / 100,
        ymax = upr_response2 / 100
      ),
    alpha = 0.1,
    fill = "#00BFC4",
    color = NA
    
  ) +
  
  ggtitle("b)") +
  
  xlab("Year") +
  
  ylab("Rate of Minimally Invasive Thymectomy for MG") +
  
  theme_bw() +
  
  theme(
    
    plot.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 25),
    legend.title = element_text(size = 25), 
    legend.text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
    
  ) +
  
  # legend and override alpha
  scale_color_manual(
    
    values = c("pre" = "#FF5744",
               "post" = "#00BFC4"), 
    
    labels = c("Pre-Publication",
               "Post-Publication"),
    
    name = ""
    
  ) +
  
  guides(
    
    color = guide_legend(
      override.aes = list(alpha = 1)
    )
    
  )




# write .png -------------------------------------------------------------------
ggsave(
  
  "out/plot_year_minimal.png", 
  height = 8, 
  width = 12
  
  )
