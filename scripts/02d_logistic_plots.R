

# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggsignif)


# critval ----------------------------------------------------------------------
critval <- 1.96


# predicted_narrow0 ------------------------------------------------------------
newdata_narrow0 <- data.frame(mg = "0")

preds_narrow0 <- predict(
  
  mod_narrow, 
  newdata = newdata_narrow0, 
  type = "link", 
  se.fit = TRUE
  
)

fit_link_narrow0 <- preds_narrow0$fit

fit_response <- mod_narrow$family$linkinv(fit_link_narrow0)

upr_link_narrow0 <- preds_narrow0$fit + (critval * preds_narrow0$se.fit)

lwr_link_narrow0 <- preds_narrow0$fit - (critval * preds_narrow0$se.fit)

upr_response <- mod_narrow$family$linkinv(upr_link_narrow0)

lwr_response <- mod_narrow$family$linkinv(lwr_link_narrow0)

fit_narrow0 <- as.data.frame(
  
  cbind(
    newdata_narrow0,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)

fit_narrow0$outcome <- "Narrow"


# predicted_narrow1 ------------------------------------------------------------
newdata_narrow1 <- data.frame(mg = "1")

preds_narrow1 <- predict(
  
  mod_narrow, 
  newdata = newdata_narrow1, 
  type = "link", 
  se.fit = TRUE
  
)

fit_link_narrow1 <- preds_narrow1$fit

fit_response <- mod_narrow$family$linkinv(fit_link_narrow1)

upr_link_narrow1 <- preds_narrow1$fit + (critval * preds_narrow1$se.fit)

lwr_link_narrow1 <- preds_narrow1$fit - (critval * preds_narrow1$se.fit)

upr_response <- mod_narrow$family$linkinv(upr_link_narrow1)

lwr_response <- mod_narrow$family$linkinv(lwr_link_narrow1)

fit_narrow1 <- as.data.frame(
  
  cbind(
    newdata_narrow1,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)

fit_narrow1$outcome <- "Narrow"


# predicted_broad0 -------------------------------------------------------------
newdata_broad0 <- data.frame(mg = "0")

preds_broad0 <- predict(
  
  mod_broad, 
  newdata = newdata_broad0, 
  type = "link", 
  se.fit = TRUE
  
)

fit_link_broad0 <- preds_broad0$fit

fit_response <- mod_broad$family$linkinv(fit_link_broad0)

upr_link_broad0 <- preds_broad0$fit + (critval * preds_broad0$se.fit)

lwr_link_broad0 <- preds_broad0$fit - (critval * preds_broad0$se.fit)

upr_response <- mod_broad$family$linkinv(upr_link_broad0)

lwr_response <- mod_broad$family$linkinv(lwr_link_broad0)

fit_broad0 <- as.data.frame(
  
  cbind(
    newdata_broad0,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)

fit_broad0$outcome <- "Broad"


# predicted_broad1
newdata_broad1 <- data.frame(mg = "1")

preds_broad1 <- predict(
  
  mod_broad, 
  newdata = newdata_broad1, 
  type = "link", 
  se.fit = TRUE
  
)

fit_link_broad1 <- preds_broad1$fit

fit_response <- mod_broad$family$linkinv(fit_link_broad1)

upr_link_broad1 <- preds_broad1$fit + (critval * preds_broad1$se.fit)

lwr_link_broad1 <- preds_broad1$fit - (critval * preds_broad1$se.fit)

upr_response <- mod_broad$family$linkinv(upr_link_broad1)

lwr_response <- mod_broad$family$linkinv(lwr_link_broad1)

fit_broad1 <- as.data.frame(
  
  cbind(
    newdata_broad1,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)

fit_broad1$outcome <- "Broad"


# combine
fit <- rbind(
  
  fit_narrow0,
  fit_narrow1,
  fit_broad0,
  fit_broad1
  
)


# plot
fit %>%
  
  ggplot(
    
    aes(
      x = outcome, 
      y = fit_response,
      fill = mg
    )
    
  ) +
  
  geom_errorbar(
    
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    width = 0.05,
    alpha = 1,
    position = position_dodge(width = 0.5)
    
  ) +
  
  geom_point(
    
    pch = 21,
    color = "black",
    size = 3,
    position = position_dodge(width = 0.5)
    
  ) +
  
  scale_y_continuous(
    
    labels = scales::percent,
    limits = c(0, 0.15)
    
  ) +
  
  ggtitle("") +
  
  xlab("Composite Outcome") +
  
  ylab("Probability of Outcome") +
  
  labs(fill = "Diagnosis") +
  
  scale_fill_manual(
    
    values = c("0" = "#F8766D", "1" = "#00BFC4"),
    labels = c("Non-MG", "MG")
    
  ) +
  
  theme_bw() +
  
  theme(
    
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12.5)
    
  ) +
  
  geom_signif(
    
    y_position = 0.1075, 
    xmin = 0.875,
    xmax = 1.125,
    annotation = "n.s.", 
    tip_length = 0.025,
    color = "black",
    textsize = 5,
    vjust = -0.5
    
  )  +
  
  geom_signif(
    
    y_position = 0.0625, 
    xmin = 1.875,
    xmax = 2.125,
    annotation = "n.s.", 
    tip_length = 0.025,
    color = "black",
    textsize = 5,
    vjust = -0.5
    
  )




# write .png -------------------------------------------------------------------
ggsave(
  
  "out/logistic.png",
  height = 4,
  width = 6
  
)
