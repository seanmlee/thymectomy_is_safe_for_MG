library(tidyverse)
library(ggsignif)
library(table1)




# read data --------------------------------------------------------------------
kam <- read.csv(
  
  "data/kam.csv", 
  header = TRUE
  
)




# format mortality -------------------------------------------------------------
kam <- kam %>%
  
  mutate(
    
    mortality = ifelse(
      is.na(dopertod),
      0,
      1
    )
    
  )




# factor -----------------------------------------------------------------------
kam$mg <- as.factor(kam$mg)
kam$sex <- as.factor(kam$sex)
kam$diabetes <- as.factor(kam$diabetes)
kam$hypermed <- as.factor(kam$hypermed)
kam$dialysis <- as.factor(kam$dialysis)
kam$asaclas <- as.factor(kam$asaclas)
kam$steroid <- as.factor(kam$steroid)
kam$othdvt <- as.factor(kam$othdvt)
kam$inout <- as.factor(kam$inout)
kam$failwean <- as.factor(kam$failwean)
kam$transfus <- as.factor(kam$transfus)
kam$mortality <- as.factor(kam$mortality)
kam$wndinfd <- as.factor(kam$wndinfd)
kam$supinfec <- as.factor(kam$supinfec)
kam$orgspcssi <- as.factor(kam$orgspcssi)
kam$oupneumo <- as.factor(kam$oupneumo)
kam$failwean <- as.factor(kam$failwean)
kam$renafail <- as.factor(kam$renafail)
kam$cnscva <- as.factor(kam$cnscva)
kam$cnscva <- as.factor(kam$cnscva)
kam$cdarrest <- as.factor(kam$cdarrest)
kam$cdmi <- as.factor(kam$cdmi)
kam$pulembol <- as.factor(kam$pulembol)
kam$returnor <- as.factor(kam$returnor)
kam$othsysep <- as.factor(kam$othsysep)
kam$reintub <- as.factor(kam$reintub)
kam$othdvt <- as.factor(kam$othdvt)
kam$transfus <- as.factor(kam$transfus)




# format narrow ----------------------------------------------------------------
kam <- kam %>%
  
  mutate(
    
    narrow = ifelse(
      cdmi == "1" | 
        cdarrest == "1" |
        othdvt == "1" | 
        cnscva == "1" | 
        renafail == "1" | 
        dialysis == "1" | 
        oupneumo == "1" | 
        wndinfd == "1" | 
        orgspcssi == "1",
      1, 
      0
    )
    
  ) 




# format broad -----------------------------------------------------------------
kam <- kam %>%
  
  mutate(
    
    broad = ifelse(
      mortality == "1" | 
        wndinfd == "1" |
        supinfec == "1" |
        orgspcssi == "1" |
        oupneumo == "1" | 
        failwean == "1" |
        renafail == "1" |
        cnscva == "1" |
        cnscva == "1" | 
        cdarrest == "1" |
        cdmi == "1" |
        pulembol == "1" |
        returnor == "1" |
        othsysep == "1" |
        reintub == "1" |
        othdvt == "1" |
        transfus == "1"
      ,
      1, 
      0
    )
    
  )




# remove NA --------------------------------------------------------------------
kam <- kam %>%
  
  drop_na(
    
    c(
      
      mg,
      narrow,
      broad,
      age,
      sex,
      bmi,
      diabetes,
      hypermed,
      dialysis,
      prcreat,
      asaclas,
      steroid,
      inout,
      optime,
      tothlos
      
    )
    
  )




# table1------------------------------------------------------------------------
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
    
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


# 
table1(
  
  ~
    age +
    sex +
    bmi +
    diabetes +
    hypermed +
    dialysis +
    prcreat +
    asaclas +
    steroid +
    inout +
    optime +
    tothlos | 
    mg,
  overall = FALSE,
  extra.col = list(`p-value`= pvalue),
  data = kam
  
)


# 
table1(
  
  ~
    as.factor(cpt) | 
    mg,
  data = kam
  
)


# 
table1(
  
  ~
    as.factor(broad) | 
    mg,
  data = kam
  
)


# 
table1(
  
  ~
    as.factor(narrow) | 
    mg,
  data = kam
  
)


# 
table1(
  
  ~
    as.factor(mortality) | 
    mg,
  data = kam
  
)




# iptw -------------------------------------------------------------------------
source("scripts/04b_psm.R")





# fit models -------------------------------------------------------------------
mod_narrow <- glm(
  
  narrow ~
    mg,
  family = "binomial",
  weights = w.out$weights,
  data = kam
  
)

summary(
  
  mod_narrow
  
)


mod_broad <- glm(
  
  broad ~
    mg,
  family = "binomial",
  weights = w.out$weights,
  data = kam
  
)

summary(
  
  mod_broad
  
)




# plot -------------------------------------------------------------------------
critval <- 1.96


# predicted_narrow0 ------------------------------------------------------------
newdata_narrow0 <- data.frame(
  
  mg = "0"
  
)

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
newdata_narrow1 <- data.frame(
  
  mg = "1"
  
)

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
newdata_broad0 <- data.frame(
  
  mg = "0"
  
)

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
newdata_broad1 <- data.frame(
  
  mg = "1"
  
)

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


fit %>%
  
  ggplot(
    
    aes(
      x = outcome, 
      y = fit_response,
      color = mg
    )
    
  ) +
  
  geom_point(
    
    size = 3,
    position = position_dodge(width = 0.5)
    
  ) +
  
  geom_errorbar(
    
    aes(
      ymin = lwr_response,
      ymax = upr_response,
      color = mg
    ),
    width = 0.05,
    alpha = 1,
    position = position_dodge(width = 0.5)
    
  ) +
  
  scale_y_continuous(
    
    labels = scales::percent,
    limits = c(0, 0.3)
    
  ) +
  
  ggtitle("") +
  
  xlab("Composite Outcome") +
  
  ylab("Probability of Outcome") +
  
  labs(color = "Diagnosis") +
  
  scale_color_manual(
    
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
    
    y_position = 0.175, 
    xmin = 0.875,
    xmax = 1.125,
    annotation = "*", 
    tip_length = 0.025,
    color = "black",
    textsize = 8
    
  )  +
  
  geom_signif(
    
    y_position = 0.095, 
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
  
  "out/plot_logit.png",
  height = 4,
  width = 6
  
)
