

# libraries --------------------------------------------------------------------
library(WeightIt)
library(cobalt)


# inverse probability of treatment weighting -----------------------------------
w.out <- weightit(
  
  mg ~ 
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
    fnstatus2 +
    hxcopd +
    hxchf
  ,
  data = nsqip_logistic, 
  estimand = "ATT", 
  focal = "1"
  
)

summary(w.out)


# assess balance ---------------------------------------------------------------
bal.tab(w.out)

love.plot(
  
  w.out, 
  drop.distance = TRUE, 
  var.order = "unadjusted",
  abs = TRUE,
  line = TRUE, 
  thresholds = c(m = .25),
  colors = c("#F8766D", "#00BFC4"),
  stars = "raw",
  sample.names = c("Unweighted", "Weighted"),
  limits = c(0, 3),
  position = c(.70, .25)
  
) +
  
  theme(
    
    legend.box.background = element_rect(), 
    legend.box.margin = margin(1, 1, 1, 1),
    plot.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    aspect.ratio = 1
    
  )
