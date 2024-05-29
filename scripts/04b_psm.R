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
    optime +
    tothlos, 
  data = kam, 
  estimand = "ATT", 
  focal = "1"
  
  )

summary(
  
  w.out
  
  )




# assess balance ---------------------------------------------------------------
# table
bal.tab(w.out)


# love plot
love.plot(
  
  w.out, 
  stats = c("mean.diffs", "variance.ratios"),
  thresholds = c(m = .1, v = 2), 
  abs = TRUE, 
  binary = "std",
  var.order = "unadjusted"
  
  )


new.names <- c(
  
  age = "Age (Years)",
  sex_Male = "Sex (Female/Male)",
  bmi = "BMI",
  diabetes = "Diabetes (Y/N)",
  hypermed = "Hypertension (Y/N)",
  dialysis = "Dialysis (Y/N)",
  prcreat = "Creatinine",
  asaclas_1 = "ASA: 1",
  asaclas_2 = "ASA: 2",
  asaclas_3 = "ASA: 3",
  asaclas_4 = "ASA: 4",
  asaclas_5 = "ASA: 5",
  steroid = "Steroid (Y/N)",
  inout = "Inpatient/Outpatient",
  optime = "Operation Time (Minutes)",
  tothlos = "LOS (Days)"
  
  )


love.plot(
  
  w.out, 
  drop.distance = TRUE, 
  var.order = "unadjusted",
  abs = TRUE,
  line = TRUE, 
  thresholds = c(m = .1),
  var.names = new.names,
  colors = c("#F8766D", "#00BFC4"),
  stars = "raw",
  sample.names = c("Unweighted", "Weighted"),
  limits = c(0, 3),
  position = c(.70, .25)
  
  ) +
  
  theme(
    
    legend.box.background = element_rect(), 
    legend.box.margin = margin(1, 1, 1, 1),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    aspect.ratio = 1
    
    )

# balance plots
bal.plot(w.out, var.name = "age")
bal.plot(w.out, var.name = "sex")
bal.plot(w.out, var.name = "bmi")
bal.plot(w.out, var.name = "diabetes")
bal.plot(w.out, var.name = "hypermed")
bal.plot(w.out, var.name = "dialysis")
bal.plot(w.out, var.name = "prcreat")
bal.plot(w.out, var.name = "asaclas")
bal.plot(w.out, var.name = "steroid")
bal.plot(w.out, var.name = "inout")
bal.plot(w.out, var.name = "optime")
bal.plot(w.out, var.name = "tothlos")
