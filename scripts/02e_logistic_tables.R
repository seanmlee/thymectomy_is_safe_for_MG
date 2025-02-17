

# libraries --------------------------------------------------------------------
library(tidyverse)
library(sjPlot)
library(table1)


# model summaries --------------------------------------------------------------
tab_model(mod_broad)
tab_model(mod_narrow)


# pvalue function --------------------------------------------------------------
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


# descriptive for logistic model -----------------------------------------------
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
    fnstatus2 +
    hxcopd +
    hxchf | 
    mg,
  overall = FALSE,
  extra.col = list(`p-value`= pvalue),
  data = nsqip_logistic
  
)


# cpt --------------------------------------------------------------------------
table1(
  
  ~
    as.factor(cpt) | 
    mg,
  data = nsqip_logistic
  
)


# broad ------------------------------------------------------------------------
table1(
  
  ~
    as.factor(broad) | 
    mg,
  data = nsqip_logistic
  
)


# narrow -----------------------------------------------------------------------
table1(
  
  ~
    as.factor(narrow) | 
    mg,
  data = nsqip_logistic
  
)


# mortality --------------------------------------------------------------------
table1(
  
  ~
    as.factor(dopertod) | 
    mg,
  data = nsqip_logistic
  
)

