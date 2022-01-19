round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

percentage.table <- function(x, digits = 1){
  tab <- table(x)
  percentage.tab <- 100*tab/(sum(tab))
  rounded.tab <- round(x = percentage.tab, digits = digits)
  return(rounded.tab)
}

engagement.model <- function(dt, outcome.name, input.names, model.type){
  res <- fit.model(dt = dt, outcome.name = outcome.name, input.names = input.names, model.type = model.type)
  return(res)
}

fit.model <- function(dt, outcome.name, input.names, model.type, digits = 3){
  library(formulaic)
  the.formula <- create.formula(outcome.name = outcome.name, input.names = input.names, dat = dt, reduce = T)
  
  if(model.type == "logistic"){
    mod <- glm(formula = the.formula, family = "binomial", data = dt)
    mod.summary <- logistic.regression.summary(glm.mod = mod, digits = digits)
  }
  if(model.type == "linear"){
    mod <- lm(formula = the.formula, data = dt)
    mod.summary <- linear.regression.summary(lm.mod = mod, digits = digits)
  }
  mod.summary.rounded <- mod.summary[, lapply(X = .SD, FUN = "round.numerics", digits = digits)]
  return(mod.summary.rounded)
}

logistic.regression.summary <- function(glm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  glm.coefs <- as.data.table(summary(glm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = glm.coefs, old = "rn", new = "Variable")
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  
  return(glm.coefs[])
}


linear.regression.summary <- function(lm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  lm.coefs <- as.data.table(summary(lm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = lm.coefs, old = "rn", new = "Variable")
  
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(lm.coefs)
}


###############
topx_engagement = function(respondent_variable, respondent_variable_sub, state_of_engagement, rank_length = 3){ 
  
  data[eval(as.name(respondent_variable)) == respondent_variable_sub, .("Awareness_Rate" = round.numerics(mean(get(state_of_engagement), na.rm = TRUE),2)), by = Product][order(-rank(Awareness_Rate))][1:rank_length]
}

topx_brand_perception = function(rank_length = 3){
  
  data[, lapply(X = .SD, FUN = "mean", na.rm = TRUE), .SDcols = bp_traits, by = Product][, .("Overall_Brand_Perception" = round.numerics(rowMeans(.SD),2)), .SDcols = bp_traits, by = Product][order(-rank(Overall_Brand_Perception))][1:rank_length]
}

topx_gap = function(outcome1, outcome2, rank_length = 3){
  data[, .("Gap_Between_Rates" = round.numerics(mean(get(outcome1), na.rm = TRUE) - mean(get(outcome2), na.rm = TRUE),2)), by = Product][order(-rank(Gap_Between_Rates))][1:rank_length]
}

log_regression = function(data, product_type, engagement, alpha, digits){
  library(knitr)
  
  baseline = data[, .SD, .SDcols = -c(8:24)][, .SD[1], by = id]
  a = data[Product != product_type][, .("Aggregated.Engagement" = mean(get(engagement), na.rm = TRUE)), by = id]
  b = data[Product == product_type, get(engagement), by = id]
  c = Reduce(merge, list(a,b,baseline))
  
  model1 = glm(V1 ~ Age_Group+Gender+Income_Group+Region+Persona+Aggregated.Engagement, family = "binomial", data = c)
  
  glm_coefs = as.data.table(summary(model1)$coefficients, keep.rownames = TRUE)
  setnames(x = glm_coefs, old = "rn", new = "Variable")
  setnames(x = glm_coefs, old = "Pr(>|z|)", new = "P-Value")
  z = qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm_coefs[, Odds.Ratio := exp(Estimate)]
  glm_coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm_coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  
  return(kable(glm_coefs[, c("Variable", "P-Value", "Odds.Ratio", "OR.Lower.95", "OR.Upper.95")]))
}

lin_regression = function(data, product_type, engagement, alpha, digits){
  library(knitr)
  
  baseline = data[, .SD, .SDcols = -c(8:24)][, .SD[1], by = id]
  a = data[, .("Aggregated.Engagement" = mean(get(engagement), na.rm = TRUE)), by = id]
  b = data[Product == product_type, get(engagement), by = id]
  c = Reduce(merge, list(a,b,baseline))
  
  model1 = lm(V1 ~ Age_Group+Gender+Income_Group+Region+Persona+Aggregated.Engagement, data = c)
  
  lm_coefs = as.data.table(summary(model1)$coefficients, keep.rownames = TRUE)
  setnames(x = lm_coefs, old = "rn", new = "Variable")
  setnames(x = lm_coefs, old = "Pr(>|t|)", new = "P-Value")
  z = qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm_coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm_coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  
  return(kable(lm_coefs[, c("Variable", "Estimate", "P-Value", "Coef.Lower.95", "Coef.Upper.95")]))
}


# PP Functions


numerize_table <- function(data, limit = 10) {
  for (col in colnames(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  return(data)
}

factorize_table <- function(data, limit = 30) {
  for (col in colnames(data)) {
    if ((length(unique(data[[col]])) <= limit) &
        (class(data[[col]]) == 'character')) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  return(data)
}
asdate_col <- function(datacol, format = "%m/%d/%Y"){
  return(as.Date(datacol, format = format))
}
