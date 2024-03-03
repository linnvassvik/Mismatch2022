
make_prediction <- function(Newdata, model){
  Newdata$pred <- predict(model, Newdata, level = 0)
  Designmat <- model.matrix(formula(model)[-2], Newdata)
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  Newdata$SE <- sqrt(predvar) 
  return(Newdata)
}


make_prettyplot <- function(dat, xaxis, yaxis, Newdata, prediction, ColorVariable = NULL, ColorDot = NULL, SE, line_type = NULL, shape = NULL){
  cmult = 1.96 #make CI
  ggplot(dat, aes(x = {{xaxis}} , y = {{yaxis}}))+ 
    geom_point(aes(color = {{ColorDot}}), alpha = 0.5) +
    geom_line(Newdata, mapping = aes(x = {{xaxis}}, y = {{prediction}}, color = {{ColorVariable}}, shape = {{shape}}), linetype = line_type) +
    geom_ribbon(Newdata, mapping = aes(y= {{prediction}}, ymin = {{prediction}} - cmult * {{SE}}, ymax = {{prediction}} + cmult * {{SE}}, fill = {{ColorVariable}}), alpha = 0.5) +
    theme_minimal()
}



make_glmer_prediction <- function(Newdata, model){
  Newdata$pred <- predict(model,Newdata, re.form=NA)
  mm <- model.matrix(terms(model),Newdata)
  pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
  tvar1 <- pvar1+VarCorr(model)$siteID[1] 
  cmult <- 1.96 ## make CI
  Newdata <- data.frame(
    Newdata
    , plo = Newdata$pred-cmult*sqrt(pvar1)
    , phi = Newdata$pred+cmult*sqrt(pvar1)
    , tlo = Newdata$pred-cmult*sqrt(tvar1)
    , thi = Newdata$pred+cmult*sqrt(tvar1)
  )  
}



make_glmer_plot <- function(dat, xaxis, yaxis, Newdata, prediction, ColorVariable = NULL, ColorDot = NULL, plo, phi){
  ggplot(dat, aes(x = {{xaxis}} , y = {{yaxis}}))+ 
    geom_point(aes(color = {{ColorDot}}), alpha = 0.5) +
    geom_line(Newdata, mapping = aes(x = {{xaxis}}, y = plogis({{prediction}}), color = {{ColorVariable}})) +
    geom_ribbon(Newdata, mapping = aes(y= {{prediction}},ymin = plogis({{plo}}), ymax = plogis({{phi}}), fill = {{ColorVariable}}), alpha = 0.5) +
    theme_minimal()
}
