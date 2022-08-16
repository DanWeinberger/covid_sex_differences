
pred_interval <-function(mod,model.type='poisson',ds.glm=ds1, seedN=123,stage1.samples=10000, stage2.samples=1){
  coef1 <- coef(mod)
  coef1[is.na(coef1)] <- 0

v.cov.mat <- vcov(mod)
v.cov.mat[is.na(v.cov.mat)] <- 0

disp <- mod$deviance/mod$df.residual

set.seed (seedN)
pred.coefs.reg.mean <-
  MASS::mvrnorm(n = stage1.samples,
                mu = coef1,
                Sigma = v.cov.mat)
if(model.type=='poisson'){
form2 <- update(mod$formula, 'N_deaths~. ')
}else{
form2 <- as.formula(paste0( 'N_deaths~'  ,mod$terms[[3]] ))
}
mod.mat.pred <- model.matrix(form2, data = ds.glm, family = "poisson")

preds.stage1.regmean <- mod.mat.pred %*% t(pred.coefs.reg.mean)
preds.stage1.regmean <- apply(preds.stage1.regmean, 2,
                              function(x) x + ds.glm$log_offset)

preds.stage1.regmean[preds.stage1.regmean >100] <- 100

if(model.type=='poisson'){
  preds.stage2 <- rpois(n = length(preds.stage1.regmean) * stage2.samples,
                        exp(preds.stage1.regmean))
}else{
  preds.stage2 <- rnbinom(n = length(preds.stage1.regmean) * stage2.samples,
                          size = mod$theta, mu = exp(preds.stage1.regmean))
}
preds.stage2 <- matrix(preds.stage2,
                       nrow = nrow(preds.stage1.regmean),
                       ncol = ncol(preds.stage1.regmean) * stage2.samples)

preds.stage2.q <-
  t(apply(preds.stage2, 1,
          quantile,
          probs = c(0.025, 0.5, 0.975)))

preds.stage2.q <- as.data.frame(preds.stage2.q)
names(preds.stage2.q) <- c('pred_lcl','pred_median','pred_ucl')
return(preds.stage2.q)
}