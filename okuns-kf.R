

dur <- data$dur
mod <- dlmModPoly(order=1, dV=1, dW=var(dur)*0.1)

fn <- function(parm) { 
  mod <- dlmModPoly(order = 1) + 
    dlmModReg(cbind(1, data$d2lgdp, data$l2d2lrulc), addInt = FALSE, 
              m0=c(0.51339, -0.12713, 0.05504))   
  V(mod) <- exp(parm[1])
  diag(W(mod)) <- exp(c(parm[2:3], 0, 0)) 
  return(mod) } 

parm <- unname(c(var(lm(dur ~ d2lgdp + l2d2lrulc, data)$residuals), 
                 var(diff(dur)), 
                 summary(lm(dur ~ d2lgdp + l2d2lrulc, data))$coefficients[,"Std. Error"]^2))
fit <- dlmMLE(data$dur, parm=parm, build = fn, hessian = TRUE)

#param ckf(2) -0.0495 ckf(3) 0.01942 ckf(4) -2.8913 ckf(5) -4.1757 ckf(6) -6.2466	

data.frame(m = dropFirst(dlmFilter(data$dur, fn(fit$par))$m)) |>
  bind_cols(data) |> 
  mutate(mu = -m.2/m.3[length(m.3)]) |> 
  #filter(date > yearquarter("1980 Q1")) |> 
  ggplot(aes(x=date, y=mu)) + 
  geom_line() + 
  geom_line(aes(y=ur), col="red")

# ---- latent trend fixed beta 

fn <- function(parm) { 
  mod <- dlmModReg(data$ggdp4, addInt = FALSE)   
  V(mod) <- exp(parm[1])
  diag(W(mod)) <- exp(c(parm[2:3], 0, 0)) 
  return(mod) } 




