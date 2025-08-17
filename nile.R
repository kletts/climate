

make_yq <- function(start, nqtrs) { 
  make_yearquarter(start[1], start[2]) + 0:(nqtrs-1)
}

# https://kevinkotze.github.io/ts-4-tut/

# update function `exp` ensures the optimisation produces positive variances
nileBuild <- function(par) {
  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
}

# Fit to the data: Nile
nileMLE <- dlmMLE(Nile, rep(0,2), nileBuild)

# Update the model with the solution to the MLE search
nileMod <- nileBuild(nileMLE$par)

# Generate the fitted filtered and smoothed values from the model
nileFilt <- dlmFilter(Nile, nileMod)
nileSmooth <- dlmSmooth(Nile, nileMod)

# Generate confidence intervals
conf.tmp <- unlist(dlmSvd2var(nileFilt$U.C, nileFilt$D.C))[-1]
conf.tmp <- qnorm(0.05, lower = FALSE) * sqrt(conf.tmp)

#Plot the results
data.frame(Quarter = make_yq(c(1960,2), 100),
           Nile, 
           Filtered=nileFilt$m[-1], 
           Smoothed=nileSmooth$s[-1], 
           ConfInterval = conf.tmp) |> 
  mutate(Smooth.Upper = Smoothed + ConfInterval, 
         Smooth.Lower = Smoothed - ConfInterval) |> 
  ggplot(aes(x=Quarter, y=Nile)) + 
  geom_ribbon(aes(ymin=Smooth.Lower, ymax=Smooth.Upper), alpha=0.7, fill="lightblue") + 
  geom_line(col="gray") + 
  geom_line(aes(y=Smoothed), lwd=1.5) + 
  theme_bw() 
  
