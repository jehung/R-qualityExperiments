#### TRY THE WEIRD STUFF #########


library(xlsx)
library(plyr)
library(ggplot2)
library(bbmle)

quality = read.xlsx("QualityExperiment.xlsx", header = TRUE, sheetName = "summary", stringsAsFactors = FALSE)
quality

# set up factored variable
quality$var.F = factor(quality$var)
quality

# summary statistics
sum.func = function(data, var, alpha = 0.05){
  #browser()
  values = data[,var]
  n.nonmiss = length(na.omit(values))
  mean = mean(values, na.rm = TRUE)
  trim.mean = mean(values, trim = 0.05, na.rm = TRUE)
  se = sd(values, na.rm = TRUE)/sqrt(n.nonmiss)
  lcl = trim.mean - qt(1-alpha/2, n.nonmiss-1) * se
  ucl = trim.mean + qt(1-alpha/2, n.nonmiss-1) * se
  return(data.frame(n.nonmiss, mean, trim.mean, se, lcl, ucl))
}

sum.quality = ddply(quality, "var", sum.func, var = "n_day_interval")
sum.quality

# plot data
plot.sum = ggplot(sum.quality, aes(x = var, y = trim.mean)) +
           ggtitle("Data exploration") +
           xlab("Name") + ylab("Mean and 95% confidence interval") +
           geom_point() + geom_line(aes(group = 1)) + geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2)
plot.sum




# write a function to determine necessary sample size at required se
# using object-oriented approach of modularization

# first write a function to calcualte mean/trimmed mean of a subset 



subset.func = function(data, ind){
  newdata = data[ind,]
  return(newdata)
}

sample.func = function(size, data){
  index = sample(1:nrow(data), size, replace = TRUE)
  result = subset.func(data, index)
  return(result)
}

sum.func = function(data, var, alpha = 0.05){
  #browser()
  values = data[,var]
  n.nonmiss = length(na.omit(values))
  mean = mean(values, na.rm = TRUE)
  trim.mean = mean(values, trim = 0.05, na.rm = TRUE)
  se = sd(values, na.rm = TRUE)/sqrt(n.nonmiss)
  lcl = trim.mean - qt(1-alpha/2, n.nonmiss-1) * se
  ucl = trim.mean + qt(1-alpha/2, n.nonmiss-1) * se
  return(data.frame(n.nonmiss, mean, trim.mean, se, lcl, ucl))
}

a = sample.func(50, quality)

sum.quality = ddply(a, "var", sum.func, "n_day_interval")
sum.quality




se.func = function(size, data){
  res = rdply(10, sample.func(size = size, data), .id = "rep")
  sd = sd(res$n_day_interval, na.rm = TRUE)
  n = length(res$n)
  c(size = size, sd)
}

se.func(10, quality)



mean.func = function(data, ind){
  newdata = data[ind,]
  n = length(ind)
  mean = mean(newdata$n_day_interval, na.rm = TRUE)
  trim.mean = mean(newdata$n_day_interval, trim = 0.05, na.rm = TRUE)
  se = sd(newdata$n_day_interval, na.rm = TRUE)/sqrt(n)
  return(data.frame(n,trim.mean, mean, se))
}

# the write a function to re-sample with replacement from current sample
sample.func = function(data, size){
  #browser()
  index = sample(1:nrow(data), size, replace = TRUE)
  result = mean.func(data, index)
  return(result)
}

sample.func(quality, 15)

# use rdply()
rep = rdply(10, sample.func(quality, 15), .id = "rep")
rep

################################################################################################################
# MLE method
################################################################################################################
quality.censor = read.xlsx("QualityExperiment.xlsx", header = TRUE, sheetName = "MLE", stringsAsFactors = FALSE)
quality.censor

n = dim(quality.censor)[1]
quality.censor$censor = c(rep(0, n-2), 1, 1)
quality.censor







call.log.lik = function(p, data, return.negll = FALSE){
  # compute the log-likelihood of getting a call in a day
  # if return.negll is set to TRUE, returns the negative of log-likelihood
  #browser()
  ll = sum( (data$day.prior*log(1-p)+ log(p))*(data$censor==0)) + sum(data$day.prior*log(1-p)*(data$censor==1))
  if (return.negll){ll = -1 *ll}
  return(data.frame(p, ll))
}

call.log.lik(0.2, quality.censor)
call.log.lik(0.2, quality.censor, return.negll = TRUE)

# compute lok-likelihood by step
middlestep = ldply(seq(0, 0.99, 0.01), call.log.lik, data = quality.censor, .id = "probabiltiy")
middlestep

# plot the middlestep
plot.middlestep = ggplot(middlestep, aes(x = p, y = ll)) +
                  ggtitle("Maximum log likelohood of p") +
                  xlab("probability") + ylab("Max log likelihood") +
                  geom_point() + geom_line()
plot.middlestep

# optimize (maximize()) the log-likelihood function
# first I modify my function so that it returns a scalar, not a data frame or a vector
# nlm() requires a scalar function
call.log.lik = function(p, data, return.negll = FALSE){
  # compute the log-likelihood of becoming pregnant in a month
  # if return.negll is set to TRUE, returns the negative of log-likelihood
  ll = sum( (data$day.prior*log(1-p)+ log(p))*(data$censor==0)) + sum(data$day.prior*log(1-p)*(data$censor==1))
  if (return.negll){ll = -1 *ll}
  return(ll)
}

mle = nlm(call.log.lik, p = 0.5, data = quality.censor, return.negll = TRUE, hessian = TRUE)
mle

mle$estimate
mle$minimum

# profile MLE


profile.mle = mle2(call.log.lik, list(p = 0.4), data = list(data = quality.censor, return.negll = TRUE))
profile.mle


mle2 = coef(profile.mle)
vcov(profile.mle)
CI.profile = confint(profile.mle)
CI.profile

########################################################################
# Simulation of censored data
########################################################################
quality.censor = read.xlsx("QualityExperiment.xlsx", header = TRUE, sheetName = "MLE", stringsAsFactors = FALSE)
quality.censor

n = dim(quality.censor)[1]
quality.censor$censor = c(rep(0, n-2), 1, 1)
quality.censor






call.log.lik = function(censor.day, p, data, return.negll = FALSE){
  #browser()
  # compute the log-likelihood of getting a call in a day
  # if return.negll is set to TRUE, returns the negative of log-likelihood
  #browser()
  ll = sum( (data$day.prior*log(1-p)+ log(p))*(data$censor==0)) + sum(censor.day*log(1-p)*(data$censor==1))
  if (return.negll){ll = -1 *ll}
  return(ll)
}

call.log.lik(8, 0.1, quality.censor, return.negll = TRUE)
nlm(call.log.lik, censor.day = 8, p = 0.5, data = quality.censor, return.negll = TRUE)


for (i in (1:60)){
  asymp.mle = nlm(call.log.lik, censor.day = i, p = 0.5, data = quality.censor, return.negll = TRUE)$estimate
  anss = mle2(call.log.lik, list(p = 0.5), data = list(censor.day = i, data = quality.censor, return.negll = TRUE))
  profile.mle = coef(anss)
  profile.ci = confint(anss)
  report = data.frame(censor.day = i, asymp.mle, profile.mle, profile.ci[1], profile.ci[2])
  print(report)
}

##############################################################################
# compare MLE from different groups
##############################################################################
quality.censor = read.xlsx("QualityExperiment.xlsx", header = TRUE, sheetName = "MLE", stringsAsFactors = FALSE)
quality.censor

n = dim(quality.censor)[1]
quality.censor$censor = c(rep(0, n-2), 1, 1)
quality.censor


quality.censor.pat = subset(quality.censor, var == "Pat")
quality.censor.annie = subset(quality.censor, var == "Annie")

call.log.lik = function(censor.day, p, data, return.negll = FALSE){
  #browser()
  # compute the log-likelihood of getting a call in a day
  # if return.negll is set to TRUE, returns the negative of log-likelihood
  #browser()
  ll = sum( (data$day.prior*log(1-p)+ log(p))*(data$censor==0)) + sum(censor.day*log(1-p)*(data$censor==1))
  if (return.negll){ll = -1 *ll}
  return(ll)
}


f1 = function(n){
  report.pat = data.frame(NULL)
  for (i in (1:n)){
    asymp.mle = nlm(call.log.lik, censor.day = i, p = 0.8, data = quality.censor.pat, return.negll = TRUE)$estimate
    anss = mle2(call.log.lik, list(p = 0.8), data = list(censor.day = i, data = quality.censor.pat, return.negll = TRUE))
    profile.mle = coef(anss)
    profile.ci = confint(anss, level = 0.95)
    report.pat = rbind(report.pat, data.frame(var = "pat", censor.day = i, asymp.mle, profile.mle, profile.ci[1], profile.ci[2]))
    print(report)
  }
  return(report.pat)
}
p = f1(60)

plot.pat = ggplot(p, aes(x=censor.day, y = profile.mle)) +
           ggtitle("MLE in relation to censor day number") +
           xlab("censor day number") + ylab("MLE(p) and 95% profile confidence interval") +
           geom_point() + geom_errorbar(aes(ymin = profile.ci.1., ymax = profile.ci.2.))
plot.pat


f2 = function(n){
  report.annie = data.frame(NULL)
  for (i in (1:n)){
    asymp.mle = nlm(call.log.lik, censor.day = i, p = 0.5, data = quality.censor.annie, return.negll = TRUE)$estimate
    anss = mle2(call.log.lik, list(p = 0.5), data = list(censor.day = i, data = quality.censor.annie, return.negll = TRUE))
    profile.mle = coef(anss)
    profile.ci = confint(anss, level = 0.80)
    report.annie = rbind(report.annie, data.frame(var = "annie", censor.day = i, asymp.mle, profile.mle, profile.ci[1], profile.ci[2]))
    print(report.annie)
  }
  return(report.annie)
}

a = f2(60)





plot.annie = ggplot(a, aes(x = censor.day, y = profile.mle)) +
             ggtitle("MLE in relation to censor day number") +
             xlab("censor day number") + ylab("MLE(p) with 95% profile confidence interval") +
             geom_point() + geom_errorbar(aes(ymin = profile.ci.1., ymax = profile.ci.2.)) +
             geom_hline(aes(yintercept = 0.09), color = "red", linetype = "dashed") +
             geom_hline(aes(yintercept = 0.30), color = "red", linetype = "dashed") +
             geom_hline(aes(yintercept = 0.20), color = "red", lyinetype = "solid")
plot.annie


combined = rbind(p, a)
combined[1:100,]

plot.combined = ggplot(combined, aes(x = censor.day, y = profile.mle, group = var, color = var, shape = var)) +
                xlab("cendor day number") + ylab("MLE(pi) with 95% profile confidence interval") +
                geom_point(position = position_jitter(width = 0.1, height = 0)) + geom_errorbar(position = position_jitter(w = 0.1, h = 0), aes(ymin = profile.ci.1., ymax = profile.ci.2.))
plot.combined