data('fruitfly', package='faraway')
summary(fruitfly)

#normalize thorax
mth = mean(fruitfly$thorax)
sd = sd(fruitfly$thorax)                               

norm_thorax = (fruitfly$thorax-mth)/sd

mod_fruitfly = glm(fruitfly$longevity ~ norm_thorax + fruitfly$activity, family = Gamma(link=log))
summary(mod_fruitfly)
knitr::kable(rbind(summary(mod_fruitfly)$coef), digits=3)

# historgram
shape = 1/summary(mod_fruitfly)$dispersion

intercept_est=mod_fruitfly$coefficients[1]

scale = exp(intercept_est)/shape

hist(fruitfly$longevity,prob=TRUE,ylim=c(0,0.04))
xSeq=seq(0,120,len=1000)
lines(xSeq,dgamma(xSeq,shape = shape,scale = scale),col="red")

