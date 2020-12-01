smokeFile = Pmisc::downloadIfOld("http://pbrown.ca/teaching/appliedstats/data/smoke.RData")
load(smokeFile)
smoke = smoke[smoke$Age > 9, ]
forInla = smoke[, c("Age", "Age_first_tried_cigt_smkg",
                    "Sex", "Race", "state", "school", "RuralUrban")]
forInla = na.omit(forInla)
forInla$school = factor(forInla$school)
library("INLA")
forSurv = data.frame(time = (pmin(forInla$Age_first_tried_cigt_smkg,
                                  forInla$Age) - 4)/10, event = forInla$Age_first_tried_cigt_smkg <=
                       forInla$Age)
# left censoring
forSurv[forInla$Age_first_tried_cigt_smkg == 8, "event"] = 2
smokeResponse = inla.surv(forSurv$time, forSurv$event)


fitS2 = inla(smokeResponse ~ RuralUrban + Sex * Race +
               f(school, model = "iid", hyper = list(prec = list(prior = "pc.prec",
                param = c(0.2,0.02)))) +
               f(state, model = "iid",
                 hyper = list(prec = list(prior = "pc.prec", param =c(1, 0.05)))),
             control.family = list(variant = 1,hyper = list(alpha = list(prior = "normal", param = c(log(1), (2/3)^(-2))))),
             control.mode = list(theta = c(8,2, 5), restart = TRUE), data = forInla, family = "weibullsurv",
             verbose = TRUE)

rbind(fitS2$summary.fixed[, c("mean", "0.025quant",
"0.975quant")], Pmisc::priorPostSd(fitS2)$summary[,c("mean", "0.025quant", "0.975quant")])

summary(fitS2)
sigma=1
exp(c(-2,2)*sigma)


library(brinla)
bri.hyperpar.summary(fitS2)

#exp(qnorm(c(0.025,0.5,0.975),mean=log(1),sd=2/3))




# posterior
sdState = Pmisc::priorPostSd(fitS2)
do.call(matplot, sdState$state$matplot)
do.call(legend, sdState$legend)

sdschool = Pmisc::priorPostSd(fitS2)
do.call(matplot, sdschool$school$matplot)
do.call(legend, sdschool$legend)


fitS2$priorPost = Pmisc::priorPost(fitS2)
for (Dparam in fitS2$priorPost$parameters) {do.call(matplot, fitS2$priorPost[[Dparam]]$matplot)}
fitS2$priorPost$legend$x = "topleft"
do.call(legend, fitS2$priorPost$legend)

library("survival")
#forSurv
forSurv$one=1
xSeq=seq(5,100,len=1000)
kappa = fitS2$summary.hyper['alpha', 'mode']
lambda = exp(-fitS2$summary.fixed['(Intercept)', 'mode'])
plot(xSeq, (xSeq / (100*lambda))^kappa, col='blue', type='l', log='y',
     ylim=c(0.001, 10),xlim = c(20,100), xlab='years', ylab = 'cum haz')

hazEst = survfit(Surv(time, forSurv$one) ~ 1, data=forSurv)
plot(hazEst, fun='cumhaz')

hist(forInla$Age_first_tried_cigt_smkg,main = "",prob=TRUE)



