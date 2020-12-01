library(Pmisc)
library(kableExtra)
library(data.table)
library(Hmisc)
download.file("http://pbrown.ca/teaching/appliedstats/data/drugs.rds",
              "drugs.rds")
xSub = readRDS("drugs.rds")
table(xSub$SUB1)
table(xSub$STFIPS)[1:5]
table(xSub$TOWN)[1:2]

forInla = na.omit(xSub)
forInla$y = as.numeric(forInla$completed)

library("INLA")
ires = inla(y ~ SUB1 + GENDER + raceEthnicity +AGE+ homeless +f(STFIPS, hyper=list(prec=list(
  prior='pc.prec', param=c(0.8, 0.05)))) +
    f(TOWN,hyper=list(prec=list(prior='pc.prec', param=c(0.8, 0.05)))),
  data=forInla, family='binomial',
  control.inla = list(strategy='gaussian', int.strategy='eb'))

summary(ires)

knitr::kable(ires$summary.hyperpar, digits=2)
ires$summary.hyperpar

library(brinla)
bri.hyperpar.summary(ires)

sdState = Pmisc::priorPostSd(ires)
do.call(matplot, sdState$STFIPS$matplot)
do.call(legend, sdState$legend)

sdState2 = Pmisc::priorPostSd(ires)
do.call(matplot, sdState2$TOWN$matplot)
do.call(legend, sdState2$legend)


toPrint = as.data.frame(rbind(exp(ires$summary.fixed[,
                                                     c(4, 3, 5)]), sdState$summary[, c(4, 3, 5)]))
sss = "^(raceEthnicity|SUB1|GENDER|homeless|SD)(.[[:digit:]]+.[[:space:]]+| for )?"
toPrint = cbind(variable = gsub(paste0(sss, ".*"),
                                "\\1", rownames(toPrint)), category = substr(gsub(sss,
                                                                                  "", rownames(toPrint)), 1, 25), toPrint)
Pmisc::mdTable(toPrint, digits = 3, mdToTex = TRUE,
               guessGroup = TRUE, caption = "Posterior means and quantiles for model parameters.")


ires$summary.random$STFIPS$ID = gsub("[[:punct:]]|[[:digit:]]",
                                     "", ires$summary.random$STFIPS$ID)
ires$summary.random$STFIPS$ID = gsub("DISTRICT OF COLUMBIA",
                                     "WASHINGTON DC", ires$summary.random$STFIPS$ID)
toprint = cbind(ires$summary.random$STFIPS[1:26, c(1,
                                                   2, 4, 6)], ires$summary.random$STFIPS[-(1:26),
                                                                                         c(1, 2, 4, 6)])
colnames(toprint) = gsub("uant", "", colnames(toprint))
knitr::kable(toprint, digits = 1,format="latex")

