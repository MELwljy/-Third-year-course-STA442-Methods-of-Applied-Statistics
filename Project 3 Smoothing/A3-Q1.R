cUrl = paste0("http://scrippsco2.ucsd.edu/assets/data/atmospheric/",
              "stations/flask_co2/daily/daily_flask_co2_mlo.csv")
cFile = basename(cUrl)
if (!file.exists(cFile)) download.file(cUrl, cFile)
co2s = read.table(cFile, header = FALSE, sep = ",",
                  skip = 69, stringsAsFactors = FALSE, col.names = c("day",
                                                                     "time", "junk1", "junk2", "Nflasks", "quality",
                                                                     "co2"))
co2s$date = strptime(paste(co2s$day, co2s$time), format = "%Y-%m-%d %H:%M",
                     tz = "UTC")
co2s$date
# remove low-quality measurements
co2s[co2s$quality >= 1, "co2"] = NA
plot(co2s$date, co2s$co2, log = "y", cex = 0.3, col = "#00000040",
     xlab = "time", ylab = "ppm")

plot(co2s[co2s$date > ISOdate(2015, 3, 1, tz = "UTC"),
          c("date", "co2")], log = "y", type = "o", xlab = "time",
     ylab = "ppm", cex = 0.5)

timeOrigin = ISOdate(1980, 1, 1, 0, 0, 0, tz = "UTC")
co2s$days = as.numeric(difftime(co2s$date, timeOrigin,
                                units = "days"))
co2s$days

co2s$cos12 = cos(2 * pi * co2s$days/365.25)
co2s$sin12 = sin(2 * pi * co2s$days/365.25)
co2s$cos6 = cos(2 * 2 * pi * co2s$days/365.25)
co2s$sin6 = sin(2 * 2 * pi * co2s$days/365.25)
cLm = lm(co2 ~ days + cos12 + sin12 + cos6 + sin6,
         data = co2s)

summary(cLm)$coef[, 1:2]


newX = data.frame(date = seq(ISOdate(1990, 1, 1, 0,
                                     0, 0, tz = "UTC"), by = "1 days", length.out = 365 *
                               30))
newX$days = as.numeric(difftime(newX$date, timeOrigin,
                                units = "days"))
newX$cos12 = cos(2 * pi * newX$days/365.25)
newX$sin12 = sin(2 * pi * newX$days/365.25)
newX$cos6 = cos(2 * 2 * pi * newX$days/365.25)
newX$sin6 = sin(2 * 2 * pi * newX$days/365.25)

coPred = predict(cLm, newX, se.fit = TRUE)
coPred = data.frame(est = coPred$fit, lower = coPred$fit -
                      2 * coPred$se.fit, upper = coPred$fit + 2 * coPred$se.fit)
plot(newX$date, coPred$est, type = "l")
matlines(as.numeric(newX$date), coPred[, c("lower",
                                           "upper", "est")], lty = 1, col = c("yellow", "yellow",
                                                                              "black"))
newX = newX[1:365, ]
newX$days = 0
plot(newX$date, predict(cLm, newX))

library("INLA")
# time random effect
timeBreaks = seq(min(co2s$date), ISOdate(2025, 1, 1,
                                         tz = "UTC"), by = "14 days")
timeBreaks
timePoints = timeBreaks[-1]
timePoints
co2s$timeRw2 = as.numeric(cut(co2s$date, timeBreaks))
co2s$timeRw2

# derivatives of time random effect
D = Diagonal(length(timePoints)) - bandSparse(length(timePoints),
                                              k = -1)
derivLincomb = inla.make.lincombs(timeRw2 = D[-1, ])
names(derivLincomb) = gsub("^lc", "time", names(derivLincomb))

# seasonal effect
StimeSeason = seq(ISOdate(2009, 9, 1, tz = "UTC"),
                  ISOdate(2011, 3, 1, tz = "UTC"), len = 1001)
StimeYear = as.numeric(difftime(StimeSeason, timeOrigin,
                                "days"))/365.35
seasonLincomb = inla.make.lincombs(sin12 = sin(2 *
                                                 pi * StimeYear), cos12 = cos(2 * pi * StimeYear),
                                   sin6 = sin(2 * 2 * pi * StimeYear), cos6 = cos(2 *
                                                                                    2 * pi * StimeYear))
names(seasonLincomb) = gsub("^lc", "season", names(seasonLincomb))

# predictions
StimePred = as.numeric(difftime(timePoints, timeOrigin,
                                units = "days"))/365.35

predLincomb = inla.make.lincombs(timeRw2 = Diagonal(length(timePoints)),
                                 `(Intercept)` = rep(1, length(timePoints)), sin12 = sin(2 *pi * StimePred), cos12 = cos(2 * pi * StimePred),
                                 sin6 = sin(2 * 2 * pi * StimePred), cos6 = cos(2 * 2 * pi * StimePred))
                                                                                           
names(predLincomb) = gsub("^lc", "pred", names(predLincomb))
StimeIndex = seq(1, length(timePoints))
timeOriginIndex = which.min(abs(difftime(timePoints, timeOrigin)))

library("INLA")
mm = get("inla.models", INLA:::inla.get.inlaEnv())
if(class(mm) == 'function') mm = mm()
mm$latent$rw2$min.diff = NULL
assign("inla.models", mm, INLA:::inla.get.inlaEnv())


co2res = inla(co2 ~ sin12 + cos12 + sin6 + cos6 +
                f(timeRw2, model = 'rw2',
                  values = StimeIndex,
                  prior='pc.prec', param = c(log(1.01)/26, 0.5)),
              data = co2s, family='gamma', lincomb = c(derivLincomb, seasonLincomb, predLincomb),
              control.family = list(hyper=list(prec=list(prior='pc.prec', param=c(2, 0.5)))),
              # add this line if your computer has trouble
              # control.inla = list(strategy='gaussian', int.strategy='eb'),
              verbose=TRUE)
summary(co2res)


# histogram

CO2_data=na.omit(co2s$co2)
mean(CO2_data)
var(CO2_data)

shape= (mean(CO2_data))^2/var(CO2_data)
shape

scale <- exp(5.885)/shape
scale
hist(CO2_data,prob=TRUE)
xseq = seq(280,450, len=1000)
dgamma(xseq,shape=shape, scale= scale)
lines(xseq,dgamma(xseq,shape=shape,scale=scale),col="red")




matplot(timePoints, exp(co2res$summary.random$timeRw2[,
                                                      c("0.5quant", "0.025quant", "0.975quant")]), type = "l",
        col = "black", lty = c(1, 2, 2), log = "y", xaxt = "n",
        xlab = "time", ylab = "ppm")
xax = pretty(timePoints)
xax
axis(1, xax, format(xax, "%Y"))
derivPred = co2res$summary.lincomb.derived[grep("time",
                                                rownames(co2res$summary.lincomb.derived)), c("0.5quant",
                                                                                             "0.025quant", "0.975quant")]
scaleTo10Years = (10 * 365.25/as.numeric(diff(timePoints,
                                              units = "days")))
matplot(timePoints[-1], scaleTo10Years * derivPred,
        type = "l", col = "black", lty = c(1, 2, 2), ylim = c(0,
                                                              0.1), xlim = range(as.numeric(co2s$date)),
        xaxs = "i", xaxt = "n", xlab = "time", ylab = "log ppm, change per 10yr")
axis(1, xax, format(xax, "%Y"))
abline(v = ISOdate(2008, 1, 1, tz = "UTC"), col = "blue")
abline(v = ISOdate(1973, 10, 1, tz = "UTC"), col = "red")
abline(v = ISOdate(1980, 1, 1, tz = "UTC"), col = "grey")
abline(v = ISOdate(1991, 11, 1, tz = "UTC"), col = "green")
abline(v = ISOdate(2001, 12, 11, tz = "UTC"), col = "purple")
abline(v = ISOdate(2015, 12, 12, tz = "UTC"), col = "pink")
abline(v = ISOdate(2003, 1, 1, tz = "UTC"), col = "orange")


matplot(StimeSeason, exp(co2res$summary.lincomb.derived[grep("season",
                                                             rownames(co2res$summary.lincomb.derived)), c("0.5quant",
                                                                                                          "0.025quant", "0.975quant")]), type = "l", col = "black",
        lty = c(1, 2, 2), log = "y", xaxs = "i", xaxt = "n",
        xlab = "time", ylab = "relative ppm")
xaxSeason = seq(ISOdate(2009, 9, 1, tz = "UTC"), by = "2 months",
                len = 20)
axis(1, xaxSeason, format(xaxSeason, "%b"))
abline(v = ISOdate(2010, 8, 1, tz = "UTC"), col = "orange")
abline(v = ISOdate(2009, 12, 1, tz = "UTC"), col = "orange")
abline(v = ISOdate(2009, 10, 1, tz = "UTC"), col = "grey")
abline(v = ISOdate(2010, 5, 20, tz = "UTC"), col = "grey")
abline(v = ISOdate(2010, 10, 1, tz = "UTC"), col = "grey")



timePred = co2res$summary.lincomb.derived[grep("pred",
                                               rownames(co2res$summary.lincomb.derived)), c("0.5quant",
                                                                                            "0.025quant", "0.975quant")]

matplot(timePoints, exp(timePred), type = "l", col = "black",
        lty = c(1, 2, 2), log = "y", xlim = ISOdate(c(2010,2025), 1, 1, tz = "UTC"), ylim = c(390, 435),
        xaxs = "i", xaxt = "n", xlab = "time", ylab = "ppm")
xaxPred = seq(ISOdate(2010, 1, 1, tz = "UTC"), by = "5 years",
              len = 20)
axis(1, xaxPred, format(xaxPred, "%Y"))

abline(v = ISOdate(2020, 1, 1, tz = "UTC"), col = "red")






