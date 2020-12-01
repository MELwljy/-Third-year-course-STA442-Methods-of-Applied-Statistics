heatUrl = "http://pbrown.ca/teaching/appliedstats/data/sableIsland.rds"
heatFile = tempfile(basename(heatUrl))
download.file(heatUrl, heatFile)
x = readRDS(heatFile)
x$month = as.numeric(format(x$Date, "%m"))
xSub = x[x$month %in% 5:10 & !is.na(x$Max.Temp...C.),
         ]
xSub$Date
weekValues = seq(min(xSub$Date), ISOdate(2030, 1, 1,
                                         0, 0, 0, tz = "UTC"), by = "7 days")
xSub$week = cut(xSub$Date, weekValues)
xSub$weekIid = xSub$week
xSub$day = as.numeric(difftime(xSub$Date, min(weekValues),
                               units = "days"))
xSub$day
xSub$Date

xSub$cos12 = cos(xSub$day * 2 * pi/365.25)
xSub$sin12 = sin(xSub$day * 2 * pi/365.25)
xSub$cos6 = cos(xSub$day * 2 * 2 * pi/365.25)
xSub$sin6 = sin(xSub$day * 2 * 2 * pi/365.25)
xSub$yearFac = factor(format(xSub$Date, "%Y"))

lmStart = lm(Max.Temp...C. ~ sin12 + cos12 + sin6 +
               cos6, data = xSub)
startingValues = c(lmStart$fitted.values, rep(lmStart$coef[1],
                                              nlevels(xSub$week)), rep(0, nlevels(xSub$weekIid) +
                                                                         nlevels(xSub$yearFac)), lmStart$coef[-1])
INLA::inla.doc('^t$')
library("INLA")
mm = get("inla.models", INLA:::inla.get.inlaEnv())
if(class(mm) == 'function') mm = mm()
mm$latent$rw2$min.diff = NULL
assign("inla.models", mm, INLA:::inla.get.inlaEnv())
sableRes = INLA::inla(
  Max.Temp...C. ~ 0 + sin12 + cos12 + sin6 + cos6 +
    #random slope
    f(week, model='rw2',
      constr=FALSE,
      prior='pc.prec',
      param = c(0.1/(52*100), 0.05)) +
    #random intercept 
    f(weekIid, model='iid',
      prior='pc.prec',
      param = c(1, 0.5)) +
    #random intercept
    f(yearFac, model='iid', prior='pc.prec',
      param = c(1, 0.5)),
  family='T',
  # Vi
  control.family = list(
    hyper = list(
      prec = list(prior='pc.prec', param=c(1, 0.5)),
      dof = list(prior='pc.dof', param=c(10, 0.5)))),
  control.mode = list(theta = c(-1,2,20,0,1),
                      x = startingValues, restart=TRUE),
  control.compute=list(config = TRUE),
  # control.inla = list(strategy='gaussian', int.strategy='eb'),
  data = xSub, verbose=TRUE)


#histogram
Temp_data = na.omit(xSub$Max.Temp...C.)
hist(Temp_data,prob=TRUE)


sableRes$summary.hyper[, c(4, 3, 5)]
sableRes$summary.fixed[, c(4, 3, 5)]

#Pmisc::priorPost(sableRes)$summary[, c(1, 3, 5)]

mySample = inla.posterior.sample(n = 24, result = sableRes,
                                 num.threads = 8, selection = list(week = seq(1,
                                                                        nrow(sableRes$summary.random$week))))
length(mySample)
names(mySample[[1]])
weekSample = do.call(cbind, lapply(mySample, function(xx) xx$latent))
dim(weekSample)
head(weekSample)
plot(x$Date, x$Max.Temp...C., col = mapmisc::col2html("black",
                                                      0.3))
forAxis = ISOdate(2016:2020, 1, 1, tz = "UTC")
plot(x$Date, x$Max.Temp...C., xlim = range(forAxis),
     xlab = "time", ylab = "degrees C", col = "red",
     xaxt = "n")
points(xSub$Date, xSub$Max.Temp...C.)
axis(1, forAxis, format(forAxis, "%Y"))

sableRes$summary.random$week
weekValues[-1]
matplot(weekValues[-1], sableRes$summary.random$week[,
                                                     paste0(c(0.5, 0.025, 0.975), "quant")], type = "l",
        lty = c(1, 2, 2), xlab = "time", ylab = "degrees C",
        xaxt = "n", col = "black", xaxs = "i")
forXaxis2 = ISOdate(seq(1880, 2040, by = 20), 1, 1,
                    tz = "UTC")

axis(1, forXaxis2, format(forXaxis2, "%Y"))

myCol = mapmisc::colourScale(NA, breaks = 1:8, style = "unique",
                             col = "Set2", opacity = 0.3)$col
matplot(weekValues[-1], weekSample, type = "l", lty = 1,
        col = myCol, xlab = "time", ylab = "degrees C",
        xaxt = "n", xaxs = "i")
axis(1, forXaxis2, format(forXaxis2, "%Y"))
weekSample
