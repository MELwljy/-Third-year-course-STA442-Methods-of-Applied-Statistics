pedestrainFile = Pmisc::downloadIfOld("http://pbrown.ca/teaching/appliedstats/data/pedestrians.rds")
pedestrians = readRDS(pedestrainFile)
pedestrians = pedestrians[!is.na(pedestrians$time),
                          ]
pedestrians$y = pedestrians$Casualty_Severity == "Fatal"
pedestrians$timeCat = format(pedestrians$time, "%Y_%b_%a_h%H")
pedestrians$strata = paste(pedestrians$Light_Conditions,
                           pedestrians$Weather_Conditions, pedestrians$timeCat)

theTable = table(pedestrians$strata, pedestrians$y)
# theTable[, 2]==0 ???TRUE 
# theTable[, 1]==0 ???false

onlyOne = rownames(theTable)[which(theTable[, 1] ==
                                     0 | theTable[, 2] == 0)]
x = pedestrians[!pedestrians$strata %in% onlyOne, ]

summary(glm(y ~ sex + age + Light_Conditions + Weather_Conditions,
                    data = x, family = "binomial"))$coef[1:4, ]

a=glm(y ~ sex + age + Light_Conditions + Weather_Conditions,
       data = x, family = "binomial")

hist(a$coefficients,main = "",prob=TRUE)
summary(a)

install.packages("Publish")
library(Publish)
publish(a,intercept=TRUE)


library("survival")
theClogit = clogit(y ~ age + age:sex + strata(strata),
                   data = x)
summary(theClogit)
knitr::kable(summary(theClogit)$coef,digits=2)

publish(theClogit,intercept=TRUE)

theCoef = rbind(as.data.frame(summary(theClogit)$coef),
                `age 26 - 35` = c(0, 1, 0, NA, NA))
theCoef$sex = c("Male", "Female")[1 + grepl("Female",
                                            rownames(theCoef))]
theCoef$age = as.numeric(gsub("age|Over| - [[:digit:]].*|[:].*",
  "", rownames(theCoef)))
theCoef = theCoef[order(theCoef$sex, theCoef$age),
                  ]
matplot(theCoef[theCoef$sex == "Male", "age"], exp(as.matrix(theCoef[theCoef$sex ==
                                                                       "Male", c("coef", "se(coef)")]) %*% Pmisc::ciMat(0.99)),
        ylab="odds(male,age range)/odds(male,age=25-35)", log = "y", type = "l", col = "black", lty = c(1, 2, 2), xaxs = "i", yaxs = "i")

matplot(theCoef[theCoef$sex == "Female", "age"], exp(as.matrix(theCoef[theCoef$sex ==
                                                                         "Female", c("coef", "se(coef)")]) %*% Pmisc::ciMat(0.99)),
        ylab="odds(female,age range)/odds(male,age=25-35)",log = "y", type = "l", col = "black", lty = c(1,2, 2), xaxs = "i")







