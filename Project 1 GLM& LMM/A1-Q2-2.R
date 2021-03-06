# Second Model
smokeUrl = 'http://pbrown.ca/teaching/appliedstats/data/smoke.RData'
(smokeFile = tempfile(fileext='.RData'))
download.file(smokeUrl, smokeFile, mode='wb')
(load(smokeFile))
dim(smoke)

smoke[1:20,c('Age','Sex','Grade','RuralUrban','Race', 'ever_tobacco_hookah_or_wa')]
smokeFormats[smokeFormats$colName == 'ever_tobacco_hookah_or_wa', ]
smoke$everSmoke = factor(smoke$ever_tobacco_hookah_or_wa, levels=c('TRUE','FALSE'), labels=c('yes','no'))
table(smoke$Grade, smoke$Age, exclude=NULL) 
table(smoke$Race, smoke$everSmoke, exclude=NULL)


smokeSub = smoke[smoke$Age >= 10 & !is.na(smoke$Race) & 
                   !is.na(smoke$everSmoke) & !is.na(smoke$ever_tobacco_hookah_or_wa) &!is.na(smoke$Sex), ] 
dim(smokeSub)

smokeAgg = reshape2::dcast(smokeSub,
                           Age + Sex + Race + RuralUrban ~ everSmoke,
                           length)
dim(smokeAgg)
smokeAgg = na.omit(smokeAgg)
dim(smokeAgg)

smokeAgg$y = cbind(smokeAgg$yes, smokeAgg$no)
smokeFit = glm(y ~ Age + Sex + Race + RuralUrban, 
               family=binomial(link='logit'), data=smokeAgg)

knitr::kable(summary(smokeFit)$coef, digits=3)

smokeAgg$ageC = smokeAgg$Age - 15
smokeFit2 = glm(y ~ ageC + Sex + Race + RuralUrban, 
                family=binomial(link='logit'), data=smokeAgg) 
knitr::kable(summary(smokeFit2)$coef, digits=3)
#The odds of regular use of hookah
knitr::kable(exp(summary(smokeFit)$coef), digits=3)

# The CI of parameter
sum =summary(smokeFit2)
est=sum$coefficients[,1]
std=sum$coefficients[,2]

exp_upper_bound = exp(est+2*std)
exp_upper_bound
exp_lower_bound= exp(est-2*std)
exp_lower_bound
cbind(exp_lower_bound,exp_upper_bound)
