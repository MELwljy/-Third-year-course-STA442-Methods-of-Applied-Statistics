# install.packages("nlme")
# install.packages("rmarkdown")
# install.packages("MEMSS")
# install.packages("Pmisc", repos = "http://r-forge.r-project.org")
# install.packages("kableExtra")
# install.packages("data.table")
library(knitr)
library(nlme)
library(rmarkdown)
library(Pmisc)
library(kableExtra)
library(Hmisc)
install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")
data("MathAchieve", package = "MEMSS")
head(MathAchieve)


model1<- lme(MathAch ~ Minority + Sex + SES , random = ~1 | School, data=MathAchieve)
summary(model1)

knitr::kable(Pmisc::lmeTable(model1),digits = 2,escape= FALSE, 
             format="latex")

intervals(model1)$fixed

hist(model1$coefficients$random$School,main="")

# xSeq=seq(0,120,len=1000)


library('lme4')
# model2<-lmer(MathAch ~ Minority + Sex + SES+(1 | School), data=MathAchieve)
# summary(model2)

