PAphos <- c(64,60,71,61,54,77,81,93,93,51,76,96,77,93,95,54,168,99)
inorg <- c(0.4,0.4,3.1,0.6,4.7,1.7,9.4,10.1,11.6,12.6,10.9,23.1,23.1,21.6,23.1,1.9,26.8,29.9)
org <-  c(53,23,19,34,24,65,44,31,29,58,37,46,50,44,56,36,58,51)
par(mfrow=c(1,2))
plot(inorg,PAphos,xlab="Inorganic phosphate", ylab="Plant available phosphate")
plot(org,PAphos,xlab="Organic phosphate", ylab="Plant available phosphate")

#a(i)
iolr <- lm(PAphos~inorg)
summary(iolr)

#a(ii)
olr <- lm(PAphos~org)
summary(olr)

#a(iii)
ioolr <- lm(PAphos~org+inorg)
summary(ioolr)

#b
#I would use the regression of PAphos on inorg as it has better
#significance levels and a higher adjusted R-squared than either
#of the other models

#c
#E(PAphos) = alpha + beta*inorg
#E(PAphos) = 59.26 + 1.8434 * inorg
#

#d
#Use summary table & ttest
iolrsummary <- summary(iolr)
beta <-iolrsummary$coefficients[2]
stderrbeta <- iolrsummary$coefficients[4]
ci <- c(beta - qt(0.975, length(PAphos)-2)*stderrbeta, beta + qt(0.975, length(PAphos)-2)*stderrbeta)
#or use a builtin
ci2 <- c(confint(iolr)[2], confint(iolr)[4])
#both yield (0.8282098, 2.8586622)

#e
#iid normal distributed error with mean 0
#normally distributed response variable