library(foreign)
fevspss = read.spss(file = "fev.sav")
fevtxt = read.table(file ="fev.txt", header = TRUE)

test = function(){
  for (cat in fevtxt){
    print(is.numeric(cat))
  }
  print(summary(fevtxt))
  
  for (cat in fevspss){
    print(is.numeric(cat))
  }
  print(summary(fevspss))
}

fev = fevspss
fev$SEX = as.factor(fev$SEX)
fev$SMOKING = as.factor(fev$SMOKING)

par(mfrow=c(2,2))
fev_smokers = fev$FEV[fev$SMOKING==1]
fev_non = fev$FEV[fev$SMOKING==0]
boxplot(fev_smokers,fev_non,names = c("S", "N"),horizontal = FALSE, ylab="FAV")

plot(fev$AGE,fev$HEIGHT, "n")
line(fev$AGE[fev$SEX == 1],fev$HEIGHT[fev$SEX == 1], col=1)


