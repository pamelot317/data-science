library(MASS)
library(leaps)
abalone = read.csv("abalone.csv", header=FALSE)
names(abalone) = c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
abalone$age = abalone$Rings + 1.5
n = nrow(abalone)
drops = c("Rings") #drop Rings from data
abalone = abalone[,!(names(abalone)%in%drops)]

sapply(abalone, class) #get variable type
sapply(abalone, function(x) sum(is.na(x))) #check missing values
summary(abalone) #summary statistics

par(mfrow = c(3,3))
for(i in 2:9) {
    hist(abalone[,i], xlab= names(abalone)[i], main = paste("Histogram of", names(abalone)[i]))
} #histograms of variables

#pairwise scatter plot of quantitative variables
par(mfrow = c(1,1))
pairs(~age + Length + Diameter + Height + Whole + Shucked + Viscera + Shell, data = abalone) #pairwise scatter plots
#pairwise correlation matrix of quantitative variables
round(cor(abalone[,-1]),2)
#pie chart of qualitative variable
par(mfrow = c(1,1))
pie(table(abalone$Sex), col = c('blue','purple','green'), main = 'Gender')

#boxplot of qualitative variable vs. Age
boxplot(abalone$age~abalone$Sex, main = 'Age with gender', xlab = 'gender', ylab = 'age', col = rainbow(3))

#transformation of age
par(mfrow = c(2,2))
hist(log(abalone$age), xlab = "log(age)", main = "Histogram of log(age)")
hist(sqrt(abalone$age),xlab = "square root age", main = "Histogram of square root age")
hist(1/abalone$age,xlab = "1/age", main = "Histogram of 1/age")

#split data 
set.seed(10)
index=sample(1:n, size = 4176/2, replace = FALSE)
abalone.c = abalone[index,]
abalone.v = abalone[-index,]
#side-by-side box plots of training vs validation data
vars = c("age", "Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell")
train.box = abalone.c[,(names(abalone.c)%in%vars)]
vali.box = abalone.v[,(names(abalone.v)%in%vars)]

par(mfrow = c(2,4))
for(i in 1:8){
    boxplot(cbind(train.box[,i],vali.box[,i]),names=c("training","validation"),ylab=names(train.box[i]),col=rainbow(2))
}

#try different orders of the model:
fit1 = lm(age ~ ., data = abalone.c) #full model, no transformation
fitp = lm(age ~ Sex + Length + Diameter + Height+ poly(Whole, 2, raw=TRUE) + poly(Shucked, 2, raw=TRUE) + poly(Viscera, 2, raw=TRUE) + poly(Shell, 2, raw=TRUE), data = abalone.c) #quadratic model
summary(fit1)
summary(fitp)
par(mfrow = c(1,1))
boxcox(fit1)

#logtransformation of age:
abalone$age = log(abalone$age) #perform log transformation of age
abalone.c$age = log(abalone.c$age)
abalone.v$age = log(abalone.v$age)
#plots after transformation
par(mfrow = c(1,1))
hist(abalone$age, xlab="log(age)", main = "Histogram of log(age)")
pairs(~age + Length + Diameter + Height + Whole + Shucked + Viscera + Shell, data = abalone) #pairwise scatter plots
boxplot(abalone$age~abalone$Sex, main = 'logage with gender', xlab = 'gender', ylab = 'age', col = rainbow(3))
#fit full model after transformation
fit_full = lm(age ~ ., data = abalone.c)
summary(fit_full)
hist(abalone.c$age)

#null model
none_mod = lm(age~1, data = abalone.c)
#best subsets
sub_set = regsubsets(age ~ ., data = abalone.c, nbest = 1, nvmax = 8 , method = "exhaustive")
sum_sub = summary(sub_set)
p.m = as.integer(rownames(sum_sub$which))+1
ssto = sum((abalone.c$age - mean(abalone.c$age))^2)
sse = (1 - sum_sub$rsq)*ssto
aic = n*log(sse/n)+2*p.m 
bic = n*log(sse/n)+log(n)*p.m
res_sub = cbind(sum_sub$which, sse, sum_sub$rsq, sum_sub$adjr2, sum_sub$cp, bic, aic)
colnames(res_sub)[12]= "R2"
colnames(res_sub)[13]="Ra2"
colnames(res_sub)[14]="Cp"
round(res_sub,2)
#forward stepwise
fs1 = stepAIC(none_mod, scope = list(upper=fit_full), direction = "both", k = 2)
par(mfrow = c(1,1))
plot(fs1, which = 1)  #residuals vs. fitted values
plot(fs1, which = 2) #residuals Q-Q plot
#Model 2
fit2 = lm(age~.+.*., data = abalone.c)
summary(fit2)
#forward stepwise Model 2
fs2 = stepAIC(none_mod, scope = list(upper=fit2), direction = "both", k = 2)
par(mfrow = c(1,1))
plot(fs2, which = 1)  #residuals vs. fitted values
plot(fs2, which = 2) #residuals Q-Q plot
#forward selection
fs3 = stepAIC(none_mod, scope = list(upper=fit2), direction = "forward", k = 2)
#Model 3
fit3 = lm(age~Shell + Shucked + Diameter + Whole + Sex + Viscera + Height + Length + Shucked:Whole + Shucked:Sex + Shell:Whole + Diameter:Viscera + Whole:Viscera + Diameter:Sex + Diameter:Height + Shell:Height + Shell:Viscera + Diameter:Length + Shucked:Height + Shucked:Length + Viscera:Height + Shucked:Viscera, data = abalone.c)
summary(fit3)
sum_fs1 = summary(fs1)
sum_fs2 = summary(fs2)
n.c = nrow(abalone.c)
#info for fs1
sse_fs1 = (1 - 0.5962)*ssto
mse_full = 0.02979076
cp_fs1 = (sse_fs1/mse_full)-(n.c-2*9)
press_fs1 = sum((fs1$residuals/(1-influence(fs1)$hat))^2)
#info for fs2
sse_fs2 = (1 - 0.6546)*ssto
cp_fs2 = (sse_fs2/mse_full)-(n-2*23)
press_fs2 = sum((fs2$residuals/(1-influence(fs2)$hat))^2)
