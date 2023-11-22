rm(list=ls())
source("read_data.R")

N=nrow(DATA)


### categorical

categorical


chisq.test(x=DATA[,categorical[1]], y=Y_variables)
chisq.test(x=DATA[,categorical[2]], y=Y_variables)
chisq.test(x=DATA[,categorical[3]], y=Y_variables) # significant

mosaic::chisq(table(DATA[,categorical[1]], Y_variables))
mosaic::chisq(table(DATA[,categorical[2]], Y_variables))
mosaic::chisq(table(DATA[,categorical[3]], Y_variables))

# chisq.test(x=table(DATA[,categorical[1]], Y_variables))
# chisq.test(x=table(DATA[,dummy[1]], Y_variables))


### binary

dummy



pchisq(q=mosaic::chisq(table(DATA[,dummy[1]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[2]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[3]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[4]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[5]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[6]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[7]], Y_variables)), df=1, lower.tail = F)



# chisq.test(x=DATA[,dummy[1]], y=Y_variables)
# pchisq(q=1.9295, df=1, lower.tail = F)

X_variables=DATA[,dummy[1]]
p0=sum(X_variables==0)/N; p1=sum(X_variables==1)/N 
q0=sum(Y_variables==0)/N; q1=sum(Y_variables==1)/N
E00 = N*p0*q0; O00 = sum(X_variables==0 & Y_variables==0)
E01 = N*p0*q1; O01 = sum(X_variables==0 & Y_variables==1)
E10 = N*p1*q0; O10 = sum(X_variables==1 & Y_variables==0)
E11 = N*p1*q1; O11 = sum(X_variables==1 & Y_variables==1)
O_vec = c(O00,O01,O10,O11); E_vec = c(E00, E01, E10, E11)
sum((O_vec - E_vec)^2 / E_vec)
mosaic::chisq(table(DATA[,dummy[1]], Y_variables))




### continuous

continuous


## normality check 

hist(DATA[,continuous[1]])
hist(DATA[,continuous[2]])
hist(DATA[,continuous[3]])
hist(DATA[,continuous[4]])

qqnorm(DATA[,continuous[1]]); qqline(DATA[,continuous[1]])
qqnorm(DATA[,continuous[2]]); qqline(DATA[,continuous[2]])
qqnorm(DATA[,continuous[3]]); qqline(DATA[,continuous[3]])
qqnorm(DATA[,continuous[4]]); qqline(DATA[,continuous[4]])

shapiro.test(DATA[,continuous[1]])
shapiro.test(DATA[,continuous[2]])
shapiro.test(DATA[,continuous[3]])
shapiro.test(DATA[,continuous[4]])

ks.test(DATA[,continuous[1]]+rnorm(N,sd=0.1), "pnorm")
ks.test(DATA[,continuous[2]]+rnorm(N,sd=0.1), "pnorm")
ks.test(DATA[,continuous[3]]+rnorm(N,sd=0.1), "pnorm")
ks.test(DATA[,continuous[4]]+rnorm(N,sd=0.1), "pnorm")


## Two-sample t-test

X_var = DATA[,continuous[1]]; t.test(x=X_var[Y_variables==0], y=X_var[Y_variables==1])
X_var = DATA[,continuous[2]]; t.test(x=X_var[Y_variables==0], y=X_var[Y_variables==1])
X_var = DATA[,continuous[3]]; t.test(x=X_var[Y_variables==0], y=X_var[Y_variables==1])
X_var = DATA[,continuous[4]]; t.test(x=X_var[Y_variables==0], y=X_var[Y_variables==1])


## Logistic regression

X_var = DATA[,continuous[1]]; res=glm(Y_variables ~ X_var); summary(res) # best
X_var = DATA[,continuous[2]]; res=glm(Y_variables ~ X_var); summary(res)
X_var = DATA[,continuous[3]]; res=glm(Y_variables ~ X_var); summary(res)
X_var = DATA[,continuous[4]]; res=glm(Y_variables ~ X_var); summary(res)


X_var = DATA[,continuous[1]]; res=glm(Y_variables ~ X_var - 1); summary(res) # best
X_var = DATA[,continuous[2]]; res=glm(Y_variables ~ X_var - 1); summary(res)
X_var = DATA[,continuous[3]]; res=glm(Y_variables ~ X_var - 1); summary(res)
X_var = DATA[,continuous[4]]; res=glm(Y_variables ~ X_var - 1); summary(res)









