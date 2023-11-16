rm(list=ls())
source("read_data.R")

X_Y = data.frame(
    X1 = as.factor(DATA[,categorical[3]]),
    X2 = DATA[,dummy[4]],
    X3 = DATA[,dummy[7]],
    Y = Y_variables
)

X_Y$X1 <- relevel(X_Y$X1, ref = "1")

glm_res = glm(Y ~ X1 + X2 + X3, data = X_Y)

summary(glm_res)




