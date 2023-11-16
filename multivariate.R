rm(list=ls())
source("read_data.R")

X_variables = DATA[,c(categorical[3], dummy[4], dummy[7])]

glm_res = glm(Y_variables~X_variables[,1] + X_variables[,2] + X_variables[,3])

summary(glm_res)




