rm(list=ls())
source("read_data.R")

X_variables = DATA[,c(categorical[3], dummy[4], dummy[7])]

glm_res1 = glm(Y_variables~X_variables[,1] + X_variables[,2] + X_variables[,3])
summary(glm_res1)

glm_res2 = glm(Y_variables~X_variables[,2] + X_variables[,3])
summary(glm_res2)

anova_result <- anova(glm_res1, glm_res2, test = "Chi") 
anova_result

# anova_result <- anova(glm_res2, glm_res1, test = "Chi") # reverse gives us the same result.
# anova_result







