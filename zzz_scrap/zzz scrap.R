rm(list=ls())
source("read_data.R")

names(DATA)
# X_variable = DATA[,"Paintype"]
# X_variable = DATA[,"gender"]
X_variable = DATA[,"ethnicity"]



cross_table=table(X_variable, Y_variables)
X_table=table(X_variable)
X_percent = X_table / nrow(DATA)
final_table=cbind(cross_table, X_percent)
final_table



# X_variable = DATA[,"age"]
# mean(X_variable); sd(X_variable)
# mean(X_variable)-sd(X_variable); mean(X_variable)+sd(X_variable) 

