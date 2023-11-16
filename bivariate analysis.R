rm(list=ls())
source("read_data.R")

N=nrow(DATA)


### categorical

categorical
table(DATA[,categorical[1]])
table(DATA[,categorical[2]]) # Let us drop group=5 and group=6.
table(DATA[,categorical[3]])

X_variables = DATA[,categorical[1]]
pchisq(q=mosaic::chisq(table(X_variables, Y_variables)), df=length(unique(X_variables))-1, lower.tail = F)

X_variables = DATA[,categorical[2]]
drop=which(is.element(X_variables, c(5,6))) # dropped such categories
X_variables_new = X_variables[-drop]
Y_variables_new = Y_variables[-drop]
pchisq(q=mosaic::chisq(table(X_variables_new, Y_variables_new)), df=length(unique(X_variables_new))-1, lower.tail = F)

X_variables = DATA[,categorical[3]]
pchisq(q=mosaic::chisq(table(X_variables, Y_variables)), df=length(unique(X_variables))-1, lower.tail = F) # significant



### binary


# table(DATA[,dummy[1]])
# table(DATA[,dummy[2]])
# table(DATA[,dummy[3]])
# table(DATA[,dummy[4]])
# table(DATA[,dummy[5]])
# table(DATA[,dummy[6]])
# table(DATA[,dummy[7]])

dummy
pchisq(q=mosaic::chisq(table(DATA[,dummy[1]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[2]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[3]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[4]], Y_variables)), df=1, lower.tail = F) # almost significant
pchisq(q=mosaic::chisq(table(DATA[,dummy[6]], Y_variables)), df=1, lower.tail = F)
pchisq(q=mosaic::chisq(table(DATA[,dummy[7]], Y_variables)), df=1, lower.tail = F) # significant

drop_index=which(DATA[,dummy[5]]==3) # This variable has weird behavior.
pchisq(q=mosaic::chisq(table(DATA[-drop_index,dummy[5]], Y_variables[-drop_index])), df=1, lower.tail = F)





### continuous

continuous
X_var = DATA[,continuous[1]]; res=glm(Y_variables ~ X_var); summary(res) 
X_var = DATA[,continuous[2]]; res=glm(Y_variables ~ X_var); summary(res)
X_var = DATA[,continuous[3]]; res=glm(Y_variables ~ X_var); summary(res)
X_var = DATA[,continuous[4]]; res=glm(Y_variables ~ X_var); summary(res)

X_var = DATA[,continuous[3]] - DATA[,continuous[1]]; res=glm(Y_variables ~ X_var); summary(res) 


