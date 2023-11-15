rm(list=ls())
source("read_data.R")

N=nrow(DATA)


categorical[3]

DATA[,categorical[3]]
pchisq(q=mosaic::chisq(table(X_variables, Y_variables)), df=length(unique(X_variables))-1, lower.tail = F) # significant







