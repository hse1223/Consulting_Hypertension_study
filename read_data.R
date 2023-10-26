rm(list=ls())

## Read the data and rename the columns.

DATA=readxl::read_excel("data.xlsx", sheet=2,col_names = T, skip=1)[,1:16]
DATA = as.data.frame(DATA)
rownames(DATA) <- DATA[,1]
DATA <- DATA[,-1]
colnames(DATA) = c("Paintype", "BP1", "BP2", "BP3", "analgesic_med", "antihypertensive","age",
    "gender", "ethnicity","substance_abuse", "chronic_pain","doc_level","discharged","refer_PCP", "refer_HTN")

## Resort the data.

categorical=c("Paintype", "ethnicity", "doc_level")
dummy = c("analgesic_med", "antihypertensive", "gender", "substance_abuse", "chronic_pain", "discharged", "refer_PCP")
continuous = c("BP1", "BP2", "BP3", "age")
Y_variable = c("refer_HTN")

DATA = DATA[,c(categorical,dummy, continuous, Y_variable)]
dummies = DATA[,dummy]
dummies[dummies==2] = 0
Y_variables = DATA[,Y_variable]
Y_variables[Y_variables==2] = 0
DATA[,dummy] = dummies
DATA[,Y_variable] = Y_variables

# dim(DATA)
# str(DATA)









