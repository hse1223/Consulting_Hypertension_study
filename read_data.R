rm(list=ls())

DATA=readxl::read_excel("data.xlsx", sheet=2,col_names = T, skip=1)[,1:16]
DATA = as.data.frame(DATA)

rownames(DATA) <- DATA[,1]
DATA <- DATA[,-1]
colnames(DATA) = c("Paintype", "BP1", "BP2", "BP3", "analgesic_med", "antihypertensive","age",
    "gender", "ethnicity","substance_abuse", "chronic_pain","doc_level","discharged","refer_PCP", "refer_HTN")

categorical=c("Paintype", "ethnicity", "doc_level")
dummy = c("analgesic_med", "antihypertensive", "gender", "substance_abuse", "chronic_pain", "discharged", "refer_PCP")
continuous = c("BP1", "BP2", "BP3", "age")
Y_variable = c("refer_HTN")

DATA = DATA[,c(categorical,dummy, continuous, Y_variable)]
dummies = DATA[,dummy]
dummies[dummies==2] = 0
DATA[,dummy] = dummies

dim(DATA)

str(DATA)



# categorical = DATA[,c("Paintype", "ethnicity", "doc_level")]
# dummy = DATA[,c("analgesic_med", "antihypertensive", "gender", "substance_abuse", "chronic_pain", "discharged", "refer_PCP")]
# continuous = DATA[,c("BP1", "BP2", "BP3","age")]
# Y_variable = DATA[,c("refer_HTN")]
# dim(categorical)[2] + dim(dummy)[2] + dim(continuous)[2] 
# DATA[,]

