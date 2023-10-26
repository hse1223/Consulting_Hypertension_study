# Script made by Xingchi Li (anthony.li@stat.tamu.edu) on 10/25/2023.

## Read the data and rename the columns.
data <- readxl::read_excel("data.xlsx", sheet = 2, col_names = TRUE, skip = 1)[, seq_len(16)]
data <- as.data.frame(data)
rownames(data) <- data[, 1]
data <- data[, -1]
colnames(data) <- c(
  "Paintype", # categorical_variables
  "Initial BP", # continuous_variables
  "Discharge BP", # continuous_variables
  "Highest BP", # continuous_variables
  "Have Analgesic", # binary_variables
  "Have Antihypertensives", # binary_variables
  "Age", # continuous_variables
  "Is Male", # binary_variables
  "Ethnicity", # categorical_variables
  "Have Abuse", # binary_variables
  "Have Chronic Pain", # binary_variables
  "Doc Level", # categorical_variables
  "Is Discharged", # binary_variables
  "Is Referred PCP", # binary_variables
  "Is Referred HTN" # response_variable
)

## Resort the data.
categorical_variables <- c("Paintype", "Ethnicity", "Doc Level")
binary_variables <- c(
  "Have Analgesic",
  "Have Antihypertensives",
  "Is Male",
  "Have Abuse",
  "Have Chronic Pain",
  "Is Discharged",
  "Is Referred PCP"
)
bp_variables <- c("Initial BP", "Highest BP", "Discharge BP")
continuous_variables <- c(bp_variables, "Age")
response_variable <- "Is Referred HTN"

stopifnot(length(categorical_variables) + length(binary_variables) == 10)
stopifnot(10 + length(continuous_variables) + 1 == ncol(data))


for (column_name in c(binary_variables, response_variable)) {
  data[data[[column_name]] == 2, column_name] <- 0
}

for (column_name in c(categorical_variables, binary_variables, response_variable)) {
  data[, column_name] <- as.factor(data[, column_name])
}

pdf("bivariate_analysis/balloonplot.pdf", width = 20, height = 18)
# Deal with categorical variables.
par(mfrow = c(4, 5))
chisq_list <- list()
for (column_name in c(binary_variables, categorical_variables)) {
  column_table <- table(data[, c(response_variable, column_name)])
  gplots::balloonplot(
    column_table,
    main = paste0("Balloon Plot of ", column_name, " and ", response_variable),
    xlab = response_variable,
    ylab = column_name,
    label = FALSE,
    show.margins = FALSE
  )
}
for (column_name in c(binary_variables, categorical_variables)) {
  column_table <- table(data[, c(response_variable, column_name)])
  chisq_list[[column_name]] <- chisq.test(t(column_table))
  corrplot::corrplot(chisq_list[[column_name]]$residuals, is.cor = FALSE, ylab = column_name)
}
dev.off()

pdf("bivariate_analysis/Doc Level.pdf", width = 10, height = 10)
par(mfrow = c(1, 1))
column_name <- "Doc Level"
column_table <- table(data[, c(response_variable, column_name)])
gplots::balloonplot(
  column_table,
  main = paste0("Balloon Plot of ", column_name, " and ", response_variable),
  xlab = response_variable,
  ylab = column_name,
  label = FALSE,
  show.margins = FALSE
)
corrplot::corrplot(chisq_list[[column_name]]$residuals, is.cor = FALSE)
dev.off()

molten_data <- reshape2::melt(data[, c(bp_variables, response_variable)], id.vars = response_variable)
bp_box_plots <- ggplot2::ggplot(molten_data, ggplot2::aes(x = variable, y = value, fill = `Is Referred HTN`)) + ggplot2::geom_boxplot()

age_data <- reshape2::melt(data[, c("Age", response_variable)], id.vars = response_variable)
age_box_plot <- ggplot2::ggplot(age_data, ggplot2::aes(x = variable, y = value, fill = `Is Referred HTN`)) + ggplot2::geom_boxplot()

pdf("bivariate_analysis/boxplot.pdf", width = 16, height = 10)
gridExtra::grid.arrange(bp_box_plots, age_box_plot, ncol = 2, widths = c(2, 1))
dev.off()
