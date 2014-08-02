# source("Z:\\vertica & hadoop\\R programming\\R scripts\\credit_score.R")
# Load the data.
# credit_data<-read.csv(file.choose(), header=T)
credit_data<-read.csv("Z:\\Credit Score\\CREDITSCORE\\data.csv", header=T)

### Bivariate Analysis.
perc.function<-function(x,y){
	rate = round((x * 100)/y,2)
	rate = paste0(rate,"%")
	return(rate)
}

lhs<-colnames(credit_data)
lhs<-lhs[-c(1,22,23)]
# eq <- paste("remove(",lhs[1:n],")", sep="",collapse=";")
n<-length(lhs)
rhs <- paste("ddply(credit_data,~", lhs[1:n],",summarise,Observations=length(", lhs[1:n],"),Default_On_Payment=sum(credit_data[credit_data$Default_On_Payment == 1 & credit_data$", lhs[1:n]," == ", lhs[1:n],",length(credit_data)-1]), Default_rate=perc.function(Default_On_Payment,Observations))", sep="")
eq <- paste(paste(lhs, rhs, sep="<-"), collapse=";")
eval(parse(text=eq))

### Logistic Regression Model.
# Create Training data from the Raw data.
train <- credit_data$Customer_ID <= 104000
training_data <- credit_data[train,]

# Find Generalized Linear Model(glm) with only 1 variable.
null <- glm(Default_On_Payment ~ Status_Checking_Acc, family = binomial, data = training_data)

# Next find glm with all the variables.
full <- glm(Default_On_Payment ~ ., family = binomial, data = training_data) 

# Tune the model, so that it will fit the exacty variables.
reg <- step(null, scope=formula(full), direction="forward", k=log(5000))

# To the prediction with sample data.
predreg <- predict(reg, newdata=credit_data[-train,], type="response")

# Calculate the errors in the model.
errorreg <- credit_data$Default_On_Payment[-train]-(predreg >= .5)
# mean(abs(errorreg))

### Writing the output to Excel File.
# Create Work book.
wb <- createWorkbook()

# Initialize the rowno variable. It is used to specify from which row to start writing the output.
rowno <- 2

# Style the Output in the Excel.
cs1 <- CellStyle(wb) + 
	Alignment(h="ALIGN_CENTER") +
	Border(color="black", position=c("TOP", "BOTTOM", "RIGHT", "LEFT"), pen=c("BORDER_THIN")) +
	Fill(backgroundColor="lavender", foregroundColor="lavender", pattern="SOLID_FOREGROUND") +
	Font(wb, isBold=TRUE)
	
cs2 <- CellStyle(wb) + 
	Border(color="black", position=c("TOP", "BOTTOM", "RIGHT", "LEFT"), pen=c("BORDER_THIN"))

# Specify the sheet name in the Excel. (Bivariate Analysis)
sheet  <- createSheet(wb, sheetName="Bivariate Analysis")

# Start adding the data frames.
addframe<- paste("addDataFrame(", lhs[1:n] ,", sheet, startRow=rowno, startColumn=2, row.names=FALSE, colnamesStyle=cs1, colStyle=list(`1`=cs2, `2`=cs2, `3`=cs2, `4`=cs2))", sep="")
rhs <- paste("rowno + (length(",lhs[1:n],"[,1])+2)",sep="")
eq1<- paste("rowno", rhs, sep="<-")
eq <- paste(paste(addframe,eq1, sep=";"), collapse=";")
eval(parse(text=eq))

# Specify next sheet. (Logistic Regression)
sheet  <- createSheet(wb, sheetName="Logistic Regression")

# Fill the data in the sheet.
model_res <- cbind("Model Accuracy",paste0(round((1-mean(abs(errorreg)))*100,2),"%"))
addDataFrame(model_res, sheet, startRow=2, startColumn=2, row.names=FALSE, col.names=FALSE, colnamesStyle=cs1, colStyle=list(`1`=cs2, `2`=cs2, `3`=cs2, `4`=cs2))

addDataFrame(summary(reg)$coefficient, sheet, startRow=4, startColumn=2, colnamesStyle=cs1, colStyle=list(`1`=cs2, `2`=cs2, `3`=cs2, `4`=cs2))

# Next sheet. Data with Predictions.
sheet  <- createSheet(wb, sheetName="Prediction Data")

# Fill the data.
data_t <- credit_data
data_t$predict<-predict(reg, newdata=data_t, type="response")
addDataFrame(data_t, sheet, startRow=2, startColumn=2, row.names=FALSE, colnamesStyle=cs1)

# Finally save the work book with the file name.
saveWorkbook(wb, "Credit_Score_Analysis.xlsx")
