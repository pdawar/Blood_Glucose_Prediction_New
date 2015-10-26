setwd("/home/bassamqer/Desktop/AI/Diabetes-Data")


for(j in 1:70){
DD = read.table(paste("data-",j,sep=""),colClasses=c("character","character","numeric","numeric"))
GL_Codes = c(48,57,58,59,60,61,62,63,64)
DD = within(DD, V1 <- paste(V1,V2,sep=" "))
DD_GL = DD[which(DD$V3 %in% GL_Codes),]

# get the date and time that have a GL value

DD_Clean = DD[which(DD$V1 %in% DD_GL$V1),]
# expand the column to many
n = dim(DD_Clean)

DD_expanded = list()
Codes = c(33,34,35,48,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72)
i = 1
while(i <= n[1]){
  dt = DD_Clean[i,1]
  code = DD_Clean[i,3]
  codeindex = which(Codes == code)
  temp_list = c(dt,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  # codeindex + 1 because of dt is at first position
  temp_list[codeindex+1] = DD_Clean[i,4]
  DD_expanded[[i]] = temp_list
  i = i + 1
}
# convert to data frame
m = length(DD_expanded)
DD_expanded = data.frame(matrix(unlist(DD_expanded), nrow=m, byrow=T),stringsAsFactors=FALSE)

# change columns type to numeric
cols.num <- c("X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21")
DD_expanded[cols.num] <- sapply(DD_expanded[cols.num],as.numeric)
sapply(DD_expanded, class)
DD_expanded[c("X1")] <-as.POSIXct(strptime(DD_expanded$X1, "%m-%d-%Y %H:%M"))
# group by date time
library(data.table)
DD_ex = as.data.table(DD_expanded)
DD_ex[, lapply(.SD, sum), by = X1]
DD_ex = within(DD_ex,X22 <-X5+X6+X7+X8+X9+X10+X11+X12+X13)

DD_ex = as.data.frame(DD_ex)

#data.frame(DD_ex$X1,DD_ex$X22)
library(rpart)
fit <- rpart(X22 ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21,data = DD_ex, method = "anova")

DD_pred = DD_ex
DD_pred = within(DD_pred, X23 <- X22)
DD_pred = within(DD_pred, X24 <- X22)



k = dim(DD_ex)
count = 0
for(i in 1:k[1]){
  TT <- DD_ex[i,]
  realTT <-  DD_ex[i,22]
  TT <- subset(TT,select = - c(X22))
  predTT <-predict(fit,TT)
  error <- abs(realTT - predTT)
  DD_pred[i,23] = predTT
  DD_pred[i,24] = error

}
DD_pred$X24 <- round(DD_pred$X24,2)
DD_pred$X23 <- round(DD_pred$X23,2)
write.table(DD_pred,paste(paste("Output_Dataset/data-",j,sep=""),"-output.txt",sep=""),quote=FALSE,row.names=FALSE)
}



