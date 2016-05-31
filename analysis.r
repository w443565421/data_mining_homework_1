#导入数据
Records<-read.table("Analysis.txt",header=F,dec=".",col.names=c("season","size","speed","mxPH","mnO2","Cl","NO3","NH4","oPO4","PO4","Chla","a1","a2","a3","a4","a5","a6","a7"),na.strings=c("XXXXXXX"))

#得到数据基本的统计量
summary(Records)

#统计缺失值个数
missing_mxPH<-nrow(Records[!complete.cases(Records$mxPH),])
missing_mnO2<-nrow(Records[!complete.cases(Records$mnO2),])
missing_Cl<-nrow(Records[!complete.cases(Records$Cl),])
missing_NO3<-nrow(Records[!complete.cases(Records$NO3),])
missing_NH4<-nrow(Records[!complete.cases(Records$NH4),])
missing_oPO4<-nrow(Records[!complete.cases(Records$oPO4),])
missing_PO4<-nrow(Records[!complete.cases(Records$PO4),])
missing_Chla<-nrow(Records[!complete.cases(Records$Chla),])

#画直方图
png(file="hist_mxPH.png", bg="transparent")
hist(Records$mxPH, prob=T, main="hist plot:mxPH")
dev.off()

png(file="hist_mnO2.png", bg="transparent")
hist(Records$mnO2, prob=T, main="hist plot:mnO2")
dev.off()

png(file="hist_Cl.png", bg="transparent")
hist(Records$Cl, prob=T, main="hist plot:Cl")
dev.off()

png(file="hist_NO3.png", bg="transparent")
hist(Records$NO3, prob=T, main="hist plot:NO3")
dev.off()

png(file="hist_NH4.png", bg="transparent")
hist(Records$NH4, prob=T, main="hist plot:NH4")
dev.off()

png(file="hist_oPO4.png", bg="transparent")
hist(Records$oPO4, prob=T, main="hist plot:oPO4")
dev.off()

png(file="hist_PO4.png", bg="transparent")
hist(Records$PO4, prob=T, main="hist plot:PO4")
dev.off()

png(file="hist_Chla.png", bg="transparent")
hist(Records$Chla, prob=T, main="hist plot:Chla")
dev.off()

#画QQ图
png(file="qq_mxPH.png", bg="transparent")
qqnorm(Records$mxPH, main="Q-Q plot:mxPH")
qqline(Records$mxPH)
dev.off()

png(file="qq_mnO2.png", bg="transparent")
qqnorm(Records$mnO2, main="Q-Q plot:mnO2")
qqline(Records$mnO2)
dev.off()

png(file="qq_Cl.png", bg="transparent")
qqnorm(Records$Cl, main="Q-Q plot:Cl")
qqline(Records$Cl)
dev.off()

png(file="qq_NO3.png", bg="transparent")
qqnorm(Records$NO3, main="Q-Q plot:NO3")
qqline(Records$NO3)
dev.off()

png(file="qq_NH4.png", bg="transparent")
qqnorm(Records$NH4, main="Q-Q plot:NH4")
qqline(Records$NH4)
dev.off()

png(file="qq_oPO4.png", bg="transparent")
qqnorm(Records$oPO4, main="Q-Q plot:oPO4")
qqline(Records$oPO4)
dev.off()

png(file="qq_PO4.png", bg="transparent")
qqnorm(Records$PO4, main="Q-Q plot:PO4")
qqline(Records$PO4)
dev.off()

png(file="qq_Chla.png", bg="transparent")
qqnorm(Records$Chla, main="Q-Q plot:Chla")
qqline(Records$Chla)
dev.off()

#画盒图
png(file="box_mxPH.png", bg="transparent")
boxplot(Records$mxPH,col=c("steelblue"), main="box plot:mxPH")
dev.off()

png(file="box_mnO2.png", bg="transparent")
boxplot(Records$mnO2,col=c("mediumturquoise"), main="box plot:mnO2")
dev.off()

png(file="box_Cl.png", bg="transparent")
boxplot(Records$Cl,col=c("sandybrown"), main="box plot:Cl")
dev.off()

png(file="box_NO3.png", bg="transparent")
boxplot(Records$NO3,col=c("hotpink"), main="box plot:NO3")
dev.off()

png(file="box_NH4.png", bg="transparent")
boxplot(Records$NH4,col=c("red"), main="box plot:NH4")
dev.off()

png(file="box_oPO4.png", bg="transparent")
boxplot(Records$oPO4,col=c("yellow"), main="box plot:oPO4")
dev.off()

png(file="box_PO4.png", bg="transparent")
boxplot(Records$PO4,col=c("green"), main="box plot:PO4")
dev.off()

png(file="box_Chla.png", bg="transparent")
boxplot(Records$Chla,col=c("orange"), main="box plot:Chla")
dev.off()

#将缺失部分剔除
Records_1<-na.omit(Records)
write.table(Records_1, file='Analysis_1.txt', row.names=F, quote=F)

#用最高频率值来填补缺失值
library(DMwR)
data(Records)
Records_2<-Records[-manyNAs(Records),]
Records_2<-centralImputation(Records)
write.table(Records_2, file='Analysis_2.txt', row.names=F, quote=F)

#分析属性的相关关系
symnum(cor(Records[, 4:18], use = "complete.obs"))

#通过数据对象之间的相似性来填补缺失值
#Records_3<-Records[-manyNAs(Records),]
Records_3<-knnImputation(Records_2, k=10)
write.table(Records_3, file='Analysis_3.txt', row.names=F, quote=F)

 write.table(Records, file = "E:/1_RecordsMining2015/assignment1/cleanRecords.txt", row.names = F, quote = F, sep="\t") 