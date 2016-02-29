#Enter data (Three groups of 8 final test scores)
grade = c(70,56,56,49,56,57,49,60,70,69,65,81,47,64,62,72,73,77,82,72,79,73,84,82)
group = c(rep("A", 8),rep("B", 8),rep("C", 8))
data = data.frame(Group=group,Grade=grade)
data$group <- factors(data$group)
levels(data$Group)<-c("Conceptual learning","Conceptual+Examples","Conceptual\n+Examples\n+Computer")

#Plot groups means with +/- 1 SE
library(gplots)

#Calculate group standard errors
groupMeans = tapply(data$Grade,data$Group,mean)
groupSD = tapply(data$Grade,data$Group,sd)
groupN = tapply(data$Grade,data$Group,length)
groupSE = groupSD/sqrt(groupN)

#Save plot to pdf
png("~/Dropbox/GSI/PSYCH 613/Problem Sets/Problem Set 2/2dPlotR.png")
#plot.new()
plotCI(x = groupMeans, uiw = groupSE,liw = groupSE,err="y",xaxt="n",ylab="Mean Test Scores (Error bars = +/- 1 se)",main = "Mean Test Score by Teaching Method", xlab="Teaching Method")
labs=levels(data$Group)
axis(side=1, 1:3, labels=levels(data$Group))
dev.off()

#ANOVA
out <- aov(data$Grade ~ data$Group)
print(summary(out))

