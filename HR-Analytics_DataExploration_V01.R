library(ggplot2)
library(corrplot)
library(gplots)

#Read the HR Dataset
hr.df <- read.csv("HR.csv", header = TRUE)


####################Data Exploration####################

#Satisfaction of the employees based on their salary level
boxplot(Emp_Satisfaction ~ salary, data=hr.df, horizontal=TRUE,
        ylab="Salary Level", xlab="Satisfaction Level", las =1,
        main="Satisfaction of the employees based on their salary level",
        col=c("azure2","gold","darksalmon"))

#Box Plot of Satisfaction Level Vs Left company
boxplot(Emp_Satisfaction ~left_Company, data=hr.df, horizontal=TRUE,
        ylab="Left", xlab="Satisfaction level", las=1,
        main="Employee Left on the basis of their satisfaction level",
        col=c("azure2","gold")
)

#Barplot of employees leaving/not-leaving the company vs time spend using GGPLOT
ggplot(aes(x = factor(hr.df$time_spend_company)),data = hr.df) + 
  geom_bar(fill = 'lightcyan2',color='navy') + 
  xlab("Time spend at company in years") + 
  ylab("Frequency")+
  labs(title = "Barplot of employee leaving the Company vs time spend")  +
  facet_wrap(~left_Company)

#----------------------------------------Why good employees leave--------------------------------------------------- 
#people that left
leavers = subset(hr.df,hr.df[,19] == 1)

#filter out people with a good last evaluation. Taking rating 7 as the threshold
leaving_performers <- subset(leavers,leavers[,15] > 7)

#Analyzing reasons for such employees to have left the company

#Was number of projects, they were assigned to the reason?
table(No_of_projects = leaving_performers$number_project, Good_Emp_leavers =  leaving_performers$left_Company)

#or may be it was Salary
ggplot(aes(x = Department),data = leaving_performers ) +
  geom_bar(aes(fill = salary))  +
  xlab('Department') + 
  ylab('EmpCounts') +
  coord_flip()

Sal_leavers <- xtabs(~Department+salary, data = leaving_performers)
Sal_leavers

#Correlation Matrix
# Convert Category values to Factors
hr.df$salary <- factor(hr.df$salary, levels = c("high", "low", "medium"), 
                       labels = c(1, 3, 2))
hr.df$Gender <- factor(hr.df$Gender, levels = c("F", "M"), 
                       labels = c(0, 1))
hr.df$Role <- factor(hr.df$Role, levels = c("Director","Level 1","Level 2-4","Manager","Senior Director","Senior Manager","VP"),
                     labels = c(3,7,6,5,2,4,1))

#Convert Factors into Numeric
hr.df$salary = as.numeric(paste(hr.df$salary))
hr.df$Gender = as.numeric(paste(hr.df$Gender))
hr.df$Role = as.numeric(paste(hr.df$Role))

hrform.df <- hr.df[,-c(1,2,3,4,11)]
heatmap.2(cor(hrform.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(hrform.df),2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

