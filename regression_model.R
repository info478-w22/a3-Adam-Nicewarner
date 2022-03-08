
library(data.table)
library(mgcv)
library(dlnm)


###############################################################################################################
### Life expectancy        
###############################################################################################################
#Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)
#Developed or Developing status
#Life Expectancy in age
#Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)
#percentage expenditure, Expenditure on health as a percentage of Gross Domestic Product per capita(%)
#GDP, Gross Domestic product 
#Schooling in years 


dir<-"H:/repos/iSchool/disease_modelling/"
data<-fread(paste0(dir,"Life Expectancy Data.csv"))
str(data)

DATA<-data[,c("Country","Year","Status","Life expectancy","Alcohol","percentage expenditure","Total expenditure",
              "BMI","GDP","Income composition of resources","Schooling")]

COR<-cor(na.omit(DATA[,4:11]))
  
pairs.panels(DATA[,4:11], histogram=TRUE, pch=19)
     
colnames(DATA)<-c("Country", "Year", "Status", "Life_expectancy", "Alcohol", "percentage_expenditure", 
              "Total_expenditure", "BMI", "GDP",  "Income_composition_of_resources", "Schooling")


mgam<-gam(Life_expectancy ~ as.factor(Status)+Year+s(GDP,k=5,fx=TRUE)+s(BMI), data=DATA)
summary(mgam)
plot(mgam)
