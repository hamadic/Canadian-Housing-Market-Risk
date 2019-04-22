#The main questions in this project are #1, 11, 12, 13 
#(Other numbers are steps to prepare and analyse data needed to answer main questions)
#1-What is the risk assessment criteria? 
#Create Data frame "Housing Market Risk Assessment Indicators"
Indicator<- c('Credit/GDP', 'Cr_G/GDP_G', 'House_P_G','P_Income', 'P_Rent')
Period <-c( 'Current Year', 'Mean (Last-3Y)','Mean (Last-3Y)','Current Year/Mean(Last-3Y)', 'Current Year/Mean(Last-3Y)'  )
Threshold<-c(130, 2.5,3,20,20)
Criteria<-c('<130%', '<2.5%','<3%','<20%','<20%')
Risk_Ind<-data.frame(Indicator,Period,Threshold, Criteria)
View(Risk_Ind)
# 2- Specify variables needed to construct housing market risk indicators
# Credit, GDP, Credit Growth, GDP Growth, House Price Index, House Price Growth , HousePrice_to_Income, HousePrice_to_Rent
# 3-Install excell package
install.packages(readxl)
library(readxl)
install.packages("writexl")
install.packages("WriteXLS")
library(writexl)
library(WriteXLS)
# 4- Import  five Datasets and take subsets with required variables
#4-1- Import dataset #1
Price_Rent<-read_xlsx("C:/Users/Cherin/Desktop/R/PtoRent.xlsx")
View(Price_Rent)
names(Price_Rent)
#4-1-1- Take subset that contains all rows and three columns (end of year)
Price_Rent1<-Price_Rent[,c("Country","Q4-2014", "Q4-2015", "Q4-2016", "Q4-2017")]
#4-1-2- rename columns 
names(Price_Rent1)[c(2,3,4,5)]=c("2014","2015","2016", "2017")

#4-1-3 Create Indicator Ratio (2017)/ (Mean of all available years)
Price_Rent1$PR_Mean=rowMeans(Price_Rent1[,2:5], na.rm = TRUE)
Price_Rent1$P_Rent=Price_Rent1$`2017`/Price_Rent1$PR_Mean 
View(Price_Rent1)
#Indicator 'P_Rent has been created
#4-2- Import Dataset #2
Price_Income<-read_xlsx("C:/Users/Cherin/Desktop/R/PIncome.xlsx")
#4-2-1 Take subset that contains all rows and three columns (end of year)
Price_Income1<-Price_Income[,c("Country", "Q4-2014","Q4-2015", "Q4-2016", "Q4-2017")]
#4-2-2 rename columes
names(Price_Income1)[c(2,3,4,5)]=c("2014","2015","2016", "2017")
View(Price_Income1)
#4-2-3 Create Indicator Price to Income Ratio (2017)/ (Mean of all available years)
Price_Income1$MeanP=rowMeans(Price_Income1[,2:5], na.rm = TRUE)
#Note: Class of variables 2015, 2016, 2017 were characters. So I forced them into numeric
Price_Income1$`2015`=as.numeric(Price_Income1$`2015`)
Price_Income1$`2016`=as.numeric(Price_Income1$`2016`)
Price_Income1$`2017`=as.numeric(Price_Income1$`2017`)
Price_Income1$MeanP=rowMeans(Price_Income1[,2:5], na.rm = TRUE)
Price_Income1$P_Income=Price_Income1$`2017`/Price_Income1$MeanP
View(Price_Income1)
#Indicator 'P_Income has been created
#5- Import Dataset #3
Credit<-read_xlsx("C:/Users/Cherin/Desktop/R/Credit.xlsx")
View(Credit)
#Note This dataset contains monthly data
# 5-1- Take subset to make it annual data (1-1-YYYY = 31-12-YYYY(-1))
Credit1<-subset(Credit, Month ==1, select= c('Credit', 'Year'));
Credit=Credit1$Credit
Year=Credit1$Year-1
Credit2<-data.frame(Credit,Year)
View(Credit2)
# 5-2 Calculate credit expansion (annual % change) 
# Note Could not figure out how to calculate it in R (so I did it on excel)
# export file to excel and calculate growth rate
#Then import newdataset (including growth rate) to R
write_xlsx(Credit2, "C:/Users/Cherin/Desktop/R/Credit2.xlsx")
#calculate growth rate in excel and save as credit3
Credit3<- read_xlsx("C:/Users/Cherin/Desktop/R/Credit3.xlsx")
# 6- Import dataset#4
Housing<- read_xlsx("C:/Users/Cherin/Desktop/R/Housing.xlsx")
House_P<-Housing [Housing$Country== "CAN",c("Country", 'Year','Cost')]
# Cost Growth is needed. I export the dataset to excel, calculate Cost.G and import newdataset to R
write_xlsx(House_P,  "C:/Users/Cherin/Desktop/R/House_P.xlsx")
House_P1<- read_xlsx("C:/Users/Cherin/Desktop/R/House_p1.xlsx")
View(House_P1)
#7- Import dataset#5
GDP<-read_xlsx("C:/Users/Cherin/Desktop/R/GDP.xlsx")
# 8- Merge Credit3 and GDP then add colum that calculate Credit%G to GDP%G ratio
Credit_GDP=merge(x=Credit3, y=GDP, by="Year")
# 8-1 Credit/GDP ratio: Note# Credit is in Million CAD, GDP is in Billion CAD
# 8-1-1- standardize unit (Credit/1000)
Credit_GDP$CR_Bn<-Credit_GDP$Credit/1000
#8-1-2- Create ratios as a new columns in dataframe
Credit_GDP$CR_GDP<-(Credit_GDP$CR_Bn/Credit_GDP$GDP)*100
Credit_GDP$CRG_GDPG<-Credit_GDP$`Credit%G`/Credit_GDP$`GDP%G
View(Credit_GDP)
#9-Merge Credit_GDP and House_P1
mydata<-merge(Credit_GDP,House_P1)
View(mydata)
#9- Take a subset of last three years to create indicators
mydata1<-mydata[26:28, c('Year','CR_GDP',"CRG_GDP_G","Cost%G")]
View(mydata1)
#9-1 import variables(2indicators) from 2 dataframes "Price_Income1", "P_Rent1"
mydata1$P_Income<-Price_Income1$P_Income[Price_Income1$Country=='Canada']
mydata1$P_Rent<-Price_Rent1$P_Rent[Price_Rent1$Country=='Canada']
#9-2 calculate indicators (average 3 years of CRG_GDP_G,) (average 3 years of house price growth)
mydata1$AVG_CRG_GDPG<-mean(mydata1$CRG_GDP_G)
mydata1$AVG_House_PG<-mean(mydata1$`Cost%G`)
View(mydata1)
#10- create subset with for 2017 indicators
Canada<-mydata1[3,c('CR_GDP','AVG_CRG_GDPG','AVG_House_PG','P_Income','P_Rent')]
View(Canada)
#11- How does each indicator compare to the threshold in the assessment frameowrk?
##plot plot each indicator with its threshold 
barplot(c(Canada$AVG_CRG_GDPG, Risk_Ind$Threshold[2]), main = "Credit Expansion to GDP Growth Ratio: Threshold<2.5%", xlab = "", ylab="%",col=c("blue", "Red"), legend= c('Canada', 'Threshold'))
barplot(c(Canada$AVG_House_PG, Risk_Ind$Threshold[3]), main = "Credit Expansion to GDP Growth Ratio: Threshold<2.5%", xlab = "", ylab="%",col=c("blue", "Red"), legend= c('Canada', 'Threshold'))
barplot(c(Canada$AVG_House_PG, Risk_Ind$Threshold[3]), main = " Avg House Price Growth (3Y): Threshold<3%", xlab = "", ylab="%",col=c("blue", "Red"), legend= c('Canada', 'Threshold'))
barplot(c(Canada$P_Income, Risk_Ind$Threshold[3]), main = " Avg Price to Income (3Y): Threshold<20%", xlab = "", ylab="%",col=c("blue", "Red"), legend= c('Canada', 'Threshold'))
barplot(c(Canada$P_Rent, Risk_Ind$Threshold[3]), main = " Avg Price to Rent (3Y): Threshold<20%", xlab = "", ylab="%",col=c("blue", "Red"), legend= c('Canada', 'Threshold'))

#12- How can the risk assessment criteria be applied to the indicators?
##Create argument for housing market risk assessment
I1= Canada$CR_GDP
I2=Canada$AVG_CRG_GDPG
I3=Canada$AVG_House_PG
I4=Canada$P_Income
I5=Canada$P_Rent
if ((I2>2.5) &(I3>3.5)) {
  print(" Moderate Risk Zone")
} else 
  if ((I4>20)& (I5>20) & (I1<130)) {
    print(" Moderate Risk Zone")
  }else if ((I2<2.5) &(I3<3.5) & (I4<20)& (I5<20) & (I1<130)) {
    print("Low Risk Zone")
  }else if ((I2>2.5)& (I3>3.5)){
    print("High Risk Zone")
  } else if ((I4>20)& (I5>20) & (I1>130)){
    print("High Risk Zone")
  } else if ((I2<2.5)&(I3<3.5)&(I4<20) &(I5<20)& (I1>130)){
    print("Highly Leveraged Household")
  }
#13- What is the trend in the house prices? 
# Plot Historical House Price Index
plot(x=mydata$Year, y=mydata$Cost, type='l', xlab='Year', ylab = "House Price", main = 'Hous Price Index')
abline(v=2008, col='blue')