setwd("C:/Users/mavul/Downloads/Online_Retail.csv")
library(dplyr)
library(readxl)
library(readr)
retail <- read.csv("C:/Users/mavul/Downloads/Online_Retail.csv")
colnames(retail)
nrow(retail)


# 1. Show the breakdown of the number of transactions by countries 

retail %>% group_by(Country) %>% 
  summarise(transactions = n(), percentage = (transactions/541909)*100 ) %>% 
  filter(percentage>1)

# 2. Create a new variable 'TransactionValue' that is the product of the exising 'Quantity' and 'UnitPrice' variables. 

retail["TransactionValue"] <- retail$Quantity* retail$UnitPrice
View(retail)

# 3. Using the newly created variable, TransactionValue, show the breakdown of transaction values by countries 

retail %>% group_by(Country) %>% summarise(total=sum(TransactionValue))
Transactionexceeding <-retail %>% 
  group_by(Country) %>% summarise(total=sum(TransactionValue)) %>% 
  filter(total>130000)
View(Transactionexceeding)

# 4. converting InvoiceDate into a POSIXlt object:
  
Temp=strptime(retail$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')
retail$New_Invoice_Date <- as.Date(Temp)
retail$New_Invoice_Date[20000]- retail$New_Invoice_Date[10]
retail$Invoice_Day_Week= weekdays(retail$New_Invoice_Date)
retail$New_Invoice_Hour = as.numeric(format(Temp, "%H"))
retail$New_Invoice_Month = as.numeric(format(Temp, "%m"))

# a) Show the percentage of transactions (by numbers) by days of the week 

retail %>% group_by(Invoice_Day_Week) %>% 
  summarise(count=n())%>% mutate(percentage= (count/nrow(retail)*100))

# b) Show the percentage of transactions (by transaction volume) by days of the week

retail %>% group_by(Invoice_Day_Week)%>%
  summarise(total=sum(TransactionValue))%>%mutate(percentage=total/sum(total)*100)

# c) Show the percentage of transactions (by transaction volume) by month of the year

retail %>% group_by(New_Invoice_Month)%>%
  summarise(total=sum(TransactionValue))%>%mutate(percentage=total/sum(total)*100)

# d) What was the date with the highest number of transactions from Australia? 
# By observing the tibble, we can get that the maximum no.of transactions from Australia was 139  on 2011-06-15.

retail %>% group_by(New_Invoice_Date) %>% 
  filter(Country == "Australia")  %>% tally(sort= TRUE) 

# e) The company needs to shut down the website for two consecutive hours for maintenance. What would be the hour of the day to start this so that the distribution is at minimum for the customers? The responsible IT team is available from 7:00 to 20:00 every day. 

retail %>% 
  filter(New_Invoice_Hour>= 7 & New_Invoice_Hour<=20) %>% group_by(New_Invoice_Hour) %>% 
  tally(sort = TRUE) %>% arrange(n) 

# By observing the table, the 19th , 20th are the two consecutive hours which has the lowest sum of two consecutive hours.

# 5. Plot the histogram of transaction values from Germany. Use the hist() function to plot.

retail%>% filter(Country == "Germany") %>% 
  summary(total = sum(TransactionValue))-> Germany
hist(x=(retail$TransactionValue[retail$Country=="Germany"]),xlab = " TransactionValue",main = 'Germany Transactions',ylab = ' Frequency')

# 6. Which customer had the highest number of transactions? Which customer is most valuable (i.e. highest total sum of transactions)?
  
retail %>% group_by(CustomerID) %>% tally(sort = TRUE) %>% 
  filter(!is.na(CustomerID)) %>% filter(n==max(n))
retail%>% group_by(CustomerID) %>% summarise(highesttotalsumoftransactions = sum(TransactionValue))%>% 
  arrange(desc(highesttotalsumoftransactions))%>% filter(CustomerID != "NA")%>%
  filter(highesttotalsumoftransactions ==max(highesttotalsumoftransactions) )

# 7. Calculate the percentage of missing values for each variable in the dataset.

Missingvalues <- colMeans(is.na(retail)*100)
View(Missingvalues)

# 8. What are the number of transactions with missing CustomerID records by countries?

nrow(retail[is.na(retail$CustomerID),])
retail[is.na(retail$CustomerID),] %>% group_by(Country) %>% 
  summarise(missingcustomerID = n())

# 9. On average, how often the costumers comeback to the website for their next shopping? 

retail%>% group_by(CustomerID)%>% 
  summarise(avg_no_of_days= diff(New_Invoice_Date)) %>% 
  filter(avg_no_of_days>0)
mean(retail$avg_no_of_days)

# 10. what is the return rate for the French customers? 

return_val<-nrow(retail%>% group_by(CustomerID)%>% 
      filter((Country=='France')&(TransactionValue<0)&(CustomerID != 'Na')))
total_french_customer<-nrow(retail%>% group_by(CustomerID)%>% 
      filter((Country=='France')&(CustomerID != 'Na')))
print(paste('Return rate for french customer is',((return_val)/(total_french_customer))*100,'%'))

# 11. What is the product that has generated the highest revenue for the retailer? 

retail %>% group_by(Description) %>% 
  summarise(total=sum(TransactionValue)) %>% filter(total == max(total))

# 12. How many unique customers are represented in the dataset? You can use unique() and length() functions.

length(unique(retail$CustomerID))
