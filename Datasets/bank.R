bank<- read.csv("/Users/AdamLiu/Code/Machine_Learning/Datasets/Kaggle/bank-failures.csv")
mac<- read.csv("/Users/AdamLiu/Code/Machine_Learning/Datasets/Kaggle/macdonalds_menu.csv")

bank$Failure.Date = as.Date(bank$Failure.Date, format='%m/%d/%Y')

names(bank)

# [1] "Financial.Institution.Number" "Institution.Name"             "Institution.Type"
# [4] "Charter.Type"                 "Headquarters"                 "Failure.Date"
# [7] "Insurance.Fund"               "Certificate.Number"           "Transaction.Type"
# [10] "Total.Deposits"               "Total.Assets"                 "Estimated.Loss..2015."

range(table(bank$Failure.Date))
range(bank$Failure.Date)
table(bank$Headquarters)

barplot(table(bank$Insurance.Fund))

as.data.frame(table(bank$Insurance.Fund))



library(ggplot2)
ggplot(as.data.frame(table(bank$Insurance.Fund)), aes(x = factor(Var1), y = Freq)) +
    geom_bar(stat="identity") +
    geom_text(aes(label = Freq), position = "stack", vjust = 1) +
    theme_bw()

ggplot(bank, aes(x = Failure.Date)) + geom_density()










