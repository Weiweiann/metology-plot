library(xlsx)
data = read.xlsx('./Metology-report.xlsx',sheetIndex = 1,encoding = "UTF-8")

# rm ourselves and unimportant columns
data = data[c(-1:-8),c(-1,-28:-30)]

income = factor(data[,5], levels = c('10萬以下','10-50萬', '50-100萬', '100-200萬', '200萬-500萬','500萬以上'))
HC_times = factor(data[,7], levels = c('0次', '1-5次', '6-10次', '10次以上'))
data[,8] = droplevels(data[,8])
HC_money = factor(data[,8], levels = c('無','0元', '5000元以下', '5000~10000元', '10000~15000元', '15000~30000元'))
willing_pay = factor(data[,9], levels = c('0元','5000元以下','5000~10000元', '10000~15000元', '15000~30000元','30000元以上'))
data[,12] = droplevels(data[,12])
bigdata_pay = factor(data[,12], levels = c('0元', '5000元以下', '5000~10000元', '10000~15000元', '15000~30000元','30000元以上'))
df = data.frame( Sex = data[,1],
                 Age = data[,3],
                 Job = data[,4],
                 Income = income,
                 SelfEstimate = as.integer(data[,6]),
                 HCTimes = HC_times,
                 HCPayment = HC_money,
                 WillingToPay = willing_pay,
                 HCQuality = factor(as.integer(data[,10])),
                 BigdataWilling = data[,11],
                 BigdataPayment = bigdata_pay,
                 Diseases = data[,13:25])

#Merge some levles.
library(spatstat)
df$HCPayment = mergeLevels(df$HCPayment, "0元"=c("無", "0元"))

#output
write.csv(df, file="data.csv")

# validate
df2 = read.csv("data.csv", header=TRUE)
