library(xlsx)
data = read.csv('./data.csv',header = T,encoding = "UTF-8", stringsAsFactors = T)

# rm ourselves and unimportant columns

# rm student
data_no_student <- data[-which(data[,4] == "學生"),] 
data_no_student

library(ggplot2)

colnames(data_no_student)
# variables
sex = data_no_student[,2]
age = data_no_student[,3]
job = data_no_student[,4]
income = factor(data_no_student[,5], levels = c('10萬以下','10-50萬', '50-100萬', '100-200萬', '200萬-500萬','500萬以上'))
self_estimate = data_no_student[,6]
HC_times = data_no_student[,7]
HC_money = data_no_student[,8]
willing_pay = factor(data_no_student[,9], levels = c('0元','5000元以下','5000~10000元', '10000~15000元', '15000~30000元','30000元以上'))
HC_quality = data_no_student[,10]
bigdata_willing = as.numeric(as.character(data_no_student[,11]))
bigata_pay = factor(data_no_student[,12],levels = c('0元','5000元以下','5000~10000元', '10000~15000元', '15000~30000元','30000元以上'))
diseases = data_no_student[,13:25]
## comment = data[,26]; comment = comment[-which(comment == "")]

havewilling_idx <- which(bigdata_willing >= 3)
x = data.frame(BigdataPay = bigata_pay, Willing = bigdata_willing, Pay= willing_pay)
x_melt = melt(x, id=c("Willing"))

tmp = c('0元','5000元以下','5000~10000元', '10000~15000元', '15000~30000元','30000元以上')

x_melt_sorted = vector()
for(t in tmp){
  x_melt_sorted <- rbind(x_melt_sorted, x_melt[which(x_melt[,3] == t), ])
}

x_melt[,3] <- factor(x_melt[,3], levels = c('0元','5000元以下','5000~10000元', '10000~15000元', '15000~30000元','30000元以上'))



library("ggthemes")
p1 = ggplot(x_melt, aes(x = value, fill=variable)) + geom_bar(position = position_dodge()) + facet_wrap(~ Willing, nrow = 5) +
  scale_fill_manual(values = c("#70C1B3", "#F3FFBD"), labels = c('Metology建議健檢後所額外願意花的費用', '之前健檢所花費用'), guide = guide_legend(title = NULL)) + 
  theme_fivethirtyeight(base_family = 'SimSun',base_size = 18)  + xlab("使用意願") + ylab("數量") + ggtitle("使用Metology意願與花費比較")
p1


x <- data.frame(Wills=willing_pay, Income = income)

p1 = ggplot(x, aes(x = Income, fill=Wills)) + geom_bar(position = "fill") +
  scale_fill_brewer(palette="GnBu", guide = guide_legend(title = NULL)) + 
  theme_fivethirtyeight(base_family = 'SimSun',base_size = 18)  + xlab("收入") + ylab("比例") + ggtitle("收入與健檢花費意願比較")
p1





