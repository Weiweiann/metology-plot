library(xlsx)
data = read.xlsx('./Metology-report.xlsx',sheetIndex = 1,encoding = "UTF-8")

# rm ourselves and unimportant columns
data = data[c(-1:-7),c(-1,-28:-30)]

# rm student
data_no_student <- data[-which(data[,4] == "學生"),] 
data_no_student

library(ggplot2)

colnames(data_no_student)
# variables
sex = data_no_student[,1]
age = data_no_student[,3]
job = data_no_student[,4]
income = factor(data_no_student[,5], levels = c('10萬以下','10-50萬', '50-100萬', '100-200萬', '200萬-500萬','500萬以上'))
self_estimate = data_no_student[,6]
HC_times = data_no_student[,7]
HC_money = data_no_student[,8]
willing_pay = factor(data_no_student[,9], levels = c('0元','5000元以下','5000~10000元', '10000~15000元', '15000~30000元','30000元以上'))
HC_quality = data_no_student[,10]
bigdata_willing = data_no_student[,11]
bidata_pay = data_no_student[,12]
diseases = data_no_student[,13:25]
comment = data[,26]; comment = comment[-which(comment == "")]


cor(as.numeric(willing_pay), as.numeric(age))
table(data_no_student[,9])

income_age_data = data.frame('Income'=income, 'Age'=age)
ggplot(income_age_data, aes(x = Age, fill=income)) + geom_dotplot(stackgroups = T, binpositions = 'all', ) + coord_flip()
ggplot(income_age_data, aes(x = Age, fill=income)) + geom_bar(position = "fill") + scale_fill_brewer() + theme(text = element_text(family = 'SimSun',size = 18))+xlab("年齡")+ylab("比例")


library(plotly)
library(RColorBrewer)


col = colorRampPalette(brewer.pal(9,"PuBu"))(length(levels(HC_quality)))
x <- data.frame(Category=factor(sort(as.numeric(HC_quality))), 
                Frequency=as.integer(HC_quality))
a <- aggregate(Frequency ~ Category, x, sum)

p = plot_ly(a, type="pie",values=a$Frequency,
        labels=paste(a$Category, "分", sep=""),
        textposition="outside",marker=list(colors=col) ) %>%
  layout(title = "對於健檢的滿意度評分")
p
Sys.setenv("plotly_username"="l.w.jasons")
Sys.setenv("plotly_api_key"="n75fpddg16")
plotly_POST(p, filename = "對於健檢的滿意度評分(Pie chart)")

