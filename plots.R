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
Sys.setenv("plotly_username"="l.w.jasons")
Sys.setenv("plotly_api_key"="n75fpddg16")

# 健康檢查的結果，是否能夠準確代表您的身體健康狀態？
col = colorRampPalette(brewer.pal(9,"PuBu"))(length(levels(HC_quality)))
x <- data.frame(Category=factor(sort(as.numeric(HC_quality))), 
                Frequency=as.integer(HC_quality))
a <- aggregate(Frequency ~ Category, x, sum)

m = list(
  l = 100,
  r = 40,
  b = 20,
  t = 100,
  pad = 0
)
p = plot_ly(a, type="pie",values=a$Frequency,sort = FALSE,
        labels=paste(a$Category, "分", sep=""),
        textposition="outside",marker=list(colors=col) ) %>%
  layout(title = "健康檢查的結果，是否能夠準確代表您的身體健康狀態？",margin = m)
p

plotly_POST(p, filename = "健康檢查的結果，是否能夠準確代表您的身體健康狀態？(Pie chart)")

# 大數據分析來提供您更加個人化的健檢項目，會增加您自費健檢的意願嗎?
x <- data.frame(Category=bigdata_willing, 
                Frequency=as.integer(bigdata_willing))
a <- aggregate(Frequency ~ Category, x, sum)
plot_ly(a,type="bar", y =a$Frequency, x = paste(a$Category, "分", sep="")) %>%
  layout(
    title = "使用大數據分析來提供您更加個人化的健檢項目，會增加您自費健檢的意願嗎?",
    xaxis = list(title=""),
    yaxis = list(title="")
  )

m = list(
  l = 100,
  r = 40,
  b = 20,
  t = 100,
  pad = 0
)
col = colorRampPalette(brewer.pal(9,"PuBu"))(length(levels(bigdata_willing)))
p = plot_ly(a, type="pie",values=a$Frequency,sort = FALSE,
            labels=paste(a$Category, "分", sep=""),
            textposition="outside",marker=list(colors=col) ) %>%
  layout(title = "使用大數據分析來提供您更加個人化的健檢項目，會增加您自費健檢的意願嗎?",margin = m)
p
plotly_POST(p, filename = "大數據分析(Pie chart)")

col = colorRampPalette(brewer.pal(9,"PuBu"))(length(levels(self_estimate)))
data_no_blank = data_no_student[data_no_student[,6]!="",]
data = data.frame(SelfEstimate = factor(as.integer(data_no_blank[,6])),
                  Age = data_no_blank[,3],
                  BigdataWilling = data_no_blank[,11])
data = data[order(data$SelfEstimate),]

p = plot_ly(data,color=paste(data$SelfEstimate, "分", sep=""), x=data$BigdataWilling, type="box") %>%
  layout(title = "自身健康狀態vs使用大數據分析是否會增加您自費健檢的意願",
         xaxis =list(title="使用大數據分析是否會增加您自費健檢的意願"))
p
plotly_POST(p, filename = "自身健康狀態vs大數據分析(Box chart)")

data = data.frame(Age = data_no_student[,3],
                  BigdataWilling = data_no_student[,11])
p = plot_ly(data,color=data$Age, x=data$BigdataWilling, type="box") %>%
  layout(title = "年齡vs使用大數據分析是否會增加您自費健檢的意願",
         xaxis =list(title="使用大數據分析是否會增加您自費健檢的意願"))
p
plotly_POST(p, filename = "年齡vs大數據分析(Box chart)")

# 願意花多少錢在進階健檢?
x <- data.frame(Category=willing_pay, 
                Frequency=rep(1, length(willing_pay)))
a <- aggregate(Frequency ~ Category, x, sum)
col = brewer.pal(6,"Pastel2")
p = plot_ly(a, type="pie",values=a$Frequency, sort=FALSE,
            labels=a$Category,
            textposition="outside",marker=list(colors=col) ) %>%
  layout(title = "願意花多少錢在進階健檢?",margin = m)
p
plotly_POST(p, filename = "願意花多少錢在進階健檢?(Pie chart)")

# 職業分布
x <- data.frame(Category=job, 
                Frequency=rep(1, length(job)))
a <- aggregate(Frequency ~ Category, x, sum)
col = brewer.pal(11,"Set3")
p = plot_ly(a, type="pie",values=a$Frequency, sort=FALSE,
            labels=a$Category,
            textposition="outside",marker=list(colors=col) ) %>%
  layout(title = "職業分布",margin = m)
p
plotly_POST(p, filename = "職業分布(Pie chart)")


data = data.frame(SelfEstimate = factor(as.integer(data_no_student[,6])),
                  WillingPay = willing_pay,
                  BigdataPay = data_no_student[,12],
                  Frequency=rep(1, length(willing_pay)))
data = data[data$WillingPay!="0元",]
data = data[data$BigdataPay!="0元",]
a <- aggregate(Frequency ~ WillingPay, data, sum)
col = brewer.pal(11,"RdYlBu")

g = ggplot(data, aes(x=WillingPay, y=BigdataPay)) + geom_point()
p = ggplotly(g)
plotly_POST(p, filename = "test")
plot_ly(data, color=as.integer(data$SelfEstimate),
        z=as.integer(data$SelfEstimate), x=data$WillingPay, y = data$BigdataPay,type="scatter3d",
        mode="markers",colors=col) 
 p
