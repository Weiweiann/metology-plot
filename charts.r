# df----
df = read.csv("data.csv", header=TRUE)
ColW = c("#FF9F1C","#FFBF69","#FFFFFF","#CBF3F0","#2EC4B6")
ColW = c("#FF9F1C","#FFB24F","#FFBF69","#CBF3F0","#2EC4B6","#124F48")
ColColerful = c("#FFBE0B","#FB5607","#FF006E","#8338EC","#3A86FF")
ColBright = c("#FFFFFF","#CEA892","#FFE74C","#FF5964","#35A7FF")
Col1 = c("#068D9D","#53599A","#6D9DC5","#80DED9","#AEECEF")
Col = c("#006BA6","#0496FF","#FFBC42","#D81159","#8F2D56")
Col9 = c("#1A535C","#4ECDC4","#F7FFF7","#FF6B6B","#FFE66D",
         "#309776","#5AE693","#FF6B45","#FFAD73")
Col5B = c("#13293D","#006494","#247BA0","#1B98E0","#E8F1F2")

library(sjPlot)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(spatstat)
Sys.setenv("plotly_username"="l.w.jasons")
Sys.setenv("plotly_api_key"="n75fpddg16")
# 職業人數 ----
df$Job = mergeLevels(df$Job, "其他"=c("已退休", "農林漁牧", "待業中"))
levels(df$Job)[levels(df$Job)=="工業(製造、營造、水電)"] <- "營造業"
x <- data.frame(Category=df$Job, 
                Frequency=rep(1, length(df$Job)))
a <- aggregate(Frequency ~ Category, x, sum)

# ggplot 
#g <- ggplot(df, aes(x=reorder(Job,Job,FUN=length)))+ geom_bar(fill="steelblue3") 
#g + theme_fivethirtyeight()+ coord_flip()
#sjp.setTheme(theme = theme_light())
#ggplot(df, aes(Job, fill=Job))+  geom_bar()

# sjp 
#sjp.frq(df$Job, axisLimits.y = c(0,150), geom.colors="steelblue3", printPlot = FALSE)$plot+theme_fivethirtyeight()
#sjp.frq(df$Job, axisLimits.y = c(0,150),geom.colors="steelblue3", printPlot = FALSE, coord.flip = TRUE)$plot+theme_fivethirtyeight()
# pie 
Col9 = c("#FF9F1C","#FF6B6B","#FB5607","#00BD9D","#2EC4B6",
         "#3A86FF","#8338EC","#7F68FF","#FFBE0B")
col = colorRampPalette(ColW)(length(a$Frequency))
plot_ly(a, type="pie",values=a$Frequency,
        labels=a$Category,
        textposition="outside",
        marker=list(colors=col))

#bp <- ggplot(a, aes(x="", y = Frequency, fill=Category))+
#  geom_bar(width=1, stat="identity")
#bp + coord_polar("y", start=0) + scale_fill_brewer(palette="Paired")


# 扣掉學生 ----
df <- df[-which(df$Job == "學生"),] 

# 年齡 -----
x <- data.frame(Category=df$Age, 
                Frequency=rep(1, length(df$Age)))
a <- aggregate(Frequency ~ Category, x, sum)

ggplot(df, aes(x = Age, fill=Income)) + geom_bar(position = "fill") + theme_fivethirtyeight() +
  scale_fill_brewer() +xlab("年齡")+ylab("比例") +
  theme(legend.position="right", legend.direction="vertical")


col = colorRampPalette(ColColerful)(length(a$Frequency))
t <- list(
  family = "sans serif",
  size = 18,
  color = toRGB("black")
)
t2 <- list(
  family = "sans serif",
  size = 10,
  color = toRGB("black")
)
p = plot_ly(a, type="pie",values=a$Frequency, sort=FALSE,
        labels=a$Category,
        textposition="auto",
        textinfo="label+value+percent",
        outsidetextfont = t2,
        insidetextfont=t,
        marker=list(colors=col))
p
plotly_POST(p, filename = "年齡")
# 健康檢查的結果，是否能夠準確代表您的身體健康狀態？ ----
x <- data.frame(Category=df$HCQuality, 
                Frequency=rep(1, length(df$HCQuality)))
a <- aggregate(Frequency ~ Category, x, sum)
a$Percent <- a$Frequency/sum(a$Frequency) * 100

col = colorRampPalette(Col5B)(length(a$Frequency))
plot_ly(a, type="pie",values=a$Frequency, sort=FALSE,
        labels=a$Category,
        textposition="outside",
        marker=list(colors=col))

col = colorRampPalette(brewer.pal(9,"PuBu"))(length(a$Frequency))
ggplot(a, aes(x="", y=a$Percent,fill=factor(Category))) +
  geom_bar(stat='identity') + theme_fivethirtyeight() + scale_fill_manual(values=col)+ theme(legend.position="none")+
  coord_flip()

# 大數據分析來提供您更加個人化的健檢項目，會增加您自費健檢的意願嗎? ----
x <- data.frame(Category=df$BigdataWilling, 
                Frequency=rep(1, length(levels(factor(df$BigdataWilling)))))
a <- aggregate(Frequency ~ Category, x, sum)
a$Percent <- a$Frequency/sum(a$Frequency) * 100

col = colorRampPalette(ColColerful)(length(a$Frequency))
t <- list(
  family = "sans serif",
  size = 18,
  color = toRGB("black")
)
t2 <- list(
  family = "sans serif",
  size = 10,
  color = toRGB("black")
)
p = plot_ly(a, type="pie",values=a$Frequency, sort=FALSE,
            labels=a$Category,
            textposition="auto",
            textinfo="label+percent",
            outsidetextfont = t2,
            insidetextfont=t,
            marker=list(colors=col))
p

col = colorRampPalette(brewer.pal(9,"PuBu"))(length(a$Frequency))
ggplot(a, aes(x="", y=a$Percent,fill=factor(Category))) + 
  geom_bar(stat='identity') + theme_fivethirtyeight() + scale_fill_manual(name="評分",values=col)+
  coord_flip()

# 收入意願比較 ----
ggplot(df, aes(x = Income, fill=WillingToPay)) + geom_bar(position = "fill") + theme_fivethirtyeight() +
  scale_fill_brewer() +xlab("年齡")+ylab("比例") +
theme(legend.position="right", legend.direction="vertical")



df2 = df
df2 = df2[df2$WillingToPay!="0元",]
df2 = df2[df2$WillingToPay!="0元",]
data = data.frame(SelfEstimate = df2$SelfEstimate,
                  Income = df2$Income,
                  WillingToPay = df2$WillingToPay,
                  BigdataPayment = df2$BigdataPayment,
                  Frequency=rep(1, length(df2$WillingToPay)))

a <- aggregate(Frequency ~ WillingToPay+BigdataPayment+Income , data, sum)
col = brewer.pal(11,"RdYlBu")
p =plot_ly(a, color=a$Income, size = a$Frequency,
           z=a$Income, x=a$WillingToPay, y = a$BigdataPayment,type="scatter3d",
           mode="markers",colors=col) 
p