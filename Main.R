options(scipen=10, digits=4)


dat = lapply(dat, as.factor)
dat = data.frame(dat)


data.bone = dat

data.bone$Group = ifelse(
  data.bone$Bone == "Yes" | 
    data.bone$Brain == "Yes" | 
    data.bone$Lung == "Yes" |
    data.bone$Liver == "Yes", "Yes", "No")

data.bone$Bone =data.bone$Brain = data.bone$Lung = data.bone$Liver = NULL
# data.bone$Br = data.bone$bone = data.bone$lung = data.bone$`Distant LN` = NULL
data.bone$Time = data.bone$Status = NULL

# bone.u = Logistic(data.bone, type = "u")
# bone.u$sig = ifelse(
#   bone.u$`p-value` < 0.001,
#   "***",
#   ifelse(
#     bone.u$`p-value` < 0.01,
#     "**",
#     ifelse(
#       bone.u$`p-value` < 0.05,
#       "*",
#       ""
#     )
#   )
# )
# data.bone$Race = data.bone$M = NULL

data.bone$M = NULL
wistu.website.r.tools::LoadFunc("RandomForest")
bone.RF = RandomForest(data.bone)
bone.RF$pic.imp

wistu.website.r.tools::LoadFunc("SVM_RFE")
d = data.bone %>% 
  lapply(as.factor) %>% 
  data.frame() %>% 
  lapply(as.numeric) %>%
  data.frame() 
bone.SVM = SVM_RFE(d, sizes = 1:length(colnames(d)) - 1)

venn::venn(list(RandomForest = bone.RF$result$Item, "SVM-RFE" = bone.SVM), ilabels = TRUE, zcolor = "style", )
dt = c(bone.RF$result$Item, bone.SVM)[duplicated(c(bone.RF$result$Item, bone.SVM))]

MLogistic <<- function(data, covariates = NULL){
  group = data$Group
  data$Group = NULL
  if (is.null(covariates)) {
    covariates = colnames(data)
  }
  data$Group = as.factor(group)
  exp = parse(text = paste0("glm(Group~`",paste0(covariates,collapse="`+`") , "`, data = data, family = 'binomial')"))
  result = eval(exp)
  
  print("多因素Logistic结果：")
  print(result)
  print(summary(result))
  print("开始逐步回归：")
  
  logit.step <- step(result, direction="both")
  print("逐步回归结果：")
  print(logit.step)
  print(summary(logit.step))
  
  return(list(Logistic = result, Logistic.Step = logit.step))
}

re = MLogistic(data.bone, covariates = dt)
dt = dt[dt!="Stage"]

# No必须为0，Yes为1
data.bone$Group = ifelse(data.bone$Group == "No", 0, 1)

re = MLogistic(data.bone, covariates = dt)
summary(re)
p <- predict(re$Logistic.Step, type='response')

library("ggplot2")
library("rms")

qplot(sort(p),col="response")


NomoLogistic <<- function(data, covariates = NULL){
  group = data$Group
  data$Group = NULL
  if (is.null(covariates)) {
    covariates = colnames(data)
  }
  data$Group = as.factor(group)
  exp = parse(text = paste0("lrm(Group~`",paste0(covariates,collapse="`+`") , "`, data = data, x = T, y = T)"))
  result = eval(exp)
  
  dd = datadist(result)
  options(datadist=dd) 
  
  nom1 <- nomogram(result,
                 fun=plogis,
                 lp=F,
                 fun.at = c(0.1,0.3,0.5,0.7,0.9),
                 funlabel = "Risk")
  plot(nom1)
  
  
  cal1 <- calibrate(v, cmethod='hare', method='boot', B=100,data=data.bone)#建模组中绘制校准曲线
  plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0))#打印出校准曲线
}

NomoLogistic(data.bone)

v = lrm(Group ~`Chemotherapy`+`Age`+`T`+`Radiation`+`N`, data = data.bone, x = T, y = T)

dd = datadist(data.bone)
options(datadist=dd) 

nom1<-nomogram(v,
               fun=plogis,
               lp=F,
               fun.at = c(0.1,0.3,0.5,0.7,0.9),
               funlabel = "Risk")
plot(nom1)


cal1 <- calibrate(v, cmethod='hare', method='boot', B = 1000,data=data.bone)#建模组中绘制校准曲线
plot(cal1)#打印出校准曲线




# glm绘制森林图
fit.full = re$Logistic.Step
fit.result<-summary(fit.full)
library(tableone)
fit.OR = ShowRegTable(fit.full)
df1<-fit.result$coefficients
df2<-confint(fit.full)
df3<-cbind(df1,df2)
df4<-data.frame(df3[-1,c(1,4,5,6)])
df4$Var<-rownames(df4)
colnames(df4)<-c("OR","Pvalue","OR_1","OR_2","Var")
df5<-df4[,c(5,1,2,3,4)]
df5$OR_mean<-df5$OR
df5$OR<-paste0(round(df5$OR,2),
               "(",
               round(df5$OR_1,2),
               "~",
               round(df5$OR_2,2),
               ")")
df5$Pvalue<-round(df5$Pvalue,3)

write.csv(df5, "fr1.csv")
fit.OR

library(forestplot)
fp = read.csv("fr.csv", header = T)
forestplot(labeltext=as.matrix(fp[,1:4]),
           mean=fp$OR_mean,
           lower=fp$OR_1,
           upper=fp$OR_2,
           zero=1,
           boxsize=0.2,
           lineheight = unit(9,'mm'),
           colgap=unit(5,'mm'),
           lwd.zero=1.5,
           lwd.ci=2, 
           col=fpColors(box='#458B00',
                        summary='#8B008B',
                        lines = 'black',
                        zero = '#7AC5CD'),
           xlab="OR",
           graphwidth=unit(60,"mm"),
           lwd.xaxis =1,
           txt_gp = fpTxtGp(ticks = gpar(cex = 0.85),
                            xlab  = gpar(cex = 0.8),
                            cex = 0.9),
           lty.ci = "solid",
           title = "Forestplot", 
           line.margin = 0.08,
           graph.pos=3)






library(readxl)
val <- read_excel("Table S1.xlsx")


val$Lung = ifelse(val$Lung == "Unknow", "No", val$Lung)
val$Liver = ifelse(val$Liver == "Unknow", "No", val$Liver)
val$Bone = ifelse(val$Bone == "Unknow", "No", val$Bone)
val$Brain = ifelse(val$Brain == "Unknow", "No", val$Brain)



data.val = val
data.val$Time = data.val$Time.M = data.val$Status = NULL

data.val = lapply(data.val, as.factor)
data.val = data.frame(data.val)

data.val$Group = data.val$Metasis
data.val$Lung = data.val$Liver = data.val$Bone = data.val$Brain = data.val$Metasis = NULL

data.val = data.frame(data.val)


valpro<-predict(object =v,type = "fitted",newdata = data.val )#根据建模组方程，计算验证组预测概率

fit.vad<-lrm(Group~valpro,data=data.val,x=T,y=T)#验证组根据预测概率建立回归方程

fit.vad#展示fit.vad方程的参数

dd = datadist(valpro)
options(datadist='dd')

summary(fit.vad)

cal2 <- calibrate(fit.vad, cmethod='hare', method='boot', B=150,data=data.val)#验证组中绘制校准曲线
plot(cal2)#打印出校准曲线




library(readxl)
val <- read_excel("Table S1.xlsx")
val$Time = val$Time.M
val$Group = val$Metasis
val = subset(val, val$Time.M != 0)

wistu.website.r.tools::LoadFunc("Plot.KM")
val$Time = as.numeric(val$Time)

val$Group = as.factor(val$Group)

val$Time = as.numeric(val$Time)
val$Status = as.numeric(val$Status)
val$Status = ifelse(val$Status == 1, 0 ,1)

val$Time = val$Time / 12

Plot.KM(val)

dat.os = data.frame(
  Time = dat$Time,
  Status = dat$Status,
  Group = ifelse(
    dat$Bone == "Yes" | 
      dat$Brain == "Yes" | 
      dat$Lung == "Yes" |
      dat$Liver == "Yes", "Yes", "No")
)
dat.os$Time = as.numeric(dat.os$Time)
dat.os$Status = ifelse(dat.os$Status == "Dead", 1, 0)

dat.os$Time = dat.os$Time / 12
Plot.KM(dat.os)



ROC()
#
library(ResourceSelection)
hl <- hoslem.test(data.bone$Group,fitted(re$Logistic.Step),g=10)
hl
# 绘制ROC
pr.e <- predict(re$Logistic.Step, data = data.bone, type = c("response"))#得出预测概率

library(pROC)
modelroc <- roc(data.bone$Group, pr.e)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
         grid.col=c("green", "red"), max.auc.polygon=TRUE,
         auc.polygon.col="skyblue", print.thres = TRUE)


pr <- predict(re$Logistic.Step, newdata = data.val,type = c("response"))
modelroc <- roc(data.val$Group, pr)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres = TRUE)



dat$Time = as.numeric(dat$Time)


# LASSO
dat.os = dat
dat.os$Group = ifelse(
  dat.os$Bone == "Yes" | 
    dat.os$Brain == "Yes" | 
    dat.os$Lung == "Yes" |
    dat.os$Liver == "Yes", "Yes", "No")
dat.os = subset(dat.os, dat.os$Group == "Yes")
dat.os$Group = NULL
dat.os$Status = ifelse(dat.os$Status == "Dead", 1 , 0)

wistu.website.r.tools::LoadFunc("LASSO")
result.lasso = LASSO(dat.os)

# 单因素Cox与多因素Cox
dat.os$T = dat.os$M = NULL
wistu.website.r.tools::LoadFunc("Cox")
result.cox = Cox(dat.os, type = "mix", plot = T)


# 查看肝转移
dat.os$Group = dat.os$Liver
wistu.website.r.tools::LoadFunc("Plot.KM")
dat.os$Time = dat.os$Time / 12
# val$Time = as.numeric(val$Time)
Plot.KM(dat.os)


dat.nomo = dat.os
time = dat.nomo$Time
status = dat.nomo$Status
dat.nomo$Time = dat.nomo$Status = NULL

library(survival)
library(rms)

dd = datadist(dat.nomo)
options(datadist='dd')

cox = cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,x=T,y=T,data=dat.nomo,surv=T)
surv <- Survival(cox) # 建立生存函数

dat.nomo$lp =  predict(cox, type = "lp")

library(survivalROC)
nobs <- NROW(dat.nomo)
roc <- survivalROC(
  Stime = time, ## 生存时间
  status = status, ## 终止事件
  marker = dat.nomo$lp, ## marker value
  predict.time = 12, ## 预测1、3、5年
  # lambda=0.05,
  span = 0.01*nobs^(-0.20),
  method = "KM")
# plot(roc$FP, roc$TP,
#      type = "l", col = "green", xlim = c(0, 1), ylim = c(0, 1),
#      xlab = "False positive rate",
#      ylab = "True positive rate",
#      thresholds="best",
#      print.thres="best")
# library(ggplotify)
# # p <- ggplot_build(p)
# abline(a = 0, b = 1, lty = 3)
# abline(a = 0, b = 1, lty = 3)

roc3 = survivalROC(
  Stime = time, ## 生存时间
  status = status, ## 终止事件
  marker = dat.nomo$lp, ## marker value
  predict.time = 2*12, ## 预测近5年
  # lambda=0.05,
  span = 0.01*nobs^(-0.20),
  method = "KM")
# plot = plot +  ggplot_build(roc3)
# plot = plot + 
# lines(roc3$FP, roc3$TP, type="l",col="yellow",xlim=c(0,1), ylim=c(0,1))

roc5 = survivalROC(
  Stime = time, ## 生存时间
  status = status, ## 终止事件
  marker = dat.nomo$lp, ## marker value
  predict.time = 3*12, ## 预测近5年
  # lambda=0.05,
  span = 0.01*nobs^(-0.20),
  method = "KM")
# plot = plot + ggplot_build(roc5)
# plot = plot + ggplot_build(lines(roc5$FP, roc5$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1)))
# lines(roc5$FP, roc5$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1))
# p = p + lines(roc5$FP, roc5$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1))
# lines(roc3$FP, roc3$TP, type="l",col="yellow",xlim=c(0,1), ylim=c(0,1))


df_plot <- data.frame(tpr = as.numeric(roc$TP),
                      fpr = as.numeric(roc$FP))
df_plot2 <- data.frame(tpr = as.numeric(roc3$TP),
                       fpr = as.numeric(roc3$FP))
df_plot5 <- data.frame(tpr = as.numeric(roc5$TP),
                       fpr = as.numeric(roc5$FP))
df_plot$year = c(rep("1-year"))
df_plot2$year = c(rep("2-year"))
df_plot5$year = c(rep("3-year"))

df_plot = rbind(df_plot, df_plot2)
df_plot = rbind(df_plot, df_plot5)

df_plot = data.frame(df_plot)

#roc[["AUC"]][1]
# roc可以一次性做出所有时间

p <- ggplot(df_plot, aes(fpr, tpr, color = year)) +
  geom_smooth(se=FALSE, size=1.2)+ # 这就是平滑曲线的关键
  geom_abline(slope = 1, intercept = 0, color = "grey10",linetype = 2) +
  scale_color_manual(values = c("#E41A1C","#377EB8","#4DAF4A"),
                     name = NULL, 
                     labels = c(paste0("AUC at 1 year: ",round(roc[["AUC"]][1],2)), 
                                paste0("AUC at 2 year: ",round(roc3[["AUC"]][1],2)), 
                                paste0("AUC at 3 year: ",round(roc5[["AUC"]][1],2)))
  ) + 
  coord_fixed(ratio = 1) +
  labs(x = "Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(legend.position = c(0.7,0.15), 
        panel.border = element_rect(fill = NA),
        axis.text = element_text(color = "black"))

p






# nomo
surv1 <- function(x)surv(1*12,lp=x)
surv2 <- function(x)surv(2*12,lp=x)
surv3 <- function(x)surv(3*12,lp=x)

nom = nomogram(cox,
         fun=list(surv1,surv2,surv3),
         lp= F,
         funlabel=c('1-Year survival','2-Years survival','3-Years survival'),
         maxscale=100,
         fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1')
         )
plot(nom)








coxm_1 <- cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,data=dat.nomo,surv=T,x=T,y=T,time.inc = 12)
cal_1<-calibrate(coxm_1,u=12,cmethod='KM',m=300,B=1000)
##绘制1年生存期校准曲线
par(mar=c(7,4,4,3),cex=1.0)
plot(cal_1,lwd=2,lty=1, ##设置线条形状和尺寸
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), ##设置一个颜色
     xlab='Nomogram-Predicted Probability of 1-year OS',#便签
     ylab='Actual 1-year OS(proportion)',#标签
     col=c(rgb(192,98,83,maxColorValue = 255)),#设置一个颜色
     xlim = c(0,1),ylim = c(0,1)) ##x轴和y轴范围

##绘制2年生存期校曲线
##time.in 和 u 要是一样的，都是要评价的时间节点
coxm_2 <- cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,data=dat.nomo,surv=T,x=T,y=T,time.inc = 24)
cal_2<-calibrate(coxm_2,u=2*12,cmethod='KM',m=300,B=1000)
plot(cal_2,lwd=2,lty=1,  ##设置线条宽度和线条类型
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), ##设置一个颜色
     xlab='Nomogram-Predicted Probability of 2-year OS',#便签
     ylab='Actual 2-year OS(proportion)',#标签
     col=c(rgb(192,98,83,maxColorValue = 255)),#设置一个颜色
     xlim = c(0,1),ylim = c(0,1)) ##x轴和y轴范围



coxm_5 <- cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,data=dat.nomo,surv=T,x=T,y=T,time.inc = 3*12)
cal_5<-calibrate(coxm_5,u=3*12,cmethod='KM',m=300,B=1000)
plot(cal_5,lwd=2,lty=1,  ##设置线条宽度和线条类型
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), ##设置一个颜色
     xlab='Nomogram-Predicted Probability of 3-year OS',#便签
     ylab='Actual 3-year OS(proportion)',#标签
     col=c(rgb(192,98,83,maxColorValue = 255)),#设置一个颜色
     xlim = c(0,1),ylim = c(0,1)) ##x轴和y轴范围


















val.os = val
val.os$Time = val.os$Time.M
val.os$Status = as.character(val.os$Status)
val.os = subset(val.os, val.os$Time != 0)

time = val.os$Time

val.os = lapply(val.os, as.factor)
val.os = data.frame(val.os)

status = val.os$Status
val.os$Time = val.os$Status = NULL

library(rms)
dd = datadist(val.os)
options(datadist='dd')

predict(cox, newdata = val.os)


ROC <- timeROC(T = time,   
               delta = status,   
               marker = val.os$lp,   
               cause = 1,                
               weighting = "marginal",   
               times = c(1, 2, 3),       
               iid = TRUE)

# ROC
library(survivalROC)
nobs <- NROW(val.os)
roc <- survivalROC(
  Stime = time, ## 生存时间
  status = status, ## 终止事件
  marker = val.os$lp, ## marker value
  predict.time = 12, ## 预测1、3、5年
  # lambda=0.05,
  span = 0.01*nobs^(-0.20),
  method = "KM")
# plot(roc$FP, roc$TP,
#      type = "l", col = "green", xlim = c(0, 1), ylim = c(0, 1),
#      xlab = "False positive rate",
#      ylab = "True positive rate",
#      thresholds="best",
#      print.thres="best")
# library(ggplotify)
# # p <- ggplot_build(p)
# abline(a = 0, b = 1, lty = 3)
# abline(a = 0, b = 1, lty = 3)

roc3 = survivalROC(
  Stime = time, ## 生存时间
  status = status, ## 终止事件
  marker = val.os$lp, ## marker value
  predict.time = 2*12, ## 预测近5年
  # lambda=0.05,
  span = 0.01*nobs^(-0.20),
  method = "KM")
# plot = plot +  ggplot_build(roc3)
# plot = plot + 
# lines(roc3$FP, roc3$TP, type="l",col="yellow",xlim=c(0,1), ylim=c(0,1))

roc5 = survivalROC(
  Stime = time, ## 生存时间
  status = status, ## 终止事件
  marker = val.os$lp, ## marker value
  predict.time = 3*12, ## 预测近5年
  # lambda=0.05,
  span = 0.01*nobs^(-0.20),
  method = "KM")
# plot = plot + ggplot_build(roc5)
# plot = plot + ggplot_build(lines(roc5$FP, roc5$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1)))
# lines(roc5$FP, roc5$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1))
# p = p + lines(roc5$FP, roc5$TP, type="l",col="red",xlim=c(0,1), ylim=c(0,1))
# lines(roc3$FP, roc3$TP, type="l",col="yellow",xlim=c(0,1), ylim=c(0,1))


df_plot <- data.frame(tpr = as.numeric(roc$TP),
                      fpr = as.numeric(roc$FP))
df_plot2 <- data.frame(tpr = as.numeric(roc3$TP),
                      fpr = as.numeric(roc3$FP))
df_plot5 <- data.frame(tpr = as.numeric(roc5$TP),
                      fpr = as.numeric(roc5$FP))
df_plot$year = c(rep("1-year"))
df_plot2$year = c(rep("2-year"))
df_plot5$year = c(rep("3-year"))

df_plot = rbind(df_plot, df_plot2)
df_plot = rbind(df_plot, df_plot5)

df_plot = data.frame(df_plot)

#roc[["AUC"]][1]
# roc可以一次性做出所有时间

p <- ggplot(df_plot, aes(fpr, tpr, color = year)) +
  geom_smooth(se=FALSE, size=1.2)+ # 这就是平滑曲线的关键
  geom_abline(slope = 1, intercept = 0, color = "grey10",linetype = 2) +
  scale_color_manual(values = c("#E41A1C","#377EB8","#4DAF4A"),
                     name = NULL, 
                     labels = c(paste0("AUC at 1 year: ",round(roc[["AUC"]][1],2)), 
                                paste0("AUC at 2 year: ",round(roc3[["AUC"]][1],2)), 
                                paste0("AUC at 3 year: ",round(roc5[["AUC"]][1],2)))
  ) + 
  coord_fixed(ratio = 1) +
  labs(x = "Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(legend.position = c(0.7,0.15), 
        panel.border = element_rect(fill = NA),
        axis.text = element_text(color = "black"))

p









cutoff_1 <- roc$cut.values[which.max(roc$TP-roc$FP)]
y <- roc$TP[roc$cut.values==cutoff_1]
x <- roc$FP[roc$cut.values==cutoff_1]
points(x = x, y = y, col="green", pch = 19)

cutoff_3 <- roc3$cut.values[which.max(roc3$TP-roc3$FP)]
y <- roc3$TP[roc3$cut.values==cutoff_3]
x <- roc3$FP[roc3$cut.values==cutoff_3]
points(x = x, y = y, col="yellow", pch = 19)

cutoff_5 <- roc5$cut.values[which.max(roc5$TP-roc5$FP)]
y <- roc5$TP[roc5$cut.values==cutoff_5]
x <- roc5$FP[roc5$cut.values==cutoff_5]
points(x = x, y = y, col="red", pch = 19)


cat(paste("AUC of 1 year =",round(roc$AUC, 3), ", Cutoff = ",cutoff_1 ,"\n"))
cat(paste("AUC of 3 years =",round(roc3$AUC,3), ", Cutoff = ",cutoff_3 ,"\n"))
cat(paste("AUC of 5 years =",round(roc5$AUC,3), ", Cutoff = ",cutoff_5 ,"\n"))

p = p + legend(0.6,0.2,c(paste("AUC of 1 year =\t",round(roc$AUC, 3)),
                         paste("AUC of 3 years =",round(roc3$AUC,3)),
                         paste("AUC of 5 years =",round(roc5$AUC,3))),
               x.intersp=1, y.intersp=0.8,
               lty= 1 ,lwd= 2,
               col=c("green","yellow","red"),
               bty = "n",# bty框的类型
               seg.len=1,cex=0.8)# 





time = as.numeric(time)
status = as.numeric(status)
coxm_1 <- cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,data=val.os,surv=T,x=T,y=T,time.inc = 12)
cal_1<-calibrate(coxm_1,u=12,cmethod='KM',m=30,B=1000)
##绘制1年生存期校准曲线
par(mar=c(7,4,4,3),cex=1.0)
plot(cal_1,lwd=2,lty=1, ##设置线条形状和尺寸
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), ##设置一个颜色
     xlab='Nomogram-Predicted Probability of 1-year OS',#便签
     ylab='Actual 1-year OS(proportion)',#标签
     col=c(rgb(192,98,83,maxColorValue = 255)),#设置一个颜色
     xlim = c(0,1),ylim = c(0,1)) ##x轴和y轴范围

##绘制2年生存期校曲线
##time.in 和 u 要是一样的，都是要评价的时间节点
coxm_2 <- cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,data=val.os,surv=T,x=T,y=T,time.inc = 24)
cal_2<-calibrate(coxm_2,u=24,cmethod='KM',m=30,B=1000)
plot(cal_2,lwd=2,lty=1,  ##设置线条宽度和线条类型
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), ##设置一个颜色
     xlab='Nomogram-Predicted Probability of 2-year OS',#便签
     ylab='Actual 2-year OS(proportion)',#标签
     col=c(rgb(192,98,83,maxColorValue = 255)),#设置一个颜色
     xlim = c(0,1),ylim = c(0,1)) ##x轴和y轴范围



coxm_5 <- cph(Surv(time,status)~Age + Grade + Bone + Brain + Lung + Radiation + Chemotherapy,data=val.os,surv=T,x=T,y=T,time.inc = 36)
cal_5<-calibrate(coxm_5,u=36,cmethod='KM',m=30,B=1000)
plot(cal_5,lwd=2,lty=1,  ##设置线条宽度和线条类型
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), ##设置一个颜色
     xlab='Nomogram-Predicted Probability of 3-year OS',#便签
     ylab='Actual 3-year OS(proportion)',#标签
     col=c(rgb(192,98,83,maxColorValue = 255)),#设置一个颜色
     xlim = c(0,1),ylim = c(0,1)) ##x轴和y轴范围







# plot = plot +  ggplot_build(pl)