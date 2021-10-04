
df<- read.csv("D:/Courses/Data Analytics/HW4/dataset_hw3_analytics_v2.csv")


#Get the summary of GPA
colnames(df)


#Creating a data set with only numeric
df_num <- subset(df, select=c( "Highschool.GPA", "Area.of.Study",
                               "Section.5.Exam", "Section.6.Exam",
                              "Section.7.Exam","Section.8.Exam", "Section.9.Exam",
                              "Section.10.Exam","Section.11.Exam","Section.12.Exam",
                              "Section.13.Exam","Section.14.Exam","Section.15.Exam",
                              "Section.16.Exam","Section.17.Exam","Section.18.Exam",
                              "Section.19.Exam","Section.20.Exam","Section.21.Exam",
                              "Section.22.Exam", "Final.GPA"))

df_num <- subset(df_num, Area.of.Study == "Engineering")


#Get the names of the columns
colnames(df_num)
length(df_num$Final.GPA)

#Remove outliers of GPA
Q <- quantile(df_num$Final.GPA, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_num$Final.GPA)
df_num_wo<- subset(df_num, df_num$Final.GPA > (Q[1] - 1.5*iqr) & df_num$Final.GPA < (Q[2]+1.5*iqr))

length(df_num_wo$Final.GPA)
length(df_num_wo$Highschool.GPA)
colnames(df_num_wo)

#Correlation
a <- cor(df_num_wo$Highschool.GPA, df_num_wo$Final.GPA, method = c("spearman"))
a

#Normality Test
final_gpa = sample(c(df_num_wo$Final.GPA), size = 4000)
shapiro.test(final_gpa)

#Histogram
hist(df_num$Final.GPA, xlab = "Final GPA",col = "gray",border = "black")
hist(df_num_wo$Final.GPA, xlab = "Final GPA",col = "gray",border = "black")

#Boxplot
boxplot(df_num$Final.GPA, main="Boxplot of Final GPA")
boxplot(df_num_wo$Final.GPA, main="Boxplot of Final GPA")

#** CORRELATION MAP!!!!
cormat <- round(cor(df_num_wo, method = c("spearman")),2)
head(cormat)

library(reshape2)
library(ggplot2)



# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "gray20", high = "gray20", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#New excerise but for ANOVA test, but using Kruskal-wallis test
#install.packages("tidyverse")
#install.packages("ggpubr")
#install.packages("rstatix")

library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)


#Creating a data set with only numeric
df_test <- subset(df_num_wo, select=c("Final.GPA", "Area.of.Study"))

df_test$Area.of.Study <- ordered(df_test$Area.of.Study,
                           levels = c("Engineering", "Business", "Creative Studies",
                                      "Health", "Social Sciences", "Built Environment"))

group_by(df_test, Area.of.Study) %>%
  summarise(
    count = n(),
    mean = mean(Final.GPA, na.rm = TRUE),
    sd = sd(Final.GPA, na.rm = TRUE),
    median = median(Final.GPA, na.rm = TRUE),
    IQR = IQR(Final.GPA, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(df_test, x = "Area.of.Study", y = "Final.GPA", 
          color = "Area.of.Study", palette = c("gray60", "gray50", "gray40", "gray30",
                                         "gray20", "gray10"),
          order = c("Engineering", "Business", "Creative Studies",
                    "Health", "Social Sciences", "Built Environment"),
          ylab = "Final GPA", xlab = "Area of Study")

colnames(df_test)
length(df_test$Final.GPA)

#ONE way anova test
# Compute the analysis of variance
res.aov <- aov(Final.GPA ~ Area.of.Study, data = df_test)

# Summary of the analysis
summary(res.aov)

#Which groups are different between each other
TukeyHSD(res.aov)


#test of ANOVA for non-parametric variables
kruskal.test(Final.GPA ~ Area.of.Study, data = df_test)

#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

#which mean are different?
pairwise.wilcox.test(df_test$Final.GPA, df_test$Area.of.Study,
                     p.adjust.method = "BH")







#Checking linear visual linear relationships
plot(df_num_wo$Highschool.GPA, df_num_wo$Final.GPA, xlab = "Highschool GPA", ylab= "Final GPA", main="Scatter plot")
plot(df_num$Puntaje.Examen, df_num$Promedio.Final, xlab = "PAA score", ylab= "Final GPA")
plot(df_num$Puntaje.Seccion.5.Examen, df_num$Promedio.Final, xlab = "Section 5", ylab= "Final GPA")
plot(df_num$Puntaje.Seccion.6.Examen, df_num$Promedio.Final, xlab = "Section 6", ylab= "Final GPA")
plot(df_num$Puntaje.Seccion.14.Examen, df_num$Promedio.Final, xlab = "Section 14", ylab= "Final GPA")
plot(df_num$Puntaje.Seccion.17.Examen, df_num$Promedio.Final, xlab = "Section 17", ylab= "Final GPA")



gpa_high = sample(c(df_num$CalPromedioPrepa), size = 4000)
shapiro.test(gpa_high)
paa_score = sample(c(df_num$Puntaje.Examen), size = 3000)
shapiro.test(paa_score)
sec_5 = sample(c(df_num$Puntaje.Seccion.5.Examen), size = 4000)
shapiro.test(sec_5)
sec_6 = sample(c(df_num$Puntaje.Seccion.6.Examen), size = 4000)
shapiro.test(sec_6)
sec_14 = sample(c(df_num$Puntaje.Seccion.14.Examen), size = 4000)
shapiro.test(sec_14)
sec_17 = sample(c(df_num$Puntaje.Seccion.17.Examen), size = 4000)
shapiro.test(sec_17)
final_gpa = sample(c(df_num$Promedio.Final), size = 4000)
shapiro.test(final_gpa)

#Histogram

plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

#Histograms
plotn(df_num$CalPromedioPrepa,main="Histogram of Highschool GPA", xlab = "Highschool GPA", ylab ="Frequency")#Grafico de x
plotn(df_num$Puntaje.Examen,main="Histogram of PAA Score", xlab = "PAA Score", ylab ="Frequency")#Grafico de x
plotn(df_num$Puntaje.Seccion.5.Examen,main="Histogram of Section 5", xlab = "Section 5", ylab ="Frequency")#Grafico de x
plotn(df_num$Puntaje.Seccion.6.Examen,main="Histogram of Section 6", xlab = "Section 6", ylab ="Frequency")#Grafico de x
plotn(df_num$Puntaje.Seccion.14.Examen,main="Histogram of Section 14", xlab = "Section 14", ylab ="Frequency")#Grafico de x
plotn(df_num$Puntaje.Seccion.17.Examen,main="Histogram of Section 17", xlab = "Section 17", ylab ="Frequency")#Grafico de x
plotn(df_num$Promedio.Final,main="Histogram of Final GPA", xlab = "Final GPA", ylab ="Frequency")#Grafico de x
plotn(df_num$Edad,main="Histogram of Age", xlab = "Age", ylab ="Frequency")#Grafico de x
plotn(df_num$Total.Uni.Inscritas,main="Histogram of Credits taken", xlab = "Credits", ylab ="Frequency")#Grafico de x


#Boxplot
boxplot(df_num$CalPromedioPrepa, main="Boxplot of Highschool GPA")
boxplot(df_num$Puntaje.Examen, main="Boxplot of PAA Score")
boxplot(df_num$Puntaje.Seccion.5.Examen, main="Boxplot of Section 5")
boxplot(df_num$Puntaje.Seccion.6.Examen, main="Boxplot of Section 6")
boxplot(df_num$Puntaje.Seccion.14.Examen, main="Boxplot of Section 14")
boxplot(df_num$Puntaje.Seccion.17.Examen, main="Boxplot of Section 17")
boxplot(df_num$Promedio.Final, main="Boxplot of Final GPA")


#Barplot
barplot(table(df$Entrada), main="Areas of Study",
        xlab="") 
barplot(table(df$Foraneo), main="Foreign Student",
        xlab="") 
barplot(table(df$Desc.Genero), main="Gender",
        xlab="")
barplot(table(df$Becado), main="Scholarship",
        xlab="")


#Getting a Simple Linear Relationship*************************
#Get data set only for ingenieria
df_ing <- subset(df, Entrada == "Ingeniería")
#Remove catergorical variables
df_ing$Entrada <- NULL
df_ing$Foraneo <- NULL
df_ing$Desc.Genero <- NULL
df_ing$Becado <- NULL
df_ing$Edad <- NULL

colnames(df_ing)
#Remove outliers of GPA
hist(df_ing$Promedio.Final)
boxplot(df_ing$Promedio.Final)

#79, as limit of outliers, then remove the outliers, without outliers
Q <- quantile(df_ing$Promedio.Final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_ing$Promedio.Final)
df_ing_wo<- subset(df_ing, df_ing$Promedio.Final > (Q[1] - 1.5*iqr) & df_ing$Promedio.Final < (Q[2]+1.5*iqr))

length(df_ing$Promedio.Final)
length(df_ing_wo$Promedio.Final)

boxplot(df_ing_wo$Promedio.Final)

#Get the linear between the sec 14 and final gpa

library(ggplot2)
#stat_smooth draws a line in the scatter plot
ggplot(df_ing_wo, aes(x = Puntaje.Seccion.14.Examen, y = Promedio.Final)) + geom_point() + stat_smooth(method = lm)

cor(df_ing_wo$Puntaje.Seccion.14.Examen, df_ing_wo$Promedio.Final)

model <- lm(Final.GPA~Highschool.GPA, data = df_num_wo)

summary(model)

##*******************************Evaluation of the model
##The Validation set Approach
##Split the data into training and test set 
library(caret)
set.seed(123)
training.samples <- df_ing_wo$Promedio.Final %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data <- df_ing_wo[training.samples,]
test.data <- df_ing_wo[-training.samples,]

#build the model
model <- lm(Promedio.Final~Puntaje.Seccion.14.Examen, data = train.data)

#Make the predictions
predictions <- model %>% predict(test.data)

data.frame(R2 = R2 (predictions, test.data$Promedio.Final),
           RMSE = RMSE (predictions, test.data$Promedio.Final),
           MAE = MAE(predictions, test.data$Promedio.Final))

RMSE(predictions, test.data$Promedio.Final)/mean(test.data$Promedio.Final)


#******Leave one out cross validation
#Define training control
train.control <- trainControl(method = 'LOOCV')

#Train the model
model <- train(Promedio.Final~Puntaje.Seccion.14.Examen, data = df_ing_wo, method = "lm",
               trControl = train.control)

print(model)


#******K-fold Cross Validation
#Define training control
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)

#Train the model
model <- train(Promedio.Final~Puntaje.Seccion.14.Examen, data = df_ing_wo, method = "lm",
               trControl = train.control)
print(model)


#******Repeated K-fold cross-validation
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Train the model 
model <- train(Promedio.Final~Puntaje.Seccion.14.Examen, data = df_ing_wo, method = "lm",
               trControl = train.control)

#Summarize the results
print(model)

#********************************Multilinear Regression for all
df_all <- df
#Remove catergorical variables
df_all$Entrada <- NULL
df_all$Foraneo <- NULL
df_all$Desc.Genero <- NULL
df_all$Becado <- NULL
df_all$Edad <- NULL

#79, as limit of outliers, then remove the outliers, without outliers
Q <- quantile(df_all$Promedio.Final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_all$Promedio.Final)
df_all_wo<- subset(df_all, df_all$Promedio.Final > (Q[1] - 1.5*iqr) & df_all$Promedio.Final < (Q[2]+1.5*iqr))

#Get the Multiple Regression with All the variables
model.all.entradas <- lm(Promedio.Final~CalPromedioPrepa + Puntaje.Examen +
                  Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
                  Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
                  Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
                  Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
                  Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
                  Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
                , data = df_all_wo)

summary(model.all.entradas)

#Using Stepwise backwards -> 
model.all.entradas.back <- step(model.all.entradas, direction="backward", trace=0)
##-> Calprepa, sec14, sec5, sec15, sec16, sec17, sec18, sec21
summary(model.all.entradas.back)

hist(df_all$Promedio.Final)
hist(df_all_wo$Promedio.Final)

length(df_all$Promedio.Final)
length(df_all_wo$Promedio.Final)
min(df_all_wo$Promedio.Final)

#**********************Multilinear Regression Ingenieria
#Get data set only for ingenieria
df_in <- subset(df, Entrada == "Ingeniería")

#Print the names of the columns
colnames(df_in)


#Remove outliers of GPA
Q <- quantile(df_in$Promedio.Final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_in$Promedio.Final)
df_in_wo<- subset(df_in, df_in$Promedio.Final > (Q[1] - 1.5*iqr) & df_in$Promedio.Final < (Q[2]+1.5*iqr))
#df_in_wo<- subset(df_in, df_in$Promedio.Final > 85)

#Check the length
length(df_in$Promedio.Final)
length(df_in_wo$Promedio.Final)

#Check the boxplot
#boxplot(df_in$Promedio.Final)
#boxplot(df_in_wo$Promedio.Final)

#Get the Multiple Regression with All the variables
model.all <- lm(Promedio.Final~CalPromedioPrepa+ Puntaje.Examen +
              Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
              Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
              Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
              Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
              Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
              Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
            , data = df_in_wo)

summary(model.all)

#Using Stepwise backwards -> this is better 0.23 adj r2
model.back <- step(model.all, direction="backward", trace=0)
##-> results
summary(model.back)#*******

#Using Stepwise forward
model.minimal <- lm(Promedio.Final ~ 1, data=df_in_wo)
summary(model.minimal)
model.forw <- step(model.minimal, scope=list(upper = model.all,lower= model.minimal), 
     direction="forward",trace=0)
##-> results
summary(model.forw)

#Using Stepwise backwards and forward
model.both <- step(model.minimal, scope=list(upper = model.all,lower= model.minimal), 
     direction="both",trace=0)
##-> results
summary(model.both)


#******Repeated K-fold cross-validation for Ingenieria

library(caret)
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Train the model 
model <- train(Promedio.Final~CalPromedioPrepa+ Total.Uni.Inscritas + Puntaje.Examen +
                 Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
                 Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
                 Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
                 Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
                 Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
                 Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
               , data = df_in_wo, method = "lm",
               trControl = train.control)

#Summarize the results
print(model)

#**********************Multilinear Regression for Negocios
#Get data set only for ingenieria
df_ng <- subset(df, Entrada == "Negocios")
#Remove catergorical variables
df_ng$Entrada <- NULL
df_ng$Foraneo <- NULL
df_ng$Desc.Genero <- NULL
df_ng$Becado <- NULL
df_ng$Edad <- NULL

colnames(df_ng)
#Remove outliers of GPA
hist(df_ng$Promedio.Final)
boxplot(df_ng$Promedio.Final)

#79, as limit of outliers, then remove the outliers, without outliers
Q <- quantile(df_ng$Promedio.Final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_ng$Promedio.Final)
df_ng_wo<- subset(df_ng, df_ng$Promedio.Final > (Q[1] - 1.5*iqr) & df_ng$Promedio.Final < (Q[2]+1.5*iqr))

length(df_ng$Promedio.Final)
length(df_ng_wo$Promedio.Final)

boxplot(df_ng_wo$Promedio.Final)

#Get the Multiple Regression with All the variables
model <- lm(Promedio.Final~CalPromedioPrepa + Puntaje.Examen +
              Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
              Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
              Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
              Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
              Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
              Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
            , data = df_ng_wo)

summary(model)

#Using Stepwise backwards -> this is better 0.23 adj r2
model.back.generic <- step(model, direction="backward", trace=0)
##-> Calprepa, sec14, sec5, sec15, sec16, sec17, sec18, sec21
summary(model.back.generic)

#******Repeated K-fold cross-validation for Negocios
library(caret)
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Train the model 
model <- train(Promedio.Final~CalPromedioPrepa+ Total.Uni.Inscritas + Puntaje.Examen +
                 Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
                 Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
                 Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
                 Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
                 Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
                 Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
               , data = df_ng_wo, method = "lm",
               trControl = train.control)

#Summarize the results
print(model)


#######*******************ANOVA TEST FOR PUNTAJE EXAMEN VS ENTRADAS
#New excerise but for ANOVA test, but using Kruskal-wallis test
#install.packages("tidyverse")
#install.packages("ggpubr")
#install.packages("rstatix")

library(tidyverse)
library(ggpubr)
library(rstatix)

#Creating a data set with only numeric
df_test <- subset(df, select=c("Puntaje.Examen", "Entrada"))

df_test$Entrada <- ordered(df_test$Entrada,
                           levels = c("Ingeniería", "Negocios", "Estudios Creativos",
                                      "Salud", "Ciencias Sociales", "Ambiente Construido"))

library(dplyr)
group_by(df_test, Entrada) %>%
  summarise(
    count = n(),
    mean = mean(Puntaje.Examen, na.rm = TRUE),
    sd = sd(Puntaje.Examen, na.rm = TRUE),
    median = median(Puntaje.Examen, na.rm = TRUE),
    IQR = IQR(Puntaje.Examen, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(df_test, x = "Entrada", y = "Puntaje.Examen", 
          color = "Entrada", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FF00FF",
                                         "#0000FF", "#800080"),
          order = c("Ingeniería", "Negocios", "Estudios Creativos",
                    "Salud", "Ciencias Sociales", "Ambiente Construido"),
          ylab = "Puntaje Examen", xlab = "Exploratory Stage")

#test
kruskal.test(Puntaje.Examen ~ Entrada, data = df_test)

#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

#which mean are different?
pairwise.wilcox.test(df_test$Puntaje.Examen, df_test$Entrada,
                     p.adjust.method = "BH")

##****Logistic Regression for Ingeniería (data set w0 in prom final)
df <- read.csv("C:/Users/Milara/Desktop/Tec21/dataset_wo_hw8_analytics_v1_logisticRegression.csv")
df_ing <- subset(df, Entrada == "Ingeniería")


mean(df_ing$Promedio.Final)

#1st model
mylogit <- glm(Ab_En_Mean ~  Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + 
                 Puntaje.Seccion.16.Examen + 
                 Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + 
                 CalPromedioPrepa  , data = df_ing, family = "binomial")
summary(mylogit)

#2nd model with all the variables
mylogit_2 <- glm(Ab_En_Mean ~ CalPromedioPrepa+ Puntaje.Examen +
                   Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
                   Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
                   Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
                   Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
                   Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
                   Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
                 , data = df_ing, family = "binomial")
summary(mylogit_2)


#**Evaluation of  1st model 
glm.probs <- predict(mylogit,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, 1.0, 0.0)
xtab <- table(glm.pred,df_ing$Ab_En_Mean)
mean(glm.pred == df_ing$Ab_En_Mean)

library(caret)
confusionMatrix(xtab)

#**Evaluation of 2nd model  
glm.probs <- predict(mylogit_2,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, 1.0, 0.0)
table(glm.pred,df_ing$Ab_En_Mean)
mean(glm.pred == df_ing$Ab_En_Mean)

##*********Logistic Regression for Areas (data set w0 in prom final)
df <- read.csv("C:/Users/Milara/Desktop/Tec21/dataset_wo_hw8_analytics_v1_logisticRegression.csv")
df_ing <- subset(df, Entrada == "Negocios")


mean(df_ing$Promedio.Final)

#1st model
mylogit <- glm(Ab_En_Mean ~  CalPromedioPrepa+ Puntaje.Seccion.7.Examen +
                 Puntaje.Seccion.13.Examen + Puntaje.Seccion.14.Examen +
                 Puntaje.Seccion.19.Examen + Puntaje.Seccion.20.Examen 
               , data = df_ing, family = "binomial")
summary(mylogit)

#2nd model with all the variables
mylogit_2 <- glm(Ab_En_Mean ~ CalPromedioPrepa+ Puntaje.Examen +
                   Puntaje.Seccion.5.Examen + Puntaje.Seccion.6.Examen + Puntaje.Seccion.7.Examen +
                   Puntaje.Seccion.8.Examen + Puntaje.Seccion.9.Examen + Puntaje.Seccion.10.Examen +
                   Puntaje.Seccion.11.Examen + Puntaje.Seccion.12.Examen + Puntaje.Seccion.13.Examen +
                   Puntaje.Seccion.14.Examen + Puntaje.Seccion.15.Examen + Puntaje.Seccion.16.Examen +
                   Puntaje.Seccion.17.Examen + Puntaje.Seccion.18.Examen + Puntaje.Seccion.19.Examen +
                   Puntaje.Seccion.20.Examen + Puntaje.Seccion.21.Examen + Puntaje.Seccion.22.Examen
                 , data = df_ing, family = "binomial")
summary(mylogit_2)

barplot(df_ing$Ab_En_Mean)

#**Evaluation of  1st model 
glm.probs <- predict(mylogit,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, 1.0, 0.0)
table(glm.pred,df_ing$Ab_En_Mean)
mean(glm.pred == df_ing$Ab_En_Mean)


#**Evaluation of 2nd model  
glm.probs <- predict(mylogit_2,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, 1.0, 0.0)
table(glm.pred,df_ing$Ab_En_Mean)
mean(glm.pred == df_ing$Ab_En_Mean)

#**************Code to maintain for sintaxis but by itself had a bad objective
#*Get a random sample of 5 studens from each entrada
#*Make the predictions
#*Get the mean of the predictions
#Report

#separate the dataframe
df_neg <- subset(df, Entrada == "Negocios")
df_est <- subset(df, Entrada == "Estudios Creativos")
df_sal <- subset(df, Entrada == "Salud")
df_cie <- subset(df, Entrada == "Ciencias Sociales")
df_amb <- subset(df, Entrada == "Ambiente Construido")

# Get a random sample of 5 studens from each entrada
s_ing <- df_ing[sample(1:nrow(df_ing), 10, replace=FALSE),] 
s_neg <- df_neg[sample(1:nrow(df_neg), 10, replace=FALSE),] 
s_est <- df_est[sample(1:nrow(df_est), 10, replace=FALSE),]
s_sal <- df_sal[sample(1:nrow(df_sal), 10, replace=FALSE),]
s_cie <- df_cie[sample(1:nrow(df_cie), 10, replace=FALSE),]
s_amb <- df_amb[sample(1:nrow(df_amb), 10, replace=FALSE),]

#Make the predictions
s_ing.probs <- predict(mylogit,newdata = s_ing, type="response")
mean(s_ing.probs)
s_neg.probs <- predict(mylogit,newdata = s_neg, type="response")
mean(s_neg.probs)
s_est.probs <- predict(mylogit,newdata = s_est, type="response")
mean(s_est.probs)
s_sal.probs <- predict(mylogit,newdata = s_sal, type="response")
mean(s_sal.probs)
df_cie.probs <- predict(mylogit,newdata = df_cie, type="response")
mean(df_cie.probs)
s_amb.probs <- predict(mylogit,newdata = s_amb, type="response")
mean(s_amb.probs)