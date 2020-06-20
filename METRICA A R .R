library(tidyverse) 
library(mfx) 
library(rio) 
library(lmtest) 
library(texreg) 
library(ivpack)

############## Задание 1 ############## 
df = import('/Users/marina/Downloads/forestfires.csv')
head(df)
# удалим часть нулевых значений зависимой переменной, чтобы минимизировать смещение в сторону нулевого значения
df.sorted <- df[order(df$area),]
new.df <- df.sorted[-c(1:100),]
# на графике ниже видно, что после некоторого удаления нулевых значений целевой переменной все равно смещены к нулю, поэтому прологарифмируем area
qplot(data = new.df, area, fill ="brick")
qplot(data = new.df, log(area), fill ="brick")
# сделаем из переменных month и day дамми переменные 
library("caret")
dum <- dummyVars(" ~ .", data = new.df)
df.dum.sort <- data.frame(predict(dum, newdata = new.df))



############## Задание 2 ############## 


# посчитаем основные статистики
summary(df.dum.sort)
# отберем признаки
library("corrplot")
cor(new.df$X, new.df$area)
cor(new.df$Y, new.df$area)
cor(new.df$RH, new.df$area)
cor(new.df$DMC, new.df$area)
cor(new.df$ISI, new.df$area)
cor(new.df$FFMC, new.df$area)
cor(new.df$DC, new.df$area)
cor(new.df$wind, new.df$area)
cor(new.df$temp, new.df$area)
cor(new.df$rain, new.df$area)
cor = cor(df.dum.sort)
corrplot(cor, method="circle")
# После построения графика корреляции можно сделать вывод о том, что почти никакая переменная сильно не коррелирует с показателеи площади подаров. В модели останутся следующие параметры: monthsep, dayfri, daysat, temp, RH
# Видно, что коэффициенты FFMC, DMC, DC, ISI, а также temp сильно коррелируют друг с дургом. Данные коэффициенты указывают на степень сухости и ветренности, что значит, что они могут быть объяснены переменной tepm. Поэтому, а также из-за корреляции с зависимой переменной, оставим в нашей модели температуру. Чем она выше, тем больше будут площади распространения пожаров.
# monthsep, скорее всего, коррелирует с зависимой переменной из-за того, что в данный месяц может наблюдаться самая высокая температура, что сильно увеличивает площади пожаров.
# dayfri, судя по таблице, имеет отрицательную корреляция с зависимой переменной. Пока не очень понятно, чем это может быть объяснено
# daysat оказывает прямое воздействие на возможность возгорания и увеличение площади пожаров, так как в выходные есть риск возгорания по человеческой неосторожности
# RH - это показатель относительной влажности. Чем он выше, тем меньше пожаров будет наблюдаться 

# визуализируем - гистограммы 
boxplot(df.dum.sort$monthsep, main = 'monthsep', col = 'green')
boxplot(df.dum.sort$dayfri, main = 'dayfri', col = 'green')
boxplot(df.dum.sort$daysat, main = 'daysat', col = 'red')
boxplot(df.dum.sort$temp, main = 'temp', col = 'pink')
boxplot(df.dum.sort$RH, main = 'RH', col = 'yellow')
# визуализируем - box whisker 
qplot(data = df.dum.sort, monthsep , binwidth = 1)
qplot(data = df.dum.sort, dayfri, binwidth = 1)
qplot(data = df.dum.sort, daysat, binwidth = 1)
qplot(data = df.dum.sort, temp, binwidth = 1)
qplot(data = df.dum.sort, RH, binwidth = 1)


############## Задание 3 ############## 

model <- lm(data = df.dum.sort, log(area+1) ~ monthsep + dayfri + daysat + temp + RH)

library("car")
# посчитаем коэффициент вздутия диспресии для нашей модели 
vif(model)
# посчитаем conditinal number для модели
kappa(model)
# VIF подучился не очень больших, что говорит об отсутствии мультиколлинеарности между факторами в модели
# значение CN больше, чем хотелось бы

############## Задание 4 ############## 

# оценим нашу модель 
summary(model)

# оказалось, что положительно на параметр area влияют переменные monthsep, daysat. То есть гипотеза про выходные подтвердилась, а также подтвердилась гипотеза о высокой температуре и сухости в сентябре. RH отрицательно влияет на площадь пожаров, что тоже логично, гипотеза подтвердилась. Однако, температура отрицательно влияет на площадь пожаров, это говорит о том, что в большей части наблюдений погода была низкой (скорее всего, характерны были дожди, снег, сырость и прочее). 
# значим только коэффициент при регрессоре monthsep на уровне 99%

# Проверим остатки регрессии на нормальность 
library("tseries")
jarque.bera.test(residuals(model))
# гипотеза о нормальности отвергается при любом уровне значимости

############## Задание 5 ##############

# строим прогноз для среднего
predict(model, newdata = df.dum.sort, interval = "confidence")
# строим индивидуальный прогноз
predict(model, newdata = df.dum.sort, interval = "prediction")

############## Задание 6 ##############

# гетероскедастичность может быть вызвана RH, который указывает на влажность. Чем ниже будет данный показатель, тем больше будет разброс данных площади пожаров. Чем выше данный показатель, тем меньше будет разброс. Это есть отрицательная гетероскедастичность.
# также гетероскедастичным может быть параметр temp. Чем выше температура, тем больше будет разброс переменной area и наоборот. Положительная гетероскедастичность.

############## Задание 7 ##############

# Построим графики и попробуем визуально увидеть гетероскедастичность
qplot(data = df.dum.sort, RH, log(area+1))
qplot(data = df.dum.sort, temp, log(area+1))
# Визуально описанные выше гипотезы подтверждаются, так как можно увидь положительную и отрицательную гетероскедастичность у temp и RH соответсвенно

# Протестируем на гетероскедастичность с помощью теста Голдфельда-Квандта
library("dplyr")
library("broom")
library("prettyR")
gqtest(model, order.by = ~temp, data = df.dum.sort)
gqtest(model, order.by = ~RH, data = df.dum.sort)
# для показателя temp нулевая гипотеза отвергается, то есть имеет место гетероскедастичность. А для показателя RH гипотеза не отвергается, что говорит о гомоскедастичности.


############## Задание 8 ##############

# взвешанный МНК
weight = 1/(model$fitted.values)^2
model1 <- lm(data = df.dum.sort, log(area+1) ~ monthsep + dayfri + daysat + temp + RH)
model2 <- lm(data = df.dum.sort, log(area+1) ~ monthsep + dayfri + daysat + temp + RH, weight = weight)
summary(model1)
summary(model2)
plot(data = df.dum.sort, model1)
plot(data = df.dum.sort, model2)
# модель обычного МНК показывает лучшие результаты, чем модель взвешанного МНК. Так как monthsep был значим на уровне 99%, а в модели ВМНК это 95%


############## Задание 9 ##############


library("sandwich")

# В качетсве матрицы ошибок используем матрицу робастных ошибок в форме Уайта. Данная матрица устойчива к гетероскедастичности. На месте ковариационной матрицы в оценке бета коэффициентов стоитдиагональная матрица с оценками итых остатков регрессии в квадрате
vcovHC(model, type = "HC0")
coeftest(model, vcov. = vcovHC(model))
coeftest(model)
# p-value в модели с устойчивой к гетероскедастичности матрицей меньше, чем в обычной модели

# теперь посмотрим на модель, в которой используются стандартные ошибки в форме Ньюи-Веста. В данной модели ковариационной матрицей является диагональная матрица с  оценками итых остатков регрессии в квадрате плюс дополнительные элементы весов. Такая форма позволяет получить состоятельные оценки
vcovHC(model, type = "HC3")
coeftest(model, vcov. = vcovHC(model))
coeftest(model)

############## Задание 10 ##############


DF <- data.frame(monthsep_pca = df.dum.sort$monthsep, daysat_pca = df.dum.sort$daysat, dayfri_pca = df.dum.sort$dayfri, temp_pca = df.dum.sort$temp, HR_pca = df.dum.sort$RH)
DF_pca <- prcomp(DF, scale = TRUE)
# первая главная компонента объясняет 30% дисперсии, вторая - 24%

# построим регрессию на первую и вторую главную компоненту
pc_1 <- DF_pca$x[, 1]
pc_2 <- DF_pca$x[, 2]
model3 <- lm(data = df.dum.sort, log(area+1) ~ pc_1 + pc_2)
summary(model3)
summary(model)







