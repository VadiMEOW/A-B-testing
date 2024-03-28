#install.packages("dplyr")
#install.packages("tidyverse")
library(dplyr)
library (tidyverse)
dir()
setwd("C:/Users/groshev.va/Desktop/ab")

data = read.csv("ab_test_results_aggregated_views_clicks_6.csv")
names(data) <- c("user_id", "group", "clicks","number_of_purchases" )

#Выбираем контрольную группу, для подсчета базовой конверсии
control_group = filter(data, group == 'control')
test_group = filter(data, group == 'test')
#Подсчитываем базовую конверсию 
#Конверсия = (Количество успешных конверсий / Общее количество кликов) 
A_conversion = (sum(control_group$number_of_purchases) / sum(control_group$clicks))
#Конверсия = 0.0694
#Допустим, что мы хотим увеличить конверсию на 15%
mde = 0.15
target_conversion = A_conversion*mde + A_conversion
#Желаемая конверсия = 0,0867

#Считаем мощность теста при нашей выборке
power.prop.test(n = length( data$user_id), p1 = A_conversion, p2 = target_conversion, sig.level = 0.05, 
                alternative = "two.sided")
#Так как мощность = 1, мы можем утверждать, что вероятность того, что 
#тест покажет различия между тестовой и контрольной группами, при условии, что они действительно есть является 100%
#Это объясняется объемом датасета, обычно рекомендованная мощность равна 0,8
power.prop.test( p1 = A_conversion, p2 = target_conversion, sig.level = 0.05, power = 0.8, 
                alternative = "two.sided")
#Нам бы хватило 3752 наблюдений, для проведения теста

B_conversion = (sum(test_group$number_of_purchases) / sum(test_group$clicks))#0.0799
dif_conv = (B_conversion - A_conversion)/A_conversion 
#Конверсия увеличилась более чем на 15% желаемых процентов, при мощности теста = 1
#уже можно считать, что мы победили 
#Проверим наши данные на нормальность распределений

qqnorm(test_group$clicks)
qqline(test_group$clicks)
hist(test_group$clicks, col='steelblue', main='Normal')

qqnorm(test_group$number_of_purchases)
qqline(test_group$number_of_purchases)
hist(test_group$number_of_purchases, col='steelblue', main='Normal')

qqnorm(control_group$clicks)
qqline(control_group$clicks)
hist(control_group$clicks, col='steelblue', main='Normal')

qqnorm(control_group$number_of_purchases)
qqline(control_group$number_of_purchases)
hist(control_group$number_of_purchases, col='steelblue', main='Normal')
#Исходя из графиков выше, можно сделать вывод о ненормальности распределения

#Проводим не параметрические тесты, т.к. респределение не нормальное
#Например, критерий Манна-Уитни
#Тест Крускалла-Уоллиса
#Бутстрэп
#Критерий Стьюдента? 
