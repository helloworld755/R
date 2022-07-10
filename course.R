#Задача: проверить статистическую гипотезу о различии веса мужчин и женщин.

#библиотеки
#install.packages('plyr')
#install.packages('dplyr')
#install.packages("effsize")
#install.packages("pwr")
library(plyr)
library(dplyr)
library(effsize)
library(pwr)
#detach(package:plyr)
#detach(package:dplyr)

#загрузка датасета
sdata <- read.csv("data.csv", header = TRUE, sep = ";")
men = sdata$weight[sdata$gender==2]
women = sdata$weight[sdata$gender==1]

#среднее арифметическое
mean(men)
mean(women)

#гистограммы для визуализации распределения
hist(men)
hist(women)

#доверительный интервал
infer <- sdata %>% group_by(gender) %>% summarize(
  mu = mean(weight),
  k = qt(0.975, length(weight)-1),
  se = sd(weight)/sqrt(length(weight)),
  lowlevel = mean(weight) - k*se,
  highlevel = mean(weight) + k*se
)

di_w <- as.numeric(c(infer[1, 5], infer[1, 6]))
di_w
di_m <- as.numeric(c(infer[2, 5], infer[2, 6]))
di_m

# Интерпретация: доверительные интервалы узкие, что указывает на точную оценку.

# Статистический анализ

# 1. Стандартное отклонение неизвестно, предполагаем, что выборки с разной дисперсией.
# 2. Нулевая гипотеза H0: mu = mu0. Альтернативная гипотеза H1: mu != mu0.
# 3. Уровень значимости = 0.05.
# 4. Расчет мощности теста.
# 5. Тестирование гипотезы.

# статистика d Коэна (вручную и через библиотеку)

s.pool <- sqrt(((length(women)-1)*var(women)+(length(men)-1)*var(men))/(length(women)+length(men)-2))
d <- (mean(men)-mean(women))/s.pool
d
cohen.d(d=men,women)

# мощность теста
# интерпретация: если мощность высокая (=1), значит, различия очень маленькие

pwr.t2n.test(n1=length(women),n2=length(men), d=d, sig.level = 0.05, alternative = "two.sided")

# тестирование гипотезы
# интерпретация: p-value маленькое, причиной чему может быть то, что выборка очень большая,
# но также говорит о том, что тест надежен и мы нашли статистически значимые различия

t.test(men, women, alternative = "two.sided")

# Результат: было получено очень маленькое значение p-value, и следует отвергнуть H0.
