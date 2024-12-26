#Копнин Н.Е. постройте картосхему обилия деревьев родов Ива и Лиственница . 

# Установим необходимые пакеты (если не установлены)
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")

library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Очистим полностью память 
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("D:/AR/R_2")

# Считаем данные в переменные
greendb = read.csv("greendb.csv") 
map = sf::read_sf("moscow.geojson")

# График с заливкой
ggplot(map) + geom_sf(aes(fill = NAME)) + theme(legend.position = "none")

# Фильтруем данные для обилия деревьев родов Ива и Лиственница 
spec = greendb$species_ru
genus = stringr::str_split(spec, pattern = " ", simplify = TRUE)[, 1]
data = greendb %>% mutate(Genus = genus)

# Фильтрация по обилию и группировка по региону и роду 
d = data %>% 
  group_by(adm_region, Genus) %>% 
  summarise(count = n(), .groups = "drop")  %>% # Считаем количество деревьев и сбрасываем   
  filter(Genus %in% c("Ива", "Лиственница"))  # Условия для родов 

# Преобразуем данные в широкий формат
dd= pivot_wider(d, names_from = Genus, values_from = count) 

# Объединяем данные с картой
map = map %>% mutate(adm_region = NAME) 
map = left_join(map, dd, by = "adm_region") 

# Построение картосхемы для Ива
ggplot(map) +
  geom_sf(aes(fill = `Ива`)) + 
  theme() 
  
# Построение картосхемы для Лиственница
ggplot(map) +
  geom_sf(aes(fill = `Лиственница`)) + 
  theme()
