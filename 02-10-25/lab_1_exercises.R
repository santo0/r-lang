# I had problems when installing some dependencies, this fixes this
options("install.lock"=FALSE)


library(readr)
cars <- read_csv("02-10-25/cars.csv")

summary(cars)
# quantitative: mpg, cubinches, hp, weightlbs, time-to-60
# qualitative: cylinders, brand, year

hist(cars$mpg)
hist(cars$hp)
hist(cars$weightlbs)

brand_distribution <- table(cars$brand)
barplot(brand_distribution)  
cylinders_distribution <- table(cars$cylinders)
barplot(cylinders_distribution)  
year_distribution <- table(cars$year)
barplot(year_distribution)  

# Construct a descriptive statistics table with mean, median and standard deviation for the variables mpg, hp and weight.

prop.table(table(cars$mpg))



boxplot(weightlbs~brand,data=cars)