setwd("C:/Users/panze/Desktop/INFERENZA STATISTICA/Dataset")

library(MASS)
library(leaps)
library(GGally)
library(faraway)
library(ellipse)
library(car)
library(RColorBrewer)

MenuMcDonalds <- read.csv("MenuMcDonalds.csv", header=TRUE, sep=",")
attach(MenuMcDonalds)

my_colors = brewer.pal( 9,'Set2') #numero categorie
boxplot( Calories ~ Category, xlab = 'Category', ylab = 'Calories', main = 'Calories for Category', col = my_colors )
abline( h = mean( Calories ) )

tapply(MenuMcDonalds$Calories,MenuMcDonalds$Category,length)
tapply(MenuMcDonalds$Calories,MenuMcDonalds$Category,mean)
tapply(MenuMcDonalds$Calories,MenuMcDonalds$Category,median)


# Verifico ipotesi normalita intragruppo

Ps = tapply( Calories,Category, function( x ) ( shapiro.test( x )$p ) )
Ps


qqnorm(Calories[Category=='Breakfast'],main = 'Breakfast')
qqline(Calories[Category=='Breakfast'])
qqnorm(Calories[Category=='Beef & Pork'],main = 'Beef & Pork')
qqline( Calories[Category=='Beef & Pork'])
qqnorm(Calories[Category=='Chicken & Fish'],main = 'Chicken & Fish')
qqline(Calories[Category=='Chicken & Fish'])
qqnorm(Calories[Category=='Salads'],main = 'Salads')
qqline(Calories[Category=='Salads'])
qqnorm(Calories[Category=='Snacks & Sides'],main = 'Snacks & Sides')
qqline(Calories[Category=='Snacks & Sides'])
qqnorm(Calories[Category=='Desserts'],main = 'Desserts')
qqline(Calories[Category=='Desserts'])
qqnorm(Calories[Category=='Beverages'],main='Beverages')
qqline(Calories[Category=='Beverages'])
qqnorm(Calories[Category=='Coffee & Tea'],main='Coffee & Tea')
qqline(Calories[Category=='Coffee & Tea'])
qqnorm(Calories[Category=='Smoothies & Shakes'],main='Smoothies & Shakes')
qqline(Calories[Category=='Smoothies & Shakes'])

# Verifichiamo l'omoschedasticità

leveneTest( Calories, Category )



# Confronto tra le mediane

kruskal.test(Calories, Category)