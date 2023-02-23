setwd("C:/Users/panze/Desktop/INFERENZA STATISTICA/Dataset")

library(MASS)
library(leaps)
library(GGally)
library(faraway)
library(ellipse)
library(car)
library(RColorBrewer)
library(ResourceSelection)
library(rms)
library(lrmest)

MenuMcDonaldsfinale <- read.csv("MenuMcDonaldsfinale.csv", header=TRUE, sep=",")
attach(MenuMcDonaldsfinale)

#inserisco variabile binomiale 
plot( Calories,Punta,pch = ifelse( Punta == 1, 3, 4 ), 
      col = ifelse( Punta == 1, 'forestgreen', 'red' ),
      xlab = 'Calories', ylab = 'Punta', main = 'Punta vs. Calories', 
      lwd = 2, cex = 1.5 )
min( Calories ) # 1 
max( Calories )#1880
#inserisci in x il limite delle classi classi da 188 calorie
x = c( 0, 200, 400, 600, 800,1000,1200,1400,1600,1800,2000 )
mid = c( ( x [ 2:11 ] + x [ 1:10 ] )/2 )
#suddivido i dati nelle classi creatw #Age per noi è calories
GRAGE = cut( Calories, breaks = x, include.lowest = TRUE, right = FALSE ) 
GRAGE

#calcola media e sovrapponi
y = tapply( Punta, GRAGE, mean )

plot( Calories, Punta, pch = ifelse( Punta == 1, 3, 4 ),
      col = ifelse( Punta == 1, 'forestgreen', 'red' ),
      xlab = 'Calories', ylab = 'Punta',
      main = 'Punta vs. Calories', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

mod = glm( Punta ~ Calories, family = binomial( link = logit ), maxit=100 )
summary( mod ) 

mod$linear.predictors
mod$fitted.values

# Facciamo un graficoco della predizione del modello. 
plot( Calories, Punta, pch = ifelse( Punta == 1, 3, 4 ), 
      col = ifelse( Punta== 1, 'forestgreen', 'red' ), 
      xlab = 'Calories', ylab = 'Punta', 
      main = 'Punta vs. Calories', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )
lines( Calories, mod$fitted, col = 'darkblue' )


#calcolo odds ratio
summary( mod )



odds= exp( 200 * coef( mod ) [ 2 ] ) #al posto di 10 metti la larghezza della classe



#Ic per la regressione logistica
#95 per cento di confidenza per OR
alpha = 0.05
qalpha = qnorm( 1 - alpha/2 ) 
qalpha 

IC.sup = exp( 200 * coef( mod ) [ 2 ] + qalpha * 200* summary( mod )$coefficients[ 2, 2 ] ) 
IC.inf = exp( 200* coef( mod ) [ 2 ] - qalpha * 200* summary( mod )$coefficients[ 2, 2 ] ) 
c( IC.inf, IC.sup ) 




#posso provare a calcolarlo cosi anche



V = vcov( mod ) #matrice covarianza 
V
x = 500#esempio di valore
# errore standard
predict( mod, data.frame( Calories = 500 ), se = TRUE )
#rappresento intervallo di confidenza
grid = ( 1:1880 )
se = predict( mod, data.frame( Calories = grid ), se = TRUE ) # errori standard corrispondenti ai valori della griglia
help( binomial )
gl = binomial( link = logit )
# funzione di link utilizzata
# Family objects provide a convenient way to specify the details 

plot( mid, y, col = "red", pch = 3, ylim = c( 0, 1 ),
      ylab = "Probability of Punta", 
      xlab = "Calories",
      main = "IC per la Regressione Logistica" ) 
lines( grid, gl$linkinv( se$fit ) ) 
lines( grid, gl$linkinv( se$fit - qnorm( 1-0.025 ) * se$se ), col = "red", lty = 2 ) 
lines( grid, gl$linkinv( se$fit + qnorm( 1-0.025 ) * se$se ), col = "red", lty = 2 )



#Goodness of fit



mod2 = lrm( Punta ~ Calories, x = TRUE, y = TRUE ) 
mod2 



hoslem.test( mod$y, fitted( mod ), g = 10) #g deve essere maggiore di p
#p value alto vuol dire buon fitting