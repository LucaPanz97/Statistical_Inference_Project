library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)

MenuMcDonalds <- read.csv("MenuMcDonalds.csv", header=TRUE, sep=",")
attach(MenuMcDonalds)

g=lm(Calories~Serving.Size+Calories.from.Fat+Total.Fat+Total.Fat....Daily.Value.+Saturated.Fat+Saturated.Fat....Daily.Value.+Trans.Fat+Cholesterol+Cholesterol....Daily.Value.+Sodium+Sodium....Daily.Value.+Carbohydrates+Carbohydrates....Daily.Value.+Dietary.Fiber+Dietary.Fiber....Daily.Value.+Sugars+Protein+Vitamin.A....Daily.Value.+Vitamin.C....Daily.Value.+Calcium....Daily.Value.+Iron....Daily.Value.,data=MenuMcDonalds)
summary(g)
plot(g$residuals, ylab="Residuals", main="Plot of residuals")
qqnorm(g$residuals)
qqline(g$residuals)
shapiro.test(g$residuals)

b=boxcox(g)
names(b)
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda #0.989899
#non uso la trasformazione box-cox

X=model.matrix(g)
heatmap( cor( X ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)
vif(g)
g1=lm(Calories~Serving.Size+Calories.from.Fat+Saturated.Fat+Trans.Fat+Cholesterol+Sodium+Carbohydrates+Dietary.Fiber+Sugars+Protein+Vitamin.A....Daily.Value.+Vitamin.C....Daily.Value.+Calcium....Daily.Value.+Iron....Daily.Value.,data=MenuMcDonalds)
X1=model.matrix(g1)
heatmap( cor( X1 ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)
vif(g1)

#inizio a togliere da p-value più alto
g2 = update( g1, . ~ . - Dietary.Fiber)
summary(g2)
qqnorm(g2$res)
qqline(g2$res)
anova(g2,g1)

g3 = update( g2, . ~ . - Sodium)
summary(g3)
qqnorm(g3$res)
qqline(g3$res)
anova(g3,g2)

g4 = update( g3, . ~ . - Iron....Daily.Value.)
summary(g4)
qqnorm(g4$res)
qqline(g4$res)
anova(g4,g3)

g5 = update( g4, . ~ . - Trans.Fat)
summary(g5)
qqnorm(g5$res)
qqline(g5$res)
anova(g5,g4)

g6 = update( g5, . ~ . - Serving.Size)
summary(g6)
qqnorm(g6$res)
qqline(g6$res)
anova(g6,g5)

g7 = update( g6, . ~ . - Vitamin.A....Daily.Value.)
summary(g7)
qqnorm(g7$res)
qqline(g7$res)
anova(g7,g6)

g8 = update( g7, . ~ . - Saturated.Fat)
summary(g8)
qqnorm(g8$res)
qqline(g8$res)
anova(g8,g7)

g9 = update( g8, . ~ . - Vitamin.C....Daily.Value.)
summary(g9)
anova(g9,g8)

g10 = update( g8, . ~ . - Cholesterol)
summary(g10)
anova(g10,g8)

g11 = update( g8, . ~ . - Sugars)
summary(g11)
anova(g11,g8)

g12 = update( g8, . ~ . - Calcium....Daily.Value.)
summary(g12)
anova(g12,g8)

g13 = update( g8, . ~ . - Calories.from.Fat)
summary(g13)
anova(g13,g8)

g14 = update( g8, . ~ . - Carbohydrates)
summary(g14)
anova(g14,g8)

g15 = update( g8, . ~ . - Protein)
summary(g15)
anova(g15,g8)

#Modello finale
gf=g8
summary(gf)
shapiro.test(gf$res)
qqnorm(gf$res)
qqline(gf$res)

#Leverages
Z=model.matrix(gf)
H = Z %*% solve( t( Z ) %*% Z ) %*% t( Z )
lev = diag( H )
plot( gf$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
n=dim(MenuMcDonalds)[1]
p=gf$rank
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( gf$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

#togliamo leverages
id_to_keep_lev = !( 1:n %in% watchout_ids_lev )
gfin_lev = lm( Calories ~ Calories.from.Fat + Cholesterol + Carbohydrates + Sugars + Protein + Vitamin.C....Daily.Value. + Calcium....Daily.Value., MenuMcDonalds[ id_to_keep_lev, ] )
shapiro.test(gfin_lev$residuals)
#lo shapiro test peggiora 

#Residui standardizzati
gs = summary(gf)
res_std = gf$res/gs$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
plot( gf$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( gf$fitted.values[watchout_ids_rstd],
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )

#togliamo residui standardizzati
id_to_keep_rstd = !( 1:n %in% watchout_ids_rstd)
gfin_rstd = lm( Calories ~ Calories.from.Fat + Cholesterol + Carbohydrates + Sugars + Protein + Vitamin.C....Daily.Value. + Calcium....Daily.Value., MenuMcDonalds[ id_to_keep_rstd, ] )
shapiro.test(gfin_rstd$residuals)

#Residui studentizzati
stud = gf$residuals / ( gs$sigma * sqrt( 1 - lev ) )
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
plot( gf$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( gf$fitted.values[watchout_ids_stud],
        stud[watchout_ids_stud], col = 'pink', pch = 16 )

#togliamo residui studentizzati
id_to_keep_stud = !( 1:n %in% watchout_ids_stud)
gfin_stud = lm( Calories ~ Calories.from.Fat + Cholesterol + Carbohydrates + Sugars + Protein + Vitamin.C....Daily.Value. + Calcium....Daily.Value., MenuMcDonalds[ id_to_keep_stud, ] )
shapiro.test(gfin_stud$residuals)

#Outliers
boxplot(Calories, main = 'Calorie')
abline(h = mean(Calories))
q1=quantile(Calories)
IQR=q1[4]-q1[2]
Lim=IQR*1.5+q1[4]
watchout_out = which(Calories > Lim )
id_to_keep_out = !( 1:n %in% watchout_out )
gfin_out=lm( Calories ~ Calories.from.Fat + Cholesterol + Carbohydrates + Sugars + Protein + Vitamin.C....Daily.Value. + Calcium....Daily.Value., MenuMcDonalds[ id_to_keep_out, ] )
shapiro.test(gfin_out$residuals)

#Distanza di Cook
Cdist = cooks.distance( gf )
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
plot( gf$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( gf$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )

#togliamo cooks distance
id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gfin_cook = lm( Calories ~ Calories.from.Fat + Cholesterol + Carbohydrates + Sugars + Protein + Vitamin.C....Daily.Value. + Calcium....Daily.Value., MenuMcDonalds[ id_to_keep, ] )
summary(gfin_cook)
shapiro.test(gfin_cook$residuals)

#Modello Finalissimo
gult=gfin_cook
shapiro.test(gult$res)
vif(gult)

#IC
alpha=0.05
nult=dim(MenuMcDonalds[ id_to_keep, ])[1]
pult=gult$rank
t_alpha=qt(1-alpha/2,nult-pult)

beta_hat_int=gult$coefficients[1]
se_beta_hat_int = summary(gult)[[4]][1,2]
IC_int=c(beta_hat_int-se_beta_hat_int*t_alpha,beta_hat_int+se_beta_hat_int*t_alpha)

beta_hat_cff=gult$coefficients[2]
se_beta_hat_cff = summary(gult)[[4]][2,2]
IC_cff=c(beta_hat_cff-se_beta_hat_cff*t_alpha,beta_hat_cff+se_beta_hat_cff*t_alpha)

beta_hat_chol=gult$coefficients[3]
se_beta_hat_chol = summary(gult)[[4]][3,2]
IC_chol=c(beta_hat_chol-se_beta_hat_chol*t_alpha,beta_hat_chol+se_beta_hat_chol*t_alpha)

beta_hat_carb=gult$coefficients[4]
se_beta_hat_carb = summary(gult)[[4]][4,2]
IC_carb=c(beta_hat_carb-se_beta_hat_carb*t_alpha,beta_hat_carb+se_beta_hat_carb*t_alpha)

beta_hat_sug=gult$coefficients[5]
se_beta_hat_sug = summary(gult)[[4]][5,2]
IC_sug=c(beta_hat_sug-se_beta_hat_sug*t_alpha,beta_hat_sug+se_beta_hat_sug*t_alpha)

beta_hat_pro=gult$coefficients[6]
se_beta_hat_pro = summary(gult)[[4]][6,2]
IC_pro=c(beta_hat_pro-se_beta_hat_pro*t_alpha,beta_hat_pro+se_beta_hat_pro*t_alpha)

beta_hat_vitc=gult$coefficients[7]
se_beta_hat_vitc = summary(gult)[[4]][7,2]
IC_vitc=c(beta_hat_vitc-se_beta_hat_vitc*t_alpha,beta_hat_vitc+se_beta_hat_vitc*t_alpha)

beta_hat_calc=gult$coefficients[8]
se_beta_hat_calc = summary(gult)[[4]][8,2]
IC_calc=c(beta_hat_calc-se_beta_hat_calc*t_alpha,beta_hat_calc+se_beta_hat_calc*t_alpha)

