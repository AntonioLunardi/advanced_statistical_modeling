library(ISLR)
library(MASS)
require(ggplot2)
require(ggResidpanel)
library(car)

data(Carseats)
help(Carseats)


# 1.0 EXPLORATORY DATA ANALYSIS

# First rows
head(Carseats)

# Statistical information
summary(Carseats)

# Variables, types and categories
str(Carseats)

# Sales histogram
hist(Carseats$Sales, 
     main = "Sales histogram", 
     xlab = "Sales")

# Correlation matrix
cor(Carseats[, c("Sales", "Price", "Advertising", "Population", "Age",
                 "Education", "Income")])


# 2.0 MODEL CREATION AND VALIDATION

# Model generation
linear_multiple_model <- lm(Sales ~., data = Carseats)

# Model summary
summary(linear_multiple_model)

# Verify the existence of constant variance (it is a need for linear models) 
resid_panel(linear_multiple_model, 
            plots = c("resid", "qq", "ls", "cookd"), 
            qqbands = TRUE, 
            nrow = 2)

# Residuals of each variable
resid_xpanel(linear_multiple_model)

# 3.0 PREDICTION

# Sales prediction
new_data <- data.frame(
  CompPrice = 125,
  Income = 80,
  Advertising = 0,
  Population = 300,
  Price = 125,
  ShelveLoc = 'Good',
  Age = 60,
  Education = 15,
  Urban = 'No',
  US = 'Yes')

predict(linear_multiple_model, newdata = new_data)

# First element ordinary and t-student residuals
residuals(linear_multiple_model)[1]
rstudent(linear_multiple_model)[1]

# K expected sold seats variation for an increase of 10 dolars in comp. price
# (1000 x factor)
10 * coef(linear_multiple_model)['CompPrice']

# K expected sold seats variation for changing the classification of ShelveLoc
# from 'Good' to 'Bad' and 'Good' to 'Medium'
good <- coef(linear_multiple_model)['ShelveLocGood']
medium <- coef(linear_multiple_model)['ShelveLocMedium']
good
medium
good - medium

# Advertising variable 99% confidence interval
confint(linear_multiple_model, 'Advertising', level=0.99)

# Expected sold unities for stores with the following data and 95% confidence
new_data <- data.frame(
  CompPrice = 125,
  Income = 80,
  Advertising = 0,
  Population = 300,
  Price = 125,
  ShelveLoc = 'Good',
  Age = 60,
  Education = 15,
  Urban = 'No',
  US = 'Yes'
)
predict(linear_multiple_model, newdata = new_data, interval = 'confidence') 

# Predicted sold unities for stores with the same data and 95% confidence
predict(linear_multiple_model, newdata = new_data, interval = 'prediction')

# Covariables selection with selection algorithms (backward, forward, stepwise) 
# based on AIC and BIC






adjust_back_AIC <- step(linear_multiple_model, 
                        direction = 'backward', 
                        k = 2)
summary(adjust_back_AIC)

adjust_null <- lm(Sales ~1, data = Carseats)
adjust_for_AIC <- step(adjust_null, 
                       direction = 'forward', 
                       k = 2, 
                       scope = formula(linear_multiple_model))

### Na aplicação do método forward, partimos do modelo nulo (apenas com o intercepto)
### e precisamos definir a fórmula do maior modelo a ser investigado.
summary(adjust_for_AIC)

adjust_both_AIC <- step(linear_multiple_model, 
                        direction = 'both', 
                        k = 2)
summary(adjust_both_AIC)

### Para a aplicação do critério BIC, devemos definir k=log(n)
k_BIC <- log(nrow(Carseats))

adjust_back_BIC <- step(linear_multiple_model, 
                        direction = 'backward', 
                        k = k_BIC)
summary(adjust_back_BIC) 

adjust_for_BIC <- step(adjust_null, 
                       direction = 'forward', 
                       k = k_BIC, 
                       scope = formula(linear_multiple_model))
### Na aplicação do método forward, partimos do modelo nulo (apenas com o intercepto)
### e precisamos definir a fórmula do maior modelo a ser investigado.
summary(adjust_for_BIC)

adjust_both_BIC <- step(linear_multiple_model, 
                        direction = 'both', 
                        k = k_BIC)
summary(adjust_both_BIC)

# Vamos comparar os modelos selecionados usando cada método.
compareCoefs(adjust_back_AIC, adjust_for_AIC, adjust_both_AIC,
             adjust_back_BIC, adjust_for_BIC, adjust_both_BIC)

# Calcule os resíduos do modelo
residuals <- residuals(linear_multiple_model)

# Calcule os valores ajustados pelo modelo
fitted_data <- fitted(linear_multiple_model)

# Crie o gráfico de resíduos vs. valores ajustados
plot(fitted_data, residuals, 
     xlab = 'Fitted data', 
     ylab = 'Residuals',
     main = 'Residuals vs. Fitted data')
