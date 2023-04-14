library(readxl)
data<-read_excel(file.choose())
View(data)

library(jtools)
library(olsrr)
library(moments)
library(lmtest)
library(carData)
library(car)

# Parameters for the Regression output
confidence_level<- 0.95
output_digits<- 4

# Regression Model
Reg_model<- lm(Aperf ~ Estab + Rel + Finances + Achal + SEHa, data = data)
Reg_model


# Unstandardized Results
summ(Reg_model, confint = TRUE, ci.width = confidence_level, digits = output_digits)

# Standardized Results
summ(Reg_model, scale = TRUE, transform.response = TRUE, digits = output_digits)


#Assumptions of multiple regression

  # Multicolinearity Test
  ols_vif_tol(Reg_model)

  # Normality of the Residuals
  ols_plot_resid_hist(Reg_model)
  ols_plot_resid_qq(Reg_model)

  # Outlier Diagnostics
  ols_plot_resid_stud(Reg_model)

  # Independence Test
  durbinWatsonTest(Reg_model)

  # Linearity Test
  raintest(Reg_model)
  plot(Reg_model$residuals)

  # Homoscedasticity Test
  ols_plot_resid_fit(Reg_model)
  bptest(Reg_model)
  gqtest(Reg_model, order.by = ~ Estab + Rel + Finances + Achal + SEHa, data = data, fraction = 0)

cor(data)
