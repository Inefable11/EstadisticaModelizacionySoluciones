datos <- read.csv("D:/MAESTRÍA EN BIG DATA Y DATA ANALYTICS/Análisis e interpretación de datos/ACTIVIDAD 2/Proyecto/Actividad 2 Analisis e interpretacion/diabetes.csv")
summary(datos$Glucose)
summary(datos$BMI)
summary(datos$Age)
summary(datos$DiabetesPedigreeFunction)
summary(datos$Insulin)
summary(datos$Outcome)


hist(datos$Glucose[datos$Outcome == 0], col = "blue", main = "Histograma de Glucosa por Resultado de Diabetes", xlab = "Nivel de Glucosa")
hist(datos$Glucose[datos$Outcome == 1], col = "red", add = TRUE)
legend("topright", legend = c("No Diabetes", "Diabetes"), fill = c("blue", "red"))

hist(datos$BMI[datos$Outcome == 0], col = "blue", main = "Histograma de BMI por Resultado de Diabetes", xlab = "Índice de Masa Corporal")
hist(datos$BMI[datos$Outcome == 1], col = "red", add = TRUE)
legend("topright", legend = c("No Diabetes", "Diabetes"), fill = c("blue", "red"))

hist(datos$Age[datos$Outcome == 0], col = "blue", main = "Histograma de Edad por Resultado de Diabetes", xlab = "Edad")
hist(datos$Age[datos$Outcome == 1], col = "red", add = TRUE)
legend("topright", legend = c("No Diabetes", "Diabetes"), fill = c("blue", "red"))

hist(datos$BloodPressure[datos$Outcome == 0], col = "blue", main = "Histograma de Presión Arterial por Resultado de Diabetes", xlab = "Presión Arterial")
hist(datos$BloodPressure[datos$Outcome == 1], col = "red", add = TRUE)
legend("topright", legend = c("No Diabetes", "Diabetes"), fill = c("blue", "red"))

hist(datos$Insulin[datos$Outcome == 0], col = "blue", main = "Histograma de Niveles de Insulina por Resultado de Diabetes", xlab = "Niveles de Insulina")
hist(datos$Insulin[datos$Outcome == 1], col = "red", add = TRUE)
legend("topright", legend = c("No Diabetes", "Diabetes"), fill = c("blue", "red"))

hist(datos$DiabetesPedigreeFunction[datos$Outcome == 0], col = "blue", main = "Histograma de Porcentaje de diabetes segùn antecedentes por Resultado de Diabetes", xlab = "DiabetesPedigreeFunction")
hist(datos$DiabetesPedigreeFunction[datos$Outcome == 1], col = "red", add = TRUE)
legend("topright", legend = c("No Diabetes", "Diabetes"), fill = c("blue", "red"))


variables_interes <- datos[, c("Outcome", "Glucose", "BMI", "Age", "BloodPressure", "Insulin", "DiabetesPedigreeFunction")]

# Calcular la matriz de correlación de Pearson
correlacion_pearson <- cor(variables_interes, use = "complete.obs")

# Mostrar la matriz de correlación
print(correlacion_pearson)


library(ggplot2)

# Gráfico de dispersión para Glucose vs. Outcome
ggplot(datos, aes(x = Glucose, y = Outcome)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Glucose vs. Outcome",
       x = "Glucose",
       y = "Outcome")

# Gráfico de dispersión para BMI vs. Outcome
ggplot(datos, aes(x = BMI, y = Outcome)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: BMI vs. Outcome",
       x = "BMI",
       y = "Outcome")

# Gráfico de dispersión para Age vs. Outcome
ggplot(datos, aes(x = Age, y = Outcome)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Age vs. Outcome",
       x = "Age",
       y = "Outcome")