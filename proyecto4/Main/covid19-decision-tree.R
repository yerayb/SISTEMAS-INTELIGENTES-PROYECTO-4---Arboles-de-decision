# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart) #funcion que crea arbol de decision (R<ecursive Partition)
library(rpart.plot) 
library(RColorBrewer)
library(rattle)

############# SOLUCIÓN DEL PROBLEMA ####################################################################

# Read data from CSV: 
filename = "../Data/covid-19-symptoms.tab"
data <- read.csv(file = filename, sep =" ", header = TRUE)

# Remove non-numerical columns of the data

# Percentaje os training examples
training_p <- 0.80

count <- 1

while(count<=10){
  # Generate data partition 80% training / 20% test. The result is a vector with the indexes 
  # of the examples that will be used for the training of the model.
  training_indexes <- createDataPartition(y = data$TARGET, p = training_p, list = FALSE)
  
  
  # Split training and test data
  training_data <- data[training_indexes, ]  # Extract training data using training_indexes
  test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 
  
  # Create Linear Model using training data. 
  model <- rpart(formula = TARGET ~., data = training_data, 
                 minsplit = 1, #Menor numero de ejemplos que se contemplan en una rama - Minimo un ejemplo
                 minbucket = 1, #Menor numero de ejemplos en ramas finales
                 maxdepth = 5) #Profundidad maxima del arbol - se analizan 5 atributos
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class") #Crea una lista de resultados booleanos
  
 
  print(paste0("- IT: ", count)) 
  
  # Calculate accuracy using Confusion Matrix: 
  #El parametro accuracy mide la precision, indica el acierto del arbol de decision. 
  prediction_results <- table(test_data$TARGET, prediction)
  matriz_confusion <- confusionMatrix(prediction_results)
  print(matriz_confusion) 
  fancyRpartPlot(model,main = count)
  
  count <- count + 1
}
######################################################################################################

############################################## TEST RPART ############################################

# TEST 1
model <- rpart(formula = TARGET ~., data = training_data, 
               minsplit = 1, 
               minbucket = 1, 
               maxdepth = 5) 
prediction <- predict(model, test_data, type = "class") 
prediction_results <- table(test_data$TARGET, prediction)
matriz_confusion <- confusionMatrix(prediction_results)
print(matriz_confusion) 

# TEST 2
model <- rpart(formula = TARGET ~., data = training_data, 
               minsplit = 1, 
               minbucket = 1, 
               maxdepth = 2) 
prediction <- predict(model, test_data, type = "class") 
prediction_results <- table(test_data$TARGET, prediction)
matriz_confusion <- confusionMatrix(prediction_results)
print(matriz_confusion) 


# TEST 3
model <- rpart(formula = TARGET ~., data = training_data, 
               minsplit = 30, 
               minbucket = 30, 
               maxdepth = 2) 
prediction <- predict(model, test_data, type = "class") 
prediction_results <- table(test_data$TARGET, prediction)
matriz_confusion <- confusionMatrix(prediction_results)
print(matriz_confusion) 


