#myAPI.R 
library(tidymodels)
library(tidyverse)

# Load model and data
set.seed(42)
fitted_model <- readRDS("model/full_model.RDS")
data <- readRDS("data/model_data.RDS")
data <- data |>
  mutate(Diabetes_binary = factor(Diabetes_binary, levels = c("Yes", "No")))

# Find mean or most common values for each feature to pass in as function default
BMI_mean <- mean(data$BMI)
PhysActivity_mode <- names(which.max(table(data$PhysActivity)))
Smoker_mode <- names(which.max(table(data$Smoker)))
AnyHealthcare_mode <- names(which.max(table(data$AnyHealthcare)))
# We are going to treat GenHlth as quasi-categorical since our model has only seen integer values and find the most common response
GenHlth_mode <- as.numeric(names(which.max(table(data$GenHlth))))


#* Find prediction of model based on given inputs
#* @param BMI BMI of Individual
#* @param PhysActivity Has the individual participated in physical activity in the last 30 days?
#* @param AnyHealthcare Does the individual have access to healthcare?
#* @param Smoker Has the individual smoked more than 100 cigarettes?
#* @param GenHlth Individual health rating from 1-5, 1 being best 5 being worst
#* @get /pred
function(BMI = BMI_mean, 
         PhysActivity = PhysActivity_mode,
         Smoker = Smoker_mode,
         AnyHealthcare = AnyHealthcare_mode,
         GenHlth = GenHlth_mode){
  # Coerce Data Types into dataframe
  BMI <- as.numeric(BMI)
  PhysActivity <- factor(PhysActivity, levels = c("No", "Yes"))
  Smoker <- factor(Smoker, levels = c("No", "Yes"))
  AnyHealthcare <- factor(AnyHealthcare, levels = c("No", "Yes"))
  GenHlth <- as.numeric(GenHlth)
  pred_data <- tibble(BMI, PhysActivity, Smoker, AnyHealthcare, GenHlth)
  predict(fitted_model, pred_data)
}

#query with http://localhost:8000/pred?BMI=20&PhysActivity=Yes&Smoker=No&AnyHealthcare=Yes&GenHlth=1
#query with http://localhost:8000/pred?BMI=40&PhysActivity=No&Smoker=Yes&AnyHealthcare=Yes&GenHlth=5
#query with http://localhost:8000/pred?BMI=40&PhysActivity=No

#* Provide info for project including name and URL of pages site
#* @get /info
function(){
  name <- "Thad Creech"
  url <- "https://t-creech.github.io/ST558-Project3/"
  paste(name, url, sep = "   ")
}

#http://localhost:8000/info

#* Plot of confusion matrix
#* @serializer png
#* @get /confusion
function(){
  predictions <- predict(fitted_model, data) |> bind_cols(data)
  confusion_matrix <- conf_mat(predictions, truth = Diabetes_binary, estimate = .pred_class)
  p <- autoplot(confusion_matrix, type = "heatmap")
  print(p)
}
#http://localhost:8000/confusion
