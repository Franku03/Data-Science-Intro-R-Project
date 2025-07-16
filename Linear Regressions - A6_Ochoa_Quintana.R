# MODELOS DE REGRESIÓN LINEAL PARA EL PROYECTO - A6_OCHOA_QUINTANA

# If janitor package is not install
# install.packages("janitor")


rm(list = ls())

library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Set working directory

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# ----------------------------------DATA CLEANING------------------------------------

# Setting data source and doing some initial cleaning

project_data <-read_excel('A3_Ochoa_Quintana.xlsx') %>% 
  janitor::clean_names() %>% # For changing names to snake_case
  janitor::remove_empty() # For clearing blank records (we erased the first record which was a test)

# Changing column names

column_names <- project_data %>% 
  names()

project_data <- project_data %>% 
  dplyr::rename(
    "timestamp" = column_names[1],
    "semester" = column_names[2],
    "gender" = column_names[3],
    "age" = column_names[4],
    "english_level" = column_names[5],
    "used_external_resources" = column_names[6],
    "tech_learning_platforms" = column_names[7],
    "platform_choice_factors" = column_names[8],
    "paid_for_online_course" = column_names[9],
    "online_course_spending" = column_names[10],
    "alt_high_cost_course" = column_names[11],
    "is_english_obstacle" = column_names[12],
    "solution_english_barrier" = column_names[13],
    "improved_uni_project_performance" = column_names[14],
    "projects_in_portfolio" = column_names[15],
    "career_benefits_from_courses " = column_names[16],
  ) 

semesters_order <- c(
  "Primero", "Segundo", "Tercero", "Cuarto",
  "Quinto", "Sexto", "Séptimo", "Octavo",
  "Noveno", "Décimo"
) # Necessary for ordering data related to semesters

rm(column_names) # Erasing the column names vector



# CONSIDERATIONS FOR DATA SET:

# As said in Assignment #5, given the predominance of DISCRETE VARIABLES,
# Regression models presented some difficulty when developing them,
# so in the data set we had to make some adjustments:

# 1) In both models we must convert the variables to Ordered factors and then to numeric

# 2) For both models we also calculate the average of each discrete value, in order 
# avoid superposition of values in the model, and for displaying the
# trend of the mean (i.e., how average spending changes as the semester progresses).
# Plotting the averages makes the graph much cleaner and the 
# regression relationship sightly evident.

# 3) Given the modest sample of the size (30 records), we also associate the 
# low fidelity of the following models to this low amount of records.
# By having more responses, and continuous variables,
# we could probably have seen a most significant linear relation between variables used in the models.

# --------------------------------REGRESSION ANALYSIS--------------------------------------

# ------------------------------------------------------------------------------------------
# REGRESION MODEL 1 (Linear Regression): Relation between semester and online course spending
# ------------------------------------------------------------------------------------------

# This is for the FIRST research Question

# First we Convert 'online_course_spending' to an ORDERED factor, then to NUMERIC ---
# This is necessary for geom_smooth(method="lm") to plot a meaningful line,
# and to calculate the average semester per spending category.

# Necessary for ordering data related to how much students have paid for online courses
money_spent_order <- c("$0", "Menos de $10", "Entre $10 y $50", "Más de $50")

# We select and adjust the data we need
payment_by_semester <- project_data %>%
  select(
    semester,
    online_course_spending,
  ) %>% 
  mutate(
    online_course_spending = case_when(online_course_spending %in% c("Nunca he pagado por un curso") ~ "$0",
                                        TRUE ~ online_course_spending
    ), # We clean values for the Visualization
    
    # We convert online_course_spending to ordered factor and then to numeric (1,2,3,4)
    online_course_spending = factor(online_course_spending, levels = money_spent_order),
    online_course_spending_numeric = as.numeric(online_course_spending),
    
    # We convert semester to order factor and then to numeric (1 to 10)
    semester = factor(semester, levels =semesters_order),
    semester_numeric = as.numeric(semester) 
  )

# We calculate the average spending by semester, this is what will be displayed in the model
avg_spending_semester <- payment_by_semester %>%
  group_by(semester, semester_numeric) %>% # We group by semesters
  summarise(
    
    # Calculate average of spending base on the number of respondents that selected that options
    avg_spending = mean(online_course_spending_numeric, na.rm = TRUE),
    .groups = 'drop'
    
  ) %>%
  arrange(semester_numeric) # We order the data set by semesters

# mu_y <- mean(payment_by_semester$online_course_spending_numeric)
# mu_x <- mean(payment_by_semester$semester_numeric)
# s_y <- sd(payment_by_semester$online_course_spending_numeric)
# s_x <- sd(payment_by_semester$semester_numeric)

# We calculate the correlation coefficient, in order to estimate the correlation 
# between variables and direction of the lineal relation
r <- cor(payment_by_semester$semester_numeric, payment_by_semester$online_course_spending_numeric)

# --- Adjust Lineal regression model ---
# We see the model summary for interpretation of coefficients, R-squared, etc.
# Dependent Variable (response): online_course_spending_numeric
# Independent Variable (predictor): semester_numeric
lm_course_spending <- lm(online_course_spending_numeric ~ semester_numeric, data = payment_by_semester)

print("\nResumen del modelo de regresión (online_course_spending ~ semester):")
summary(lm_course_spending)

# We display the model through ggplot
avg_spending_semester %>%
  ggplot(aes(semester_numeric, avg_spending)) +
  geom_point(size = 4, color = "red", alpha = 0.5) +
  geom_smooth(formula = y~x, method = "lm", color = "#2cc99c") +
    labs(
      title = "Relación entre Semetre y Gastos en cursos online",
      subtitle = "Con línea de tendencia de regresión lineal",
      x = "Semestre Promedio",
      y = "Nivel de Gasto en Cursos Online"

    ) +
    scale_x_continuous(
      breaks = payment_by_semester$semester_numeric,
      labels = payment_by_semester$semester
    ) +
    scale_y_continuous(
      breaks = payment_by_semester$online_course_spending_numeric,
      labels = payment_by_semester$online_course_spending
    ) + 
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title = element_text(face = "bold"),
    )

# OBSERVATIONS:
# 1) we must considered that the correlation coefficient is equal to 0.067, very approximate to 0
# which indicates that there's a very weak positive correlation, we could even say that's nonexistent

# 2) When it comes to the semester_numeric estimate: A value of 0.04167 means that, on average, 
# for every semester a student progresses, their estimated spending level increases 
# by 0.04167 units on the numerical spending scale. 
# This is a very small change

# 3) Regarding the Multiple R-squared: This indicates that only 0.451% of the 
# variability in spending level (online_course_spending_numeric) is 
# explained by the semester (semester_numeric) in the model. 
# An R-squared value this close to zero suggests the model is 
# very poor at explaining the variability of the dependent variable

# CONCLUSIONS:

# Based on these results, 
# the linear regression model suggests that there is 
# no statistically significant linear relationship
# between the student's current semester and 
# the amount of money they are willing to spend on online courses.

# Although the regression line might display a slight upward slope (due to the 0.04167 coefficient), 
# the p-values indicate that this slope is so small, and the variability in the data is so large, 
# that this observed relationship could easily be due to random chance within the sample. 
# In practice, it would not be considered a useful or reliable predictor for spending.

# By having more data, 
# we could probably observe a more strong relation between semesters and 
# the amount of money students spend on online courses as they progress in the career








# ----------------------------------------------------------------------------------
# REGRESION MODEL 2 (Linear Regression): Relation between semester and English level
# ----------------------------------------------------------------------------------

# This is for the SECOND research question

# Necessary for ordering data related to English proficiency level
english_level_order <- c("Básico", "Intermedio", "Avanzado")

# We select and adjust the data we need
english_level_by_semester <- project_data %>%
  mutate(
    # We convert english_level to order factor and then to numeric (1, 2, 3)
    english_level = factor(english_level, levels = english_level_order),
    english_level_numeric = as.numeric(english_level),
    
    # We convert semester to order factor and then to numeric (1 to 10)
    semester = factor(semester, levels = semesters_order),
    semester_numeric = as.numeric(semester)
  )

# We calculate the average English level by semester, this is what will be displayed in the model

avg_english_level_by_semester <- english_level_by_semester %>%
  group_by(semester, semester_numeric) %>%
  summarise(
    avg_english_level = mean(english_level_numeric, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(semester_numeric)

# We calculate the correlation coefficient, in order to estimate the correlation 
# between variables and direction of the lineal relation
r <- cor(english_level_by_semester$semester_numeric, english_level_by_semester$english_level_numeric)

# --- Adjust Lineal regression model ---
# We see the model summary for interpretation of coefficients, R-squared, etc.
# Dependent Variable (response): english_level_numeric
# Independent Variable (predictor): semester_numeric
lm_english_semester <- lm(english_level_numeric ~ semester_numeric, data = english_level_by_semester)

print("\nResumen del modelo de regresión (english_level ~ semester):")
summary(lm_english_semester)

# We display the model through ggplot
ggplot(avg_english_level_by_semester, aes(x = semester_numeric, y = avg_english_level)) +
  geom_point(size = 4, color = "#4CAF50") + # Puntos para los promedios de semestre
  geom_smooth(method = "lm", se = TRUE, color = "#FFC107", linetype = "dashed") + # Línea de regresión
  labs(
    title = "Relación entre Nivel de Inglés y Semestre Académico",
    subtitle = "Con línea de tendencia de regresión lineal",
    x = "Semestre Promedio",
    y = "Nivel de inglés"
  ) +
  scale_x_continuous(
    breaks = avg_english_level_by_semester$semester_numeric,
    labels = avg_english_level_by_semester$semester
  ) +
  scale_y_continuous(
    breaks = english_level_by_semester$english_level_numeric,
    labels = english_level_by_semester$english_level
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


# OBSERVATIONS:
# 1) we must considered that the correlation coefficient is equal to 0.126, slightly approximate to 0
# which indicates that there's a very weak positive correlation

# 2) When it comes to the semester_numeric estimate: A value of 0.04762 means that, 
# on average, for each additional semester a student progresses, 
# their English level is predicted to increase by a 
# very small amount (less than 0.05 units on the English level scale of 1, 2, 3). 
# This indicates a very slight positive trend.

# 3) Regarding the Multiple R-squared: A value of 0.01598 means that only 1.6% 
# of the variability in English level (english_level_numeric) is explained by 
# the student's semester (semester_numeric) in the model. 
# An R-squared value this close to zero indicates that the model is very 
# poor at explaining the variance in the dependent variable.

# CONCLUSIONS:

# The linear regression model suggests that there is no 
# statistically significant linear relationship between a student's academic semester 
# and their English level.

# While the regression line might show a 
# very slight upward trend (due to the small positive coefficient), 
# the high p-values for semester_numeric and the overall model, 
# combined with the very low R-squared, indicate that this observed 
# trend is weak and likely due to random chance in the sample. 
# Based on these data, a student's semester, by itself, does 
# not appear to be a meaningful linear predictor of their English proficiency.

# By having more data, 
# we could probably observe a more strong relation between semesters and 
# the develop of students'English level through semesters
