# PROYECTO AVANCES - A4_OCHOA_QUINTANA_RANGEL

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
library(viridis) #Library for ggplot colors when there are too many values


# Set working directory

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Setting data source and doing some initial cleaning

project_data <-read_excel('A3_Ochoa_Quintana_Rangel.xlsx') %>% 
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
) # Neccesary for ordering data related to semesters

rm(column_names)

# print(colnames(project_data))
# print(sapply(project_data, class))

# FIRST VISUALIZATION: Proportion of Platforms used by semester

platforms_by_semester <- project_data %>% 
  dplyr::select( 
      semester,
      tech_learning_platforms,
    ) %>% 
  separate_rows(tech_learning_platforms, sep=",") %>% # We separate platforms into different rows, we make a long dataset
  dplyr::mutate(
    tech_learning_platforms = trimws(tech_learning_platforms), # We clean blank spaces
    tech_learning_platforms = case_when(tech_learning_platforms %in% c("Código facilito") ~ "Codigofacilito",
                                        tech_learning_platforms %in% c("IA tools", "IA") ~ "IA",
                                        tech_learning_platforms %in% c("google") ~ "Google",
                                        TRUE ~ tech_learning_platforms
                                        ), # We clean values given by the Otras option
    semester = factor(semester, levels = semesters_order, ordered = TRUE) # We convert the semester column into a factor so it appears order in the plot
  ) %>% 
  count(semester, tech_learning_platforms, name = "count") # We count how many times a platform was selected by students in a semester

ggplot(platforms_by_semester, aes(x=semester, y = count, fill=tech_learning_platforms)) + 
  geom_bar(stat = "identity", position="fill") +
  labs(
    title = "Uso de Plataformas de Aprendizaje por Semestre",
    x = "Semestre",
    y = "Número de Estudiantes",
    fill = "Plataforma"
  ) +
  scale_y_continuous(labels = scales::percent) + # Format Y axis as percentage
  theme_solarized(light = FALSE) + scale_color_solarized() + # We specify the theme -> light = FALSE if we want dark mode
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # We adjust X axis values labels presentation by giving them angle
  scale_fill_viridis_d(option = "D") # To change colors use: "A", "B", "C", "E" 


# SECOND VISUALIZATION: Frequency of Platforms' factors of use by semester

choice_factors_by_semester <- project_data %>% 
  dplyr::select( 
    semester,
    platform_choice_factors,
  ) %>% 
  separate_rows(platform_choice_factors, sep=",") %>% 
  dplyr::mutate(
    platform_choice_factors = trimws(platform_choice_factors),
    semester = factor(semester, levels = semesters_order, ordered = TRUE) 
  ) %>% 
  count(semester, platform_choice_factors, name = "count")  # We count the occurrences for each combination of semester and factor


ggplot(choice_factors_by_semester, aes(x = semester, y = platform_choice_factors, size = count, color = count)) +
  geom_point(alpha = 0.8) + # We adjust transparency in case points overlap each other
  labs(
    title = "Frecuencia de Factores de Elección por Semestre",
    x = "Semestre",
    y = "Factor de Elección de Plataforma"
  ) +
  scale_size_area(max_size = 10, name = "Frecuencia") + # Scale for points' size
  scale_color_viridis_c(option = "A", name = "Frecuencia") + # Continuous color scale
  theme_solarized( light = FALSE ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# THIRD VISUALIZATION: Proportion of students who have spent money on courses by semester

has_spent_by_semester <- project_data %>% 
  dplyr::select( 
    semester,
    paid_for_online_course,
  ) %>% 
  group_by(semester) %>%
  count(paid_for_online_course, name="people_who_has_paid") %>%
  dplyr::mutate(
    proportion = people_who_has_paid / sum(people_who_has_paid),
    semester = factor(semester, levels = semesters_order, ordered = TRUE) 
  ) %>% 
  ungroup() %>%
  filter(paid_for_online_course == "Sí")   # Filter records where paid_for_online_course is "Sí"


ggplot(has_spent_by_semester, aes(x = semester, y = proportion, group = 1)) +
  geom_line(color = "#2cc99c", linewidth = 1) + # Line to show the tendency
  geom_point(color = "#2cc99c", size = 3) +     # Points for each semester
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            vjust = -0.8, color = "white", size = 3) + # Show percentage in each point
  labs(
    title = "Proporción de Estudiantes que Pagan por Cursos Online por Semestre",
    x = "Semestre",
    y = "Proporción de Estudiantes que Pagan"
  ) +
  scale_y_continuous(labels = scales::percent) + 
  theme_solarized(light = FALSE) + scale_color_solarized()


# FOURTH VISUALIZATION: Ways to avoid obstacles because of the english barrier


english_obstacle_solutions <- project_data %>% 
  separate_rows(solution_english_barrier, sep=",") %>%
  mutate(
    solution_english_barrier = trimws(solution_english_barrier) # Elimina espacios extra
  ) %>%
  # Asegúrate de eliminar posibles entradas vacías después de trimws o si hay comas dobles
  filter(solution_english_barrier != "El inglés no supone un obstáculo para mí")

print("\nDatos de Soluciones Desanidados (fragmento):")
print(head(english_obstacle_solutions))


solution_proportions <- english_obstacle_solutions %>%
  count(solution_english_barrier, name = "total_conteo") %>% # Cuenta la frecuencia de cada solución
  mutate(proportion = total_conteo / sum(total_conteo)) %>%  # Calcula la proporción
  arrange(desc(proportion)) # Ordena de la solución más frecuente a la menos frecuente

print("\nProporciones de Soluciones:")
print(solution_proportions)

ggplot(solution_proportions, aes(x = reorder(solution_english_barrier, proportion), y = proportion)) +
  geom_col(fill = "#2cc99c") + # Barras de columna con un color azul agradable
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            hjust = -0.1, # Coloca la etiqueta ligeramente a la derecha de la barra
            size = 3.5,
            color = "black") +
  labs(
    title = "Proporción de Alternativas para Superar la Barrera del Inglés",
    x = "Alternativa o Solución",
    y = "Proporción de Menciones"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) + # Expande el eje Y
  coord_flip() + # ¡Hace las barras horizontales! Crucial para etiquetas largas
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10), # Ajusta el tamaño de la fuente de las etiquetas de las soluciones
    plot.title = element_text(hjust = 0.5, face = "bold"), # Título centrado y en negrita
    axis.title = element_text(face = "bold") # Etiquetas de los ejes en negrita
  )

# FIFTH VISUALIZATION: Proportion of what students that do not know english and have no resources do


students_with_obstacles <- project_data %>%
  filter(
    is_english_obstacle == "Sí",
    alt_high_cost_course == "Busco alternativas gratuitas"
  )

print("\nDatos Filtrados (fragmento, solo los relevantes):")
print(head(students_with_obstacles))

students_with_obstacles <- students_with_obstacles %>%
  separate_rows(solution_english_barrier, sep = ",") %>%
  mutate(solution_english_barrier = trimws(solution_english_barrier)) %>%
  filter(solution_english_barrier != "")

print("\nDatos de Soluciones Desanidados (fragmento):")
print(head(students_with_obstacles))


alternatives_proportions <- students_with_obstacles %>%
  count(solution_english_barrier, name = "total_conteo_solucion") %>%
  mutate(proportion = total_conteo_solucion / sum(total_conteo_solucion)) %>%
  # Opcional: Ordenar por proporción para mejor visualización
  arrange(desc(proportion))

print("\nProporciones de Soluciones:")
print(alternatives_proportions)

ggplot(alternatives_proportions, aes(x = reorder(solution_english_barrier, proportion), y = proportion)) +
  geom_col(fill = "#2f48c7") + # Barras de columna (geom_col usa directamente el valor 'y')
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            hjust = -0.1, # Ajusta la posición del texto (a la derecha de la barra si x es texto)
            size = 3.5,
            color = "black") +
  labs(
    title = "Proporción de Soluciones para la Barrera del Inglés",
    subtitle = "Entre estudiantes que tienen el inglés como obstáculo y buscan alternativas gratuitas",
    x = "Solución",
    y = "Proporción de Menciones"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) + # Expande el eje Y para el texto
  coord_flip() + # Voltear el gráfico para que las barras sean horizontales (mejor para etiquetas largas)
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10), # Ajustar tamaño de texto de las soluciones
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# SIXTH VISUALIZATION: Proportion of platforms used by students that do no pay anything

datos_filtrados_no_pago <- project_data %>%
  filter(paid_for_online_course == "No")

print("\nDatos Filtrados (solo estudiantes que NO han pagado, fragmento):")
print(head(datos_filtrados_no_pago))

# --- Paso 2: Desanidar la variable de plataformas ---
datos_desanidados_plataformas_filtrado <- datos_filtrados_no_pago %>%
  separate_rows(tech_learning_platforms, sep = ",") %>%
  mutate(
    tech_learning_platforms = trimws(tech_learning_platforms), # We clean blank spaces
    tech_learning_platforms = case_when(tech_learning_platforms %in% c("Código facilito") ~ "Codigofacilito",
                                        tech_learning_platforms %in% c("IA tools", "IA") ~ "IA",
                                        tech_learning_platforms %in% c("google") ~ "Google",
                                        TRUE ~ tech_learning_platforms
    ), # We clean values given by the Otras option
  )

print("\nPlataformas Desanidadas (fragmento):")
print(head(datos_desanidados_plataformas_filtrado))

# --- Paso 3: Calcular las proporciones de cada plataforma ---
proporciones_plataformas_no_pago <- datos_desanidados_plataformas_filtrado %>%
  count(tech_learning_platforms, name = "total_conteo") %>% # Cuenta la frecuencia
  mutate(proportion = total_conteo / sum(total_conteo)) %>% # Calcula la proporción
  arrange(desc(proportion)) # Ordena de mayor a menor proporción

print("\nProporciones de Plataformas para NO Pagadores:")
print(proporciones_plataformas_no_pago)

# --- Paso 4: Graficar las proporciones ---
ggplot(proporciones_plataformas_no_pago, aes(x = reorder(tech_learning_platforms, proportion), y = proportion)) +
  geom_col(fill = "#a0e62c") + # Barras de columna con un color azul claro
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            hjust = -0.1, # Ajusta la posición de la etiqueta (derecha de la barra)
            size = 3.5,
            color = "black") +
  labs(
    title = "Proporción de Plataformas de Online Autónomo utilizadas",
    subtitle = "Por estudiantes que NO han pagado por un curso online",
    x = "Plataforma de Aprendizaje Online",
    y = "Proporción de Menciones"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) + # Expande el eje Y para las etiquetas
  coord_flip() + # ¡Barras horizontales! Ideal para etiquetas de categoría largas
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10), # Ajusta el tamaño de la fuente de las etiquetas de las plataformas
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


##################### Pie charts

# Necesitamos contar las ocurrencias de cada género y calcular los porcentajes.
# También calcularemos las posiciones para las etiquetas de texto.
data_for_pie <- project_data %>%
  group_by(gender) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    percentage = count / sum(count),
    # Crear una nueva columna para la leyenda combinando nombre y porcentaje
    # Usamos fct_reorder para asegurar que el orden de la leyenda coincida con el del gráfico si es necesario,
    # aunque arrange(desc(percentage)) al final también ayuda.
    gender_label = paste0(gender, " (", scales::percent(percentage, accuracy = 0.1), ")")
  ) 
  # Opcional: ordenar las rebanadas por tamaño para una mejor visualización si no hay un orden lógico preestablecido
  # Si quieres el orden alfabético original de "Hombre", "Mujer", "No binario", omite este arrange
  # arrange(desc(percentage)) %>%
  # # Convertir gender_label a factor con el orden deseado para que la leyenda lo respete
  # mutate(gender_label = factor(gender_label, levels = gender_label))


# --- Elaborar el gráfico circular sin etiquetas en el pastel, con etiquetas en la leyenda ---
ggplot(data_for_pie, aes(x = "", y = percentage, fill = gender_label)) + # Usamos gender_label para el fill
  geom_bar(width = 1, stat = "identity", color = "white", linewidth = 1) + # Borde blanco entre rebanadas
  coord_polar("y", start = 0) +
  labs(
    title = "Participación de los estudiantes por semestre en la encuesta",
    fill = "Género" # El título de la leyenda
  ) +
  theme_void() + # Tema minimalista sin ejes
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold"),
    legend.position = "right", # Posición de la leyenda
    legend.text = element_text(size = 10) # Tamaño del texto de la leyenda
  )

# print("Datos Agregados para el Gráfico Circular con Etiquetas de Leyenda:")
# print(data_for_pie)


##################### Some Calculations

mean(project_data$age);

frecuencias <- table(project_data$english_level)
print("Tabla de Frecuencias:")
print(frecuencias)

moda_posicion <- which.max(frecuencias)
moda_nivel <- names(frecuencias)[moda_posicion]

print(paste("La moda del nivel de inglés es:", moda_nivel))

frecuencias <- table(project_data$semester)
print("Tabla de Frecuencias:")
print(frecuencias)

moda_posicion <- which.max(frecuencias)
moda_nivel <- names(frecuencias)[moda_posicion]

print(paste("La moda de semestre es:", moda_nivel))

# Código de regresión no utilizado

# avg_semester_by_english_level <- english_level_by_semester %>%
#   group_by(english_level, english_level_numeric) %>%
#   summarise(
#     avg_semester = mean(semester_numeric, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   arrange(english_level_numeric)
# 
# r <- cor(english_level_by_semester$english_level_numeric, english_level_by_semester$semester_numeric)
# 
# # --- Ajustar el modelo de regresión lineal ---
# # Variable dependiente (respuesta): semester_numeric
# # Variable independiente (predictora): english_level_numeric
# lm_semester_english<- lm(semester_numeric ~ english_level_numeric, data = english_level_by_semester)
# 
# # --- Ver el resumen del modelo para interpretar los coeficientes ---
# print("\nResumen del modelo de regresión (semester ~ english_level):")
# summary(lm_semester_english)
# 
# ggplot(avg_semester_by_english_level, aes(x = english_level_numeric, y = avg_semester)) +
#   geom_point(size = 4, color = "#4CAF50") + # Puntos para los promedios de semestre
#   geom_smooth(method = "lm", se = TRUE, color = "#FFC107", linetype = "dashed") + # Línea de regresión
#   labs(
#     title = "Relación entre Nivel de Inglés y Semestre Académico",
#     subtitle = "Con línea de tendencia de regresión lineal",
#     x = "Nivel de Inglés",
#     y = "Semestre Promedio"
#   ) +
#   scale_x_continuous(
#     breaks = avg_semester_by_english_level$english_level_numeric, # Marcas numéricas
#     labels = avg_semester_by_english_level$english_level # Etiquetas de texto del nivel de inglés
#   ) +
#   scale_y_continuous(breaks = 1:10) + # Tus semestres van del 1 al 10
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     axis.title = element_text(face = "bold")
#   )
