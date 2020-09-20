#Tema general para las gráficas---------------------------------------------
general_theme <- theme(legend.position = "top",
                       legend.direction = "horizontal",
                       legend.box = "horizontal",
                       legend.margin=margin(t = 0, unit='cm'),
                       legend.spacing.x = unit(0, 'cm'),
                       legend.key=element_blank(),
                       legend.text = element_text(size = 9.6),
                       legend.title = element_text(size = 9.6),
                       axis.title = element_text(size = 9.6),
                       panel.background = element_rect(fill = "gray97"),
                       plot.title = element_text(hjust = 0.5))
#Función para crear una representación gráfica de las observaciones en el estudio de supervivencia-------------------
#inicio: Tiempo de inicio de las observaciones
#t_fallo: Tiempo de fallo para cada una de las observaciones
#censura?: Indicador binario para la censura: 0-no censura, 1-censura
survival_study_graph <- function(inicio = 0, t_fallo, censura){
  #n: Cantidad de individuos
  #censura: número de la forma para los puntos
  tibble(observaciones = t_fallo, n = seq_along(t_fallo), 
         inicio = inicio ,censura = factor(censura)) %>% 
    ggplot(aes(x = observaciones, y = n))+
    #Graficación del tiempo de origen
    geom_point(aes(x = inicio), shape = 19, size = 2)+
    #Graficación del tiempo de fallo
    geom_point(aes(shape=censura), size = 3)+
    #Graficación de las lineas
    geom_segment(aes(x = inicio, xend = t_fallo, yend = n))+
    scale_shape_manual(values=c(4, 0), labels = c("Fallo:", "Censura:"),
                       guide = guide_legend(label.position = "left",
                                            label.hjust = 0.5))+
    labs(shape = NULL, x = "Tiempo", y = "Observaciones")+
    general_theme
}
#Funciones para el cálculo de superviviencia-----------------------------
suma_regresiva <- function(vector){
  c <- sum(vector)
  for(i in 1:length(vector)-1){
    c[i+1] <- sum(vector[1:(length(vector)-i)])
  }
  c
}

suma_a_saltos <- function(vector){
  c <- sum(vector)
  for(i in 2:length(vector)){
    c[i] <- sum(vector[i:(length(vector))])
  }
  c
}