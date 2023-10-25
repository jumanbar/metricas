# metricas

Metricas para evaluar modelos predictivos de clorofila en agua a partir de firmas espectrales y/o productos satelitales

Ver documentacion de las [metricas](metricas.Rmd)

# Shiny-app

Para correr la aplicación shiny desde tu PC, usando RStudio:

- Abrir RStudio en la carpeta raíz (recomendación: crear un proyecto de RStudio en esta carpeta y abrir ese proyecto; al momento de crearlo ya está abierto allí, así que en realidad lo segundo sería redundante)

- Abrir cualquiera de los archivos de la app: global.R, ui.R o server.R

- Apretar el botón de correr la app ("Run App", creo)

También se puede visitar la app en vivo disponible aquí:

https://jumanbar.shinyapps.io/metricas/

# Figuras VIIJEA

El script para crear las figuras está en la carpeta R: [R/figuras_VIIJEA.R](R/figuras_VIIJEA.R)

El código de este archivo asume que se está trabajando en el proyecto RStudio cuya raíz es la raíz de este repositorio. Esto es relevante, porque si no será imposible importar los datos (encontrados en la carpeta [datos](datos)
