library(tidyverse)


# DATOS -------

## cloa ----
dcloa <- read_delim(
  "datos/cloa.csv",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  select(
    calib = `1CALI/2VALI`,
    in_situ = `CLOROFILA IN SITU`,
    Drozd = `CLOROFILA (DRODZ)`,
    Mishra = `CLOROFILA (MISHRA)`,
    RN = `CLOROFILA (RN)`,
    MDN = `CLOROFILA (MDN)`
  ) %>%
  pivot_longer(Drozd:MDN)

## turb ----
dturb <- read_delim(
  "datos/turb.csv",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  select(
    calib = `1CALI/2VALI`,
    in_situ = `TURBIDEZ IN SITU`,
    Zhan = `TURBIDEZ (ZHAN)`,
    Delegido = `TURBIDEZ (DELEGIDO)`,
    Maciel.v1 = `TURBIDEZ (MACIEL B6)`,
    Maciel.v2 = `TURBIDEZ (MACIEL B8A)`,
    RN = `TURBIDEZ (RN)`
  ) %>%
  pivot_longer(Zhan:RN)

## cdom ----
dcdom <- read_delim(
  "datos/cdom.csv",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  select(
    # calib = `1CALI/2VALI`,
    in_situ = `CDOM IN SITU`,
    Ficek = `CDOM (FICEK)`,
    Chen.v1 = `CDOM (CHEN v1)`,
    Chen.v2 = `CDOM (CHEN v2)`,
    RN = `CDOM (RN)`
  ) %>%
  pivot_longer(Ficek:RN)

# SCATTERPLOTS --------

## cloa ----
rng_cloa <- range(c(range(dcloa$in_situ), range(dcloa$value)))
dcloa %>%
  filter(calib == 2) %>%
  ggplot() +
  aes(in_situ, value) +
  scale_y_log10(limits = rng_cloa) + scale_x_log10(limits = rng_cloa) +
  geom_abline(slope = 1, intercept = 0, alpha = .7, color = 'red') +
  facet_wrap(vars(name)) +
  coord_fixed() +
  geom_point(alpha = .5) +
  xlab("Medido") +
  ggtitle("Clorofila-a (ug/l)") +
  ylab("Estimado") +
  theme_bw()

ggsave(filename = "cloa.png", width = 110, height = 110, units = "mm")

## turb -----
rng_turb <- range(c(range(dturb$in_situ), range(dturb$value)))
dturb %>%
  filter(calib == 2) %>%
  ggplot() +
  aes(in_situ, value) +
  scale_y_continuous(limits = rng_turb) +
  scale_x_continuous(limits = rng_turb) +
  geom_abline(slope = 1, intercept = 0, alpha = .7, color = 'red') +
  facet_wrap(vars(name)) +
  coord_fixed() +
  geom_point(alpha = .5) +
  xlab("Medido") +
  ylab("Estimado") +
  ggtitle("Turbidez (NTU)") +
  theme_bw()

ggsave(filename = "turb.png", width = 110 * 4 / 3, height = 110, units = "mm")

## cdom ----
rng_cdom <- range(c(range(dcdom$in_situ), range(dcdom$value)))
dcdom %>%
  # filter(calib == 2) %>%
  ggplot() +
  aes(in_situ, value) +
  scale_y_continuous(limits = rng_cdom) +
  scale_x_continuous(limits = rng_cdom) +
  geom_abline(slope = 1, intercept = 0, alpha = .7, color = 'red') +
  facet_wrap(vars(name)) +
  coord_fixed() +
  geom_point(alpha = .5) +
  xlab("Medido") +
  ylab("Estimado") +
  ggtitle("CDOM (m-1)") +
  theme_bw()

ggsave(filename = "cdom.png", width = 110, height = 110, units = "mm")

# METRICAS -------

cloa <- read_delim("datos/comparativo_cloa.csv",
                   delim = "\t", escape_double = FALSE,
                   trim_ws = TRUE)

turb <- read_delim("datos/comparativo_turb.csv",
                   delim = "\t", escape_double = FALSE,
                   trim_ws = TRUE)

cdomv <- read_delim("datos/comparativo_cdom_validados.csv",
                    delim = "\t", escape_double = FALSE,
                    trim_ws = TRUE)

cdomt <- read_delim("datos/comparativo_cdom_todo.csv",
                    delim = "\t", escape_double = FALSE,
                    trim_ws = TRUE)

cloa$metricas <- gsub("^chl_", "", cloa$metricas)
turb$metricas <- gsub("^chl_", "", turb$metricas)
cdomv$metricas <- gsub("^chl_", "", cdomv$metricas)
cdomt$metricas <- gsub("^chl_", "", cdomt$metricas)


# TEXTO -----
# Evaluador1: Si es posible mencionar el tipo de modelos desarrollados.

# Evaluadora2: El trabajo es interesante y pertinente pero no se proporcionan
# los resultados, se hace referencia a que hubo diferencias importantes en los
# modelos de estimación de calidad del agua y que los modelos locales fueron más
# precisos pero no dice cuán precisos fueron, qué grado de diferencia tuvieron
# los modelos entre sí, en qué se basaron para seleccionar uno y cuál
# seleccionaron, etc.


# Como son varias métricas entiendo que deberíamos hacer una selección de esas y
# hacer algo como el MNR o en su defecto simplemente hacer un análisis métrica a
# métrica (de las seleccionadas).
#
# Para tener una referencia, para el MNR entiendo que usamos: MSA, epsilon,
# beta, intercepto y pendiente. De sumar alguna otra métrica pienso que tal vez
# podrían ser  AEmedian y RMSLE.


# https://www.data-to-viz.com/caveat/spider.html

# https://r-graph-gallery.com/spider-or-radar-chart.html


# COMPARACION ----

cloa$metricas

selmet <- c('n', 'min', 'mean', 'median', 'max', 'MSA',
            'Epsilon', 'Beta', 'Int.TheilSen', 'Slope.TheilSen')
cloa %>% filter(metricas %in% selmet) # En todas mejor RN
turb %>% filter(metricas %in% selmet) # En todas mejor RN
cdomv %>% filter(metricas %in% selmet) # En todas mejor RN
cdomt %>% filter(metricas %in% selmet) #


names(cloa)[2:5] <- c("Mishra", "MDN", "Drozd", "RN")
names(turb)[2:6] <- c("Zhan", "Delegido", "Maciel.v1", "Maciel.v2", "RN")
names(cdomv)[2:5] <- c("Ficek", "Chen.v1", "Chen.v2", "RN")
names(cdomt)[2:5] <- c("Ficek", "Chen.v1", "Chen.v2", "RN")

niveles <- c("Slope.TheilSen", "Int.TheilSen", "Beta", "Epsilon", "MSA")
breaks  <- c("RN", "[alt]", "Valor óptimo")

plot_metricas <- function(.data, breaks, niveles) {
  d <- .data %>%
    filter(metricas %in% selmet[-1:-5]) %>%
    mutate(metricas = factor(metricas, levels = niveles),
           optimo = if_else(metricas == "Slope.TheilSen", 1, 0)) %>%
    pivot_longer(-c(metricas, optimo, RN))

  p <- d %>% ggplot() + aes(metricas, RN) +
    facet_wrap(vars(name)) +
    theme_minimal() +
    geom_segment(aes(x = metricas, xend = metricas, y = RN, yend = value),
                 color = "grey") +
    geom_point(aes(y = RN, color = breaks[1], size = breaks[1]), alpha = .8) +
    geom_point(aes(y = value, color = breaks[2], size = breaks[2]), alpha = .8) +
    geom_point(aes(y = optimo, color = breaks[3], size = breaks[3])) +
    coord_flip() +
    ylab("") +
    xlab("") +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 10),
      strip.text = element_text(size = 14),
      legend.position = "bottom"
    ) +
    scale_color_manual("",
                       breaks = breaks,
                       values = c("#69b3a2", "grey", "black")) +
    scale_size_manual("",
                      breaks = breaks,
                      values = c(5, 3, .5))
  print(p)
}

plot_metricas(cloa, breaks, niveles)
ggsave(filename = "cloa_vs.png", width = 140, height = 80, units = "mm")

plot_metricas(turb, breaks, niveles)
ggsave(filename = "turb_vs.png", width = 140, height = 140, units = "mm")

plot_metricas(cdomv, breaks, niveles)
ggsave(filename = "cdomv_vs.png", width = 140, height = 80, units = "mm")

plot_metricas(cdomt, breaks, niveles)
ggsave(filename = "cdomt_vs.png", width = 140, height = 80, units = "mm")
