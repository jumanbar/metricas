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
    drozd = `CLOROFILA (DRODZ)`,
    mishra = `CLOROFILA (MISHRA)`,
    rn = `CLOROFILA (RN)`,
    mdn = `CLOROFILA (MDN)`
  ) %>%
  pivot_longer(drozd:mdn)

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
    zhan = `TURBIDEZ (ZHAN)`,
    delegido = `TURBIDEZ (DELEGIDO)`,
    maciel.v1 = `TURBIDEZ (MACIEL B6)`,
    maciel.v2 = `TURBIDEZ (MACIEL B8A)`,
    rn = `TURBIDEZ (RN)`
  ) %>%
  pivot_longer(zhan:rn)

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
    ficek = `CDOM (FICEK)`,
    chen.v1 = `CDOM (CHEN v1)`,
    chen.v2 = `CDOM (CHEN v2)`,
    rn = `CDOM (RN)`
  ) %>%
  pivot_longer(ficek:rn)

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
  xlab('Clorofila-a (IN SITU)') +
  ylab('ESTIMACIÓN')

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
  xlab('Turbidez (IN SITU)') +
  ylab('ESTIMACIÓN')

## cdom ----
rng_cdom <- range(c(range(dcdom$in_situ), range(dcdom$value)))
dcdom %>%
  ggplot() +
  aes(in_situ, value) +
  scale_y_continuous(limits = rng_cdom) +
  scale_x_continuous(limits = rng_cdom) +
  geom_abline(slope = 1, intercept = 0, alpha = .7, color = 'red') +
  facet_wrap(vars(name)) +
  coord_fixed() +
  geom_point(alpha = .5) +
  xlab('CDOM (IN SITU)') +
  ylab('ESTIMACIÓN')


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
cloa %>% filter(metricas %in% selmet) # En todas mejor rn
turb %>% filter(metricas %in% selmet) # En todas mejor rn
cdomv %>% filter(metricas %in% selmet) # En todas mejor rn
cdomt %>% filter(metricas %in% selmet) #




