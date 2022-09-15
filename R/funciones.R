num_ok <- function(x) {
  !(is.na(x) | is.nan(x) | is.infinite(x))
}

#' Rango de predicciones para Theil-Sen
#'
#' Para hacer el geom_ribbon
#'
#' @param datos
#' @param po
#'
#' @examples
#' filter(dcut, sat == 'msi', corr == 'acolite', model == 'mdn') %>%
#'   ts_range(TRUE)
ts_range <- function(datos, po) {

  M <- datos$m
  E <- datos$e

  if (po) {
    x <- log10(M)
    y <- log10(E)
  } else {
    x <- log10(E)
    y <- log10(M)
  }

  the <- lmod(x, y, deming::theilsen)
  # print(the)
  X <- matrix(c(rep.int(1, length(x)), x), ncol = 2)
  beta <- the$coefficients
  # predictor <- drop(X %*% beta)
  # y <- 10 ** predictor
  ci <- the$ci
  v <- cbind(beta, ci, c(ci[1, 1], ci[2, 2]), c(ci[1, 2], ci[2, 1]))
  pred <- drop(X %*% v)
  predictor_low  <- apply(pred[, -1], 1, min)
  predictor_high <- apply(pred[, -1], 1, max)
  out <- tibble::tibble(
    M = M,
    E = E,
    y = 10 ** pred[, 1],
    y_min = 10 ** predictor_low,
    y_max = 10 ** predictor_high
  ) %>%
    dplyr::arrange(M)
  return(out)
}

tsummary <- function (.data, columna) {
  require(magrittr)
  columna <- dplyr::enquo(columna)
  if (rlang::quo_name(columna) %in% c("n", "Min", "1er Cu", "Media",
                               "Mediana", "3er Cu", "Max")) {
    .data <- dplyr::rename(.data, x = {{ columna }})
    columna <- rlang::sym("x")
  }
  .data %>%
    dplyr::summarise(
      n = dplyr::n(),
      Min = min({{ columna }}, na.rm = TRUE),
      `1er Cu` = stats::quantile({{ columna }}, na.rm = TRUE, probs = 0.25),
      Media = mean({{ columna }}, na.rm = TRUE) %>% round(ifelse(. < 1, 5, 3)),
      Mediana = stats::median({{ columna }}, na.rm = TRUE),
      `3er Cu` = stats::quantile({{ columna }}, na.rm = TRUE, probs = 0.75),
      Max = max({{ columna }}, na.rm = TRUE)
    ) %>%
    dplyr::filter(!is.na(Mediana))
}

# . -----------

# METRICAS ----

#' Calcular métricas
#'
#' @param d Datos
#' @param po Para interceptos y pendientes, debe usar P vs O (E vs M)?
#' @examples
#' ## Tabla con columnas apropiadas (e y m):
#' d <- tibble(m = rnorm(100) + 9, e = m + rnorm(100))
#' calc_metricas(d)
calc_metricas <- function(d, po = TRUE) {
  # if (missing(perfo_list)) {
  #   perfo_list = c("AEmean", "AEmedian", "REmean",
  #                  "REmedian", "RMSE", "RMSLE", "Bias",
  #                  "Beta", "MSB", "MAE", "MSE", "MAPE", "Epsilon", "MWR",
  #                  "DMC", "DSD", "Int.OLS", "Slope.OLS", "Int.SMA", "Slope.SMA",
  #                  "Int.Theil-Sen", "Slope.Theil-Sen",
  #                  "SI", "R2.OLS")
  # }

  regresiones <- regs(d)

  # Calculo de todas las métricas:
  met <-
    d %>%
    # dplyr::group_by(sat, corr, model) %>%
    dplyr::summarise(n = dplyr::n(),
                     chl_min    = min(m, na.rm = TRUE),
                     chl_mean   = mean(m, na.rm = TRUE),
                     chl_median = median(m, na.rm = TRUE),
                     chl_max    = max(m, na.rm = TRUE),
                     AEmean     = aemean(e, m),
                     AEmedian   = aemedian(e, m),
                     REmean     = remean(e, m),
                     REmedian   = remedian(e, m),
                     RMSE       = rmse(e, m),
                     RMSLE      = rmsle(e, m),
                     mu         = mu(e, m),
                     MAE        = mae(e, m),
                     MAPE       = mape(e, m),
                     Bias       = bias(e, m),
                     Beta       = beta_(e, m),
                     MSB        = msb(e, m),
                     Epsilon    = epsilon(e, m),
                     MSA        = msa(e, m),
                     DMC        = dmc(e, m),
                     DSD        = dsd(e, m),
                     .groups = "drop")

  x <- c(unlist(met), unlist(regresiones))
  out <- tibble::tibble(metrica = names(x),
                        valor = x)
  return(out)
}

# . . . . . . . . . .  -----------

# Predicciones numericas -----
#' Median Absolute Error (ver Zhang et al. )
#'
#' MdAE (Morley et al. 2018)
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' aemean(dcut$value, dcut$in_situ_chl)
#' aemean(4, 4) # óptimo = 0
aemean <- function(e, m) {
  mean(abs(e - m), na.rm = TRUE)
}

#' Median Absolute Error (ver Zhang et al. )
#'
#' MdAE (Morley et al. 2018)
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' aemedian(dcut$value, dcut$in_situ_chl)
#' aemedian(4, 4) # óptimo = 0
aemedian <- function(e, m) {
  median(abs(e - m), na.rm = TRUE)
}

#' Porcentaje de sesgo simétrico signado
#'
#' Symmetric Signed Percentage Bias. No confundir con \code{link[base]{beta}}
#'
#' Sería una alternativa a Bias: en lugar de usar el **promedio** de las
#' diferencias entre medidas (m) y estimados (e), en escala logarítmica de base
#' 10, usa la **mediana** de estas diferencias, multiplicada por el signo de esa
#' mediana.
#'
#' @param e
#' @param m
#'
#' @return
#' @export
#'
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' beta_(dcut$value, dcut$in_situ_chl)
#' beta_(4, 4) # óptimo = 0
beta_ <- function(e, m) {
  z <- median(log10(e / m), na.rm = TRUE)
  return(sign(z) * (10 ^ abs(z) - 1))
}

#' Bias (Seegers et al. 2018)
#'
#' @param e
#' @param m
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' bias(dcut$value, dcut$in_situ_chl)
#' bias(4, 4) # óptimo = 1
bias <- function(e, m) {
  logQ <- log10(e / m)
  w <- which(is.infinite(logQ))
  x <- if (length(w)) {
    warning("Infinite log10(e/m) values were eliminated")
    logQ[-w]
  } else {
    logQ
  }
  z <- mean(x, na.rm = TRUE)
  return(10 ** z)
}

#' MSB (modificado de Seegers et al. 2018)
#'
#' Mean Symmetric Bias?
#'
#' Modificada de la bias original, para que siga el mismo criterio que beta_ o
#' epsilon, en lo que refiere a su interpretación: en vez de devolver 10^z,
#' devuelve 10^z -1.
#'
#' @param e
#' @param m
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' msb(dcut$value, dcut$in_situ_chl)
#' msb(4, 4) # óptimo = 0
msb <- function(e, m) {
  logQ <- log10(e / m)
  w <- which(is.infinite(logQ))
  x <- if (length(w)) {
    warning("Infinite log10(e/m) values were eliminated")
    logQ[-w]
  } else {
    logQ
  }
  z <- mean(x, na.rm = TRUE)
  return(sign(z) * (10 ** abs(z) - 1))
}

#' DMC (Zhang et al. 2015)
#'
#' Standard Deviation from the Mean
#'
#' @param e
#' @param m
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' dmc(dcut$value, dcut$in_situ_chl)
#' dmc(4, 4) # óptimo = 0
dmc <- function(e, m) {
  me <- mean(e, na.rm = TRUE)
  mm <- mean(m, na.rm = TRUE)
  return((me - mm) / mm)
}

#' DMC (Zhang et al. 2015)
#'
#' Standard Deviation of the Starndard Deviation
#'
#' @param e
#' @param m
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' dsd(dcut$value, dcut$in_situ_chl)
#' dsd(x <- rnorm(10, 9), x) # óptimo = 0
dsd <- function(e, m) {
  sde <- sd(e, na.rm = TRUE)
  sdm <- sd(m, na.rm = TRUE)
  return((sde - sdm) / sdm)
}

#' Precisión simétrica mediana
#'
#' Inspirado en Morley et al (2018)
#'
#' Median Symmetric Accuracy: MdSA
#'
#' Sería una alternativa al MAE: en lugar de usar el **promedio** de las
#' diferencias absolutas entre medidas (m) y estimados (e), en escala
#' logarítmica de base 10, usa la **mediana** de estas diferencias.
#'
#' @param e
#' @param m
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' epsilon(dcut$value, dcut$in_situ_chl)
#' epsilon(4, 4) # óptimo = 0
epsilon <- function(e, m) {
  y <- median(abs(log10(e / m)), na.rm = TRUE)
  return(10 ^ y - 1)
}

#' @describeIn rmse
lmod <- function(x, y, FUN = stats::lm, ...) {
  ok <- num_ok(x) & num_ok(y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) > 2 && length(y) > 2) {
    out <- FUN(y ~ x, ...)
  } else {
    out <- c(NA, NA)
  }
  return(out)
}

#' MAE (Seegers et al. 2018)
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' mae(dcut$value, dcut$in_situ_chl)
#' mae(4, 4) # óptimo = 1
mae <- function(e, m) {
  logQ <- log10(e / m)
  w <- which(is.infinite(logQ))
  x <- if (length(w)) {
    warning("Infinite log10(e/m) values were eliminated")
    logQ[-w]
  } else {
    logQ
  }
  y <- mean(abs(x), na.rm = TRUE)
  return(10 ** y)
}

#' MAPE (Pahlevan et al. 2018)
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' mape(dcut$value, dcut$in_situ_chl)
#' mape(4, 4) # óptimo = 0
mape <- function(e, m) {
  median(abs(e - m) / m, na.rm = TRUE)
}

#' MSE
#'
#' Es como MAE pero incluye un -1 para que sea más fácil la interpretación
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' msa(dcut$value, dcut$in_situ_chl)
#' msa(4, 4) # óptimo = 0
msa <- function(e, m) {
  logQ <- log10(e / m)
  w <- which(is.infinite(logQ))
  x <- if (length(w)) {
    warning("Infinite log10(e/m) values were eliminated")
    logQ[-w]
  } else {
    logQ
  }
  y <- mean(abs(x), na.rm = TRUE)
  return(10 ^ y - 1)
}

#' mu
#'
#' Inventada por mí (JMB)
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' mu(dcut$value, dcut$in_situ_chl)
#' mu(4, 4) # óptimo = 0
mu <- function(e, m) {
  r <- rmsle(e, m)
  return(10 ** r - 1)
}

#' Model Win Rate
#'
#' @param datos
#'
#' @return
#' @export
#'
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' mwr(dcut)
mwr <- function(datos) {
  datos %>%
    group_by(station_id, codigo_pto, in_situ_datetime,
             sat, corr, in_situ_chl) %>%
    summarise(win = win(in_situ_chl, value, model)) %>%
    ungroup() %>%
    group_by(sat, corr) %>%
    mutate(n = n()) %>%
    group_by(sat, corr, n) %>%
    tidyr::nest() %>%
    mutate(MWR = purrr::map(data, mwr_group)) %>%
    select(-data) %>%
    tidyr::unnest(MWR) %>%
    ungroup()
}

#' Model Win Rate para datos agrupados
#'
#' @param datos agrupados y con columna win ya preparada
mwr_group <- function(datos) {
  models <- sort(unique(datos$win))
  out <- numeric(length(models))
  for (i in 1:length(models)) {
    out[i] <- sum(datos$win == models[i]) / nrow(datos)
  }
  return(tibble(model = models, MWR = out))
}

# Normalized Ranking
nr <- function(x, perfo_name = '') {
  perfo_name <- perfo_name[[1]]
  # cat(">>>>> nr:", perfo_name, "\n")
  # if (perfo_name == "R2") browser()
  op <- (perfo_name %in%
           c("MAE", "Bias", "Slope", "Slope.sma", "Slope.theil", "R2",
             "MWR", "MNR", "MdNR", "MNR", "MDNR")) %>%
    as.numeric
  # if (perfo_name == 'Int') x <- 10 ** x
  if (perfo_name %in% c('n')) op <- max(x)
  scores <- abs(x - op)
  N <- length(x)
  out <- numeric(N)
  # Equivalent to a pp: 1 - ((rank(s, ties.method = 'min') - 1) / length(s))
  for (i in 1:N) out[i] <- sum(scores[i] <= scores) / N
  return(out)
}

#' Regresiones PO / OP
#'
#' @param datos
#' @param po
#'
#' @return
#' @export
#'
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' regre(dcut, TRUE)
regre <- function(datos, po = FALSE) {
  datos %>%
    group_by(sat, corr, model) %>%
    tidyr::nest() %>%
    mutate(reg = purrr::map(data, regs, po = po)) %>%
    select(-data) %>%
    tidyr::unnest(reg) %>%
    ungroup()
}


# Mean Relative Error
# (ver Zhang et al. )
remean <- function(e, m) {
  mean(abs(e - m) / m, na.rm = TRUE)
}

#' DMC (Zhang et al. 2015)
#'
#' Median Relative Error
#'
#' @param e
#' @param m
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' remedian(dcut$value, dcut$in_situ_chl)
#' remedian(x <- rnorm(10, 9), x) # óptimo = 0
remedian <- function(e, m) {
  median(abs(e - m) / m, na.rm = TRUE)
}

#' RMSE
#'
#' Root Mean Squared Error
#'
#' @param e valores estimados por el modelo
#' @param m valores medidos in-situ
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' rmse(dcut$value, dcut$in_situ_chl)
#' rmse(5, 5) # óptimo = 0
rmse <- function(e, m) {
  semean <- mean((e - m) ** 2, na.rm = TRUE)
  sqrt(semean)
}

#' RMSLE
#'
#' @param e Estimaciones
#' @param m Medidas
#' @examples
#' dcut <- readRDS("data/dcut.rds")
#' rmsle(dcut$value, dcut$in_situ_chl)
#' rmsle(4, 4) # óptimo = 0
rmsle <- function(e, m) {
  logQ <- log10(e / m)
  w <- which(is.infinite(logQ))
  x <- if (length(w)) {
    warning("Infinite log10(e/m) values were eliminated")
    logQ[-w]
  } else {
    logQ
  }
  return(sqrt(mean(x ^ 2, na.rm = TRUE)))
}

#' @describeIn rmse
regs <- function(datos, po = TRUE) {

  M <- datos$m
  E <- datos$e

  if (po) {
    x <- log10(M)
    y <- log10(E)
  } else {
    x <- log10(E)
    y <- log10(M)
  }

  # cat('---------\n')
  # print(datos)
  ols <- lmod(x, y)
  sma <- lmod(x, y, smatr::sma, method = "SMA")
  the <- lmod(x, y, RobustLinearReg::theil_sen_regression)

  if (!all(is.na(ols))) {
    s <- summary(ols)
    ols_i <- coef(s)[[1]]
    ols_s <- coef(s)[[2]]
    ols_i_pv <- s$coefficients[1, 4]
    ols_s_pv <- s$coefficients[2, 4]
    r2 <- s$adj.r.squared
    if (is.null(r2)) r2 <- NA_real_
  } else {
    ols_s <- NA_real_
    ols_i <- NA_real_
    ols_s_pv <- NA_real_
    ols_i_pv <- NA_real_
    r2 <- NA_real_
  }

  if (!all(is.na(sma))) {
    sma_i <- coef(sma)[[1]]
    sma_s <- coef(sma)[[2]]
  } else {
    sma_i <- NA_real_
    sma_s <- NA_real_
  }

  if (!all(is.na(the))) {
    the_i <- coef(the)[[1]]
    the_s <- coef(the)[[2]]
  } else {
    the_i <- NA_real_
    the_s <- NA_real_
  }

  tibble(Int.OLS = ols_i,
         Int.OLS.Pvalue = ols_i_pv,
         Slope.OLS = ols_s,
         Slope.OLS.Pvalue = ols_s_pv,
         R2.OLS = r2,
         Int.SMA = sma_i,
         Slope.SMA = sma_s,
         Int.TheilSen = the_i,
         Slope.TheilSen = the_s)
}

si <- function(S, I) {
  # Una prueba para combinar pendiente e intercepto:
  (S - 1) * I
}

#' Alias de beta_
#'
#' Symmetric Signed Percentage Bias (Morley et al., 2018). La diferencia con
#' beta_ es que usa log en base e, en lugar de base 10.
#'
#' @param e
#' @param m
#' @examples
#' n <- 10000
#' for (i in 1:n) {
#'   if (i == 1) s <- 0
#'   e <- rnorm(1000) + 20
#'   m  <- rnorm(1000) + 20
#'   ae <- (b <- beta_(e, m)) - (ss <- sspb(e, m)) < sqrt(.Machine$double.eps)
#'   if (ae) cat(s <- s + 1, 'de', n, '\r') else {
#'     print(b - ss)
#'   }
#' }
sspb <- function(e, m) {
  Qi <- e / m
  sign(median(log(Qi))) * (exp(abs(median(log(Qi)))) - 1)
}

#' Which model wins?
#'
#' @param e
#' @param m
#' @param model
#'
win <- function(e, m, model) {
  dif <- abs(e - m)
  model[which.min(dif)]
}

# . . . . . . . . . .  -----------

# Clasificacion binaria -----

f1_score <- function(precision, sensitivity, beta) {
  (1 + beta ** 2) * (precision * sensitivity) / ((beta ** 2) * precision + sensitivity)
}

# . -----------
# FIGURAS ----

#' Scatterplots 1
#'
#' @param .data
#' @param po
#' @param titulo
#' @param alpha
#' @param ts_slope para cuando la pendiente e intercepto de tabla corresponden a
#'   regresión theil-sen. en ese caso no se hace el geom_smooth con lm.
#' @param cutoff
#'
#' @examples
#' d <- tibble(m = rnorm(100) + 9, e = m + rnorm(100))
#' g_em(d)
g_em <- function(.data, cutoff, titulo, po = TRUE, alpha = 1, ts_slope = TRUE) {
  require(ggplot2)
  forma <- 21

  tms <- .data

  tms$M <- tms$m
  tms$E <- tms$e

  axismin <- min(c(tms$E, tms$M), na.rm = TRUE)
  axismax <- max(c(tms$E, tms$M), na.rm = TRUE)
  axislim <- c(axismin, axismax)

  axpr <- if (any(abs(log10(axislim)) > 4)) {
    scales::scientific(axislim, digits = 3)
  } else axislim

  aes_ <- if (po) {
    aes(x = M, y = E)
  } else {
    aes(x = E, y = M)
  }

  if (ts_slope) {
    tms <- ts_range(tms, po = po)
  }

  out <-
    tms %>%
    ggplot() + aes_ +
    # linetype: http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
    geom_abline(slope = 1, intercept = 0, linetype = 'longdash') +
    scale_y_log10(
      limits = axislim,
      breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
      labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
    ) +
    scale_x_log10(
      limits = axislim,
      breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
      labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
    ) +
    theme_bw()

  if (!missing(titulo)) {
    out <- out + ggtitle(titulo)
  }

  if (ts_slope) {
    tseg <- tms %>%
      dplyr::summarise(x1 = first(M), y1 = first(y),
                       x2 = last(M), y2 = last(y))
    out <- out +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = tseg,
                   color = "#e41a1c")
    out <- if (po) {
      out + geom_ribbon(aes(x = M, min = y_min, ymax = y_max),
                        fill = alpha("#e41a1c", .1), colour = NA)
    } else {
      out + geom_ribbon(aes(x = E, ymin = ymin, y_max = y_max),
                        fill = alpha("#e41a1c", .1), colour = NA)
    }
  } else {
    out <- out + geom_smooth(aes_, method = stats::lm, alpha = .1, size = .5)
  }

  print(out)

  if (!missing(cutoff)) {
    out <- out +
      geom_hline(yintercept = cutoff, color = "#377eb8", alpha = .35) +
      geom_vline(xintercept = cutoff, color = "#377eb8", alpha = .35)
  }

  out <- out + geom_point(shape = forma, color = "#e41a1c", alpha = alpha)

  expr_m <- expression('M (mg m' ^-3 * ')')
  expr_e <- expression('E (mg m' ^-3 * ')')

  out <- if (po) {
    out + xlab(expr_m) + ylab(expr_e)
  } else {
    out + xlab(expr_e) + ylab(expr_m)
  }

  out <- out + coord_fixed(1)

  return(out)
}


# . -----------

