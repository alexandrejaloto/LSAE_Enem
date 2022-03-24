library(ggplot2)
library(data.table)
library (mirt)

rel <- c()

for (area in c('CH', 'CN', 'LC', 'MT'))
{
  pars <- fread (
    paste0 ('parametros/pars_', area, '_2019_1.csv'),
    dec = ','
  )

  pars.mirt <- data.frame(
    a1 = pars$a_transf*100,
    d = ((pars$b_transf-500)/100)*-(pars$a_transf*100),
    g = pars$g
  )

  mod <- mirtCAT::generate.mirt_object(pars.mirt, '3PL')

  rel <- c(rel, mirt::marginal_rxx(mod))
}


# reliability graphic

theta <- seq(-3,8,.1)

for (area in c('CH', 'CN', 'LC', 'MT'))
{
  pars <- fread (
    paste0 ('parametros/pars_', area, '_2019_1.csv'),
    dec = ','
  )

  pars.mirt <- data.frame(
    a1 = pars$a_transf*100,
    d = ((pars$b_transf-500)/100)*-(pars$a_transf*100),
    g = pars$g
  )

  mod <- mirtCAT::generate.mirt_object(pars.mirt, '3PL')

  info <- testinfo(mod, theta)
  rel <- info/(info+1)

  plot <- ggplot() +
    geom_line(aes (x = theta, y = rel)) +
    scale_y_continuous(
      name = 'Reliability',
      limits = c(0,1)
    ) +
    scale_x_continuous(
      name = paste0(
        'Sandardized score - ',
        area
      )
    )+
    geom_hline(yintercept = .91, linetype = 2) +
    theme_bw()

  jpeg (
    filename = paste0 ('rxx/confiabilidade_', area, '_2019_1.jpg'),
    width = 600,
    height = 600,
    units = "px",
    pointsize = 12,
    quality = 100,
    bg = "white",
    res = NA,
    restoreConsole = TRUE
  )

  plot (plot)

  dev.off()

}

