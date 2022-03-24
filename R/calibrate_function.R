
calibrate <- function(n, tests, year, aplication, area)
{
  # import items information
  items <- read.table (
    paste0('D:/Microdados/', year, '/ITENS_PROVA_', year, '.csv'),
    sep = ";",
    header = T
  )

  # import data obtained from prepare_data()
  data <- fread(
    paste0('bancos/banco_', disc, '_', ano, '_', aplicacao, '.csv'),
    dec = ','
  ) %>%
    # inserir os estratos: p25, p95
    # classify participants according to their strata: p25 and p95
    dplyr::mutate(
      strata = cut(
        SCORE_CCT,
        breaks = c(-Inf, quantile (SCORE_CCT, .25), quantile (SCORE_CCT, .95), Inf),
        labels = 1:3
      )
    ) %>%
    # order by strata (it is necessary for strata() function)
    arrange(strata)

  universe <- count(data, strata)

  # sampling and calibrating
  # if number of participants is fewer than n, then use them all (if equal or more than 1000)

  # first, if there are less than 1000 participants
  if (nrow(data) < 1000)
  {
    warn <- c (
      paste0 (
        'there are not enough participants for a 1000 people sample in ',
        area,
        ', aplication ',
        aplication,
        ', year ',
        year,
        ': lower stratum with ',
        universe[1,2],
        ', upper stratum with ',
        universe[3,2],
        ', and middle stratum with ',
        universe[2,2],
        '. Total: ',
        nrow(data)
      )
    )

    # save file with the warning
    cat(
      warn,
      file = paste0(
        'bancos/aviso_',
        area,
        '_',
        year,
        '_',
        aplication,
        '.txt'
      )
    )

    # if there are more than 1000 and less than n participants
  } else {
    if (nrow(data) < n)
    {
      warn <- c (
        paste0 (
          'there are not enough participants for a ',
          n,
          ' people sample in ',
          area,
          ', aplication ',
          aplication,
          ', year ',
          year,
          ': lower stratum with ',
          universe[1,2],
          ', upper stratum with ',
          universe[3,2],
          ', and middle stratum with ',
          universe[2,2],
          '. Total: ',
          nrow(data)
        )
      )

      # save file with the warning
      cat(
        warn,
        file = paste0(
          'bancos/aviso_',
          area,
          '_',
          year,
          '_',
          aplication,
          '.txt'
        )
      )

      # the sample is composed of all participants
      sample <- dplyr::select(data, -strata) %>%
        data.frame()

      # if there are more than 5000 participants, but
      # there are not enough participants to compose upper stratum (n < 25,000)
    } else if (universe[3,2] < .25 * n)
    {

      # novos valores para composição dos estratos. primeiro, pegar todos que existem do
      # estrato superior. em seguida, dividir o resto entre os outros estratos,
      # proporcionalmente (1 pra 2)

      # new values for strata composition. first, get all upper stratum participants
      # next, supplement proportionally with other strata (1:2)
      new.upper <- as.numeric (universe[3,2])
      division <- n - new.upper
      new.lower <- ceiling(division * 1/3)
      new.mid <- ceiling(division * 2/3)

      warn <- c (
        paste0 (
          'there are not enough participants for the upper stratum in ',
          area,
          ', aplication ',
          aplication,
          ', year ',
          year,
          ': lower stratum with ',
          universe[1,2],
          ', upper stratum with ',
          universe[3,2],
          ', and middle stratum with ',
          universe[2,2],
          '. Total: ',
          nrow(data)
        )
      )

      # save file with the warning
      cat(
        warn,
        file = paste0(
          'bancos/aviso_',
          area,
          '_',
          year,
          '_',
          aplication,
          '.txt'
        )
      )

      set.seed (1000)
      sample <- strata(
        data,
        stratanames = 'strata',
        size = c(new.lower, new.mid, new.upper),
        method = 'srswor'
      ) %>%
        getdata(data, .) %>%
        dplyr::select (-strata, -ID_unit, -Prob, -Stratum)

    } else {

      # if there are more than 25,000 participants
      set.seed (1000)
      sample = strata(
        data,
        stratanames = 'strata',
        size = c(.25 * n, .50 * n, .25 * n),
        method = 'srswor'
      ) %>%
        getdata(data, .) %>%
        dplyr::select (-strata, -ID_unit, -Prob, -Stratum)
    }

    # verificar a quantidade de itens. são as colunas que começam com 'I'
    # primeiro, a sequencia de itens

    # verify quantity of items (columns that start with 'I')
    # first, sequence of items
    seq.items <- names(data) %>%
      stringr::str_starts('I')
    n.items <- sum(seq.items)

    # for mirt
    mirtCluster(remove = TRUE)

    # IRT model with prior
    model <- mirt.model(
      paste (
        paste0 ('F1 = 1-', n.items),
        paste0 ('PRIOR = (1-', n.items),
        paste0 (', a1, lnorm, 0, 0.5),(1-', n.items, ',g, expbeta, 7, 28'),
        sep = '\n'
      )
    )

    # calibrate
    calib <- mirt (sample[,seq.items], model, itemtype = '3PL', TOL = .01)

    # parameters
    pars. <- coef (calib, IRTpars=TRUE, simplify = TRUE)

    # equate to Enem scale

    # 1. verify proficiency mean of sample (0,1)
    prof.calib <- data.frame (fscores (calib, qdpts = 40))

    m <- mean (prof.calib$F1)
    s <- sd (prof.calib$F1)

    # 2. verify oficial mean of sample (500,100)
    of.mean <- mean (sample[,paste0 ('NU_NOTA_', area)])
    of.sd <- sd (sample[,paste0 ('NU_NOTA_', area)])

    # transform parameters
    pars <- data.frame (ITEM = rownames (pars.$items), pars.$items[,-4]) %>%
      mutate (a_transf = (a*s)/of.sd,
              b_transf = ((b - m)/s) * of.sd + of.mean)

    # constants
    constants <- data.frame (m = m, s = s, mean = of.mean, sd = of.sd)

    # save sample, parameters, constants, and correlation
    fwrite (sample, paste0 ('bancos/amostra_', area, '_', year, '_', aplication, '.csv'), sep = ';', dec = ',', row.names = FALSE, col.names = TRUE)
    fwrite (pars, paste0 ('parametros/pars_', area, '_', year, '_', aplication, '.csv'), dec = ',', sep = ';', row.names = FALSE)
    fwrite (constantes, paste0 ('constantes/constantes_', area, '_', year, '_', aplication, '.csv'), dec = ',', sep = ';', row.names = FALSE)
  }
}


