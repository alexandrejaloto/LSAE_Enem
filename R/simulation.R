library (data.table)
library (INEPsico)
library (mirtCAT)
library (dplyr)
library (catIrt)

rm(list = ls())
gc()

# import 2019 data
data_19 <- fread (
  'D:/Microdados/2019/MICRODADOS_ENEM_2019.csv',
  select = c(
    paste0(
      c('NU_NOTA_'),
      rep (c('CH', 'CN', 'LC', 'MT'))
    )
  )
)

# scores in all areas
scores <- list()
areas <- c ('CH', 'CN', 'LC', 'MT')

for (area in areas)
{
  scores[[area]] = data[,get (paste0('NU_NOTA_', area))] %>%
    subset (. > 0) %>%
    data.frame()
}

# random sample from 2019 participants for simulation

true_score <- list()

# sampling error = 3
samp.error <- 3
alpha <- .05

for (area in areas)
{

  names (scores[[area]]) = 'scores'

  S <- var(scores[[area]])
  sd <- sd (scores[[area]]$scores)
  Z <- qnorm(1-(alpha/2))
  n <- ceiling ((sd*Z/samp.error)^2)

  set.seed(1000)
  true_score[[area]] = sample(scores[[area]]$scores, n)

}

sd_function <- function (x)
{
  unlist (x) %>%
    sd()
}

# compare sd
lapply (notas, sd_function)
lapply (true, sd_function)


hist(true_score[[area]])
hist(scores[[area]]$scores)

lapply (true_score, summary)
lapply (scores, summary)

summary (true_score[[area]])
summary (scores[[area]])

# import parameters
pars <- vector('list', 4)
names (pars) <- areas
for (year in 2009:2019)
{

  for (aplication in 1:3)
  {
    for (area in areas)
    {
      if (file.exists(paste0 ('parametros/pars_', area, '_', year, '_', aplication, '.csv')))
      {pars. = fread(paste0 ('parametros/pars_', area, '_', year, '_', aplication, '.csv'), dec = ',')
      pars[[area]] = rbind (pars[[area]], pars.)
      }
    }
  }
}

pars

# true scores in (0,1)
true.mirt <- true

for (area in areas)
{
  true.mirt[[area]] <- (true.mirt[[area]]-500) / 100

  # object with all parameters (0,1)
  pars.mirt <- pars[[area]] %>%
    mutate(a_transf = a_transf*100, b_transf = ((b_transf - 500)/100)) %>%
    # mirt uses intercept
    mutate (b_transf = -a_transf*b_transf) %>%
    select(a_transf, b_transf, g)

  names (pars.mirt) <- c ('a1', 'd', 'g')

  # model with all parameters
  mod <- generate.mirt_object (pars.mirt, itemtype = '3PL')

  # simulate responses to all items
  set.seed (1000)
  resps <- data.frame (generate_pattern(mod, matrix(real.mirt[[area]])))

  # simulate CAT

  scores <- c()
  errors <- c()
  items <- c()

  for (i in 1:nrow (resps))
  {
    sim <- mirtCAT (
      mo = mod,
      local_pattern = resps[i,],
      method = 'EAP',
      criteria = 'MI',
      start_item = 'MI',
      design = list(
        max_items = 45,
        min_SEM = .3
      )
    )
    scores[i] = sim$thetas
    errors[i] = sim$SE_thetas
    items[i] = sum (!is.na (sim$raw_responses))
  }

  results <- data.frame (
    scores = (scores * 100 + 500),
    errors = errors,
    items = items,
    true = true[[area]]
  )

  fwrite (results, paste0 ('data-raw/resultados_erro45_', area, '.csv'), dec = ',', sep = ';', row.names = FALSE)
}

