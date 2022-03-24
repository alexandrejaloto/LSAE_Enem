
prepare_data <- function(microdata, tests, year, application, area)
{
  # Enem 2009 microdata has a different structure
  if (year != 2009)
  {
    data <- microdata %>%

      # select only variables that we will work with
      dplyr::select (paste0 (c('TP_PRESENCA_', 'TX_RESPOSTAS_', 'NU_NOTA_', 'CO_PROVA_', 'TX_GABARITO_'), disc), NU_INSCRICAO, TP_LINGUA) %>%

      # select students that were present (TP_PRESENCA_area == 1) and did no left blank (signed as 45 '.')
      filter (get (paste0 ('TP_PRESENCA_', area)) == 1 & get (paste0 ('TX_RESPOSTAS_', disc)) != '.............................................') %>%

      # since 2010 LC has had 5 items in foreign language. students that choose English have 99999 in Spanish items. And vice-versa.
      filter (get (paste0 ('TX_RESPOSTAS_', area)) != '99999.............................................' & get (paste0 ('TX_RESPOSTAS_', area)) != '.....99999........................................') %>%

      # we do not want TP_PRESENCA anymore
      dplyr::select (NU_INSCRICAO, paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_', 'TX_GABARITO_', 'NU_NOTA_'), disc), TP_LINGUA) %>%

      # select students who did not ask for adaptations in their tests
      filter ( get (paste0 ('CO_PROVA_', disc)) %in% provas[[disc]]) %>%

      # this person does not have answers in NS 2010, so he/she will be excluded
      filter (NU_INSCRICAO != '200000422956') %>%

      # this person does not have answers in LC 2010, so he/she will be excluded
      filter (NU_INSCRICAO != '200003346138') %>%

      # these people have 50 characters on LC 2010, so they will be excluded
      subset (NU_INSCRICAO != '130007115904' & NU_INSCRICAO != '130007724368' & NU_INSCRICAO != '130010749792' & NU_INSCRICAO != '130012004954') %>%

      # this person does not have answers in NS 2014, so he/she will be excluded
      filter (NU_INSCRICAO != '140006934374') %>%

      # this person does not have answers in LC 2014, so he/she will be excluded
      filter (NU_INSCRICAO != '140004278178') %>%

      # these people have 50 characters on LC 2014, so they will be excluded
      subset (NU_INSCRICAO != '140000402586' & NU_INSCRICAO != '140002367377' & NU_INSCRICAO != '140003633040' &
                NU_INSCRICAO != '140003876089' & NU_INSCRICAO != '140005136402' & NU_INSCRICAO != '140005544152' &
                NU_INSCRICAO != '140005807737' & NU_INSCRICAO != '140007664422') %>%

      # these people have 45 characters on LC 2015, so they will be excluded
      subset (NU_INSCRICAO != '150005340871' & NU_INSCRICAO != '150007797393')


  } else {
    data <- microdata %>%

      # select only variables that we will work with
      dplyr::select (paste0 (c('TP_PRESENCA_', 'TX_RESPOSTAS_', 'NU_NOTA_', 'CO_PROVA_', 'TX_GABARITO_'), disc), NU_INSCRICAO) %>%

      # select students that were present (TP_PRESENCA_area == 1) and did no left blank (signed as 45 '.')
      filter (get (paste0 ('TP_PRESENCA_', area)) == 1 & get (paste0 ('TX_RESPOSTAS_', area)) != '.............................................') %>%

      # since 2010 LC has had 5 items in foreign language. students that choose English have 99999 in Spanish items. And vice-versa.
      filter (get (paste0 ('TX_RESPOSTAS_', area)) != '99999.............................................' & get (paste0 ('TX_RESPOSTAS_', area)) != '.....99999........................................') %>%

      # we do not want TP_PRESENCA anymore
      dplyr::select (NU_INSCRICAO, paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_', 'TX_GABARITO_', 'NU_NOTA_'), area)) %>%

      # select students who did not ask for adaptations in their tests
      filter ( get (paste0 ('CO_PROVA_', area)) %in% provas[[area]]) %>%

      # this person has 44 characters on LC, so he/she will be excluded
      filter (NU_INSCRICAO != 100000703699)

  }

  data <- stringr::str_split (data[, get (paste0('TX_RESPOSTAS_', area))], '', simplify = TRUE) %>%
    data.frame (data, .) %>%
    replace.value (paste0('X', 1:10), 9, as.double(NA))

  # import item file
  items <- read.table(
    paste0(
      'D:/Microdados/',
      year,
      '/ITENS_PROVA_',
      year,
      '.csv'
    ),
    sep = ";",
    header = T
  )

  data.new <- data.frame()

  # corrigir a prova de acordo com o caderno
  for (k in 1:length(tests[[area]]))
  {

    booklet <- tests[[area]] [k]
    resp.book <- subset (data, get (paste0 ('CO_PROVA_', area)) == booklet)

    # In 2nd application in 2012, (CO_PROVA %in% 153:156), variable TX_GABARITO
    # has 45 "-" for all participants
    if (booklet %in% c(153:156))
    {key <- items %>%
      subset (CO_PROVA == booklet) %>%
      dplyr::select (TX_GABARITO) } else {

        key <- resp.book[1, paste0 ('TX_GABARITO_', area)] %>%
          stringr::str_replace('\\*', 'X') %>%
          strsplit (NULL) %>%
          data.frame()
      }

    cod.items <- subset (items, CO_PROVA == booklet)$CO_ITEM

    if (disc == 'LC')
    {

      # if microdata is from 2010 to 2014 (they have a different format)
      if (year %in% c (2010:2014))
      {

        # for those who chose spanish
        eval (parse (text = paste0('XL', 1:10, " = NA"
        )))

        spanish <- resp.book %>%
          subset (TP_LINGUA == 1) %>%
          cbind (XL1, XL2, XL3, XL4, XL5)
        names(spanish)[c(7:11, 12:51)] <- paste0 ('X', 6:50)
        names(spanish)[52:56] <- paste0 ('X', 1:5)

        english <- resp.book %>%
          subset (TP_LINGUA == 0) %>%
          cbind (XL6, XL7, XL8, XL9, XL10)
        names(english)[12:51] <- paste0 ('X', 11:50)
        names(english)[52:56] <- paste0 ('X', 6:10)

        resp.book <- data.table::rbindlist(list (spanish, english), fill = TRUE) %>%
          data.frame()

        rm (list = c ('spanish', 'english'))
        gc()

        # variable TX_RESPOSTAS needs to have 50 items
        resp.book$TX_RESPOSTAS_LC <- resp.book[, paste0 ('X', 1:50)] %>%
          apply (1, stringr::str_c, collapse = '')
      }

      if (year != 2009)
      {
        correction <- key2binary (resp.book [,paste ("X", 1:50, sep = "")], key)
      } else {
        correction <- key2binary (resp.book [,paste ("X", 1:45, sep = "")], key)
      }

    } else {
      correction <- key2binary (resp.book [,paste ("X", 1:45, sep = "")], key)
    }

    # score (CTT)
    data.new. <- apply (correction, 1, sum, na.rm = TRUE) %>%
      cbind (correction, .) %>%
      data.frame (
        resp.book[, c('NU_INSCRICAO', paste0( c('CO_PROVA_', 'TX_RESPOSTAS_'), area))],
        .,
        resp.cad[ , paste0 ('NU_NOTA_',area)]
      )

    # verify if there are item codes. if not, call them I1:I45
    if (length (cod.items) > 0)
    {
      names (data.new.) = c (
        'NU_INSCRICAO',
        paste0( c('CO_PROVA_', 'TX_RESPOSTAS_'), area),
        cod.items,
        'SCORE_CTT',
        paste0 ('NU_NOTA_', area)
      )

      # verify if an item was excluded. we have to exclude this item from IRT calibration
      # only when k == 1, because it is when the items are ordered as the final data
      if (k == 1)
      {
        # code of excluded item
        cod.excluded <- subset (items, CO_PROVA == booklet)$CO_ITEM [which (key == 'X')]
        # column (position in booklet) of excluded item
        col.excluded <- which (key == 'X')
      }
    } else {
      # verify if an item was excluded. we have to exclude this item from IRT calibration
      # column (position in booklet) of excluded item
      col.excluded = which (key == 'X')
      cod.excluded = which (key == 'X')

      names (data.new.) <- c(
        'NU_INSCRICAO',
        paste0(c('CO_PROVA_', 'TX_RESPOSTAS_'), area),
        paste0('I', 1:nrow(key)),
        'SCORE_CTT', paste0 ('NU_NOTA_', area)
      )
    }
    data.new <- rbind (data.new, data.new.)

    # if there is no item code, stop the loop
    if (length (cod.items) <= 0) break
  }
  names (data.new) = c(
    'NU_INSCRICAO',
    paste0 ( c('CO_PROVA_', 'TX_RESPOSTAS_'), area),
    paste0('I', 1:nrow(key)),
    'SCORE_CTT',
    paste0 ('NU_NOTA_', area)
  )

  if (length (col.excluded) > 0)
  {  data.new = dplyr::select(data.new, -paste0('I', col.excluded)) }

  rm (list = c('data.new.', 'resp.book'))
  rm ('data')
  gc()

  fwrite(
    data.new,
    paste0 ('bancos/banco_', area, '_', year, '_', aplication, '.csv'),
    sep = ';',
    dec = ',',
    row.names = FALSE,
    col.names = TRUE
  )
}

