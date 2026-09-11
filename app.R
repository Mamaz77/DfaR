###############################################################################
# DfaR - ANALISI DELLA FLUCTUATING ASYMMETRY (FA) DENTALE
#
# Uso:
# 1. Aprire questo file in RStudio.
# 2. Premere "Run App".
# 3. Caricare il file Excel originale.
# 4. Premere "Esegui analisi FA".
# 5. Controllare i risultati.
# 6. Scaricare il file Excel finale.
#
# L'app NON modifica il file originale.
###############################################################################

# ---------------------------------------------------------------------------
# 0. PACCHETTI
# ---------------------------------------------------------------------------

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(openxlsx)
library(DT)
library(ggplot2)
library(scales)

# ---------------------------------------------------------------------------
# 1. FUNZIONI DI SERVIZIO
# ---------------------------------------------------------------------------

primo_non_NA <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA)
  x[1]
}

crea_trait <- function(arch, tooth, dimension) {
  prefisso <- ifelse(arch == "Upper", "U", "L")
  paste0(prefisso, tooth, dimension)
}

a_numero <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

grubbs_singolo <- function(x) {

  x <- as.numeric(x)
  n <- length(x)

  if (n < 3) return(NULL)

  media_x <- mean(x)
  sd_x <- sd(x)

  if (!is.finite(sd_x) || sd_x == 0) return(NULL)

  deviazioni <- abs(x - media_x)
  indice <- which.max(deviazioni)

  G <- deviazioni[indice] / sd_x

  denominatore <- (n - 1)^2 - n * G^2

  if (denominatore <= 0) {
    p_value <- 0
  } else {
    t_value <- sqrt(
      n * (n - 2) * G^2 / denominatore
    )

    p_value <- min(
      1,
      2 * n * pt(
        t_value,
        df = n - 2,
        lower.tail = FALSE
      )
    )
  }

  list(
    indice = indice,
    G = G,
    p_value = p_value
  )
}


grubbs_holm_iterativo <- function(
    dati,
    colonna_valore,
    colonne_gruppo,
    alpha = 0.05) {

  dati_correnti <- dati
  registro <- tibble()
  esclusi <- character()
  iterazione <- 1

  repeat {

    fattore_gruppo <- do.call(
      interaction,
      c(
        dati_correnti[colonne_gruppo],
        list(drop = TRUE, lex.order = TRUE)
      )
    )

    gruppi <- split(
      dati_correnti,
      fattore_gruppo
    )

    candidati_lista <- lapply(
      gruppi,
      function(g) {

        if (nrow(g) < 3) return(NULL)

        risultato <- grubbs_singolo(
          g[[colonna_valore]]
        )

        if (is.null(risultato)) return(NULL)

        candidato <- g[
          risultato$indice,
          ,
          drop = FALSE
        ]

        candidato$N_tested <- nrow(g)
        candidato$G <- risultato$G
        candidato$p_raw <- risultato$p_value

        candidato
      }
    )

    candidati <- bind_rows(candidati_lista)

    if (nrow(candidati) == 0) break

    candidati <- candidati %>%
      mutate(
        p_Holm = p.adjust(
          p_raw,
          method = "holm"
        ),
        Iteration = iterazione,
        Excluded = ifelse(
          p_Holm < alpha,
          "Yes",
          "No"
        )
      )

    registro <- bind_rows(
      registro,
      candidati
    )

    nuovi_esclusi <- candidati %>%
      filter(Excluded == "Yes") %>%
      pull(RecordKey)

    if (length(nuovi_esclusi) == 0) break

    esclusi <- unique(
      c(esclusi, nuovi_esclusi)
    )

    dati_correnti <- dati_correnti %>%
      filter(!(RecordKey %in% nuovi_esclusi))

    iterazione <- iterazione + 1

    if (iterazione > 50) {
      stop("La procedura Grubbs ha superato 50 iterazioni.")
    }
  }

  list(
    clean = dati_correnti,
    log = registro,
    excluded_keys = esclusi
  )
}


# Test omnibus D'Agostino-Pearson K2.
test_normalita <- function(x) {

  x <- x[is.finite(x)]
  n <- length(x)

  if (n < 8) {

    risultato <- shapiro.test(x)

    return(
      list(
        metodo = "Shapiro-Wilk (n < 8)",
        statistica = as.numeric(risultato$statistic),
        p_value = risultato$p.value
      )
    )
  }

  media_x <- mean(x)

  m2 <- mean((x - media_x)^2)
  m3 <- mean((x - media_x)^3)
  m4 <- mean((x - media_x)^4)

  if (m2 == 0) {
    return(
      list(
        metodo = "D'Agostino-Pearson K2",
        statistica = NA_real_,
        p_value = NA_real_
      )
    )
  }

  skewness <- m3 / (m2^(3 / 2))

  y <- skewness * sqrt(
    ((n + 1) * (n + 3)) /
      (6 * (n - 2))
  )

  beta2 <- (
    3 * (n^2 + 27 * n - 70) *
      (n + 1) * (n + 3)
  ) / (
    (n - 2) *
      (n + 5) *
      (n + 7) *
      (n + 9)
  )

  W2 <- -1 + sqrt(
    2 * (beta2 - 1)
  )

  delta <- 1 / sqrt(
    0.5 * log(W2)
  )

  alpha <- sqrt(
    2 / (W2 - 1)
  )

  Z_skew <- delta * asinh(
    y / alpha
  )

  kurtosis_b2 <- m4 / (m2^2)

  expected_b2 <- 3 * (n - 1) / (n + 1)

  variance_b2 <- (
    24 * n * (n - 2) * (n - 3)
  ) / (
    (n + 1)^2 *
      (n + 3) *
      (n + 5)
  )

  x_kurt <- (
    kurtosis_b2 - expected_b2
  ) / sqrt(variance_b2)

  sqrt_beta1 <- (
    6 * (n^2 - 5 * n + 2) /
      ((n + 7) * (n + 9))
  ) * sqrt(
    6 * (n + 3) * (n + 5) /
      (n * (n - 2) * (n - 3))
  )

  A <- 6 + (
    8 / sqrt_beta1
  ) * (
    2 / sqrt_beta1 +
      sqrt(
        1 + 4 / (sqrt_beta1^2)
      )
  )

  term1 <- 1 - 2 / (9 * A)

  denominatore <- 1 +
    x_kurt * sqrt(
      2 / (A - 4)
    )

  if (denominatore == 0) {

    Z_kurt <- NA_real_

  } else {

    term2 <- sign(denominatore) * (
      (1 - 2 / A) /
        abs(denominatore)
    )^(1 / 3)

    Z_kurt <- (
      term1 - term2
    ) / sqrt(
      2 / (9 * A)
    )
  }

  if (is.na(Z_skew) || is.na(Z_kurt)) {

    K2 <- NA_real_
    p_value <- NA_real_

  } else {

    K2 <- Z_skew^2 +
      Z_kurt^2

    p_value <- pchisq(
      K2,
      df = 2,
      lower.tail = FALSE
    )
  }

  list(
    metodo = "D'Agostino-Pearson K2",
    statistica = K2,
    p_value = p_value
  )
}


anova_FA_singolo_tratto <- function(d) {

  n <- nrow(d)

  if (n < 3) return(NULL)

  R1 <- d$R_1
  R2 <- d$R_2
  L1 <- d$L_1
  L2 <- d$L_2

  tutti_valori <- c(
    R1, R2, L1, L2
  )

  grand_mean <- mean(tutti_valori)

  media_individuo <- rowMeans(
    cbind(R1, R2, L1, L2)
  )

  media_R <- mean(c(R1, R2))
  media_L <- mean(c(L1, L2))

  cell_R <- (R1 + R2) / 2
  cell_L <- (L1 + L2) / 2

  SS_individual <- 4 * sum(
    (media_individuo - grand_mean)^2
  )

  SS_side <- n * 2 * (
    (media_R - grand_mean)^2 +
      (media_L - grand_mean)^2
  )

  SS_interaction <- 2 * sum(
    (cell_R - media_individuo - media_R + grand_mean)^2 +
      (cell_L - media_individuo - media_L + grand_mean)^2
  )

  SS_error <- sum(
    (R1 - cell_R)^2 +
      (R2 - cell_R)^2 +
      (L1 - cell_L)^2 +
      (L2 - cell_L)^2
  )

  df_individual <- n - 1
  df_side <- 1
  df_interaction <- n - 1
  df_error <- 2 * n

  MS_individual <- SS_individual / df_individual
  MS_side <- SS_side / df_side
  MS_interaction <- SS_interaction / df_interaction
  MS_error <- SS_error / df_error

  F_side <- MS_side / MS_interaction

  p_side <- pf(
    F_side,
    df1 = df_side,
    df2 = df_interaction,
    lower.tail = FALSE
  )

  F_FA <- MS_interaction / MS_error

  p_FA <- pf(
    F_FA,
    df1 = df_interaction,
    df2 = df_error,
    lower.tail = FALSE
  )

  FA_variance_component <- max(
    0,
    (MS_interaction - MS_error) / 2
  )

  normalita <- test_normalita(
    d$R_minus_L
  )

  size_test <- tryCatch(
    cor.test(
      d$FA_abs,
      d$Mean_size,
      method = "spearman",
      exact = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(size_test)) {
    rho_size <- NA_real_
    p_size <- NA_real_
  } else {
    rho_size <- as.numeric(
      size_test$estimate
    )
    p_size <- size_test$p.value
  }

  problemi <- character()

  if (p_FA >= 0.05) {
    problemi <- c(
      problemi,
      "FA not > ME"
    )
  }

  if (p_side < 0.05) {
    problemi <- c(
      problemi,
      "directional asymmetry"
    )
  }

  if (!is.na(normalita$p_value) &&
      normalita$p_value < 0.05) {
    problemi <- c(
      problemi,
      "non-normal R-L"
    )
  }

  if (length(problemi) == 0) {
    screening_status <- "passes basic screening"
  } else {
    screening_status <- paste(
      problemi,
      collapse = "; "
    )
  }

  trait <- unique(d$Trait)
  arch <- unique(d$Arch)
  tooth <- unique(d$ToothType)
  dimension <- unique(d$Dimension)

  paper_comparable <- ifelse(
    tooth %in% c("C", "M1", "M2"),
    "Yes",
    "No (M3 exploratory)"
  )

  summary_row <- tibble(
    Trait = trait,
    Arch = arch,
    ToothType = tooth,
    Dimension = dimension,
    PaperComparable = paper_comparable,
    N_pairs_clean = n,
    Mean_R = mean(d$R_mean),
    Mean_L = mean(d$L_mean),
    `Mean_R-L` = mean(d$R_minus_L),
    `SD_R-L` = sd(d$R_minus_L),
    F_Side_DA = F_side,
    p_Side_DA = p_side,
    `F_Individual_x_Side` = F_FA,
    p_FA_vs_ME = p_FA,
    `MS_Individual_x_Side` = MS_interaction,
    MS_Error = MS_error,
    FA_variance_component = FA_variance_component,
    Normality_test = normalita$metodo,
    Normality_statistic = normalita$statistica,
    `p_Normality_R-L` = normalita$p_value,
    `Spearman_rho_absFA_Size` = rho_size,
    p_SizeEffect = p_size,
    Primary_FA_Index = "FA_abs = |R-L|",
    Screening_status = screening_status
  )

  detail <- tibble(
    Trait = trait,
    Source = c(
      "Individual",
      "Side (DA)",
      "Individual x Side (FA)",
      "Error (replicate ME)"
    ),
    SS = c(
      SS_individual,
      SS_side,
      SS_interaction,
      SS_error
    ),
    df = c(
      df_individual,
      df_side,
      df_interaction,
      df_error
    ),
    MS = c(
      MS_individual,
      MS_side,
      MS_interaction,
      MS_error
    ),
    F = c(
      NA,
      F_side,
      F_FA,
      NA
    ),
    p = c(
      NA,
      p_side,
      p_FA,
      NA
    ),
    N_pairs_clean = n
  )

  list(
    summary = summary_row,
    detail = detail
  )
}



# ---------------------------------------------------------------------------
# FUNZIONI AGGIUNTIVE PER LE ANALISI BIOLOGICHE
# ---------------------------------------------------------------------------

# Converte codifiche comuni di presenza/assenza in 0/1.
# Lascia NA quando il valore non è interpretabile.
a_binario <- function(x) {

  z <- trimws(tolower(as.character(x)))

  risultato <- rep(NA_real_, length(z))

  risultato[z %in% c(
    "1", "yes", "y", "si", "sì",
    "present", "presente", "true"
  )] <- 1

  risultato[z %in% c(
    "0", "no", "n",
    "absent", "assente", "false"
  )] <- 0

  suppressWarnings({
    numerico <- as.numeric(z)
  })

  risultato[is.na(risultato) & numerico %in% c(0, 1)] <-
    numerico[is.na(risultato) & numerico %in% c(0, 1)]

  risultato
}


# Media ignorando NA; se tutti i valori sono NA restituisce NA.
media_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}


# Massimo ignorando NA; se tutti i valori sono NA restituisce NA.
max_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}


# Traduce 0/1 in etichette leggibili.
etichetta_presenza <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x == 1, "Present", "Absent")
  )
}


# Analisi automatica per una variabile numerica Y.
#
# Se X è numerica:
#   Spearman.
#
# Se X è categoriale con 2 gruppi:
#   Wilcoxon rank-sum.
#
# Se X è categoriale con >2 gruppi:
#   Kruskal-Wallis + confronti pairwise Wilcoxon con Holm.
analisi_numerica_automatica <- function(dati, y, x) {

  d <- dati[
    !is.na(dati[[y]]) &
      !is.na(dati[[x]]),
    ,
    drop = FALSE
  ]

  if (nrow(d) < 3) {
    return(
      list(
        testo = "Dati insufficienti per il test.",
        dati = d,
        tipo = "none"
      )
    )
  }

  xv <- d[[x]]
  yv <- as.numeric(d[[y]])

  # Variabile numerica continua: Spearman.
  if (is.numeric(xv) && length(unique(xv)) > 5) {

    test <- suppressWarnings(
      cor.test(
        yv,
        xv,
        method = "spearman",
        exact = FALSE
      )
    )

    testo <- paste0(
      "Test: correlazione di Spearman\n",
      "N = ", nrow(d), "\n",
      "rho = ", round(as.numeric(test$estimate), 3), "\n",
      "p = ", signif(test$p.value, 4)
    )

    return(
      list(
        testo = testo,
        dati = d,
        tipo = "numeric"
      )
    )
  }

  # Tutto il resto viene trattato come fattore.
  gruppo <- droplevels(
    factor(xv)
  )

  n_gruppi <- nlevels(gruppo)

  if (n_gruppi < 2) {
    return(
      list(
        testo = "La variabile selezionata contiene meno di due gruppi utilizzabili.",
        dati = d,
        tipo = "factor"
      )
    )
  }

  conteggi <- table(gruppo)

  conteggi_testo <- paste(
    paste0(
      names(conteggi),
      ": n=",
      as.integer(conteggi)
    ),
    collapse = "; "
  )

  if (n_gruppi == 2) {

    test <- suppressWarnings(
      wilcox.test(
        yv ~ gruppo,
        exact = FALSE
      )
    )

    testo <- paste0(
      "Test: Wilcoxon rank-sum / Mann-Whitney\n",
      conteggi_testo, "\n",
      "W = ", round(as.numeric(test$statistic), 3), "\n",
      "p = ", signif(test$p.value, 4)
    )

  } else {

    test <- kruskal.test(
      yv ~ gruppo
    )

    pair <- suppressWarnings(
      pairwise.wilcox.test(
        yv,
        gruppo,
        p.adjust.method = "holm",
        exact = FALSE
      )
    )

    matrice_p <- capture.output(
      print(
        round(
          pair$p.value,
          4
        )
      )
    )

    testo <- paste0(
      "Test: Kruskal-Wallis\n",
      conteggi_testo, "\n",
      "chi-square = ",
      round(as.numeric(test$statistic), 3),
      "; df = ",
      as.numeric(test$parameter),
      "; p = ",
      signif(test$p.value, 4),
      "\n\nConfronti pairwise Wilcoxon, p corretti con Holm:\n",
      paste(matrice_p, collapse = "\n")
    )
  }

  list(
    testo = testo,
    dati = d,
    tipo = "factor"
  )
}


# Test per una risposta binaria/categoriale (es. presenza LEH).
# Usa Fisher exact; è preferibile con campioni piccoli o celle poco popolate.
analisi_categoriale_fisher <- function(dati, y, x) {

  d <- dati[
    !is.na(dati[[y]]) &
      !is.na(dati[[x]]),
    ,
    drop = FALSE
  ]

  if (nrow(d) < 3) {
    return(
      list(
        testo = "Dati insufficienti per il test.",
        dati = d
      )
    )
  }

  tab <- table(
    d[[y]],
    d[[x]]
  )

  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(
      list(
        testo = "Sono necessari almeno due livelli per entrambe le variabili.",
        dati = d
      )
    )
  }

  test <- tryCatch(
    fisher.test(tab),
    error = function(e) NULL
  )

  if (is.null(test)) {

    test_chi <- chisq.test(
      tab,
      simulate.p.value = TRUE,
      B = 10000
    )

    testo <- paste0(
      "Test: chi-quadrato con p simulato (10.000 repliche)\n",
      "N = ", sum(tab), "\n",
      "p = ", signif(test_chi$p.value, 4)
    )

  } else {

    testo <- paste0(
      "Test: Fisher exact\n",
      "N = ", sum(tab), "\n",
      "p = ", signif(test$p.value, 4)
    )
  }

  list(
    testo = testo,
    dati = d,
    tabella = tab
  )
}


# ---------------------------------------------------------------------------
# 2. FUNZIONE PRINCIPALE: ESEGUE TUTTO IL WORKFLOW
# ---------------------------------------------------------------------------

esegui_pipeline_FA <- function(
    file_input,
    alpha = 0.05,
    includi_M3 = TRUE,
    leh_blank_absence = TRUE) {

  fogli <- readxl::excel_sheets(file_input)

  richiesti <- c(
    "Tooth_Master",
    "LEH_Bands"
  )

  mancanti <- setdiff(
    richiesti,
    fogli
  )

  if (length(mancanti) > 0) {
    stop(
      paste0(
        "Mancano i seguenti fogli Excel: ",
        paste(mancanti, collapse = ", ")
      )
    )
  }

  Tooth_Master <- readxl::read_excel(
    file_input,
    sheet = "Tooth_Master"
  )

  LEH_Bands <- readxl::read_excel(
    file_input,
    sheet = "LEH_Bands"
  )

  colonne_richieste <- c(
    "ID",
    "Sex",
    "AgeClass",
    "Funerary",
    "ToothType",
    "Arch",
    "Side",
    "WearStage",
    "MD (1)",
    "BL (1)",
    "MD (2)",
    "BL (2)"
  )

  colonne_mancanti <- setdiff(
    colonne_richieste,
    names(Tooth_Master)
  )

  if (length(colonne_mancanti) > 0) {
    stop(
      paste0(
        "Nel foglio Tooth_Master mancano le colonne: ",
        paste(colonne_mancanti, collapse = ", ")
      )
    )
  }

  Tooth_Master <- Tooth_Master %>%
    mutate(
      ToothType = ifelse(
        ToothType == "i2",
        "I2",
        ToothType
      )
    )

  if ("Tooth" %in% names(LEH_Bands)) {
    LEH_Bands <- LEH_Bands %>%
      mutate(
        Tooth = ifelse(
          Tooth == "i2",
          "I2",
          Tooth
        )
      )
  }

  colonne_misure <- c(
    "MD (1)",
    "BL (1)",
    "MD (2)",
    "BL (2)"
  )

  Tooth_Master <- Tooth_Master %>%
    mutate(
      across(
        all_of(colonne_misure),
        a_numero
      )
    )

  # Rimuove esclusivamente il duplicato già identificato.
  Tooth_Master <- Tooth_Master %>%
    filter(
      !(
        ID == "T54" &
          ToothType == "M2" &
          Arch == "Lower" &
          Side == "L" &
          WearStage == 5
      )
    )

  duplicati_residui <- Tooth_Master %>%
    count(
      ID,
      ToothType,
      Arch,
      Side
    ) %>%
    filter(n > 1)

  if (nrow(duplicati_residui) > 0) {
    stop(
      paste0(
        "Esistono ancora ",
        nrow(duplicati_residui),
        " posizioni dentali duplicate. ",
        "Correggere il file prima di continuare."
      )
    )
  }

  Tooth_Master <- Tooth_Master %>%
    mutate(
      SourceRow = row_number() + 1
    )

  # Versioni numeriche delle variabili LEH.
  # Le colonne originali restano intatte.
  if ("LEH_Present" %in% names(Tooth_Master)) {
    Tooth_Master$LEH_Present_num <- a_binario(
      Tooth_Master$LEH_Present
    )
  } else {
    Tooth_Master$LEH_Present_num <- NA_real_
  }

  if ("LEH_Count" %in% names(Tooth_Master)) {
    Tooth_Master$LEH_Count_num <- a_numero(
      Tooth_Master$LEH_Count
    )
  } else {
    Tooth_Master$LEH_Count_num <- NA_real_
  }

  # -------------------------------------------------------
  # INTERPRETAZIONE DEI VUOTI IN LEH_Present
  # -------------------------------------------------------
  #
  # Nel dataset originale possono esserci valori 1 per la presenza
  # e celle vuote invece di espliciti 0.
  #
  # Se leh_blank_absence = TRUE:
  #   1. individuiamo i tipi dentali nei quali compare almeno un LEH positivo;
  #   2. SOLO per quei tipi dentali, interpretiamo i blank come "assenza".
  #
  # In questo modo non trasformiamo automaticamente in assenza i blank
  # di denti che non sono stati utilizzati per la registrazione LEH.
  #
  # Questa assunzione viene registrata nel foglio FA_Clean_Method.

  denti_LEH_scorabili <- sort(
    unique(
      Tooth_Master$ToothType[
        Tooth_Master$LEH_Present_num == 1 &
          !is.na(Tooth_Master$LEH_Present_num)
      ]
    )
  )

  if (
    isTRUE(leh_blank_absence) &&
    length(denti_LEH_scorabili) > 0
  ) {

    indice_blank_scorabile <-
      is.na(Tooth_Master$LEH_Present_num) &
      Tooth_Master$ToothType %in% denti_LEH_scorabili

    Tooth_Master$LEH_Present_num[
      indice_blank_scorabile
    ] <- 0
  }

  denti_FA <- if (includi_M3) {
    c("C", "M1", "M2", "M3")
  } else {
    c("C", "M1", "M2")
  }

  prepara_dimensione <- function(
      dati,
      dimensione) {

    if (dimensione == "MD") {
      col_1 <- "MD (1)"
      col_2 <- "MD (2)"
    } else {
      col_1 <- "BL (1)"
      col_2 <- "BL (2)"
    }

    dati %>%
      transmute(
        ID,
        Arch,
        ToothType,
        Side,
        SourceRow,
        Dimension = dimensione,
        Rep1 = .data[[col_1]],
        Rep2 = .data[[col_2]]
      ) %>%
      filter(
        ToothType %in% denti_FA,
        Arch %in% c("Upper", "Lower"),
        Side %in% c("R", "L"),
        !is.na(Rep1),
        !is.na(Rep2)
      ) %>%
      mutate(
        Trait = crea_trait(
          Arch,
          ToothType,
          Dimension
        ),
        RepDiff = Rep2 - Rep1,
        RecordKey = paste(
          SourceRow,
          Dimension,
          sep = "_"
        )
      )
  }

  misure_long <- bind_rows(
    prepara_dimensione(
      Tooth_Master,
      "MD"
    ),
    prepara_dimensione(
      Tooth_Master,
      "BL"
    )
  )

  # STADIO 1: errore di misura
  ME_screen <- grubbs_holm_iterativo(
    dati = misure_long,
    colonna_valore = "RepDiff",
    colonne_gruppo = c(
      "Arch",
      "ToothType",
      "Dimension"
    ),
    alpha = alpha
  )

  misure_ME_clean <- ME_screen$clean
  ME_log <- ME_screen$log

  controllo_lati <- misure_ME_clean %>%
    count(
      ID,
      Arch,
      ToothType,
      Dimension,
      Side
    ) %>%
    filter(n > 1)

  if (nrow(controllo_lati) > 0) {
    stop(
      "Esistono record multipli per lo stesso individuo, tratto e lato."
    )
  }

  meta_individui <- Tooth_Master %>%
    group_by(ID) %>%
    summarise(
      Sex = primo_non_NA(Sex),
      AgeClass = primo_non_NA(AgeClass),
      Funerary = primo_non_NA(Funerary),
      .groups = "drop"
    )

  coppie <- misure_ME_clean %>%
    select(
      ID,
      Arch,
      ToothType,
      Dimension,
      Side,
      Rep1,
      Rep2,
      SourceRow
    ) %>%
    pivot_wider(
      id_cols = c(
        ID,
        Arch,
        ToothType,
        Dimension
      ),
      names_from = Side,
      values_from = c(
        Rep1,
        Rep2,
        SourceRow
      ),
      names_glue = "{.value}_{Side}"
    ) %>%
    filter(
      !is.na(Rep1_R),
      !is.na(Rep2_R),
      !is.na(Rep1_L),
      !is.na(Rep2_L)
    ) %>%
    rename(
      R_1 = Rep1_R,
      R_2 = Rep2_R,
      L_1 = Rep1_L,
      L_2 = Rep2_L
    ) %>%
    left_join(
      meta_individui,
      by = "ID"
    ) %>%
    mutate(
      Trait = crea_trait(
        Arch,
        ToothType,
        Dimension
      ),
      R_mean = (R_1 + R_2) / 2,
      L_mean = (L_1 + L_2) / 2,
      R_minus_L = R_mean - L_mean,
      FA_abs = abs(R_minus_L),
      Mean_size = (R_mean + L_mean) / 2,
      FA_relative = FA_abs / abs(Mean_size),
      RecordKey = paste(
        ID,
        Arch,
        ToothType,
        Dimension,
        sep = "_"
      )
    )

  # STADIO 2: outlier R-L
  RL_screen <- grubbs_holm_iterativo(
    dati = coppie,
    colonna_valore = "R_minus_L",
    colonne_gruppo = c(
      "Arch",
      "ToothType",
      "Dimension"
    ),
    alpha = alpha
  )

  FA_Pairs_Clean <- RL_screen$clean
  RL_log <- RL_screen$log

  # ANOVA per tratto
  lista_tratti <- split(
    FA_Pairs_Clean,
    FA_Pairs_Clean$Trait
  )

  risultati_anova <- lapply(
    lista_tratti,
    anova_FA_singolo_tratto
  )

  risultati_anova <- risultati_anova[
    !vapply(
      risultati_anova,
      is.null,
      logical(1)
    )
  ]

  FA_ANOVA_Clean <- bind_rows(
    lapply(
      risultati_anova,
      function(x) x$summary
    )
  )

  FA_ANOVA_Clean_Detail <- bind_rows(
    lapply(
      risultati_anova,
      function(x) x$detail
    )
  )

  ME_esclusi <- if (nrow(ME_log) > 0) {
    ME_log %>%
      filter(Excluded == "Yes")
  } else {
    tibble()
  }

  RL_esclusi <- if (nrow(RL_log) > 0) {
    RL_log %>%
      filter(Excluded == "Yes")
  } else {
    tibble()
  }

  ME_count <- if (nrow(ME_esclusi) > 0) {
    ME_esclusi %>%
      count(
        Trait,
        name = "N_ME_exclusions"
      )
  } else {
    tibble(
      Trait = character(),
      N_ME_exclusions = integer()
    )
  }

  RL_count <- if (nrow(RL_esclusi) > 0) {
    RL_esclusi %>%
      count(
        Trait,
        name = "N_RL_exclusions"
      )
  } else {
    tibble(
      Trait = character(),
      N_RL_exclusions = integer()
    )
  }

  FA_ANOVA_Clean <- FA_ANOVA_Clean %>%
    left_join(
      ME_count,
      by = "Trait"
    ) %>%
    left_join(
      RL_count,
      by = "Trait"
    ) %>%
    mutate(
      N_ME_exclusions = replace_na(
        N_ME_exclusions,
        0L
      ),
      N_RL_exclusions = replace_na(
        N_RL_exclusions,
        0L
      )
    ) %>%
    relocate(
      N_ME_exclusions,
      N_RL_exclusions,
      .after = N_pairs_clean
    ) %>%
    arrange(
      Arch,
      ToothType,
      Dimension
    )

  # -------------------------------------------------------------------------
  # LEH A LIVELLO INDIVIDUALE E DELLO STESSO TIPO DENTALE
  # -------------------------------------------------------------------------

  LEH_Individual <- Tooth_Master %>%
    group_by(ID) %>%
    summarise(
      Sex = primo_non_NA(Sex),
      AgeClass = primo_non_NA(AgeClass),
      Funerary = primo_non_NA(Funerary),
      N_LEH_teeth_scored = sum(!is.na(LEH_Present_num)),
      Individual_LEH_num = ifelse(
        N_LEH_teeth_scored > 0,
        max(LEH_Present_num, na.rm = TRUE),
        NA_real_
      ),
      Individual_LEH_CountMax = max_na(LEH_Count_num),
      .groups = "drop"
    ) %>%
    mutate(
      Individual_LEH = etichetta_presenza(
        Individual_LEH_num
      )
    )

  LEH_same_tooth <- Tooth_Master %>%
    group_by(
      ID,
      Arch,
      ToothType
    ) %>%
    summarise(
      N_LEH_same_tooth_scored =
        sum(!is.na(LEH_Present_num)),
      LEH_same_tooth_num = ifelse(
        N_LEH_same_tooth_scored > 0,
        max(LEH_Present_num, na.rm = TRUE),
        NA_real_
      ),
      LEH_same_tooth_CountMax =
        max_na(LEH_Count_num),
      Wear_mean_pair =
        media_na(a_numero(WearStage)),
      .groups = "drop"
    ) %>%
    mutate(
      LEH_same_tooth = etichetta_presenza(
        LEH_same_tooth_num
      )
    )

  FA_Pairs_Clean <- FA_Pairs_Clean %>%
    left_join(
      FA_ANOVA_Clean %>%
        select(
          Trait,
          Screening_status
        ),
      by = "Trait"
    ) %>%
    left_join(
      LEH_Individual %>%
        select(
          ID,
          Individual_LEH,
          Individual_LEH_CountMax,
          N_LEH_teeth_scored
        ),
      by = "ID"
    ) %>%
    left_join(
      LEH_same_tooth,
      by = c(
        "ID",
        "Arch",
        "ToothType"
      )
    ) %>%
    mutate(
      PaperComparable = ifelse(
        ToothType %in% c(
          "C",
          "M1",
          "M2"
        ),
        "Yes",
        "No (M3 exploratory)"
      ),
      Use_for_group_tests = ifelse(
        PaperComparable == "Yes" &
          Screening_status ==
          "passes basic screening",
        "Yes",
        "No"
      )
    ) %>%
    select(
      ID,
      Sex,
      AgeClass,
      Funerary,
      Trait,
      Arch,
      ToothType,
      Dimension,
      R_1,
      R_2,
      L_1,
      L_2,
      R_mean,
      L_mean,
      R_minus_L,
      FA_abs,
      Mean_size,
      FA_relative,

      Individual_LEH,
      Individual_LEH_CountMax,
      N_LEH_teeth_scored,

      LEH_same_tooth,
      LEH_same_tooth_CountMax,
      N_LEH_same_tooth_scored,

      Wear_mean_pair,

      PaperComparable,
      Screening_status,
      Use_for_group_tests,
      SourceRow_R,
      SourceRow_L
    ) %>%
    arrange(
      ID,
      Trait
    )

  # -------------------------------------------------------------------------
  # COMPOSITE FLUCTUATING ASYMMETRY (FA) A LIVELLO INDIVIDUALE
  # -------------------------------------------------------------------------
  #
  # Il composito viene costruito SOLO con i tratti che:
  # - sono C, M1 o M2;
  # - hanno superato lo screening FA.
  #
  # Per evitare che un tratto con FA assoluta naturalmente maggiore pesi
  # più degli altri, ciascun valore FA_abs viene standardizzato rispetto
  # alla media del proprio tratto:
  #
  #   FA_standardized = FA_abs / mean(FA_abs del tratto)
  #
  # Il Composite_FA è la media individuale dei valori standardizzati.
  #
  # Vengono prodotte tre versioni:
  # - Composite_FA: usa tutti i tratti validi disponibili per l'individuo;
  # - Composite_FA_ge2: restituisce il punteggio solo con almeno 2 tratti;
  # - Composite_FA_complete: richiede tutti i tratti validi disponibili
  #   nell'intero campione.
  #
  # N_FA_traits registra quanti tratti contribuiscono al punteggio.

  tratti_FA_validi <- FA_ANOVA_Clean %>%
    filter(
      PaperComparable == "Yes",
      Screening_status == "passes basic screening"
    ) %>%
    pull(Trait)

  N_tratti_FA_validi <- length(
    tratti_FA_validi
  )

  FA_trait_means <- FA_Pairs_Clean %>%
    filter(
      Trait %in% tratti_FA_validi
    ) %>%
    group_by(Trait) %>%
    summarise(
      Mean_FA_abs_trait = mean(
        FA_abs,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  FA_Pairs_Clean <- FA_Pairs_Clean %>%
    left_join(
      FA_trait_means,
      by = "Trait"
    ) %>%
    mutate(
      FA_standardized = ifelse(
        Trait %in% tratti_FA_validi &
          !is.na(FA_abs) &
          !is.na(Mean_FA_abs_trait) &
          Mean_FA_abs_trait > 0,
        FA_abs / Mean_FA_abs_trait,
        NA_real_
      )
    )

  Composite_FA_Individual <- FA_Pairs_Clean %>%
    filter(
      Trait %in% tratti_FA_validi
    ) %>%
    group_by(ID) %>%
    summarise(
      Sex = primo_non_NA(Sex),
      AgeClass = primo_non_NA(AgeClass),
      Funerary = primo_non_NA(Funerary),

      Composite_FA = ifelse(
        any(!is.na(FA_standardized)),
        mean(
          FA_standardized,
          na.rm = TRUE
        ),
        NA_real_
      ),

      N_FA_traits = sum(
        !is.na(FA_standardized)
      ),

      Composite_FA_complete = ifelse(
        N_tratti_FA_validi > 0 &
          sum(!is.na(FA_standardized)) ==
            N_tratti_FA_validi,
        mean(
          FA_standardized,
          na.rm = TRUE
        ),
        NA_real_
      ),

      .groups = "drop"
    ) %>%
    mutate(
      N_FA_traits_total_available =
        N_tratti_FA_validi,

      Composite_FA_ge2 = ifelse(
        N_FA_traits >= 2,
        Composite_FA,
        NA_real_
      )
    ) %>%
    left_join(
      LEH_Individual %>%
        select(
          ID,
          Individual_LEH,
          Individual_LEH_CountMax,
          N_LEH_teeth_scored
        ),
      by = "ID"
    )

  FA_Pairs_Clean <- FA_Pairs_Clean %>%
    left_join(
      Composite_FA_Individual %>%
        select(
          ID,
          Composite_FA,
          Composite_FA_ge2,
          Composite_FA_complete,
          N_FA_traits,
          N_FA_traits_total_available
        ),
      by = "ID"
    )

  # -------------------------------------------------------------------------
  # RIEPILOGO INDIVIDUALE
  # -------------------------------------------------------------------------

  Individual_Summary <- LEH_Individual %>%
    transmute(
      ID,
      Sex,
      AgeClass,
      Funerary,
      LEH_Present_Individual = Individual_LEH,
      LEH_MaxBands = Individual_LEH_CountMax
    ) %>%
    full_join(
      Composite_FA_Individual %>%
        select(
          ID,
          Composite_FA,
          N_FA_traits,
          Composite_FA_ge2,
          Composite_FA_complete
        ),
      by = "ID"
    ) %>%
    arrange(ID)

  # Audit ME
  if (nrow(ME_log) > 0) {
    FA_ME_Outliers <- ME_log %>%
      select(
        Iteration,
        Trait,
        Arch,
        ToothType,
        Dimension,
        ID,
        Side,
        SourceRow,
        Rep1,
        Rep2,
        RepDiff,
        N_tested,
        G,
        p_raw,
        p_Holm,
        Excluded
      ) %>%
      rename(
        RepDiff_2minus1 = RepDiff
      ) %>%
      arrange(
        Iteration,
        Trait
      )
  } else {
    FA_ME_Outliers <- tibble(
      Nota = "Nessun candidato Grubbs disponibile."
    )
  }

  # Audit R-L
  if (nrow(RL_log) > 0) {
    FA_RL_Outliers <- RL_log %>%
      select(
        Iteration,
        Trait,
        Arch,
        ToothType,
        Dimension,
        ID,
        R_minus_L,
        FA_abs,
        N_tested,
        G,
        p_raw,
        p_Holm,
        Excluded
      ) %>%
      arrange(
        Iteration,
        Trait
      )
  } else {
    FA_RL_Outliers <- tibble(
      Nota = "Nessun candidato Grubbs disponibile."
    )
  }

  # Flag nel master
  chiavi_MD <- character()
  chiavi_BL <- character()
  motivo_MD <- character()
  motivo_BL <- character()

  if (nrow(ME_esclusi) > 0) {

    ME_MD <- ME_esclusi %>%
      filter(Dimension == "MD")

    ME_BL <- ME_esclusi %>%
      filter(Dimension == "BL")

    if (nrow(ME_MD) > 0) {
      chiavi_MD <- ME_MD$RecordKey

      motivo_MD <- setNames(
        paste0(
          "Excluded from FA only: Grubbs replicate difference; iteration ",
          ME_MD$Iteration,
          "; Holm p=",
          signif(
            ME_MD$p_Holm,
            6
          )
        ),
        ME_MD$RecordKey
      )
    }

    if (nrow(ME_BL) > 0) {
      chiavi_BL <- ME_BL$RecordKey

      motivo_BL <- setNames(
        paste0(
          "Excluded from FA only: Grubbs replicate difference; iteration ",
          ME_BL$Iteration,
          "; Holm p=",
          signif(
            ME_BL$p_Holm,
            6
          )
        ),
        ME_BL$RecordKey
      )
    }
  }

  # -------------------------------------------------------------------------
  # DATASET PER LA DIMENSIONE DENTARIA
  # -------------------------------------------------------------------------
  #
  # Per evitare pseudoreplicazione:
  # 1. si media la replica 1 e 2 separatamente per ciascun lato;
  # 2. si calcolano gli indici di dimensione per quel lato;
  # 3. R e L vengono poi mediati per ottenere UNA osservazione per individuo,
  #    tipo dentale e arcata.
  #
  # CrownArea = MD x BL (mm^2): proxy principale della superficie coronale.
  # CrownGM   = sqrt(MD x BL) (mm): media geometrica con unità lineare.
  # CrownModule = (MD + BL)/2 (mm): modulo coronale.
  # CrownIndex  = 100 x BL/MD: indice di forma, NON di sola dimensione.

  Tooth_Size_side <- Tooth_Master %>%
    mutate(
      key_MD = paste(
        SourceRow,
        "MD",
        sep = "_"
      ),
      key_BL = paste(
        SourceRow,
        "BL",
        sep = "_"
      ),

      MD_side = ifelse(
        !is.na(`MD (1)`) &
          !is.na(`MD (2)`) &
          !(key_MD %in% chiavi_MD),
        (`MD (1)` + `MD (2)`) / 2,
        NA_real_
      ),

      BL_side = ifelse(
        !is.na(`BL (1)`) &
          !is.na(`BL (2)`) &
          !(key_BL %in% chiavi_BL),
        (`BL (1)` + `BL (2)`) / 2,
        NA_real_
      ),

      CrownArea_side = ifelse(
        !is.na(MD_side) &
          !is.na(BL_side),
        MD_side * BL_side,
        NA_real_
      ),

      CrownGM_side = ifelse(
        !is.na(CrownArea_side) &
          CrownArea_side > 0,
        sqrt(CrownArea_side),
        NA_real_
      ),

      CrownModule_side = ifelse(
        !is.na(MD_side) &
          !is.na(BL_side),
        (MD_side + BL_side) / 2,
        NA_real_
      ),

      CrownIndex_side = ifelse(
        !is.na(MD_side) &
          !is.na(BL_side) &
          MD_side != 0,
        100 * BL_side / MD_side,
        NA_real_
      )
    )

  Tooth_Size_Individual <- Tooth_Size_side %>%
    group_by(
      ID,
      Arch,
      ToothType
    ) %>%
    summarise(
      Sex = primo_non_NA(Sex),
      AgeClass = primo_non_NA(AgeClass),
      Funerary = primo_non_NA(Funerary),

      N_sides_MD = sum(!is.na(MD_side)),
      N_sides_BL = sum(!is.na(BL_side)),
      N_sides_size = sum(!is.na(CrownArea_side)),

      MD = media_na(MD_side),
      BL = media_na(BL_side),

      CrownArea = media_na(CrownArea_side),
      CrownGM = media_na(CrownGM_side),
      CrownModule = media_na(CrownModule_side),
      CrownIndex = media_na(CrownIndex_side),

      Wear_mean = media_na(
        a_numero(WearStage)
      ),

      N_LEH_same_tooth_scored =
        sum(!is.na(LEH_Present_num)),

      LEH_same_tooth_num = ifelse(
        N_LEH_same_tooth_scored > 0,
        max(LEH_Present_num, na.rm = TRUE),
        NA_real_
      ),

      LEH_same_tooth_CountMax =
        max_na(LEH_Count_num),

      .groups = "drop"
    ) %>%
    mutate(
      SizeTrait = paste0(
        ifelse(Arch == "Upper", "U", "L"),
        ToothType
      ),
      LEH_same_tooth = etichetta_presenza(
        LEH_same_tooth_num
      )
    ) %>%
    left_join(
      LEH_Individual %>%
        select(
          ID,
          Individual_LEH,
          Individual_LEH_CountMax,
          N_LEH_teeth_scored
        ),
      by = "ID"
    ) %>%
    arrange(
      SizeTrait,
      ID
    )

  Tooth_Master_output <- Tooth_Master %>%
    mutate(
      key_MD = paste(
        SourceRow,
        "MD",
        sep = "_"
      ),
      key_BL = paste(
        SourceRow,
        "BL",
        sep = "_"
      ),
      ME_Exclude_MD = ifelse(
        key_MD %in% chiavi_MD,
        1,
        NA
      ),
      ME_Exclude_BL = ifelse(
        key_BL %in% chiavi_BL,
        1,
        NA
      ),
      ME_Reason_MD = unname(
        motivo_MD[key_MD]
      ),
      ME_Reason_BL = unname(
        motivo_BL[key_BL]
      )
    ) %>%
    select(
      -SourceRow,
      -key_MD,
      -key_BL,
      -LEH_Present_num,
      -LEH_Count_num
    )

  FA_Clean_Method <- tibble(
    Voce = c(
      "FA CLEANED ANALYSIS - OUTLIER PROTOCOL",
      "Raw data policy",
      "Cleaning 1",
      "Cleaning 2",
      "Stage 1 - measurement error",
      "Stage 1 test",
      "Stage 1 multiplicity",
      "Stage 1 iteration",
      "Stage 1 exclusion unit",
      "Stage 1 exclusions",
      "Stage 2 - signed asymmetry",
      "Stage 2 test",
      "Stage 2 exclusion unit",
      "Stage 2 exclusions",
      "Clean bilateral pairs",
      "Mixed ANOVA",
      "Directional asymmetry",
      "FA versus measurement error",
      "Normality",
      "Size relationship",
      "Primary individual FA score",
      "Group-test recommendation",
      "LEH blank coding",
      "LEH scorable tooth types",
      "Tooth-size index",
      "Tooth-size independence",
      "Composite FA",
      "Composite FA inclusion",
      "Alpha",
      "Important"
    ),
    Descrizione = c(
      "",
      "Nessuna misura odontometrica originale viene cancellata o modificata; le esclusioni sono flag analitici.",
      "ToothType i2 standardizzato a I2.",
      "Eliminato il duplicato T54 Lower L M2 con WearStage 5; mantenuto WearStage 3.",
      "Per ciascun tratto Arch x ToothType x Dimension vengono esaminate le differenze signed Rep2-Rep1.",
      "Test di Grubbs a due code sul valore più estremo di ciascun tratto.",
      "A ogni iterazione i p-value dei candidati dei diversi tratti vengono corretti con il metodo di Holm.",
      "Dopo le esclusioni confermate il test viene ripetuto; ci si ferma quando nessun nuovo candidato ha p_Holm < alpha.",
      "Un outlier ME esclude soltanto quello specifico dente-lato-dimensione dalla FA.",
      as.character(nrow(ME_esclusi)),
      "Dopo la pulizia ME le repliche vengono mediate per R e L; l'asimmetria signed è R-L.",
      "Test di Grubbs a due code su R-L con la stessa procedura iterativa e correzione di Holm.",
      "Un outlier R-L esclude soltanto quella coppia bilaterale da quello specifico tratto.",
      as.character(nrow(RL_esclusi)),
      as.character(nrow(FA_Pairs_Clean)),
      "ANOVA mista a due vie: Individual=random; Side=fixed; due misure replicate per lato.",
      "L'effetto Side viene testato contro Individual x Side.",
      "Individual x Side viene testato contro l'errore residuo tra repliche.",
      "D'Agostino-Pearson K2 sulla distribuzione R-L; Shapiro-Wilk soltanto se n<8.",
      "Correlazione di Spearman tra |R-L| e dimensione media (R+L)/2.",
      "FA_abs = |R-L|.",
      "Nel foglio FA_Pairs_Clean usare FA_abs; per l'analisi primaria filtrare Use_for_group_tests = Yes.",
      ifelse(
        leh_blank_absence,
        "Per i tipi dentali nei quali compare almeno un LEH positivo, i blank di LEH_Present sono interpretati come assenza.",
        "I blank di LEH_Present restano missing e non vengono interpretati come assenza."
      ),
      paste(
        denti_LEH_scorabili,
        collapse = ", "
      ),
      "CrownArea = MD x BL (mm^2) e' la misura principale di dimensione coronale; CrownGM = sqrt(MD x BL) e' disponibile come indice lineare.",
      "Le dimensioni R e L non sono trattate come osservazioni indipendenti: gli indici vengono calcolati per lato e poi mediati per individuo e tipo dentale.",
      "FA_abs viene standardizzata dividendo per la media campionaria FA_abs dello stesso tratto; Composite_FA e' la media individuale dei valori standardizzati dei soli tratti che superano lo screening.",
      "N_FA_traits registra il numero di tratti inclusi; Composite_FA_ge2 richiede almeno 2 tratti; Composite_FA_complete richiede tutti i tratti validi.",
      as.character(alpha),
      "Non sono state applicate esclusioni biologiche per usura, carie, danno, tartaro ecc. L'eta' puo' influenzare soprattutto MD tramite usura interprossimale."
    )
  )

  list(
    Tooth_Master = Tooth_Master_output,
    LEH_Bands = LEH_Bands,
    FA_ME_Outliers = FA_ME_Outliers,
    FA_RL_Outliers = FA_RL_Outliers,
    FA_Pairs_Clean = FA_Pairs_Clean,
    FA_ANOVA_Clean = FA_ANOVA_Clean,
    FA_ANOVA_Clean_Detail = FA_ANOVA_Clean_Detail,
    Tooth_Size_Individual = Tooth_Size_Individual,
    LEH_Individual = LEH_Individual,
    Composite_FA_Individual = Composite_FA_Individual,
    Individual_Summary = Individual_Summary,
    FA_Clean_Method = FA_Clean_Method,
    ME_n = nrow(ME_esclusi),
    RL_n = nrow(RL_esclusi),
    Pair_n = nrow(FA_Pairs_Clean),
    usable_traits = FA_ANOVA_Clean %>%
      filter(
        PaperComparable == "Yes",
        Screening_status ==
          "passes basic screening"
      ) %>%
      pull(Trait)
  )
}


# ---------------------------------------------------------------------------
# 3. FUNZIONE PER CREARE IL FILE EXCEL DA SCARICARE
# ---------------------------------------------------------------------------

crea_excel_finale <- function(
    risultati,
    file_output) {

  wb <- openxlsx::createWorkbook()

  stile_header <- openxlsx::createStyle(
    fgFill = "#1F4E78",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    valign = "center"
  )

  stile_ok <- openxlsx::createStyle(
    fgFill = "#D9EAD3",
    textDecoration = "bold"
  )

  stile_outlier <- openxlsx::createStyle(
    fgFill = "#F4CCCC",
    textDecoration = "bold"
  )

  scrivi_foglio <- function(
      nome,
      dati,
      filtro = TRUE) {

    openxlsx::addWorksheet(
      wb,
      nome
    )

    openxlsx::writeData(
      wb,
      sheet = nome,
      x = dati,
      withFilter = FALSE
    )

    openxlsx::addStyle(
      wb,
      sheet = nome,
      style = stile_header,
      rows = 1,
      cols = seq_len(ncol(dati)),
      gridExpand = TRUE
    )

    openxlsx::freezePane(
      wb,
      sheet = nome,
      firstRow = TRUE
    )

    if (filtro && ncol(dati) > 0) {
      openxlsx::addFilter(
        wb,
        sheet = nome,
        rows = 1,
        cols = seq_len(ncol(dati))
      )
    }

    openxlsx::setColWidths(
      wb,
      sheet = nome,
      cols = seq_len(ncol(dati)),
      widths = "auto"
    )
  }

  scrivi_foglio(
    "Tooth_Master",
    risultati$Tooth_Master
  )

  scrivi_foglio(
    "LEH_Bands",
    risultati$LEH_Bands
  )

  scrivi_foglio(
    "FA_ME_Outliers",
    risultati$FA_ME_Outliers
  )

  scrivi_foglio(
    "FA_RL_Outliers",
    risultati$FA_RL_Outliers
  )

  scrivi_foglio(
    "FA_Pairs_Clean",
    risultati$FA_Pairs_Clean
  )

  scrivi_foglio(
    "FA_ANOVA_Clean",
    risultati$FA_ANOVA_Clean
  )

  scrivi_foglio(
    "FA_ANOVA_Clean_Detail",
    risultati$FA_ANOVA_Clean_Detail
  )

  scrivi_foglio(
    "Tooth_Size_Individual",
    risultati$Tooth_Size_Individual
  )

  scrivi_foglio(
    "LEH_Individual",
    risultati$LEH_Individual
  )

  scrivi_foglio(
    "Composite_FA_Individual",
    risultati$Composite_FA_Individual
  )

  scrivi_foglio(
    "Individual_Summary",
    risultati$Individual_Summary
  )

  scrivi_foglio(
    "FA_Clean_Method",
    risultati$FA_Clean_Method,
    filtro = FALSE
  )

  # Evidenzia le righe/celle principali quando le colonne esistono.
  if ("Use_for_group_tests" %in%
      names(risultati$FA_Pairs_Clean)) {

    col_use <- which(
      names(risultati$FA_Pairs_Clean) ==
        "Use_for_group_tests"
    )

    righe_si <- which(
      risultati$FA_Pairs_Clean$Use_for_group_tests ==
        "Yes"
    ) + 1

    if (length(righe_si) > 0) {
      openxlsx::addStyle(
        wb,
        "FA_Pairs_Clean",
        stile_ok,
        rows = righe_si,
        cols = col_use,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  if ("Excluded" %in%
      names(risultati$FA_ME_Outliers)) {

    col_excl <- which(
      names(risultati$FA_ME_Outliers) ==
        "Excluded"
    )

    righe_escl <- which(
      risultati$FA_ME_Outliers$Excluded ==
        "Yes"
    ) + 1

    if (length(righe_escl) > 0) {
      openxlsx::addStyle(
        wb,
        "FA_ME_Outliers",
        stile_outlier,
        rows = righe_escl,
        cols = col_excl,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  if ("Excluded" %in%
      names(risultati$FA_RL_Outliers)) {

    col_excl <- which(
      names(risultati$FA_RL_Outliers) ==
        "Excluded"
    )

    righe_escl <- which(
      risultati$FA_RL_Outliers$Excluded ==
        "Yes"
    ) + 1

    if (length(righe_escl) > 0) {
      openxlsx::addStyle(
        wb,
        "FA_RL_Outliers",
        stile_outlier,
        rows = righe_escl,
        cols = col_excl,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  openxlsx::saveWorkbook(
    wb,
    file = file_output,
    overwrite = TRUE
  )
}


# ---------------------------------------------------------------------------
# 4. INTERFACCIA GRAFICA
# ---------------------------------------------------------------------------

ui <- fluidPage(

  tags$head(
    tags$title("DfaR")
  ),

  titlePanel(
    "DfaR"
  ),

  sidebarLayout(

    sidebarPanel(

      fileInput(
        "file",
        "1. Carica il file Excel originale",
        accept = c(".xlsx")
      ),

      numericInput(
        "alpha",
        "Livello alfa per lo screening FA",
        value = 0.05,
        min = 0.001,
        max = 0.10,
        step = 0.01
      ),

      checkboxInput(
        "m3",
        "Includi M3 come tratto FA esplorativo",
        value = TRUE
      ),

      checkboxInput(
        "leh_blank_absence",
        "LEH: interpreta i blank come assenza nei tipi dentali effettivamente scorati",
        value = TRUE
      ),

      helpText(
        "Nel file attuale LEH_Present usa valori 1 e celle vuote. Questa opzione rende esplicita l'interpretazione dei vuoti."
      ),

      actionButton(
        "run",
        "2. Prepara i dati",
        class = "btn-primary"
      ),

      br(),
      br(),

      downloadButton(
        "download",
        "3. Scarica Excel finale"
      ),

      hr(),

      helpText(
        "Il file originale non viene modificato."
      )
    ),

    mainPanel(

      tabsetPanel(

        # -------------------------------------------------------------------
        # RIEPILOGO
        # -------------------------------------------------------------------
        tabPanel(
          "Riepilogo",
          br(),
          uiOutput("summary_boxes"),
          br(),
          h4("Tratti FA consigliati per le analisi primarie"),
          verbatimTextOutput("usable_traits"),
          hr(),
          h4("Dove sono i dati da usare"),
          tags$ul(
            tags$li(
              tags$b("FA: "),
              "FA_Pairs_Clean; risposta primaria = FA_abs"
            ),
            tags$li(
              tags$b("Dimensione dentaria: "),
              "Tooth_Size_Individual; risposta primaria = CrownArea"
            ),
            tags$li(
              tags$b("LEH individuale: "),
              "LEH_Individual"
            )
          )
        ),

        # -------------------------------------------------------------------
        # ANALISI FA
        # -------------------------------------------------------------------
        tabPanel(
          "FA: grafici e test",
          br(),

          fluidRow(
            column(
              4,
              checkboxInput(
                "fa_primary",
                "Usa soltanto tratti che superano lo screening FA",
                value = TRUE
              )
            ),
            column(
              4,
              selectInput(
                "fa_trait",
                "Tratto",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "fa_response",
                "Indice FA",
                choices = c(
                  "FA assoluta |R-L|" = "FA_abs",
                  "FA relativa" = "FA_relative"
                ),
                selected = "FA_abs"
              )
            )
          ),

          fluidRow(
            column(
              6,
              selectInput(
                "fa_predictor",
                "Variabile da confrontare con FA",
                choices = c(
                  "Sesso" = "Sex",
                  "Classe di eta'" = "AgeClass",
                  "Trattamento funerario" = "Funerary",
                  "LEH individuale" = "Individual_LEH",
                  "LEH dello stesso tipo dentale" = "LEH_same_tooth",
                  "Dimensione media dello stesso carattere" = "Mean_size",
                  "Usura media della coppia" = "Wear_mean_pair"
                )
              )
            ),
            column(
              6,
              checkboxInput(
                "fa_certain_sex",
                "Se X = sesso, usa solo M e F certi",
                value = TRUE
              )
            )
          ),

          plotOutput(
            "fa_plot",
            height = "460px"
          ),

          h4("Test statistico"),
          verbatimTextOutput(
            "fa_test"
          ),

          h4("Dati usati nel test"),
          DTOutput(
            "fa_analysis_table"
          ),

          hr(),
          h3("Composite FA individuale"),

          p(
            "Ogni FA_abs viene standardizzata rispetto alla media del proprio tratto; il punteggio individuale e' la media dei tratti FA validi disponibili."
          ),

          fluidRow(
            column(
              4,
              selectInput(
                "cfa_response",
                "Punteggio composito",
                choices = c(
                  "Composite FA - almeno 2 tratti" = "Composite_FA_ge2",
                  "Composite FA - tutti i tratti disponibili" = "Composite_FA",
                  "Composite FA - complete case" = "Composite_FA_complete"
                ),
                selected = "Composite_FA_ge2"
              )
            ),
            column(
              4,
              selectInput(
                "cfa_predictor",
                "Variabile di confronto",
                choices = c(
                  "Sesso" = "Sex",
                  "Classe di eta'" = "AgeClass",
                  "Trattamento funerario" = "Funerary",
                  "LEH individuale" = "Individual_LEH",
                  "Numero massimo di bande LEH" = "Individual_LEH_CountMax"
                )
              )
            ),
            column(
              4,
              checkboxInput(
                "cfa_certain_sex",
                "Se X = sesso, usa solo M e F certi",
                value = TRUE
              )
            )
          ),

          plotOutput(
            "cfa_plot",
            height = "430px"
          ),

          h4("Test statistico sul Composite FA"),
          verbatimTextOutput(
            "cfa_test"
          ),

          h4("Dati individuali del Composite FA"),
          DTOutput(
            "cfa_table"
          )
        ),

        # -------------------------------------------------------------------
        # DIMENSIONE DENTARIA
        # -------------------------------------------------------------------
        tabPanel(
          "Dimensione dentaria",
          br(),

          p(
            tags$b("Misura principale: "),
            "CrownArea = MD × BL (mm²). È disponibile anche CrownGM = √(MD × BL), che mantiene unità lineari (mm). R e L vengono prima calcolati separatamente e poi mediati, quindi ogni individuo compare una sola volta per tipo dentale."
          ),

          fluidRow(
            column(
              4,
              selectInput(
                "size_trait",
                "Tipo dentale / arcata",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "size_response",
                "Variabile di dimensione",
                choices = c(
                  "Crown area = MD × BL (mm²)" = "CrownArea",
                  "Geometric mean = √(MD × BL) (mm)" = "CrownGM",
                  "Crown module = (MD + BL)/2 (mm)" = "CrownModule",
                  "Mesiodistal diameter (mm)" = "MD",
                  "Buccolingual diameter (mm)" = "BL",
                  "Crown index = 100 × BL/MD (forma)" = "CrownIndex"
                ),
                selected = "CrownArea"
              )
            ),
            column(
              4,
              selectInput(
                "size_predictor",
                "Variabile da confrontare",
                choices = c(
                  "Sesso" = "Sex",
                  "Classe di eta'" = "AgeClass",
                  "Trattamento funerario" = "Funerary",
                  "LEH individuale" = "Individual_LEH",
                  "LEH dello stesso tipo dentale" = "LEH_same_tooth",
                  "Usura media" = "Wear_mean"
                )
              )
            )
          ),

          checkboxInput(
            "size_certain_sex",
            "Se X = sesso, usa solo M e F certi",
            value = TRUE
          ),

          conditionalPanel(
            condition = "input.size_predictor == 'AgeClass'",
            div(
              style = "padding:10px; background:#fff3cd; border:1px solid #ffe69c; margin-bottom:15px;",
              tags$b("Nota metodologica: "),
              "la dimensione coronale biologica non cambia dopo la formazione, ma soprattutto MD può diminuire con l'usura interprossimale. Un'associazione con AgeClass deve quindi essere interpretata insieme a Wear_mean."
            )
          ),

          plotOutput(
            "size_plot",
            height = "460px"
          ),

          h4("Test statistico"),
          verbatimTextOutput(
            "size_test"
          ),

          h4("Dati usati nel test"),
          DTOutput(
            "size_analysis_table"
          )
        ),

        # -------------------------------------------------------------------
        # LEH
        # -------------------------------------------------------------------
        tabPanel(
          "LEH: grafici e test",
          br(),

          fluidRow(
            column(
              6,
              selectInput(
                "leh_response",
                "Variabile LEH",
                choices = c(
                  "Presenza/assenza individuale" = "Individual_LEH",
                  "Numero massimo di bande osservate" = "Individual_LEH_CountMax"
                ),
                selected = "Individual_LEH"
              )
            ),
            column(
              6,
              selectInput(
                "leh_predictor",
                "Variabile di confronto",
                choices = c(
                  "Sesso" = "Sex",
                  "Classe di eta'" = "AgeClass",
                  "Trattamento funerario" = "Funerary"
                )
              )
            )
          ),

          checkboxInput(
            "leh_certain_sex",
            "Se X = sesso, usa solo M e F certi",
            value = TRUE
          ),

          plotOutput(
            "leh_plot",
            height = "460px"
          ),

          h4("Test statistico"),
          verbatimTextOutput(
            "leh_test"
          ),

          h4("Dati usati nel test"),
          DTOutput(
            "leh_analysis_table"
          )
        ),

        # -------------------------------------------------------------------
        # TABELLE DI CONTROLLO
        # -------------------------------------------------------------------
        tabPanel(
          "ANOVA FA",
          br(),
          DTOutput("anova_table")
        ),

        tabPanel(
          "FA individuale",
          br(),
          DTOutput("pairs_table")
        ),

        tabPanel(
          "Composite FA data",
          br(),
          DTOutput("composite_table")
        ),

        tabPanel(
          "Tooth size data",
          br(),
          DTOutput("size_table")
        ),

        tabPanel(
          "Outlier misura",
          br(),
          DTOutput("me_table")
        ),

        tabPanel(
          "Outlier R-L",
          br(),
          DTOutput("rl_table")
        ),

        tabPanel(
          "Metodo",
          br(),
          DTOutput("method_table")
        )
      )
    )
  )
)


# ---------------------------------------------------------------------------
# 5. SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  risultati <- reactiveVal(NULL)

  observeEvent(
    input$run,
    {

      req(input$file)

      withProgress(
        message = "Preparazione e analisi dei dati...",
        value = 0,
        {

          incProgress(
            0.15,
            detail = "Lettura e controllo del file"
          )

          risultato <- tryCatch(
            {

              incProgress(
                0.30,
                detail = "Screening FA e outlier"
              )

              x <- esegui_pipeline_FA(
                file_input = input$file$datapath,
                alpha = input$alpha,
                includi_M3 = input$m3,
                leh_blank_absence = input$leh_blank_absence
              )

              incProgress(
                0.45,
                detail = "Preparazione dei dataset biologici"
              )

              x
            },
            error = function(e) {

              showModal(
                modalDialog(
                  title = "Errore",
                  paste(
                    "L'analisi non è stata completata:",
                    e$message
                  ),
                  easyClose = TRUE
                )
              )

              NULL
            }
          )

          risultati(risultato)
        }
      )

      if (!is.null(risultati())) {
        showNotification(
          "Dati preparati correttamente.",
          type = "message",
          duration = 5
        )
      }
    }
  )


  # -------------------------------------------------------------------------
  # AGGIORNA LE SCELTE DEI TRATTI
  # -------------------------------------------------------------------------

  observe({

    x <- risultati()
    req(x)

    if (isTRUE(input$fa_primary)) {

      scelte_FA <- x$usable_traits

    } else {

      scelte_FA <- sort(
        unique(
          x$FA_Pairs_Clean$Trait
        )
      )
    }

    updateSelectInput(
      session,
      "fa_trait",
      choices = scelte_FA,
      selected = if (length(scelte_FA) > 0) scelte_FA[1] else NULL
    )

    scelte_size <- sort(
      unique(
        x$Tooth_Size_Individual$SizeTrait[
          !is.na(x$Tooth_Size_Individual$CrownArea)
        ]
      )
    )

    updateSelectInput(
      session,
      "size_trait",
      choices = scelte_size,
      selected = if (length(scelte_size) > 0) scelte_size[1] else NULL
    )
  })


  # -------------------------------------------------------------------------
  # RIEPILOGO
  # -------------------------------------------------------------------------

  output$summary_boxes <- renderUI({

    x <- risultati()
    req(x)

    fluidRow(

      column(
        4,
        wellPanel(
          h3(
            x$ME_n,
            style = "margin-top:0"
          ),
          "outlier di errore di misura esclusi"
        )
      ),

      column(
        4,
        wellPanel(
          h3(
            x$RL_n,
            style = "margin-top:0"
          ),
          "outlier R-L esclusi"
        )
      ),

      column(
        4,
        wellPanel(
          h3(
            x$Pair_n,
            style = "margin-top:0"
          ),
          "coppie FA pulite"
        )
      )
    )
  })


  output$usable_traits <- renderText({

    x <- risultati()
    req(x)

    if (length(x$usable_traits) == 0) {
      return(
        "Nessun tratto supera tutti i criteri di screening."
      )
    }

    paste(
      x$usable_traits,
      collapse = "\n"
    )
  })


  # -------------------------------------------------------------------------
  # ANALISI FA
  # -------------------------------------------------------------------------

  fa_dati <- reactive({

    x <- risultati()
    req(x, input$fa_trait, input$fa_predictor)

    d <- x$FA_Pairs_Clean %>%
      filter(
        Trait == input$fa_trait
      )

    if (
      input$fa_predictor == "Sex" &&
      isTRUE(input$fa_certain_sex)
    ) {
      d <- d %>%
        filter(
          Sex %in% c("M", "F")
        )
    }

    d
  })


  fa_result <- reactive({

    d <- fa_dati()
    req(nrow(d) > 0)

    analisi_numerica_automatica(
      dati = d,
      y = input$fa_response,
      x = input$fa_predictor
    )
  })


  output$fa_plot <- renderPlot({

    res <- fa_result()
    d <- res$dati

    validate(
      need(
        nrow(d) >= 3,
        "Dati insufficienti per il grafico."
      )
    )

    y <- input$fa_response
    xvar <- input$fa_predictor

    if (
      is.numeric(d[[xvar]]) &&
      length(unique(d[[xvar]])) > 5
    ) {

      ggplot(
        d,
        aes(
          x = .data[[xvar]],
          y = .data[[y]]
        )
      ) +
        geom_point(
          size = 2.5,
          alpha = 0.75
        ) +
        geom_smooth(
          method = "loess",
          se = FALSE
        ) +
        labs(
          x = xvar,
          y = y,
          title = paste(
            input$fa_trait,
            "-",
            y,
            "vs",
            xvar
          )
        ) +
        theme_minimal(
          base_size = 13
        )

    } else {

      ggplot(
        d,
        aes(
          x = factor(.data[[xvar]]),
          y = .data[[y]]
        )
      ) +
        geom_boxplot(
          outlier.shape = NA
        ) +
        geom_jitter(
          width = 0.12,
          height = 0,
          alpha = 0.70,
          size = 2.2
        ) +
        labs(
          x = xvar,
          y = y,
          title = paste(
            input$fa_trait,
            "-",
            y,
            "by",
            xvar
          )
        ) +
        theme_minimal(
          base_size = 13
        )
    }
  })


  output$fa_test <- renderText({
    fa_result()$testo
  })


  output$fa_analysis_table <- renderDT({

    d <- fa_result()$dati

    datatable(
      d,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })


  # -------------------------------------------------------------------------
  # COMPOSITE FA INDIVIDUALE
  # -------------------------------------------------------------------------

  cfa_dati <- reactive({

    x <- risultati()
    req(x, input$cfa_predictor)

    d <- x$Composite_FA_Individual

    if (
      input$cfa_predictor == "Sex" &&
      isTRUE(input$cfa_certain_sex)
    ) {
      d <- d %>%
        filter(
          Sex %in% c("M", "F")
        )
    }

    d
  })


  cfa_result <- reactive({

    d <- cfa_dati()
    req(nrow(d) > 0)

    analisi_numerica_automatica(
      dati = d,
      y = input$cfa_response,
      x = input$cfa_predictor
    )
  })


  output$cfa_plot <- renderPlot({

    res <- cfa_result()
    d <- res$dati

    validate(
      need(
        nrow(d) >= 3,
        "Dati insufficienti per il grafico."
      )
    )

    y <- input$cfa_response
    xvar <- input$cfa_predictor

    ggplot(
      d,
      aes(
        x = factor(.data[[xvar]]),
        y = .data[[y]]
      )
    ) +
      geom_boxplot(
        outlier.shape = NA
      ) +
      geom_jitter(
        width = 0.12,
        height = 0,
        alpha = 0.70,
        size = 2.2
      ) +
      labs(
        x = xvar,
        y = y,
        title = paste(
          y,
          "by",
          xvar
        )
      ) +
      theme_minimal(
        base_size = 13
      )
  })


  output$cfa_test <- renderText({
    cfa_result()$testo
  })


  output$cfa_table <- renderDT({

    d <- cfa_result()$dati

    datatable(
      d,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })


  # -------------------------------------------------------------------------
  # ANALISI DIMENSIONE DENTARIA
  # -------------------------------------------------------------------------

  size_dati <- reactive({

    x <- risultati()
    req(x, input$size_trait, input$size_predictor)

    d <- x$Tooth_Size_Individual %>%
      filter(
        SizeTrait == input$size_trait
      )

    if (
      input$size_predictor == "Sex" &&
      isTRUE(input$size_certain_sex)
    ) {
      d <- d %>%
        filter(
          Sex %in% c("M", "F")
        )
    }

    d
  })


  size_result <- reactive({

    d <- size_dati()
    req(nrow(d) > 0)

    analisi_numerica_automatica(
      dati = d,
      y = input$size_response,
      x = input$size_predictor
    )
  })


  output$size_plot <- renderPlot({

    res <- size_result()
    d <- res$dati

    validate(
      need(
        nrow(d) >= 3,
        "Dati insufficienti per il grafico."
      )
    )

    y <- input$size_response
    xvar <- input$size_predictor

    if (
      is.numeric(d[[xvar]]) &&
      length(unique(d[[xvar]])) > 5
    ) {

      ggplot(
        d,
        aes(
          x = .data[[xvar]],
          y = .data[[y]]
        )
      ) +
        geom_point(
          size = 2.5,
          alpha = 0.75
        ) +
        geom_smooth(
          method = "loess",
          se = FALSE
        ) +
        labs(
          x = xvar,
          y = y,
          title = paste(
            input$size_trait,
            "-",
            y,
            "vs",
            xvar
          )
        ) +
        theme_minimal(
          base_size = 13
        )

    } else {

      ggplot(
        d,
        aes(
          x = factor(.data[[xvar]]),
          y = .data[[y]]
        )
      ) +
        geom_boxplot(
          outlier.shape = NA
        ) +
        geom_jitter(
          width = 0.12,
          height = 0,
          alpha = 0.70,
          size = 2.2
        ) +
        labs(
          x = xvar,
          y = y,
          title = paste(
            input$size_trait,
            "-",
            y,
            "by",
            xvar
          )
        ) +
        theme_minimal(
          base_size = 13
        )
    }
  })


  output$size_test <- renderText({
    size_result()$testo
  })


  output$size_analysis_table <- renderDT({

    d <- size_result()$dati

    datatable(
      d,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })


  # -------------------------------------------------------------------------
  # ANALISI LEH
  # -------------------------------------------------------------------------

  leh_dati <- reactive({

    x <- risultati()
    req(x, input$leh_predictor)

    d <- x$LEH_Individual

    if (
      input$leh_predictor == "Sex" &&
      isTRUE(input$leh_certain_sex)
    ) {
      d <- d %>%
        filter(
          Sex %in% c("M", "F")
        )
    }

    d
  })


  leh_result <- reactive({

    d <- leh_dati()
    req(nrow(d) > 0)

    if (input$leh_response == "Individual_LEH") {

      analisi_categoriale_fisher(
        dati = d,
        y = "Individual_LEH",
        x = input$leh_predictor
      )

    } else {

      analisi_numerica_automatica(
        dati = d,
        y = "Individual_LEH_CountMax",
        x = input$leh_predictor
      )
    }
  })


  output$leh_plot <- renderPlot({

    d <- leh_result()$dati

    validate(
      need(
        nrow(d) >= 3,
        "Dati insufficienti per il grafico."
      )
    )

    xvar <- input$leh_predictor

    if (input$leh_response == "Individual_LEH") {

      dd <- d %>%
        filter(
          !is.na(Individual_LEH),
          !is.na(.data[[xvar]])
        ) %>%
        count(
          .data[[xvar]],
          Individual_LEH,
          name = "n"
        ) %>%
        group_by(
          .data[[xvar]]
        ) %>%
        mutate(
          proportion = n / sum(n)
        ) %>%
        ungroup()

      ggplot(
        dd,
        aes(
          x = factor(.data[[xvar]]),
          y = proportion,
          fill = Individual_LEH
        )
      ) +
        geom_col(
          position = "fill"
        ) +
        scale_y_continuous(
          labels = scales::percent
        ) +
        labs(
          x = xvar,
          y = "Proporzione",
          fill = "LEH",
          title = paste(
            "LEH individuale by",
            xvar
          )
        ) +
        theme_minimal(
          base_size = 13
        )

    } else {

      ggplot(
        d,
        aes(
          x = factor(.data[[xvar]]),
          y = Individual_LEH_CountMax
        )
      ) +
        geom_boxplot(
          outlier.shape = NA
        ) +
        geom_jitter(
          width = 0.12,
          alpha = 0.70,
          size = 2.2
        ) +
        labs(
          x = xvar,
          y = "Numero massimo di bande LEH",
          title = paste(
            "LEH count by",
            xvar
          )
        ) +
        theme_minimal(
          base_size = 13
        )
    }
  })


  output$leh_test <- renderText({
    leh_result()$testo
  })


  output$leh_analysis_table <- renderDT({

    d <- leh_result()$dati

    datatable(
      d,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })


  # -------------------------------------------------------------------------
  # TABELLE DI CONTROLLO
  # -------------------------------------------------------------------------

  output$anova_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$FA_ANOVA_Clean,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })


  output$pairs_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$FA_Pairs_Clean,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })


  output$composite_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$Composite_FA_Individual,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })


  output$size_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$Tooth_Size_Individual,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })


  output$me_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$FA_ME_Outliers,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })


  output$rl_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$FA_RL_Outliers,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
  })


  output$method_table <- renderDT({

    x <- risultati()
    req(x)

    datatable(
      x$FA_Clean_Method,
      options = list(
        pageLength = 30,
        scrollX = FALSE
      ),
      rownames = FALSE
    )
  })


  # -------------------------------------------------------------------------
  # DOWNLOAD EXCEL
  # -------------------------------------------------------------------------

  output$download <- downloadHandler(

    filename = function() {
      "DfaR_results.xlsx"
    },

    content = function(file) {

      x <- risultati()

      if (is.null(x)) {
        stop(
          "Prima preparare i dati."
        )
      }

      crea_excel_finale(
        risultati = x,
        file_output = file
      )
    }
  )
}


# ---------------------------------------------------------------------------
# 6. AVVIO DELL'APP
# ---------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
