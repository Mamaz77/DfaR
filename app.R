###############################################################################
# DfaR - DENTAL FLUCTUATING ASYMMETRY (FA) ANALYSIS
#
# Use:
# 1. Open this file in RStudio.
# 2. Click "Run App".
# 3. Upload the original Excel file.
# 4. Click "Prepare data".
# 5. Check the results.
# 6. Download the final Excel file.
#
# The app NEVER modifies the original file.
###############################################################################

# ---------------------------------------------------------------------------
# 0. PACKAGES
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
# 1. HELPER FUNCTIONS
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
      stop("The Grubbs procedure exceeded 50 iterations.")
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

# Converts common presence/absence codings to 0/1.
# Leaves NA when the value cannot be interpreted.
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


# Automatic analysis for a numeric response Y.
#
# If X is numeric:
#   Spearman.
#
# If X is categorical with 2 groups:
#   Wilcoxon rank-sum.
#
# If X is categorical with >2 groups:
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
        testo = "Insufficient data for the test.",
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
      "Test: Spearman correlation\n",
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
        testo = "The selected variable contains fewer than two usable groups.",
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
      "\n\nPairwise Wilcoxon comparisons, Holm-adjusted p-values:\n",
      paste(matrice_p, collapse = "\n")
    )
  }

  list(
    testo = testo,
    dati = d,
    tipo = "factor"
  )
}


# Test for a binary/categorical response (e.g. LEH presence).
# Uses Fisher exact; preferable for small samples or sparse cells.
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
        testo = "Insufficient data for the test.",
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
        testo = "At least two levels are required for both variables.",
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
      "Test: chi-square with simulated p-value (10,000 replicates)\n",
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
# 2. MAIN FUNCTION: RUNS THE COMPLETE WORKFLOW
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
        "The following Excel sheets are missing: ",
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
        "The following columns are missing from Tooth_Master: ",
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
  # INTERPRETATION OF BLANKS IN LEH_Present
  # -------------------------------------------------------
  #
  # In the original dataset, value 1 may indicate presence
  # e celle vuote invece di espliciti 0.
  #
  # Se leh_blank_absence = TRUE:
  #   1. identify tooth types with at least one positive LEH observation;
  #   2. ONLY for those tooth types, interpret blanks as "absence".
  #
  # This avoids automatically converting blanks to absence
  # for teeth that were not used for LEH recording.
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

  # STAGE 1: measurement error
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
      "Multiple records exist for the same individual, trait, and side."
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

  # STAGE 2: R-L outliers
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

  # ANOVA by trait
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
  # LEH AT THE INDIVIDUAL AND SAME-TOOTH-TYPE LEVEL
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
  # COMPOSITE FLUCTUATING ASYMMETRY (FA) AT THE INDIVIDUAL LEVEL
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
  # INDIVIDUAL SUMMARY
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
      Nota = "No Grubbs candidate available."
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
      Nota = "No Grubbs candidate available."
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
  # TOOTH-SIZE DATASET
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
    Item = c(
      "FA CLEANED ANALYSIS - OUTLIER PROTOCOL",
      "Raw data policy",
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
    Description = c(
      "",
      "No original odontometric measurement is deleted or modified; exclusions are analytical flags.",
      "For each Arch × ToothType × Dimension trait, signed Rep2-Rep1 differences are examined.",
      "Two-sided Grubbs test on the most extreme value of each trait.",
      "At each iteration, candidate p-values across traits are adjusted using the Holm method.",
      "After confirmed exclusions, the test is repeated until no new candidate has p_Holm < alpha.",
      "A measurement-error outlier excludes only that specific tooth-side-dimension from FA.",
      as.character(nrow(ME_esclusi)),
      "After measurement-error cleaning, replicates are averaged for R and L; signed asymmetry is R-L.",
      "Two-sided Grubbs test on R-L using the same iterative procedure and Holm correction.",
      "An R-L outlier excludes only that bilateral pair from that specific trait.",
      as.character(nrow(RL_esclusi)),
      as.character(nrow(FA_Pairs_Clean)),
      "Two-way mixed ANOVA: Individual=random; Side=fixed; two replicated measurements per side.",
      "The Side effect is tested against Individual × Side.",
      "Individual × Side is tested against residual replicate error.",
      "D’Agostino-Pearson K2 for the R-L distribution; Shapiro-Wilk only if n<8.",
      "Spearman correlation between |R-L| and mean trait size (R+L)/2.",
      "FA_abs = |R-L|.",
      "In FA_Pairs_Clean use FA_abs; for the primary analysis filter Use_for_group_tests = Yes.",
      ifelse(
        leh_blank_absence,
        "For tooth types with at least one positive LEH observation, blank LEH_Present cells are interpreted as absence.",
        "Blank LEH_Present cells remain missing and are not interpreted as absence."
      ),
      paste(
        denti_LEH_scorabili,
        collapse = ", "
      ),
      "CrownArea = MD × BL (mm²) is the primary crown-size measure; CrownGM = sqrt(MD × BL) is available as a linear index.",
      "Right and left dimensions are not treated as independent observations: indices are calculated by side and then averaged by individual and tooth type.",
      "FA_abs is standardized by dividing it by the sample mean FA_abs of the same trait; Composite_FA is the individual mean of standardized values from traits that pass screening.",
      "N_FA_traits records the number of included traits; Composite_FA_ge2 requires at least 2 traits; Composite_FA_complete requires all valid traits.",
      as.character(alpha),
      "No biological exclusions for wear, caries, damage, calculus, etc. were applied. Age may affect MD in particular through interproximal wear."
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
# 3. FUNCTION TO CREATE THE DOWNLOADABLE EXCEL FILE
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
# 4. USER INTERFACE
# ---------------------------------------------------------------------------

# Embedded into app.R by integrate_dfar_help.R; no extra runtime files needed.
dfar_help_css <- function() tags$style(HTML('
.dfar-guide {max-width:1040px; color:#263745; line-height:1.65; padding:8px 4px 28px;}
.dfar-guide h2,.dfar-guide h3 {color:#164b60;}
.dfar-guide .guide-lead {font-size:17px; max-width:850px;}
.dfar-guide .guide-steps {background:#eff6f8; border-left:4px solid #27778c; padding:18px 22px; border-radius:5px;}
.dfar-guide li {margin-bottom:8px;}
.dfar-guide details {border:1px solid #d5e1e5; border-radius:6px; margin:12px 0; padding:14px 18px; background:white;}
.dfar-guide summary {cursor:pointer; font-size:17px; font-weight:600; color:#164b60;}
.dfar-guide details[open] summary {margin-bottom:14px;}
.dfar-guide .guide-table-wrap {overflow-x:auto; margin:12px 0;}
.dfar-guide table {border-collapse:collapse; width:100%; font-size:14px;}
.dfar-guide th,.dfar-guide td {text-align:left; vertical-align:top; padding:10px; border-bottom:1px solid #dbe4e8;}
.dfar-guide th {background:#eff6f8;}
.dfar-guide .guide-note {background:#fff5df; padding:12px 16px; border-left:4px solid #b6862f; margin:14px 0;}
.dfar-guide code {white-space:normal; color:#294d60; background:#eef3f5;}
.dfar-guide .guide-caption {font-size:13px; color:#5c6c76;}
.dfar-context {color:#526875; border-left:3px solid #9dbec9; padding:8px 12px; margin:10px 0 18px;}
@media print {.dfar-guide details {break-inside:avoid;} .dfar-guide {max-width:none;}}
'))

dfar_guide_table <- function(headers, rows) {
 tags$div(class='guide-table-wrap', tags$table(
   tags$thead(tags$tr(lapply(headers,tags$th))),
   tags$tbody(lapply(rows,function(row) tags$tr(lapply(row,tags$td))))))
}

dfar_user_guide <- function() {
 tags$div(class='dfar-guide',
  h2('How to use DfaR'),
  p(class='guide-lead','From repeated tooth measurements to fluctuating asymmetry, tooth-size and linear enamel hypoplasia summaries. Follow the steps below, then open the sections you need.'),
  tags$div(class='guide-steps',
   h3('Start here',style='margin-top:0'),
   tags$ol(
    tags$li(tags$b('Prepare your workbook. '),'Use an .xlsx file with Tooth_Master and LEH_Bands sheets. Check column names, tooth codes and repeated measurements below.'),
    tags$li(tags$b('Upload and choose settings. '),'Select your file in the sidebar. Decide whether to include M3 and whether your LEH recording protocol supports treating missing entries as absence.'),
    tags$li(tags$b('Click Prepare data. '),'Wait for the success message. Run this step again after changing the file, outlier alpha, M3 option or LEH coding option.'),
    tags$li(tags$b('Check Summary and the control tables. '),'Review FA ANOVA, Measurement outliers and R-L outliers before interpreting group comparisons.'),
    tags$li(tags$b('Explore plots and tests. '),'Choose a trait, response and comparison variable. These controls update the displayed analysis without rerunning preparation.'),
    tags$li(tags$b('Download final Excel. '),'Keep the output together with the original workbook and the settings used. The input file on disk is not overwritten.')
   )),
  tags$details(open=NA,tags$summary('1. Prepare the Excel file'),
   p('Use one row per individual, tooth type, arch and side in Tooth_Master. Put repeat measurements in columns on the same row, not in duplicate rows. A bilateral FA observation needs two measurements on the right and two on the left for the same dimension.'),
   dfar_guide_table(c('Required column','What to enter'),list(
    c('ID','A stable individual identifier, repeated consistently for all teeth from that individual.'),
    c('Sex','Use M or F for definite assessments if you want the definite-sex filter to retain them. Other labels may be kept, but that filter excludes them.'),
    c('AgeClass; Funerary','Your comparison categories. These columns must exist even if some entries are missing. Keep metadata consistent across an individual’s rows; the app takes the first nonempty value.'),
    c('ToothType','C, M1, M2 and optionally M3 enter FA. Other tooth types do not enter the FA workflow.'),
    c('Arch','Exactly Upper or Lower for FA.'),
    c('Side','Exactly R or L for FA.'),
    c('WearStage','Your recorded wear stage; use numeric values for mean-wear comparisons. The column is required, but there is no general automatic wear exclusion.'),
    c('MD (1); MD (2)','First and second mesiodistal measurements, in the same units.'),
    c('BL (1); BL (2)','First and second buccolingual measurements, in the same units.')
   )),
   p('Use millimetres for the units stated in the app’s tooth-size output. Decimal points and decimal commas are accepted in measurement cells. Leave unmeasured dimensions blank; do not use zero as a missing-value code. Text that cannot be converted to a number becomes missing.'),
   p(tags$b('Optional LEH columns in Tooth_Master: '),tags$code('LEH_Present'),' (prefer explicit 1 = present, 0 = absent) and ',tags$code('LEH_Count'),' (numeric number of bands). Leave unknown or unobservable values blank and turn off the missing-as-absence option.'),
   p(tags$b('LEH_Bands is required as a sheet. '),'Its contents are preserved in the output; individual LEH results are calculated from Tooth_Master, not from the rows in LEH_Bands. If there are no band records, retain the sheet with column headers and no data rows.'),
   h4('Example of the measurement layout'),
   dfar_guide_table(c('ID','ToothType','Arch','Side','MD (1)','MD (2)','BL (1)','BL (2)'),list(
     c('Example_01','M1','Upper','R','10.20','10.22','11.05','11.02'),
     c('Example_01','M1','Upper','L','10.10','10.13','10.98','11.00'))),
   p(class='guide-caption','Illustrative rows only: add all required metadata columns. This is not a sufficient analysis sample. Trait ANOVA requires at least three clean bilateral pairs and estimable variation.')
  ),
  tags$details(tags$summary('2. Understand the preparation settings'),
   dfar_guide_table(c('Control','Effect'),list(
    c('Alpha for iterative outlier removal','Default 0.05. Controls the two iterative Grubbs outlier procedures, with Holm adjustment across trait candidates at each iteration. It does not change the fixed 0.05 thresholds used to label trait screening.'),
    c('Include M3 as an exploratory FA trait','Adds M3 to FA preparation and exploratory results. M3 is excluded from the primary-trait list and composite FA even if it passes basic screening. This checkbox does not filter tooth-size or LEH summaries.'),
    c('LEH: interpret blanks as absence','When enabled, the app finds tooth types with at least one LEH_Present value interpreted as positive anywhere in the workbook, then converts missing/unrecognized LEH_Present values for those tooth types to zero. The rule is by tooth type, not individual, arch or group. It does not fill LEH_Count.'),
    c('Prepare data','Rebuilds the results for the uploaded workbook and preparation settings. Changing settings alone does not refresh existing results or the Excel download.')
   )),
   p('Only enable the LEH option when the recording protocol justifies the assumption. Otherwise disable it and record explicit 0/1 values for assessed teeth. A blank is not automatically evidence of absence.')
  ),
  tags$details(tags$summary('3. Review screening and exclusions'),
   p('Start with Summary. Counts of excluded records and clean pairs are not counts of unique individuals: one individual can contribute several traits.'),
   tags$ul(
    tags$li(tags$b('Measurement outliers: '),'the app screens signed replicate differences, Rep2 − Rep1, within each arch × tooth type × dimension. An excluded record removes that side/dimension from FA and from the corresponding tooth-size dimension.'),
    tags$li(tags$b('R-L outliers: '),'after averaging repeats, the app screens signed right-minus-left differences. Exclusion removes that bilateral trait pair from FA; it does not remove the tooth from the separate size calculation.'),
    tags$li(tags$b('Read Excluded = Yes. '),'The audit tables also include tested candidates marked No. An outlier flag is a statistical exclusion under the current workflow, not confirmation of a recording mistake.'),
    tags$li(tags$b('FA ANOVA: '),'inspect sample size, the FA-versus-measurement-error test, the directional-asymmetry test and the signed R−L normality check. Basic screening requires p_FA_vs_ME < 0.05, no detected directional asymmetry, and no detected departure from normality under the app’s rules.'),
    tags$li(tags$b('Primary traits: '),'C, M1 and M2 traits passing basic screening enter the primary list and composite. The size-association result is reported but is not part of this pass/fail rule.')
   ),
   p('Trait codes combine U/L for arch, tooth type and MD/BL: for example UM1MD is upper first-molar mesiodistal diameter. A trait that passes screening is not automatically validated for every biological interpretation.'),
   p('The source workbook remains intact. The exported Tooth_Master is a processed copy with analytical flags. Identify records by ID, arch, tooth type and side; SourceRow references refer to the processed input table.')
  ),
  tags$details(tags$summary('4. Use the analysis tabs'),
   dfar_guide_table(c('Tab / analysis','How to use it'),list(
    c('FA: plots and tests','Choose a trait, Absolute FA or Relative FA, then a comparison variable. Keep the screening filter on for the primary view. Turn it off to inspect other available traits, including exploratory M3.'),
    c('Individual composite FA','Choose at least 2 traits (the default), all available traits, or complete cases. Compare N_FA_traits as well as the score; different individuals may contribute different sets of traits.'),
    c('Tooth size','Choose a tooth/arch and a size index. Replicates are averaged within side, indices calculated by side, then available sides averaged per individual. One usable side can contribute. Trait choices require a nonmissing CrownArea somewhere in that trait.'),
    c('LEH: plots and tests','Compare individual presence or the maximum band count. Presence means at least one assessed tooth is positive; maximum count is the largest recorded count on a tooth, not the sum across teeth.'),
    c('Definite M and F filter','Acts only when the selected comparison variable is Sex. It does not filter other comparisons by sex.'),
    c('Data used in the test','Use this table to check which records and sample sizes actually entered the displayed test. Records missing the selected response or comparison variable are omitted.')
   )),
   h4('What the main indices mean'),
   dfar_guide_table(c('Index','Definition'),list(
    c('FA_abs','Absolute difference between the mean right and mean left repeat measurements: |R − L|.'),
    c('FA_relative','FA_abs divided by |(R + L)/2|; a ratio, not a percentage and not a fitted allometric correction.'),
    c('Composite_FA','Mean of an individual’s valid FA_abs values after dividing each by the sample mean for its trait. A dimensionless, sample-dependent index; it is not directly comparable across separately normalized datasets.'),
    c('Composite_FA_ge2 / Composite_FA_complete','At least two contributing traits / all eligible traits in the current sample, respectively.'),
    c('CrownArea / CrownGM','MD × BL (mm²) / sqrt(MD × BL) (mm), calculated by side before averaging available sides.'),
    c('CrownModule / CrownIndex','(MD + BL)/2 (mm) / 100 × BL/MD. CrownIndex describes proportions rather than size alone.')
   ))
  ),
  tags$details(tags$summary('5. Read tests and download results'),
   p('Numeric responses use Spearman correlation only when the comparison variable is numeric and has more than five distinct values. Otherwise it is treated as categorical: two groups use Wilcoxon rank-sum; more than two use Kruskal–Wallis and pairwise Wilcoxon tests with Holm adjustment. Individual LEH presence uses Fisher’s exact test, with a simulated chi-square fallback when necessary.'),
   p('The composite and LEH-count plots display categories even when their numeric comparison variable triggers a Spearman test. Read the test label and the data table. A nonsignificant test does not establish equality; significance alone does not identify a biological cause. Pairwise correction does not cover every exploratory comparison made across tabs.'),
   p('Download final Excel exports the prepared datasets and screening results. The current interactive plot and its selected group-test output are not included in that workbook; record the trait, response, comparison variable, filters, sample size and test output separately when reporting them.'),
   dfar_guide_table(c('Output worksheet','Contents'),list(
    c('Individual_Summary','One row per individual with metadata, individual LEH and composite FA measures.'),
    c('FA_Pairs_Clean','Retained bilateral trait pairs, FA indices, screening labels and linked metadata.'),
    c('FA_ANOVA_Clean / FA_ANOVA_Clean_Detail','Trait screening summaries / sums of squares, mean squares and model details.'),
    c('Composite_FA_Individual','Composite scores and the number of contributing traits.'),
    c('Tooth_Size_Individual','Size indices and numbers of contributing sides by individual and tooth/arch.'),
    c('LEH_Individual','Individual LEH presence, maximum count and number of scored teeth.'),
    c('FA_ME_Outliers / FA_RL_Outliers','Iteration logs, candidate tests and exclusion flags.'),
    c('FA_Clean_Method','Preparation choices and method descriptions for the run.'),
    c('Tooth_Master / LEH_Bands','Processed master table / preserved band-record sheet.')
   ))
  ),
  tags$details(tags$summary('6. Troubleshooting'),
   dfar_guide_table(c('What you see','What to check'),list(
    c('Missing sheet or column message','Match the exact sheet names and required column headers, including spaces and parentheses in MD (1), BL (1), MD (2), BL (2).'),
    c('Duplicate-position message','Keep one row per ID × ToothType × Arch × Side. Store repeats in the numbered measurement columns.'),
    c('Empty trait list or no trait passes screening','Check tooth codes, both sides, both repeats, clean pair counts and FA ANOVA. For exploration, turn off the primary-trait filter. Do not change thresholds simply to obtain a result.'),
    c('Insufficient data or preparation error on a sparse file','At least three clean bilateral pairs are needed for trait ANOVA, with nonzero estimable variation. A file with no usable FA traits can fail preparation even if LEH or size records exist.'),
    c('Unexpectedly low LEH prevalence','Check the missing-as-absence option and Tooth_Master LEH fields. LEH_Bands alone does not populate individual LEH.'),
    c('Plot and Excel appear unchanged','Click Prepare data again after changing the upload or preparation settings. Plot selectors update only the interactive comparison, not the workbook contents.'),
    c('Cannot upload a large workbook','A default local Shiny installation may limit uploads to 5 MB unless configured otherwise. Remove unused workbook content or ask the app maintainer to configure an appropriate limit.')
   ))),
  p(class='guide-caption','Guide matched to the supplied DfaR English code. FA = fluctuating asymmetry; DA = directional asymmetry; ME = replicate measurement error; LEH = linear enamel hypoplasia. The app does not automatically apply general exclusions for wear, caries or damage.')
 )
}

dfar_write_guide <- function(file) {
 page <- tags$html(lang='en',tags$head(tags$meta(charset='utf-8'),
   tags$meta(name='viewport',content='width=device-width, initial-scale=1'),
   tags$title('DfaR - User guide'),dfar_help_css(),
   tags$style(HTML('body{font-family:Arial,sans-serif;margin:24px auto;padding:0 20px;max-width:1080px;}'))),
   tags$body(dfar_user_guide()))
 # Expand the downloaded guide for reading and printing; app sections stay collapsible.
 html <- as.character(page)
 html <- gsub('<details>','<details open>',html,fixed=TRUE)
 writeLines(enc2utf8(c('<!DOCTYPE html>',html)),file,useBytes=TRUE)
}

ui <- fluidPage(
  dfar_help_css(),

  tags$head(
    tags$title("DfaR")
  ),

  titlePanel(
    "DfaR"
  ),

  sidebarLayout(

    sidebarPanel(

      actionLink("open_help", "How to use DfaR"),
      br(), br(),
      downloadButton("download_guide", "Download user guide"),
      hr(),
      fileInput(
        "file",
        "1. Upload the original Excel file",
        accept = c(".xlsx")
      ),

      numericInput(
        "alpha",
        "Alpha for iterative outlier removal",
        value = 0.05,
        min = 0.001,
        max = 0.10,
        step = 0.01
      ),

      checkboxInput(
        "m3",
        "Include M3 as an exploratory FA trait",
        value = TRUE
      ),

      checkboxInput(
        "leh_blank_absence",
        "LEH: interpret blanks as absence for tooth types actually scored",
        value = TRUE
      ),

      helpText(
        "Enable only if your recording protocol supports missing-as-absence coding. See How to use for the exact tooth-type rule."
      ),

      actionButton(
        "run",
        "2. Prepare data",
        class = "btn-primary"
      ),

      br(),
      br(),

      downloadButton(
        "download",
        "3. Download final Excel"
      ),

      hr(),

      helpText(
        "The original file is never modified."
      )
    ),

    mainPanel(

      tabsetPanel(
        id = "main_tabs",
        tabPanel("How to use", value = "how_to_use", dfar_user_guide()),

        # -------------------------------------------------------------------
        # SUMMARY
        # -------------------------------------------------------------------
        tabPanel(
          "Summary",
          br(),
          uiOutput("summary_boxes"),
          br(),
          h4("FA traits recommended for primary analyses"),
          verbatimTextOutput("usable_traits"),
          hr(),
          h4("Where to find the analysis datasets"),
          tags$ul(
            tags$li(
              tags$b("FA: "),
              "FA_Pairs_Clean; primary response = FA_abs"
            ),
            tags$li(
              tags$b("Tooth size: "),
              "Tooth_Size_Individual; primary response = CrownArea"
            ),
            tags$li(
              tags$b("Individual LEH: "),
              "LEH_Individual"
            )
          )
        ),

        # -------------------------------------------------------------------
        # FA ANALYSIS
        # -------------------------------------------------------------------
        tabPanel(
          "FA: plots and tests",
          br(),
          p(class = "dfar-context", "Choose a screened trait, response and comparison variable. Check the test label and sample in Data used in the test. Composite FA is below the trait analysis."),

          fluidRow(
            column(
              4,
              checkboxInput(
                "fa_primary",
                "Use only traits that pass FA screening",
                value = TRUE
              )
            ),
            column(
              4,
              selectInput(
                "fa_trait",
                "Trait",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "fa_response",
                "FA index",
                choices = c(
                  "Absolute FA |R-L|" = "FA_abs",
                  "Relative FA" = "FA_relative"
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
                "Variable to compare with FA",
                choices = c(
                  "Sex" = "Sex",
                  "Age class" = "AgeClass",
                  "Funerary variable" = "Funerary",
                  "Individual LEH" = "Individual_LEH",
                  "LEH of the same tooth type" = "LEH_same_tooth",
                  "Mean size of the same trait" = "Mean_size",
                  "Mean wear of the antimeric pair" = "Wear_mean_pair"
                )
              )
            ),
            column(
              6,
              checkboxInput(
                "fa_certain_sex",
                "If X = sex, use only definite M and F",
                value = TRUE
              )
            )
          ),

          plotOutput(
            "fa_plot",
            height = "460px"
          ),

          h4("Statistical test"),
          verbatimTextOutput(
            "fa_test"
          ),

          h4("Data used in the test"),
          DTOutput(
            "fa_analysis_table"
          ),

          hr(),
          h3("Individual composite FA"),

          p(
            "Each FA_abs value is standardized by the mean of its own trait; the individual score is the mean of the available valid FA traits."
          ),

          fluidRow(
            column(
              4,
              selectInput(
                "cfa_response",
                "Composite score",
                choices = c(
                  "Composite FA - at least 2 traits" = "Composite_FA_ge2",
                  "Composite FA - all available traits" = "Composite_FA",
                  "Composite FA - complete cases" = "Composite_FA_complete"
                ),
                selected = "Composite_FA_ge2"
              )
            ),
            column(
              4,
              selectInput(
                "cfa_predictor",
                "Comparison variable",
                choices = c(
                  "Sex" = "Sex",
                  "Age class" = "AgeClass",
                  "Funerary variable" = "Funerary",
                  "Individual LEH" = "Individual_LEH",
                  "Maximum number of LEH bands" = "Individual_LEH_CountMax"
                )
              )
            ),
            column(
              4,
              checkboxInput(
                "cfa_certain_sex",
                "If X = sex, use only definite M and F",
                value = TRUE
              )
            )
          ),

          plotOutput(
            "cfa_plot",
            height = "430px"
          ),

          h4("Statistical test for composite FA"),
          verbatimTextOutput(
            "cfa_test"
          ),

          h4("Individual composite FA data"),
          DTOutput(
            "cfa_table"
          )
        ),

        # -------------------------------------------------------------------
        # DIMENSIONE DENTARIA
        # -------------------------------------------------------------------
        tabPanel(
          "Tooth size",
          br(),
          p(class = "dfar-context", "Indices are calculated by side, then averaged across available sides for each individual and tooth type. CrownIndex describes proportions. Check N_sides_size in the data table."),

          p(
            tags$b("Primary measure: "),
            "CrownArea = MD × BL (mm²). CrownGM = √(MD × BL) is also available and retains linear units (mm). Right and left sides are first calculated separately and then averaged, so each individual contributes only one observation per tooth type."
          ),

          fluidRow(
            column(
              4,
              selectInput(
                "size_trait",
                "Tooth type / arch",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "size_response",
                "Size variable",
                choices = c(
                  "Crown area = MD × BL (mm²)" = "CrownArea",
                  "Geometric mean = √(MD × BL) (mm)" = "CrownGM",
                  "Crown module = (MD + BL)/2 (mm)" = "CrownModule",
                  "Mesiodistal diameter (mm)" = "MD",
                  "Buccolingual diameter (mm)" = "BL",
                  "Crown index = 100 × BL/MD (shape)" = "CrownIndex"
                ),
                selected = "CrownArea"
              )
            ),
            column(
              4,
              selectInput(
                "size_predictor",
                "Comparison variable",
                choices = c(
                  "Sex" = "Sex",
                  "Age class" = "AgeClass",
                  "Funerary variable" = "Funerary",
                  "Individual LEH" = "Individual_LEH",
                  "LEH of the same tooth type" = "LEH_same_tooth",
                  "Mean wear" = "Wear_mean"
                )
              )
            )
          ),

          checkboxInput(
            "size_certain_sex",
            "If X = sex, use only definite M and F",
            value = TRUE
          ),

          conditionalPanel(
            condition = "input.size_predictor == 'AgeClass'",
            div(
              style = "padding:10px; background:#fff3cd; border:1px solid #ffe69c; margin-bottom:15px;",
              tags$b("Methodological note: "),
              "biological crown size does not change after formation, but MD in particular can decrease through interproximal wear. Any association with AgeClass should therefore be interpreted together with Wear_mean."
            )
          ),

          plotOutput(
            "size_plot",
            height = "460px"
          ),

          h4("Statistical test"),
          verbatimTextOutput(
            "size_test"
          ),

          h4("Data used in the test"),
          DTOutput(
            "size_analysis_table"
          )
        ),

        # -------------------------------------------------------------------
        # LEH
        # -------------------------------------------------------------------
        tabPanel(
          "LEH: plots and tests",
          br(),
          p(class = "dfar-context", "Results use LEH_Present and LEH_Count in Tooth_Master. LEH_Bands is preserved but does not supply these summaries. Maximum count is not a sum across teeth."),

          fluidRow(
            column(
              6,
              selectInput(
                "leh_response",
                "LEH variable",
                choices = c(
                  "Individual presence/absence" = "Individual_LEH",
                  "Maximum number of recorded bands" = "Individual_LEH_CountMax"
                ),
                selected = "Individual_LEH"
              )
            ),
            column(
              6,
              selectInput(
                "leh_predictor",
                "Comparison variable",
                choices = c(
                  "Sex" = "Sex",
                  "Age class" = "AgeClass",
                  "Funerary variable" = "Funerary"
                )
              )
            )
          ),

          checkboxInput(
            "leh_certain_sex",
            "If X = sex, use only definite M and F",
            value = TRUE
          ),

          plotOutput(
            "leh_plot",
            height = "460px"
          ),

          h4("Statistical test"),
          verbatimTextOutput(
            "leh_test"
          ),

          h4("Data used in the test"),
          DTOutput(
            "leh_analysis_table"
          )
        ),

        # -------------------------------------------------------------------
        # CONTROL TABLES
        # -------------------------------------------------------------------
        tabPanel(
          "FA ANOVA",
          br(),
          DTOutput("anova_table")
        ),

        tabPanel(
          "Individual FA",
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
          "Measurement outliers",
          br(),
          DTOutput("me_table")
        ),

        tabPanel(
          "R-L outliers",
          br(),
          DTOutput("rl_table")
        ),

        tabPanel(
          "Methods",
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

# Consistent age ordering across every categorical plot; other variables retain their order.
dfar_plot_factor <- function(value, variable) {
  if (!identical(variable, "AgeClass")) return(factor(value))
  preferred <- c("YA", "MA", "OA")
  additional <- sort(setdiff(unique(as.character(value[!is.na(value)])), preferred))
  factor(as.character(value), levels = c(preferred, additional))
}

server <- function(input, output, session) {
  observeEvent(input$open_help, {
    updateTabsetPanel(session, "main_tabs", selected = "how_to_use")
  })
  output$download_guide <- downloadHandler(
    filename = function() "DfaR_user_guide.html",
    content = function(file) dfar_write_guide(file),
    contentType = "text/html; charset=utf-8"
  )


  risultati <- reactiveVal(NULL)

  observeEvent(
    input$run,
    {

      req(input$file)

      withProgress(
        message = "Preparing and analyzing data...",
        value = 0,
        {

          incProgress(
            0.15,
            detail = "Reading and checking the file"
          )

          risultato <- tryCatch(
            {

              incProgress(
                0.30,
                detail = "FA and outlier screening"
              )

              x <- esegui_pipeline_FA(
                file_input = input$file$datapath,
                alpha = input$alpha,
                includi_M3 = input$m3,
                leh_blank_absence = input$leh_blank_absence
              )

              incProgress(
                0.45,
                detail = "Preparing biological datasets"
              )

              x
            },
            error = function(e) {

              showModal(
                modalDialog(
                  title = "Error",
                  paste(
                    "The analysis could not be completed:",
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
        updateTabsetPanel(session, "main_tabs", selected = "Summary")
        showNotification(
          "Data prepared successfully.",
          type = "message",
          duration = 5
        )
      }
    }
  )


  # -------------------------------------------------------------------------
  # UPDATE TRAIT CHOICES
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
  # SUMMARY
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
          "measurement-error outliers excluded"
        )
      ),

      column(
        4,
        wellPanel(
          h3(
            x$RL_n,
            style = "margin-top:0"
          ),
          "R-L outliers excluded"
        )
      ),

      column(
        4,
        wellPanel(
          h3(
            x$Pair_n,
            style = "margin-top:0"
          ),
          "clean FA pairs"
        )
      )
    )
  })


  output$usable_traits <- renderText({

    x <- risultati()
    req(x)

    if (length(x$usable_traits) == 0) {
      return(
        "No trait passes all screening criteria."
      )
    }

    paste(
      x$usable_traits,
      collapse = "\n"
    )
  })


  # -------------------------------------------------------------------------
  # FA ANALYSIS
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
        "Insufficient data for the plot."
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
          x = dfar_plot_factor(.data[[xvar]], xvar),
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
  # INDIVIDUAL COMPOSITE FA
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
        "Insufficient data for the plot."
      )
    )

    y <- input$cfa_response
    xvar <- input$cfa_predictor

    ggplot(
      d,
      aes(
        x = dfar_plot_factor(.data[[xvar]], xvar),
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
  # TOOTH-SIZE ANALYSIS
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
        "Insufficient data for the plot."
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
          x = dfar_plot_factor(.data[[xvar]], xvar),
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
  # LEH ANALYSIS
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
        "Insufficient data for the plot."
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
          x = dfar_plot_factor(.data[[xvar]], xvar),
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
          y = "Proportion",
          fill = "LEH",
          title = paste(
            "Individual LEH by",
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
          x = dfar_plot_factor(.data[[xvar]], xvar),
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
          y = "Maximum number of LEH bands",
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
  # CONTROL TABLES
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
  # EXCEL DOWNLOAD
  # -------------------------------------------------------------------------

  output$download <- downloadHandler(

    filename = function() {
      "DfaR_results.xlsx"
    },

    content = function(file) {

      x <- risultati()

      if (is.null(x)) {
        stop(
          "Prepare the data first."
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
# 6. START THE APP
# ---------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
