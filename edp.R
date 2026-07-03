## This code generate the EDP stats 
## It cannot be run without access to the EDP data, which is confidential and cannot be shared
## It must be accessed through the casd

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(ggplot2)

folder <- "\\\\casd.fr\\casdfs\\Projets\\PRESFAM\\Data\\EDP_EDP_2022"


con <- dbConnect(duckdb::duckdb(), dbdir = "cache/rct")

dbExecute(con, paste0("SET threads = ", max(1L, parallel::detectCores(logical = FALSE) - 1L), ";"))
dbExecute(con, "SET memory_limit = '12GB';")


# 1. Importing data -----

# Fiscal REVENU - household level
dbExecute(con, paste0("
  CREATE OR REPLACE TABLE rev AS
  SELECT
    -- Clés
    ID_FISC_LOG_DIFF,
    CAST(AN_FISC   AS INTEGER) AS AN_FISC,
    CAST(AN_REVENU AS INTEGER) AS AN_REVENU,
    -- Revenus et éligibilité
    REVDECM, REVDISPM,
    -- Niveau de vie
    NB_UC, NIVVIEM, CENTILE, I_PAUVRE50M, I_PAUVRE60M,
    -- Type de ménage
    TU10, OCCTYP, TYPMEN9, TYPMENR,
    -- Géographie
    REG, DEP, I_CHAMPM, DEPCOM
  FROM read_parquet('", folder, "\\EDP_BE2022_FISC_REVENU_*.PARQUET',
    union_by_name = true)
  QUALIFY ROW_NUMBER() OVER (PARTITION BY ID_FISC_LOG_DIFF, AN_FISC) = 1
"))

# Fiscal INDIVIDU: needed to identify births and mothers
dbExecute(con, paste0("
                      CREATE OR REPLACE TABLE ind AS
                      SELECT
                      ID_DIFF,
                      ID_FISC_LOG_DIFF,
                      ID_FISC_FOY_DIFF,
                      CAST(AN_FISC AS INTEGER) AS AN_FISC,
                      CAST(ANAIS AS INTEGER) AS ANAIS,
                      ORDREFIP,
                      TYPE_FISC,
                      TYPE_PRES,
                      TYPE_DECL,
                      SEXE,
                      CODNAIS,
                      LIEN_FAMILIAL
                        FROM read_parquet('", folder, "\\EDP_BE2022_FISC_INDIVIDU_*.PARQUET',
    union_by_name = true)
"))


# Fisc REVDET
dbExecute(con, paste0("
  CREATE OR REPLACE TABLE revdet AS
  SELECT
    ID_FISC_FOY_DIFF,
    CAST(AN_FISC AS INTEGER) AS AN_FISC,
    ORDREFIP, TYPE_FISC,
    REV_PRINC,
    YSALI, YCHOI, YRSTI,
    YBICI, YBNCI,
    YRAGI, YALRI
  FROM read_parquet('", folder, "\\EDP_BE2022_FISC_REVDET_*.PARQUET',
    union_by_name = true)
"))

# EAR
dbExecute(con, paste0("
  CREATE OR REPLACE TABLE ear_ind AS
  SELECT
    ID_DIFF,
    CAST(ANNEE_COL AS INTEGER) AS ANNEE_COL,
    DIPL
  FROM read_parquet('", folder, "\\EDP_BE2022_EAR*_INDIVIDU.PARQUET',
    union_by_name = true)
"))




# 2. Identify birth households -----


# Households with a birth registered in the fiscal declaration in years 2019, 2020 and 2021
dbExecute(con, "
         CREATE TABLE birth_hh AS
         SELECT
         r.ID_FISC_LOG_DIFF,
         r.AN_FISC,
         r.DEP,
         CASE
         WHEN r.DEP = '75' THEN 'Paris'
         WHEN r.DEP = '93' THEN 'Seine-Saint-Denis'
         WHEN r.DEP = '94' THEN 'Val-de-Marne'
         ELSE 'Reste de France'
         END AS dep_group
         FROM rev r
         INNER JOIN ind i
         ON r.ID_FISC_LOG_DIFF = i.ID_FISC_LOG_DIFF
         AND r.AN_FISC = i.AN_FISC
         WHERE r.AN_FISC IN (2020, 2021, 2022)
         AND i.ANAIS = r.AN_FISC - 1
         AND i.TYPE_FISC NOT IN ('01', '1', '02', '2')
         AND i.TYPE_PRES != 9
         AND r.DEP NOT IN ('971', '972', '973', '974', '976')
         ")

# Check: distribution by department group
dbGetQuery(con, "
           SELECT dep_group, COUNT(*) AS n_households
           FROM birth_hh
           GROUP BY dep_group
           ORDER BY dep_group
           ")


# 3. Identify the mother in each birth household -----

# mother is a female adult (SEXE = 2) who is declarant or conjoint
# TYPE_FISC (1, 01, 2, 02)

dbExecute(con, "
          CREATE TABLE mother AS
          SELECT
          b.ID_FISC_LOG_DIFF,
          b.AN_FISC,
          b.dep_group,
          i.ID_DIFF AS ID_DIFF_mere,
          i.ID_FISC_FOY_DIFF,
          i.ORDREFIP,
          i.TYPE_FISC,
          i.CODNAIS,
          (b.AN_FISC - 1) - i.ANAIS AS age_at_birth
          FROM birth_hh b
          INNER JOIN ind i
          ON b.ID_FISC_LOG_DIFF = i.ID_FISC_LOG_DIFF
          AND b.AN_FISC = i.AN_FISC
          WHERE i.SEXE = '2'
          AND i.TYPE_FISC IN ('01', '1', '02', '2')
          AND i.TYPE_PRES != 9
          QUALIFY ROW_NUMBER() OVER (PARTITION BY b.ID_FISC_LOG_DIFF ORDER BY i.ORDREFIP) = 1
          ")


dbGetQuery(con, "
           SELECT
           b.dep_group,
           COUNT(b.ID_FISC_LOG_DIFF) AS n_birth_hh,
           COUNT(m.ID_FISC_LOG_DIFF) AS n_with_mother,
           ROUND(100.0 * COUNT(m.ID_FISC_LOG_DIFF) / COUNT(b.ID_FISC_LOG_DIFF), 1) AS pct_matched
           FROM birth_hh b
           LEFT JOIN mother m USING (ID_FISC_LOG_DIFF, AN_FISC)
           GROUP BY b.dep_group
           ORDER BY b.dep_group
           ")



# 4. Employment from revdet -----

# attach employment to mother table
dbExecute(con, "
          CREATE TABLE mother_emp AS
          SELECT
          m.*,
          CASE
          WHEN NULLIF(TRIM(d.REV_PRINC), '.') IS NULL THEN NULL
          WHEN TRIM(d.REV_PRINC) IN ('1', '5', '6', '7') THEN 1
          ELSE 0
          END AS employed
          FROM mother m
          LEFT JOIN revdet d
          ON m.ID_FISC_FOY_DIFF = d.ID_FISC_FOY_DIFF
          AND m.AN_FISC = d.AN_FISC
          AND m.ORDREFIP = d.ORDREFIP
          AND m.TYPE_FISC = d.TYPE_FISC
          ")


# Check employment coverage rates by dep_group
dbGetQuery(con, "
           SELECT
           dep_group,
           COUNT(*) AS n,
           SUM(CASE WHEN employed IS NULL THEN 1 ELSE 0 END) AS n_missing_emp,
           ROUND(100.0 * AVG(employed), 1) AS emp_rate_pct
           FROM mother_emp
           GROUP BY dep_group
           ORDER BY dep_group
           ")


# 5. Education from EAR -----

# EAR is only available for EDP tracked individuals (non NULL ID_DIFF)
# we take the most recent non null DIPL observation


# Most recent DIPL per individual (forward fill: freshest observation wins)
dbExecute(con, "
          CREATE TABLE ear_dipl AS
          SELECT ID_DIFF, ANNEE_COL, DIPL
          FROM ear_ind
          QUALIFY ROW_NUMBER() OVER (PARTITION BY ID_DIFF ORDER BY ANNEE_COL DESC) = 1
          ")

# Recode DIPL into a dummy 1 ! strictly above bac, 0 = bac or below
dbExecute(con, "
          CREATE OR REPLACE TABLE ear_dipl_coded AS
          SELECT
          ID_DIFF,
          CASE
          WHEN ANNEE_COL < 2016 THEN
          CASE
          WHEN TRIM(DIPL) IN ('01', '02', '03', '11', '12', '13', '14', '15', '16') THEN 0
          WHEN TRIM(DIPL) IN ('17', '18') THEN 1
          ELSE NULL
          END
          ELSE
          CASE
          WHEN TRIM(DIPL) IN ('01', '02', '03', '11', '12', '13', '14', '15') THEN 0
          WHEN TRIM(DIPL) IN ('16', '17', '18', '19') THEN 1
          ELSE NULL
          END
          END AS sup_bac
          FROM ear_dipl
          ")



# 6. Final mother table: income + migration background + education -----

dbExecute(con, "
          CREATE TABLE mother_final AS
          SELECT
          m.ID_FISC_LOG_DIFF,
          m.AN_FISC - 1 AS birth_year,
          m.dep_group,
          m.age_at_birth,
          m.employed,
          CASE
          WHEN m.CODNAIS IS NULL OR m.CODNAIS = '00000' THEN NULL
          WHEN SUBSTR(m.CODNAIS, 1, 2) = '99' THEN 1
          ELSE 0
          END AS born_abroad,
          r.REVDECM,
          e.sup_bac
          FROM mother_emp m
          LEFT JOIN rev r
          ON m.ID_FISC_LOG_DIFF = r.ID_FISC_LOG_DIFF
          AND m.AN_FISC = r.AN_FISC
          LEFT JOIN ear_dipl_coded e
          ON m.ID_DIFF_mere = e.ID_DIFF
          ")


# 7. Descriptive statistics table -----


dbGetQuery(con, "
           SELECT
           birth_year,
           group_label,
           COUNT(*) AS n,
           ROUND(AVG(age_at_birth), 1) AS mean_age,
           ROUND(100.0 * AVG(employed), 1) AS emp_rate,
           ROUND(100.0 * AVG(born_abroad), 1) AS pct_born_abroad,
           ROUND(AVG(REVDECM), 1) AS mean_hh_inc,
           COUNT(sup_bac) AS n_with_dipl,
           ROUND(100.0 * AVG(sup_bac), 1) AS pct_sup_bac
           FROM (
           SELECT birth_year, 'La France' AS group_label, age_at_birth, employed, born_abroad, REVDECM, sup_bac
           FROM mother_final
           UNION ALL
           SELECT birth_year, '3 dep. IDF' AS group_label, age_at_birth, employed, born_abroad, REVDECM, sup_bac
           FROM mother_final
           WHERE dep_group IN ('Paris', 'Seine-Saint-Denis', 'Val-de-Marne')
           UNION ALL
           SELECT birth_year, dep_group AS group_label, age_at_birth, employed, born_abroad, REVDECM, sup_bac
           FROM mother_final
           WHERE dep_group IN ('Paris', 'Seine-Saint-Denis', 'Val-de-Marne')
           ) t
           GROUP BY birth_year, group_label
           ORDER BY birth_year,
           CASE group_label
           WHEN 'La France' THEN 1
           WHEN '3 dep. IDF' THEN 2
           WHEN 'Paris' THEN 3
           WHEN 'Val-de-Marne' THEN 4
           WHEN 'Seine-Saint-Denis' THEN 5
           END 
           ")


dbGetQuery(con, paste0("
                       WITH base AS (
                       SELECT
                       SUBSTR(NAI_DATE, 1, 4) AS birth_year,
                       CASE
                       WHEN SUBSTR(NAI_LIEU, 1, 2) = '75' THEN 'Paris'
                       WHEN SUBSTR(NAI_LIEU, 1, 2) = '93' THEN 'Seine-Saint-Denis'
                       WHEN SUBSTR(NAI_LIEU, 1, 2) = '94' THEN 'Val-de-Marne'
                       END AS dep_group,
                       CAST(SUBSTR(NAI_DATE, 1, 4) AS INTEGER)
                       - CAST(NULLIF(SUBSTR(MERE_IND_NAIS_DATE, 1, 4), '0000') AS INTEGER) AS age_mere,
                       CASE
                       WHEN TRIM(MERE_LIEU_NAIS_INDIC) = 'E' THEN 1
                       WHEN TRIM(MERE_LIEU_NAIS_INDIC) = 'F' THEN 0
                       ELSE NULL
                       END AS mere_born_abroad
                       FROM read_parquet('", folder, "\\EDP_BE2022_INDIVIDU.PARQUET')
                       WHERE SUBSTR(NAI_DATE, 1, 4) IN ('2019', '2020', '2021', '2022')
                       AND SUBSTR(NAI_LIEU, 1, 2) IN ('75', '93', '94')
                       )
                       
                       SELECT
                       birth_year,
                       dep_group,
                       COUNT(*) AS n,
                       ROUND(AVG(age_mere), 1) AS mean_age_mere,
                       ROUND(100.0 * AVG(mere_born_abroad), 1) AS pct_mere_born_abroad
                       FROM base
                       GROUP BY birth_year, dep_group
                       ORDER BY birth_year, dep_group
                       "))


# ALTERNATIVE APPROACH ----


# 1. EDP birth records -----

# NAI_LIEU = department of birth of the EDP child
# MERE_IND_NAIS_DATE = year of the mothers's EDP baby
# MERE_LIEU_NAIS_INDIC = F = born in France, E = born abroad

dbExecute(con, paste0("
                      CREATE OR REPLACE TABLE edp_babies AS
                      SELECT
                      ID_DIFF,
                      SUBSTR(NAI_DATE, 1, 4) AS birth_year,
                      CAST(SUBSTR(NAI_DATE, 1, 4) AS INTEGER) + 1 AS an_fisc_target,
                      CASE
                       WHEN SUBSTR(NAI_LIEU, 1, 2) = '75' THEN 'Paris'
                       WHEN SUBSTR(NAI_LIEU, 1, 2) = '93' THEN 'Seine-Saint-Denis'
                       WHEN SUBSTR(NAI_LIEU, 1, 2) = '94' THEN 'Val-de-Marne'
                       END AS dep_group,
                       CAST(NULLIF(SUBSTR(MERE_IND_NAIS_DATE, 1, 4), '0000') AS INTEGER) AS mere_birth_year,
                       CAST(SUBSTR(NAI_DATE, 1, 4) AS INTEGER)
                       - CAST(NULLIF(SUBSTR(MERE_IND_NAIS_DATE, 1, 4), '0000') AS INTEGER) AS age_mere,
                       CASE
                       WHEN TRIM(MERE_LIEU_NAIS_INDIC) = 'E' THEN 1 
                       WHEN TRIM(MERE_LIEU_NAIS_INDIC) = 'F' THEN 0
                       ELSE NULL
                       END AS born_abroad
                       FROM read_parquet('", folder, "\\EDP_BE2022_INDIVIDU.PARQUET')
                      WHERE SUBSTR(NAI_DATE, 1, 4) IN ('2019', '2020', '2021', '2022')
                      AND SUBSTR(NAI_LIEU, 1, 2) != '99'
                      AND SUBSTR(NAI_LIEU, 1, 3) NOT IN ('971', '972', '973', '976', '974')
                      "))

dbGetQuery(con, "
           SELECT
           birth_year,
           COUNT(*) AS n_edp_babies,
           COUNT(i.ID_FISC_LOG_DIFF) AS n_in_fiscal_record,
           COUNT(CASE WHEN r.TYPMENR != '6' THEN 1 END) AS n_non_complexe
           FROM edp_babies b
           LEFT JOIN ind i
           ON b.ID_DIFF = i.ID_DIFF
           AND i.AN_FISC = b.an_fisc_target
           LEFT JOIN rev r
           ON i.ID_FISC_LOG_DIFF = r.ID_FISC_LOG_DIFF
           AND i.AN_FISC = r.AN_FISC
           GROUP BY birth_year
           ORDER BY birth_year
           ")


# 2. Attach employment of the mother ------

## Step 1: Link each EDP baby to its fiscal household ----
dbExecute(con, "
          CREATE OR REPLACE TABLE edp_baby_hh AS
          SELECT
          b.ID_DIFF AS ID_DIFF_baby,
          b.birth_year,
          b.an_fisc_target,
          b.dep_group,
          b.mere_birth_year,
          i.ID_FISC_LOG_DIFF,
          r.TYPMENR
          FROM edp_babies b
          INNER JOIN ind i
          ON b.ID_DIFF = i.ID_DIFF
          AND i.AN_FISC = b.an_fisc_target
          INNER JOIN rev r
          ON i.ID_FISC_LOG_DIFF = r.ID_FISC_LOG_DIFF
          AND i.AN_FISC = r.AN_FISC
          ")


## Step B: Mother identification -----

# Non complex = women (SEXE = 2) with TYPE_FISC declarant / coinjoint
# complex = woman (SEXe = 2) whose ANAIS matches the mother's birth year from the EDP baby birth record

dbExecute(con, "
          CREATE OR REPLACE TABLE edp_mother AS
          
          -- Track 1: non complex households
          SELECT
          h.ID_DIFF_baby,
          h.birth_year,
          h.an_fisc_target,
          h.dep_group,
          h.ID_FISC_LOG_DIFF,
          i.ID_DIFF AS ID_DIFF_mere,
          i.ID_FISC_FOY_DIFF,
          i.ORDREFIP,
          i.TYPE_FISC,
          'non_complex' AS source
          FROM edp_baby_hh h
          INNER JOIN ind i
          ON h.ID_FISC_LOG_DIFF = i.ID_FISC_LOG_DIFF
          AND i.AN_FISC = h.an_fisc_target
          WHERE TYPMENR != '6'
          AND i.SEXE = '2'
          AND TYPE_FISC IN ('01', '1', '02', '2') 
          AND i.TYPE_PRES != 9
          QUALIFY ROW_NUMBER() OVER (PARTITION BY h.ID_DIFF_baby ORDER BY i.ORDREFIP) = 1
          
          UNION ALL
          
          --Track 2: complex households -> match on mother's birth year from birth record
          SELECT
          h.ID_DIFF_baby,
          h.birth_year,
          h.an_fisc_target,
          h.dep_group,
          h.ID_FISC_LOG_DIFF,
          i.ID_DIFF AS ID_DIFF_mere,
          i.ID_FISC_FOY_DIFF,
          i.ORDREFIP,
          i.TYPE_FISC,
          'complex_matched' AS source,
          FROM edp_baby_hh h
          INNER JOIN ind i
          ON h.ID_FISC_LOG_DIFF = i.ID_FISC_LOG_DIFF
          AND i.AN_FISC = h.an_fisc_target
          WHERE h.TYPMENR = '6'
          AND i.SEXE = '2'
          AND i.ANAIS = h.mere_birth_year
          AND i.TYPE_PRES != 9
          QUALIFY ROW_NUMBER() OVER (PARTITION BY h.ID_DIFF_baby ORDER BY i.ORDREFIP) = 1
          ")

dbGetQuery(con, "
           SELECT source, COUNT(*) AS n
           FROM edp_mother
           GROUP BY source
           ")


## Step C: Attach employment and household income ----
dbExecute(con, "
          CREATE OR REPLACE TABLE edp_final AS
          SELECT
          b.birth_year,
          b.dep_group,
          b.age_mere,
          b.born_abroad,
          m.ID_FISC_LOG_DIFF,
          m.ID_DIFF_mere,
          CASE 
          WHEN NULLIF(TRIM(d.REV_PRINC), '.') IS NULL THEN NULL
          WHEN TRIM(d.REV_PRINC) IN ('1', '5', '6', '7') THEN 1
          ELSE 0
          END AS employed,
          r.REVDECM,
          e.sup_bac
          FROM edp_babies b
          LEFT JOIN edp_mother m ON b.ID_DIFF = m.ID_DIFF_baby
          LEFT JOIN revdet d
          ON m.ID_FISC_FOY_DIFF = d.ID_FISC_FOY_DIFF
          AND b.an_fisc_target = d.AN_FISC
          AND m.ORDREFIP = d.ORDREFIP
          AND m.TYPE_FISC = d.TYPE_FISC
          LEFT JOIN rev r
          ON m.ID_FISC_LOG_DIFF = r.ID_FISC_LOG_DIFF
          AND b.an_fisc_target = r.AN_FISC
          LEFT JOIN ear_dipl_coded e
          ON m.ID_DIFF_mere = e.ID_DIFF
          ")


# 3. Summary table -----
dbGetQuery(con, "
           SELECT
           birth_year,
           group_label,
           COUNT(*) AS n,
           COUNT(ID_FISC_LOG_DIFF) AS n_with_fiscal_record,
           ROUND(AVG(age_mere), 1) AS mother_age,
           ROUND(100.0 * AVG(born_abroad), 1) AS mother_born_abroad,
           ROUND(100.0 * AVG(employed), 1) AS mother_employment_rate,
           ROUND(AVG(REVDECM), 1) AS hh_income,
           COUNT(sup_bac) AS n_with_dipl,
           ROUND(100.0 * AVG(sup_bac), 1) AS pct_sup_bac
           FROM (
           SELECT birth_year, 'La France' AS group_label, ID_FISC_LOG_DIFF, age_mere, employed, born_abroad, REVDECM, sup_bac
           FROM edp_final
           UNION ALL
           SELECT birth_year, '3 dep. IDF' AS group_label, ID_FISC_LOG_DIFF, age_mere, employed, born_abroad, REVDECM, sup_bac
           FROM edp_final
           WHERE dep_group IN ('Paris', 'Seine-Saint-Denis', 'Val-de-Marne')
           UNION ALL
           SELECT birth_year, dep_group AS group_label, ID_FISC_LOG_DIFF, age_mere, employed, born_abroad, REVDECM, sup_bac
           FROM edp_final
           WHERE dep_group IN ('Paris', 'Seine-Saint-Denis', 'Val-de-Marne')
           ) t
           GROUP BY birth_year, group_label
           ORDER BY birth_year,
           CASE group_label
           WHEN 'La France' THEN 1
           WHEN '3 dep. IDF' THEN 2
           WHEN 'Paris' THEN 3
           WHEN 'Val-de-Marne' THEN 4
           WHEN 'Seine-Saint-Denis' THEN 5
           END 
           ")


# 4. Other investigations ----

# so the thing is to investigate why the fiscal sample has on average older mothers at year of birth than the
# one based on the table individu, restricted to EDP babies

# Fiscal source
dbGetQuery(con, "
           SELECT
           age_cat AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct
           FROM (
           SELECT
           CASE
           WHEN age_at_birth < 25 THEN '< 25'
           WHEN age_at_birth <= 29 THEN '25-29'
           WHEN age_at_birth <= 34 THEN '30-34'
           WHEN age_at_birth <= 39 THEN '35-39'
           WHEN age_at_birth >= 40 THEN '40+'
           END AS age_cat
           FROM mother_final
           WHERE age_at_birth IS NOT NULL
           )
           GROUP BY age_cat
           ORDER BY MIN(CASE age_cat
           WHEN '< 25' THEN 1
           WHEN '25-29' THEN 2
           WHEN '30-34' THEN 3
           WHEN '35-39' THEN 4
           WHEN '40+' THEN 5
           END)
           ")

# EDP source (table individu)
dbGetQuery(con, "
           SELECT
           age_cat AS n,
           ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct
           FROM (
           SELECT
           CASE
           WHEN age_mere < 25 THEN '< 25'
           WHEN age_mere <= 29 THEN '25-29'
           WHEN age_mere <= 34 THEN '30-34'
           WHEN age_mere <= 39 THEN '35-39'
           WHEN age_mere >= 40 THEN '40+'
           END AS age_cat
           FROM edp_final
           WHERE age_mere IS NOT NULL
           )
           GROUP BY age_cat
           ORDER BY MIN(CASE age_cat
           WHEN '< 25' THEN 1
           WHEN '25-29' THEN 2
           WHEN '30-34' THEN 3
           WHEN '35-39' THEN 4
           WHEN '40+' THEN 5
           END)
           ")

# On fait un plot pour voir la distribution d'age des meres sur les deux bases
age_fiscal <- dbGetQuery(con, "
                         SELECT age_at_birth AS age, 'Fiscal' AS source
                         FROm mother_final 
                         WHERE age_at_birth BETWEEN 15 AND 55
                         ")

age_edp <- dbGetQuery(con, "
                         SELECT age_mere AS age, 'EDP individu' AS source
                         FROm edp_final 
                         WHERE age_mere BETWEEN 15 AND 55
                         ")

age_combined <- rbind(age_fiscal, age_edp)

ggplot(age_combined, aes(x = age, colour = source, fill = source)) +
         geom_density(alpha = 0.2) +
         scale_x_continuous(breaks = seq(15, 55, 5)) +
         labs(
           x = "Age de la mere ? la naissance",
           y = "Densit?",
           colour = "Source",
           fill = "Source"
         ) +
         theme_minimal()

# Pour voir pourquoi cette disparit? a lieu, on regarde dans la base source EDP (donc b?b?s EDP de la table individu) le coverage dans les donn?es fiscales.
# Dans d'autres mots, combien de ces b?b?s EDP ont un logement fiscal attach? ? leur identifiant (donc le ID_DIFF du b?b? pr?sent dans les tables fiscales)
dbGetQuery(con, "
           SELECT
           CASE
           WHEN age_mere < 25 THEN '< 25'
           WHEN age_mere <= 29 THEN '25-29'
           WHEN age_mere <= 34 THEN '30-34'
           WHEN age_mere <= 39 THEN '35-39'
           WHEN age_mere >= 40 THEN '40+'
           END AS age_cat,
           COUNT(*) AS n,
           COUNT(ID_FISC_LOG_DIFF) AS n_with_fiscal,
           ROUND(100.0 * COUNT(ID_FISC_LOG_DIFF) / COUNT(*), 1) AS pct_coverage
           FROM edp_final
           WHERE age_mere IS NOT NULL
           GROUP BY 1
           ORDER BY MIN(age_mere)
 ")

# Comment -> On voit que le coverage sur les donn?es fiscales semble etre moins important quand la m?re est jeune
         