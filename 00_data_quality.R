# ============================================================
# EMU-430 Project — EV Charging Infrastructure, Turkey
# Script 00: Data Quality Check
# Author  : Azra Azizoğlu
# Data    : EPDK Şarj İstasyonları Lisans Kayıt Verileri
# ============================================================

library(readxl)
library(dplyr)
library(stringr)

# ── Load data ────────────────────────────────────────────────
df <- read_excel("sarj_istasyonlari_birlesik.xlsx")

cat("=======================================================\n")
cat("DATA QUALITY REPORT — sarj_istasyonlari_birlesik.xlsx\n")
cat("=======================================================\n\n")

cat(sprintf("Rows: %s   Columns: %d\n", format(nrow(df), big.mark=","), ncol(df)))
cat("Column names:", paste(names(df), collapse=", "), "\n\n")

# ── 1. Missing values ────────────────────────────────────────
cat("--- 1. MISSING VALUES ---\n")
na_counts <- colSums(is.na(df))
for (col in names(na_counts)) {
  pct <- na_counts[col] / nrow(df) * 100
  cat(sprintf("  %-30s: %5s (%4.1f%%)\n", col, format(na_counts[col], big.mark=","), pct))
}

# ── 2. Full duplicate rows ───────────────────────────────────
cat("\n--- 2. DUPLICATE ROWS (full row) ---\n")
n_dup_full <- sum(duplicated(df))
cat(sprintf("  Fully duplicate rows: %s\n", format(n_dup_full, big.mark=",")))

# ── 3. Duplicate Soket No ────────────────────────────────────
cat("\n--- 3. DUPLICATE SOKET NO ---\n")
n_dup_soket <- sum(duplicated(df$`Soket No`))
cat(sprintf("  Duplicate Soket No   : %s\n", format(n_dup_soket, big.mark=",")))
cat(sprintf("  Unique Soket No      : %s / %s\n",
    format(n_distinct(df$`Soket No`), big.mark=","),
    format(nrow(df), big.mark=",")))

# ── 4. Soket Tipi values ─────────────────────────────────────
cat("\n--- 4. SOKET TIPI VALUES ---\n")
print(table(df$`Soket Tipi`))
unexpected <- df$`Soket Tipi`[!df$`Soket Tipi` %in% c("AC", "DC")]
cat(sprintf("  Unexpected values: %d\n", length(unexpected)))

# ── 5. Hizmet Şekli values ───────────────────────────────────
cat("\n--- 5. HIZMET SEKLI VALUES ---\n")
print(table(df$`Hizmet Şekli`))

# ── 6. Blank marka ───────────────────────────────────────────
cat("\n--- 6. BLANK MARKA ---\n")
blank_marka <- sum(is.na(df$Marka) | str_trim(df$Marka) == "")
cat(sprintf("  Blank/null marka: %d\n", blank_marka))

# ── 7. City extraction ───────────────────────────────────────
cat("\n--- 7. CITY EXTRACTION ---\n")
df <- df %>%
  mutate(City = str_extract(Adres, "[^/]+$") %>%
                str_trim() %>%
                str_to_upper())
city_na <- sum(is.na(df$City))
cat(sprintf("  Rows where city could not be parsed: %d (%.1f%%)\n",
    city_na, city_na / nrow(df) * 100))
cat(sprintf("  Unique cities: %d\n", n_distinct(df$City, na.rm=TRUE)))

# ── 8. Duplicate origin ──────────────────────────────────────
cat("\n--- 8. DUPLICATE ORIGIN (which source files?) ---\n")
dup_rows <- df[duplicated(df$`Soket No`) | duplicated(df$`Soket No`, fromLast=TRUE), ]
cat("Top source files contributing duplicates:\n")
print(sort(table(dup_rows$kaynak_dosya), decreasing=TRUE)[1:10])

cat("\nBrand breakdown in duplicates:\n")
print(sort(table(dup_rows$Marka), decreasing=TRUE)[1:8])

# ── Conclusion ───────────────────────────────────────────────
cat("\n--- CONCLUSION ---\n")
cat("  Root cause: same sockets appear in multiple monthly EPDK\n")
cat("  snapshot files. Deduplication by Soket No is required.\n")
cat(sprintf("  Rows to remove: %s\n", format(n_dup_soket, big.mark=",")))
cat(sprintf("  Clean dataset  : %s rows\n",
    format(nrow(df) - n_dup_soket, big.mark=",")))
