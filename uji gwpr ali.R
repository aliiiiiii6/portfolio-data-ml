setwd("C:/Users/asus/Downloads/skripsi ali")
library(plm); library(lmtest); library(car); library(GWmodel); library(spdep)
library(readxl); library(stats); library(dplyr); library(spgwr); library(sp)
indo4 <- read_excel("data siap uji.xlsx") |> as.data.frame()
indo4$provinsi <- as.factor(indo4$provinsi)
indo4$tahun <- as.factor(indo4$tahun)
colSums(is.na(indo4))
model_mk <- lm(y ~ x1_akses_internet + x2_APM + x3_TPAK + x4_Pembiayaan_Syariah, data = indo4)
data.frame(VIF = vif(model_mk))
paneldata <- pdata.frame(indo4, index = c("provinsi", "tahun"))
formula_panel <- y ~ x1_akses_internet + x2_APM + x3_TPAK + x4_Pembiayaan_Syariah
cem <- plm(formula_panel, data = paneldata, model = "pooling")
fem <- plm(formula_panel, data = paneldata, model = "within", effect = "individual")
rem <- plm(formula_panel, data = paneldata, model = "random", effect = "individual")
summary(cem); summary(fem); summary(rem)
pFtest(fem, cem); phtest(fem, rem)
plmtest(formula_panel, data = paneldata, effect = "individual", type = "bp")
bptest(rem)
coord <- read_excel("data siap uji.xlsx")
coordinates(coord) <- ~Longitude + Latitude
bw <- bw.gwr(formula_panel, data = coord, kernel = "bisquare", adaptive = TRUE)
print(bw)
gwpr_model <- gwr.basic(formula_panel, data = coord, bw = bw, kernel = "bisquare", adaptive = TRUE)
bw.bisq <- bw; model.bisq <- gwpr_model
bw.gaus <- bw.gwr(formula_panel, data = coord, kernel = "gaussian", adaptive = TRUE)
model.gaus <- gwr.basic(formula_panel, data = coord, bw = bw.gaus, kernel = "gaussian", adaptive = TRUE)
summary(gwpr_model)
gwpr_model$GW.diagnostic
parameter_gwpr <- as.data.frame(gwpr_model$SDF)[, 1:5]
parameter_gwpr$provinsi <- indo4$provinsi
gwpr_df <- as.data.frame(gwpr_model$SDF)
get_significance <- function(coef, se, df) {
  tval <- coef / se
  pval <- 2 * pt(-abs(tval), df = df)
  data.frame(tval = tval, pval = pval, signif = ifelse(pval < 0.1, "Signifikan ", "Tidak"))
}
df_gwpr <- gwpr_model$GW.diagnostic$edf
sig_x1 <- get_significance(gwpr_df$x1_akses_internet, gwpr_df$x1_akses_internet_SE, df_gwpr)
sig_x2 <- get_significance(gwpr_df$x2_APM, gwpr_df$x2_APM_SE, df_gwpr)
sig_x3 <- get_significance(gwpr_df$x3_TPAK, gwpr_df$x3_TPAK_SE, df_gwpr)
sig_x4 <- get_significance(gwpr_df$x4_Pembiayaan_Syariah, gwpr_df$x4_Pembiayaan_Syariah_SE, df_gwpr)
signif_gwpr <- data.frame(
  Akses_Internet = sig_x1$signif,
  APM = sig_x2$signif,
  TPAK = sig_x3$signif,
  Pembiayaan_Syariah = sig_x4$signif,
  provinsi = indo4$provinsi
)
p_val <- gwr.t.adjust(gwpr_model)$results$p
p_val_df <- data.frame(Signifikan  = ifelse(p_val <= 0.1, "Signifikan ", "Tidak"), provinsi = indo4$provinsi)
rss_rem <- sum(rem$residuals^2)
rss_gwpr <- model.bisq$GW.diagnostic$RSS.gw
df_rem <- rem$df.residual; df_gwpr <- model.bisq$GW.diagnostic$edf
Fhit <- (rss_gwpr / df_gwpr) / (rss_rem / df_rem)
coef_gwpr <- as.data.frame(gwpr_model$SDF)
coef_gwpr$provinsi <- indo4$provinsi
# Simpan urutan provinsi sesuai data asli
urutan_prov <- unique(indo4$provinsi)

# Ubah kolom provinsi di coef_gwpr jadi factor dengan level sesuai urutan
coef_gwpr$provinsi <- factor(coef_gwpr$provinsi, levels = urutan_prov)

# Rekap parameter per provinsi tanpa menyusun ulang
coef_per_prov <- coef_gwpr %>%
  group_by(provinsi) %>%
  summarise(
    Intercept = mean(Intercept, na.rm = TRUE),
    x1_akses_internet = mean(x1_akses_internet, na.rm = TRUE),
    x2_APM = mean(x2_APM, na.rm = TRUE),
    x3_TPAK = mean(x3_TPAK, na.rm = TRUE),
    x4_Pembiayaan_Syariah = mean(x4_Pembiayaan_Syariah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(urutan = match(provinsi, urutan_prov)) %>%
  arrange(urutan) %>%
  select(-urutan)  # hilangkan kolom bantu

print(coef_per_prov, n = Inf)
fitted_gwpr <- coef_gwpr$Intercept +
  coef_gwpr$x1_akses_internet * indo4$x1_akses_internet +
  coef_gwpr$x2_APM * indo4$x2_APM +
  coef_gwpr$x3_TPAK * indo4$x3_TPAK +
  coef_gwpr$x4_Pembiayaan_Syariah * indo4$x4_Pembiayaan_Syariah
gwpr_result <- data.frame(
  provinsi = indo4$provinsi,
  tahun = indo4$tahun,
  y_actual = indo4$y,
  fitted = fitted_gwpr
)
localR2 <- model.bisq$SDF@data$Local_R2
cat("\nSummary Local R2 (per titik):\n")
print(summary(localR2))
cat("Mean Local R2:", mean(localR2, na.rm = TRUE), "\n")
df_local <- as.data.frame(model.bisq$SDF@data)
df_local$provinsi <- factor(indo4$provinsi, levels = unique(indo4$provinsi))
r2_local_per_prov <- df_local %>%
  group_by(provinsi) %>%
  summarise(mean_localR2 = mean(Local_R2, na.rm = TRUE))
cat("\nLocal R2 rata-rata per provinsi:\n")
print(r2_local_per_prov, n = Inf)
comparison <- data.frame(
  R2 = c(model.bisq$GW.diagnostic$gw.R2, summary(rem)$r.squared[1]),
  R2adj = c(model.bisq$GW.diagnostic$gwR2.adj, summary(rem)$r.squared[2]),
  RSS = c(model.bisq$GW.diagnostic$RSS.gw, rss_rem)
)
rownames(comparison) <- c("GWPR_Bisquare", "REM")
comparison

# Asumsi: 'coord' sudah berupa SpatialPointsDataFrame
# Pastikan data spasial berisi provinsi (identitas) unik
library(spdep)

# Membuat neighbors menggunakan k-nearest neighbors atau distance threshold
# Contoh: pakai 4 tetangga terdekat
coords_matrix <- coordinates(coord)
knn_nb <- knn2nb(knearneigh(coords_matrix, k = 4))

# Visualisasi (opsional)
plot(knn_nb, coords_matrix)

# Buat spatial weights list
weights_list <- nb2listw(knn_nb, style = "W")
# Buat vektor Local R2
local_r2 <- model.bisq$SDF@data$Local_R2

# Hitung Local Moran's I
lisa_result <- localmoran(local_r2, weights_list)
# Buat dataframe hasil LISA
lisa_df <- data.frame(
  provinsi = indo4$provinsi[!duplicated(indo4$provinsi)],
  Local_R2 = local_r2,
  Moran_I = lisa_result[, "Ii"],
  E_Ii    = lisa_result[, "E.Ii"],
  Var_Ii  = lisa_result[, "Var.Ii"],
  Z_Ii    = lisa_result[, "Z.Ii"],
  P_Value = lisa_result[, "Pr(z != E(Ii))"]
)



# Hapus duplikat provinsi jika data tahunan (gunakan data tahun terakhir misalnya)
lisa_df <- lisa_df[!duplicated(lisa_df$provinsi), ]

# Tampilkan hasil
print(lisa_df)
moran.test(local_r2, listw = weights_list)
