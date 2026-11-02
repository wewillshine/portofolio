#load package
#Packages Yang Digunakan 
library(readxl) 
library(openxlsx) 
library(spdep) 
library(rgdal) 
library(raster) 
library(corrplot)  
library(DescTools) 
library(nortest) 
library(car) 
library(spatialreg)
library(sf)
library(nortest) 
library(DescTools) 
library(lmtest)
library(tidyverse)
library(spgwr)
##########################--------------------------------###########################
setwd("D:/WICE 2025")
library(readxl)
data_Agregat <- read_excel("data Agregat.xlsx", + sheet = "Tanpa")
data<-Bismillah
head(data)
dim(data)

#input peta shp
library(sf)

# baca shapefile provinsi
peta <- st_read("D:/WICE 2025/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")
# cek isi data
print(peta)
View(peta)
jateng <- peta[peta$PROVINSI == "JAWA TENGAH", ]
plot(jateng["PROVINSI"])

##########################--------------------------------###########################

#Summary

summary(data)

#Cek linearitas
for (i in 1:8) {
  xvar <- paste0("X", i)  # Buat nama variabel dinamis (X1, X2, ...)
  
  # Plot scatter
  plot(data[[xvar]], data$Y,
       xlab = xvar,
       ylab = "Y",
       main = paste("Scatter Plot of", xvar, "and Y"),
       pch = 20, col = "orange", cex = 1.5)
  
  # Model regresi linear sederhana
  reg.klasik <- lm(as.formula(paste("Y ~", xvar)), data = data)
  
  # Tambahkan garis regresi
  abline(reg.klasik, col = "blue", lwd = 2)
}

#Pengujian Korelasi

corrplot(cor(data[,2:8]), method = "number")

##########################--------------------------------###########################

#Sebaran data spasial

library(sf)
library(sp)

# Hilangkan Z/M (misalnya kalau datanya XYZ atau XYM)
peta_xy <- st_zm(peta)

# Konversi ke Spatial
peta_sp <- as(peta_xy, "Spatial")

# Tambahkan variabel Y
peta_sp$y <- data$Y

# Plot
#spplot(peta_sp, "y", col.regions=color, main="Sebaran Spasial Peubah Y")

##########################--------------------------------###########################

#matriks pembobot

library(sf)

# 1. Pastikan geometri valid
peta <- st_make_valid(peta)

# 2. Hitung centroid
centroid <- st_centroid(peta)

# 3. Ambil koordinat X (longitude) dan Y (latitude)
longlat <- st_coordinates(centroid)

# 4. Tambahkan ke data frame peta (opsional)
peta$lon <- longlat[,1]
peta$lat <- longlat[,2]

# menghitung matriks jarak 
djarak<-dist(longlat) 
m.djarak<-as.matrix(djarak) #matriks jarak

head(m.djarak)

# 1 KNN
#k=5, 5 tetangga terdekat 
W.knn<-knn2nb(knearneigh(longlat,k=5,longlat=TRUE)) #matriks bobot dengan knn k=5 #knearneigh(x, k=1, longlat = NULL, use_kd_tree=TRUE) 

W.knn

class(W.knn)

#plot(peta, col='gray', border='blue', main ="KNN dengan k=5") 

#plot(W.knn1, longlat, col='red', lwd=2, add=TRUE)
W.knn1 <- nb2listw(W.knn,style='W') 
W.knn1
#k=4
W.knnk4<-knn2nb(knearneigh(longlat,k=4,longlat=TRUE)) #matriks bobot dengan knn k=5 #knearneigh(x, k=1, longlat = NULL, use_kd_tree=TRUE) 
W.knnk41 <- nb2listw(W.knnk4,style='W') 
W.knnk41

#k=6
W.knnk6<-knn2nb(knearneigh(longlat,k=6,longlat=TRUE)) #matriks bobot dengan knn k=5 #knearneigh(x, k=1, longlat = NULL, use_kd_tree=TRUE) 
W.knnk61 <- nb2listw(W.knnk6,style='W') 
W.knnk61

#k=7
W.knnk7<-knn2nb(knearneigh(longlat,k=7,longlat=TRUE)) #matriks bobot dengan knn k=5 #knearneigh(x, k=1, longlat = NULL, use_kd_tree=TRUE) 
W.knnk71 <- nb2listw(W.knnk7,style='W') 
W.knnk71

#k=8
W.knnk8<-knn2nb(knearneigh(longlat,k=8,longlat=TRUE)) #matriks bobot dengan knn k=5 #knearneigh(x, k=1, longlat = NULL, use_kd_tree=TRUE) 
W.knnk81 <- nb2listw(W.knnk8,style='W') 
W.knnk81

# 2. Power Distance Weight / Invers Distance Weight (Bobot Jarak Invers)
#Alpha = 1 
alpha1=1 
W.idw <-1/(m.djarak^alpha1) 
class(W.idw)

#dinormalisasi 
diag(W.idw)<-0 
rtot<-rowSums(W.idw,na.rm=TRUE)

W.idw.sd<-W.idw/rtot #row-normalized 
rowSums(W.idw.sd,na.rm=TRUE)

class(W.idw.sd)

W.idw.1 = mat2listw(W.idw.sd,style='W') 

summary(W.idw.1)


#Alpha = 2
alpha1=2 
W.idw2 <-2/(m.djarak^alpha1) 
class(W.idw2)

#dinormalisasi 
diag(W.idw2)<-0 
rtot<-rowSums(W.idw2,na.rm=TRUE)

W.idw.sd2<-W.idw2/rtot #row-normalized 
rowSums(W.idw.sd2,na.rm=TRUE)

W.idw.22 = mat2listw(W.idw.sd2,style='W') 

summary(W.idw.22)

# 3. Exponential Distance Weight

#alpa 1

alpha=1 

W.e<-exp((-alpha)*m.djarak) #dinormalisasi 
diag(W.e)<-0 

rtot<-rowSums(W.e,na.rm=TRUE) 

W.e.sd<-W.e/rtot #row-normalized 
rowSums(W.e.sd,na.rm=TRUE)

W.ed1 = mat2listw(W.e.sd,style='W') 

summary(W.ed1)

#alpa 2

alpha=2

W.e2<-exp((-alpha)*m.djarak) #dinormalisasi 
diag(W.e2)<-0 

rtot<-rowSums(W.e2,na.rm=TRUE) 

W.e.sd2<-W.e2/rtot #row-normalized 
rowSums(W.e.sd2,na.rm=TRUE)

W.ed2 = mat2listw(W.e.sd2,style='W') 

summary(W.ed2)

##########################--------------------------------###########################

# 4. Gaussian Distance Weight
h <- mean(m.djarak)  # bisa diganti sd(m.djarak) atau nilai tertentu
W.g <- exp(-(m.djarak^2) / (2*h^2))

# buang diagonal
diag(W.g) <- 0

# normalisasi
rtot <- rowSums(W.g, na.rm=TRUE)
W.g.sd <- W.g / rtot
rowSums(W.g.sd, na.rm=TRUE)

# ubah ke listw
W.gaussian <- mat2listw(W.g.sd, style="W")

# cek Moran I
MI.gaussian <- moran(data$Y, W.gaussian, 
                     n=length(W.gaussian$neighbours), 
                     S0=Szero(W.gaussian))

MI.gaussian


##########################--------------------------------###########################
#Pemilihan matriks bobot

MI.knn <- moran(data$Y, W.knn1, n=length(W.knn1$neighbours), S0=Szero(W.knn1))
MI.knnk41 <- moran(data$Y, W.knnk41, n=length(W.knn1$neighbours), S0=Szero(W.knnk41))
MI.knnk61 <- moran(data$Y, W.knnk61, n=length(W.knn1$neighbours), S0=Szero(W.knnk61))
MI.knnk71 <- moran(data$Y, W.knnk71, n=length(W.knn1$neighbours), S0=Szero(W.knnk71))
MI.knnk81 <- moran(data$Y, W.knnk81, n=length(W.knn1$neighbours), S0=Szero(W.knnk81))
MI.power1 <- moran(data$Y, W.idw.1, n=length(W.idw.1$neighbours), S0=Szero(W.idw.1))
MI.exp1 <- moran(data$Y, W.ed1, n=length(W.ed1$neighbours), S0=Szero(W.ed1))
MI.power2 <- moran(data$Y, W.idw.22, n=length(W.idw.22$neighbours), S0=Szero(W.idw.22))
MI.exp2 <- moran(data$Y, W.ed2, n=length(W.ed2$neighbours), S0=Szero(W.ed2))

moranindeks<-data.frame(
  "Matriks Bobot"=c("Gaussian","KNN (k=5)","KNN (k=4)", "KNN (k=6)", "KNN (k=7)", "KNN (k=8)", "Power distance weight (alpha=1)", "Exponential distance weight (alpha=1)", "Power distance weight (alpha=2)", "Exponential distance weight (alpha=2)"),
  "Nilai Indeks Moran"=c(MI.gaussian$I, MI.knn$I,MI.knnk41$I, MI.knnk61$I,MI.knnk71$I, MI.knnk81$I , MI.power1$I,MI.exp1$I, MI.power2$I, MI.exp2$I))

moranindeks

Woptimum <- W.ed2
moran.test(data$Y, Woptimum, randomisation = TRUE, zero.policy = TRUE)

#Moran Plot
head(data)
moran.plot(data$Y, Woptimum, labels=data$Provinsi)

##########################--------------------------------###########################

# Rename otomatis kolom mulai dari ke-2 sampai terakhir
#names(data)[2:ncol(data)] <- paste0("X", 1:(ncol(data)-1))

# Cek hasilnya
head(data)

reg.klasik = lm(Y~X1+X3+X4+X5+X7+X8+X9+X11+X12, data = data)

summary(reg.klasik)

#Uji Multikol

car::vif(reg.klasik)

#Uji Normalitas

err.regklasik<-reg.klasik$residuals
ad.test(err.regklasik) #anderson darling

#Uji Autokorelasi Spasial

moran.test(reg.klasik$residuals, listw=Woptimum, alternative="two.sided", zero.policy = TRUE)

#Uji Homogenitas

bptest(reg.klasik)
##########################-------------------------

library(car)
library(nortest)
library(lmtest)
library(spdep)

# Daftar kombinasi (sesuai list dari gambar)
kombinasi_list <- list(
  c("X1","X2","X3","X5","X6","X7","X9","X11","X13"),
  c("X1","X3","X4","X5","X7","X8","X9","X11","X12"),
  c("X1","X4","X5","X7","X8","X9","X11","X12","X13"),
  c("X2","X3","X4","X6","X7","X8","X9","X11","X12"),
  c("X3","X4","X5","X7","X8","X9","X11","X12","X13"),
  c("X4","X5","X6","X7","X8","X9","X11","X12","X13"),
  c("X1","X2","X3","X4","X6","X9","X11","X12")
)

hasil <- data.frame()

for (vars in kombinasi_list) {
  form <- as.formula(paste("Y ~", paste(vars, collapse = "+")))
  model <- lm(form, data = data)
  
  # Simpan hasil
  hasil <- rbind(hasil, data.frame(
    Kombinasi = paste(vars, collapse = "+"),
    R2 = summary(model)$r.squared,
    AdjR2 = summary(model)$adj.r.squared,
    stringsAsFactors = FALSE
  ))
}

# Urutkan berdasarkan R2 terbesar
hasil_sorted <- hasil[order(-hasil$R2), ]

View(hasil_sorted)


##########################--------------------------------###########################

#GWR
b.gwr <- gwr.sel(Y~X1+X3+X4+X5+X7+X8+X9+X11+X12,
                 data = data,
                 coords = longlat,
                 gweight = gwr.Gauss)

b.gwr

# 1. Pastikan koordinat pakai XY saja, bukan XYZ
peta <- st_zm(peta)   # buang Z dimension
centroid <- st_centroid(peta)
longlat <- st_coordinates(centroid)

#Pemodelan

#####################--

rtg.model <- gwr(Y~X1+X3+X4+X5+X7+X8+X9+X11+X12,
                 data = data,
                 coords = longlat,
                 bandwidth = b.gwr,
                 gweight = gwr.Gauss ,
                 hatmatrix = TRUE)
rtg.model

koef_lokal <- as.data.frame(rtg.model$SDF)
head(koef_lokal)


#Uji Diagnostik Sisaan
err.rtg<-rtg.model$SDF$gwr.e
ad.test(err.rtg) #anderson darling
hist(err.rtg)
qqnorm(err.rtg,datax=T)
qqline(rnorm(length(err.rtg),mean(err.rtg),sd(err.rtg)),datax=T, col="red")
bptest(rtg.model$lm, weights = rtg.model$gweight) 
gwr.morantest(rtg.model, Woptimum, zero.policy = TRUE)

#Evaluasi Model
BFC99.gwr.test(rtg.model)
BFC02.gwr.test(rtg.model)
BFC02.gwr.test(rtg.model,approx = T)
anova(rtg.model)
anova(rtg.model,approx=T)

data.frame("MODEL" = c("RTG","Regresi Klasik"),
           "AIC" = c(rtg.model[["results"]][["AICh"]],AIC(reg.klasik)))%>% arrange(AIC)

############################################

# Ambil koefisien lokal
koef_lokal <- as.data.frame(rtg.model$SDF)

View(koef_lokal)
write.xlsx(koef_lokal, "koef_GWR.xlsx")

# Derajat bebas
df <- 33  

# t kritis untuk alpha 5% (dua sisi)
t_critical <- qt(0.975, df)  
t_critical
