setwd("G:/Mi unidad/1 Academia/1 - CURSOS/220301 - Satelitales/")
#install.packages("raster")
library(raster)
library(ncdf4)
library(rgdal)
library(reshape)

#________________________________________________________________________________________
#### 0 - Datos ####
PNCP <- readOGR("shape/Cabo_Pulmo_PN.shp")
indices <- read.csv("data/indices.den2.csv")
names(indices)
sitios <- aggregate(Year ~ Site + N_coord + W_coord, data = indices, FUN = sum)
pulmo <- data.frame(Lon =(sitios$W_coord), Lat=(sitios$N_coord))

sst1 <- raster("nc data/erdMWsstdmday_LonPM180_3a87_c274_09e0.nc")
plot(sst1)
plot(PNCP, add = T)


sst <- stack("nc data/erdMWsstdmday_LonPM180_3a87_c274_09e0.nc")
plot(sst)
plot(sst, col = rev(rainbow(100)), xlim=c(-109.5, -109.35), ylim= c(23.35, 23.5))
plot(sst[[1]], col = rev(rainbow(100)), xlim=c(-109.5, -109.35), ylim= c(23.35, 23.5))
plot(PNCP, add = T)
points(pulmo, pch = 20 , cex = 1)


#### verificador
fechas <- as.numeric(substr(names(sst), 2,11))
fechas <- as.Date(as.POSIXct(fechas, origin = "1970-01-01"))
fechas[[46]] <- as.Date("2006-10-16")
names(sst) <- fechas

19*12 #228 capas
nlayers(sst)

boxplot(sst[[1]])
boxplot(sst)

#OJO: Agregué estas lineas de codigo para poder eliminar las dos fechas con valores raros:
sst[["X2013.12.16"]] # en band veo que es la capa numero 129
sst[["X2014.01.16"]] # en band veo que es la capa numero 130
plot(sst[[129]]) # la grafico para que vean que no tiene valores, por eso arroja NA's
plot(PNCP, add = T)

sst <- dropLayer(sst, c(129, 130)) # con esta linea las esliminamos
nlayers(sst) # pasamos de 224 a 222

names(sst)
capas <- data.frame(year =substr(names(sst),2,5), 
                    month =substr(names(sst),7,8), 
                    capa =1)
capas <- cast(capas, year ~ month, value = "capa", sum)
capas

mes <- unique(substr(names(sst),7,8))

names(sst)
sst.clim <- mean(sst[[(1:222)[substr(names(sst), 7,8) == "01"]]], na.rm=T) # cambié de 224 a 222
for(i in 2:12) sst.clim <- stack(sst.clim, mean(sst[[(1:222)[substr(names(sst), 7,8) == mes[i]]]], na.rm=T))
names(sst.clim) <- month.abb[]
plot(sst.clim, col = rev(rainbow(100)), zlim = c(19,32), xlim=c(-109.5, -109.35), ylim= c(23.35, 23.5))

# extraemos los valores originales de sst a partir de las coordenadas del objeto sitios
plot(sst)
sst.ext <- extract(sst, pulmo, buffer = 2000, fun = mean)
sst.ext <- data.frame(t(sst.ext))
names(sst.ext) <- sitios$Site

write.csv(sst.ext, "write_csv/sst.csv")


##### NUEVO CODIGO
class(sst.ext)
sst.ext$date <- rownames(sst.ext)
sst.ext$year <- substr(sst.ext$date, 2,5)
sst.ext$month <- substr(sst.ext$date, 7,8) 
head(sst.ext, 3)

#la siguiente funcion sirve para "apliar los valores de sst de cada sitio para que aparezcan en una solo columna
melt.sst <- melt(sst.ext, id.vars = c("date", "year", "month"))
colnames(melt.sst)[4] <- "sitio"
colnames(melt.sst)[5] <- "sst"
head(melt.sst, 3)
melt.sst$id_clim <- paste(melt.sst$sitio, melt.sst$month, sep = "-")

sst.mean <- aggregate(sst ~ sitio + month, data=melt.sst, FUN = mean)
sst.mean$id_clim2 <- paste(sst.mean$sitio, sst.mean$month, sep = "-")
head(sst.mean, 3)

sst.sd <- aggregate(sst ~ sitio + month, data=melt.sst, FUN = sd)
sst.sd$sd <- paste(sst.sd$sitio, sst.sd$month, sep = "-")
head(sst.sd, 3)

library(DataCombine)
#use "findreplace" y "merge" para generar codigos unicos (basados en mes y sitio) para que pudiera quenerar un dataframe que contenga la climatologia (promedio mensual) y la desviacion estandar de cada observacion para poder calcular la anomalia
sst.final <- FindReplace(data = melt.sst, Var = "id_clim", replaceData = sst.mean, from = "id_clim2", to = "sst")
head(sst.final)
colnames(sst.final)[6] <- "sst.mean"
sst.final$sst.mean <- as.numeric(sst.final$sst.mean)

sd.final <- FindReplace(data = melt.sst, Var = "id_clim", replaceData = sst.sd, from = "sd", to = "sst")
head(sd.final)
colnames(sd.final)[6] <- "sst.sd"
sd.final$sst.sd <- as.numeric(sd.final$sst.sd)

sst.mean.sd <- merge(sst.final, sd.final)
sst.mean.sd$anomalia <- (sst.mean.sd$sst - sst.mean.sd$sst.mean) / sst.mean.sd$sst.sd
head(sst.mean.sd)
boxplot(anomalia ~ year, data = sst.mean.sd)

#### clorofila ####
chla <- stack("nc data/erdMWchlamday_LonPM180_b989_c4f6_b9f8.nc")
fechas.chla <- as.numeric(substr(names(chla), 2,11))
fechas.chla <- as.Date(as.POSIXct(fechas.chla, origin = "1970-01-01"))
fechas.chla[[46]] <- as.Date("2006-10-16")
names(chla) <- fechas.chla

nlayers(chla)

boxplot(chla[[1]])
boxplot(chla)

#OJO: Agregué estas lineas de codigo para poder eliminar las dos fechas.chla con valores raros:
#chla[["X2013.12.16"]] # en band veo que es la capa numero 129
#chla[["X2014.01.16"]] # en band veo que es la capa numero 130
#plot(chla[[131]]) # la grafico para que vean que no tiene valores, por eso arroja NA's
#plot(PNCP, add = T)

#chla <- dropLayer(chla, c(129, 130)) # con esta linea las esliminamos
nlayers(chla) # pasamos de 224 a 222

names(chla)
capas.chla <- data.frame(year =substr(names(chla),2,5), 
                    month =substr(names(chla),7,8), 
                    capa =1)
capas.chla <- cast(capas.chla, year ~ month, value = "capa", sum)
capas.chla

#mes <- unique(substr(names(chla),7,8))

names(chla)

# extraemos los valores originales de chla a partir de las coordenadas del objeto sitios
plot(chla)
chla.ext <- extract(chla, pulmo, buffer = 2000, fun = mean)
chla.ext <- data.frame(t(chla.ext))
names(chla.ext) <- sitios$Site

write.csv(chla.ext, "write_csv/chla.csv")


##### NUEVO CODIGO
class(chla.ext)
chla.ext$date <- rownames(chla.ext)
chla.ext$year <- substr(chla.ext$date, 2,5)
chla.ext$month <- substr(chla.ext$date, 7,8) 
head(chla.ext, 3)

#la siguiente funcion sirve para "apliar los valores de chla de cada sitio para que aparezcan en una solo columna
melt.chla <- melt(chla.ext, id.vars = c("date", "year", "month"))
colnames(melt.chla)[4] <- "sitio"
colnames(melt.chla)[5] <- "chla"
head(melt.chla, 3)
melt.chla$id_clim <- paste(melt.chla$sitio, melt.chla$month, sep = "-")

chla.mean <- aggregate(chla ~ sitio + month, data=melt.chla, FUN = mean)
chla.mean$id_clim2 <- paste(chla.mean$sitio, chla.mean$month, sep = "-")
head(chla.mean, 3)

chla.sd <- aggregate(chla ~ sitio + month, data=melt.chla, FUN = sd)
chla.sd$sd <- paste(chla.sd$sitio, chla.sd$month, sep = "-")
head(chla.sd, 3)

#library(DataCombine)
#use "findreplace" y "merge" para generar codigos unicos (basados en mes y sitio) para que pudiera quenerar un dataframe que contenga la climatologia (promedio mensual) y la desviacion estandar de cada observacion para poder calcular la anomalia
chla.final <- FindReplace(data = melt.chla, Var = "id_clim", replaceData = chla.mean, from = "id_clim2", to = "chla")
head(chla.final)
colnames(chla.final)[6] <- "chla.mean"
chla.final$chla.mean <- as.numeric(chla.final$chla.mean)

sd.final <- FindReplace(data = melt.chla, Var = "id_clim", replaceData = chla.sd, from = "sd", to = "chla")
head(sd.final)
colnames(sd.final)[6] <- "chla.sd"
sd.final$chla.sd <- as.numeric(sd.final$chla.sd)

chla.mean.sd <- merge(chla.final, sd.final)
chla.mean.sd$anomalia <- (chla.mean.sd$chla - chla.mean.sd$chla.mean) / chla.mean.sd$chla.sd
head(chla.mean.sd)
boxplot(anomalia ~ year, data = chla.mean.sd)



#### PAR ####
par <- stack("nc data/erdMWpar0mday_LonPM180_6cdd_12fc_7686.nc")
fechas.par <- as.numeric(substr(names(par), 2,11))
fechas.par <- as.Date(as.POSIXct(fechas.par, origin = "1970-01-01"))
fechas.par[[46]] <- as.Date("2006-10-16")
names(par) <- fechas.par

nlayers(par)

#boxplot(par[[1]])
boxplot(par)

#OJO: Agregué estas lineas de codigo para poder eliminar las dos fechas.par con valores raros:
#par[["X2013.12.16"]] # en band veo que es la capa numero 129
#par[["X2014.01.16"]] # en band veo que es la capa numero 130
#plot(par[[131]]) # la grafico para que vean que no tiene valores, por eso arroja NA's
#plot(PNCP, add = T)

#par <- dropLayer(par, c(129, 130)) # con esta linea las esliminamos
nlayers(par) # pasamos de 224 a 222

names(par)
capas.par <- data.frame(year =substr(names(par),2,5), 
                    month =substr(names(par),7,8), 
                    capa =1)
capas.par <- cast(capas.par, year ~ month, value = "capa", sum)
capas.par

#mes <- unique(substr(names(par),7,8))

names(par)

# extraemos los valores originales de par a partir de las coordenadas del objeto sitios
plot(par)
par.ext <- extract(par, pulmo, buffer = 2000, fun = mean)
par.ext <- data.frame(t(par.ext))
names(par.ext) <- sitios$Site

write.csv(par.ext, "write_csv/par.csv")


##### NUEVO CODIGO
#class(par.ext)
par.ext$date <- rownames(par.ext)
par.ext$year <- substr(par.ext$date, 2,5)
par.ext$month <- substr(par.ext$date, 7,8) 
head(par.ext, 3)

#la siguiente funcion sirve para "apliar los valores de par de cada sitio para que aparezcan en una solo columna
melt.par <- melt(par.ext, id.vars = c("date", "year", "month"))
colnames(melt.par)[4] <- "sitio"
colnames(melt.par)[5] <- "par"
head(melt.par, 3)
melt.par$id_clim <- paste(melt.par$sitio, melt.par$month, sep = "-")

par.mean <- aggregate(par ~ sitio + month, data=melt.par, FUN = mean)
par.mean$id_clim2 <- paste(par.mean$sitio, par.mean$month, sep = "-")
head(par.mean, 3)

par.sd <- aggregate(par ~ sitio + month, data=melt.par, FUN = sd)
par.sd$sd <- paste(par.sd$sitio, par.sd$month, sep = "-")
head(par.sd, 3)

#library(DataCombine)
#use "findreplace" y "merge" para generar codigos unicos (basados en mes y sitio) para que pudiera quenerar un dataframe que contenga la climatologia (promedio mensual) y la desviacion estandar de cada observacion para poder calcular la anomalia
par.final <- FindReplace(data = melt.par, Var = "id_clim", replaceData = par.mean, from = "id_clim2", to = "par")
head(par.final)
colnames(par.final)[6] <- "par.mean"
par.final$par.mean <- as.numeric(par.final$par.mean)

sd.final <- FindReplace(data = melt.par, Var = "id_clim", replaceData = par.sd, from = "sd", to = "par")
head(sd.final)
colnames(sd.final)[6] <- "par.sd"
sd.final$par.sd <- as.numeric(sd.final$par.sd)

par.mean.sd <- merge(par.final, sd.final)
par.mean.sd$anomalia <- (par.mean.sd$par - par.mean.sd$par.mean) / par.mean.sd$par.sd
head(par.mean.sd)
boxplot(anomalia ~ year, data = par.mean.sd)


#### Dataframe final / indices ####
head(sst.mean.sd, 3) #mean(sst.mean.sd$anomalia)
head(chla.mean.sd, 3) #mean(chla.mean.sd$anomalia)
head(par.mean.sd, 3) #mean(par.mean.sd$anomalia)

nrow(sst.mean.sd); nrow(chla.mean.sd); nrow(par.mean.sd)

head(indices, 3)

# primero calcular los promedios anuales de anomalías para cada sitio (de las 3 variables)
anom.sst <- aggregate(anomalia ~ sitio + year, data=sst.mean.sd, FUN = mean, na.rm = TRUE)
anom.sst$id.y.s <- paste(anom.sst$year, anom.sst$sitio, sep ="")
head(anom.sst, 3)
anom.chla <- aggregate(anomalia ~ sitio + year, data=chla.mean.sd, FUN = mean, na.rm = TRUE)
anom.chla$id.y.s <- paste(anom.chla$year, anom.chla$sitio, sep ="")
head(anom.chla, 3) 
anom.par <- aggregate(anomalia ~ sitio + year, data=par.mean.sd, FUN = mean, na.rm = TRUE)
anom.par$id.y.s <- paste(anom.par$year, anom.par$sitio, sep ="")
head(anom.par, 3)


#### findreplace ####
#primero sst
indices$id <- indices$X
datos <- FindReplace(data = indices, Var = "id", replaceData = anom.sst, from = "id.y.s", to = "anomalia")
colnames(datos)[136] <- "a.sst" #de una vez cambiamos el nombre de la columna por el real
head(datos)

#ahora chla
datos$id <- datos$X
datos <- FindReplace(data = datos, Var = "id", replaceData = anom.chla, from = "id.y.s", to = "anomalia")
colnames(datos)[137] <- "a.chla" #de una vez cambiamos el nombre de la columna por el real
head(datos)

#ahora par
datos$id <- datos$X
datos <- FindReplace(data = datos, Var = "id", replaceData = anom.par, from = "id.y.s", to = "anomalia")
colnames(datos)[138] <- "a.par" #de una vez cambiamos el nombre de la columna por el real
head(datos)
summary(datos)
write.csv(datos, "data/datos.csv", row.names = F)
