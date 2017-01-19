library(raster)
library(sp)
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB4.rda")
load("data/vcfGewata.rda")
gewata <- brick(GewataB1, GewataB2,GewataB3,GewataB4,GewataB5, GewataB7,vcfGewata)

hist(gewata$gewataB1,breaks = seq(0, 2000, by = 100))
hist(gewata$gewataB2, breaks = seq(0, 3000, by = 100))
hist(gewata$gewataB3, breaks = seq(0, 3000, by = 100))
hist(gewata$gewataB4, breaks = seq(0, 5000, by = 100))
hist(gewata$gewataB5, breaks = seq(0, 16000, by = 1000))
hist(gewata$gewataB7, breaks = seq(0, 16000,by=100))
hist(gewata$vcf2000Gewata ,breaks = seq(0, 300, by = 50))

gewata$gewataB1[gewata$gewataB1<100 ] <- NA
gewata$gewataB2[gewata$gewataB2<300 ] <- NA
gewata$gewataB3[gewata$gewataB3< 200] <- NA
gewata$gewataB4[gewata$gewataB4<100 ] <- NA
gewata$gewataB5[gewata$gewataB5<500 ] <- NA
gewata$gewataB7[gewata$gewataB7<300 ] <- NA
gewata$vcf2000Gewata[gewata$vcf2000Gewata<30 ] <- NA

names(gewata) <- c("band1", "band2", "band3", "band4", "band5","band7","vcf")
df <- as.data.frame(getValues(gewata))
model_lm=lm(formula = vcf ~ band1 + band2 + band3 + band4 + band5 +band7,data=df)
summary(model_lm)$r.squared
pred <- predict(gewata, model=model_lm, na.rm=TRUE)


library(hydroGOF)
predmatrix=getValues(pred)
vcfmatrix=getValues(vcfGewata)
rmse(predmatrix,vcfmatrix,na.rm = TRUE)
load("data/trainingPoly.rda")

trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, pred, field='Code')
cols <- c("orange", "dark green", "light blue")

names(classes) <- "class"
difference=pred-gewata$vcf
zonal(difference,classes)
