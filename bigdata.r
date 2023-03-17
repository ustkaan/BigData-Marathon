# -*- coding: utf-8 -*-

#Paketleri yüklüyoruz.

install.packages("caret") #Verileri parçalamak için eklememiz gereken kütüphane

install.packages("clusterSim") #Normalizasyon için

install.packages("class") #Makine öğrenmesi  için

install.packages("chron") #Tarihi dönüştürmek için eklenmişti

install.packages("GGally")

install.packages("plotrix")

install.packages("randomForest")

install.packages('e1071') #SVR için

install.packages("MASS")

#Paketleri projeye dahil ediyoruz.

library("caret")

library("clusterSim")

library("class")

library("chron")

library("GGally") #ggcor icin yani korelasyon grafigi icin.

library("plotrix")

library("randomForest")

library('e1071')

library("MASS") #stepAIC icin

#Veri setini indirip okuyoruz.

#Kaggle Dataset Link : https://www.kaggle.com/datasets/rojour/boston-results

#kaggle datasets download -d rojour/boston-results

m1 = as.data.frame(read.csv("/content/marathon_results_2015.csv"), header=TRUE, sep=',', dec='.')

m2 = as.data.frame(read.csv("/content/marathon_results_2016.csv"), header=TRUE, sep=',', dec='.')

m3 = as.data.frame(read.csv("/content/marathon_results_2017.csv"), header=TRUE, sep=',', dec='.')

#Birleştirmeden önceden stün sayıları eşitmi onu kontrol etmek için bakıyoruz.

stunSayisi = c(ncol(m1),ncol(m2),ncol(m3))

stunSayisi

#Farklılık yaratan stünü buluyoruz.

setdiff(colnames(m1),colnames(m2))

setdiff(colnames(m3),colnames(m2))

#Farklılık yaratan stünları verimizden çıkarıyoruz.

m1 = m1[-grep("X.1", colnames(m1))]

m3 = m3[-grep("X.1", colnames(m3))]

#Tekrar stünları kontrol ediyoruz.

setdiff(colnames(m1),colnames(m2))

stunSayisi = c(ncol(m1),ncol(m2),ncol(m3))

stunSayisi

#Veri setlerini birleştiriyoruz.rbind ile verileri satir olarak alt alta ekliyoruz.

marathon = rbind(m1,m2,m3)

#Satır sayılarını kontrol ediyoruz.

nrow(marathon)

sum(nrow(m1),nrow(m2),nrow(m3))

#Veri setimizi tanıyoruz.

summary(marathon)

head(marathon)

tail(marathon)

#NA veya tekrar etmiş mi kontrolunu yapıyoruz.

anyNA(marathon)

anyDuplicated(marathon)

#Verilerimizi dosyaya yazıyoruz.

write.csv(marathon,file="/content/marathon_results_15_16_17.csv")

#Verilerimizin tamamını View ile görebiliriz.

View(marathon)

#Verilerimiz görselleştiriyoruz.

#Barplot ile stün grafiği şeklinde gösterim yapacağız.

barplot(table(marathon$M.F),main="Erkek ve Kadın Sporcuların Sayısı",xlab="Cinsiyet",ylab="Katılımcı Sayısı",names.arg=c("Kadın","Erkek"),,col=c("red","blue"), legend=c("Kadın","Erkek"))

#Grafiklerin cikstisini almak icin asagidaki adimlari uygulayarak yaptik.
#1#jpeg("~/Pictures/EKKS.jpg")
#2#barplot(table(marathon$M.F),main="Erkek ve Kadın Sporcuların Sayısı",xlab="Cinsiyet",ylab="Katılımcı Sayısı",names.arg=c("Kadın","Erkek"),,col=c("red","blue"), legend=c("Kadın","Erkek"))
#3#dev.off()

#2. adimda her bir cizim icin kodlar degisiyor.
#1. adimda dosya isminde degisiklik yapiyoruz.

barplot(table(marathon$M.F)/nrow(marathon),main="Erkek ve Kadın Sporcular %",xlab="Cinsiyet",ylab="Yüzde",names.arg=c("Kadın","Erkek"),col=c("red","blue"), legend=c("Kadın","Erkek"))

#Pie ile pasta dilimi şeklinde gösterim yapacağız.

pie(table(marathon$M.F)/nrow(marathon), main="Erkek ve Kadın Sporcular %",col=c("red","blue"))

barplot(table(marathon$Country),main="Ülkelere göre Sporcuların Sayısı",xlab="Ülkeler",ylab="Katılımcı Sayısı")

barplot(table(marathon$Age),main="Yaşlara göre Sporcuların Sayısı",xlab="Yaşlar",ylab="Katılımcı Sayısı")

#Histogram grafiği çizdirmek için

hist(marathon$Age, main="Yaşlara göre Sporcuların Sayısı",xlab="Yaşlar",ylab="Katılımcı Sayısı")

#Histogram grafiğini yüzde olarak görmek için freq = FALSE olarak atadık.

hist(marathon$Age, main="Yaşlara göre Sporcuların %", freq = FALSE,xlab="Yaşlar",ylab="Yüzde")

#Stün isimlerini görmek için buna göre bizler bazı verilerimiz ile işlem yapacağız.

colnames(marathon)

danismanli = as.data.frame(marathon[c(4,10,11,12,13,14,15,16,17,18,21,22,23,24)])

#Verimizde boş veya eksik değerler var ise bunu tespit edip ilgili satırı sileceğiz.

danismanli[danismanli == "-"] = NA

danismanli[danismanli == ""] = NA

danismanli[danismanli == " "] = NA

danismanli = na.omit(danismanli) #NA olan tüm değerleri silmek için

#Kaç satır verimizden silindiğini görmek için

print( nrow( marathon ) - nrow( danismanli ) )

View(danismanli)

write.csv(danismanli,file="/content/marathon_results_15_16_17_cl.csv")

#Korelasyon grafiğini çizdirerek verilerin birbirleri ile olan ilişkilerine bakacağız.

ggcorr(danismanli)

cor(danismanli$Overall, danismanli$Gender) #Korelasyon varmı cor ile hesaplayıpta görebiliriz.

#Verilerin nasıl dağıldığını göstermek için scatter.smooth kullandık.

scatter.smooth(x =danismanli$Overall, y=danismanli$Gender, main ="Overall ve Gender veri dagilimi")

scatter.smooth(x =danismanli$Overall, y=danismanli$Division, main ="Overall ve Division veri dagilimi")

scatter.smooth(x =danismanli$Overall, y=danismanli$Age, main ="Overall ve Age veri dagilimi")

scatter.smooth(x =danismanli$Overall, y=danismanli$Overall, main ="Overall ve Overall veri dagilimi")

danismansiz = as.data.frame(danismanli[1:10])

View(danismansiz)

#Not: Pekiştirmeli ve karma modeller kullanılmamıştır.

#Bizler için şimdilik 10 000 veri yeterli olduğundan rastegele 10 000 satır alacağız.

rastgele_dan = danismanli[sample(nrow(danismanli),10000), ]

#Kaç satır aldığımızı görmek için

nrow(rastgele_dan)

#Verileri normalleştirmek için

rastgele_dsz = danismansiz[sample(nrow(danismansiz),10000), ]

View(rastgele_dan)

View(rastgele_dsz)

normal_danisan = data.Normalization(x= rastgele_dan, type="n4", normalization = "column")

normal_danisan

normal_dansz = data.Normalization(x= rastgele_dsz, type="n4", normalization = "column")

normal_dansz

#Verileri test ve eğitim olarak ayırıyoruz.

egitimIndis_dan = createDataPartition(y=rastgele_dan$Overall,p=.70,list=FALSE)

nrow(egitimIndis_dan)

egitimIndis_dsz = createDataPartition(y=rastgele_dsz$Age,p=.70,list=FALSE)

#testIndis = createDataPartition(y=rastgele$Gender,p=0.30,list=FALSE)

egitim_dan = rastgele_dan[egitimIndis_dan, ]

test_dan = rastgele_dan[-egitimIndis_dan, ]

egitim_dsz = rastgele_dsz[egitimIndis_dsz, ]

test_dsz = rastgele_dsz[-egitimIndis_dsz, ]

#Lineer Regression

lineer_dan = lm(egitim_dan$Overall ~ egitim_dan$Gender,data=egitim_dan)

summary(lineer_dan)

testVeri = as.data.frame(x = 26407)

tahmin = predict(lineer_dan, testVeri, interval = "confidence")

print(round(mean(tahmin)))

sigma(lineer_dan) #RSS==Residual Standart Error gosterir.

AIC(lineer_dan) #Deger dusuk olmalidir.

stepAIC(lineer_dan)

confusionMatrix(lineer_dan)

plot(resid(lineer_dan)) #Kalintirlari cizdirmek icin

#Logistic Regression

logistic_dan = glm(egitim_dan$Overall ~ egitim_dan$Gender, data=egitim_dan, family = "Gamma")

summary(logistic_dan)

colnames(test_dan)

tahminSinif = predict(logistic_dan, type="response", newdata=test_dan[,-11])

tahmin = table(tahminSinif, egitim_dan$Overall, dnn= c("Tahmin", "Gercek"))

hata = mean(tahminSinif != test_dan$Overall)

hata = round(hata, 2)

print(hata)

sigma(logistic_dan)

AIC(logistic_dan)

stepAIC(logistic_dan)

plot(resid(logistic_dan)) #Kalintirlari cizdirmek icin

#Multiple Regression

multiple_dan = lm(egitim_dan$Overall~egitim_dan$Gender + egitim_dan$Division + egitim_dan$Age ,data=egitim_dan)

summary(multiple_dan)

sigma(multiple_dan)

AIC(multiple_dan)

stepAIC(multiple_dan)

plot(resid(multiple_dan))

#Random Forest

randomFor_dan = randomForest(x = egitim_dan[-12],y = egitim_dan$Overall,ntree = 50)

summary(randomFor_dan)

tahmin = predict(randomFor_dan, newdata = as.data.frame(test_dan[-12]))

sigma(randomFor_dan)

#SVR

svr_dan = svm( egitim_dan$Overall~ egitim_dan$Gender + egitim_dan$Division + egitim_dan$Age, data = egitim_dan, type='eps-regression')

summary(svr_dan)

tahmin = predict(svr_dan, newdata = as.data.frame(test_dan[-12]))

plot(resid(svr_dan))

head(tahmin)

#Algoritmaları karşılaştıracağız.

plot(lineer_dan)

plot(logistic_dan)

plot(multiple_dan)

plot(randomFor_dan)

plot(svr_dan)
