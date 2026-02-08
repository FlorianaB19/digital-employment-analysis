setwd("C:/Users/flory/Desktop/ADSEMINAR_FINAL")

database <- read.csv("DATEutile.csv", header = TRUE, sep = ",")

head(database)
summary(database)

#install.packages("corrplot")
#install.packages("factoextra")
#install.packages("FactoMineR")
#install.packages("ca")
#install.packages(c("ppcor","moments","tseries","car"))
#install.packages("psych")
#install.packages("nFactors")
#install.packages("GPArotation")
#install.packages("cluster")
#install.packages("NbClust")

library(moments)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(ca)
library(car)
library(tseries)
library(psych)
library(nFactors)
library(GPArotation)
library(cluster)
library(NbClust)

sk<-apply(database[,-c(1)],2,skewness)
sk

#--------------
# X1: EMPLOYEED RATE 
# X3: CAPACITATE DE ADAPTARE 
# X4: FRECVENTA CELOR CARE FOLOSESC NET
# X5: LONG TERM UNEMPLOYEED
# X6: SOMAJ
# X7: ANGAJATI IN IT 
# x8: FIRME CARE FOLOSESC NET 
# X10: AUTOMATIZARE
# X11: BIG DATA 
# X12: ECON DIG


#se observa ca va trebui sa standardizam datele deoarece sunt diferente mari 
vars_pastrate<-c("REGIUNE","X1","X3","X4","X5","X6","X7","X8","X10","X11","X12")
database_sel <- database[, vars_pastrate]

# variabilele standardizate
vars_standardizate<-c("X1","X3","X4","X5","X6","X7","X8","X10","X11","X12")

# standardizarea (media= 0, abaterea= 1)
database_sel[, vars_standardizate]<-scale(database_sel[, vars_standardizate])
database_final <- database_sel

head(database_final)

k<-apply(database_final[,-c(1)],2,kurtosis)
k
medii<-apply(database_final[,-c(1)],2,mean)
medii
abateri<-apply(database_final[,-c(1)],2,sd)
abateri # abateriea este 1 
q<-apply(database_final[,-c(1)],2,quantile)
q
sk<-apply(database_final[,-c(1)],2,skewness)
sk  # sk X1 este pozitiv deci cateva regiuni au rate ale angajarii mult peste media UE 
# 75% din regiuni au valori sub 0.44 deci majoritatea UE este sub media generala a angajarii
# max= 2.04 regiune cu ocupatie exceptional de mare si min=-1.38 regiune cu ocupatie f scazuta 
# deci UE este poalrizata: cateva regiuni sunt foarte performaante, restul au rate mici si medii ale angajarii

#sk X4 este negativ => exista regiuni cu frecventa foarte scazuta a utilizarii internetului 
# majoritatea sunt valori peste mediana
# min = -2.81 regiune cu acces scazut la internet si max=1.51 regiuni foarte active digital 

# sk X5 este foarte pozitiv si coada e foarte lunga la dreapta => regiuni cu somaj lung foarte ridicat 
# 75% din regiuni au valori sub medie deci majoritatea UE sta bine 


#sk X8 este foarte negativ  => cateva regiuni au valori extrem de mici in ceea ce priveste digitalizarea in companii
# mediana= 0.374 un nivel bun, max=0.744 si min=-3.23 deci regiune slab digitalizata
#majoritatea regiunilor sunt modernizate, dar unele raman slab in urma.

boxplot(database_final$X1, col="green3", horizontal=TRUE,
        main="Boxplot EMPLOYEED RATE")
boxplot.stats(database_final$X1)$out

hist(database_final$X1, col="orange2",
     main="Histograma EMPLOYEED RATE")
#nu se observa valori outlieri in boxplot
#in histograma sunt predominant valori mici, asimetrie la stanga, coada lunga la dreapta


boxplot(database_final$X3, col="blue3", horizontal=TRUE,
        main="Boxplot ADAPTABILITY")
boxplot.stats(database_final$X1)$out
hist(database_final$X3, col="yellow2",
     main="Histograma ADAPTABILITY")
# nu se observa valori outlieri in boxplot
#in histograma sunt predominant valori mici, asimetrie la stanga, coada lunga la dreapta


# recapitulare test ACP 
matrice_Corelatie<-cor(database_final[,c(-1)])
matrice_Corelatie



corrplot(matrice_Corelatie,method="number",type=("full"),title="corel", bg="white")
#matricea de corelatie arata cat de bine se coreleaza datele intre ele
# -1 corel negativa ( una creste alta scade )
# +1 corel pozitiva (amandoua evolueaza in acelasi sens) 
#ex: X1 si X5 cand rata somajului creste long term unemploymeed scade
#X5 si X3 au o corel de 0.88 (puternica) deci somerii pe termen lung se adapteaza situatiei 
#X1 si X11 AU O COREL NEGATIVA cand rata somajului creste valoarea big data scade      
#X3 si X7 au o corelatie pozitiva(0.76) se influenteaza una pe alta => forta de munca specialziata in it
#X10 si X1: automatizarea nu reduce angajarea, poate chiar sa stimuelze 
#X8 si X1 corelatie pozitiva, deci daca firmele sunt digitalizate atunci angajarea creste                
#X5 si X6 unde rata somaj este mare creste si somajul pe termen lung 


#ANALIZA COMPONENTELOR PRINCIPALE:sintetizeaza informatia prin reducerea dimensionalitatii 
                                   #si eliminarea redundantei informationale

#matricea de covarianta
matrice_Covarianta<-scale(database_final[,c(-1)],center=T,scale=F) #scale=F deci nu impartim la sd
COV<-cov(matrice_Covarianta)
COV # pe diagonala principala = variantele variabilelor care trebuie sa fie = 1 pt datele standardizate
# rX3,X5 aprox 0.88 indica o corelatie pozitiva ( cand capacitatea de adapare creste, nr de angajatii in IT creste  )
# rX1,X3 aproximativ -0.25 indica o corelatie negativ slaba

acp<-princomp(database_final[,c(-1)],cor=F,scores=TRUE) # 10 variabile si 63 de observatii 
summary(acp)
#output pt Comp1: standard deviation este 1.73, deci lambda1= 1.73^2
#       pt Comp2: standard deviation este 1.47, deci lambda2 = 1.47^2
#       pt Comp3: standard deviation este 1.04, deci lambda3 = 1.04^2
#       pt Comp4: standard deviation este 1.003, deci lambda4 = 1.003^2
# Comp1 explica 30.01% din varianta totala 
# Comp2 explica 21.78% din varianta totala
# Comp3 explica 10.89% din variatia totala
# Comp4 explica 10.06% din varianta totala
# Primele doua componente (Comp1, Comp2) explica impreuna 51.79% din varianta totala
# Primele patru componente explica impreuna 72.27% din varianta totala
# Primele sase componente explica 90.70% din varianta totala
# proportia este = lambdaj/ suma lambdaj 


head(acp)
acp$loadings #avem vectorii proprii cu care construim CP / coeficientii variabilelor
# SS LOADINGS = 1 
#pentru cp1=0.176⋅X1−0.541⋅X3−0.158⋅X4−0.521⋅X5−0.387⋅X6−0.430⋅X7−0.163⋅X11
#pentru cp2=0.578X2+0X3+0.460X4+(-0.110)X6+....+(-0.382)X11+0X12
#din acp rezulta pt componenta 1: valoarea lambda este 1.73, proportia de varianta= 0.30 = lambda/suma de lambda ori 100
#cumulative proportion: informatia cumulata de primele n CP, ex p3 se interpreteaza: primele 3 CP contin 72,75% din informatia totala


#matricea de cov se descompune:
E<-eigen(COV)
lambda<-E$values # valorile proprii cu care putem scrie CP
V<-E$vectors # matricea vectorilor proprii 
V


acp1<-princomp(database_final[,c(-1)],cor=T,scores=TRUE) #determinam scorurile 
summary(acp1) 
scoruri<-acp1$scores #scores = valorile observatiilor pentru fiecare componenta principala
head(scoruri) #fiecare linie este o regiune si fiecare coloana este contributia acelei regiuni pe o componenta
coef<-acp1$loadings #vectorii proprii 
#exemplu: 
# Z1scores=0.176(0.44362327)−0.541(−1.477418735)−0.158(−3.16263243)−0.521(−4.12074446)−0.387(−1.16516939)−0.430(−3.155866751)−0.163(−0.139438909)
# comp1 = dimensiunea digitalizare si piata muncii digitala
# comp2 = dimensiunea tehnologie avansata si angajare inalta 
# comp 3 si 4 explica alte elem secundare 
# observatia 4 este puternic caracterizata de structura data de comp1 
# scor pozitiv mare pe o componenta atunci regiuni cu valori ridicate pt variabile care definesc acea componenta
#regiunile 2,3,4,5=6 au valori pozitive in CP1 iar regiunea 5 are scor negativ sub media esantionului pe dim aia




V[,1] 
# X1: 0.176 (rata somajului) contributie pozitiva relativ mica 
# X2: -0.561 (capacitatea de adapatare) contributie negativa majora 
# X3: -0.158 (frecventa celor care folosesc net) contributie negativa relativ mica 
#Comp1 = 0.175·X1 − 0.540·X3 − 0.158·X4 − 0.521·X5 − 0.387·X6 − 0.430·X7 − 0.097·X8 − 0.018·X10 − 0.163·X11 − 0.093·X12
#Comp1 masoara problemele pe piata muncii si aa digitalizarii 


sqrt(sum(V[,1]^2))
norme<-apply(V, 2, function(V) sqrt(sum(V^2)))
norme<-round(norme,3)
norme
descompunere<-V%*%diag(lambda)%*%t(V)
#norma unui vector=lungimea lui
#norma vectorului propriu=1 deci vectorii sunt unitari cu baza ortonormata 



#Care este legătura dintre valorile cu abaterile standard de mai sus și valorile Lambda?
#var(z)=lambda deci abatere std= radical din lambda
#sd_values <- sqrt(Lambda)
#lambda <- e$values
#abatere standard <- sqrt(val proprie)

#Ce sunt loadings? Comparati cu V? Ce observati?
acp$loadings
round(V,3)
# Un vector propriu al unei matrici înmulțit cu o constanta este tot vector propriu pentru acea matrice.
# loadings = coeficientii vectorilor proprii si arata cat de mult contribuie fiecare var la formarea CP(corelatii intre componente si variabila)





# SEMINAR 4 ---------------------------------------------------------------

acp2<-princomp(database_final[,c(-1)],cor=T,scores=TRUE)
summary(acp2)

#legatura valorilor cu abaterile standard de mai sus și valorile Lambda
sdev<-acp2$sdev                     # deviațiile standard ale componentelor principale
lambda2<-sdev^2                     # valorile proprii (varianțele componentelor)
round(data.frame(sdev=sdev,lambda=lambda2), 4)
sqrt(lambda2)

C <- cor(database_final[,-1])
E2 <- eigen(C)
V<-round(E2$vectors, 3)                  # vectorii proprii ai matricei de corelație
lambda<-round(E2$values, 3)  # corelatiile variabilei X1 (rata angajarii) cu CPi 

#matricea factor
MF<-cor(database_final[,c(-1)], scoruri)  
round(MF, 4)
# fiecare rand = variabila si coloana = CP
# fiecare elem arata cat de bine este corelata variabila cu comp resp
# Valorile mari (|r| > 0.5) = variabila este bine reprezentata pe componenta respectiva
# Valorile mici (|r| < 0.3) = componenta nu explica acea variabila
# pt comp1 cele mai mari corelatii sunt cu X3,X5,X7,X6,X4,X8 => piata muncii este problematica 
# scor poz regiuni cu adaptabilitate buna si somaj redus / scor negativ regiuni cu probleme mari in somaj 
# pt comp2 cele mai mari corelatii sunt cu X1,X4,X8,X10,X11 deci economie digitala de baza + angajare 
# scor poz regiuni moderne cu angajare ridicata si digitalizare buna /scor negativ regiuni cu big data ridicat dar fara angajare 
# comp3 separa adoptarea tehnologiilor avansate de digitalizarea clasica, diferente intre economii digitalizate superficial vs. profund
# comp 4 indica cresterea digitalizarii 
# comp 5 specializare it(multi angajati in it) vs somaj scazut: eficienta pietei muncii modernizate 
# comp 6 componenta utilizatorului avansat cu x4=0.526 si x11=0.4876
# comp 7 digitalizarea la nivel de business x8= -0.3985
# comp 8-10 sunt variabilele zgomot unde acestea incep sa scada sub 0.35 




biplot(acp2)
# reprez regiunilor in spatiu cu 2 dim
# punctele negre sunt regiunile in spatiul componentelor principale 
# sageti scurte informatie dispersata, sageti luni variabile bine explicate

# variabilele X4,X13,X7,X10 sunt bine corelate cu axele X si Y 
# sagetile scurte X6,X11 sunt slab reprezentate si variante lor este explicata de alte componente
# vectorii X4,X1,X10 sunt in dreapta deci corelatie pozitiva pentru Comp1 
# vectorii X3,X7,X6,X11 sunt negativ corelate cu Comp1 
# vectorii de sus X4,X10,X1 sunt poizitivi corelati cu Comp2
# vectorii in jos X11 sunt negativi corelati cu Comp2 
# unghiul dintre X4 si X10 indica o corelatie puternic pozitiva 
# unghiul obtuz dintre X3 si X4 indica o necorelatie 
# unghiul drept dintre X7 si X11 indica faptul ca variabilele sunt negativ corelate 

# comp 1 separa somajul si vulnerabilitatea pietei muncii de adaptare si digitalizare 
# comp 2 surprinde utilizarea internetului si tehnologiile avansate digital




acp3<-PCA(database_final[,c(-1)]) 
# Dim 1 = piata muncii si capacitatea de adaptare 
# Dim 2 = digitalizare si ocupare
# comp4: x12 are o val f mare de =0.8163 care masoara nivelul de dezvoltare a economiei digitale
# comp5: x6 si x7 eficienta structurala, unde sectorul it e dezvoltat rata somaj tinde sa fie mica 

# prima axa Dim1 explica 30.01% din varianta totala, iar a doua axa Dim2 explica 21.78% 
# cercul are raza r=1 si masoara corelatia perfecta 
# sageti aproape de cerc (X3, X5): Reprezentare excelenta, varianta lor este aproape integral explicata de Dim 1 si Dim 2.

# Comp1 (Pozitiv): X3, X5, X6, X7 au corelatie puternica pozitiva cu Comp1
# Comp2 (Pozitiv): X1, X4, X8, X10 au corelatie puternica pozitiva cu Comp2
# unghi mic atunci variabile corelate pozitiv, unghi drept atunci variabile necorelate, unghi obtuz atunci corelatie negativa



acp3$eig # valorile lambda, proportia de valoare pentru fiecare variabila si cumulativa
summary(acp3) 
# contributia variabilei X1 la Dim1 este de 3.097 
# Comp 1 (piata muncii si adaptare) are variabile dominante X3,X5,X7,X6 deci adaptare vs somaj pe termen lung 
# Comp 2 (digitalizare si utilizare internet) reflecta nivel ocuparii si digitalizarea firmelor
# Comp 3 se axeaza pe gradul de tehnologie avansata si digitalizare la nivel de firma si economie 
# primele 4 componente descriu peste 70% din variatia totala
# cele mai bine reprezentate observatii sunt 7,8,9,10
# cos2 >0.7 pe una din axe pentru X3,X5,X1,X4 


print(fviz_eig(acp3)) 
# scre plot este un criteriu in care det numarul de componente principale pe care le pastram, aici 3 CP
# Kaiser: alegem atatea componente principale cate au lambda>1 si avem 3 lambda mai mari decat 1 => 3 CP

print(fviz_pca_ind(acp3)) 
# observam un grup dens in partea stanga in jurul valorilor -1 si 1 
# regiunile apropiate au profil asemanatoare pe variabilele analizate iar cele departate sunt diferite(6 sau 42 si 23 sau 19)
# 19, 23,24,26 sunt departate de restul si pot fi regiuni cu profil unic,grupuri separate,valori atipice 
#sus dreapta valori mari pe ambele componente
#sus stanga valori mari pe dim2 dar valori mici pe dim1
#jos stanga valori mici pe ambele componente
#jos dreapta valori mici pe dim2 si valori mari pe dim1 

# liderii digitali si de performanta (val poz) sunt x1,x4 si x7 pe comp1=>regiuni dezvoltate(centre urbane)
# val neg pe comp1 x3,x5,x6 

print(fviz_pca_var(acp3, col.var = "contrib"))
#sagetile reprezinta variabile si lungimea arata cat de mult contribuie la axa PCA 
# X5, X3, X7 sunt aproximativ in aceeasi directie deci puternic corelate pozitiv
# X1 sus stanga si X11 jos dreapta sunt puternic corelate negativ
# X1 si X3 foarte slab corelate ( employeed rate si capacitatea de adaptare nu se influenteaza prea mult)
# X3, X5, X7, X6 explica cel mai mult variatia pe Dim1.
# X1 puternic pozitiv,X11 puternic negativ 

# dim1 (X3...X8,X12) avem DIGITALIZARE SI PIATA MUNICII DIGITALA: 
                   #digitalizarea nu reduce somajul, unele regiuni pot avea tranzitii dificile pe piata muncii
# dim 2 (X1,X10,X11) DENSITATEA TEHNOLOGICA AVANSATA: zonele avansate tehnologic pe big data pot avea 
                                                      #sectoare economice dar nu neaparat angajare masiva 
# somajul merge in aceeasi directie cu digtializarea (digitalizarea poate creste somajul in unele regiuni)






# SEMINAR 7 & 8 ---------------------------------------------------------------
# ANALIZA CORESPONDENTELOR: studiaza corelatia dintre doua variabile categoriale

# X1: EMPLOYEED RATE 
# X7: ANGAJATI IN IT 

dim(database_final[,-c(1)])
Er <- rep(0,145)
Ait <- rep(0,145)

#CUANTILE
q1 <- quantile(database_final$X1,probs=c(0.33,0.66,0.99))
q1  
# cuantilele pentru probs 33% , 66% , 99% 
# 33% din regiuni au o rata de angajare sub media europeana
# 66% au o rata de angajare  foarte aproape de media europeana
# 99% : doar 1% din regiuni au o rata de angajare peste media europeana
# majoritatea Europei este sub media UE la angajare, doar cateva regiuni au angajare exceptional de mare

q2 <- quantile(database_final$X7,probs=c(0.33,0.66,0.99))
q2  
# o treime din regiuni au foarte putini angajati in it=> mult sub media europeana 
# doua treimi din regiuni au valori apropiate sau putin sub media europeana
# doar 1% din regiuni au multi angajati in it 
# Europa este polarizata puternic cu 1% din regiuni super performante, in care domeniul IT nu e distribuit uniform


Er[database_final$X1<=q1[1]] <- "Er_P"  #categoria de jos/low 
                               # 52 de cazuri sunt de categorie mica
Er[database_final$X1>q1[1] & database_final$X1<=q1[2]] <- "Er_Mij" #categoria de mijloc/ middle
                                                          # 44 cazuri sunt de categorie mijlocie
Er[database_final$X1>q1[2] ] <- "Er_M"  # categoria mare/high 
                             # 49 cazuri sunt de categorie mare 
table(Er)


Ait[database_final$X7<=q2[1]]<-"Ait_Mic"
Ait[database_final$X7>q2[1]&database_final$X7<=q2[2]]<-"Ait_Mij"
Ait[database_final$X7>q2[2]]<-"Ait_Mare"
table(Ait)

contingenta <- table(Er,Ait) # tabelul care arata corelatiile dintre cele doua variabile 
contingenta 
# Er impartita la trei niveuri in fd X1 si Ait impartita la trei niveluri in fd X7
# categ Er_Mij are cele mai multe cazuri in Ait_Mij = corelare pe mijloc 
# categ Er_M se imparte in doua cazuri 21 Ait_M si 24 in Ait_Mij = nu avem tendita clara
# cei din mijlocul Er tind sa fie la mijloc Ait iar extremele Er sa fie si extremele Ait 
# regiunile slabe la angajare tind sa fie slabe si la it 
# regiunile medii tind sa fie mai puternice in it 
# 18+18+13 din regiuni au rata angajarii = 49 indiferent de cati angajati sunt in IT

X2 <- chisq.test(contingenta)
X2 # p-value f mic < 0.05 => resp H0: Er si Ait nu sunt independente=>  exista asociere intre variabilele Er si Ait
# chi patrat 27.121 pt df=4=(3-1)(3-1) arata ca 
          #diferentele dintre valorile observate si cele asteptate sunt consistente si nu se datoreaza fluctuatiei aleatoare 


#observed= frecvente observate 
X2$observed # valorile din date inainte ca chi patrat sa calculeze valorile asteptate
             # cate pers sunt simultan intr o categ Er si in Ait 
             # ex: 21 cazuri sunt in Er_M si Ait_M , 13 in Er_P si Ait_Mij 


# expected= frecvente asteptate 
round(X2$expected,3)  # valorile care ar trebui sa apara daca Er si Ait ar fi independente 



ac <- ca(contingenta)

plot(ac,main="Analiza componentelor pt  cuantile")
# dimensiunea 1 explica aproape toata varianta 87.2% , iar dim 2 explica 12,6%
# partea dreapta: Er_P si Ait_Mic => regiuni cu angajare mica tind sa aiba si putini angajati it
# partea stanga: Ait_Mare si Er_Mij => regiunile cu angajare medie tind sa aiba multi angajati in it 
# centru: Er_M si Ait_Mij=> regiunile cu angajare mare au profil it divers 

summary(ac) 
# calitatea reprezentarii este data de qlt care trebuie sa fie aproape de 1  si ki sunt coordonatele 
# inertia = varianta explicata de fiecare linie/coloana (inertia pe linii e mai mica decat cea pe coloane)
# dim1 explica 87.2% din inertia totala si dim 2 12.8% (prima dimensiune e mai dominanta)
# Er_M este o categorie importanta si foarte bine reprezentata si definita pe axa 2, nu contribuie la axa1 pt ca ctr = 1
# Er_Mij defineste axa1, este excelent reprezentata si contributia =564 
# Ait_Mj defineste axa2 
# Categoria At_Mc contribuie cel mai mult la prima dimensiune
# asocieri intre Er_Mj si At_Mr in zona negativa a axei 1 

#DECILE
d1 <- quantile(database_final$X1,probs=c(0.25,0.5,0.75,1))
d1   # decilele la probs 25% , 50% , 75% si 100% 
d2 <- quantile(database_final$X5,probs=c(0.25,0.5,0.75,1))
d2 
Er1 <- rep(0,145)
Ait1 <- rep(0,145)

Er1[database_final$X1<=d1[1]] <- "Er_Redus"
Er1[database_final$X1>d1[1] & database_final$X1<=d1[2]] <- "Er_Mediu"
Er1[database_final$X1>d1[2]& database_final$X1<=d1[3]] <-"Er_Ridicat"
Er1[database_final$X1>d1[3]]<- "Er_FTR"
table(Er1)

Ait1[database_final$X7<=d2[1]] <- "Ait_Redus"
Ait1[database_final$X7>d2[1] & database_final$X7<=d2[2]] <- "Ait_Mediu"
Ait1[database_final$X7>d2[2]& database_final$X7<=d2[3]] <-"Ait_Ridicat"
Ait1[database_final$X7>d2[3] ] <- "Ait_FTR"
table(Ait1)

contingenta1 <- table(Er1,Ait1)
contingenta1 

Hipatrat1 <- chisq.test(contingenta1)
Hipatrat1 # p-value f mic < 0.05 => resp H0: Er si Ait nu sunt independente=>  exista asociere intre variabilele Er si Ait
# chi patrat 27.121 pt df=4=(3-1)(3-1) arata ca diferentele dintre valorile observate si cele asteptate sunt consistente si nu se datoreaza fluctuatiei aleatoare 


Hipatrat1$observed
round(Hipatrat1$expected,3) 


ac1 <- ca(contingenta1)


plot(ac1,main="Analiza componentelor pt  decile")
summary(ac1)
# Er_Mediu este asociat cu Ait_Mare (regiuni cu angajare medie, dar IT foarte dezvoltat)
# Dimesniunea 2 evidentiaza polarizarea pietei
# extremele sunt vizibile si structura e mai dispersata 


#par(mfrow = c(1,2))
plot(ac, main = "Analiza componentelor pt cuantile ")
plot(ac1, main = "Analiza componentelor pt decile ")
# categ Er sunt reprezentate de punctele albastre iar pt categ Ait de triunghiuri rosii
# avem 2 dimensiuni:
# dim1 explica 99% din variatie deci aproape tot ce vedem pe orizontala este important
# dim2 explica doar 0.06% din variatie deci verticala nu conteaza deloc
# stanga neg: Er_M (valori mari la Er) se asociaza cu Ait_P
# centru: categ sunt relativ aproape
# dreapta poz: categ de mijloc se asociaza intre ele
# Er_Mij merge cu Ait_Mij de aia sunt in dreapta aproape unul de altul. 
# dif intre cuantile si decile esste distanta usor compacta la decile. 



date_fa<-database[,c(-1)]
Matrice_R<-cor(date_fa)
KMO(date_fa)
kmo_rezultat<-KMO(Matrice_R) #masura in care datele mele sunt potrivite pentru analiza
kmo_rezultat # aceste date au obtinut o valoare extrem de slaba
#0.00 to 0.49 inacceptabil
#0.50 to 0.59 extrem de slab
#0.60 to 0.69 mediocru
#0.70 to 0.79 acceptabil
#0.80 to 0.89 foarte bine
#0.90 to 1.00 excelent
# acest set de date, sunt extrem de slabe 


# Testul Bartlett
# H0: Variabilele nu sunt corelate (matricea este identitate)
# H1: Variabilele sunt corelate (putem face analiza)

bartlett_test <- cortest.bartlett(Matrice_R, n = nrow(database))
print(bartlett_test) # df=n(n-1)/2 , n=10 var
# chisq = 299.3368 =: corel puternice in datele alese
# chisq masoara daca matricea de corel difera mult fd o matrice identitate 
# p-value<0.05 => resping h0 deci exista variabile corelate




# SEMINAR 9 : ANALIZA FACTORIALA ----------------

#cerinta 1
x<-database$X3 # ADAPTABILITY
y<-database$X5 #long term unemploymeed
z<-database$X7 #ANGAJATI IT 

# resizuri= diferenta dintre val reala si cea prezisa de model
# val negativa at valoarea reala este mai mica decat cea prezisa (X7 a supraestimat variabila)
# val pozitiva at valoarea reala este mai mare decat cea prezisa (X7 a subestimat variabila)
# reziduuri apropiate de 0 indica predictie buna  

model_xz<-lm(x ~ z)
model_xz
# Cat din Adaptare se explica prin IT
# Exista o relatie liniara pozitiva intre z si x: cresterea lui z conduce la cresterea lui x
# Valoarea lui x este 459.41 atunci cand z este zero.
# daca z creste cu o unitate, variabila x creste in medie cu 17.69 unitati

rez_x<-resid(model_xz) 
rez_x
# rez_x = partea din adaptabilitate care NU poate fi explicata prin angajati it 
# valori negative mari deci avem regiuni unde nivelul de adaptare este mai mic decat ne am astepta dupa nivelul IT
# valori pozitive mari deci regiuni unde adaptarea este mult mai amre decat explica nivelul de IT


model_yz<-lm(y ~ z) # Cat din Somaj lung se explica prin IT
# Valoarea lui x este 9.7513 atunci cand z este zero
# daca z creste cu o unitate, variabila x creste in medie cu 0.4832 unitati
rez_y<-resid(model_yz) # ez_y = partea din somajul lung care NU se explica prin IT

cor_partiala<-cor(rez_x,rez_y)
cor_partiala
cor(x,y)     #relatie puternica, aproape perfecta intre adaptare si somaj lung
cor_partiala #relatie puternica si fara influenta it, corelatia dintre x si y dupa eliminarea influentei variabilei z 
             # angajatii in it X7 explica doar o mica parte din relatia dintre capacitatea de adaptare X3 si LTU X5 


#met 3
vars_metoda3<-c("X3", "X5", "X7")
df_m3<-database[,vars_metoda3]
R<-cor(df_m3) #matricea de corelatie cu diag prin = 1 
print(round(R, 3))
P<-solve(R) #inversa matricii, matricea de precizie 
# X3= 8.35 dependenta mare, cel mai puternic legata de cele doua variabile
# X7= 2.71 cea mai mica dependenta, cea mai independenta variabila 
# elem negative at corelatie partiala negative, elem pozitive at corelatie partiala pozitiva 
# X5 si X7 relatie partiala negativa, X3 si X7 dependenta pozitiva moderata dupa controlul lui X5 
print(round(P, 3))
numarator<- -P[1, 2]
numitor<-sqrt(P[1, 1] * P[2, 2])
cor_partiala_m3<-numarator / numitor


date_part<-database[,c("X3", "X5", "X7")]
P<-solve(cor(date_part))
cor_part<- -P[1, 2]/sqrt(P[1, 1] * P[2, 2])
var_analizata<-database_final$X5

#par(mfrow=c(1, 2))
hist(var_analizata, prob=TRUE,col="lightgrey", main="Histograma X5")
lines(density(var_analizata),col="blue", lwd=2)
qqPlot(var_analizata, main="QQ-Plot X5",col.lines="blue", id=FALSE) 
#compara distributia reala a datelor X5 cu o distributie normala teoretica 
# daca somajul pe termen lung ar fi normal distribuit atunci datele ar sta pe diagonala
# in dreapta punctele se ridica  deasupra liniei => outlier extrem si coada superioara este mai lunga decat in dsitributia normala
# asimetrie pozitiva
# in coada stanga sunt punctele sub linie care sunt mai mici decat o distributie normala dar care duc spre asimetrie pozitiva 
# in mijloc punctele sunt ok
# banda albastra arata zona acceptabila pentru normalitate 
# X5 nu urmeaza o distributie normala  
# curba de densitate nu are forma de clopot deci nu avem distributie normala 

#Jarque Bera:
# H0 variabila urmeaza o distributie normala
# H1 variabila NU urmeaza o didtributie normala 
print(jarque.bera.test(var_analizata)) 
# p-value mult mai mic fd 0.05 deci resping ip nula. 
# X-squared = 890.68 statistica testului => asimetrie mare intre date => nu urmeaza distributie normala






fa.parallel(date_fa,fm ="minres",fa="fa",  #fa=factorial analysis 
            main="Parallel Analysis Scree Plot (Alegerea Factorilor)")
# aza y (eigenvalues) cu cat val e mai mare cu atat factorul explica mai multa info din date
# axa x (factor number) este numarul factorului
# triunghiurile&linia albastra fa actual data sunt datele  mele: cata info contine fiecare factor extras din database_final
# linia rosie fa simulated/resampled data este generata articial , "daca datele ar fi aleatoare ce val ar avea factorii?"
# pastram factorii care se afla deasupra liniei rosii deci vom pastra 2 factori  


model_fa1<-fa(date_fa, nfactors=3,rotate = "none",fm = "ml")  #fara rotatie
model_fa1
# model_fa2<-fa(date_fa, nfactors=2,rotate = "varimax",fm = "minres")
print(model_fa1$loadings,cutoff =0.4) 

# ss loadingsi = suma patratelor din MRi
# proportion var este proportia preluata din variatia tot: primul factor explica 0.236=23.6% din variatia totala
# cumulative proportion= proportia cumulativa 
# ss laodings1= (0.939X3)^2+ (0.880)^2+...= 2.799


fa.diagram(model_fa1,main="Analiza Factoriala - Diagrama Legaturilor")
#arata incarcaturile factoriale= cat de puternic se leaga fiecare variabila de factorii extrasi ML1 ML2 ML3 
#valorile sunt intre -1 si 1 
# 0.7-1 leg puternica,  0.4-0.7 leg moderata, 0.3- 0.4 leg slaba, <0.3 irelevanta 
# X3 are contributie foarte mare, X5 foarte mare, X7 puternica, X6 moderata, X12 foarte puternica 
# Factor 1 este WORK DIGITAL , Factor 2 este LONG TERM UNEMPLOYMENT

summary(model_fa1) # peste 0.90 la Tucker Lewis atunci model bun  si RMSEA,BIC trebuie sa fie mic
head(round(cbind(h2=model_fa1$communality,u2 =model_fa1$uniquenesses),3)) 
# h2 (Comunalitate) = cat % din varianta variabilei e explicata de factori
# 0.80–1.00 explicata aproape complet de factori (excelent)
# 0.50–0.80 explicata bine
# 0.30–0.50 explicata slab
# < 0.30 factorii NU explica acea variabila
# X1 explicata aproape complet, X3 foarte bine explicata, X4 foarte slab explicata, X5 excelent explicata, X6 acceptabil
# variabilele cu h2 mari confirma ca factorii sunt bine definiti 
# u2 (Unicitate) = cat % este specific variabilei (neexplicat de factori)
# h2+u2=1 
# h2 1= suma X1^2 ; h2 2= suma X2^2 
# Primul factor (ML1) este dominant si explica 28% din varianta totala, fiind definit in principal de X3, X5 si X7, ceea ce indica o dimensiune legata de adaptarea pietei muncii si structura ocupationala moderna.
# Al doilea factor (ML2) explica 18% din varianta si este asociat mai ales cu X1 si X4, reflectand nivelul de dezvoltare economica si integrarea digitala.
# Al treilea factor (ML3) explica doar 7% din varianta si are o contributie redusa, iar per ansamblu primii doi factori concentreaza 46% din varianta si descriu esenta fenomenului analizat.


scoruri_factori<-model_fa1$scores 
head(scoruri_factori)
round(model_fa1$uniquenesses, 3)




#Seminar 11 ANALIZA CLUSTER 


#crearea setului de date
X_initial <- database[,-1]
X <- database_final[,c(2,3)]
rownames(X) <- 1:nrow(X)
X

km <- kmeans(X,2)   #pasii alg k-means
# se identifica doua clustere cu 47, respectiv 16 observatii 
# pt clusterul 1 X1 este sub medie si X3 usor peste medie iar pentru clustrul 2 rata de angajare este peste medie si capacitate de adaptare sub medie 

clase <- km$cluster # apartenenta la clase
# observatiile 1-6 sunt in clusterul 1, observatiile 8-11 sunt in clusterul 2.. observatiile 40-44 sunt in clusterul 2 

km$centers # centroizii: coordonatele datelor, profilul mediu al fiecarui cluster 
# clusterul 1 grupeaza observatii cu rata angajare mic si capacitate de adaptare relativ mare 
# clusterul 2 cuprinde observatii cu nivel foarte ridicat al variabilei X1, dar cu capacitate de adaptare mai redusa

km$totss # variabilitate totala/ dispersia totala a datelor fata de media generala 
km$withinss # variabilitate intra clasa, daca e mic at grupe compacte 
            # clusterul 1 este omogen, clusterul 2 este eterogen 

km$tot.withinss # variabilitate intra clasa total
                # aproximativ 52.3% din variatia totala este neexplicata de separarea in clustere (64.86373/124 = 0.523)
km$betweenss # diferenta intre clase, daca e mare atunci grupe bine separate 

#analiza cluster in R

d<-dist(X) #matricea distantelor euclidiene intre obiecte
dist(rbind(X[1,],X[4,]),method="euclidian") #distanta euclidiana doar intre observatia 1 si 4 
sqrt((X[1,1]-X[4,1])^2+(X[1,2]-X[4,2])^2)


#metoda celor mai apropiati vecini / METODA SINGLE 
ierarhie2 <- hclust(d,method="single") #complete = dep vec
ierarhie2$merge # etape
ierarhie2$height # distantele unde au fost unite clusterele  
plot(ierarhie2) # dendrograma => doua clustere mari
               # primul contine grupari cu inaltimi miic <0.3 deci este eterogen iar al doilea se uneste cu restul la inaltimi mari
solutie2 <- cutree(ierarhie2,k=2)
# toate regiunile sunt in cluster 1 si regiunile 19,23,26,24 formeaza un cluster2 foarte strans, sunt unite la inaltimi foarte mici 
# si se afla in extrema dreapta => sunt foarte asemanatoare intre ele


#metoda distantelor medii / METODA AVERAGE
ierarhie3 <- hclust(d,method="average")
ierarhie3$merge # etape
ierarhie3$height # distantele de agregare
plot(ierarhie3) # dendrograma
solutie3 <- cutree(ierarhie3,k=2)
# se unesc doua distante pe baza distantei medii, clusterele sunt echilibrate,stabile si fara efect 
# clusterul A cu observatiile 19,23,26,24 se unesc la inaltimi mici si sunt foarte apropiate intre ele
# clusterul B este grupul central cu aproximativ 50 observati, aici se afla mijlocul distributiei.
# clusterul C cu observatia 41 si grupele 7,40,44,13,49,63,61,45,42,48,47,62 



#Seminar 11 
X_initial <- database[,-1]
X <- database_final[,c(2,3)]

dim(X)
names(X)
acp <- princomp(X,cor=T,scores=T) #sau functia PCA
scoruri <- data.frame(acp$scores[,1:2])
names(scoruri) <- c("Z1","Z2")
scoruri



#1. Matricea distantelor
d <- dist(scoruri)
ierarhie <- hclust(d,method="ward.D2") #"single", "complete", "average", "centroid" 
#ward produce dendrograme curate si clustere definite 
#distance: datele mele sunt standardizate deci au interpretare clara 
#number of obj: 63 de observatii




plot(ierarhie)
rect.hclust(ierarhie, k = 3, border = 2:5)
# axa orizontala cele 63 observatii 
# doua clustere mari si clar separate 
# primul cluster 11, 45, 63, 42, 48, 47, 7, 40, 41, 10, 43, 19, 24, 26, 25
# al doilea cluster: 23, 14, 19, 57, 26, 22, 12, 32, 33, 36, 38, 29, 30, 59, 60...
# Aplicarea metodei ierarhice Ward.D2 asupra celor 63 de observatii, folosind distanta euclidiana, a generat o dendrograma cu doua clustere 
# Primul cluster este compact si omogen, cu observatii unite la inaltimi reduse, ceea ce indica o similaritate ridicata intre unitatile componente.
# Al doilea cluster este mai extins, dar se structureaza in subgrupuri interne stabile.
# Daca se adopta un prag inferior , structura se imparte in trei clustere bine definite.
# Metoda Ward asigura minimizarea variantiei interne si obtinerea de grupuri echilibrate si bine separate.



nr <- NbClust(scoruri, distance="euclidean", min.nc=2, max.nc=7, method="ward.D2", index="all")
nr$All.index
nr$Best.nc
# Graficul Dindex arata o scadere continua a valorilor pe masura ce numarul de clustere creste, 
# ceea ce indica faptul ca separarea intre grupuri devine treptat mai putin clara.
# Prima ruptura semnificativa apare intre 2 si 3 clustere, unde Dindex scade puternic, 
# sugerand ca un numar mai mare de 2 clustere poate descrie mai bine structura datelor.
# Graficul diferentelor de ordinul doi (Second differences) evidentiaza un varf pronuntat la 3 clustere, 
# unde se observa cea mai mare schimbare a Dindex-ului.
# Valoarea maxima a diferentelor de ordinul doi pentru 3 clustere indica faptul ca aceasta este solutia 
# optima conform criteriului Dindex.
# Metoda Ward utilizata produce clustere echilibrate 


solutie3 <- cutree(ierarhie,k=3) #apartenenta obiectelor la clase
table(solutie3) # dstructura clusterelor dezechilibrata
aggregate(scoruri, list(solutie3), mean) #centroizii
# Rezultatele metodei ierarhice Ward.D2 cu k = 3 clustere arata o impartire clara a celor 63 de observatii
#in trei grupuri cu marimi diferite: 43 de obs in clusterul 1, 16 obs in clusterul 2 si 4 observatii in cluster3
# Clusterul 1 are centroizii Z1 = -0.285 si Z2 = -0.507, ceea ce indica valori usor sub medie pe ambii factori. 
# Acest grup reprezinta clasa "medie", cu profil echilibrat si fara abateri extreme.
# Clusterul 2 are valori ridicate pentru Z1 = 1.453 si Z2 = 0.831, sugerand ca acest grup include observatii 
# performante sau avansate, pozitionate puternic deasupra mediei pe ambii factori. Este clusterul cu profil dominant pozitiv.
# Clusterul 3 prezinta valori extreme si opuse: Z1 = -2.743 (foarte scazut) si Z2 = 2.122 (foarte ridicat). 
# Acest lucru arata un grup mic, dar foarte diferit
# cluster 1 are regiuni cu nivel moderat de digitalizare, clusterul 2 regiuni performante si clusterul 3 regiuni cu profil dezechilibrat



sol <- hcut(X,k=3,hc_method="centroid")
# Clusterul 1: profil comun, valori scazute, grup numeros si omogen.
# Clusterul 2: valori ridicate pe prima dimensiune, grup diferentiat.
# Clusterul 3: valori foarte ridicate pe a doua dimensiune, grup atipic si izolat.


#Reprezentare solutie
#windows()
 #pentru solutiile extrase cu cutree => 3 clustere 
# Clusterul 1 (rosu) este cel mai numeros si compact, cu valori mai mici ale lui X1 si X3, indicand observatii relativ omogene.
# Clusterul 2 (verde) este bine delimitat, cu valori ridicate ale ratei de angajare si valori moderate–scazute ale capacitatii de adaptare, 
# iar clusterul 3 (albastru) este mic si izolat, caracterizat prin valori foarte mari ale lui X3, ceea ce il diferentiaza clar de celelalte doua grupuri.

#regiunea 6 are cel mai mare scor pe comp1 si un scor negativ pe comp2 unde x11 este neg => regiune cu multi angajati dar cu tehnologii nu toate avansate
# scor neg pe comp 1 ca la regiunea 5 indica regiuni care au somaj ridicat si cap de adaptare scazuta=> zone de investit
#regiunea 1 are scoruri mici deoarece comp4 e def de x12(econ digitala) 

#reprezentare grafic silhouette
s <- silhouette(solutie3,d) #cat de bn se potriveste regiunea intr un cluster 
plot(s)
# Clusterul 2 (16 observatii) este cel mai bine definit, cu o silhouette medie 0.74
# Clusterul 1 (43 observatii) este bine separat (0.69), dar mai eterogen
# Clusterul 3 (4 observatii) are o calitate ceva mai slaba (0.60), fiind mic si mai putin stabil


#Algoritm kmeans
km <- kmeans(scoruri,3)
km
clase <- km$cluster # apartenenta la clase
table(clase)
print(fviz_cluster(km, data=scoruri))
s <- silhouette(clase,d)
plot(s)



km$centers# centroizii
km$totss # variabilitate totala
km$withinss # variabilitate intra clasa
km$tot.withinss # variabilitate intra clasa totala
km$betweenss # variabilitate inter clasa
# clusterul 1 are 16 obiecte cu valori ridicate ale ambelor componente, clusterul doi are 4 cu profil atipic, clusterul 3 are 43 de obiecte cu valori moderate spre scazute  
# Cele trei grupuri sunt definite prin valorile medii (centroizii) ale variabilelor Z1 si Z2 
# Clusterul 1 (centroid: Z1 = 1.45, Z2 = 0.83) cluster cu profil puternic pe prima dimensiune
# Clusterul 2 (centroid: Z1 = -2.74, Z2 = 2.12) cluster izolat , cazuri speciale
# Clusterul 3 (centroid: Z1 = -0.28, Z2 = -0.50 categoria tipica cea mai aropiata de media setului 
# 85.3% din variatia totala este explicata de separarea dintre clustere, ceea ce indica o structura buna si corecta a datelor
# Clusterizarea K-means a identificat: un grup mare, moderat (Cluster 3), un grup puternic pozitiv pe Z1 si Z2 (Cluster 1), un grup foarte mic si atipic, cu valori extreme (Cluster 2)