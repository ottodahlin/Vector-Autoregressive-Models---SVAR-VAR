########################################################################################
# Structural VAR Models - Otto Dahlin
########################################################################################


library(writexl)
library(ggplot2)
library(dplyr)
library(AER)
library(lmtest)
library(tseries)
library(urca)
library(dynlm)
library(sandwich)
library(readxl)
library(forecast)
library(xts)
library(vars)
library(zoo)
library(timeSeries)
library(quantmod)
library(mFilter)
library(seasonal)
library(lubridate)
library(pxweb)
library(CARS)
library(car)

######################################################################################
## VARIABLER:                                                                       ##
##                                                                                  ##
## CCI, FPI, SKULD & REPORÄNTA (FIs HUSHÅLLSMODELL)                                 ##
## CCI, FPI, SKULD, KIX & REPORÄNTA (FIs HUSHÅLLSMODELL - UTÖKAD)                   ##
## RU, FPI, SKULD, KIX & BORÄNTA (FIs HUSHÅLLSMODELL - MODIFIERAD(1))               ##
## ARBL12, FPI, SKULD, KIX & BORÄNTA (FIs HUSHÅLLSMODELL - MODIFIERAD(2))           ##
## BNP, FPI, SKULD, KIX & BORÄNTA (KONVENTIONELL MAKROMODELL)                       ##
######################################################################################
##                                    Läs in data:                                  ##
######################################################################################

FImodell<- read_excel("FIreplikering.xlsx")

#######################################################################################
#
# TIDSSERIE - KONFIDENSINDIKATOR HUSHÃLL (CCI) (INITIALT I MÅNADSDATA) - ALLA
#  
# Hushållens konfidensindikator beräknas som ett genomsnitt av nettotalen
# för de fyra frågorna om den egna OCH svenska ekonomin.
# i nuläget respektive 12 månader framåt, samt frågan om det är förmånligt
# att köpa kapitalvaror nu.
#
# VARIABELNAMN: "CCIHushållenskonfidensindikator" (Original, ej manipulerad)
#
#################################################################################

## Konverterar CCI till en tidsserie. 

## Från 1996Q2 - 2018Q2
CCI_dataframe <- FImodell[,"CCI"]
class(CCI_dataframe)
CCI_monthly<- ts(CCI_dataframe, start=c(1996,1), end = c(2018,6), frequency = 12) # MÅNADSDATA!

is.ts(CCI_monthly)
ts.plot(CCI_monthly, main = "CCI - M
        ", col = "darkgreen",   lwd = 1, ylab = "Utveckling (Nettotal)")

# Månadsvis.
CCI_monthly

## CONVERTING MONTHLY CCI  TO QUARTERLY DATA = ARITMETISKT MEDELVÄRDE.

CCI_serie <- ts(CCI_monthly, end = c(2018,6), frequency = 12)
CCI_serie <- aggregate(CCI_serie, FUN=mean, nfrequency=4)
CCI_serie <- ts(CCI_serie, end = c(2018,2), frequency = 4)


# Framgår Q1, Q2, Q3 & Q4, med start 1996:Q1!
CCI_serie

# FINAL TIDSSERIE "CCI - KVARTALSDATA" 
ts.plot(CCI_serie, main = "CCI - Q", col = "darkgreen",   lwd = 2, ylab = "Change (Nettotal)")


## Konverterar CCI STANDARDISERAD I EXCEL till en tidsserie.

## Från 1996Q1 - 2018Q2

CCI_STAND.dataframe <- FImodell[,"CCISTAND"]
class(CCI_STAND.dataframe)
CCI_STANDmonthly<- ts(CCI_STAND.dataframe, start=c(1996,1), end = c(2018,6), frequency = 12) # MÅNADSDATA!

is.ts(CCI_STANDmonthly)
ts.plot(CCI_STANDmonthly, main = "CCI STANDARDISERAD- M
        ", col = "darkgreen",   lwd = 1, ylab = "STANDARDISERAD")

# Månadsvis.
CCI_STANDmonthly

## CONVERTING MONTHLY CCI  TO QUARTERLY DATA, STANDARDISERAD!
CCI_STAND <- ts(CCI_STANDmonthly,start=c(1996,1), end = c(2018,6))
CCI_STAND <- aggregate(CCI_STANDmonthly, FUN=mean, nfrequency=4)
CCI.STAND_serie<- ts(CCI_STAND, start=c(1996,1), end = c(2018,2), frequency = 4) # KVARTALSDATA
CCI.STAND_serie

# FINAL TIDSSERIE "CCI STANDARDISERAD - KVARTALSDATA" 
ts.plot(CCI.STAND_serie, main ="CCI STANDARDISERAD - Q", lty ="dotted", col ="purple")


# LOGGAD CCI - för att få changes i konsument sentimentet, see Leeper (1992), Bram & Ludvigson etc.
CCI_serie # kvartalsdata
log.CCI_serie <- log(CCI_serie)
log.CCI_serie
plot(log.CCI_serie, main ="LOG CCI - Q", xlab="Change in CCI")


#################################################################################
#
# TIDSSERIE - FASTIGHETSPRISINDEX (FPI) - (REDAN I KVARTALSDATA)
#  
# VARIABELNAMN: "FPI" frÃ¥n FIs dataset: "FIreplikering.xlsx"
#
#################################################################################

## Konverterar FPI till en tidsserie. 
## Från 1996Q2 - 2015Q2
FPI_dataframe <- FImodell[,"FPI"]
class(FPI_dataframe)
FPI_dataframe <- na.omit(FPI_dataframe)
FPI_serie<- ts(FPI_dataframe, end = c(2018,2), frequency = 4)

## Skapar ett objekt fÃ¶r hela serien d.v.s frÃ¥n 1996:Q2 - 2015:Q1
is.ts(FPI_serie)
class(FPI_serie)
ts.plot(FPI_serie, main =" FASTIGHETSPRISINDEX (FPI) - Q", lty="dotted", col ="blue", ylab="Change")

# FramgÃr Q1, Q2, Q3 & Q4, med start 2015Q2!
FPI_serie

# FINAL TIDSSERIE "FPI - KVARTALSDATA" 
ts.plot(FPI_serie, main =" FASTIGHETSPRISINDEX (FPI) - Q", lty="dotted", col ="blue", ylab="Change")


#####################################################################
#
# TIDSSERIE - SKULDKVOT (REDAN I KVARTALSDATA)
#
# VARIABELNAMN: "SKULDKVOT" frÃn FIs dataset: "FIreplikering.xlsx"
#
#####################################################################

## Konverterar SKULDKVOT till en tidsserie.  FrÃ¥n 1996Q2 - 2015Q2

Skuldkvot_dataframe <- FImodell[,"Skuldkvot"]
class(Skuldkvot_dataframe)
Skuldkvot_serie<- ts(Skuldkvot_dataframe, start=c(1996,2), end = c(2018,2), frequency = 4)

## Skapar ett objekt fÃ¶r hela serien d.v.s frÃ¥n 1996:Q2 - 2015:Q1

is.ts(Skuldkvot_serie)
class(Skuldkvot_serie)
ts.plot(Skuldkvot_serie, main =" SKULDKVOT - Q", col ="RED", ylab="Change")


# FramgÃr Q1, Q2, Q3 & Q4, med start 2015Q2!
Skuldkvot_serie


## FINAL TIDSSERIE "SKULDKVOT - KVARTALSDATA" 
ts.plot(Skuldkvot_serie, main =" SKULDKVOT - Q", col ="RED", ylab="Change")

#####################################################################
#
# TIDSSERIE - FÄRDIGSTÄLLDA BOSTÄDER (REDAN I KVARTALSDATA)
#
# VARIABELNAMN: "NYABOSTADER" frÃn FIs dataset: "FIreplikering.xlsx"
#
#####################################################################

## Konverterar NYA BOSTÄDER till en tidsserie.  Från 1996Q2 - 2018Q2

Nyabostader_dataframe <- FImodell[,"nyabostader"]
class(Nyabostader_dataframe)
Nyabostader_serie<- ts(Nyabostader_dataframe, start=c(1996,2), end = c(2018,2), frequency = 4)

## Skapar ett objekt för hela serien d.v.s från 1996:Q2 - 2018:Q1

is.ts(Nyabostader_serie)
class(Nyabostader_serie)
ts.plot(Nyabostader_serie, main =" FÄRDIGSTÄLLDA BOSTÄDER - Q", col ="RED", ylab="Change")


# Framgår Q1, Q2, Q3 & Q4, med slut 2018Q2!
Nyabostader_serie


## FINAL TIDSSERIE "SKULDKVOT - KVARTALSDATA" 
ts.plot(Nyabostader_serie, main =" FÄRDIGSTÄLLDA BOSTÄDER  - Q", col ="RED", ylab="Change")


#####################################################################
#
# TIDSSERIE - BNP (REDAN I KVARTALSDATA) I MKR
#
# VARIABELNAMN: "BNP" frÃn FIs dataset: "FIreplikering.xlsx"
#
#####################################################################


## Konverterar BNP till en tidsserie.  Från 1996Q2 - 2015Q2

BNP_dataframe <- FImodell[,"BNP"]
class(BNP_dataframe )
BNP_dataframe <- na.omit(BNP_dataframe)
BNP_serie<- ts(BNP_dataframe , end = c(2018,2), frequency = 4)

## Skapar ett objekt för hela serien d.v.s från 1996:Q2 - 2015:Q1

is.ts(BNP_serie)
class(BNP_serie)
ts.plot(BNP_serie, main =" BNP - Q", col ="RED", ylab="Change")


# Framgår Q1, Q2, Q3 & Q4, med start 2015Q2!
BNP_serie


## FINAL TIDSSERIE "SKULDKVOT - KVARTALSDATA" 
ts.plot(BNP_serie, main ="BNP  - Q", col ="RED", ylab="Change")


#####################################################################
#
# TIDSSERIE - ARBL (REDAN I KVARTALSDATA) 
#
# VARIABELNAMN: "ARBL" frÃn FIs dataset: "FIreplikering.xlsx"
#
#####################################################################

## Konverterar BNP till en tidsserie.  FrÃ¥n 1996Q2 - 2015Q2

ARBL_dataframe <- FImodell[,"ARBL"]
class(ARBL_dataframe )
ARBL_serie<- ts(ARBL_dataframe , start=c(1996,2), end = c(2018,2), frequency = 4)

## Skapar ett objekt fÃ¶r hela serien d.v.s frÃ¥n 1996:Q2 - 2015:Q1

is.ts(ARBL_serie)
class(ARBL_serie)
ts.plot(ARBL_serie, main =" ARBL - Q", col ="RED", ylab="Change")


# FramgÃ¥r Q1, Q2, Q3 & Q4, med start 2015Q2!
ARBL_serie


## FINAL TIDSSERIE "SKULDKVOT - KVARTALSDATA" 
ts.plot(ARBL_serie, main ="BNP  - Q", col ="RED", ylab="Change")


#####################################################################
#
# TIDSSERIE - KIX (REDAN I KVARTALSDATA) 
#
# VARIABELNAMN: "KIX" frÃn FIs dataset: "FIreplikering.xlsx"
#
#####################################################################

## Konverterar BNP till en tidsserie.  FrÃ¥n 1996Q2 - 2015Q2

KIX_dataframe <- FImodell[,"KIX"]
class(KIX_dataframe )
KIX_serie<- ts(KIX_dataframe , start=c(1996,2), end = c(2018,2), frequency = 4)

## Skapar ett objekt fÃ¶r hela serien d.v.s frÃ¥n 1996:Q2 - 2015:Q1

is.ts(KIX_serie)
class(KIX_serie)
ts.plot(KIX_serie, main =" KIX - Q", col ="RED", ylab="Change")


# FramgÃ¥r Q1, Q2, Q3 & Q4, med start 2015Q2!
KIX_serie


## FINAL TIDSSERIE "SKULDKVOT - KVARTALSDATA" 
ts.plot(KIX_serie, main ="KIX  - Q", col ="RED", ylab="Change")


#####################################################################
#
# TIDSSERIE - REPORÄNTA (REDAN I KVARTALSDATA) 
#
# VARIABELNAMN: "Reporanta" frÃn FIs dataset: "FIreplikering.xlsx"
#
####################################################################

## Konverterar stibor till en tidsserie.  FrÃ¥n 1996Q2 - 2015Q2

Reporanta_dataframe <- FImodell[,"Reporanta"]
class(Reporanta_dataframe )
Reporanta_serie<- ts(Reporanta_dataframe , start=c(1996,2), end = c(2018,2), frequency = 4)

## Skapar ett objekt fÃ¶r hela serien d.v.s frÃ¥n 1996:Q2 - 2015:Q1

is.ts(Reporanta_serie)
class(Reporanta_serie)
ts.plot(Reporanta_serie, main =" Reporanta- Q", col ="RED", ylab="Change")


# FramgÃ¥r Q1, Q2, Q3 & Q4, med start 2015Q2!
Reporanta_serie


## FINAL TIDSSERIE "SKULDKVOT - KVARTALSDATA" 
ts.plot(Reporanta_serie, main ="Reporanta  - Q", col ="RED", ylab="Change")


########################################################################
#
# TIDSSERIE - RESURSUTNYTTJANDE (REDAN I KVARTALSDATA)
# 
# VARIABELNAMN: "Sysselsattning" frÃ¥n FIs dataset: "FIreplikering.xlsx"
#
########################################################################

## Konverterar BORÄNTA till en tidsserie. 
## FrÃn 1996Q2 - 2015Q2
RU_dataframe <- FImodell[,"RU"]
class(RU_dataframe)
RU_serie<- ts(RU_dataframe, start=c(1996,2), end = c(2018,2), frequency = 4)

## Skapar ett objekt fÃ¶r hela serien d.v.s frÃ¥n 1996:Q2 - 2015:Q1
is.ts(RU_serie)
class(RU_serie)
ts.plot(RU_serie, main ="RESURSUTNYTTJANDEINDIKATORN - Q", col ="orange", ylab="%")

## FramgÃ¥r Q1, Q2, Q3 & Q4, med start 2015Q2!
RU_serie

## FINAL TIDSSERIE "BORÄNTA - KVARTALSDATA" 
ts.plot(RU_serie, main ="RESURSUTNYTTJANDEINDIKATORN - Q", col ="orange", ylab="1000-TAL")


#################################################################################
#
# TIDSSERIE - ARBETSLÖSHET 12 MÅN FRAMÅT (KONJUNKTURBAROMETER)
#   
# VARIABELNAMN: "ARBL12" I NIVÅ, MEN STANDARDISERAD: "ARBL12stand"
#
#################################################################################


## FrÃn 1996Q2 - 2015Q2
ARBL12_dataframe <- FImodell[,"ARBL12"]
class(ARBL12_dataframe)
ARBL12_monthly<- ts(ARBL12_dataframe, start=c(1996,1), end = c(2018,9), frequency = 12) # MÃNADSDATA!

is.ts(ARBL12_monthly)

# MÃNnadsvis.
ARBL12_monthly
ts.plot(ARBL12_monthly, main = "ARBETSLÖSHET 12M FRAM - M")

## CONVERTING MONTHLY ARBL12  TO QUARTERLY DATA = ARITMETISKT MEDELVÄRDE.

ARBL12_serie <- ts(ARBL12_monthly, start=c(1996,1), end = c(2015,2), frequency = 12)
ARBL12_serie <- aggregate(ARBL12_serie, FUN=mean, nfrequency=4)
ARBL12_serie<- ts(ARBL12_serie, start = c(1996,1), end = c(2018,3), frequency = 4)


# FramgÃr Q1, Q2, Q3 & Q4, med start 2015Q2!
ARBL12_serie

# FINAL TIDSSERIE "ARBL12 - KVARTALSDATA" 
ts.plot(ARBL12_serie, main = "ARBETSLÖSHET 12M FRAM - Q", col = "darkgreen")

## Konverterar ARBL12 STANDARDISERAD I EXCEL till en tidsserie.

ARBL12_STAND.dataframe <- FImodell[,"ARBL12stand"]
class(ARBL12_STAND.dataframe)
ARBL12_STANDmonthly<- ts(ARBL12_STAND.dataframe, start=c(1996,2), end = c(2018,2), frequency = 12) # MÃNADSDATA!

is.ts(ARBL12_STANDmonthly)
ts.plot(ARBL12_STANDmonthly, main = "ARBL12 STANDARDISERAD- M
        ", col = "darkgreen",   lwd = 1, ylab = "STANDARDISERAD")

# MÃNnadsvis.
ARBL12_STANDmonthly

## CONVERTING MONTHLY CCI  TO QUARTERLY DATA, STANDARDISERAD!
ARBL12_STAND <- ts (ARBL12_STANDmonthly,start=c(1996,2), end = c(2018,2))
ARBL12_STAND <- aggregate(ARBL12_STANDmonthly, FUN=mean, nfrequency=4)
ARBL12.STAND_serie<- ts(ARBL12_STAND , start=c(1996,2), end = c(2018,2), frequency = 4) # KVARTALSDATA
ARBL12.STAND_serie

# FINAL TIDSSERIE "CCI STANDARDISERAD - KVARTALSDATA" 
ts.plot(ARBL12.STAND_serie, main ="ARBL12 STANDARDISERAD - Q", lty ="dotted", col ="purple")



#################################################################################
#
# TIDSSERIE - BNPGap (Output) - (REDAN I KVARTALSDATA)
#  
# VARIABELNAMN: "OutputGap" från FIs dataset: "FIreplikering.xlsx"
#
#################################################################################

# Läs in data
OutputGap_dataframe <- FImodell[,"OutputGap"]

# Ta bort Na's
OutputGap_dataframe <- na.omit(OutputGap_dataframe)

# Skapa tidserie
OutputGap_serie <- ts(OutputGap_dataframe, start=c(1994,3), end = c(2018,2), frequency = 4)

# Deskriptiv statistik
mean(OutputGap_serie)
sd(OutputGap_serie)

ts.plot(OutputGap_serie, main ="Output Gap - Q", col ="red", ylab="%")

# Standardisera
Stand_OutputGap_dataframe <- scale(OutputGap_dataframe)
Stand_OutputGap_serie <- ts(Stand_OutputGap_dataframe, start=c(1994,3), end = c(2018,2), frequency = 4)
ts.plot(Stand_OutputGap_serie)

#################################################################################

ADF.test <- function(data){
  ADF <- function(type, data) {
    require(urca)
    result1 <- ur.df(data,
                     type = type,
                     lags = 3*frequency(data),
                     selectlags = "AIC")
    cbind(t(result1@teststat),result1@cval)
  }
  types <- c("trend", "drift", "none")
  result2 <- apply(t(types), 2, ADF, data)
  cat(rep("#", 17),'\n')
  cat("Augmented Dickey--Fuller test\n")
  cat(rep("#", 17),'\n')
  round(rbind(result2[[1]][c(1,3),],
              result2[[2]],
              result2[[3]]), 2)
}

###############################################
#                                             #
# ADF-TESTET & KPSS-TESTET                    #
#                                             #    
###############################################
## KPSS-TESTET: 

## H0: Stationär
## HA: Icke-stationär 

## Teststatistikan är ett mått pÃ variansen i testekvationen och om
## den är s.k. 'liten', är detta då en indikation på att vår serie
## är stationär. För att serien ska klassificeras som stationär krävs
## det att nollhypotesen i det genomförda KPSS-testet inte förkastas.


#######################################################################
### ADF-TESTET & KPSS:  BNP
#######################################################################
# BNP
ADF.test(BNP_serie) # FINNS ENHETSROT.
d.BNP_serie <- diff(BNP_serie)
ADF.test(d.BNP_serie) # FINNS INGEN ENHETSROT
# DIFF LOG
d.log.BNP_serie <- diff(log(BNP_serie))
ADF.test(d.log.BNP_serie) # INGEN ENHETSROT, DIFFLOG BNP
ts.plot(d.log.BNP_serie, main ="DIFF BNP", col ="orange")

# KPSS-TESTET VISAR ATT BÅDE DIFF OCH DIFFLOG AV BNP ÄR STATIONÄRA!
ur.kpss(d.log.BNP_serie, type = "tau")@teststat
# teststatistika: 0.04507724
ur.kpss(d.log.BNP_serie, type ="tau")@cval
# 0.04507724 < 0.146 = STATIONÄRT!


### KPSS-TESTET:  BNP


# KPSS BNP I NIVÅ:
ur.kpss(BNP_serie, type = "tau")@teststat
# 0.296612
ur.kpss(BNP_serie, type ="tau")@cval
# ej stationärt, på valfri signifikans nivå


# KPSS-TESTET VISAR ATT BÅDE DIFF OCH DIFFLOG AV BNP ÄR STATIONÄRA!
ur.kpss(d.log.BNP_serie, type = "tau")@teststat
# teststatistika: 0.05282659
ur.kpss(d.log.BNP_serie, type ="tau")@cval
# 0.04507724 < 0.146 = STATIONÄRT!


################################################################################
# ADF-TESTET & KPSS: NYA BOSTÄDER / FÄRDIGSTÄLLANDET AV NYA BOSTÄDER
#################################################################################
ADF.test(Nyabostader_serie) # FINNS ENHETSROT.
d.Nyabostader_serie <- diff(Nyabostader_serie)
ADF.test(d.Nyabostader_serie) # FINNS INGEN ENHETSROT.

# DIFF LOG NYA BOSTÄDER
d.log.Nyabostader_serie <- diff(log(Nyabostader_serie))
ADF.test(d.log.Nyabostader_serie) # FINNS INGEN ENHETSROT!
ts.plot(d.log.Nyabostader_serie, main ="DIFF NYA BOSTÄDER", col ="red")



### KPSS-TESTET:  NYA BOSTÄDER


# KPSS NYABOSTÄDER I NIVÅ
ur.kpss(Nyabostader_serie, type = "tau")@teststat
# 0.1180279
ur.kpss(Nyabostader_serie, type ="tau")@cval
# 0.1180279 < 0.146 (5pct). STATIONÄRT!

# LOG NYABOSTÄDER
log.Nyabostader_serie <- log(Nyabostader_serie)
ts.plot(log.Nyabostader_serie, main ="LOG Nyabostäder - Q")

# KPSS LOG NYABOSTÄDER 
ur.kpss(log.Nyabostader_serie, type = "tau")@teststat
# 0.1549439
ur.kpss(log.Nyabostader_serie, type ="tau")@cval
# 0.1549439 < 0.146 (5pct) EJ STATIONÄRT ISÅFALL I LOG.


# KPSS-TESTET VISAR ATT BÅDE DIFF OCH DIFFLOG AV NYA BOSTÄDER ÄR STATIONÄRA!
ur.kpss(d.log.Nyabostader_serie, type = "tau")@teststat
# teststatistika: 0.05698594
ur.kpss(d.log.Nyabostader_serie, type ="tau")@cval
# 0.05698594 < 0.146 = STATIONÄRT!



###################################################################
### ADF-TESTET & KPSS:  CCI HUSHÃLLENS KONFIDENSINDIKATOR
#####################################################################

ADF.test(CCI_serie) 
# Finns ingen enhetsrot, då tau3, -3.47 > -3.45 (5pct)
# STATIONÄRT.

## CCI I NIVÅ:
ts.plot (CCI_serie, main =" CCI - Q",lty = "dotted", col ="darkgreen", ylab="%", lwd = "2")

# DIFF AV CCI:
d.CCI_serie <- diff(CCI_serie)
ADF.test(d.CCI_serie) # INGEN ENHETSROT.

# LOG AV CCI:
log.CCI_serie <- log(CCI_serie)
ADF.test(log.CCI_serie) # FINNS ENHETSROT


# DIFF LOG AV CCI:
d.log.CCI_serie <- diff(log(CCI_serie))
ADF.test(d.log.CCI_serie) # INGEN ENHETSROT.

### KPSS-TESTET:  CCI

# KPSS-TESTET VISAR ATT BÅDE DIFF OCH DIFFLOG AV NYA BOSTÄDER ÄR STATIONÄRA!
ur.kpss(CCI_serie, type = "tau")@teststat
# teststatistika: 0.181838
ur.kpss(CCI_serie, type ="tau")@cval
# 0.05698594 < 0.146 = EJ STATIONÄRT på 5pct.


### CCI STANDARDISERAD:
# STANDARDISERAD 
ADF.test(CCI.STAND_serie) # Finns enhetsrot

# FÖRTA DIFF
d.CCI.STAND_serie <- diff(CCI.STAND_serie)
ADF.test(d.CCI.STAND_serie)
# FINNS INGEN ENHETSROT.



### KPSS-TESTET: CCI STANDARDISERAD 

# KPSS-TESTET VISAR ATT BÅDE DIFF OCH DIFFLOG AV NYA BOSTÄDER ÄR STATIONÄRA!
ur.kpss(CCI.STAND_serie, type = "tau")@teststat
# teststatistika: 0.181838
ur.kpss(CCI.STAND_serie, type ="tau")@cval
# 0.05698594 < 0.146 = ICKE STATIONÄRT på 5pct.

### KPSS-TESTET: DIFF/DIFFLOG CCI


ur.kpss(d.log.CCI_serie, type = "tau")@teststat
# teststatistika: 0.04788343
ur.kpss(CCI.STAND_serie, type ="tau")@cval
# 0.047883434 < 0.146 =  STATIONÄRT på 5pct.

### KPSS-TESTET: LOG CCI

ur.kpss(log.CCI_serie, type = "tau")@teststat
# teststatistika: 0.1877313
ur.kpss(log.CCI_serie, type ="tau")@cval
# 0.1877313 > 0.146 =  EJ STATIONÄRT på 5pcT, MEN STATIONÄRT PÅ 1 PCT.


################################################################
### ADF-TESTET:  FASTIGHETSPRISINDEX (FPI)
###############################################################

ADF.test(FPI_serie)
# Finns enhetsrot, dÃ¥ tau3, -2.47> -3.45 (5pct).
# EJ STATIONÄRT I LEVELS!

## FÖRSTA DIFFERENS
d.FPI_serie <- diff(FPI_serie)
ts.plot (d.FPI_serie, main ="DIFF FPI - Q",lty = "dotted", col ="blue", ylab="%", lwd = "2")

ADF.test(d.FPI_serie)
# Tau2, -2.95 < -2.89 = FINNS INGEN ENHETSROT!

### KPSS-TESTET: DIFF FPI

ur.kpss(d.FPI_serie, type = "tau")@teststat
# teststatistika: 0.09148311
ur.kpss(d.FPI_serie, type ="tau")@cval
# 0.09148311 < 0.146 =  STATIONÄRT på 5pct.


### DIFF LOG AV FPI SERIE ###

d.log.FPI_serie <- diff(log(FPI_serie))
ts.plot(d.log.FPI_serie, main ="DIFF LOG FPI - Q")

ADF.test(d.log.FPI_serie) # Stationärt.
# Tau2, -3.00 < -2.89 (5pct)  = FINNS INGEN ENHETSROT!


### KPSS-TESTET: DIFF LOG FPI

ur.kpss(d.log.FPI_serie, type = "tau")@teststat
# teststatistika: 0.08881831
ur.kpss(CCI.STAND_serie, type ="tau")@cval
# 0.08881831 < 0.146 =  STATIONÄRT på 5pct.


### KPSS-TESTET: LOG FPI

log.FPI_serie <- log(FPI_serie)

ur.kpss(log.FPI_serie, type = "tau")@teststat
# teststatistika: 0.4307422
ur.kpss(log.FPI_serie, type ="tau")@cval
# 0.4307422 > 0.146 =  STATIONÄRT på 5pct.


############################################################
### ADF-TESTET: SKULDKVOT
############################################################

ADF.test(Skuldkvot_serie)
# Finns enhetsrot då (tau3), -2.45 > -3.45 (5pct)

# FÖRSTA DIFFERENS
d.Skuldkvot_serie <- diff(Skuldkvot_serie)
ts.plot(d.Skuldkvot_serie, main =" DIFF SKULDKVOT - Q", lty="dotted",
        col ="RED", ylab="Change", lwd = "2")

ADF.test(d.Skuldkvot_serie)
# FINNS ENHETSROT I ÖVERENSTÄMMELSE MED SAMTLIGA STATISTIKOR!

### DIFF LOG AV SKULDKVOT SERIE ###
d.log.Skuldkvot_serie <- diff(log(Skuldkvot_serie))
ts.plot(d.log.Skuldkvot_serie, main ="DIFF LOG SKULDKVOT - Q")
ADF.test(d.log.Skuldkvot_serie) 
# FINNS FORTSATT ENHETSROT

### KPSS-TESTET: SKULDKVOT

# DIFF LOG
ur.kpss(d.log.Skuldkvot_serie, type = "tau")@teststat
# teststatistika: 0.08477426
ur.kpss(d.log.Skuldkvot_serie, type ="tau")@cval
# 0.198134< 0.216 =  STATIONÄRT på 1pct.

log.Skuldkvot_serie <- log(Skuldkvot_serie)

############################################################
### ADF-TESTET: ARBETSLÖSHET
############################################################

ADF.test(ARBL_serie)
# FINNS INGEN ENHETSROT I NIVÅ
# TAU3, -3.86 < -3.45

# Log av ARBL:
log.ARBL_serie <- log(ARBL_serie)
ADF.test(log.ARBL_serie) # INGEN ENHETSROT!

### KPSS-TESTET: ARBLETSLÖSHET

ur.kpss(ARBL_serie, type = "tau")@teststat
# teststatistika: 0.2448073
ur.kpss(ARBL_serie, type ="tau")@cval
# 0.2448073 > 0.146 =  EJ STATIONÄRT PÅ 5PCT I NIVÅ!

# DIFFAD SERIE
d.ARBL_serie <- diff(ARBL_serie)
ts.plot(d.ARBL_serie)

# DIFF LOG ARBL
d.log.ARBL_serie <- diff(log(ARBL_serie))


ur.kpss(d.log.ARBL_serie, type = "tau")@teststat
# teststatistika: 0.08751434
ur.kpss(d.log.ARBL_serie, type ="tau")@cval
# 0.08751434 < 0.146 =  STATIONÄRT PÅ 5PCT I NIVÅ!

########################################################################
### ADF-TESTET: KIX
########################################################################

ADF.test(KIX_serie)
# FINNS ENHETSROT PÅ 5PCT

# FÖRSTA DIFF PÅ KIX:
d.KIX_serie <- diff(KIX_serie)
ADF.test(d.KIX_serie) # INGEN ENHETSROT!

# DIFF LOG AV KIX:
d.log.KIX_serie <- diff(log(KIX_serie))
ADF.test(d.log.KIX_serie) # INGEN ENHETSROT!

### KPSS-TESTET: KIX

log.KIX_serie <-log(KIX_serie)

# KIX I NIVÅ
ur.kpss(KIX_serie, type = "tau")@teststat
# 0.1456021
ur.kpss(KIX_serie, type ="tau")@cval
# 0.1456021 < 0.146 STATIONÄRT PÅ 5PCT.

ur.kpss(d.log.KIX_serie, type = "tau")@teststat
# teststatistika: 0.05220739
ur.kpss(KIX_serie, type ="tau")@cval
# 0.05220739 < 0.146 =  STATIONÄRT PÅ 5PCT I NIVÅ!


# LOG
ur.kpss(log.KIX_serie, type = "tau")@teststat
# 0.1456021
ur.kpss(log.KIX_serie, type ="tau")@cval
# 0.1456021 < 0.146 STATIONÄRT PÅ 5PCT.


########################################################################
### ADF-TESTET: Reporänta 
########################################################################

ADF.test(Reporanta_serie)
# FINNS INGEN ENHETSROT PÅ 5PCT I NIVÅ!

ur.kpss(Reporanta_serie, type = "tau")@teststat
# teststatistika: 0.04707213
ur.kpss(Reporanta_serie, type ="tau")@cval
# 0.04707213< 0.146 =   STATIONÄRT PÅ 5PCT I NIVÅ!



########################################################################
### ADF-TESTET/KPSS-TESTET: RESURSUTNYTTJANDE!
########################################################################

# RU I NIVÅ
ADF.test(RU_serie)
# FINNS ENHETSROT!

# DIFF RU
d.RU_serie <- diff(RU_serie)
ADF.test(d.RU_serie) # FINNS INGEN ENHETSROT!

# GÅR EJ ATT LOGGA RU SERIEN.

### KPSS-TESTET: RU

# RU I NIVÅ
ur.kpss(RU_serie, type = "tau")@teststat
# teststatistika: 0.07380743 = # STATIONÄRT I NIVÅ!
ur.kpss(RU_serie, type ="tau")@cval
# 0.07380743 < 0.146 = STATIONÄRT PÅ 5PCT I NIVÅ!

# DIFFAD RU
ur.kpss(d.RU_serie, type = "tau")@teststat
# teststatistika: 0.04139592 = # STATIONÄRT I NIVÅ!
ur.kpss(RU_serie, type ="tau")@cval
# 0.04139592< 0.146 = STATIONÄRT PÅ 5PCT I NIVÅ!

########################################################################
### ADF-TESTET/KPSS-TESTET: ARBL12 MÅN FRAMÅT
########################################################################

# ARBL12 I NIVÅ
ADF.test(ARBL12_serie)
# FINNS INGEN ENHETSROT

# ARBL12 I STANDARDISERAD
ADF.test(ARBL12.STAND_serie)
# FINNS INGEN ENHETSROT - standardiserad.

### KPSS-TESTET: ARBL12

# RU I NIVÅ
ur.kpss(ARBL12_serie, type = "tau")@teststat
# teststatistika: 0.05225874 = # STATIONÄRT I NIVÅ!
ur.kpss(ARBL12_serie, type ="tau")@cval
# 0.05225874 < 0.146 = STATIONÄRT PÅ 5PCT I NIVÅ!

# LOG
log.ARBL12_serie <- log(ARBL12_serie) # går ej att logaritmera negativa tal

# ARBL12 I STANDARDISERAD
ur.kpss(ARBL12.STAND_serie, type = "tau")@teststat
# teststatistika: 0.05225874 = # STATIONÄRT I STANDARDISERAD!
ur.kpss(ARBL12.STAND_serie, type ="tau")@cval
# 0.05225874 < 0.146 = STATIONÄRT PÅ 5PCT I STANDARDISERAD



########################################################################
### ADF-TESTET/KPSS-TESTET: Output Gap!
########################################################################

# ADF-test

ADF.test(OutputGap_serie)

# Stationär

# KPSS-tets

ur.kpss(OutputGap_serie, type = "tau")@teststat
ur.kpss(OutputGap_serie, type = "tau")@cval

# Ej statyionärt enl KPSS

# Diffar.

d.OutputGap_serie <- diff(OutputGap_serie)

ADF.test(d.OutputGap_serie)

# Stationär

# KPSS-tets

ur.kpss(d.OutputGap_serie, type = "tau")@teststat
ur.kpss(d.OutputGap_serie, type = "tau")@cval

# Stationär

# Standarsdiserade

ADF.test(Stand_OutputGap_serie)

# Stationär

# KPSS-tets

ur.kpss(Stand_OutputGap_serie, type = "tau")@teststat
ur.kpss(Stand_OutputGap_serie, type = "tau")@cval

# Ej stationär enl kpss

# Diffar.

d.Stand_OutputGap_serie <- diff(Stand_OutputGap_serie)

ADF.test(d.Stand_OutputGap_serie)

# Stationär

# KPSS-tets

ur.kpss(d.Stand_OutputGap_serie, type = "tau")@teststat
ur.kpss(d.Stand_OutputGap_serie, type = "tau")@cval

# Stationär


########################################################################



#################################################################################
######################################################################################
## Integrationsordning:
##
##
## 
## 
##                    
######################################################################################
####################################################################
#
## MODELL 1 : HUSHÅLLSMODELLEN!
##
## FIs HUSHÅLLSMODELL OCH ORDNING - UTAN KIX
## P.G.A ALLA SERIER ÄR I FÖRSTA DIFF, SÄTTER VI START = 1996Q3!
## CCI, FPI, SKULDKVOT, REPORÄNTA
#
###############################################################


var.data.CCI <- cbind(window(CCI.STAND_serie, start = c(1996,3)),
                      window(d.log.FPI_serie, start = c(1996,3)),
                      window(d.log.Skuldkvot_serie, start = c(1996,3)),
                      window(Reporanta_serie, start = c(1996,3))) 

dimnames(var.data.CCI)[[2]] <- c("CCIstand","FPIdifflog","Skuldkvotdifflog","Reporantalevels")
var.data.CCI

VARselect(var.data.CCI, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test <- ca.jo(var.data.CCI, type="trace", K=4)
CCI.vecm.test@teststat
CCI.vecm.test@cval

# r = 2 vid K=2.

CCI.vecm2var.model <- vec2var(CCI.vecm.test, r=2)
CCI.vecm2var.model

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model)
serial.test(CCI.vecm2var.model)

# Problem med kvarvarande autokorr samt normalitet. Även problem vid K=3. Vid höjning av K till 4 så får vi r=0.
# Går vidare med att estimera en VAR-modell.

var.result.CCI <- VAR(var.data.CCI, p = 3, type ="const") 
summary(var.result.CCI)
serial.test(var.result.CCI) # Ingen autokorr!

# TEST NORMALITET. 
normality.test(var.result.CCI)$jb.mul$JB
# P-value: 
normality.test(var.result.CCI)$jb.mul$Skewness
# p-value = 
normality.test(var.result.CCI)$jb.mul$Kurtosis
# p-value = 

# GIVET P=2, har vi problem med normalitet men ej med autokorr!
# Vid ökning av lagglängd till 3 ser det ok ut, 4 ser det bra ut.

roots(var.result.CCI) # stabilt.


################################################
# IRF
################################################
irf(var.result.CCI, impulse ="Reporantalevels", response = "CCIstand", n.ahead = 16, seed = 4654)
irf(var.result.CCI, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654)
irf(var.result.CCI, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 32, seed = 4654)

plot(irf(var.result.CCI, impulse ="Reporantalevels", response = "CCIstand", n.ahead = 16, seed = 4654))
plot(irf(var.result.CCI, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654))
plot(irf(var.result.CCI, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt growth rate (%)", main="Baseline Model; Orthogonal Impulse Response From Repo Rate")

################################################
# FEVD
################################################

fevd(var.result.CCI, n.ahead=16)$Skuldkvotdifflog



################################################
#
# Alternativ HUSHÅLLSMODELL - diff-log KIX
# CCI, FPI, Skuld, Reporänta, KIX
#
################################################

var.data.CCI.kix.diff <- cbind(window(CCI.STAND_serie, start = c(1996,3)),
                               window(d.log.FPI_serie, start = c(1996,3)),
                               window(d.log.Skuldkvot_serie, start = c(1996,3)),
                               window(Reporanta_serie, start = c(1996,3)),
                               window(d.log.KIX_serie, start = c(1996,3))) 

dimnames(var.data.CCI.kix.diff)[[2]] <- c("CCIstand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.CCI.kix.diff

VARselect(var.data.CCI.kix.diff, type ="const") # SC/BIC sÃger 1 lags
VARselect(var.data.CCI.kix.diff, type ="trend") # SC/BIC sÃger 1 lags


# JOHANSEN

CCI.KIX.diff.vecm.test <- ca.jo(var.data.CCI.kix.diff, type="trace", K=3)
CCI.KIX.diff.vecm.test@teststat
CCI.KIX.diff.vecm.test@cval

# r = 3 vid K=2.

CCI.KIX.diff.vecm2var.model <- vec2var(CCI.KIX.diff.vecm.test, r=2)
CCI.KIX.diff.vecm2var.model

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.KIX.diff.vecm2var.model)
serial.test(CCI.KIX.diff.vecm2var.model)

# Inga problem med kvarvarande autokorr men problem med normalitet. Vid höjning av K till 3 så får vi r=2
# samt att problemen med kvarvarande autokorr samt icke-normalitet försvinner.

################################################
# IRF
################################################
irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "CCIstand", n.ahead = 16, seed = 4654)
irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654)
irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "KIXdifflog", n.ahead = 16, seed = 4654)

plot(irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "CCIstand", n.ahead = 16, seed = 4654))
plot(irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654))
plot(irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 40, seed = 4654), ylab="Debt growth rate (%)", ylim=c(-0.003,0.003), main="Extended Baseline Model; Orthogonal Impulse Response From Repo Rate")
plot(irf(CCI.KIX.diff.vecm2var.model, impulse ="Reporantalevels", response = "KIXdifflog", n.ahead = 16, seed = 4654))


################################################
# FEVD
################################################

fevd(CCI.KIX.diff.vecm2var.model, n.ahead=16)$Skuldkvotdifflog


################################################
#
# Alternativ MAKROMODELL - diff-log KIX
# DifflogBNP, DIFF-LOG FPI, DIFF-LOG Skuld, LOG KIX, Reporänta
#
################################################

var.data.BNP.diff <- cbind(window(d.log.BNP_serie, start = c(1996,3)),
                           window(d.log.FPI_serie, start = c(1996,3)),
                           window(d.log.Skuldkvot_serie, start = c(1996,3)),
                           window(Reporanta_serie, start = c(1996,3)),
                           window(d.log.KIX_serie, start = c(1996,3))) 

dimnames(var.data.BNP.diff)[[2]] <- c("BNPdifflog","FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.BNP.diff

VARselect(var.data.BNP.diff, type ="const") # SC/BIC säger 1 lags
VARselect(var.data.BNP.diff, type ="trend") # SC/BIC säger 1 lags


# JOHANSEN

BNP.diff.vecm.test <- ca.jo(var.data.BNP.diff, type="trace", K=2)
BNP.diff.vecm.test@teststat
BNP.diff.vecm.test@cval

# r = 3 vid K=2.

BNP.diff.vecm2var.model <- vec2var(BNP.diff.vecm.test, r=3)
BNP.diff.vecm2var.model

# test for normalitet och kvarvarande autokorr.

normality.test(BNP.diff.vecm2var.model)
serial.test(BNP.diff.vecm2var.model)

# Inga problem med kvarvarande autokorr samt normalitet, ev. lite problem med skevhet. 
# Vid hÃ¶jning av K till 3 sÃ¥ fÃ¥r vi r=1 samt att problemet med skevheten fÃ¶rsvinner.

################################################
# IRF
################################################
irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "BNPdifflog", n.ahead = 16, seed = 4654)
irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654)
irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "KIXdifflog", n.ahead = 16, seed = 4654)

plot(irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "BNPdifflog", n.ahead = 16, seed = 4654))
plot(irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654))
plot(irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt Growth Rate (%)", main="Conventional Macro Model; Orthogonal Impulse Response From Repo Rate")
plot(irf(BNP.diff.vecm2var.model, impulse ="Reporantalevels", response = "KIXdifflog", n.ahead = 16, seed = 4654))

################################################
# FEVD
################################################

fevd(BNP.diff.vecm2var.model, n.ahead=16)$Skuldkvotdifflog

################################################################################################
################################################################################################


################################################
#
# Robustness Check CCI-modellen:
# 
################################################
# Repo, FPI, SKULDKVOT, CCI.
################################################

var.data.CCI.robust1 <- cbind(window(Reporanta_serie, start = c(1996,3)),
                              window(d.log.FPI_serie, start = c(1996,3)),
                              window(d.log.Skuldkvot_serie, start = c(1996,3)),
                              window(CCI.STAND_serie, start = c(1996,3)))


dimnames(var.data.CCI.robust1)[[2]] <- c("Reporantalevels","FPIdifflog","Skuldkvotdifflog","CCIstand")
var.data.CCI.robust1

VARselect(var.data.CCI.robust1, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI.robust1, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test.robust1 <- ca.jo(var.data.CCI.robust1, type="trace", K=4)
CCI.vecm.test.robust1@teststat
CCI.vecm.test.robust1@cval

# r=2 när K=2

CCI.vecm2var.model.robust1 <- vec2var(CCI.vecm.test.robust1, r=2)
CCI.vecm2var.model.robust1

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.robust1)
serial.test(CCI.vecm2var.model.robust1)

# Samma problem därav VAR.

var.result.CCI.robust1 <- VAR(var.data.CCI.robust1, p = 4, type ="const") 
summary(var.result.CCI.robust1)
serial.test(var.result.CCI.robust1) # Ingen autokorr!

# TEST NORMALITET. 
normality.test(var.result.CCI.robust1)$jb.mul$JB
# P-value: 
normality.test(var.result.CCI.robust1)$jb.mul$Skewness
# p-value = 
normality.test(var.result.CCI.robust1)$jb.mul$Kurtosis

# SER inte bra ut oavsett lagglängd..

plot(irf(var.result.CCI.robust1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))
irf(var.result.CCI.robust1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
# IRF ser liknande ut vid K=4, samma K som i ovan modell.

fevd(var.result.CCI.robust1, n.ahead = 16)$Skuldkvotdifflog
################################################
# FPI, SKULDKVOT, REPO, CCI. 
################################################

var.data.CCI.robust2 <- cbind(window(d.log.FPI_serie, start = c(1996,3)),
                              window(d.log.Skuldkvot_serie, start = c(1996,3)),
                              window(CCI.STAND_serie, start = c(1996,3)),
                              window(Reporanta_serie, start = c(1996,3)))



dimnames(var.data.CCI.robust2)[[2]] <- c("FPIdifflog","Skuldkvotdifflog","Reporantalevels","CCIstand")
var.data.CCI.robust2

VARselect(var.data.CCI.robust2, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI.robust2, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test.robust2 <- ca.jo(var.data.CCI.robust2, type="trace", K=4)
CCI.vecm.test.robust2@teststat
CCI.vecm.test.robust2@cval

# r=2 när K=2

CCI.vecm2var.model.robust2 <- vec2var(CCI.vecm.test.robust2, r=2)
CCI.vecm2var.model.robust2

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.robust2)
serial.test(CCI.vecm2var.model.robust2)

# Samma problem

var.result.CCI.robust2 <- VAR(var.data.CCI.robust2, p = 4, type ="const") 
summary(var.result.CCI.robust2)
serial.test(var.result.CCI.robust2) # Ingen autokorr!

# TEST NORMALITET. 
normality.test(var.result.CCI.robust2)$jb.mul$JB
# P-value: 
normality.test(var.result.CCI.robust2)$jb.mul$Skewness
# p-value = 
normality.test(var.result.CCI.robust2)$jb.mul$Kurtosis

# ser inte bra ut oavset lagglängd..

plot(irf(var.result.CCI.robust2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))
irf(var.result.CCI.robust2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
# IRF ser liknande ut vid K=4, samma K som i ovan modell.

fevd(var.result.CCI.robust2, n.ahead = 16)$Skuldkvotdifflog

################################################
# Med ny tid - från 1999Q2. 
################################################

var.data.CCI.tid1 <- window(var.data.CCI, start=c(1999,2))


dimnames(var.data.CCI.tid1)[[2]] <- c("CCIstand","FPIdifflog","Skuldkvotdifflog","Reporantalevels")
var.data.CCI.tid1

VARselect(var.data.CCI.tid1, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI.tid1, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test.tid1 <- ca.jo(var.data.CCI.tid1, type="trace", K=4)
CCI.vecm.test.tid1@teststat
CCI.vecm.test.tid1@cval

# r=2 när K=2

CCI.vecm2var.model.tid1 <- vec2var(CCI.vecm.test.tid1, r=1)
CCI.vecm2var.model.tid1

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.tid1)
serial.test(CCI.vecm2var.model.tid1)

# Ser bra ut vid K=4, får då r=1.

plot(irf(CCI.vecm2var.model.tid1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.tid1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF ev. lite annorlunda

fevd(CCI.vecm2var.model.tid1, n.ahead = 16)$Skuldkvotdifflog

################################################
# Med ny tid - från 2000Q2. 
################################################

var.data.CCI.tid2 <- window(var.data.CCI, start=c(2000,2))


dimnames(var.data.CCI.tid2)[[2]] <- c("CCIstand","FPIdifflog","Skuldkvotdifflog","Reporantalevels")
var.data.CCI.tid2

VARselect(var.data.CCI.tid2, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI.tid2, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test.tid2 <- ca.jo(var.data.CCI.tid2, type="trace", K=3)
CCI.vecm.test.tid2@teststat
CCI.vecm.test.tid2@cval

# r=2 när K=2

CCI.vecm2var.model.tid2 <- vec2var(CCI.vecm.test.tid2, r=1)
CCI.vecm2var.model.tid2

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.tid2)
serial.test(CCI.vecm2var.model.tid2)

# Ser bra ut vid K=4, får då r=1.

plot(irf(CCI.vecm2var.model.tid2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.tid2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF reltivt liknande vanliga modell.

fevd(CCI.vecm2var.model.tid2, n.ahead = 16)$Skuldkvotdifflog

################################################
# Med ny tid - från 2005Q2. 
################################################

var.data.CCI.tid3 <- window(var.data.CCI, start=c(2005,2))


dimnames(var.data.CCI.tid3)[[2]] <- c("CCIstand","FPIdifflog","Skuldkvotdifflog","Reporantalevels")
var.data.CCI.tid3

VARselect(var.data.CCI.tid3, type ="const") # SC/BIC sÃƒger 10 lags
VARselect(var.data.CCI.tid3, type ="trend") # SC/BIC sÃƒger 10 lags

# JOHANSEN

CCI.vecm.test.tid3 <- ca.jo(var.data.CCI.tid3, type="trace", K=3)
CCI.vecm.test.tid3@teststat
CCI.vecm.test.tid3@cval

# r=1 när K=3. 10 enl bic är för många

CCI.vecm2var.model.tid3 <- vec2var(CCI.vecm.test.tid3, r=1)
CCI.vecm2var.model.tid3

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.tid3)
serial.test(CCI.vecm2var.model.tid3)

# Ser bra ut vid K=3, får då r=1.

plot(irf(CCI.vecm2var.model.tid3, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.tid3, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF reltivt liknande vanliga modell.

fevd(CCI.vecm2var.model.tid3, n.ahead = 16)$Skuldkvotdifflog

################################################
# Med ny tid - från 2006Q2. 
################################################

var.data.CCI.tid4 <- window(var.data.CCI, start=c(2006,2))


dimnames(var.data.CCI.tid4)[[2]] <- c("CCIstand","FPIdifflog","Skuldkvotdifflog","Reporantalevels")
var.data.CCI.tid4

VARselect(var.data.CCI.tid4, type ="const") # SC/BIC sÃƒger 10 lags
VARselect(var.data.CCI.tid4, type ="trend") # SC/BIC sÃƒger 10 lags

# JOHANSEN

CCI.vecm.test.tid4 <- ca.jo(var.data.CCI.tid4, type="trace", K=3)
CCI.vecm.test.tid4@teststat
CCI.vecm.test.tid4@cval

# r=1 när K=3. 10 enl bic är för många

CCI.vecm2var.model.tid4 <- vec2var(CCI.vecm.test.tid4, r=1)
CCI.vecm2var.model.tid4

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.tid4)
serial.test(CCI.vecm2var.model.tid4)

# Ser bra ut vid K=3, får då r=1.

plot(irf(CCI.vecm2var.model.tid4, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.tid4, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF reltivt liknande vanliga modell.

fevd(CCI.vecm2var.model.tid4, n.ahead = 16)$Skuldkvotdifflog

################################################
# Med ny tid - från 2007Q2. 
################################################

var.data.CCI.tid5 <- window(var.data.CCI, start=c(2007,2))


dimnames(var.data.CCI.tid5)[[2]] <- c("CCIstand","FPIdifflog","Skuldkvotdifflog","Reporantalevels")
var.data.CCI.tid5

VARselect(var.data.CCI.tid5, type ="const") # SC/BIC sÃƒger 9 lags
VARselect(var.data.CCI.tid5, type ="trend") # SC/BIC sÃƒger 9 lags

# JOHANSEN

CCI.vecm.test.tid5 <- ca.jo(var.data.CCI.tid5, type="trace", K=3)
CCI.vecm.test.tid5@teststat
CCI.vecm.test.tid5@cval

# r=1 när K=3. 10 enl bic är för många

CCI.vecm2var.model.tid5 <- vec2var(CCI.vecm.test.tid5, r=1)
CCI.vecm2var.model.tid5

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.tid5)
serial.test(CCI.vecm2var.model.tid5)

# Ser bra ut vid K=3, får då r=1.

plot(irf(CCI.vecm2var.model.tid5, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.tid5, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF reltivt liknande vanliga modell.

fevd(CCI.vecm2var.model.tid5, n.ahead = 16)$Skuldkvotdifflog

################################################
################################################
#
# Robustness Check CCI + KIX:
# 
################################################
# Repo, FPI, SKULDKVOT, KIX, CCI.
################################################

var.data.CCI.kix.diff.robust1 <- cbind(window(Reporanta_serie, start = c(1996,3)),
                                       window(d.log.FPI_serie, start = c(1996,3)),
                                       window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                       window(d.log.KIX_serie, start = c(1996,3)),
                                       window(CCI.STAND_serie, start = c(1996,3)))

dimnames(var.data.CCI.kix.diff.robust1)[[2]] <- c("Reporantalevels","FPIdifflog", "Skuldkvotdifflog","KIXdifflog","CCIstand")
var.data.CCI.kix.diff.robust1

VARselect(var.data.CCI.kix.diff.robust1, type ="const") # SC/BIC sÃger 1 lags
VARselect(var.data.CCI.kix.diff.robust1, type ="trend") # SC/BIC sÃger 1 lags


# JOHANSEN

CCI.KIX.diff.vecm.test.robust1 <- ca.jo(var.data.CCI.kix.diff.robust1, type="trace", K=6)
CCI.KIX.diff.vecm.test.robust1@teststat
CCI.KIX.diff.vecm.test.robust1@cval

# r = 3 vid K=2.

CCI.KIX.diff.vecm2var.model.robust1 <- vec2var(CCI.KIX.diff.vecm.test.robust1, r=1)
CCI.KIX.diff.vecm2var.model.robust1

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.KIX.diff.vecm2var.model.robust1)
serial.test(CCI.KIX.diff.vecm2var.model.robust1)

# Inga problem med kvarvarande autokorr men problem med normalitet. Vid höjning av K till 6 så får vi r=1
# samt att problemen med kvarvarande autokorr samt icke-normalitet försvinner.

# IRF

plot(irf(CCI.KIX.diff.vecm2var.model.robust1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))
irf(CCI.KIX.diff.vecm2var.model.robust1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
# Relativt liknande irf, skillnad i lagglängd. Lite mer hackig.

fevd(CCI.KIX.diff.vecm2var.model.robust1, n.ahead = 16)$Skuldkvotdifflog

################################################
# FPI, SKULDKVOT, CCI, REPO, KIX.
################################################

var.data.CCI.kix.diff.robust2 <- cbind(window(d.log.FPI_serie, start = c(1996,3)),
                                       window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                       window(Reporanta_serie, start = c(1996,3)),
                                       window(d.log.KIX_serie, start = c(1996,3)),
                                       window(CCI.STAND_serie, start = c(1996,3)))

dimnames(var.data.CCI.kix.diff.robust2)[[2]] <- c("FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog","CCIstand")
var.data.CCI.kix.diff.robust2

VARselect(var.data.CCI.kix.diff.robust2, type ="const") # SC/BIC sÃger 1 lags
VARselect(var.data.CCI.kix.diff.robust2, type ="trend") # SC/BIC sÃger 1 lags


# JOHANSEN

CCI.KIX.diff.vecm.test.robust2 <- ca.jo(var.data.CCI.kix.diff.robust2, type="trace", K=3)
CCI.KIX.diff.vecm.test.robust2@teststat
CCI.KIX.diff.vecm.test.robust2@cval

# r = 3 vid K=2.

CCI.KIX.diff.vecm2var.model.robust2 <- vec2var(CCI.KIX.diff.vecm.test.robust2, r=2)
CCI.KIX.diff.vecm2var.model.robust2

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.KIX.diff.vecm2var.model.robust2)
serial.test(CCI.KIX.diff.vecm2var.model.robust2)

# Inga problem med kvarvarande autokorr men problem med normalitet. Vid höjning av K till 3 så får vi r=2
# samt att problemen med kvarvarande autokorr samt icke-normalitet försvinner.

# IRF

plot(irf(CCI.KIX.diff.vecm2var.model.robust2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))
irf(CCI.KIX.diff.vecm2var.model.robust2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
# Relativt liknande irf, skillnad i lagglängd.

fevd(CCI.KIX.diff.vecm2var.model.robust2)$Skuldkvotdifflog

################################################
# Med ny tid - från 1999Q2. 
################################################

var.data.CCI.kix.diff.tid1 <- window(var.data.CCI.kix.diff, start=c(1999,2))


dimnames(var.data.CCI.kix.diff.tid1)[[2]] <- c("CCIstand","FPIdifflog", "Skuldkvotdifflog","KIXdifflog", "Reporantalevels")
var.data.CCI.kix.diff.tid1

VARselect(var.data.CCI.kix.diff.tid1, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI.kix.diff.tid1, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test.kix.diff.tid1 <- ca.jo(var.data.CCI.kix.diff.tid1, type="trace", K=2)
CCI.vecm.test.kix.diff.tid1@teststat
CCI.vecm.test.kix.diff.tid1@cval

# r=3 när K=2

CCI.vecm2var.model.kix.diff.tid1 <- vec2var(CCI.vecm.test.kix.diff.tid1, r=2)
CCI.vecm2var.model.kix.diff.tid1

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.kix.diff.tid1)
serial.test(CCI.vecm2var.model.kix.diff.tid1)

# Ser bra ut vid K=2, får då r=3.

plot(irf(CCI.vecm2var.model.kix.diff.tid1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.kix.diff.tid1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF ev. lite annorlunda

fevd(CCI.vecm2var.model.kix.diff.tid1)$Skuldkvotdifflog

################################################
# Med ny tid - från 2000Q2. 
################################################

var.data.CCI.kix.diff.tid2 <- window(var.data.CCI.kix.diff, start=c(2000,2))


dimnames(var.data.CCI.kix.diff.tid2)[[2]] <- c("CCIstand","FPIdifflog", "Skuldkvotdifflog","KIXdifflog", "Reporantalevels")
var.data.CCI.kix.diff.tid2

VARselect(var.data.CCI.kix.diff.tid2, type ="const") # SC/BIC sÃƒger 2 lags
VARselect(var.data.CCI.kix.diff.tid2, type ="trend") # SC/BIC sÃƒger 2 lags

# JOHANSEN

CCI.vecm.test.kix.diff.tid2 <- ca.jo(var.data.CCI.kix.diff.tid2, type="trace", K=2)
CCI.vecm.test.kix.diff.tid2@teststat
CCI.vecm.test.kix.diff.tid2@cval

# r=3 när K=2

CCI.vecm2var.model.kix.diff.tid2 <- vec2var(CCI.vecm.test.kix.diff.tid2, r=3)
CCI.vecm2var.model.kix.diff.tid2

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.kix.diff.tid2)
serial.test(CCI.vecm2var.model.kix.diff.tid2)

# Ser bra ut vid K=2, får då r=3.

plot(irf(CCI.vecm2var.model.kix.diff.tid2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.kix.diff.tid2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF ev. lite annorlunda

fevd(CCI.vecm2var.model.kix.diff.tid2)$Skuldkvotdifflog

################################################
# Med ny tid - från 2005Q2. 
################################################

var.data.CCI.kix.diff.tid3 <- window(var.data.CCI.kix.diff, start=c(2005,2))


dimnames(var.data.CCI.kix.diff.tid3)[[2]] <- c("CCIstand","FPIdifflog", "Skuldkvotdifflog","KIXdifflog", "Reporantalevels")
var.data.CCI.kix.diff.tid3

VARselect(var.data.CCI.kix.diff.tid3, type ="const") # SC/BIC sÃƒger 9 lags
VARselect(var.data.CCI.kix.diff.tid3, type ="trend") # SC/BIC sÃƒger 9 lags

# JOHANSEN

CCI.vecm.test.kix.diff.tid3 <- ca.jo(var.data.CCI.kix.diff.tid3, type="trace", K=2)
CCI.vecm.test.kix.diff.tid3@teststat
CCI.vecm.test.kix.diff.tid3@cval

# r=3 när K=2

CCI.vecm2var.model.kix.diff.tid3 <- vec2var(CCI.vecm.test.kix.diff.tid3, r=3)
CCI.vecm2var.model.kix.diff.tid3

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.kix.diff.tid3)
serial.test(CCI.vecm2var.model.kix.diff.tid3)

# Ser bra ut vid K=2, får då r=3.

plot(irf(CCI.vecm2var.model.kix.diff.tid3, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.kix.diff.tid3, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF ev. lite annorlunda

fevd(CCI.vecm2var.model.kix.diff.tid3)$Skuldkvotdifflog

################################################
# Med ny tid - från 2006Q2. 
################################################

var.data.CCI.kix.diff.tid4 <- window(var.data.CCI.kix.diff, start=c(2006,2))


dimnames(var.data.CCI.kix.diff.tid4)[[2]] <- c("CCIstand","FPIdifflog", "Skuldkvotdifflog","KIXdifflog", "Reporantalevels")
var.data.CCI.kix.diff.tid4

VARselect(var.data.CCI.kix.diff.tid4, type ="const") # SC/BIC sÃƒger 8 lags
VARselect(var.data.CCI.kix.diff.tid4, type ="trend") # SC/BIC sÃƒger 8 lags

# JOHANSEN

CCI.vecm.test.kix.diff.tid4 <- ca.jo(var.data.CCI.kix.diff.tid4, type="trace", K=2)
CCI.vecm.test.kix.diff.tid4@teststat
CCI.vecm.test.kix.diff.tid4@cval

# r=3 när K=2

CCI.vecm2var.model.kix.diff.tid4 <- vec2var(CCI.vecm.test.kix.diff.tid4, r=3)
CCI.vecm2var.model.kix.diff.tid4

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.kix.diff.tid4)
serial.test(CCI.vecm2var.model.kix.diff.tid4)

# Ser bra ut vid K=2, får då r=3.

plot(irf(CCI.vecm2var.model.kix.diff.tid4, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.kix.diff.tid4, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF ev. lite annorlunda

fevd(CCI.vecm2var.model.kix.diff.tid4)$Skuldkvotdifflog

################################################
# Med ny tid - från 2007Q2. 
################################################

var.data.CCI.kix.diff.tid5 <- window(var.data.CCI.kix.diff, start=c(2007,2))


dimnames(var.data.CCI.kix.diff.tid5)[[2]] <- c("CCIstand","FPIdifflog", "Skuldkvotdifflog","KIXdifflog", "Reporantalevels")
var.data.CCI.kix.diff.tid5

VARselect(var.data.CCI.kix.diff.tid5, type ="const") # SC/BIC sÃƒger 7 lags
VARselect(var.data.CCI.kix.diff.tid5, type ="trend") # SC/BIC sÃƒger 7 lags

# JOHANSEN

CCI.vecm.test.kix.diff.tid5 <- ca.jo(var.data.CCI.kix.diff.tid5, type="trace", K=2)
CCI.vecm.test.kix.diff.tid5@teststat
CCI.vecm.test.kix.diff.tid5@cval

# r=3 när K=2

CCI.vecm2var.model.kix.diff.tid5 <- vec2var(CCI.vecm.test.kix.diff.tid5, r=3)
CCI.vecm2var.model.kix.diff.tid5

# test for normalitet och kvarvarande autokorr.

normality.test(CCI.vecm2var.model.kix.diff.tid5)
serial.test(CCI.vecm2var.model.kix.diff.tid5)

# Ser bra ut vid K=2, får då r=3.

plot(irf(CCI.vecm2var.model.kix.diff.tid5, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

irf(CCI.vecm2var.model.kix.diff.tid5, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)

# IRF  annorlunda

fevd(CCI.vecm2var.model.kix.diff.tid5)$Skuldkvotdifflog

################################################
#
# Robustness Check Makromodellen:
# 
# Robustness inte gjord på ett liknande extensive
# sätt då makromodellen är en benchmark mot modellerna
# med sentiment indicators och därav kan ses som
# en robustness check i sig själv.
#
################################################
################################################
# Med ny tid - från 1999Q2. 
################################################

var.data.BNP.diff.tid1 <- window(var.data.BNP.diff, start=c(1999,2))

dimnames(var.data.BNP.diff.tid1)[[2]] <- c("BNPdifflog","FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.BNP.diff.tid1

VARselect(var.data.BNP.diff.tid1, type ="const") # SC/BIC säger 1 lags
VARselect(var.data.BNP.diff.tid1, type ="trend") # SC/BIC säger 1 lags


# JOHANSEN

BNP.diff.vecm.test.tid1 <- ca.jo(var.data.BNP.diff.tid1, type="trace", K=3)
BNP.diff.vecm.test.tid1@teststat
BNP.diff.vecm.test.tid1@cval

# r = 2 vid K=2.

BNP.diff.vecm2var.model.tid1 <- vec2var(BNP.diff.vecm.test.tid1, r=2)
BNP.diff.vecm2var.model.tid1

# test for normalitet och kvarvarande autokorr.

normality.test(BNP.diff.vecm2var.model.tid1)
serial.test(BNP.diff.vecm2var.model.tid1)

# Inga problem med vid K=3 och r=2.

irf(BNP.diff.vecm2var.model.tid1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
plot(irf(BNP.diff.vecm2var.model.tid1, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt Growth Rate (%)", main="Conventional Macro Model; Orthogonal Impulse Response From Repo Rate - Time 1999")

fevd(BNP.diff.vecm2var.model.tid1, n.ahead=16)$Skuldkvotdifflog

################################################
# Med ny tid - från 2000Q2. 
################################################
var.data.BNP.diff.tid2 <- window(var.data.BNP.diff, start=c(2000,2))

dimnames(var.data.BNP.diff.tid2)[[2]] <- c("BNPdifflog","FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.BNP.diff.tid2

VARselect(var.data.BNP.diff.tid2, type ="const") # SC/BIC säger 1 lags
VARselect(var.data.BNP.diff.tid2, type ="trend") # SC/BIC säger 1 lags


# JOHANSEN

BNP.diff.vecm.test.tid2 <- ca.jo(var.data.BNP.diff.tid2, type="trace", K=2)
BNP.diff.vecm.test.tid2@teststat
BNP.diff.vecm.test.tid2@cval

# r = 2 vid K=2.

BNP.diff.vecm2var.model.tid2 <- vec2var(BNP.diff.vecm.test.tid2, r=2)
BNP.diff.vecm2var.model.tid2

# test for normalitet och kvarvarande autokorr.

normality.test(BNP.diff.vecm2var.model.tid2)
serial.test(BNP.diff.vecm2var.model.tid2)

# Inga problem med vid K=2 och r=2.

irf(BNP.diff.vecm2var.model.tid2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
plot(irf(BNP.diff.vecm2var.model.tid2, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt Growth Rate (%)", main="Conventional Macro Model; Orthogonal Impulse Response From Repo Rate - Time 2000")

fevd(BNP.diff.vecm2var.model.tid2, n.ahead=16)$Skuldkvotdifflog
################################################
# Med ny tid - från 2005Q2. 
################################################

var.data.BNP.diff.tid3 <- window(var.data.BNP.diff, start=c(2005,2))

dimnames(var.data.BNP.diff.tid3)[[2]] <- c("BNPdifflog","FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.BNP.diff.tid3

VARselect(var.data.BNP.diff.tid3, type ="const") # SC/BIC säger 9 lags
VARselect(var.data.BNP.diff.tid3, type ="trend") # SC/BIC säger 9 lags


# JOHANSEN

BNP.diff.vecm.test.tid3 <- ca.jo(var.data.BNP.diff.tid3, type="trace", K=2)
BNP.diff.vecm.test.tid3@teststat
BNP.diff.vecm.test.tid3@cval

# r = 2 vid K=2.

BNP.diff.vecm2var.model.tid3 <- vec2var(BNP.diff.vecm.test.tid3, r=2)
BNP.diff.vecm2var.model.tid3

# test for normalitet och kvarvarande autokorr.

normality.test(BNP.diff.vecm2var.model.tid3)
serial.test(BNP.diff.vecm2var.model.tid3)

# Inga problem med vid K=2 och r=2.

irf(BNP.diff.vecm2var.model.tid3, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
plot(irf(BNP.diff.vecm2var.model.tid3, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt Growth Rate (%)", main="Conventional Macro Model; Orthogonal Impulse Response From Repo Rate - Time 2005")

fevd(BNP.diff.vecm2var.model.tid3, n.ahead=16)$Skuldkvotdifflog
################################################
# Med ny tid - från 2006Q2. 
################################################

var.data.BNP.diff.tid4 <- window(var.data.BNP.diff, start=c(2006,2))

dimnames(var.data.BNP.diff.tid4)[[2]] <- c("BNPdifflog","FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.BNP.diff.tid4

VARselect(var.data.BNP.diff.tid4, type ="const") # SC/BIC säger 8 lags
VARselect(var.data.BNP.diff.tid4, type ="trend") # SC/BIC säger 8 lags


# JOHANSEN

BNP.diff.vecm.test.tid4 <- ca.jo(var.data.BNP.diff.tid4, type="trace", K=2)
BNP.diff.vecm.test.tid4@teststat
BNP.diff.vecm.test.tid4@cval

# r = 2 vid K=2.

BNP.diff.vecm2var.model.tid4 <- vec2var(BNP.diff.vecm.test.tid4, r=2)
BNP.diff.vecm2var.model.tid4

# test for normalitet och kvarvarande autokorr.

normality.test(BNP.diff.vecm2var.model.tid4)
serial.test(BNP.diff.vecm2var.model.tid4)

# Inga problem med vid K=2 och r=2.

irf(BNP.diff.vecm2var.model.tid4, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
plot(irf(BNP.diff.vecm2var.model.tid4, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654), ylim=c(-0.003,0.003), ylab="Debt Growth Rate (%)", main="Conventional Macro Model; Orthogonal Impulse Response From Repo Rate - Time 2006")

fevd(BNP.diff.vecm2var.model.tid4, n.ahead=16)$Skuldkvotdifflog

#####################################################################################

################################################################################################
#
# Alternativ MAKROMODELL - diff-log KIX
# DIFFStand Output gap, DIFF-LOG FPI, DIFF-LOG Skuld, LOG KIX, Reporänta
# 
# Ej med i uppsatsen. Problemtik med diagnostiken.
#
################################################################################################

var.data.standgap <- cbind(window(d.Stand_OutputGap_serie, start = c(1996,3)),
                           window(d.log.FPI_serie, start = c(1996,3)),
                           window(d.log.Skuldkvot_serie, start = c(1996,3)),
                           window(Reporanta_serie, start = c(1996,3)), 
                           window(d.log.KIX_serie, start = c(1996,3))) 

dimnames(var.data.standgap)[[2]] <- c("OutputGapdiff","FPIdifflog","Skuldkvotdifflog","Reporantalevels","KIXdifflog")
var.data.standgap

VARselect(var.data.standgap, type ="const") # SC/BIC sÃger 1 lags
VARselect(var.data.standgap, type ="trend") # SC/BIC sÃger 1 lags


# JOHANSEN

standgap.vecm.test <- ca.jo(var.data.standgap, type="trace", K=4)
standgap.vecm.test@teststat
standgap.vecm.test@cval

# r = 2 vid K=2.

standgap.vecm2var.model <- vec2var(standgap.vecm.test, r=1)
standgap.vecm2var.model

# test for normalitet och kvarvarande autokorr.


normality.test(standgap.vecm2var.model)
serial.test(standgap.vecm2var.model)

# Problem oavsett K. Problem även vid ändring till diff i gap.
plot(irf(standgap.vecm2var.model, impulse = "Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))

################################################
# IRF
################################################
irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "BNPdifflog", n.ahead = 16, seed = 4654)
irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654)
irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654)
irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "KIXdifflog", n.ahead = 16, seed = 4654)

plot(irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "BNPdifflog", n.ahead = 16, seed = 4654))
plot(irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "FPIdifflog", n.ahead = 16, seed = 4654))
plot(irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "Skuldkvotdifflog", n.ahead = 16, seed = 4654))
plot(irf(standgap.vecm2var.model, impulse ="Reporantalevels", response = "KIXdifflog", n.ahead = 16, seed = 4654))

################################################
# FEVD
################################################

fevd_vecm.standgap <- fevd(standgap.vecm2var.model, n.ahead=16) # h = 3 år! h can be altered.
class(fevd_vecm.standgap) # "varfevd"
names(fevd_vecm.standgap)

print(fevd_vecm.standgap)
#####################################################################################



# KLAR!
##################################################################
#  RU-MODELLEN (ORIGINAL); 1996Q2 - 2018Q2 = KLAR!
##################################################################

##############################################################################################################################
# RU,  �r standardiserad av Riksbanken, s� standardiseras ej av oss! G�r ej att logaritmera. RU �r redan i niv�
# d.log.FPI_serie (Diff log FPI)
# d.log.Skuld (Diff log Skuldkvot)
# diff.log.KIX 
# Repor�nta niv�
##############################################################################################################################

var.data.johansen.RU.difflogKIX <- cbind(window(RU_serie, start = c(1996,3)),
                                         window(d.log.FPI_serie, start = c(1996,3)),
                                         window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                         window(Reporanta_serie, start = c(1996,3)),
                                         window(d.log.KIX_serie, start = c(1996,3)))

dimnames(var.data.johansen.RU.difflogKIX)[[2]] <- c("RUlevels","FPIdifflog","Skuldkvotdifflog", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.difflogKIX

VARselect(var.data.johansen.RU.difflogKIX, type ="const") # SC/BIC s�ger 1 lags
# Bortser ifr�n deterministiska termer s�som "trend" d� variabler �r differens station�ra!

#########################################################################
# JOHANSEN "TRACE" TEST
##########################################################################
vecm.t.RU.difflogKIX <- ca.jo(var.data.johansen.RU.difflogKIX, K=4, type ="trace")
vecm.t.RU.difflogKIX@teststat
vecm.t.RU.difflogKIX@cval

# r<=3, n�r k=2
# r<=3 n�r k=3
# r=0 n�r k=4. ESTIMERAR en VAR!


# ESTIMERAR en VAR! pga r=0 n�r k=4
var.result.johansen.RU.difflogKIX <- VAR(var.data.johansen.RU.difflogKIX, p = 4, type ="const") 
summary(var.result.johansen.RU.difflogKIX)
serial.test(var.result.johansen.RU.difflogKIX) 
# Ingen autokorr n�r p=2
# ingen autokorr n�r p=4

# TEST NORMALITET. 
normality.test(var.result.johansen.RU.difflogKIX)$jb.mul$JB
normality.test(var.result.johansen.RU.difflogKIX)$jb.mul$Skewness
normality.test(var.result.johansen.RU.difflogKIX)$jb.mul$Kurtosis
# normala residualer n�r p=4

roots(var.result.johansen.RU.difflogKIX) # stabil

################################################
# IRF 
################################################

# MULTIPLIER ANALYSIS: 
(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654))
(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))



# IRF PLOTS:
plot(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
plot(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="RU-Model: Orthogonal Impulse Reponse from Repo Rate")
plot(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.result.johansen.RU.difflogKIX, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


########################################################################################
# RU-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: RU (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
######################################################################################## 
fevd_var.result.johansen.RU.difflogKIX <- fevd(var.result.johansen.RU.difflogKIX, n.ahead=16) # h = 4 �r.
print(fevd_var.result.johansen.RU.difflogKIX)
#################################################################################


################################################################################################################################
################################################################################################################################


# KLAR!
# **** ARBL12-MODELLEN
####################################################################################################################
#  ARBL12-MODELL (ORIGINAL); 1996Q2 - 2018Q2 - "(Arbetsl�shet 12M�n fram�t survey data indikator fr�n KI)
####################################################################################################################

#######################################################################################
#  MODELL MED "ARBL12" IST�LLET F�R CCI 
# 
# ARBL12 stand, diff log FPI, diff log Skuldkvot, Repor�nta (niv�), diff log KIX, 
#######################################################################################

var.data.johansen.ARBL12.difflogKIX <- cbind(window(ARBL12.STAND_serie, start = c(1996,3)),
                                             window(d.log.FPI_serie, start = c(1996,3)),
                                             window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                             window(Reporanta_serie, start = c(1996,3)),
                                             window(d.log.KIX_serie, start = c(1996,3)))

dimnames(var.data.johansen.ARBL12.difflogKIX)[[2]] <- c("ARBL12stand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels", "KIXdifflog" )
var.data.johansen.ARBL12.difflogKIX

VARselect(var.data.johansen.ARBL12.difflogKIX, type ="const") # SC/BIC s�ger 1 lagg


#########################################################################
# JOHANSEN "TRACE" TEST: ARBL12 modellen
##########################################################################
vecm.t.ARBL12.difflogKIX <- ca.jo(var.data.johansen.ARBL12.difflogKIX, K=4, type ="trace")
vecm.t.ARBL12.difflogKIX@teststat
vecm.t.ARBL12.difflogKIX@cval

# utan ecdet=trend r<=3 k=2
# r=2 n�r k=3
# r=1 n�r k=4 


################################################################
# ESTIMERAR NU VECM:  ARBL12 modellen  med "diff log KIX"
################################################################
vecm.ARBL12.difflogKIX <- cajorls(vecm.t.ARBL12.difflogKIX, r=1)
vecm.ARBL12.difflogKIX$rlm
vecm.ARBL12.difflogKIX$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.difflogKIX <- vec2var(vecm.t.ARBL12.difflogKIX, r=1) # k�r trace!
var.vec2var.ARBL12.difflogKIX$A 
var.vec2var.ARBL12.difflogKIX$deterministic
serial.test(var.vec2var.ARBL12.difflogKIX) #  
# Ingen kvarvarande autokorr i resid n�r k=2 och r<=4
# Ingen kvarvarande autokorr i resid n�r k=3 och r<=2
# Ingen kvarvarande autokorr i resid n�r k=4 och r<=2

# utan ecdet=trend, ingen kvarvaranade autokorr n�r k=2 och r=3

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.difflogKIX)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.difflogKIX)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.difflogKIX)$jb.mul$Kurtosis 
# Icke-normalitet bland residualerna oberoende laggl�ngd. 
# I v�rsta fall accepterar vi icke-normalitet i systemet.
# icke-normalitet n�r k=4 och r=1 fortsatt oberoende laggl�ngd. 


################################################################
# IRF: ARBL12- MODELLEN
################################################################

(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed=4654)) 
(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed=4654))
(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed=4654))
(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed=4654))
(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed=4654))


plot(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed=4654)) 
plot(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed=4654))
plot(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed=4654), ylab="Debt Growth Rate (%)",
     ylim =c(-0.003, 0.003), main ="ARBL12-Model: Orthogonal Impulse Reponse from Repo Rate")
plot(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed=4654))
plot(irf(var.vec2var.ARBL12.difflogKIX, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed=4654))


################################################################################################ 
# ARBL12-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: ARBL12 (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
################################################################################################  
fevd_var.result.ARBL12.difflogKIX <- fevd(var.vec2var.ARBL12.difflogKIX, n.ahead=16) # h = 4 �r.
print(fevd_var.result.ARBL12.difflogKIX)
# 
################################################################################################






###########################################################################################################################
# ****** ROBUSTNESS CHECKS: ARBL12-MODEL & RU-MODEL *******
#
###########################################################################################################################

#  �NDRADE START TIDER P� URSPRUNGSMODELLERNA: 1999Q2, 2000Q1, 2005Q2, 2006Q2 & 2007Q2
#  D�R URSPRUNGSMODELLERNA �R (denna ordning): RU, FPI, SKULD, REPOR�NTA, KIX

# ------------------------- *RU-MODELLEN med olika estimationsperioder.

# KLAR!
# 1999Q2 - 2018Q2
#################################################################################
#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# Byter ordningen p� KIX och Repo; RU, FPI, SKULD, REPO, KIX.
#
#################################################################################
#################################################################################
var.data.johansen.RU.repokix1999 <- cbind(window(RU_serie, start = c(1999,3)),
                                          window(d.log.FPI_serie, start = c(1999,3)),
                                          window(d.log.Skuldkvot_serie, start = c(1999,3)),
                                          window(Reporanta_serie, start = c(1999,3)),
                                          window(d.log.KIX_serie, start = c(1999,3))) 

dimnames(var.data.johansen.RU.repokix1999)[[2]] <- c("RUlevels","FPIdifflog","Skuldkvotdifflog", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.repokix1999

VARselect(var.data.johansen.RU.repokix1999, type ="const") # SC/BIC s�ger 1 lags
# Bortser ifr�n deterministiska termer s�som "trend" d� variabler �r differens station�ra!

#########################################################################
# JOHANSEN "TRACE" TEST: RU, FPI, skuld, repo, kix
##########################################################################
vecm.t.RU.repokix1999 <- ca.jo(var.data.johansen.RU.repokix1999, K=4, type ="trace")
vecm.t.RU.repokix1999@teststat
vecm.t.RU.repokix1999@cval

###################################################################
# ESTIMERAR NU VECM av RU-modellen: RU, FPI, skuld, repo, kix
################################################################
vecm.RU.repokix1999 <- cajorls(vecm.t.RU.repokix1999, r=1)
vecm.RU.repokix1999$rlm
vecm.RU.repokix1999$beta

# FR�N VECM TILL VAR
var.vec2var.RU.repokix1999 <- vec2var(vecm.t.RU.repokix1999, r=1) # k�r trace!
var.vec2var.RU.repokix1999$A 
var.vec2var.RU.repokix1999$deterministic
serial.test(var.vec2var.RU.repokix1999) 
# Ingen kvarvarande autokorrelation i residualerna n�r k=3, r<=3 (enligt Trace-testet)

# NORMALITETS TEST: 
normality.test(var.vec2var.RU.repokix1999)$jb.mul$JB 
normality.test(var.vec2var.RU.repokix1999)$jb.mul$Skewness 
normality.test(var.vec2var.RU.repokix1999)$jb.mul$Kurtosis 
# Feltermerna �r normala n�r k=4, r<=1 (enligt Trace-testet)


################################################
# IRF VAR
################################################
plot(irf(var.vec2var.RU.repokix1999, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.RU.repokix1999, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix1999, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="RU-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.RU.repokix1999, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix1999, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


########################################################################################
# RU-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: RU (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
######################################################################################## 
fevd_var.vec2var.RU.repokix1999 <- fevd(var.vec2var.RU.repokix1999, n.ahead=16) # h = 4 �r.
print(fevd_var.vec2var.RU.repokix1999)
#################################################################################


# KLAR!
# 2000Q2 - 2018Q2
#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# Byter ordningen p� KIX och Repo; RU, FPI, SKULD, REPO, KIX.
# 
#################################################################################
#################################################################################
var.data.johansen.RU.repokix2000 <- cbind(window(RU_serie, start = c(2000,3)),
                                          window(d.log.FPI_serie, start = c(2000,3)),
                                          window(d.log.Skuldkvot_serie, start = c(2000,3)),
                                          window(Reporanta_serie, start = c(2000,3)),
                                          window(d.log.KIX_serie, start = c(2000,3))) 

dimnames(var.data.johansen.RU.repokix2000)[[2]] <- c("RUlevels","FPIdifflog","Skuldkvotdifflog", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.repokix2000

VARselect(var.data.johansen.RU.repokix2000, type ="const") # SC/BIC s�ger 1 lags
# Bortser ifr�n deterministiska termer s�som "trend" d� variabler �r differens station�ra!

#########################################################################
# JOHANSEN "TRACE" TEST: RU, FPI, skuld, repo, kix
##########################################################################
vecm.t.RU.repokix2000 <- ca.jo(var.data.johansen.RU.repokix2000, K=3, type ="trace")
vecm.t.RU.repokix2000@teststat
vecm.t.RU.repokix2000@cval

# r=3 n�r k=3


###################################################################
# ESTIMERAR NU VECM av RU-modellen: RU, FPI, skuld, repo, kix
################################################################
vecm.RU.repokix2000 <- cajorls(vecm.t.RU.repokix2000, r=3)
vecm.RU.repokix2000$rlm
vecm.RU.repokix2000$beta

# FR�N VECM TILL VAR
var.vec2var.RU.repokix2000 <- vec2var(vecm.t.RU.repokix2000, r=3) # k�r trace!
var.vec2var.RU.repokix2000$A 
var.vec2var.RU.repokix2000$deterministic
serial.test(var.vec2var.RU.repokix2000) 
# Ingen kvarvarande autokorrelation i residualerna n�r k=3, r<=3 (enligt Trace-testet)

# NORMALITETS TEST: 
normality.test(var.vec2var.RU.repokix2000)$jb.mul$JB 
normality.test(var.vec2var.RU.repokix2000)$jb.mul$Skewness 
normality.test(var.vec2var.RU.repokix2000)$jb.mul$Kurtosis 
# Feltermerna �r normala n�r k=3, r<=3 (enligt Trace-testet)

plot(irf(var.vec2var.RU.repokix2000, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.RU.repokix2000, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2000, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="RU-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.RU.repokix2000, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2000, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


########################################################################################
# RU-MODELLEN
# VARIABLER: RU, FPI, skuld, repo, kix
######################################################################################## 
fevd_var.vec2var.RU.repokix2000  <- fevd(var.vec2var.RU.repokix2000 , n.ahead=16) # h = 4 �r.
print(fevd_var.vec2var.RU.repokix2000 )
#################################################################################
#################################################################################


# KLAR!
# 2005Q2 - 2018Q2 
#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# Byter ordningen p� KIX och Repo; RU, FPI, SKULD, REPO, KIX.
# 
#################################################################################
#################################################################################
var.data.johansen.RU.repokix2005 <- cbind(window(RU_serie, start = c(2005,3)),
                                          window(d.log.FPI_serie, start = c(2005,3)),
                                          window(d.log.Skuldkvot_serie, start = c(2005,3)),
                                          window(Reporanta_serie, start = c(2005,3)),
                                          window(d.log.KIX_serie, start = c(2005,3))) 

dimnames(var.data.johansen.RU.repokix2005)[[2]] <- c("RUlevels","FPIdifflog","Skuldkvotdifflog", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.repokix2005

VARselect(var.data.johansen.RU.repokix2005, type ="const") # SC/BIC s�ger 1 lags
# Bortser ifr�n deterministiska termer s�som "trend" d� variabler �r differens station�ra!

#########################################################################
# JOHANSEN "TRACE" TEST: RU, FPI, skuld, repo, kix
##########################################################################
vecm.t.RU.repokix2005 <- ca.jo(var.data.johansen.RU.repokix2005, K=2, type ="trace")
vecm.t.RU.repokix2005@teststat
vecm.t.RU.repokix2005@cval

# r=2 n�r k=2


###################################################################
# ESTIMERAR NU VECM av RU-modellen: RU, FPI, skuld, repo, kix
################################################################
vecm.RU.repokix2005 <- cajorls(vecm.t.RU.repokix2005, r=2)
vecm.RU.repokix2005$rlm
vecm.RU.repokix2005$beta

# FR�N VECM TILL VAR
var.vec2var.RU.repokix2005 <- vec2var(vecm.t.RU.repokix2005, r=2) # k�r trace!
var.vec2var.RU.repokix2005$A 
var.vec2var.RU.repokix2005$deterministic
serial.test(var.vec2var.RU.repokix2005) 
# Ingen kvarvarande autokorrelation i residualerna n�r k=2, r<=2 (enligt Trace-testet)

# NORMALITETS TEST: 
normality.test(var.vec2var.RU.repokix2005)$jb.mul$JB 
normality.test(var.vec2var.RU.repokix2005)$jb.mul$Skewness 
normality.test(var.vec2var.RU.repokix2005)$jb.mul$Kurtosis 
# Feltermerna �r normala n�r k=2, r<=2 (enligt Trace-testet)

plot(irf(var.vec2var.RU.repokix2005, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.RU.repokix2005, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2005, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="RU-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.RU.repokix2005, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2005, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


########################################################################################
# RU-MODELLEN
# VARIABLER: RU, FPI, skuld, repo, kix
######################################################################################## 
fevd_var.vec2var.RU.repokix2005  <- fevd(var.vec2var.RU.repokix2005 , n.ahead=16) # h = 4 �r.
print(fevd_var.vec2var.RU.repokix2005)
#################################################################################



# KLAR!
# 2006Q2 - 2018Q2
#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# Byter ordningen p� KIX och Repo; RU, FPI, SKULD, REPO, KIX.
# 
#################################################################################
#################################################################################
var.data.johansen.RU.repokix2006 <- cbind(window(RU_serie, start = c(2006,3)),
                                          window(d.log.FPI_serie, start = c(2006,3)),
                                          window(d.log.Skuldkvot_serie, start = c(2006,3)),
                                          window(Reporanta_serie, start = c(2006,3)),
                                          window(d.log.KIX_serie, start = c(2006,3))) 

dimnames(var.data.johansen.RU.repokix2006)[[2]] <- c("RUlevels","FPIdifflog","Skuldkvotdifflog", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.repokix2006

VARselect(var.data.johansen.RU.repokix2006, type ="const") # SC/BIC s�ger 1 lags
# Bortser ifr�n deterministiska termer s�som "trend" d� variabler �r differens station�ra!

#########################################################################
# JOHANSEN "TRACE" TEST: RU, FPI, skuld, repo, kix
##########################################################################
vecm.t.RU.repokix2006 <- ca.jo(var.data.johansen.RU.repokix2006, K=2, type ="trace")
vecm.t.RU.repokix2006@teststat
vecm.t.RU.repokix2006@cval

# r<=2 n�r k=2

###################################################################
# ESTIMERAR NU VECM av RU-modellen: RU, FPI, skuld, repo, kix
################################################################
vecm.RU.repokix2006 <- cajorls(vecm.t.RU.repokix2006, r=2)
vecm.RU.repokix2006$rlm
vecm.RU.repokix2006$beta

# FR�N VECM TILL VAR
var.vec2var.RU.repokix2006 <- vec2var(vecm.t.RU.repokix2006, r=2) # k�r trace!
var.vec2var.RU.repokix2006$A 
var.vec2var.RU.repokix2006$deterministic
serial.test(var.vec2var.RU.repokix2006) 
# Ingen kvarvarande autokorrelation i residualerna n�r k=2, r<=2 (enligt Trace-testet)

# NORMALITETS TEST: 
normality.test(var.vec2var.RU.repokix2006)$jb.mul$JB 
normality.test(var.vec2var.RU.repokix2006)$jb.mul$Skewness 
normality.test(var.vec2var.RU.repokix2006)$jb.mul$Kurtosis 
# Feltermerna �r normala n�r k=2, r<=2 (enligt Trace-testet)

plot(irf(var.vec2var.RU.repokix2006, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.RU.repokix2006, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2006, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="RU-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.RU.repokix2006, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2006, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


########################################################################################
# RU-MODELLEN
# VARIABLER: RU, FPI, skuld, repo, kix
######################################################################################## 
fevd_var.vec2var.RU.repokix2006  <- fevd(var.vec2var.RU.repokix2006 , n.ahead=16) # h = 4 �r.
print(fevd_var.vec2var.RU.repokix2006 )
#################################################################################



# KLAR!
# 2007Q2 - 2018Q2
#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# Byter ordningen p� KIX och Repo; RU, FPI, SKULD, REPO, KIX.
# 
#################################################################################
#################################################################################
var.data.johansen.RU.repokix2007 <- cbind(window(RU_serie, start = c(2007,3)),
                                          window(d.log.FPI_serie, start = c(2007,3)),
                                          window(d.log.Skuldkvot_serie, start = c(2007,3)),
                                          window(Reporanta_serie, start = c(2007,3)),
                                          window(d.log.KIX_serie, start = c(2007,3))) 

dimnames(var.data.johansen.RU.repokix2007)[[2]] <- c("RUlevels","FPIdifflog","Skuldkvotdifflog", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.repokix2007

VARselect(var.data.johansen.RU.repokix2007, type ="const") # SC/BIC s�ger 1 lags
# Bortser ifr�n deterministiska termer s�som "trend" d� variabler �r differens station�ra!

#########################################################################
# JOHANSEN "TRACE" TEST: RU, FPI, skuld, repo, kix
##########################################################################
vecm.t.RU.repokix2007 <- ca.jo(var.data.johansen.RU.repokix2007, K=2, type ="trace")
vecm.t.RU.repokix2007@teststat
vecm.t.RU.repokix2007@cval

# r<=2 n�r k=2

###################################################################
# ESTIMERAR NU VECM av RU-modellen: RU, FPI, skuld, repo, kix
################################################################
vecm.RU.repokix2007 <- cajorls(vecm.t.RU.repokix2007, r=2)
vecm.RU.repokix2007$rlm
vecm.RU.repokix2007$beta

# FR�N VECM TILL VAR
var.vec2var.RU.repokix2007 <- vec2var(vecm.t.RU.repokix2007, r=2) # k�r trace!
var.vec2var.RU.repokix2007$A 
var.vec2var.RU.repokix2007$deterministic
serial.test(var.vec2var.RU.repokix2007) 
# Ingen kvarvarande autokorrelation i residualerna n�r k=2, r<=2 (enligt Trace-testet)

# NORMALITETS TEST: 
normality.test(var.vec2var.RU.repokix2007)$jb.mul$JB 
normality.test(var.vec2var.RU.repokix2007)$jb.mul$Skewness 
normality.test(var.vec2var.RU.repokix2007)$jb.mul$Kurtosis 
# Feltermerna �r normala n�r k=2, r<=2 (enligt Trace-testet)

plot(irf(var.vec2var.RU.repokix2007, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.RU.repokix2007, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2007, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="RU-Model; Orthogonal Impulse Reponse from Reporate" )
plot(irf(var.vec2var.RU.repokix2007, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.RU.repokix2007, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


########################################################################################
# RU-MODELLEN
# VARIABLER: RU, FPI, skuld, repo, kix
######################################################################################## 
fevd_var.vec2var.RU.repokix2007  <- fevd(var.vec2var.RU.repokix2007 , n.ahead=16) # h = 4 �r.
print(fevd_var.vec2var.RU.repokix2007)
############################################################################################





#------------------------- *ARBL12-MODELLEN MED OLIKA ESTIMATIONSPERIODER.

# * FORTS�TTNING P� ROBUSTNESS CHECKS MED KIX I SLUTET: ARBL12-MODELLEN MED OLIKA ESTIMATIONSPERIODER.

#  �NDRADE START TIDER P� URSPRUNGSMODELLERNA: 1999Q2, 2000Q1, 2005Q2, 2006Q2 & 2007Q2
#  D�R URSPRUNGSMODELLERNA �R (denna ordning): ARBL12, FPI, SKULD, REPOR�NTA, KIX


# KLAR!
# 1999Q2 - 2018Q2
#############################################################################
#  MODELL MED "ARBL12" med KIX sist
# 
# ARBL12stand, FPI, SKULD, repo, kix
#############################################################################
var.data.johansen.ARBL12.repokix1999 <- cbind(window(ARBL12.STAND_serie, start = c(1999,3)),
                                              window(d.log.FPI_serie, start = c(1999,3)),
                                              window(d.log.Skuldkvot_serie, start = c(1999,3)),
                                              window(Reporanta_serie, start = c(1999,3)),
                                              window(d.log.KIX_serie, start = c(1999,3))) 

dimnames(var.data.johansen.ARBL12.repokix1999)[[2]] <- c("ARBL12stand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels", "KIXdifflog")
var.data.johansen.ARBL12.repokix1999

VARselect(var.data.johansen.ARBL12.repokix1999, type ="const") # SC/BIC s�ger 1 lagg

#########################################################################
# JOHANSEN "TRACE" TEST: arbl12 modellen med kix sist
##########################################################################
vecm.t.ARBL12.repokix1999 <- ca.jo(var.data.johansen.ARBL12.repokix1999, K=3, type ="trace")
vecm.t.ARBL12.repokix1999@teststat
vecm.t.ARBL12.repokix1999@cval

# r=2 n�r k=3 optimalast.


################################################################
# ESTIMERAR NU VECM: med Kix sist.
################################################################
vecm.ARBL12.repokix1999 <- cajorls(vecm.t.ARBL12.repokix1999, r=2)
vecm.ARBL12.repokix1999$rlm
vecm.ARBL12.repokix1999$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.repokix1999 <- vec2var(vecm.t.ARBL12.repokix1999, r=2) # k�r trace!
var.vec2var.ARBL12.repokix1999$A 
var.vec2var.ARBL12.repokix1999$deterministic
serial.test(var.vec2var.ARBL12.repokix1999) #  
# Ingen kvarvarande autokorr i resid n�r k=3 och r<=2

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.repokix1999)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.repokix1999)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.repokix1999)$jb.mul$Kurtosis 
# normala resid n�r k=3 och r=2

plot(irf(var.vec2var.ARBL12.repokix1999, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.ARBL12.repokix1999, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix1999, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="ARBL12-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.ARBL12.repokix1999, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix1999, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


################################################################################################ 
# ARBL12-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: ARBL12 (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
################################################################################################  
fevd_var.result.ARBL12.repokix1999 <- fevd(var.vec2var.ARBL12.repokix1999, n.ahead=16) # h = 4 �r.
print(fevd_var.result.ARBL12.repokix1999)

#################################################################################


# KLAR!
# 2000Q2 - 2018Q2
#############################################################################
#  MODELL MED "ARBL12" med KIX sist
# 
# ARBL12stand, FPI, SKULD, repo, kix
#############################################################################
var.data.johansen.ARBL12.repokix2000 <- cbind(window(ARBL12.STAND_serie, start = c(2000,3)),
                                              window(d.log.FPI_serie, start = c(2000,3)),
                                              window(d.log.Skuldkvot_serie, start = c(2000,3)),
                                              window(Reporanta_serie, start = c(2000,3)),
                                              window(d.log.KIX_serie, start = c(2000,3))) 

dimnames(var.data.johansen.ARBL12.repokix2000)[[2]] <- c("ARBL12stand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels", "KIXdifflog")
var.data.johansen.ARBL12.repokix2000

VARselect(var.data.johansen.ARBL12.repokix2000, type ="const") # SC/BIC s�ger 1 lagg

#########################################################################
# JOHANSEN "TRACE" TEST: arbl12 modellen med kix sist
##########################################################################
vecm.t.ARBL12.repokix2000 <- ca.jo(var.data.johansen.ARBL12.repokix2000, K=2, type ="trace")
vecm.t.ARBL12.repokix2000@teststat
vecm.t.ARBL12.repokix2000@cval

# r<=3, n�r k=2 

################################################################
# ESTIMERAR NU VECM: med Kix sist.
################################################################
vecm.ARBL12.repokix2000 <- cajorls(vecm.t.ARBL12.repokix2000, r=3)
vecm.ARBL12.repokix2000$rlm
vecm.ARBL12.repokix2000$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.repokix2000 <- vec2var(vecm.t.ARBL12.repokix2000, r=3) # k�r trace!
var.vec2var.ARBL12.repokix2000$A 
var.vec2var.ARBL12.repokix2000$deterministic
serial.test(var.vec2var.ARBL12.repokix2000) #  
# Ingen kvarvarande autokorr i resid n�r k=2 och r<=3

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.repokix2000)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.repokix2000)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.repokix2000)$jb.mul$Kurtosis 

plot(irf(var.vec2var.ARBL12.repokix2000, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.ARBL12.repokix2000, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2000, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="ARBL12-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.ARBL12.repokix2000, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2000, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


################################################################################################ 
# ARBL12-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: ARBL12 (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
################################################################################################  
fevd_var.result.ARBL12.repokix2000 <- fevd(var.vec2var.ARBL12.repokix2000, n.ahead=16) # h = 4 �r.
print(fevd_var.result.ARBL12.repokix2000)

#################################################################################



# KLAR!
# 2005Q2 - 2018Q2
#############################################################################
#  MODELL MED "ARBL12" med KIX sist
# 
# ARBL12stand, FPI, SKULD, repo, kix
#############################################################################
var.data.johansen.ARBL12.repokix2005 <- cbind(window(ARBL12.STAND_serie, start = c(2005,3)),
                                              window(d.log.FPI_serie, start = c(2005,3)),
                                              window(d.log.Skuldkvot_serie, start = c(2005,3)),
                                              window(Reporanta_serie, start = c(2005,3)),
                                              window(d.log.KIX_serie, start = c(2005,3))) 

dimnames(var.data.johansen.ARBL12.repokix2005)[[2]] <- c("ARBL12stand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels", "KIXdifflog")
var.data.johansen.ARBL12.repokix2005

VARselect(var.data.johansen.ARBL12.repokix2005, type ="const") # SC/BIC s�ger 1 lagg

#########################################################################
# JOHANSEN "TRACE" TEST: arbl12 modellen med kix sist
##########################################################################
vecm.t.ARBL12.repokix2005 <- ca.jo(var.data.johansen.ARBL12.repokix2005, K=2, type ="trace")
vecm.t.ARBL12.repokix2005@teststat
vecm.t.ARBL12.repokix2005@cval

# r<=3, n�r k=2 

################################################################
# ESTIMERAR NU VECM: med Kix sist.
################################################################
vecm.ARBL12.repokix2005 <- cajorls(vecm.t.ARBL12.repokix2005, r=3)
vecm.ARBL12.repokix2005$rlm
vecm.ARBL12.repokix2005$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.repokix2005 <- vec2var(vecm.t.ARBL12.repokix2005, r=3) # k�r trace!
var.vec2var.ARBL12.repokix2005$A 
var.vec2var.ARBL12.repokix2005$deterministic
serial.test(var.vec2var.ARBL12.repokix2005) #  
# Ingen kvarvarande autokorr i resid n�r k=2 och r<=3

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.repokix2005)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.repokix2005)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.repokix2005)$jb.mul$Kurtosis 

plot(irf(var.vec2var.ARBL12.repokix2005, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.ARBL12.repokix2005, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2005, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="ARBL12-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.ARBL12.repokix2005, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2005, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


################################################################################################ 
# ARBL12-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: ARBL12 (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
################################################################################################  
fevd_var.result.ARBL12.repokix2005 <- fevd(var.vec2var.ARBL12.repokix2005, n.ahead=16) # h = 4 �r.
print(fevd_var.result.ARBL12.repokix2005)

#################################################################################



# KLAR!
# 2006Q2 - 2018Q2
#############################################################################
#  MODELL MED "ARBL12" med KIX sist
# 
# ARBL12stand, FPI, SKULD, repo, kix
#############################################################################
var.data.johansen.ARBL12.repokix2006 <- cbind(window(ARBL12.STAND_serie, start = c(2006,3)),
                                              window(d.log.FPI_serie, start = c(2006,3)),
                                              window(d.log.Skuldkvot_serie, start = c(2006,3)),
                                              window(Reporanta_serie, start = c(2006,3)),
                                              window(d.log.KIX_serie, start = c(2006,3))) 

dimnames(var.data.johansen.ARBL12.repokix2006)[[2]] <- c("ARBL12stand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels", "KIXdifflog")
var.data.johansen.ARBL12.repokix2006

VARselect(var.data.johansen.ARBL12.repokix2006, type ="const") # SC/BIC s�ger 1 lagg

#########################################################################
# JOHANSEN "TRACE" TEST: arbl12 modellen med kix sist
##########################################################################
vecm.t.ARBL12.repokix2006 <- ca.jo(var.data.johansen.ARBL12.repokix2006, K=2, type ="trace")
vecm.t.ARBL12.repokix2006@teststat
vecm.t.ARBL12.repokix2006@cval

# r<=3, n�r k=2 

################################################################
# ESTIMERAR NU VECM: med Kix sist.
################################################################
vecm.ARBL12.repokix2006 <- cajorls(vecm.t.ARBL12.repokix2006, r=3)
vecm.ARBL12.repokix2006$rlm
vecm.ARBL12.repokix2006$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.repokix2006 <- vec2var(vecm.t.ARBL12.repokix2006, r=3) # k�r trace!
var.vec2var.ARBL12.repokix2006$A 
var.vec2var.ARBL12.repokix2006$deterministic
serial.test(var.vec2var.ARBL12.repokix2006) #  
# Ingen kvarvarande autokorr i resid n�r k=2 och r<=3

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.repokix2006)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.repokix2006)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.repokix2006)$jb.mul$Kurtosis 

plot(irf(var.vec2var.ARBL12.repokix2006, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.ARBL12.repokix2006, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2006, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="ARBL12-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.ARBL12.repokix2006, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2006, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


################################################################################################ 
# ARBL12-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: ARBL12 (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
################################################################################################  
fevd_var.result.ARBL12.repokix2006 <- fevd(var.vec2var.ARBL12.repokix2006, n.ahead=16) # h = 4 �r.
print(fevd_var.result.ARBL12.repokix2006)
#################################################################################
#################################################################################




# 2007Q2 - 2018Q2
#############################################################################
#  MODELL MED "ARBL12" med KIX sist
# 
# ARBL12stand, FPI, SKULD, repo, kix
#############################################################################
var.data.johansen.ARBL12.repokix2007 <- cbind(window(ARBL12.STAND_serie, start = c(2007,3)),
                                              window(d.log.FPI_serie, start = c(2007,3)),
                                              window(d.log.Skuldkvot_serie, start = c(2007,3)),
                                              window(Reporanta_serie, start = c(2007,3)),
                                              window(d.log.KIX_serie, start = c(2007,3))) 

dimnames(var.data.johansen.ARBL12.repokix2007)[[2]] <- c("ARBL12stand","FPIdifflog", "Skuldkvotdifflog","Reporantalevels", "KIXdifflog")
var.data.johansen.ARBL12.repokix2007

VARselect(var.data.johansen.ARBL12.repokix2007, type ="const") # SC/BIC s�ger 1 lagg

#########################################################################
# JOHANSEN "TRACE" TEST: arbl12 modellen med kix sist
##########################################################################
vecm.t.ARBL12.repokix2007 <- ca.jo(var.data.johansen.ARBL12.repokix2007, K=2, type ="trace")
vecm.t.ARBL12.repokix2007@teststat
vecm.t.ARBL12.repokix2007@cval

# r<=3, n�r k=2 

################################################################
# ESTIMERAR NU VECM: med Kix sist.
################################################################
vecm.ARBL12.repokix2007 <- cajorls(vecm.t.ARBL12.repokix2007, r=3)
vecm.ARBL12.repokix2007$rlm
vecm.ARBL12.repokix2007$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.repokix2007 <- vec2var(vecm.t.ARBL12.repokix2007, r=3) # k�r trace!
var.vec2var.ARBL12.repokix2007$A 
var.vec2var.ARBL12.repokix2007$deterministic
serial.test(var.vec2var.ARBL12.repokix2007) #  
# Ingen kvarvarande autokorr i resid n�r k=2 och r<=3

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.repokix2007)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.repokix2007)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.repokix2007)$jb.mul$Kurtosis 

plot(irf(var.vec2var.ARBL12.repokix2007, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed = 4654)) 
plot(irf(var.vec2var.ARBL12.repokix2007, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2007, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="ARBL12-Model; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.ARBL12.repokix2007, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed = 4654))
plot(irf(var.vec2var.ARBL12.repokix2007, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed = 4654))


################################################################################################ 
# ARBL12-MODELLEN (med "diff log KIX") FEVD/VARIANCE DECOMPOSITION
# VARIABLER: ARBL12 (NIV�), diff log FPI, diff log SKULDKVOT, diff log KIX,  Repor�nta (niv�)
################################################################################################  
fevd_var.result.ARBL12.repokix2007 <- fevd(var.vec2var.ARBL12.repokix2007, n.ahead=16) # h = 4 �r.
print(fevd_var.result.ARBL12.repokix2007)

################################################################################################




# KLAR!
# ----------------------- FORTS�TTNING P� ROBUSTNESS.
# ALTERNATIVA ORDNINGAR, SEDVANLIGA ESTIMATIONS PERIOD: 1996Q2 - 2018Q2: 

# FPI, SKULD, RU, REPO, KIX
#########################################################################################################
# ** ALTERNATIVA ORDNINGAR, ESTIMATIONS PERIOD: 1996Q2 - 2018Q2: KLAR!
#
# FPI, SKULD, RU, REPO, KIX (d� RU har en del arbetsmarknadsdata antar vi 
# att RU reagerar med ett lag till en penningpolitisk chock)
#########################################################################################################

#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# Byter ordningen p� KIX och Repo; FPI, SKULD, RU, REPO, KIX
#################################################################################
var.data.johansen.RU.RUmitt <- cbind(window(d.log.FPI_serie, start = c(1996,3)),
                                     window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                     window(RU_serie, start = c(1996,3)),
                                     window(Reporanta_serie, start = c(1996,3)),
                                     window(d.log.KIX_serie, start = c(1996,3))) 

dimnames(var.data.johansen.RU.RUmitt)[[2]] <- c("FPIdifflog","Skuldkvotdifflog","RUlevels", "Reporantalevels", "KIXdifflog")
var.data.johansen.RU.RUmitt

VARselect(var.data.johansen.RU.RUmitt, type ="const") # SC/BIC s�ger 1 lags

#########################################################################
# JOHANSEN "TRACE" TEST: FPI, SKULD, RU, REPO, KIX
##########################################################################
vecm.t.RU.RUmitt <- ca.jo(var.data.johansen.RU.RUmitt, K=4, type ="trace")
vecm.t.RU.RUmitt@teststat
vecm.t.RU.RUmitt@cval

# r<=3, n�r k=2
# r<=3 n�r k=3
# r=0 n�r k=4. ESTIMERA EN VAR!


# G�r vidare med att estimera en VAR-modell.
var.result.johansen.RU.RUmitt<- VAR(var.data.johansen.RU.RUmitt, p = 3, type ="const") 
summary(var.result.johansen.RU.RUmitt)
serial.test(var.result.johansen.RU.RUmitt) # Ingen autokorr!
# ingen kvarvarande autokorr n�r p=3

# TEST NORMALITET. 
normality.test(var.result.johansen.RU.RUmitt)$jb.mul$JB
normality.test(var.result.johansen.RU.RUmitt)$jb.mul$Skewness
normality.test(var.result.johansen.RU.RUmitt)$jb.mul$Kurtosis
# problem med icke normalitet n�r p=2
# normala residualer n�r p=3


################################################
# IRF VAR: FPI, SKULD, RU, REPO, KIX
################################################

(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed=4654)) 
(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed=4654))
(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654))
(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed=4654))
(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed=4654))


plot(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed=4654)) 
plot(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed=4654))
plot(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="FPI, Debt, RU, Repo, KIX; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed=4654))
plot(irf(var.result.johansen.RU.RUmitt, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed=4654))

################################################
# FEVD VAR: FPI, SKULD, RU, REPO, KIX
################################################
fevd(var.result.johansen.RU.RUmitt, n.ahead=16)$Skuldkvotdifflog
################################################





# KLAR!
#  * MODELLORDNING:  REPO, FPI, SKULD,  KIX, RU
#################################################################################
# ** ALTERNATIVA ORDNINGAR, ESTIMATIONS PERIOD: 1996Q2 - 2018Q2: 
# 
# REPO, FPI, SKULD, RU, KIX (d� RU har en del arbetsmarknadsdata antar vi 
# att RU reagerar med ett lag till en penningpolitisk chock)
#################################################################################

#################################################################################
var.data.johansen.RU.repofirst <- cbind(window(Reporanta_serie, start = c(1996,3)),
                                        window(d.log.FPI_serie, start = c(1996,3)),
                                        window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                        window(d.log.KIX_serie, start = c(1996,3)),
                                        window(RU_serie, start = c(1996,3))) 

dimnames(var.data.johansen.RU.repofirst)[[2]] <- c("Reporantalevels","FPIdifflog","Skuldkvotdifflog", "KIXdifflog", "RUlevels")
var.data.johansen.RU.repofirst

VARselect(var.data.johansen.RU.repofirst, type ="const") # SC/BIC s�ger 1 lags

#########################################################################
# JOHANSEN "TRACE" TEST: REPO, FPI, SKULD,  KIX, RU
##########################################################################
vecm.t.RU.repofirst <- ca.jo(var.data.johansen.RU.repofirst, K=4, type ="trace")
vecm.t.RU.repofirst@teststat
vecm.t.RU.repofirst@cval

# r<=3, n�r k=2
# r<=3 n�r k=3
# r=0 n�r k=4.


# G�r vidare med att estimera en VAR-modell: REPO, FPI, SKULD,  KIX, RU
var.result.johansen.RU.repofirst <- VAR(var.data.johansen.RU.repofirst, p = 4, type ="const") 
summary(var.result.johansen.RU.repofirst)
serial.test(var.result.johansen.RU.repofirst) # Ingen autokorr!
# ingen kvarvarande autokorr n�r p=2 genom VAR(2) simulering.

# TEST NORMALITET. 
normality.test(var.result.johansen.RU.repofirst)$jb.mul$JB
normality.test(var.result.johansen.RU.repofirst)$jb.mul$Skewness
normality.test(var.result.johansen.RU.repofirst)$jb.mul$Kurtosis
# problem med icke normalitet n�r p=2
# normala residualer n�r p=4


################################################
# IRF VAR: REPO, FPI, SKULD,  KIX, RU
################################################
(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16, seed=4654)) 
(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16))
(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed=4654))
(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16))
(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16))


plot(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("RUlevels"), n.ahead=16)) 
plot(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16))
plot(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed = 4654), ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="REPO, FPI, SKULD, RU, KIX; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16))
plot(irf(var.result.johansen.RU.repofirst, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16))

################################################
# FEVD VAR: FPI, SKULD, RU, REPO, KIX
################################################
fevd(var.result.johansen.RU.repofirst, n.ahead=16)$Skuldkvotdifflog
################################################



# KLAR!
# MODELL: REPO, FPI, SKULD, KIX, ARBL12
#################################################################################
# ** ALTERNATIVA ORDNINGAR, ESTIMATIONS PERIOD: 1996Q2 - 2018Q2: 
# 
# REPO, FPI, SKULD, ARBL12, KIX 
#################################################################################

#################################################################################
#################################################################################
# ALTERNATIVA ORDNINGAR MOTIVERAT MED EKONOMISK TEORI:
# REPO, FPI, SKULD, KIX, ARBL12
#################################################################################
#################################################################################
var.data.johansen.ARBL12.repofirst <- cbind(window(Reporanta_serie, start = c(1996,3)),
                                            window(d.log.FPI_serie, start = c(1996,3)),
                                            window(d.log.Skuldkvot_serie, start = c(1996,3)),
                                            window(d.log.KIX_serie, start = c(1996,3)),
                                            window(ARBL12.STAND_serie, start = c(1996,3))) 

dimnames(var.data.johansen.ARBL12.repofirst)[[2]] <- c("Reporantalevels","FPIdifflog","Skuldkvotdifflog", "KIXdifflog", "ARBL12stand")
var.data.johansen.ARBL12.repofirst

VARselect(var.data.johansen.ARBL12.repofirst, type ="const") # SC/BIC s�ger 1 lags

#########################################################################
# JOHANSEN "TRACE" TEST: FPI, SKULD, RU, REPO, KIX
##########################################################################
vecm.t.ARBL12.repofirst <- ca.jo(var.data.johansen.ARBL12.repofirst, K=4, type ="trace", ecdet="trend")
vecm.t.ARBL12.repofirst@teststat
vecm.t.ARBL12.repofirst@cval

# r<=4 n�r k=2
# r<=2 n�r k=3
# r<=2 n�r k=4
# r<=1 n�r k=5

###################################################################
# ESTIMERAR NU VECM av RU-modellen: FPI, SKULD, RU, REPO, KIX
################################################################
vecm.ARBL12.repofirst <- cajorls(vecm.t.ARBL12.repofirst, r=2)
vecm.ARBL12.repofirst$rlm
vecm.ARBL12.repofirst$beta

# FR�N VECM TILL VAR
var.vec2var.ARBL12.repofirst <- vec2var(vecm.t.ARBL12.repofirst, r=2) # k�r trace!
var.vec2var.ARBL12.repofirst$A 
var.vec2var.ARBL12.repofirst$deterministic
serial.test(var.vec2var.ARBL12.repofirst) 
# Ingen kvarvarande autokorrelation i residualerna n�r k=2, r<=3 

# NORMALITETS TEST: 
normality.test(var.vec2var.ARBL12.repofirst)$jb.mul$JB 
normality.test(var.vec2var.ARBL12.repofirst)$jb.mul$Skewness 
normality.test(var.vec2var.ARBL12.repofirst)$jb.mul$Kurtosis 


irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", response=("ARBL12stand"), n.ahead=16, seed=4654)
irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed=4654)
irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed=4654) 
irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed=4654)
irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels",  respons=c("Reporantalevels"), n.ahead=16, seed=4654)


plot(irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("ARBL12stand"), n.ahead=16, seed=4654)) 
plot(irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("FPIdifflog"), n.ahead=16, seed=4654))
plot(irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("Skuldkvotdifflog"), n.ahead=16, seed=4654), 
     ylab="Debt Growth Rate (%)",
     ylim=c(-0.003,0.003), main ="REPO, FPI, SKULD, KIX, ARBL12; Orthogonal Impulse Reponse from Reporate")
plot(irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("KIXdifflog"), n.ahead=16, seed=4654))
plot(irf(var.vec2var.ARBL12.repofirst, impulse="Reporantalevels", respons=c("Reporantalevels"), n.ahead=16, seed=4654))

###########################################################################
# MODELL 2: FEVD/VARIANCE DECOMPOSITION
# VARIABLER: REPO, FPI, SKULD, KIX, ARBL12
####################################################################
fevd_var.vec2var.ARBL12.repofirst <- fevd(var.vec2var.ARBL12.repofirst, n.ahead=16) # h = 4 �r.
print(fevd_var.vec2var.ARBL12.repofirst)

####################################################################

####################################################################
# END
####################################################################