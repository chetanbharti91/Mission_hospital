



library(car)
library(caret)
library(caTools)
library(corrplot)

Hospital<- read.csv("MHD.csv")

str(Hospital)
#Treat the missing values 
which(is.na(Hospital$BP..HIGH))
Hospital$BP..HIGH[is.na(Hospital$BP..HIGH)] <- mean(Hospital$BP..HIGH, na.rm = TRUE)
which(is.na(Hospital$BP.LOW))
Hospital$BP.LOW[is.na(Hospital$BP.LOW)] <- mean(Hospital$BP.LOW, na.rm = TRUE)
which(is.na(Hospital$HB))
Hospital$HB[is.na(Hospital$HB)] <- mean(Hospital$HB, na.rm = TRUE)
which(is.na(Hospital$UREA))
Hospital$UREA[is.na(Hospital$UREA)] <- median(Hospital$UREA, na.rm = TRUE)
which(is.na(Hospital$CREATININE))
Hospital$CREATININE[is.na(Hospital$CREATININE)] <- median(Hospital$CREATININE, na.rm = TRUE)



#building SLR model;


# Reverse regression model

model19<- lm(TOTAL.COST.TO.HOSPITAL~AGE+MALE+UNMARRIED+ACHD+CAD.DVD+CAD.SVD+CAD.TVD+CAD.VSD+OS.ASD+other..heart+other..respiratory+other.general+other.nervous+other.tertalogy+PM.VSD+
               RHD+BODY.HEIGHT+HR.PULSE+BP..HIGH+BP.LOW+RR+Diabetes1+Diabetes2+hypertension1+hypertension2+hypertension3+other+HB+UREA+CREATININE+AMBULANCE+TRANSFERRED+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+IMPLANT+COST.OF.IMPLANT,data= Hospital)

summary(model19)

model20<- lm(TOTAL.COST.TO.HOSPITAL~AGE+MALE+UNMARRIED+ACHD+CAD.DVD+CAD.SVD+CAD.TVD+CAD.VSD+OS.ASD+other..heart+other..respiratory+other.general+other.nervous+other.tertalogy+PM.VSD+
               RHD+BODY.HEIGHT+HR.PULSE+Diabetes2+hypertension1+hypertension2+hypertension3+other+HB+UREA+CREATININE+AMBULANCE+TRANSFERRED+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+IMPLANT+COST.OF.IMPLANT,data= Hospital)
summary(model20)



model21<- lm(TOTAL.COST.TO.HOSPITAL~AGE+MALE+UNMARRIED+ACHD+CAD.DVD+CAD.SVD+CAD.TVD+CAD.VSD+OS.ASD+other..heart+other..respiratory+other.general+other.nervous+other.tertalogy+PM.VSD+
               RHD+BODY.HEIGHT+HR.PULSE+Diabetes2+hypertension1+hypertension2+hypertension3+other+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)

summary(model21)

model22<- lm(TOTAL.COST.TO.HOSPITAL~UNMARRIED+ACHD+CAD.DVD+CAD.SVD+CAD.TVD+CAD.VSD+OS.ASD+other..heart+other..respiratory+other.nervous+other.tertalogy+PM.VSD+
               RHD+BODY.HEIGHT+HR.PULSE+Diabetes2+hypertension1+hypertension2+hypertension3+other+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)

summary(model22)

model23<-lm(TOTAL.COST.TO.HOSPITAL~UNMARRIED+ACHD+CAD.DVD+CAD.SVD+CAD.TVD+CAD.VSD+OS.ASD+other..heart+other..respiratory+other.nervous+other.tertalogy+PM.VSD+
              RHD+BODY.HEIGHT+HR.PULSE+Diabetes2+hypertension2+hypertension3+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model23)

model24<- lm(TOTAL.COST.TO.HOSPITAL~UNMARRIED+ACHD+CAD.TVD+CAD.VSD+OS.ASD+other..heart+other..respiratory+other.nervous+other.tertalogy+PM.VSD+
               +BODY.HEIGHT+Diabetes2+hypertension2+hypertension3+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model24)

model25<- lm(TOTAL.COST.TO.HOSPITAL~UNMARRIED+ACHD+CAD.TVD+OS.ASD+other..heart+other..respiratory+other.tertalogy+PM.VSD+
               +BODY.HEIGHT+Diabetes2+hypertension2+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model25)

model26<-lm(TOTAL.COST.TO.HOSPITAL~UNMARRIED+ACHD+CAD.TVD+OS.ASD+other.tertalogy+PM.VSD+
              +BODY.HEIGHT+Diabetes2+hypertension2+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model26)

model27 <- lm(TOTAL.COST.TO.HOSPITAL~UNMARRIED+CAD.TVD+OS.ASD+other.tertalogy+PM.VSD+
                +Diabetes2+hypertension2+HB+UREA+CREATININE+AMBULANCE+ALERT+ELECTIVE+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model27)


model28<- lm(TOTAL.COST.TO.HOSPITAL~CAD.TVD+OS.ASD+other.tertalogy+PM.VSD+
               +Diabetes2+hypertension2+HB+UREA+CREATININE+ALERT+TOTAL.LENGTH.OF.STAY+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model28)

c<- cor(Hospital[c("CAD.TVD",
                   "OS.ASD","other.tertalogy",	
                   "PM.VSD",	
                   "Diabetes2",
                   "hypertension2",	"HB",	"UREA",
                   "CREATININE",	"ALERT",
                   "TOTAL.LENGTH.OF.STAY",	"LENGTH.OF.STAY...ICU",	"LENGTH.OF.STAY..WARD",	"IMPLANT", 	"COST.OF.IMPLANT")])

corrplot(c,method = "number",type="lower")


model29<- lm(TOTAL.COST.TO.HOSPITAL~CAD.TVD+OS.ASD+other.tertalogy+PM.VSD+
               +Diabetes2+hypertension2+HB+UREA+CREATININE+ALERT+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model29)

c1<- cor(Hospital[c("CAD.TVD",
                    "OS.ASD","other.tertalogy",	
                    "PM.VSD",	
                    "Diabetes2",
                    "hypertension2",	"HB",	"UREA",
                    "CREATININE",	"ALERT",
                    "LENGTH.OF.STAY...ICU",	"LENGTH.OF.STAY..WARD",	"IMPLANT", 	"COST.OF.IMPLANT")])

corrplot(c1,method = "number",type="lower")

model29<- lm(TOTAL.COST.TO.HOSPITAL~OS.ASD+other.tertalogy+PM.VSD+
               +Diabetes2+hypertension2+HB+UREA+CREATININE+ALERT+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model29)

c2<- cor(Hospital[c(
  "OS.ASD","other.tertalogy",	
  "PM.VSD",	
  "Diabetes2",
  "hypertension2",	"HB",	"UREA",
  "CREATININE",	"ALERT",
  "LENGTH.OF.STAY...ICU",	"LENGTH.OF.STAY..WARD", 	"COST.OF.IMPLANT")])

corrplot(c2,method = "number",type="lower")

model30<- lm(TOTAL.COST.TO.HOSPITAL~CAD.TVD+OS.ASD+other.tertalogy+PM.VSD+
               +Diabetes2+hypertension2+HB+CREATININE+ALERT+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model30)

c3<- cor(Hospital[c(
  "OS.ASD","other.tertalogy",	
  "PM.VSD",	
  "Diabetes2",
  "hypertension2",	"HB",
  "CREATININE",	"ALERT",
  "LENGTH.OF.STAY...ICU",	"LENGTH.OF.STAY..WARD", 	"COST.OF.IMPLANT")])

corrplot(c3,method = "number",type="lower")

model31<- lm(TOTAL.COST.TO.HOSPITAL~other.tertalogy+
               +Diabetes2
             +CREATININE+LENGTH.OF.STAY...ICU+LENGTH.OF.STAY..WARD+COST.OF.IMPLANT,data= Hospital)
summary(model31)



