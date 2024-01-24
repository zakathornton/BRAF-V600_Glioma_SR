#Load relevant modules
library(readxl)

#Load data extraction file. 
data<-read_xlsx("Data Extraction.xlsx")

#Only include real PFS data
data<-data[data$`PFS - Censored (0) or Real (1)`==1,]
data$`Progression Free Survival (PFS)`<-as.numeric(data$`Progression Free Survival (PFS)`)

#Separate into adult and paediatric groups
adult<-data[data$`Paediatric or Adult (>18)`=="Adult",]
paed<-data[data$`Paediatric or Adult (>18)`=="Paediatric",]

#Separate into LG and HG groups
LG<-data[data$`Tumour Grade (HG/LG)`=="LG",]
HG<-data[data$`Tumour Grade (HG/LG)`=="HG",]

#Further subset adult and paediatric by glioma grade
pLG<-paed[paed$`Tumour Grade (HG/LG)`=="LG",]
pHG<-paed[paed$`Tumour Grade (HG/LG)`=="HG",]
aLG<-adult[adult$`Tumour Grade (HG/LG)`=="LG",]
aHG<-adult[adult$`Tumour Grade (HG/LG)`=="HG",]

#Further subset adult and paediatric by monotherapy and dual therapy
pMono<-paed[paed$`MEKi Therapy (Y/N)`=="No",]
pDual<-paed[paed$`MEKi Therapy (Y/N)`=="Yes",]
aMono<-adult[adult$`MEKi Therapy (Y/N)`=="No",]
aDual<-adult[adult$`MEKi Therapy (Y/N)`=="Yes",]

#Further subset paediatric-glioma grade by monotherapy and dual therapy
pLGMono<-pLG[pLG$`MEKi Therapy (Y/N)`=="No",]
pLGDual<-pLG[pLG$`MEKi Therapy (Y/N)`=="Yes",]
pHGMono<-pHG[pHG$`MEKi Therapy (Y/N)`=="No",]
pHGDual<-pHG[pHG$`MEKi Therapy (Y/N)`=="Yes",]

#Further subset adult-glioma grade by monotherapy and dual therapy
aLGMono<-aLG[aLG$`MEKi Therapy (Y/N)`=="No",]
aLGDual<-aLG[aLG$`MEKi Therapy (Y/N)`=="Yes",]
aHGMono<-aHG[aHG$`MEKi Therapy (Y/N)`=="No",]
aHGDual<-aHG[aHG$`MEKi Therapy (Y/N)`=="Yes",]

#Perform Wilcoxon-Mann U tests to determine significant differences.
wilcox.test(paed$`Progression Free Survival (PFS)`,adult$`Progression Free Survival (PFS)`)
wilcox.test(LG$`Progression Free Survival (PFS)`, HG$`Progression Free Survival (PFS)`)
wilcox.test(pLG$`Progression Free Survival (PFS)`, pHG$`Progression Free Survival (PFS)`)
wilcox.test(aLG$`Progression Free Survival (PFS)`, aHG$`Progression Free Survival (PFS)`)
wilcox.test(pMono$`Progression Free Survival (PFS)`, pDual$`Progression Free Survival (PFS)`)
wilcox.test(aMono$`Progression Free Survival (PFS)`, aDual$`Progression Free Survival (PFS)`)
wilcox.test(pLGMono$`Progression Free Survival (PFS)`, pLGDual$`Progression Free Survival (PFS)`)
wilcox.test(aLGMono$`Progression Free Survival (PFS)`, pLGDual$`Progression Free Survival (PFS)`)
wilcox.test(aHGMono$`Progression Free Survival (PFS)`, aHGDual$`Progression Free Survival (PFS)`)
