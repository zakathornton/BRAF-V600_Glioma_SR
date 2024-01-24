#Load relevant modules
library(readxl)

#Load data extraction file. 
data<-read_xlsx("Data Extraction.xlsx")

#Only include real PFS data
data<-data[data$`OS - Censored (0) or Death (1)`==1,]
data$`Overall Survival (OS)`<-as.numeric(data$`Overall Survival (OS)`)

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

#Further subset adult and paediatric by monotheraphy and dual therapy
pMono<-paed[paed$`MEKi Therapy (Y/N)`=="No",]
pDual<-paed[paed$`MEKi Therapy (Y/N)`=="Yes",]
aMono<-adult[adult$`MEKi Therapy (Y/N)`=="No",]
aDual<-adult[adult$`MEKi Therapy (Y/N)`=="Yes",]

#Perform Wilcoxon-Mann U tests to determine significant differences. Some are not done because of insufficient sample sizes
wilcox.test(paed$`Overall Survival (OS)`, adult$`Overall Survival (OS)`)
wilcox.test(LG$`Overall Survival (OS)`, HG$`Overall Survival (OS)`)
wilcox.test(aLG$`Overall Survival (OS)`, aHG$`Overall Survival (OS)`)
wilcox.test(aMono$`Overall Survival (OS)`, aDual$`Overall Survival (OS)`)
