#Load relevant modules
library(survminer)
library(survival)
library(ggplot2)
library(readxl)

#Load data extraction file.
data<-read_xlsx("Data Extraction.xlsx")

#Categorise data by age group or NR (not reported).
paed<-data[data$`Paediatric or Adult (>18)`=="Paediatric",]
adult<-data[data$`Paediatric or Adult (>18)`=="Adult",]

#Remove any data where the progression free survival is not reported.
paed<-paed[paed$`Progression Free Survival (PFS)`!="NR",]
adult<-adult[adult$`Progression Free Survival (PFS)`!="NR",]

#Format PFS as numeric
paed$`Progression Free Survival (PFS)`<-as.numeric(paed$`Progression Free Survival (PFS)`)
adult$`Progression Free Survival (PFS)`<-as.numeric(adult$`Progression Free Survival (PFS)`)

#Remove any data where the tumour is not graded as high- or low-grade glioma.
paed<-paed[paed$`Tumour Grade (HG/LG)`!="NR",]
adult<-adult[adult$`Tumour Grade (HG/LG)`!="NR",]

#Remove any data where the OS reported is a median from a cohort of patients.
paed<-paed[paed$`PFS/OS Median`=="No",]
adult<-adult[adult$`PFS/OS Median`=="No",]

#Fit the models for the KM analysis - progression free survival (months), censored or real ~ high or low-grade
paed_fit<-survfit(Surv(paed$`Progression Free Survival (PFS)`, paed$`PFS - Censored (0) or Real (1)`) ~ paed$`Tumour Grade (HG/LG)`)
adult_fit<-survfit(Surv(adult$`Progression Free Survival (PFS)`, adult$`PFS - Censored (0) or Real (1)`) ~ adult$`Tumour Grade (HG/LG)`)

#Combine the models into a variable for plotting
fit<-list(paed_fit, adult_fit)

#Plot the KM with a table
plot<-ggsurvplot_combine(fit,data=fit,
           surv.plot.height=5,
           color="strata",
           linetype = "strata",
           linesize=1,
           conf.int=FALSE,
           title="Kaplan-Meier Survival Curve (PFS)",
           legend = "top",
           legend.labs = c("Paediatric (HG)","Paediatric (LG)","Adult (HG)","Adult (LG)"),
           legend.title="Age Group",
           xlim=c(0,70),
           xlab="Time (Months)",
           ylab="Survival Probability (%)",
           break.time.by=5,
           risk.table = "nrisk_cumcensor",
           risk.table.col ="strata",
           risk.table.fontsize=2.5,
           risk.table.height=0.5,
           ggtheme=theme_bw(),
           surv.scale = "percent")
plot

#Save the plot
png("KM - Adult v Paediatric (PFS).png", width=1200, height=800)
plot
dev.off()
