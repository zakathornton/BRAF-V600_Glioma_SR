#Load relevant modules
library(survminer)
library(survival)
library(ggplot2)
library(readxl)

#Load data extraction file.
data<-read_xlsx("~/OneDrive - University of Bristol/BRAF/Data/Data Analysis/Data Extraction.xlsx")

#Categorise data by age group or NR (not reported).
paed<-data[data$`Paediatric or Adult (>18)`=="Paediatric",]
adult<-data[data$`Paediatric or Adult (>18)`=="Adult",]

#Remove any data where the overall survival is not reported.
paed<-paed[paed$`Overall Survival (OS)`!="NR",]
adult<-adult[adult$`Overall Survival (OS)`!="NR",]

#Format OS as numeric
paed$`Overall Survival (OS)`<-as.numeric(paed$`Overall Survival (OS)`)
adult$`Overall Survival (OS)`<-as.numeric(adult$`Overall Survival (OS)`)

#Remove any data where the tumour is not graded as high- or low-grade glioma
paed<-paed[paed$`Tumour Grade (HG/LG)`!="NR",]
adult<-adult[adult$`Tumour Grade (HG/LG)`!="NR",]

#Remove any data where the OS reported is a median from a cohort of patients
paed<-paed[paed$`PFS/OS Median`=="No",]
adult<-adult[adult$`PFS/OS Median`=="No",]

#Fit the models for the KM analysis - overall survival (months), censored or real ~ high or low-grade
paed_fit=survfit(Surv(paed$`Overall Survival (OS)`, paed$`OS - Censored (0) or Death (1)`) ~ paed$`Tumour Grade (HG/LG)`)
adult_fit=survfit(Surv(adult$`Overall Survival (OS)`, adult$`OS - Censored (0) or Death (1)`) ~ adult$`Tumour Grade (HG/LG)`)

#Combine the models into a variable for plotting
fit = list(paed_fit, adult_fit)

#Plot the KM with a table
plot=ggsurvplot_combine(fit,data=fit,
           surv.plot.height=5,
           color="strata",
           linetype = "strata",
           linesize=1,
           conf.int=FALSE,
           title="Kaplan-Meier Survival Curve (OS)",
           legend = "top",
           legend.labs = c("Adult (HG)","Paediatric (HG)","Adult (LG)","Paediatric (LG)"),
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
png("~/OneDrive - University of Bristol/BRAF/Figures/KM - Adult v Paediatric (OS).png", width=1200, height=800)
plot
dev.off()
