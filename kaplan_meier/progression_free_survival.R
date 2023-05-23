## Clear working environment.

rm(list=ls())

## Load necessary modules.

library(survminer)
library(survival)
library(ggplot2)
library(readxl)

## Load data extraction file. 

data<-read_xlsx("Data Extraction.xlsx")

## Categorise data by age group or NR (not reported).

paediatric<-data[data$`Paediatric or Adult (>18)`=="Paediatric",]
adult<-data[data$`Paediatric or Adult (>18)`=="Adult",]

## Remove any data where the progression free survival is not reported.

paediatric<-paediatric[paediatric$`Progression Free Survival (PFS)`!="NR",]
adult<-adult[adult$`Progression Free Survival (PFS)`!="NR",]

## Format overall survival as numeric data type.

paediatric$`Progression Free Survival (PFS)`<-as.numeric(paediatric$`Progression Free Survival (PFS)`)
adult$`Progression Free Survival (PFS)`<-as.numeric(adult$`Progression Free Survival (PFS)`)

## Remove any data where the tumour is not graded as high- or low-grade glioma.

paediatric<-paediatric[paediatric$`Tumour Grade (HG/LG)`!="NR",]
adult<-adult[adult$`Tumour Grade (HG/LG)`!="NR",]

## Remove any data where the PFS reported is a median from a cohort of patients.

paediatric<-paediatric[paediatric$`PFS/OS Median`=="No",]
adult<-adult[adult$`PFS/OS Median`=="No",]

## Fit the models for the KM analysis - progression free survival (months), censored or real ~ high or low-grade

paed_fit=survfit(Surv(paediatric$`Progression Free Survival (PFS)`, paediatric$`PFS - Censored (0) or Real (1)`) ~ paediatric$`Tumour Grade (HG/LG)`)
adult_fit=survfit(Surv(adult$`Progression Free Survival (PFS)`, adult$`PFS - Censored (0) or Real (1)`) ~ adult$`Tumour Grade (HG/LG)`)

## Combine the models into a variable for plotting

fit = list(paed_fit, adult_fit)

## Plot the Kaplan-Meier survival curve with a table.

plot=ggsurvplot_combine(fit,data=fit,
           surv.plot.height=5,
           color="strata",
           linetype = "strata",
           linesize=1,
           conf.int=TRUE,
           title="Kaplan-Meier Survival Curve (PFS)",
           legend = "top",
           legend.labs = c("Adult (HG)","Paediatric (HG)","Adult (LG)","Paediatric (LG)"),
           legend.title="Age Group",
           xlim=c(0,80),
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

## Save the plot

pdf("KM - Adult v Paediatric (PFS) Graded.pdf", onefile=F, height=8, width=12)
plot
dev.off()
