# Load necessary modules.
library(tidyverse)
library(readxl)

# Load data extraction file. 
data<-read_xlsx("Data Extraction.xlsx")

# Remove any patients without age group.
data<-data[data$`Paediatric or Adult (>18)`!="NR",]

# Filter data to only low-grade glioma.
data<-data[data$`Tumour Grade (HG/LG)`=="LG",]

# Remove any data where the BRAFi duration is not available, then change to numeric format.
data<-data[data$`BRAFi Duration (Months)`!="NR",]
data$`BRAFi Duration (Months)`<-as.numeric(data$`BRAFi Duration (Months)`)

# Change NR to NA (for plotting) and change to numeric format in overall survival.
data$`Overall Survival (OS)`<-sub("NR",NA,data$`Overall Survival (OS)`)
data$`Overall Survival (OS)`<-as.numeric(data$`Overall Survival (OS)`)

# Change NR to NA (for plotting) and change to numeric format in progression free survival.
data$`Progression Free Survival (PFS)`<-sub("NR", NA, data$`Progression Free Survival (PFS)`)
data$`Progression Free Survival (PFS)`<-as.numeric(data$`Progression Free Survival (PFS)`)

# Change NR to NA (for plotting) and change to numeric format in time to best response. 
data$`Time to Best Response (Months)`<-sub("NR",NA,data$`Time to Best Response (Months)`)
data$`Time to Best Response (Months)`<-as.numeric(data$`Time to Best Response (Months)`)

# Change NR or No to NA (for plotting) and change to numeric format in therapy ongoing. 
data$`Therapy Ongoing`<-sub("No",NA,data$`Therapy Ongoing`)
data$`Therapy Ongoing`<-sub("NR",NA,data$`Therapy Ongoing`)

# Create a subset of data in which if OS/PFS is censored, change to NA, otherwise keep as the reported OS/PFS.
data$`Overall Survival (OS)`<-ifelse(data$`OS - Censored (0) or Death (1)`==0,NA,data$`Overall Survival (OS)`)
data$`Progression Free Survival (PFS)`<-ifelse(data$`PFS - Censored (0) or Real (1)`==0,NA,data$`Progression Free Survival (PFS)`)

# Change to the correct HGG to plot: GG, PA, PXA. In this example, I use PXA.
data<-data[data$`Tumour Type (WFP)`=="PXA",]

# Create a variable for the location of a arrowhead and arrowbody in patients where therapy is ongoing.
data$arrowhead<-ifelse((is.na(data$`Therapy Ongoing`)),NA,data$`BRAFi Duration (Months)`+1)
data$arrowbody<-ifelse((is.na(data$`Therapy Ongoing`)),NA,data$`BRAFi Duration (Months)`+0.8)

# Set label positions.
data$labelpositionPID=rep(-42,times=nrow(data))
data$labelpositionAge=rep(-33,times=nrow(data))
data$labelpositionTumourResponse=rep(-23.5,times=nrow(data))
data$labelpositionBRAFI=rep(-15,times=nrow(data))
data$labelpositionMEKI=rep(-5,times=nrow(data))

# Order data by tumour type and then by BRAFi duration (decreasing).
data<-data[order(-data$`BRAFi Duration (Months)`),]

# Create a variable for the table position by setting the max value to the number of rows and sequentially subtracting one.
data$TablePosition<-seq(nrow(data),1,by=-1)

# Plot the waterfall plot.
plot<-ggplot(dat=data, mapping=aes(x=TablePosition)) +
        
      geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=72), fill="white",) +
                
      geom_bar(mapping=aes(y=`BRAFi Duration (Months)`, fill=`Tumour Type (WFP)`),
          stat="identity",
          width=0.9) + 
  
      geom_segment(aes(x=-Inf, y=-43, xend=-Inf, yend=72)) +
      geom_segment(aes(x=Inf, y=-43, xend=Inf, yend=72)) +
      geom_hline(yintercept=-43, linetype="solid") +
      geom_hline(yintercept=72, linetype="solid") +
      
      geom_point(mapping=aes(y=`Overall Survival (OS)`),
          stat ="identity",
          position=position_dodge2(width=0.9, preserve="single"),
          colour="black",
          shape=8,
          size=3,
          show.legend = FALSE) +
        
      geom_point(mapping=aes(y=`Progression Free Survival (PFS)`),
          stat="identity",
          position=position_dodge2(width=0.9, preserve="single"),
          colour="black",
          shape=15,
          size=3,
          show.legend = FALSE) +
        
      geom_point(mapping = aes(y=`Time to Best Response (Months)`),
          stat="identity",
          position=position_dodge2(width=0.9, preserve="single"),
          shape=18,
          colour="black",
          size=3,
          show.legend = FALSE) +
        
      geom_point(mapping=aes(y=arrowhead),
          stat="identity",
          position=position_dodge2(width=0.9, preserve="single"),
          shape=62,
          colour="black",
          size=5,
          show.legend = FALSE) +
  
      geom_point(mapping=aes(y=arrowbody),
          stat="identity",
          position=position_dodge2(width=0.9, preserve="single"),
          colour="black",
          shape=95,
          size=5,
          show.legend=FALSE) +
      
      annotate("text", x=data$TablePosition, y=data$labelpositionPID, label=data$`Patient ID`, size=3) +
      annotate("text", x=data$TablePosition, y=data$labelpositionAge, label=data$`Paediatric or Adult (>18)`, size=3) +
      annotate("text", x=data$TablePosition, y=data$labelpositionTumourResponse, label=data$`RANO/RECIST Best Tumour Response`, size=3) +
      annotate("text", x=data$TablePosition, y=data$labelpositionBRAFI, label=data$BRAFi, size=3) +
      annotate("text", x=data$TablePosition, y=data$labelpositionMEKI, label=data$MEKi, size=3) +
        
      annotate("text", x=nrow(data)+1, y=data$labelpositionPID, label="Patient ID", size=3, fontface="bold") +
      annotate("text", x=nrow(data)+1, y=data$labelpositionAge, label="Age Group", size=3, fontface="bold") +
      annotate("text", x=nrow(data)+1, y=data$labelpositionTumourResponse, label="Response", size=3, fontface="bold") +
      annotate("text", x=nrow(data)+1, y=data$labelpositionBRAFI, label="BRAF Inhibitor", size=3, fontface="bold") +
      annotate("text", x=nrow(data)+1, y=data$labelpositionMEKI, label="MEK Inhibitor", size=3, fontface="bold") +
        
      geom_hline(yintercept=0, linetype="solid") +
      geom_hline(yintercept=6, linetype="dashed") +
      geom_hline(yintercept=12, linetype="dashed") +
      geom_hline(yintercept=18, linetype="dashed") +
      geom_hline(yintercept=24, linetype="dashed") +
      geom_hline(yintercept=30, linetype="dashed") +
      geom_hline(yintercept=36, linetype="dashed") +
      geom_hline(yintercept=42, linetype="dashed") +
      geom_hline(yintercept=48, linetype="dashed") +
      geom_hline(yintercept=54, linetype="dashed") +
      geom_hline(yintercept=60, linetype="dashed") +
      geom_hline(yintercept=66, linetype="dashed") +
  
      coord_flip() +
        
      labs(title=paste0("Response to BRAFi Treatment - ", data$`Tumour Type`),
          x="", y="Duration of Treatment (Months)") +
        
      scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48,54,60,66,72), expand=c(0,0)) +
      
      scale_x_continuous(expand = c(0.01,0.01)) +
        
      theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
plot

# Save the plot with the appropriate glioma type.
ggsave("WFP - PXA.png",units = "in", width=12, height=8, dpi=1200)
