## Load necessary packages

library("meta") # Version 6.2-1

## Load data

data <- read.delim("data.txt", sep = " ")

## Normal model - standard forest plot

plot <- metaprop(data$`V600 mutation prevalence`, data$`Total sample size (n)`, data$Author,
         sm="PFT", #PFT is Freeman-Tukey Double Arcsine Transformation
         incr=0.5,
         level = 0.95,
         random=TRUE,
         title="",
         complab="",
         outclab="",
         warn=TRUE)
         
         
#Model by variable - to create a forest plot into different variables

plot <- metaprop(data$`V600 mutation prevalence`, data$`Total sample size (n)`, data$Author,
         sm="PFT",
         subgroup=data$`Glioma Entity`,
         incr=0.5,
         level = 0.95,
         random=TRUE,
         title="", 
         complab="", 
         outclab="",
         warn=TRUE)
              
              
## Create forest plot

png(file='subtype_plot.png', width=800, height=15000)
forest(plot,
       rightlabs = c("g","95% CI","weight"), 
       leftlabs = c("Author"),
       lab.e = "Intervention",
       pooled.totals = FALSE,
       smlab = "BRAF V600 prevalence", 
       text.random = "Overall effect",
       print.tau2 = FALSE,
       col.diamond = "red",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2.ci = TRUE,
       digits.sd = 2
)
dev.off()
