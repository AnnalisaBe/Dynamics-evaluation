

#Author: Annalisa Bellandi, Faulkner group (John Innes centre)

#Title: Estimate 3D diffusion constant

#Content: given r (distance of the front of the signal from the starting point at aeach time point) and t (time), estimates diffusion constant D


##-------------------------------------------------------------------------------------------------------------------------
##------------------------------------------ packages needed  --------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------

library("readxl")
library("RColorBrewer")
library("ggplot2")
library("reshape2")
library("openxlsx")


##-------------------------------------------------------------------------------------------------------------------------
##------------------------------------------ set my colour palette --------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------

my_pal_div <- RColorBrewer::brewer.pal(11, "BrBG")[2:11]
my_pal_quant_1 <- RColorBrewer::brewer.pal(9, "Oranges")
my_pal_quant_2 <- RColorBrewer::brewer.pal(9, "Blues")
my_pal_gray <- RColorBrewer::brewer.pal(9, "Greys")
okabe_pal <- c("#E69F00","#56B4E9","#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7")

n <- max(length(my_pal_div), length(my_pal_quant_1), length(my_pal_quant_2), length(my_pal_gray), length(okabe_pal))

length(my_pal_div) <- n
length(my_pal_quant_1) <- n
length(my_pal_quant_2) <- n
length(my_pal_gray) <- n
length(okabe_pal) <- n

my_pal_gray_d <- data.frame(my_pal_gray)
my_pal_quant_1_d <- data.frame(my_pal_quant_1)
my_pal_quant_2_d <- data.frame(my_pal_quant_2)
my_pal_div_d <- data.frame(my_pal_div)
okabe_pal_d <- data.frame(okabe_pal)

my_col_set <- (0)
my_col_set <- cbind(my_pal_gray_d, my_pal_quant_1_d)
my_col_set <- cbind(my_col_set, my_pal_quant_2_d)
my_col_set <- cbind(my_col_set, okabe_pal_d)
my_col_set <- cbind(my_col_set, my_pal_div_d)

my_col_set_df <- data.frame(my_col_set)

order <- c(1:10)
my_col_set_df1 <- cbind(my_col_set_df, order)
my_col_set_df1

long_color <- melt(my_col_set_df1,
                   id.vars = "order",
                   variable.name = "palette",
                   value.name = "color")

my_colors_plot <- ggplot(long_color, aes(x = palette, y = order, fill = color)) +
  geom_tile(aes(width=0.93, height=0.95)) +
  scale_fill_identity() +
  scale_y_continuous(breaks=c(1:n)) +
  theme_light()+
  geom_label(aes(label=color),colour = "black", fill= "white", fontface = "bold", size = 4)

my_colors_plot


##-------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------


##=======================================================================================================================
##=======================================  to manually set before start  ====================================================

setwd ("...") #set working dyrectory
series<-'...' #set the name the data series that you will use
EXP<-'...' #set the experiment name

#copy data-frame with data of progression of the front of the signal over time
df <- read.table('clipboard', sep = '\t', header=TRUE)
head(df,10)
df<-data.frame(df)
tail(df)


#==========================================================================================================================
#======================================================= fit r=sqrt(6Dt) > diffusion in 3D ==================================
#==========================================================================================================================

coeff_t = NULL

L <- df$y  #y is the column in your df containing the position of the signal front over time

T <- df$xm   #xm is the column in your df containing the time

model_t <- nls( L ~ sqrt(6*D*T), start=c(D=60), data = df)
cf <- coef(model_t)

coeff_t <- data.frame(replicate='sqrt3D')

coeff_t$Dm <- cf[1]

#========================create values for the model  
model_t_df=NULL

Dm<-coeff_t$Dm

xm1 <- 1:max(T)
model_t_df$time <- xm1

ym <- sqrt(6*Dm*xm1)
model_t_df$ym <- ym

model_t_df <- data.frame(model_t_df)


#==========================plot the model and the data
plot <- ggplot()+
  geom_line(data=df, mapping=aes(x=xm, y=y, col=GEN), alpha=1, size=1.4)+
  geom_errorbar(data=df, mapping=aes(x=xm, ymin=y-sd, ymax=y+sd), width=1, color=my_pal_gray[5], alpha=0.5) + 
  scale_color_manual(values=c(okabe_pal[c(1,2,3,4,5,6,7)], my_pal_quant_1[8])) +
  geom_line(data=model_t_df, mapping=aes(x=time, y=ym), col=my_pal_quant_2[9], alpha=0.8, size=1.4, lty=2)+
  xlab("Time [s]") +
  ylab(paste("Distance from the centre", "\n" , "[\U003BCm]"))+
  theme_bw(base_size=18)+
  theme(text =element_text(size=24)) +
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"), legend.position = 'bottom')

plot

#saves PNG version for easy acces
ggsave(paste(EXP, series, "distance front_3D_sd", ".png"), plot, width = 8, height = 6, dpi = 450, device = 'png')


#saves pdf version for easy insertion in thesis. Problem with this pdf is that the data point are rendered as squares in 
#illustrator, however this renders correctly in inkscape
ggsave(paste(EXP, series, "distance front_3D_sd", ".pdf"), plot, width = 8, height = 6, dpi = 450)

workbook_sqrt <- createWorkbook(paste(series, "_results_3D", ".xlsx"))

addWorksheet(workbook_sqrt, 'coefficients')
writeData(workbook_sqrt, 'coefficients' , coeff_t)
saveWorkbook(workbook_sqrt, file=paste(series, "_results_3D", ".xlsx"), overwrite = TRUE)

addWorksheet(workbook_sqrt, 'model_values')
writeData(workbook_sqrt, 'model_values' , model_t_df)
saveWorkbook(workbook_sqrt, file=paste(series, "_results_3D", ".xlsx"), overwrite = TRUE)


