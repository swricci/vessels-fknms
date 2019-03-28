library(ggplot2)
library(reshape2)   
library(directlabels)
library(scales)
library(plyr)

ExportPlot <- function(gplot, filename, width=2, height=1.5) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  ggsave(paste(filename, '.jpg', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

##############################  these are FINAL FIGS for the PEERJ Paper 
setwd("/home/val/R/R_Commands/PeerJ_SubmittalFiles")

load("data_1_3_BB_noNA.Rdata")

############################# Fig 2

df_basset<-data.frame(c("Container ship", "Vehicle carrier", "Cargo",  "Bulk carrier","Tug"), c(183,182,176,180,172),c(12,10,11,8,7))

names(df_basset)<-c("classA","dB_SL","order")
df_basset
classA dB_SL order
1  Container ship   183    12
2 Vehicle carrier   182    10
3           Cargo   176    11
4    Bulk carrier   180     8
5             Tug   172     7




df_mckenna<-data.frame(c("Container ship", "Vehicle carrier","Bulk carrier", "Cargo", "Tanker"), c(184,178,184,179,176),c(188,182,187,184,185),c(12,11,8,10,9))
names(df_mckenna)<-c("classA","dB_SL_min","dB_SL_max","theorder")
df_mckenna
classA dB_SL_min dB_SL_max order
1  Container ship       184       188    12
2 Vehicle carrier       178       182    11
3    Bulk carrier       184       187     8
4           Cargo       179       184    10
5          Tanker       176       185     9
plt<-
  ggplot(data=data_1_3_BB_noNA,aes(x=with(data_1_3_BB_noNA, reorder(classA,ddB_SL_Emp,median)),y=ddB_SL_Emp)) + 
  geom_boxplot(outlier.size=0) + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  #  geom_errorbar(data=NULL,aes(x=6.5,ymax=180,ymin=170),size=0.5,color='red',width=11.75)+
  #  annotate("text",label="Richardson",x=1.5,y=182, color='red') +
  geom_errorbar(data=NULL,aes(x=11.85,ymax=175,ymin=195),size=0.5,color='blue', width=0.5)+
  annotate("text",label="McKenna",x=11.85,y=197, color='blue') +    
  geom_point(data=df_basset,aes(x=order,y=dB_SL),size=8,color='black', shape=1) +
  geom_point(data=df_basset,aes(x=order,y=dB_SL),size=5,color='black', shape="B") +  
  geom_errorbar(data=NULL,aes(x=12.25,ymin=184, ymax=188),size=.5,color='blue', width=.2) +
  geom_errorbar(data=NULL,aes(x=10.25,ymin=178, ymax=182),size=.5,color='blue', width=.2) +
  geom_errorbar(data=NULL,aes(x=8.25,ymin=184, ymax=187),size=.5,color='blue', width=.2) +
  geom_errorbar(data=NULL,aes(x=11.25,ymin=179, ymax=184),size=.5,color='blue', width=.2) +
  geom_errorbar(data=NULL,aes(x=9.25,ymin=176, ymax=185),size=.5,color='blue', width=.2) +  
  geom_errorbar(data=NULL,aes(x=6.25,ymin=175, ymax=195),size=.5,color='black', width=.2) +
  annotate("text",label="Kipple",x=6.25,y=197,color='black') +
  geom_errorbar(data=NULL,aes(x=7.75,ymin=178, ymax=192),size=.5,color='black', width=.2) +
  annotate("text",label="Arveson",x=7.75,y=194,color='black') +  
  scale_y_continuous(breaks=seq(30,280,10),lim=c(140,200))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa @ 1m)")))+
  xlab("Ship Class")+
  theme_bw()+
  ##labs(title="Source level with absorption 5-25-50-75-95 % quantiles")+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05),axis.text.x = element_text(size=12))


fname="Fig_2_Compare_sourceLevel_byClass_withLiteratureBblk_wMeanNewBasset"
ExportPlot(plt,file=fname,width=12,height=5)

#############################################################################   Fig 3
load("dBrl_quant_linear_All.Rdata")
load("dBback_quant_linear_All.Rdata")

plt<-
  ggplot()+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(10,100000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(data=dBrl_quant_linear_All,aes(x=fhz,y=q05),size=.3, color="black")+
  geom_line(data=dBrl_quant_linear_All, aes(x=fhz,y=q25),size=.3,color="black")+
  geom_line(data=dBrl_quant_linear_All, aes(x=fhz,y=q50),size=.3,color="black")+
  geom_line(data=dBrl_quant_linear_All, aes(x=fhz,y=q75),size=.3,color="black")+
  geom_line(data=dBrl_quant_linear_All, aes(x=fhz,y=q95),size=.3,color="black")+
  
  geom_line(data=dBback_quant_linear_All,aes(x=fhz,y=q05),linetype=2,color="blue")+
  geom_line(data=dBback_quant_linear_All, aes(x=fhz,y=q25),linetype=2,color="blue")+
  geom_line(data=dBback_quant_linear_All, aes(x=fhz,y=q50),linetype=2,color="blue")+
  geom_line(data=dBback_quant_linear_All, aes(x=fhz,y=q75),linetype=2,color="blue")+
  geom_line(data=dBback_quant_linear_All, aes(x=fhz,y=q95),linetype=2,color="blue")+  
  scale_y_continuous(breaks=seq(30,280,10))+##,lim=c(100,220))+ 
  ylab(expression(paste("Received level (dB re 1 ", mu, "Pa"^2,"/Hz)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  ##labs(title="Source level with absorption 5-25-50-75-95 % quantiles")+
  theme(panel.grid.major = element_line(colour="black", size=0.05),panel.grid.minor.y=element_line(color="black",size=0.01))

fname="Fig_3_receivedLevel_Linear"
ExportPlot(plt,file=fname,width=8,height=5)

##############################################################################  Fig 4
load("db_quant_1_3.Rdata")
load("db_quant_1_12.Rdata")
load("db_quant_linear.Rdata")
plt<-
  ggplot()+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(20,100000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(data=subset(db_quant_1_3,signal=="ddB_SL_EmpAbs"  & class=="All" & fhz>20),aes(x=fhz,y=q50),linetype=3,color="red")+
  geom_line(data=subset(db_quant_1_12,signal=="ddB_SL_EmpAbs"  & class=="All" & fhz>24),aes(x=fhz,y=q50),linetype=2, color="red")+
  geom_line(data=subset(db_quant_linear,signal=="ddB_SL_EmpAbs"  & class=="All" & fhz>20), aes(x=fhz,y=q25),size=.4,color="pink")+
  geom_line(data=subset(db_quant_linear,signal=="ddB_SL_EmpAbs"  & class=="All" & fhz>20), aes(x=fhz,y=q50),color="red")+
  geom_line(data=subset(db_quant_linear,signal=="ddB_SL_EmpAbs"  & class=="All" & fhz>20), aes(x=fhz,y=q75),size=.4,color="pink")+
  
  geom_line(data=subset(db_quant_1_3,signal=="ddB_SL_Emp"  & class=="All" & fhz>20),aes(x=fhz,y=q50),linetype=3,color="black")+
  geom_line(data=subset(db_quant_1_12,signal=="ddB_SL_Emp"  & class=="All" & fhz>24),aes(x=fhz,y=q50),linetype=2, color="black")+
  geom_line(data=subset(db_quant_linear,signal=="ddB_SL_Emp"  & class=="All" & fhz>20), aes(x=fhz,y=q25),size=.4,color="grey")+
  geom_line(data=subset(db_quant_linear,signal=="ddB_SL_Emp" & class=="All"  & fhz>20), aes(x=fhz,y=q50),color="black")+
  geom_line(data=subset(db_quant_linear,signal=="ddB_SL_Emp"  & class=="All" & fhz>20), aes(x=fhz,y=q75),size=.4,color="grey")+ 
  scale_y_continuous(breaks=seq(30,280,10),lim=c(100,170))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa"^2,"/frequency band @ 1m)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  ##labs(title="Source level with absorption 5-25-50-75-95 % quantiles")+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.04))


fname="Fig_4_sourceLevel_AbsAndNoAbsFinal_20hz"
ExportPlot(plt,file=fname,width=8,height=5)

#############################################################   Fig 5

load("df_ALL_quant_linear_SL.Rdata")

v_list_July_22=c("Container","Bulk carrier","Vehicle carrier","Cargo","Tanker","Passenger","Tug","Military","Fishing","Pleasure craft","Research","Miscellaneous")

plt<-
  ggplot(within(df_ALL_quant_linear_SL, class<-factor(class,rev(v_list_July_22))),aes(group=class))+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(20,100000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(aes(x=fhz,y=q50, group=class, color=class))+##, linetype=class)) +
  # geom_smooth(aes(x=fhz,y=q50, group=class, color=class),se=FALSE,method = "loess", formula = y ~ x, degree=2, span=.005, size=1)+
  scale_color_manual(values=rev(plotcolors), labels=rev(v_list_July_22)) +
  guides(colour = guide_legend(title="Vessel class", override.aes = list(size=3))) +
  scale_y_continuous(breaks=seq(30,280,10),lim=c(100,160))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa"^2,"/Hz @ 1m)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05)) 

fname="Fig_5_a_sourceLevel_50_quantilesAbsbyClass_20HzHiResRobustALLqColorReversed"
ExportPlot(plt,file=fname,width=8,height=5)

###############################################################  Fig 5 again

load("df_ALL_quant_linear_SL.Rdata")

v_list_July_22=c("Container","Bulk carrier","Vehicle carrier","Cargo","Tanker","Passenger","Tug","Military","Fishing","Pleasure craft","Research","Miscellaneous")

plotcolors <- rainbow(length(v_list_July_22))

plotcolors[1]="#8FFF00FF" #light green
plotcolors[1]="#000000FF" #black
plotcolors[2]="#FF0000FF" #red
plotcolors[3]="#0080FFFF"  #
plotcolors[4]="#8F8F00FF"
plotcolors[5]="#80FF80FF"  
plotcolors[6]="#00B0C0FF"
plotcolors[7]="#A000FFFF"
plotcolors[8]="#FF00A0FF"
plotcolors[9]="#A0FFF0FF"
plotcolors[10]="#404040FF"
plotcolors[11]="#808000FF"
plotcolors[12]="#FF8000FF"
plt<-
  ggplot(within(df_ALL_quant_linear_SL, class<-factor(class,rev(v_list_July_22))),aes(group=class))+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(20,100000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(aes(x=fhz,y=q50, group=class, color=class)) +##, linetype=class)) +
  # geom_smooth(aes(x=fhz,y=q50, group=class, color=class),se=FALSE,method = "loess", formula = y ~ x, degree=2, span=.005, size=1)+
  scale_color_manual(values=rev(plotcolors), labels=rev(v_list_July_22)) +
  guides(colour = guide_legend(title="Vessel class", override.aes = list(size=3))) +
  scale_y_continuous(breaks=seq(30,280,10),lim=c(100,160))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa"^2,"/Hz @ 1m)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05)) 

fname="Fig_5_b_sourceLevel_50_quantilesAbsbyClass_20HzHiResRobustALLqColorReversed_Container"
ExportPlot(plt,file=fname,width=8,height=5)

##############################################################  Fig 6


plt<-
  ggplot(data=within(df_ALL_quant_linear_SL, class<-factor(class,v_list_July_22)),aes(x=fhz,y=q50,group=class))+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(20,80000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(aes(x=fhz,y=q05,group=class),color='blue',size=.2,linetype=1) +
  geom_line(aes(x=fhz,y=q25,group=class),color='blue',size=.3,linetype=1) +
  geom_line(aes(x=fhz,y=q50,group=class),color='black',size=.5,linetype=1) +
  geom_line(aes(x=fhz,y=q75,group=class),color='blue',size=.3,linetype=1) +
  geom_line(aes(x=fhz,y=q95,group=class),color='blue',size=.2,linetype=1) +
  facet_wrap(~class) +
  scale_y_continuous(breaks=seq(30,280,10),lim=c(100,170))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa"^2,"/Hz @ 1m)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05))

fname="Fig_6_sourceLevel_quantilesNoAbsFacetedbyClass_20hz_Container"
ExportPlot(plt,file=fname,width=8,height=5)

##############################################################   Table
library(data.table)

shipclassCounts <- function(df){
  results = data.frame()
  tot = 0
  totUnique = 0
  for (thisclass in sort(unique(df$classA))){
    tmp = subset(df, classA == thisclass & ddB_SL_Emp > 0)
    count = nrow(tmp)
    tot = tot + count
    countUnique = length(unique(tmp$mmsi))
    totUnique = totUnique + countUnique
    sog = mean(tmp$sog)
    sogsd = sd(tmp$sog)
    results = rbind(results,data.frame(count,countUnique,sog,sogsd, thisclass))
  }
  sog = mean(df$sog)
  sogsd = sd(df$sog)
  print(c(sog,sogsd))
  results = rbind(results,data.frame(count=tot,countUnique=totUnique,sog,sogsd, thisclass="All"))
  results
}

buildSummaryTable <- function(data_frame){
  summaryTable <- NULL
  for (cls in unique(data_frame$class)){
    dat <- subset(data_frame,class==cls)
    rbind(summaryTable, c(cls,dat[1,5],dat[1,3],dat[1,4],dat[2,3],dat[2,4],dat[3,3],dat[3,4])) -> summaryTable
  }
  colnames(summaryTable) <- c("class","Count", "dBrl", "dBrl_Sig","dBSL", "dBSL_sig", "dBSLAbs", "sBSLAbs_sig")
  summaryTable <- data.table(summaryTable)
  summaryTable <- summaryTable[with(summaryTable, order(class)),]
  summaryTable
}

joinSummaryTables <- function(data1, data2){
  tmp<-NULL
  for (i in 1:length(data1$class)){
    dat1 = data1[i,]
    dat2 = subset(data2,thisclass == dat1[[1]])
    newrow<-cbind(dat1,dat2)
    rbind(tmp,newrow)-> tmp
  }
  tmp
}

load("data_1_3_BB_100.Rdata")

counts_100 <- shipclassCounts(data_1_3_BB_100)
counts_100$thisclass <- as.character(counts_100$thisclass)
counts_100 <- counts_100[with(counts_100,order(thisclass)),]

summaryTable_100 <- buildSummaryTable(data_1_3_means_BB_All_100)
joinSummaryTables(summaryTable_100, counts_100)

             class Count    dBrl dBrl_Sig    dBSL dBSL_sig dBSLAbs sBSLAbs_sig count countUnique       sog    sogsd       thisclass
1              All  2809 110.497  7.09601 173.018  7.13668 177.634     13.2322  2796        1582 14.098115 3.942260             All
2     Bulk carrier   965  110.75  5.59486 173.494    5.157 175.954      9.7651   965         734 13.675544 1.461030    Bulk carrier
3            Cargo   307 112.518  5.19494 175.201  5.03254 180.391     11.7019   307         206 14.378176 2.283046           Cargo
4   Container ship   529 115.702  4.34394 178.319  3.95663 186.748     12.7921   529         207 19.224197 1.935802  Container ship
5          Fishing    65 101.932  8.50354 163.622  9.42152 166.326     11.9283    65          28  9.053846 2.180100         Fishing
6         Military   113 99.2878  10.4333 161.394  10.3412 163.796     13.5291   113          19 11.154867 3.159707        Military
7    Miscellaneous    41 100.827  8.61008 162.768  9.31884 165.205     11.3146    41          21 11.207317 5.794972   Miscellaneous
8        Passenger    49 103.722  7.63426 165.983  8.45966 166.326     8.66284    49          31 14.375510 4.453067       Passenger
9   Pleasure craft    41 97.0861  9.50441 158.759  9.37315 160.248     9.03277    41          35 12.407317 4.916167  Pleasure craft
10        Research    14 104.451   5.5948 167.169  5.35909 170.641     14.1196    14           4 11.064286 1.762257        Research
11          Tanker   148 111.436   4.6882 174.229  4.31321 179.395     12.1058   148         101 13.800676 1.389734          Tanker
12             Tug   337  107.58  4.95519 169.345  5.20901 174.394     13.4745   337          85  8.182493 2.258059             Tug
13 Vehicle carrier   187 112.771  2.99219 175.616  2.57287 182.974     12.3928   187         111 16.909626 1.799496 Vehicle carrier

