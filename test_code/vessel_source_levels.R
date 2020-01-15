#adapting and working with code from Viers et al 2016 vessel noise paper
library(data.table)
library(pracma)

load("data/vessel_SL/df_ALL_quant_linear_SL.Rdata")

dt_vessel_SL<-data.table(df_ALL_quant_linear_SL)


#interpolate SL at octave frequencies for dBSea input
library(pracma)
fcenter  <- 10^3 * (2 ^ (seq(-14,17,1)/3))
fcenter_dBsea <- 10^3 * (2 ^ (seq(-18,22,1)/3))

tug<-dt_vessel_SL[class == "Tug", .(q50.avg = mean(q50)),by=fhz]
tug_octave<-interp1(tug$fhz,tug$q50.avg,fcenter)
#tug_octave<-rbind(fcenter,tug_octave)

cargo<-dt_vessel_SL[class == "Cargo", .(q50.avg = mean(q50)),by=fhz]
cargo_octave<-interp1(cargo$fhz,cargo$q50.avg,fcenter)
#cargo_octave<-rbind(fcenter, cargo_octave)

tanker<-dt_vessel_SL[class == "Tanker", .(q50.avg = mean(q50)),by=fhz]
tanker_octave<-interp1(tanker$fhz,tanker$q50.avg,fcenter)
#tanker_octave<-rbind(fcenter, tanker_octave)

passenger<-dt_vessel_SL[class == "Passenger", .(q50.avg = mean(q50)),by=fhz]
passenger_octave<-interp1(passenger$fhz,passenger$q50.avg,fcenter)
#passenger_octave<-rbind(fcenter, passenger_octave)

fishing<-dt_vessel_SL[class == "Fishing", .(q50.avg = mean(q50)),by=fhz]
fishing_octave<-interp1(fishing$fhz,fishing$q50.avg,fcenter)
#fishing_octave<-rbind(fcenter, fishing_octave)

pleasure_craft<-dt_vessel_SL[class == "Pleasure craft", .(q50.avg = mean(q50)),by=fhz]
pleasure_craft_octave<-interp1(pleasure_craft$fhz,pleasure_craft$q50.avg,fcenter)
#pleasure_craft_octave<-rbind(fcenter, pleasure_craft_octave)

vessel_type_SL<-data.frame(pleasure_craft_octave,fishing_octave,passenger_octave,tanker_octave,cargo_octave,tug_octave)
vessel_type_SL<-t(vessel_type_SL)

write.csv(vessel_type_SL,file = "vessel_type_SL.csv",row.names = T, quote = F)


#plotting from Viers et al
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
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05))+
  geom_line(data=tug,aes(x=fhz,y=q50.avg),color="red")

ggplot(tug)+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(20,100000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(aes(x=fhz,y=q50.avg)) +##, linetype=class)) +
  # geom_smooth(aes(x=fhz,y=q50, group=class, color=class),se=FALSE,method = "loess", formula = y ~ x, degree=2, span=.005, size=1)+
  scale_y_continuous(breaks=seq(30,280,10),lim=c(100,160))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa"^2,"/Hz @ 1m)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05))


ggplot(vessel_sl_summary)+
  scale_x_log10(breaks=c(10,100,1000,10000,100000),labels = trans_format("log10",   math_format(10^.x)),lim=c(20,100000)) + 
  annotation_logticks(sides = "tb") +
  geom_line(aes(x=fhz,y=q50.avg,group=class,color=class)) +##, linetype=class)) +
  # geom_smooth(aes(x=fhz,y=q50, group=class, color=class),se=FALSE,method = "loess", formula = y ~ x, degree=2, span=.005, size=1)+
  scale_y_continuous(breaks=seq(30,280,10),lim=c(100,160))+ 
  ylab(expression(paste("Source level (dB re 1 ", mu, "Pa"^2,"/Hz @ 1m)")))+
  xlab("Frequency (Hz)")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour="black", size=0.10),panel.grid.minor.y=element_line(color="black",size=0.05))


#interpolate SL at octave frequencies for dBSea input
library(pracma)
fcenter  <- 10^3 * (2 ^ (seq(-14,17,1)/3))

tug<-dt_vessel_SL[class == "Tug", .(q50.avg = mean(q50)),by=fhz]
tug_octave<-interp1(tug$fhz,tug$q50.avg,fcenter)
tug_octave<-rbind(fcenter,tug_octave)



fcenter_dBsea <- 10^3 * (2 ^ (seq(-18,22,1)/3))
