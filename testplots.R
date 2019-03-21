#test plots

library(ggplot2)
library(data.table)


#tracks_2017$month<-as.factor(tracks_2017$month)
#tracks_2017$month_f = factor(tracks_2017$month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul""Aug","Sep","Oct","Nov","Dec"))

#vessel_types<-c("Not Available","Other","Cargo","Tanker","Tug Tow","Passenger","Fishing","Pleasure Craft/Sailing")

tracks_2017<-tracks_2017[,`:=`(month_=format(TrckStr,'%m'))]

ggplot()+
  geom_bar(data=tracks_2017,aes(x=vssl_gr))+
  facet_wrap(.~month_,ncol=3)+
  coord_flip()

ggplot(tracks_2017)+
  geom_bar(aes(x=month_,fill=vssl_gr))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()

tracks_2017_summary<-tracks_2017[,.(trcount=.N),by=c("vssl_gr","month_")]

ggplot(tracks_2017_summary,aes(y=vssl_gr))+
  geom_point(aes(x=month_,size=trcount),alpha=0.7)+
  scale_size_area(max_size = 10)


ggplot(tracks_2017) + 
  stat_count(mapping = aes(x=vssl_gr, y=..prop.., group=1))+
  facet_wrap(.~month_f,ncol=3)


ggplot()+
  geom_bar(data=tracks_2017,aes(x=month_,fill=vssl_gr),position = "fill")+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()
