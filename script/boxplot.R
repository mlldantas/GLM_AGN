require(plyr)
require(MASS)

require(ggthemes)
AGN_data<-read.table("../data/outputdata_diagnostic.txt",header=TRUE,sep="")

cut1<-AGN_data[which(AGN_data$redshift>=0.05 & AGN_data$redshift <0.075 & AGN_data$flux_sca_fuv!=0 & AGN_data$AV_GAL_synth >0),]
cut2<-cut1[,c("Dn4000_Obs","AV_GAL_synth")]


m <- ggplot(cut2, aes(y = AV_GAL_synth, x = Dn4000_Obs,
                        group = round_any(Dn4000_Obs, 0.25)))
m + geom_boxplot(notch = F,fill="cyan")+theme_stata()+
  theme(legend.position="none",plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(vjust=0.75),axis.text.x=element_text(size=25),
        strip.text.x=element_text(size=25),
        axis.title.x=element_text(vjust=-0.25),
        text = element_text(size=20),axis.title.x=element_text(size=rel(1)))+
  ylab(expression(A[V]^{galáxia}))+xlab(expression(D[n]*4000~Observado))
