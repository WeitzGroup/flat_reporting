

#Load data for recorded cases from Georgia, USA
DATAFILE = read.csv("CaseDataRecords_compare_29Apr_13May_GA_2020.csv")

A_C_new_state = DATAFILE$NewCaseRecords_29April2020
B_C_new_state = DATAFILE$NewCaseRecords_13May2020
plotTIMES = as.Date(DATAFILE$TIMESTAMP)


######## PLOTTING #########

COLAdat = "darkgreen"
COLBdat = "darkblue" 


dev.new(width=10.395833,height=8.385417,unit="in")
par(mfrow=c(2,1),mar=c(3.8,4.5,1.75,1)) #two subplots, one per row


##############PLOT 1 - JUST KNOWLEDGE ON 29 APR

XTioks = c(plotTIMES[seq(1,60,by=7)],BplotTIMES[60])
plot(plotTIMES,B_C_new_state, ylab="New recorded cases per day in Georgia, USA",xlab="",col=NA,log="",xaxt="n",ylim=c(0,1000),xlim=c(BplotTIMES[1]-0.5,BplotTIMES[60]+0.5),xaxs="i",main="What we knew about cases on/before April 29th, as of April 29th")
axis.Date(1,at=XTioks,format = "%e %b",las=2)

#WEEKENDS
SAT = plotTIMES[1]-1
SUN = plotTIMES[1] #1st March 2020 was a Sunday
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-100,-100,5000,5000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}


#14-day WINDOWS
abline(v = as.Date("29/04/2020",format="%d/%m/%Y")-14,col=COLAdat,lwd=4)

#POINTS AND LINES
points(plotTIMES,A_C_new_state,pch=23,col=COLAdat,cex=1.5,lwd=2)
lines(plotTIMES,A_C_new_state,col=COLAdat)

#Add text annotation
text(plotTIMES[53],950,"14 day reporting window",col=COLAdat,adj=c(0.5,0.5),cex=1.2)
#Add border
box()
#Add legend
legend("topleft",bg="white",legend = c("Records from 29 April 2020"), col=c(COLAdat), pch=c(23),lty=c(1),lwd=c(2),pt.cex=1.5)



##############PLOT 2 -  KNOWLEDGE ON 13MAY

plot(plotTIMES,B_C_new_state, ylab="New recorded cases per day in Georgia, USA",xlab="",col=NA,log="",xaxt="n",ylim=c(0,1000),xlim=c(BplotTIMES[1]-0.5,BplotTIMES[60]+0.5),xaxs="i",main = "What we knew about cases on/before April 29th, as of May 13th")
axis.Date(1,at=XTioks,format = "%e %b",las=2)

#WEEKENDS
SAT = plotTIMES[1]-1 
SUN = plotTIMES[1] #1st March 2020 was a Sunday
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-100,-100,5000,5000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}

#14-day WINDOWS
abline(v = as.Date("29/04/2020",format="%d/%m/%Y")-14,col=COLAdat,lwd=4)

#POINTS AND LINES
lines(plotTIMES,B_C_new_state,pch=19,col=COLBdat)
points(plotTIMES,B_C_new_state,pch=19,col=COLBdat,cex=1.5,lwd=2)
points(plotTIMES,A_C_new_state,pch=23,col=COLAdat,cex=1.5,lwd=2)
lines(plotTIMES,A_C_new_state,col=COLAdat)

#Add text annotation
text(plotTIMES[53],950,"14 day reporting window",col=COLAdat,adj=c(0.5,0.5),cex=1.2)
#Add border
box()
#Add legend
legend("topleft",bg="white",legend = c("Records from 29 April 2020","Records from 13 May 2020"), col=c(COLAdat,COLBdat), pch=c(23,19),lty=c(1,1),lwd=c(2,2),pt.cex=1.5)

FIGFILENAME = "Comparing_CaseRecords_29April_13May_2020_GA"
#dev.copy2pdf(file=paste(FIGFILENAME,".pdf",sep=""))


dev.copy(png,paste(FIGFILENAME,".png",sep=""),width=10.395833,height=8.385417,units="in",res=400)
dev.off()


# I must copy only screen device, that there is the previous one
dev <- dev.prev() 

# Now it works
dev.copy2pdf(file=paste(FIGFILENAME,".pdf",sep=""),out.type = "pdf")
dev.off()




