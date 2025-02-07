rolls <- read.csv("rolls.csv")
library(dplyr)

# Reminder: Bonferroni correction p = 0.05/5 = 0.01

casino_table<-table(rolls$Casino)
chisq.test(casino_table)

gw_table<-table(rolls$GW)
chisq.test(gw_table)

vh_table<-table(rolls$VH)
chisq.test(vh_table)

nova_table<-table(rolls$NOVA)
chisq.test(nova_table)

pbaron_table<-table(rolls$Baron_P)
chisq.test(pbaron_table)

# Convergence Graphs

casino_rolls <-data.frame(roll=c(1:1000),shuff_rolls=sample(rolls$Casino))
casino_rolls[,"roll_total"] <- cumsum(casino_rolls$shuff_rolls)
casino_rolls$average <- casino_rolls$roll_total/casino_rolls$roll

gw_rolls <-data.frame(roll=c(1:1000),shuff_rolls=sample(rolls$GW))
gw_rolls[,"roll_total"] <- cumsum(gw_rolls$shuff_rolls)
gw_rolls$average <- gw_rolls$roll_total/gw_rolls$roll
plot(gw_rolls$roll,gw_rolls$average,type="l")

nova_rolls <-data.frame(roll=c(1:1000),shuff_rolls=sample(rolls$NOVA))
nova_rolls[,"roll_total"] <- cumsum(nova_rolls$shuff_rolls)
nova_rolls$average <- nova_rolls$roll_total/nova_rolls$roll
plot(nova_rolls$roll,nova_rolls$average,type="l")

vh_rolls <-data.frame(roll=c(1:1000),shuff_rolls=sample(rolls$VH))
vh_rolls[,"roll_total"] <- cumsum(vh_rolls$shuff_rolls)
vh_rolls$average <- vh_rolls$roll_total/vh_rolls$roll
plot(vh_rolls$roll,vh_rolls$average,type="l")

baronp_rolls <-data.frame(roll=c(1:1000),shuff_rolls=sample(rolls$Baron_P))
baronp_rolls[,"roll_total"] <- cumsum(baronp_rolls$shuff_rolls)
baronp_rolls$average <- baronp_rolls$roll_total/baronp_rolls$roll
plot(baronp_rolls$roll,baronp_rolls$average,type="l")

plot(casino_rolls$roll,casino_rolls$average,type="l",ylim=c(1,6),lwd=3,col="blue",
     xlab="Rolls",ylab="Cumulative Average",cex.lab=1.5)
lines(gw_rolls$roll,gw_rolls$average,lwd=3,col="red3")
lines(nova_rolls$roll,nova_rolls$average,lwd=3,col="darkorange")
lines(vh_rolls$roll,vh_rolls$average,lwd=3,col="purple")
lines(baronp_rolls$roll,baronp_rolls$average,lwd=3,col="forestgreen")
abline(h=3.5,lwd=3,col="darkgrey",lty=3)
legend('topright',
       legend = c("Casino","Games Workshop","Chessex","Baron (NOVA)","Baron (Precision)","Theoretical Ideal"), 
       col = c("blue","red3","purple","darkorange","forestgreen","darkgrey"),
       lwd = 3,
       lty = c(1,1,1,1,1,3),
       cex = 1.2, bty = 'n')


musing <- c(166.67,166.67,166.67,166.67,165.67,167.67)
chisq.test(musing)
