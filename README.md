```
rm(list=ls()) # clears workspace

# prior:
a = 1
b = 1

# data:
k = 9 # number of successes in Data
n = 10 #number of trials

# posterior
ap = a+k
bp = b+n-k

Theta = seq(0.001,0.999,by=0.001)    # points for plotting
pTheta = dbeta(Theta , a , b )      # prior for plotting
pThetaGivenData = dbeta( Theta , ap , bp )    # posterior for plotting
pDataGivenTheta = choose(n, k) * Theta^k * (1-Theta)^(n-k)    # likelihood for plotting
  
# Plot the results.
layout( matrix( c( 1,2,3 ) ,nrow=3 ,ncol=1 ,byrow=FALSE ) )     # 3x1 panels
par( mar=c(3,3,1,0) , mgp=c(2,0.7,0) , mai=c(0.5,0.5,0.3,0.1) ) # margins
cexAxis = 1.33
cexLab = 1.75
dotsize = 5 # how big to make the plotted dots
barsize = 5 # how wide to make the bar lines   



# y limits for prior and posterior:
yLim = c(0,1.1*max(c(pTheta,pThetaGivenData)))
  
  
  
# Plot the prior.
  plot( Theta , pTheta , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab=bquote(dbeta(theta*"|"*.(a),.(b))) , 
        cex.lab=cexLab ,
        main="Prior (beta)" , cex.main=1.5 , col="skyblue" )

# Plot the likelihood: p(Data|Theta)
  plot( Theta , pDataGivenTheta , 
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=c(0,1.1*max(pDataGivenTheta)) , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab=bquote( "p(D|" * theta * ")" ) , 
        cex.lab=cexLab ,
        main="Likelihood (Binomial)" , cex.main=1.5 , col="skyblue" )
  if ( k > .5*n ) { textx = 0 ; textadj = c(0,1) }   else { textx = 1 ; textadj = c(1,1) }
  text( textx ,1.0*max(pDataGivenTheta) ,cex=2.0
        ,bquote( "Data: k=" * .(k) * ",n=" * .(n) ) ,adj=textadj )
  
# Plot the posterior.
  plot( Theta , pThetaGivenData ,  
        pch="." , cex=dotsize , lwd=barsize ,
        xlim=c(0,1) , ylim=yLim , cex.axis=cexAxis ,
        xlab=bquote(theta) , ylab=bquote(dbeta(theta*"|"*.(ap),.(bp))) , 
        cex.lab=cexLab ,
        main="Posterior (beta)" , cex.main=1.5 , col="skyblue" )

# prior summaries
priormean = a/(a+b) 
if ( a+b-2 > 0 ) {
  priormode = (a-1)/(a+b-2)
}  
priorvar=a*b/((a+b)^2*(a+b+1))
priorsd=sqrt(priorvar)

#posterior summaries
postmean = (a+k)/(a+b+n) 
if ( ap+bp-2 > 0 ) {
  postmode = (ap-1)/(ap+bp-2)
}
postvar=ap*bp/((ap+bp)^2*(ap+bp+1))
postsd=sqrt(postvar)
cutoff=0.95            # for credible interval. 
ciL=qbeta((1-cutoff)/2,ap,bp)
ciH=qbeta(1-(1-cutoff)/2,ap,bp)
ci=c(ciL,ciH)          # credible interval. 
1-pbeta(.7,ap,bp) #higher than .7
pbeta(.6,ap,bp)-pbeta(.4,ap,bp)      #between .4 and .6
pbeta(.3,ap,bp) #smaller than .3
```
