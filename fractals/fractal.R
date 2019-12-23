###########################################################################################
# MANDELBROT SET 
# found at: http://users.utu.fi/attenka/mandelbrot_set.R
# this searches over a grid step by step and then colors the plot based on the Dist value 
###########################################################################################

Limits=c(-2,0.8)
MaxIter=25
cl=colours()
Step=seq(Limits[1],Limits[2],by=0.005)
S=floor(length(cl)/MaxIter)
Dist=0
PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
t=0


for(a in Step)
{
  for(b in Step+0.6)
  {
    x=0;y=0;n=0;Dist=0
    while(n<MaxIter & Dist<4)
    {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      Dist=newx^2+newy^2
      x=newx;y=newy
    }
    if(Dist<4) colour=24 # black colour
    else colour=n*S
    t <- t+1
    PointsMatrix[t,]=c(a,b,colour)
  }
}

plot(PointsMatrix[,1], PointsMatrix[,2], xlim=Limits, ylim=Limits+0.6, col=cl[PointsMatrix[,3]], pch=".")
