ellipsoid<-function(
	h=100,
	a=1,
	b=1,
	c=1,
	theta=45,
	phi=15,
	zlim=-1,
	scale=F,
	...
)
{
	x=seq(-a,a,2*a/h);
	y=seq(-b,b,2*b/h);
	if(zlim==-1)
	{
		zlim=c(-c,c);
	}
	lenx=length(x);
	leny=length(y);
	z=matrix(nrow=lenx, ncol=leny);
	pmat=persp(x,y,z,theta=theta,phi=phi,zlim=zlim,scale=scale,...);
	ptz=c();
	ptx=c();
	pty=c();
	for(i in 1:lenx)
	{
		for(j in 1:leny)
		{
			d=a^2*b^2-b^2*x[i]^2-a^2*y[j]^2;
			if(d>=0)
			{
				z[i,j]=abs(c/a/b)*sqrt(d);
				ptz<-c(ptz,-z[i,j]);
				ptx<-c(ptx,x[i]);
				pty<-c(pty,y[j]);

				ptz<-c(ptz,z[i,j]);
				ptx<-c(ptx,x[i]);
				pty<-c(pty,y[j]);
			}
			else
			{
				z[i,j]=NA;
			}
		}
	}
	res=trans3d(ptx,pty,ptz,pmat);
	points(res$x,res$y,pch=".");
}

res=ellipsoid();
