ellipsoid<-function(
	hx=10,
	hrot=15,
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
	x=seq(-a,a,2*a/hx);
	rot=c(seq(0,pi,pi/hrot),seq(pi,2*pi,pi/hrot));
	y=seq(-b,b,2*b/h);
	if(zlim==-1)
	{
		zlim=c(-c,c);
	}
	lenx=length(x);
	leny=length(y);
	z=matrix(nrow=lenx, ncol=leny);
	lenrot=length(rot);
	pmat=persp(x,y,z,theta=theta,phi=phi,zlim=zlim,scale=scale,...);
	for(i in 1:lenx)
	{
		X=x[i];
		Ry=abs(b*sqrt(a^2-X^2)/a);
		Rz=abs(c*sqrt(a^2-X^2)/a);
		ptz=c();
		ptx=c();
		pty=c();
		for(j in 1:lenrot)
		{
			alpha=rot[j];
			Y=Ry*cos(alpha);
			Z=Rz*sin(alpha);

			ptz<-c(ptz,Z);
			ptx<-c(ptx,X);
			pty<-c(pty,Y);
		}
		res=trans3d(ptx,pty,ptz,pmat);
		points(
			res$x,
			res$y,
			type="l"
		);
	}
}

res=ellipsoid();
