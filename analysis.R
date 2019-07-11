
# install.packages("broom")
# install.packages("mFilter")
library("broom")
library("mFilter")

tde =c(
	99066.1,		# 2000
	109276.2,	# 2001
	120480.4,	# 2002
	136576.3,	# 2003
	161415.4,	# 2004
	185998.9,	# 2005
	219028.5,	# 2006
	270704,		# 2007
	321229.5,	# 2008
	347934.9,	# 2009
	410354.1,	# 2010
	483392.8,	# 2011
	537329,		# 2012
	588141.2,	# 2013
	642097.6,	# 2014
	683390.5,	# 2015
	737074,		# 2016
	820099.5,	# 2017
	896915.6		# 2018
)
tdo =c(
	109276.2,	# 2001
	120480.4,	# 2002
	136576.3,	# 2003
	161415.4,	# 2004
	185998.9,	# 2005
	219028.5,	# 2006
	270704,		# 2007
	321229.5,	# 2008
	347934.9,	# 2009
	410354.1,	# 2010
	483392.8,	# 2011
	537329,		# 2012
	588141.2,	# 2013
	642097.6,	# 2014
	683390.5,	# 2015
	737074,		# 2016
	820099.5,	# 2017
	896915.6		# 2018
)
ts.tdo = ts(tdo, frequency = 1, start = c(2001, 1) )
ts.tde = ts(tde, frequency = 1, start = c(2000, 1) )

sumy = function(su.data){
	tem.sumy = summary( su.data, na.rm = TRUE)
	assign( "sd", sd(su.data, na.rm = TRUE))
	assign( "var", var(su.data, na.rm = TRUE))
	assign( "range", range( su.data, na.rm = TRUE)[2]-range( su.data, na.rm = TRUE)[1])
	tem.su.df = tidy(tem.sumy)
	tem.su.all = cbind( tem.su.df, sd,var, range)
	tem.su.all
}

filter.fiv = function(data, obj.env = TRUE){
	if (is.ts(data)){
		if( length(data)%% 2 == 0 ){
			tem.fiv.su.vna.even = c( "hp","bk","cf","bw","tr")
			tem.fiv.su.vna.fne = c( "HP","BK","CF","BW","TR")
			for(i in c(1:5)){
				tem.na = "tem.ts"
				tem.ts.filter = mFilter( data, filter=tem.fiv.su.vna.fne[i]) 
				tem.na.all = paste0(tem.na,".", tem.fiv.su.vna.even[i])
				if (obj.env == TRUE){
					assign( tem.na.all , tem.ts.filter , env = .GlobalEnv)
				} else if (obj.env == FALSE){
					assign( tem.na.all , tem.ts.filter)
				}
			}
			if (obj.env == FALSE){
				tem.list = list(filter.hp=tem.ts.hp,
					filter.bk=tem.ts.bk,
					filter.cf=tem.ts.cf,
					filter.bw=tem.ts.bw,
					filter.tr=tem.ts.tr)
			}
		} else {
			tem.fiv.su.vna.odd = c( "hp","bk","cf","bw")
			tem.fiv.su.vna.fno = c( "HP","BK","CF","BW")
			for(i in c(1:4)){
				tem.na = "tem.ts"
				tem.ts.filter = mFilter( data, filter=tem.fiv.su.vna.fno[i]) 
				tem.na.all = paste0(tem.na,".", tem.fiv.su.vna.odd[i])
				if (obj.env == TRUE){
					assign( tem.na.all , tem.ts.filter , env = .GlobalEnv)
				} else if (obj.env == FALSE){
					assign( tem.na.all , tem.ts.filter)
				}
			}
			if (obj.env == FALSE){
				tem.list = list(filter.hp=tem.ts.hp,
					filter.bk=tem.ts.bk,
					filter.cf=tem.ts.cf,
					filter.bw=tem.ts.bw)
			}
		}
		if (obj.env == FALSE){
			return(tem.list)
		}
	} else {
		stop("Data Error.")
	}
}

# filter.fiv(ts.tdo)
# filter.fiv(ts.tde)
# filter.fiv(ts.tdo,FALSE)
# filter.fiv(ts.tde,TRUE)


filter.fiv.su = function(data){
	filter.fiv(data,TRUE)
	tem = filter.fiv(data,FALSE)
	tem.list.na.va = names(tem)
	tem.list.na.nu = length(tem.list.na.va)

	for(i in tem.list.na.nu){
		num= tem.list.na.va[i]
		comds = paste0( "tem$",num ,"$cycle")
		evalp = eval(parse(text = comds))
		print(summary(evalp))
		#assign( k,
		#        evalp
		#    , env = .GlobalEnv)
	}
}
filter.fiv.su(ts.tdo)


# 時間序列 PIC
par(mfrow=c(1,1))
plot(cn.gdp.1y.ts)


# 檔科學記號
options(scipen=999)

# HP
cn.gdp.1y.ts.hp.cyc = cn.gdp.1y.ts.hp[["cycle"]]
summary(cn.gdp.1y.ts.hp.cyc, na.rm = TRUE)
sd(cn.gdp.1y.ts.hp.cyc, na.rm = TRUE)
var(cn.gdp.1y.ts.hp.cyc, na.rm = TRUE)
quantile(cn.gdp.1y.ts.hp.cyc, na.rm = TRUE)
range( cn.gdp.1y.ts.hp.cyc, na.rm = TRUE)[2]-range( cn.gdp.1y.ts.hp.cyc, na.rm = TRUE)[1]

# BK
cn.gdp.1y.ts.bk.cyc = cn.gdp.1y.ts.bk[["cycle"]]
summary(cn.gdp.1y.ts.bk.cyc, na.rm = TRUE)
sd(cn.gdp.1y.ts.bk.cyc, na.rm = TRUE)
var(cn.gdp.1y.ts.bk.cyc, na.rm = TRUE)
quantile(cn.gdp.1y.ts.bk.cyc, na.rm = TRUE)
range( cn.gdp.1y.ts.bk.cyc, na.rm = TRUE)[2]-range( cn.gdp.1y.ts.bk.cyc, na.rm = TRUE)[1]

# CF
cn.gdp.1y.ts.cf.cyc = cn.gdp.1y.ts.cf[["cycle"]]
summary(cn.gdp.1y.ts.cf.cyc, na.rm = TRUE)
sd(cn.gdp.1y.ts.cf.cyc, na.rm = TRUE)
var(cn.gdp.1y.ts.cf.cyc, na.rm = TRUE)
quantile(ccn.gdp.1y.ts.cf.cyc, na.rm = TRUE)
range( cn.gdp.1y.ts.cf.cyc, na.rm = TRUE)[2]-range( cn.gdp.1y.ts.cf.cyc, na.rm = TRUE)[1]

# BW
cn.gdp.1y.ts.bw.cyc = cn.gdp.1y.ts.bw[["cycle"]]
summary(cn.gdp.1y.ts.bw.cyc, na.rm = TRUE)
sd(cn.gdp.1y.ts.bw.cyc, na.rm = TRUE)
var(cn.gdp.1y.ts.bw.cyc, na.rm = TRUE)
quantile(cn.gdp.1y.ts.bw.cyc, na.rm = TRUE)
range( cn.gdp.1y.ts.bw.cyc, na.rm = TRUE)[2]-range( cn.gdp.1y.ts.bw.cyc, na.rm = TRUE)[1]

# TR
cn.gdp.1y.ts.tr.cyc = cn.gdp.1y.ts.tr[["cycle"]]
summary(cn.gdp.1y.ts.tr.cyc, na.rm = TRUE)
sd(cn.gdp.1y.ts.tr.cyc, na.rm = TRUE)
var(cn.gdp.1y.ts.tr.cyc, na.rm = TRUE)
quantile(cn.gdp.1y.ts.tr.cyc, na.rm = TRUE)
range( cn.gdp.1y.ts.tr.cyc, na.rm = TRUE)[2]-range( cn.gdp.1y.ts.tr.cyc, na.rm = TRUE)[1]


opar <- par(no.readonly=TRUE)
par(mfrow=c(1,1),mar=c(3,3,2,1))
plot(cn.gdp.1y.ts,main="1953 - 2018 國民總收入 - 億元",col=1,ylab="")
lines(cn.gdp.1y.ts.hp$trend,col=2)
lines(cn.gdp.1y.ts.bk$trend,col=3)
lines(cn.gdp.1y.ts.cf$trend,col=4)
lines(cn.gdp.1y.ts.bw$trend,col=5)
lines(cn.gdp.1y.ts.tr$trend,col=6)
legend("topleft",legend=c("series", "HP","BK","CF","BW", "TR"),col=1:6, lty=rep(1,6),ncol=2)