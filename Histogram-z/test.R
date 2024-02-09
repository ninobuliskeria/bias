View(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 1,])
table(round(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 1,]$x, digits = 1))
plot(table(round(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 1,]$x, digits = 1)))
max(table(round(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 1,]$x, digits = 2)))





View(x[x$studyPublishD == 1& x$SE < 10000& round(x$x, digits = 2) == .67,])
table(x[x$studyPublishD == 1& x$SE < 10000& round(x$x, digits = 2) == .67,]$metaID)
table(x[x$studyPublishD == 1& x$SE < 10000& x$metaID==609 &round(x$x, digits = 2) == .67,]$studyID)
View(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 609 & round(x$x, digits = 2) == .67,])
plot(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 609 & round(x$x, digits = 2) == .67,]$E)
plot(x[x$studyPublishD == 1& x$SE < 10000& x$metaID == 609 & round(x$x, digits = 2) == .67,]$SE)

table(round(x[x$studyPublishD == 1& x$SE < 10000& x$studyID==18310,]$x, digits = 2))

#17954 37  
#Garcia et al.	2005	Quality Of Life Research

#18043 41   
#Nieminen et al.	2015	European Journal Of Public Health	

#18076 43     
#Sabin	1993	Journal Of Applied Gerontology	

#18146 44
#NAKHAIE et al	2007	Review Of Radical Political Economics	Social Inequalities, Social Capital, and Health of Canadians

#18169 35     
#Sirven et al.	2008

#18190 34     
#Umberson et al.	1996	American Sociological Review	


#18222 40     
#Gorman et al.	2007	Social Science And Medicine	

#18269 48  
#Agahi et al.	2008	Journal Of Aging And Health	

#18310  85  
#2006	Applied Economics

hist(loop[loop$x < 10 & loop$metaID !="609",]$x, breaks = 150)

hist(loop[loop$x < 10 & loop$studyID !=c(17954, 18031, 18043, 18065, 18076, 
                                         18146, 18169, 18179, 18190, 18222, 
                                         18244, 18245, 18252, 18269, 18310),]$x, breaks = 100)

hist(loop[loop$x < 10,]$x, breaks = 150)
