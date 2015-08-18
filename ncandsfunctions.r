ncandsclean<-function(dat){
	
	#### TRANSFORM MISSINGS WITHIN DAT
	#### CREATE NEW DF X FOR RETURN, DROP UNNEEDED VARS
	#### CONVERT ALL NA TO 9 - THEN CONVERT BACK AT END
	### SO MUCH SYSTEMATIC MISSING DATA...... ChLvng missing in 
	### AL, IA, HI, IL, LA, MA, MI, MO, MS, NM, NV, SD, VA, WY
	# na.conv<-c("PostServ", "FamSup", "FamPres",
	# 	"FosterCr", "JuvPet", "CoChRep",
	# 	"Adopt", "CaseMang", "Counsel", "Daycare",
	# 	"Educatn", "Employ", "FamPlan", "Health",
	# 	"Homebas", "Housing", "TransLiv", "InfoRef",
	# 	"Legal", "MentHlth", "PregPar", "Respite",
	# 	"SSDisabl", "SSDelinq", "SubAbuse", 
	# 	"Transprt", "OtherSv")

	# for(j in(1:length(na.conv))){
	# 	z<-which(is.na(dat[,na.conv[j]]))
	# 	dat[z, na.conv[j]]<-9
	# 	}


	x<-list()

	dat<-dat[dat$StaTerr!="PR",]
	dat<-dat[dat$StaTerr!="XX", ]

	x$RptID<-dat$RptID
	x$ChID<-dat$ChID
	x$RptVictim<-dat$RptVictim

	x$st<-dat$StaTerr
	#### start converting vars - should probably source() this as functions
	dat$RptCnty[dat$RptCnty==-1]<-NA
	dat$RptCnty[dat$RptCnty==998]<-NA
	dat$RptCnty[dat$RptCnty==999]<-NA

	x$cnty<-dat$RptCnty

	x$rptsrc<-with(dat, ifelse(RptSrc==12, "informal",
		ifelse(RptSrc==8, "informal",
		ifelse(RptSrc==13, "informal",
		ifelse(RptSrc==6, "daycare", 
		ifelse(RptSrc==5, "education",
		ifelse(RptSrc==11, "informal",
		ifelse(RptSrc==4, "cj",
		ifelse(RptSrc==2, "medical",
		ifelse(RptSrc==3, "mh",
		ifelse(RptSrc==88, "informal", 
		ifelse(RptSrc==10, "informal",
		ifelse(RptSrc==9, "informal", 
		ifelse(RptSrc==1, "socserv", 
		ifelse(RptSrc==7, "fosterprovider",
		NA)))))))))))))))

	x$RptDisp<-dat$RptDisp

	x$victim<-(dat$RptDisp==1)|
		(dat$RptDisp==2)|
		(dat$RptDisp==3)
	x$victim[dat$RptDisp==99]<-NA

	dat$ChAge[dat$ChAge==99]<-NA
	dat$ChAge[dat$ChAge==77]<-NA
	x$chage<-dat$ChAge

	dat$ChSex[dat$ChSex==99]<-NA
	x$chmale<-dat$ChSex==1

	dat$ChRacBl[dat$ChRacBl==9]<-NA
	dat$ChRacAI[dat$ChRacAI==9]<-NA
	dat$ChRacWh[dat$ChRacWh==9]<-NA

	x$chrace<-ifelse(dat$ChRacBl==1, "black", 
	ifelse(dat$ChRacAI==1, "amind",
		ifelse(dat$ChRacWh==1, "white", "other")))

	dat$CEthn[dat$CEthn==9]<-NA
	x$chlatino<-dat$CEthn==1

	### Substantiated maltreatment 
	dat$ChMal1[is.na(dat$ChMal1)]<-9
	dat$ChMal2[is.na(dat$ChMal2)]<-9
	dat$ChMal3[is.na(dat$ChMal3)]<-9
	dat$ChMal4[is.na(dat$ChMal4)]<-9

	dat$Mal1Lev<-with(dat, (Mal1Lev==1)|(Mal1Lev==2)|(Mal1Lev==3))
	dat$Mal2Lev<-with(dat, (Mal2Lev==1)|(Mal2Lev==2)|(Mal2Lev==3))
	dat$Mal3Lev<-with(dat, (Mal3Lev==1)|(Mal3Lev==2)|(Mal3Lev==3))
	dat$Mal4Lev<-with(dat, (Mal4Lev==1)|(Mal4Lev==2)|(Mal4Lev==3))

	x$subst.phys<-with(dat,
		((ChMal1==1)*Mal1Lev)|
		((ChMal2==1)*Mal2Lev)|
		((ChMal3==1)*Mal3Lev)|
		((ChMal4==1)*Mal4Lev))

	x$subst.neg<-with(dat,
		((ChMal1==2)*Mal1Lev)|
		((ChMal2==2)*Mal2Lev)|
		((ChMal3==2)*Mal3Lev)|
		((ChMal4==2)*Mal4Lev))

	x$subst.medneg<-with(dat,
		((ChMal1==3)*Mal1Lev)|
		((ChMal2==3)*Mal2Lev)|
		((ChMal3==3)*Mal3Lev)|
		((ChMal4==3)*Mal4Lev))

	x$subst.sex<-with(dat,
		((ChMal1==4)*Mal1Lev)|
		((ChMal2==4)*Mal2Lev)|
		((ChMal3==4)*Mal3Lev)|
		((ChMal4==4)*Mal4Lev))

	x$subst.psych<-with(dat,
		((ChMal1==5)*Mal1Lev)|
		((ChMal2==5)*Mal2Lev)|
		((ChMal3==5)*Mal3Lev)|
		((ChMal4==5)*Mal4Lev))

	x$subst.other<-with(dat,
		((ChMal1==8)*Mal1Lev)|
		((ChMal2==8)*Mal2Lev)|
		((ChMal3==8)*Mal3Lev)|
		((ChMal4==8)*Mal4Lev))

	x$subst.none<-with(dat,
		((ChMal1==6)*Mal1Lev)|
		((ChMal2==6)*Mal2Lev)|
		((ChMal3==6)*Mal3Lev)|
		((ChMal4==6)*Mal4Lev))

	x$subst.missing<-with(dat,
		((ChMal1==9)*Mal1Lev)|
		((ChMal2==9)*Mal2Lev)|
		((ChMal3==9)*Mal3Lev)|
		((ChMal4==9)*Mal4Lev))



	#### MALTREATMENT CATS


	# dat$ChMal1[dat$ChMal1==9]<-NA
	# dat$ChMal2[dat$ChMal2==9]<-NA
	# dat$ChMal3[dat$ChMal3==9]<-NA
	# dat$ChMal4[dat$ChMal4==9]<-NA

	x$alleg.phys<-with(dat, 
		ifelse(ChMal1==1, TRUE, 
		ifelse(ChMal2==1, TRUE,
		ifelse(ChMal3==1, TRUE,
		ifelse(ChMal4==1, TRUE,
		FALSE)))))

	#ifelse((is.na(ChMal1)&is.na(ChMal2)&is.na(ChMal3)&is.na(ChMal4)),

	x$alleg.neg<-with(dat,
		(ChMal1==2)|(ChMal2==2)|(ChMal3==2)|(ChMal4==2)
		)
	# x$alleg.neg[is.na(x$alleg.neg)]<-FALSE

	x$alleg.medneg<-with(dat,
		(ChMal1==3)|(ChMal2==3)|(ChMal3==3)|(ChMal4==3)
		)
	# x$alleg.medneg[is.na(x$alleg.medneg)]<-FALSE

	x$alleg.sex<-with(dat,
		(ChMal1==4)|(ChMal2==4)|(ChMal3==4)|(ChMal4==4)
		)
	# x$alleg.sex[is.na(x$alleg.sex)]<-FALSE

	x$alleg.psych<-with(dat,
		(ChMal1==5)|(ChMal2==5)|(ChMal3==5)|(ChMal4==5)
		)
	# x$alleg.psych[is.na(x$alleg.psych)]<-FALSE

	x$alleg.none<-with(dat,
		(ChMal1==5)|(ChMal2==5)|(ChMal3==5)|(ChMal4==5)
		)
	# x$alleg.none[is.na(x$alleg.none)]<-FALSE

	x$alleg.other<-with(dat,
		(ChMal1==5)|(ChMal2==5)|(ChMal3==5)|(ChMal4==5)
		)
	# x$alleg.other[is.na(x$alleg.other)]<-FALSE

	#### Services provided start from 59 - services provided
	#### First convert to definite value so Booleans can evaluate, then convert to NA for later analysis


	x$serv.post<-dat$PostServ==1
	x$serv.famsup<-dat$FamSup==1
	x$serv.fampres<-dat$FamSup==1
	x$serv.foster<-dat$FamSup==1
	x$serv.juvpet<-dat$FamSup==1
	x$serv.cochrep<-dat$FamSup==1
	x$serv.adopt<-dat$FamSup==1
	x$serv.casemang<-dat$FamSup==1
	x$serv.counsel<-dat$FamSup==1
	x$serv.daycare<-dat$FamSup==1
	x$serv.educatn<-dat$FamSup==1
	x$serv.employ<-dat$FamSup==1
	x$serv.famplan<-dat$FamSup==1
	x$serv.health<-dat$FamSup==1
	x$serv.homebas<-dat$FamSup==1
	x$serv.housing<-dat$FamSup==1
	x$serv.transliv<-dat$FamSup==1
	x$serv.inforef<-dat$FamSup==1
	x$serv.legal<-dat$FamSup==1
	x$serv.menthlth<-dat$FamSup==1
	x$serv.pregpar<-dat$FamSup==1
	x$serv.respite<-dat$FamSup==1
	x$serv.ssdisabl<-dat$FamSup==1
	x$serv.ssdelinq<-dat$FamSup==1
	x$serv.subabuse<-dat$FamSup==1
	x$serv.transprt<-dat$FamSup==1
	x$serv.othersv<-dat$FamSup==1

	# dat[dat$ChLvng==99,]<-NA
	x$par.married<-ifelse(dat$ChLvng==1, TRUE,
		ifelse(dat$ChLvng==2, TRUE, FALSE))

			### ChildRace may vary year to year
	return(as.data.frame(x))
}

