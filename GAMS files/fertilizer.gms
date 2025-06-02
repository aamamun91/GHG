Set
ISO3
/
$include  list_fao.inc
/

IFACOUNTRY
/
$include  list_ifa.inc
/

I
/
v_f
ocr
gro
osd
pdr
c_b
wht
pfb
ctl
/

EU /AUT,BEL,BGR,CYP,CZE,DEU,DNK,ESP,EST,FIN,FRA,GBR,GRC,HRV,HUN,IRL,ITA,LTU,LUX,LVA,MLT,NLD,POL,PRT,ROU,SVK,SVN,SWE/
MAP_R
;
alias(I,J);
MAP_R(ISO3,ISO3)=yes;
MAP_R(EU,"EU28")=yes;
MAP_R(EU,EU)=no;
MAP_R(ISO3,"ROW")=yes;
MAP_R(IFACOUNTRY,"ROW")=no;
MAP_R(EU,"ROW")=no;
Parameter
IFA IFA data by sector
/
$include ifa_gtap.inc
/

ESTIMATED
/
$include  estim_gtap.inc
/

TOTAL Total FAO and IFA
/
$include  total_fertilizer.inc
/

TOTAL_TGT    Target by country
Rescale      Rescale FAO to target IFA
check
Ref_Total
Initial_Share

ActiveConstraint
WorldTotal
EstimatedTotal
;

TOTAL_TGT(ISO3)=TOTAL(ISO3,"N_FAO");
TOTAL_TGT(ISO3)$(TOTAL(ISO3,"N_Total"))=TOTAL(ISO3,"N_Total");
WorldTotal=Sum( (ISO3),TOTAL_TGT(ISO3));
display WorldTotal;
Rescale("ROW")=Sum(ISO3$MAP_R(ISO3,"ROW"), TOTAL(ISO3,"N_FAO")) / Sum(I,IFA("ROW",I)) ;
Rescale("EU28")=Sum(ISO3$MAP_R(ISO3,"EU28"), TOTAL(ISO3,"N_FAO")) / Sum(I,IFA("EU28",I)) ;
check("Base",IFACOUNTRY)= Sum( ISO3$MAP_R(ISO3,IFACOUNTRY), TOTAL_TGT(ISO3));
TOTAL_TGT(ISO3)$MAP_R(ISO3,"ROW")=TOTAL_TGT(ISO3)/Rescale("ROW");
TOTAL_TGT(ISO3)$MAP_R(ISO3,"EU28")=TOTAL_TGT(ISO3)/Rescale("EU28");
check("Rescaled",IFACOUNTRY)= Sum( ISO3$MAP_R(ISO3,IFACOUNTRY), TOTAL_TGT(ISO3));
WorldTotal=Sum( (ISO3),TOTAL_TGT(ISO3));
display WorldTotal;
ESTIMATED(ISO3,"ctl")=0.005*TOTAL_TGT(ISO3);
ESTIMATED(EU,"ctl" )=.1*TOTAL_TGT("IRL");
ESTIMATED("IRL","ctl" )=.4*TOTAL_TGT("IRL");
ESTIMATED(ISO3,"ctl")$Sum(IFACOUNTRY$MAP_R(ISO3,IFACOUNTRY), not IFA(IFACOUNTRY,"ctl"))=0;

EstimatedTotal(ISO3)=Sum(I,ESTIMATED(ISO3,I));
ESTIMATED(ISO3,I)$EstimatedTotal(ISO3)=ESTIMATED(ISO3,I)*TOTAL_TGT(ISO3)/EstimatedTotal(ISO3);

WorldTotal=Sum( (ISO3),TOTAL_TGT(ISO3));
Initial_Share(I, ISO3)$(not IFACOUNTRY(ISO3))= ESTIMATED(ISO3,I)/WorldTotal;
Initial_Share(I, IFACOUNTRY)$(IFA(IFACOUNTRY,I))= IFA(IFACOUNTRY,I)/Sum(J,IFA(IFACOUNTRY,J))
                                                         *TOTAL_TGT(IFACOUNTRY)/WorldTotal;
ActiveConstraint(I,ISO3)$IFA(ISO3,I)=IFA(ISO3,I)/Sum(J,IFA(ISO3,J))*TOTAL_TGT(ISO3);
ActiveConstraint(I,"ROW") = IFA("ROW",I)/Sum(J,IFA("ROW",J))*Sum(ISO3$MAP_R(ISO3,"ROW"),TOTAL_TGT(ISO3));
ActiveConstraint(I,"EU28")= IFA("EU28",I)/Sum(J,IFA("EU28",J))*Sum(ISO3$MAP_R(ISO3,"EU28"),TOTAL_TGT(ISO3));
Parameter
pre_check;
pre_check("b",ISO3)=TOTAL_TGT(ISO3)-  Sum(I, Initial_Share(I,ISO3)*WorldTotal);
parameter check_EU;
check_EU=Sum(I,ActiveConstraint(I,"EU28"))-Sum(EU,TOTAL_TGT(EU));
display check_EU;
Variable
share
objective
;

Initial_Share(I, ISO3)$(not TOTAL_TGT(ISO3) )=0;
Parameter
countryDistribution;
countryDistribution("before",I,ISO3)$Initial_share(I,ISO3)=Initial_share(I,ISO3)/Sum(J,Initial_share(J,ISO3));

share.l(I,ISO3)=Initial_Share(I, ISO3);
share.lo(I,ISO3)$Initial_Share(I, ISO3)=Initial_Share(I, ISO3)*.5;
share.up(I,ISO3)$(Initial_Share(I, ISO3))=Initial_Share(I, ISO3)*1.5;

share.lo("ctl",ISO3)$Initial_Share("ctl", ISO3)=Initial_Share("ctl", ISO3)*.5;
share.up("ctl",ISO3)$(Initial_Share("ctl", ISO3))=Initial_Share("ctl", ISO3)*400;

share.lo("ocr",ISO3)$Initial_Share("ocr", ISO3)=Initial_Share("ocr", ISO3)*.01;
share.up("ocr",ISO3)$(Initial_Share("ocr", ISO3))=Initial_Share("ocr", ISO3)*20;

share.lo(I,ISO3)$[Initial_Share(I, ISO3)*MAP_R(ISO3,"EU28")]=share.lo(I,ISO3)*.75;
share.up(I,ISO3)$[Initial_Share(I, ISO3)*MAP_R(ISO3,"EU28")]=share.up(I,ISO3)*1.25;

share.lo(I,ISO3)$[Initial_Share(I, ISO3)*MAP_R(ISO3,"ROW")]=share.lo(I,ISO3)*.75;
share.up(I,ISO3)$[Initial_Share(I, ISO3)*MAP_R(ISO3,"ROW")]=share.up(I,ISO3)*1.25;

share.up("pfb",ISO3)$[Initial_Share("pfb", ISO3)*MAP_R(ISO3,"ROW")]=share.up("pfb",ISO3)*2.5;


share.lo(I,IFACOUNTRY)$IFA(IFACOUNTRY,I)=Initial_Share(I, IFACOUNTRY)*.9;
share.up(I,IFACOUNTRY)$IFA(IFACOUNTRY,I)=Initial_Share(I, IFACOUNTRY)*1.1;

Set Cotton(ISO3);
Cotton(ISO3)$(share.l("pfb",ISO3)>0.0001)=yes;
share.fx(I,ISO3)$(TOTAL_TGT(ISO3)<.001*WorldTotal and not (EU(ISO3)+Cotton(ISO3)) )=Initial_Share(I, ISO3);


pre_check("a",ISO3)=TOTAL_TGT(ISO3)-  Sum(I, share.l(I,ISO3)*WorldTotal);

Equations
EQ_obj
EQ_constraintC
EQ_constraintI
;

EQ_constraintI(I,IFACOUNTRY)$ActiveConstraint(I,IFACOUNTRY)..
       ActiveConstraint(I,IFACOUNTRY)  =e=  Sum( ISO3$(MAP_R(ISO3,IFACOUNTRY)*Initial_Share(I, ISO3)),
                 share(I,ISO3)*WorldTotal)
;
EQ_constraintC(ISO3)$TOTAL_TGT(ISO3)..
       TOTAL_TGT(ISO3) =e=  Sum(I$Initial_Share(I, ISO3), share(I,ISO3)*WorldTotal)
;
EQ_obj..
       objective =e=  Sum((I,ISO3)$Initial_Share(I, ISO3), -share(I,ISO3) *log( share(I,ISO3)/Initial_Share(I, ISO3) ) )
;
model entropy/EQ_obj,EQ_constraintC,EQ_constraintI
/;
*,EQ_constraintI
*option iterlim=0;
*option reslim=0;
solve entropy minizing objective using nlp;
parameter
delta_share
main_change
rel_change
output
;
delta_share=1-Sum((I,ISO3), share.l(I,ISO3)) ;
display delta_share;
main_change(I,ISO3)=abs(share.l(I,ISO3)-Initial_share(I,ISO3)) ;
main_change(I,ISO3)$(main_change(I,ISO3)<0.001)=0;
rel_change("Rel",I,ISO3)$main_change(I,ISO3)=share.l(I,ISO3)/Initial_share(I,ISO3);
rel_change("bef",I,ISO3)$main_change(I,ISO3)=Initial_share(I,ISO3);
rel_change("aft",I,ISO3)$main_change(I,ISO3)=share.l(I,ISO3);
countryDistribution("after",I,ISO3)$share.l(I,ISO3)=share.l(I,ISO3)/Sum(J,share.l(J,ISO3)) ;

output(ISO3,I)=share.l(I,ISO3)*WorldTotal*1e3;
file outputfile/fert_allocated.csv/
put outputfile;
put "ISO3,SECTOR,N_Tons"   /
Loop( (ISO3,I)$output(ISO3,I),
put ISO3.tl "," I.tl "," output(ISO3,I):0:10
put /
);

Parameter
Summary;
set RDM;
RDM(ISO3)$MAP_R(ISO3,"ROW")=yes;
Summary(RDM,I,"Nini")=initial_share(I,RDM)*WorldTotal;
Summary(RDM,"TOTAL","Nini")=TOTAL_TGT(RDM);
Summary("TOTAL",I,"Nini")=Sum(RDM,Summary(RDM,I,"Nini"));
Summary("TOTAL",I,"Constraint")= ActiveConstraint(I,"ROW");
Summary(RDM,I,"Nfin")=share.l(I,RDM)*WorldTotal;
Summary(RDM,"TOTAL","Nfin")=Sum(I,Summary(RDM,I,"Nfin"));

Summary("TOTAL",I,"Nfin")=Sum(RDM,Summary(RDM,I,"Nfin"));

Summary(RDM,I,"SpaceTotal")$Summary(RDM,I,"Nini")=(share.up(I,RDM)-share.lo(I,RDM))*WorldTotal;
Summary(RDM,I,"SpaceLo")$Summary(RDM,I,"Nini")=(share.l(I,RDM)-share.lo(I,RDM))*WorldTotal;
Summary(RDM,I,"Spaceup")$Summary(RDM,I,"Nini")=(-share.l(I,RDM)+share.up(I,RDM))*WorldTotal;
Summary("TOTAL",I,"SpaceTotal")=Sum(RDM,Summary(RDM,I,"SpaceTotal"));
Summary("TOTAL",I,"SpaceLo")=Sum(RDM,Summary(RDM,I,"SpaceLo"));
Summary("TOTAL",I,"Spaceup")=Sum(RDM,Summary(RDM,I,"Spaceup"));


* Distribution par rapport a la moyenne nationale, distribution par rapport a la crop