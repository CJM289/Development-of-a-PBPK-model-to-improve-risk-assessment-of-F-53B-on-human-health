rm(list=ls())
library(deSolve)
library(openxlsx)
# initial dose for 5mg/kg
repeatdosing=0
initialdose=5*0.03*1000 

F53B_oral <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Cb<-Cb_change(t)
    dCb=(Qli*(Ali/Pliu/Wli*liver_f-Cb*blood_f)-Qki*(Cb*blood_f-Aki/Pkiu/Wki)-Qsp*(Cb*blood_f-Asp/Pspu/Wsp)-Qte*(Cb*blood_f- Ate/Pteu/Wte)-Qbr*(Cb*blood_f- Abr/Pbru/Wbr)-
           Qfa*(Cb*blood_f- Afa/Pfau/Wfa)-Qhe*(Cb*blood_f-Ahe/Pheu/Whe)-Qlu*(Cb*blood_f-Alu/Pluu/Wlu)-Qre*(Cb*blood_f-Are/Preu/Wre))/Wb;
    dAgi=-fa*Agi-fe*Agi+u;
    dAli=Qli*(Cb*blood_f-Ali/Pliu/Wli*liver_f)-Km*Ali+fa*Agi;
    
    dAki=Qki*(Cb*blood_f-Aki/Pkiu/Wki)-Qfi*Aki/Wki+Tm*Afi/Wfi/(KT+Afi/Wfi);
    dAfi= Qfi*Aki/Wki-kfi*Afi-Tm*Afi/Wfi/(KT+Afi/Wfi);
    dAur= kfi*Afi;
    dAsp=Qsp*(Cb*blood_f-Asp/Pspu/Wsp);
    dAte=Qte*(Cb*blood_f-Ate/Pteu/Wte);
    dAbr=Qbr*(Cb*blood_f-Abr/Pbru/Wbr);
    dAfa=Qfa*(Cb*blood_f-Afa/Pfau/Wfa);
    dAhe=Qhe*(Cb*blood_f-Ahe/Pheu/Whe);
    dAlu=Qlu*( Cb*blood_f-Alu/Pluu/Wlu);
    dAre=Qre*(Cb*blood_f-Are/Preu/Wre);
    list(c(dCb,dAli, dAki,dAsp, dAte,dAbr, dAlu,dAfa,dAhe,dAre,dAfi,dAur,dAgi))
    
  })
}

data1<-read.xlsx('./data_input11.xlsx')
t10 <-data1$t
t11=seq(from=0,to=8000,length=8001);

state <- c(Cb=0,Ali = 0, Aki=0,Asp=0,Ate=0,Abr=0,Alu=0,Afa=0,Ahe=0,Are=0,Afi=0,Aur=0,Agi=initialdose)


parameter_oral <- c(Wli=0.03*5.49/100, Qli=1.19*0.161, Wki=0.03*1.67/100,Wfi=0.03*1.67/100/2, Qki=1.19*0.091, Wb=0.03*0.08, Wsp=0.03*0.35/100, Qsp=1.19*0.35/100, Wte=0.03*0.598/100, 
                    Qte=1.19*0.598/100, Wbr=0.03*1.65/100, Qbr=1.19*3.3/100, Wfa=0.03*0.2915, Qfa=1.19*0.07, Whe=0.03*0.005,
                    Qhe=1.19*6.6/100, Wlu=0.03*0.73/100, Qlu=1.19*0.5/100, Wre=0.03*51.027/100, Qre=1.19*56.452/100, Qfi=1.19*0.091/2*0.05,  
                    liver_f=1, 
                    Tm=4.55, KT=0.01664671, Km=0.0004335,kfi=245,Preu=0.01755, blood_f=0.05, 
                    Pliu=266, Pkiu=10.28, Pspu=6.48, Pteu=4.22, Pbru=2.78, Pluu=19.8, Pfau=2.5, Pheu=6.7,u=repeatdosing,
                    fa=0.37,fe=0.24)

out10<-as.data.frame(ode(y = state, times = t10, func = F53B_oral, parms = parameter_oral))

#sum(colSums((log(data1[c(2:nrow(data1)),c(2:4)])-log(out10[c(2:nrow(data1)),c(2:4)]))^2))