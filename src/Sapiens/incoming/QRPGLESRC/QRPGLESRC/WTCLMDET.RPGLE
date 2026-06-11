     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/TITLE WTCLMSUM - Profound Claim Summary
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP('QILE')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Claim Summary Info.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 2/6/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwtClmDet  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WTCLMDET')
     f                                     InfDs(fids01)
     fwmcmp     uf   e             Disk
     f                                     ReName(pcm01:dbrcdf)
     f                                     InfDs(dbfds)
     fwmcdl     uf   e           k Disk    ReName(pcd01:pcd00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)

     d* Proto-types


     d* General Prototypes
     d/copy sPrString
     d/copy wPrGroup
     d/copy sPrDate
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrContact
     d/copy wPrLoc
     d/copy wPrEmplr
     d* Load Action Panel procedure Prototype and variables
     d/Copy ScopyClmPr

     D* System Data Structures
     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D outClientID     s              3    inz('000')
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D inRRNClm        s              9  0
     D inRRNPol        s              9  0
     D inMode          s              1
     D updateWMCDP     s               n   inz(*off)

     D                 ds
     D  zipCode                1      9  0
     D  zipCode1               1      5  0
     D  zipCode2               6      9  0

     D                 ds
     D  phoneNumber            1     10  0
     D  areaCode               1      3  0
     D  phone#                 4     10  0

     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
      *

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

      /Free

         ExSr  ProgramStart;

         MoveDbSc();
         Write rcdClmAcPn;
         Exfmt rcdClmDet;
         Read rcdClmAcPn;
         If chgMode and scMenuRsp=*Blanks;
            UpdateRecord();
         EndIf;

         ExSr  ProgramEnd;

       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

         If %Parms()>0;
            @@Next=scMenuRsp;
         EndIf;
         *inlr=*on;
         Return;

       endsr;
       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

         //Load default values or passed in values
         If %parms()>0;
            inRRNClm=%int(@@RRN);
            inMode=@@Mode;
         Else;
            inRRNClm=41;
            inMode='C';
         EndIf;

         // Access the claim record
         Chain(n) inRRNClm dbrcdf;

         // Get the client id
         GetClientID(outClientID);

         // Set the Mode
         Select;
         When inMode='D';
            dspMode=*on;
         When inMode='A';
            addMode=*on;
         When inMode='C';
            chgMode=*on;
         EndSl;

         // Set the Action Panel Height
         scAcPnHt=1350;

         // Payroll Query Fields
         scPayFile='WMEPP, WMPRP A';
         scPayChc='DIGITS(EPCLAS) || '' - '' || PRDESC';
         scPayVal='EPCLAS';
         inRRNPol=GetEmpYrRR(CMfyr:CMemp#:CMdiv:CMfnd:CMco#);
         scPaySel='EPFND=' + %CHAR(CMFND) + ' AND EPFYR=' +
          %CHAR(CMFYR) + ' AND EPEMP#=' + %CHAR(CMEMP#) + ' AND EPDIV=' +
          %CHAR(CMDIV) + ' AND EPCLAS=A.PRCLAS AND A.PREFDT = ' +
          '(SELECT MAX(B.PREFDT) FROM WMPRP B WHERE ' +
          ' A.PRCLAS=B.PRCLAS)';
          // ' A.PRCLAS=B.PRCLAS) AND A.PRRTCD=''' +
          // GetRateSet(inRRNPol) + '''';
         scPayOrd='EPCLAS';

         // Place County Select
         scPlcCnSel='SUBSTR(CNKEY,1,2)=''' + CMAST + '''';

       endsr;
      /End-Free
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

       // Loads Action Panel Fields
       LoadActPan();

       // Load Standard Info
       scStateClm=CMacl#;
       scEmpClm#=CMecs#;
       scSupName=CMeSpv;
       scAccDate=cvtToDate(CMacDt);
       scAccTime=%Time(CMacTm:*HMS);
       scPayClass=CMclas;
       scSicCode=CMsic;
       sc#Dep=CM#Dep;
       scAccDesc1=CMacds;
       scAccDesc2=CMacd2;
       scLocation=CMctg1;
       scNarrativ=CMacln;
       scPremises=CMeAdr;
       scPlcName=CMacNm;
       scPlcAddr=CMacDr;
       scPlcCity=CMaCty;
       scPlcSt=CMaSt;
       scPlcZip=CMacZ1*10000 + CMacZ2;
       scPlcCnty=CMaSt + CMacCy;
       scJurState=CMjrSt;
       scTPA#=CMoff#;
       scOccDesc=CMocDs;
       scOccCode=CMocCd;
       scDept=CMdept;
       scHireDate=cvtToDate(CMhiDt);
       scTenure=' ';
       scEmpSts=CmeSts;
       scDays=CMdays;
       scHours=CMhrsD;
       scShift=CMshft;
       scRptEmp=cvtToDate(CMdtRp);
       scReleased=CMssRl;
       scMethod=CMmCon;
       scActStat=CMacSt;
       scLostTime=CMltFl;
       scPossLoss=CMplFl;
       sc7Days=' ';
       scRecDate=cvtToDate(CMrcvD);
       scConDate=cvtToDate(CMtpaD);
       scNature=CMnatr;
       scPart=CMpart;
       scCause=CMcaus;
       scNCCI=CMncci;
       scCovCode=CMlsCv;
       scRecType=CMlsRv;
       scTypeCov=CMcovT;
       scRsvType=CMrsvT;
       scMCO=CMmco;
       scExcess=CMexcs;
       scCmsSub=CMmbs;
       scSpecDis=CMsdf;
       scMcoType=CMmcTp;
       scContro=CMcvFl;
       scAcquired=CMacqr;
       scDenied=CMdny;
       scSubro=CMsubr;
       scOsha=CMosha;
       scUslh=CMuslh;

       Chain(n) CMss# pcd00;
       If %Found(wmcdl);

          // Claim County Select
          scClmCnSel='SUBSTR(CNKEY,1,2)=''' + CDST + '''';

          scLsn=CDlsn;
          scFsn=CDfsn;
          scMid=CDmid;

          scClmAdr1=CDadr1;
          scClmAdr2=CDadr2;
          scProvince=CDadr3;
          scCountry=CDctry;
          scClmCty=CDcty;
          scBirthDat=cvtToDate(CDdtob);
          scGender=CDsex;
          scClmCnty=CDST + CDcnty;
          scAge=0;
          scMarStat=CDmSts;
          scClmSt=CDst;
          scClmZip=CDzip;
          scClmPhn#=CDarea*10000000 + CDphn#;
          scClmWrk#=CDwara*10000000 + CDwph#;
          scClmEmad=CDeMad;
          If scClmEmad=*Blanks;
             scClmEmadT=noEmail;
          EndIf;
          scMedBen=CDmdBn;
       Else;
          scLsn=*Blanks;
          scFsn=*Blanks;
          scMid=*Blanks;
          scClmPhn#=*Zeros;
          scClmAdr1=*Blanks;
          scClmAdr2=*Blanks;
          scProvince=*Blanks;
          scCountry=*Blanks;
          scClmCty=*Blanks;
          scClmSt=*Blanks;
          scClmZip=*Zeros;
          scClmCnty=*Blanks;
          scClmPhn#=*Zeros;
          scClmWrk#=*Zeros;
          scClmEmad=*Blanks;
          scClmEmadT=noEmail;
          Clear scBirthDat;
          scGender=*Blanks;
          scAge=*Zeros;
          scMarStat=*Blanks;
          scMedBen=*Blanks;
       EndIf;

      /End-Free
     P MoveDbSc        E
       //===================================================================
       // MoveScDb - Move Screen fields to Database fields
       //===================================================================
     P MoveScDb        B
     D MoveScDb        PI
      /Free

       // Load Standard Info
       CMacl#=scStateClm;
       CMecs#=scEmpClm#;
       CMeSpv=scSupName;
       CMacDt=DateTo7(scAccDate);
       CMacTm=%dec(scAccTime);
       CMclas=scPayClass;
       CMsic=scSicCode;
       CM#Dep=sc#Dep;
       CMacds=scAccDesc1;
       CMacd2=scAccDesc2;
       CMctg1=scLocation;
       CMacln=scNarrativ;
       CMeAdr=scPremises;
       CMacNm=scPlcName;
       CMacDr=scPlcAddr;
       CMaCty=scPlcCity;
       CMaSt=scPlcSt;
       zipCode=scPlcZip;
       CMacZ1=zipCode1;
       CMacZ2=zipCode2;
       CMacCy=%Subst(scPlcCnty:3:3);
       CMjrSt=scJurState;
       CMoff#=scTPA#;
       CMocDs=scOccDesc;
       CMocCd=scOccCode;
       CMdept=scDept;
       CMhiDt=DateTo7(scHireDate);
       CmeSts=scEmpSts;
       CMdays=scDays;
       CMhrsD=scHours;
       CMshft=scShift;
       CMdtRp=DateTo7(scRptEmp);
       CMssRl=scReleased;
       CMmCon=scMethod;
       CMacSt=scActStat;
       CMltFl=scLostTime;
       CMplFl=scPossLoss;
       CMrcvD=DateTo7(scRecDate);
       CMtpaD=DateTo7(scConDate);
       CMnatr=scNature;
       CMpart=scPart;
       CMcaus=scCause;
       CMncci=scNCCI;
       CMlsCv=scCovCode;
       CMlsRv=scRecType;
       CMcovT=scTypeCov;
       CMrsvT=scRsvType;
       CMmco=scMCO;
       CMexcs=scExcess;
       CMmbs=scCmsSub;
       CMsdf=scSpecDis;
       CMmcTp=scMcoType;
       CMcvFl=scContro;
       CMacqr=scAcquired;
       CMdny=scDenied;
       CMsubr=scSubro;
       CMosha=scOsha;
       CMuslh=scUslh;
       CMuser=q1User;
       CMpgm=q1Pgm;
       CMtime=TimeTo6(SystemTime());
       CMdt=DateTo7(SystemDate());

       CDlsn=scLsn;
       CDfsn=scFsn;
       CDmid=scMid;

       CDadr1=scClmAdr1;
       CDadr2=scClmAdr2;
       CDadr3=scProvince;
       CDctry=scCountry;
       CDcty=scClmCty;
       CDdtob=DateTo7(scBirthDat);
       CDsex=scGender;
       CDcnty=%Subst(scClmCnty:3:3);
       CDmSts=scMarStat;
       CDst=scClmSt;
       CDzip=scClmZip;
       phoneNumber=scClmPhn#;
       CDarea=areaCode;
       CDphn#=phone#;
       phoneNumber=scClmWrk#;
       CDwara=areaCode;
       CDwph#=phone#;
       CDeMad=scClmEmad;
       CDmdBn=scMedBen;
       CDuser=q1User;
       CDpgm=q1Pgm;
       CDtime=TimeTo6(SystemTime());
       CDdt=DateTo7(SystemDate());

      /End-Free
     P MoveScDb        E
       //===================================================================
       // UpdateRecord - Update DataBase
       //===================================================================
     P UpdateRecord    B
     D UpdateRecord    PI
      /Free
          Chain inRRNClm dbrcdf;
          If %Found(wmcmp);
             Chain CMss# pcd00;
             If %Found(wmcdl);
                updateWMCDP=*On;
             EndIf;
             MoveScDb();
             Update    dbrcdf;
             If updateWMCDP=*On;
                Update    pcd00;
             EndIf;
          EndIf;
      /End-Free
     P UpdateRecord    E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy ScopyClmPi
