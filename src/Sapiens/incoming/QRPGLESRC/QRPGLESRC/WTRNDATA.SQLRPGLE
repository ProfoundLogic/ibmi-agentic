     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
     H*
     H/TITLE WTRNDATA - Renewal Reminder Notice Workfile Build
     H*****************************************************************
     H* Program Description:  This program populates the Reminder     *
     H*                       Notice Workfile.                        *
     H*                                                               *
     H* Called From:  XXXXXXXX                                        *
     H* Calls To:     None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 01/16/2024 by C:340 for Project 153WAD
     H*****************************************************************
     H* REVISIONS:
     H*
     H*
     H*****************************************************************
     FXMLPreL1  IF   E           K Disk
     FWDELL     IF   E           K Disk
     FWXRnp1    O    E             Disk
     D
     D ArBalAmount     S             15  2 Inz(0)
     D inXML           S                         Like(XPXml)
     d chrIncBatch     S              1    Inz(' ')
     d chrExcludeDep   S              1    Inz(' ')
     d chrMode         S             25    Inz(' ')
     D intInv#         s             10  0
     D rcdCount        S              7S 0
     D CurDat          S              7S 0
     D EftDat          S              7S 0
     D
     D PsDs          ESDS                  ExtName(szq1p)
     D
     D/copy sPrModule
     D/copy sPrXML
     D/copy sPrDate
     D/copy wPrAr
      *
     C*
     C     *Entry        PList
     C                   Parm                    inXML
     C*

         Chain inXML XP100;
         If %Found(xmlprel1);
            ExSr WrtWXRNP;
         EndIf;
         *inLR = *On;

      *===============================================================*
      *  *InzSr  - Initialzation Subroutine
      *===============================================================*
         Begsr *InzSr;

            CurDat = %Dec(%Date():*CYMD);
            EftDat = %int(%Char(%Date() + %Days(09) : *CYMD0));

         Endsr;
      *===============================================================*
      *  WrtWXRNP - Write to Reminder Notice Workfile
      *===============================================================*
         Begsr WrtWXRNP;

            ExSr GetARBalance;
            Clear RN001;

            RNXML    = XPXmlT;
            RNCO#    = XPCo#;
            RNFND    = XPFnd;
            RNEMP#   = XPFyr;
            RNDIV    = XPDiv;
            RNFYR    = XPFyr;
            RNPFDT   = %Dec(%date(ELPfdt:*cymd):*usa);
            RNPTDT   = %Dec(%date(ELPtdt:*cymd):*usa);
            RNDATE   = DateTo8(%date());
            RNDUED   = %Dec(%date(EftDat:*cymd):*usa);
            RNBAL    = ArBalAmount;

            RNUSER   = Q1user;
            RNPGM    = Q1pgm;
            RNTIME   = Timeto6(SystemTime());
            RNDT     = Dateto7(SystemDate());
            RNIUSR   = Q1user;
            RNIPGM   = Q1pgm;
            RNITIM   = Timeto6(SystemTime());
            RNIDT    = Dateto7(SystemDate());

            Write RN001;

         Endsr;
      *===============================================================*
      * GetARBalance :  Subroutine to Get AR Balance Amount
      *===============================================================*
       BegSr GetARBalance;

          Chain (XPCo#:XPFnd:XPEmp#:XPDiv:XPFyr) WDELL;
          If %Found;
            ArBalAmount = ARBalance(ELBsDt:CurDat:ELCo#:ELFnd:ELFyr:ELEmp#);
          EndIf;

       EndSr;
      *===============================================================*
