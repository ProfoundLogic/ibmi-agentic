     HCopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     HDFTNAME(F6401r)
     HOPTION(*nodebugio:*srcstmt)
     HDFTACTGRP(*no) ACTGRP('QILE')
     HBNDDIR('SPIDATE')
     H**************************************************************************************
     H*  Description:  Import Medicare Zip9 Conversion Program                             *
     H*
     H*  Called From: F6400C1 - Medicare Zip9 Override CL
     H*
     H*  Calls To:
     H*
     H**************************************************************************************
     H*  Created: 09/18/12   Programmer C:284   Project:TX079                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H**************************************************************************************
     FSMCOL     IF   E           K DISK    Rename(Pco01:Pco00)
     FSMORL     IF   E           K DISK    Rename(Por01:Por00)
     FSMUPL     IF   E           K DISK    Rename(Pup01:Pup00)
     FFTZPL1    UF A E           K DISK
     FZIPLOC9   IF   E             DISK    Rename(Pzl01:Pzl0p)
     FQSYSPRT   O    F  132        Printer Oflind(*Inoa)

     D*** Defination - standalone variables ************************************
     D flgStatus       s              1    Inz(*blanks)
     D*
     D Kkst            S                   like(Zpst)
     D Kkzip           S                   like(Zpzip)
     D Kkpls4          S                   like(Zppls4)
     D Kkefdt          S                   like(Zpefdt) inz(1100101)
     D Kkprt           S              1
     D Kkco#           S              3  0
     D isLength        S              3  0
     D strData         S             60
     D ##Data          S             80
     D ##Leng          S              3  0
     D prCntadd        S              9  0
     D prCntupdate     S              9  0
     D prCntprocess    S              9  0
     D prDate          S              7  0

     D*** Defination - Data Structures *****************************************
     D DSpsds        ESDS                  Extname(SZQ1P)
     D*
     D                 DS
     D  DSdate                 1      5S 0
     D  DSyear                 1      4S 0
     D  DSqrtr                 5      5S 0
     D*
     D                 DS
     D  DSdate1                1      8S 0
     D  DSmnth                 1      2S 0
     D  DSday                  3      4S 0
     D  DSyear1                5      8S 0

     D*** Defination - Constants ***********************************************
     DcstTitle         C                   Const('Import Medicare Zip9 Summary -
     D                                      Report')

     D*** Defination - Procedure Prototypes ************************************
     D/Copy sPrDate
     D F6401R          pr                  Extpgm('F6401R')
     D @@Co#                          3  0
     D*
     D F6401R          pi
     D @@Co#                          3  0
     D*
     D Srcenter        pr                  Extpgm('SRCENTER')
     D ##Data                        80
     D ##Leng                         3  0
     D*
     D SRload          pr
     D*
     D SRupdate        pr
     D*
     D SRwrite         pr
     D*
     D SRrte           pr

     C     keyFTZPL1     klist
     C                   kfld                    Kkst
     C                   kfld                    Kkzip
     C                   kfld                    Kkpls4
     C                   kfld                    kkefdt
     C*
     C     keySMCOL      KLIST
     C                   KFLD                    KkCO#
     C*
     C     keySMUPL      KLIST
     C                   KFLD                    Q1USER
     C*
     C     keySMORL      KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
      /Free
       KkCo# = @@Co#;
       Setll *start Ziploc9;
       Read Ziploc9;
       DoW not %Eof(Ziploc9);

          DSdate  = Z1yrqt;

          DSday   = 01;
          DSmnth  = (3*DSqrtr) - 2;
          DSyear1 = DSyear;

          Kkefdt  = dtCYMD(DSdate1);

          Kkst   = Z1st;
          Kkzip  = Z1zip;
          Kkpls4 = Z1pls4;

          Chain keyFTZPL1  Pzp01;
          If %found(Ftzpl1);
             SRload();
             SRupdate();
             flgStatus = 'U';
          Else;
             Clear Pzp01;
             SRload();
             SRwrite();
             flgStatus = 'A';
          EndIf;
       // Logic to increment count of record updated or added this will get printed.
          If flgStatus = 'U';
             prCntupdate = prCntupdate + 1;
          ElseIf flgStatus = 'A';
             prCntAdd = prCntadd + 1;
          EndIf;

       prCntprocess = prCntprocess + 1;
       Read Pzl0p;
       Enddo;

       prDate = %dec(systemdate() : *MDY);
       Srrte();
       Except Head;
       Except Total;

       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain keySMORL Por00;
       If %found(Smorl);
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
       *Inlr = *On;
       Return;
      /End-Free
      *** Print Summary report Layout ******************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS2              56
     O*
     O**********E            ROUTE          1
     O**********             ORDIS3              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS4              56
     O**********
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS2              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS3              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O*
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O*
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           29 'COMPANY  . . . . . :'
     O                       KkCO#         Z   +001
     O*
     O          E            ROUTE          1 60
     O                                           25 '(C) COPYRIGHT STONERIVE'
     O                                         +000 'R, INC.,'
     O                       *YEAR             +001
     O*
     O          E            HEAD           1 03
     O                       ZPTIME               8 '  :  :  '
     O                       CONAME              80
     O                       prDate        Y    132
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            TOTAL       1  1
     O                                           21 'Total Records Updated'
     O                                           32 '    :'
     O                       prCntupdate       +002 '       0 '
     O*
     O          E            TOTAL          1
     O                                           20 'Total Records Added '
     O                                           32 '    :'
     O                       prCntAdd          +002 '       0 '
     O*
     O          E            TOTAL          1
     O                                           23 'Total Records Processed'
     O                                           32 '    :'
     O                       prCntProcess      +002 '       0 '
     C*=====================================================================
     C*    SUBROUTINE LOAD
     C*=====================================================================
     P SRload          B
     D SRload          PI
      /Free

       //Load Ziploc details into Ftzpl1 file.
          Zpcarr = Z1carr;
          Zploc  = Z1loc;
          Zprur  = Z1rur;
          Zp4flg = Z14flg;

      /End-Free
     P SRload          E
     C*=====================================================================
     C*    SUBROUTINE UPDATE
     C*=====================================================================
     P SRupdate        B
     D SRupdate        PI
      /Free

       //Update modified date, time, user and program.
          Zpdt   = %dec(systemDate( ) : *cymd);
          Zptime = %dec(systemTime( ) : *hms);
          Zpuser = Q1user;
          Zppgm  = Q1pgm;

          Update Pzp01;

      /End-Free
     P SRupdate        E
     C*=====================================================================
     C*    SUBROUTINE WRITE
     C*=====================================================================
     P SRwrite         B
     D SRwrite         PI
      /Free

       //Populate all the required fields.
          Zpst   = Z1st;
          Zpzip  = Z1zip;
          Zppls4 = Z1pls4;
          Zpefdt = Kkefdt;
          Zpstat = 'A';

       //Populate Initial and modified date, time, user and program.
          Zpidt  = %dec(systemDate( ) : *cymd);
          Zpdt   = %dec(systemDate( ) : *cymd);
          Zpitim = %dec(systemTime( ) : *hms);
          Zptime = %dec(systemTime( ) : *hms);
          Zpiusr = Q1user;
          Zpuser = Q1user;
          Zpipgm = Q1pgm;
          Zppgm  = Q1pgm;

          Write Pzp01;

      /End-Free
     P SRwrite         E
     C*****************************************************************
     C* SRRTE - Print routing page
     C*****************************************************************
     P SRrte           B
     D SRrte           PI
      /Free

       // GET COMPANY, USER, INFO.

       Chain keySMCOL Pco00;
       Chain keySMUPL Pup00;
       kkprt = '0';
       Chain keySMORL Por00;
       If %found(Smorl);

       //Left justify title in field
          If (Ortitl <> *blanks);
             strData = Ortitl;
             isLength = %check(' ':strData);

             If (islength < 60);
                Ortitl = %subst(strData : isLength);
             Endif;
          Endif;
       //User Name if no Distribution specified in routing

          If (Ordis1 = *blanks) and (Ordis2 = *blanks) and
              (Ordis3 = *blanks) and (Ordis4 = *blanks);

             Ordis1 = Upusnm;
          Endif;
       //***** Beg Del ***** 08/26/14 ********************************************
       //********   Except Route;
       //***** End Del ***** 08/26/14 ********************************************
       Else;
          Ortitl = cstTitle;
       Endif;
       //Center Title in Field
       ##Data = Ortitl;
       ##Leng = 60;
       Srcenter(##Data : ##Leng);
       Ortitl = ##Data;

       //Center Company name in display field
       ##Data = Coname;
       ##Leng = 40;
       Srcenter(##Data : ##Leng);
       Coname = ##Data;

      /End-Free
     P Srrte           E
