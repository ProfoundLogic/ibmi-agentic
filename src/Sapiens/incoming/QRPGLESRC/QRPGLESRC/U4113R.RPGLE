     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      // ***************************************************************
     H/TITLE U4113R - PRINT PRE-CERT DEPT LETTER
      // ***************************************************************
      // PROGRAM DESCRIPTION                                           *
      //                                                               *
      // This Program Prints Pre-Cert Letter for TML. It is called     *
      // only if client is TML.                                        *
      //                                                               *
      //                                                               *
      //                                                               *
      //  CALLED FROM:                                                 *
      //     U4113C                                                    *
      //                                                               *
      //  CALLS TO:                                                    *
      //     *NONE                                                     *
      //                                                               *
      //                                                               *
      //                                                               *
      //  PROGRAMMER: C:185                                            *
      //                                                               *
      //  CREATED:    12/15/06               PROJECT: 144MAH           *
      //                                                               *
      //****************************************************************
      // REVISIONS:                                                    *
      //
      //  05/21/14 C:062 Project: 144MCH
      //                 Add processing for two new letters.
      //                 1) Adverse Determination Letter
      //                 2) Appeal Determination Denial Letter
      //
      //  03/05/13 C:284 SUPPORT: 81823
      //                 Move the free text towards left margin so that
      //                 text does not cut off.
      //
      //  02/19/13 C:284 PROJECT: 144MCG PRISS# 18
      //                 Changes to Pre-cert Letters
      //
      //  12/14/12 C:284 PROJECT: 144MCG
      //                 Changes to Pre-cert Letters
      //
      //  11/19/08 C:191 PROJECT: 144MBL
      //                 CHANGE PRE-CERT LETTER VERISONS
      //
      //  06/20/08 C:47  PROJECT: 144MBH
      //                 SUPPRESS SS# ON PRE-AUTH LETTER
      //
      //****************************************************************
      //** Begin Chg *** 05/21/2014 ************************************
      //*********QSYSPRT   O    F  139        PRINTER OFLIND(*INOA)
     FQSYSPRT   O    F  139        PRINTER OFLIND(*INOA) usropn
      //** End   Chg *** 05/21/2014 ************************************
      //** Begin Add *** 05/21/2014 ************************************
     FU4113O    o    e             printer               usropn
      //** End   Add *** 05/21/2014 ************************************
      //
      //** Begin Add *** 05/21/2014 ************************************
     d/copy sPrDate
      //** End   Add *** 05/21/2014 ************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      //
     D                 DS
     D  PARA01                 1    210
     D  LC70A1                 1     70
     D  LC70A2                71    140
     D  LC70A3               141    210
     D                 DS
     D  PARA02                 1    210
     D  LC70A4                 1     70
     D  LC70A5                71    140
     D  LC70A6               141    210
     D                 DS
     D  PARA03                 1    210
     D  LC70A7                 1     70
     D  LC70A8                71    140
     D  LC70A9               141    210
     D                 DS
     D  PARA04                 1    210
     D  LC70AA                 1     70
     D  LC70AB                71    140
     D  LC70AC               141    210
     D                 DS
     D  PARA05                 1    210
     D  LC70AD                 1     70
     D  LC70AE                71    140
     D  LC70AF               141    210
     D                 DS
     D  PARA06                 1    210
     D  LC70AG                 1     70
     D  LC70AH                71    140
     D  LC70AI               141    210
     D                 DS
     D  PARA07                 1    210
     D  LC70AJ                 1     70
     D  LC70AK                71    140
     D  LC70AL               141    210
     D                 DS
     D  PARA08                 1    210
     D  LC70AM                 1     70
     D  LC70AN                71    140
     D  LC70AO               141    210
     D                 DS
     D  PARA09                 1    210
     D  LC70AP                 1     70
     D  LC70AQ                71    140
     D  LC70AR               141    210
     D                 DS
     D  PARA10                 1    210
     D  LC70AS                 1     70
     D  LC70AT                71    140
     D  LC70AU               141    210

      //** Begin Add *** 05/21/2014 ********************************************
     D  printerFile    s               n   Inz(*Off)
      //** End   Add *** 05/21/2014 ********************************************

     C     *ENTRY        PLIST
     C                   PARM                    LCRRN2           40
     C                   PARM                    LCRRN1           40
     C                   PARM                    LCRRA1           40
     C                   PARM                    LCEMNM           40
     C                   PARM                    LCRRA2           40
     C                   PARM                    LCRRCT           40
     C                   PARM                    LCCLNM           40
     C                   PARM                    LCSS#             9
     C                   PARM                    LCACDT            7 0
     C                   PARM                    LCSUB#            5 0
     C                   PARM                    LCFND             3 0
     C                   PARM                    LCFYR             3 0
     C                   PARM                    LCCASE            7 0
     C                   PARM                    LCCERT           15
     C                   PARM                    LCLIAB            1
     C                   PARM                    LCICD1            5
     C                   PARM                    LCIDS1           50
     C                   PARM                    LC40A1           40
     C                   PARM                    LCTPNM           40
     C                   PARM                    LC40A2           40
     C                   PARM                    LCCANM           40
     C                   PARM                    LCDCID           12
     C                   PARM                    PARA11          210
     C                   PARM                    PARA12          210
     C                   PARM                    PARA13          210
     C                   PARM                    PARA14          210
     C                   PARM                    PARA15          210
     C                   PARM                    PARA16          210
     C                   PARM                    PARA17          210
     C                   PARM                    PARA18          210
     C                   PARM                    PARA19          210
     C                   PARM                    PARA20          210
      //
     C     DTCYMD2       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      //
     C                   MOVE      LCSS#         LCSS#4            4
     C                   MOVEL     LCSS#         LCSS#3            3
     C                   MOVEL     LCSS#         LCSS#T            5
     C                   MOVE      LCSS#T        LCSS#2            2
     C                   MOVEL(P)  'XXX'         LCSS#3
     C                   MOVEL(P)  'XX'          LCSS#2
      //
     C                   MOVE      PARA11        PARA01
     C                   MOVE      PARA12        PARA02
     C                   MOVE      PARA13        PARA03
     C                   MOVE      PARA14        PARA04
     C                   MOVE      PARA15        PARA05
     C                   MOVE      PARA16        PARA06
     C                   MOVE      PARA17        PARA07
     C                   MOVE      PARA18        PARA08
     C                   MOVE      PARA19        PARA09
     C                   MOVE      PARA20        PARA10
      //
      /FREE
       IF LCDCID = 'AP2002';
         *IN51 = *ON;
         *IN54 = *OFF;
         *IN56 = *OFF;
       ENDIF;
       IF LCDCID = 'COMP2002';
         *IN56 = *ON;
         *IN51 = *OFF;
         *IN54 = *OFF;
       ENDIF;
       IF LCDCID = 'DEN2002';
         *IN54 = *ON;
         *IN51 = *OFF;
         *IN56 = *OFF;
       ENDIF;
       //** Begin Add *** 05/21/2014 ************************************
       //
       // Check to see if we are printing letter that uses printer file
       //
       If LCdcid = 'APPE2014' Or LCdcId = 'ADVE2014';
         Open U4113O;
         printerFile = *On;
       Else;
         Open QsysPrt;
         printerFile = *Off;
       EndIf;
       //** End   Add *** 05/21/2014 ************************************
       //
      /END-FREE
     C                   MOVE      LCACDT        XXDT6             6 0
     C                   Z-ADD     XXDT6         SRDTI
      //
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C                   MOVEL     SRDTO         XXDAT6            6 0
      //
     C                   TIME                    DSDTTM
      //
      //
      /FREE
       IF LCRRN1 <> *BLANKS;
         *IN39 = *ON;
       ENDIF;
       //
       //
       IF LCRRA1 <> *BLANKS;
         *IN40 = *ON;
       ENDIF;
       //
       IF LCRRA2 <> *BLANKS;
         *IN41 = *ON;
       ENDIF;
       //
       //
       IF LCRRCT <> *BLANKS;
         *IN42 = *ON;
       ENDIF;
       //
       //** Begin Add *** 05/21/2014 ********************************************
       If printerFile;
         // Current Date
         currDate = alphaUSA(systemDate());
         // Recipient Name
         If LCrrn2 <> *Blanks;
           toName = %Trim(LCrrn1) + ' - ' + LCrrn2;
         Else;
           toName = LCrrn1;
         EndIf;
         // Recipient Address
         toAddr1 = LCrrA1;
         If LCrrA2 <> *Blanks;
           toAddr2 = LCrrA2;
           toAddr3 = LCrrCt;
         Else;
           toAddr2 = LCrrCt;
           toAddr3 = *Blanks;
         EndIf;
         // Employee Name
         employee = LCclNm;
         // Employer Name
         employer = LCemNm;
         // Social Security #
         socSec# = 'XXX-XX-' + %Subst(LCss#:6:4);
         // Accident Date
         accDate = alphaUSA(cvtToDate(LCacDt));
         // Claim #
         claim# = %EditC(LCfnd:'X') + '-' +
             %EditC(LCfYr:'X') + '-' + %EditC(LCcase:'X');
         // Pre-Authorization #
         preAuth# = LCcert;
         // Diagnosis Code
         If LCicd1 <> ' ';
            diagCode = %Trim(LCicd1) + '-' + LCids1;
         EndIf;
         //
         // Write to correct record format
         //
         Select;
         When LCdcId = 'APPE2014';
           Write APPE2014;
         When LCdcId = 'ADVE2014';
           Write ADVE2014;
         EndSl;
         //
       Else;
         //** End   Add *** 05/21/2014 ********************************************
         //
         EXCEPT HEAD;
         //
         //
         IF LC70A1 <> *BLANKS;
           *IN71 = *ON;
         ENDIF;
         //
         IF LC70A2 <> *BLANKS;
           *IN72 = *ON;
         ENDIF;
         //
         IF LC70A3 <> *BLANKS;
           *IN73 = *ON;
         ENDIF;
         //
         IF LC70A4 <> *BLANKS;
           *IN74 = *ON;
         ENDIF;
         //
         IF LC70A5 <> *BLANKS;
           *IN75 = *ON;
         ENDIF;
         //
         IF LC70A6 <> *BLANKS;
           *IN76 = *ON;
         ENDIF;
         //
         IF LC70A7 <> *BLANKS;
           *IN77 = *ON;
         ENDIF;
         //
         IF LC70A8 <> *BLANKS;
           *IN78 = *ON;
         ENDIF;
         //
         IF LC70A9 <> *BLANKS;
           *IN79 = *ON;
         ENDIF;
         //
         IF LC70AA <> *BLANKS;
           *IN70 = *ON;
         ENDIF;
         //
         //
         IF LC70AB <> *BLANKS;
           *IN80 = *ON;
         ENDIF;
         //
         //
         IF LC70AC <> *BLANKS;
           *IN81 = *ON;
         ENDIF;
         //
         //
         IF LC70AD <> *BLANKS;
           *IN82 = *ON;
         ENDIF;
         //
         //
         IF LC70AE <> *BLANKS;
           *IN83 = *ON;
         ENDIF;
         //
         //
         IF LC70AF <> *BLANKS;
           *IN84 = *ON;
         ENDIF;
         //
         //
         IF LC70AG <> *BLANKS;
           *IN85 = *ON;
         ENDIF;
         //
         //
         IF LC70AH <> *BLANKS;
           *IN86 = *ON;
         ENDIF;
         //
         IF LC70AI <> *BLANKS;
           *IN87 = *ON;
         ENDIF;
         //
         IF LC70AJ <> *BLANKS;
           *IN88 = *ON;
         ENDIF;
         //
         IF LC70AK <> *BLANKS;
           *IN89 = *ON;
         ENDIF;
         //
         IF LC70AL <> *BLANKS;
           *IN90 = *ON;
         ENDIF;
         //
         IF LC70AM <> *BLANKS;
           *IN91 = *ON;
         ENDIF;
         //
         IF LC70AN <> *BLANKS;
           *IN92 = *ON;
         ENDIF;
         //
         IF LC70AO <> *BLANKS;
           *IN93 = *ON;
         ENDIF;
         //
         IF LC70AP <> *BLANKS;
           *IN94 = *ON;
         ENDIF;
         //
         IF LC70AQ <> *BLANKS;
           *IN95 = *ON;
         ENDIF;
         //
         IF LC70AR <> *BLANKS;
           *IN96 = *ON;
         ENDIF;
         //
         IF LC70AS <> *BLANKS;
           *IN97 = *ON;
         ENDIF;
         //
         IF LC70AT <> *BLANKS;
           *IN98 = *ON;
         ENDIF;
         //
         IF LC70AU <> *BLANKS;
           *IN99 = *ON;
         ENDIF;
         //
         IF *IN71 = *OFF
         AND *IN72 = *OFF
         AND *IN73 = *OFF
         AND *IN74 = *OFF
         AND *IN75 = *OFF
         AND *IN76 = *OFF
         AND *IN77 = *OFF
         AND *IN78 = *OFF
         AND *IN79 = *OFF
         AND *IN80 = *OFF
         AND *IN81 = *OFF
         AND *IN82 = *OFF
         AND *IN83 = *OFF
         AND *IN84 = *OFF
         AND *IN85 = *OFF
         AND *IN86 = *OFF
         AND *IN87 = *OFF
         AND *IN88 = *OFF
         AND *IN89 = *OFF
         AND *IN90 = *OFF
         AND *IN91 = *OFF
         AND *IN92 = *OFF
         AND *IN93 = *OFF
         AND *IN94 = *OFF
         AND *IN95 = *OFF
         AND *IN96 = *OFF
         AND *IN97 = *OFF
         AND *IN98 = *OFF
         AND *IN99 = *OFF;
           *IN55 = *ON;
         ENDIF;
         //
         EXCEPT FOOTER;
         //** Begin Add *** 05/21/2014 ********************************************
       EndIf;
       //** End   Add *** 05/21/2014 ********************************************
       //
       *INLR = *ON;
       //
       //** Beg Chg *** 02/19/13 *************************************************
       //****QSYSPRT E   13           HEAD
       //** Beg Chg *** 03/05/13 *************************************************
       //****QSYSPRT E   12           HEAD
      /END-FREE
     OQSYSPRT   E            HEAD             13
      //** End Chg *** 03/05/13 *************************************************
      //****                    DSDATEY   20
      //** Beg Chg *** 03/05/13 *************************************************
     O                       DSDATE        Y     27
      //******                  DSDATEY   16
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *************************************************
      //Current Date
      //
      //Provider Name
      //** Beg Chg *** 02/19/13 *************************************************
      //***    E   14           HEAD
      //** Beg Chg *** 03/05/13 *************************************************
      //***    E   13           HEAD
     O          E            HEAD             14
      //** End Chg *** 03/05/13 *************************************************
      //******                  LCRRN2    52
      //** Beg Chg *** 03/05/13 *************************************************
     O                       LCRRN2              59
      //******                  LCRRN2    48
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *************************************************
      //
      //Provider Address line one
     O          E    39      HEAD        1
      //** Beg Chg *** 02/19/13 *************************************************
      //******          39      LCRRN1    52
      //** Beg Chg *** 03/05/13 *************************************************
     O               39      LCRRN1              59
      //******          39      LCRRN1    48
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *************************************************
      //
      //Provider Address line two
     O          E    40      HEAD        1
      //** Beg Chg *** 02/19/13 *************************************************
      //****            40      LCRRA1    52
      //** Beg Chg *** 03/05/13 *************************************************
     O               40      LCRRA1              59
      //******          40      LCRRA1    48
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *************************************************
      //
      //Provider Address line Three
     O          E    41      HEAD        1
      //** Beg Chg *** 02/19/13 *************************************************
      //*****           41      LCRRA2    52
      //** Beg Chg *** 03/05/13 *************************************************
     O               41      LCRRA2              59
      //******          41      LCRRA2    48
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *************************************************
      //
      //Provider Address line Four
     O          E    42      HEAD        1
      //** Beg Chg *** 02/19/13 *************************************************
      //*****           42      LCRRCT    52
      //** Beg Chg *** 03/05/13 *************************************************
     O               42      LCRRCT              59
      //******          42      LCRRCT    48
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *************************************************
      //
      //Injured Employee Name, APPROVAL LETTER
      //** Beg Chg *** 03/05/13 *************************************************
      //*****    E    51      HEAD             21
     O          E    51      HEAD             21
      //** End Chg *** 03/05/13 *************************************************
      //** Beg Chg *** 02/19/13 ********************************
      //******          51      LCCLNM    61
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCCLNM              74
      //******          51      LCCLNM    64
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Employer Name
     O          E    51      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //******          51      LCEMNM    61
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCEMNM              74
      //******          51      LCEMNM    64
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Social Security Number APPROVAL LETTER
     O          E    51      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //*****           51      LCSS#3    20
      //*****           51                21 '-'
      //*****           51      LCSS#2    23
      //*****           51                24 '-'
      //*****           51      LCSS#4    28
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCSS#3              31
     O               51                          32 '-'
     O               51      LCSS#2              34
     O               51                          35 '-'
     O               51      LCSS#4              39
      //******          51      LCSS#3    21
      //******          51                22 '-'
      //******          51      LCSS#2    24
      //******          51                25 '-'
      //******          51      LCSS#4    29
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Date of Injury         APPROVAL LETTER
     O          E    51      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //****            51      XXDAT6Y   24
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      XXDAT6        Y     35
      //******          51      XXDAT6Y   25
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //File Number            APPROVAL LETTER
     O          E    51      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //****            51      LCSUB#    22
      //****            51                23 '-'
      //****            51      LCFND     26
      //****            51                27 '-'
      //****            51      LCFYR     30
      //****            51                31 '-'
      //****            51      LCCASE    38
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCSUB#              33
     O               51                          34 '-'
     O               51      LCFND               37
     O               51                          38 '-'
     O               51      LCFYR               40
     O               51                          42 '-'
     O               51      LCCASE              49
      //******          51      LCSUB#    23
      //******          51                24 '-'
      //******          51      LCFND     27
      //******          51                28 '-'
      //******          51      LCFYR     31
      //******          51                32 '-'
      //******          51      LCCASE    39
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Preauthorization NumberAPPROVAL LETTER
     O          E    51      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //****            51      LCCERT    43
      //****            51      LCLIAB    45
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCCERT              63
     O               51      LCLIAB              65
      //******          51      LCCERT    53
      //******          51      LCLIAB    55
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Diagnosis              APPROVAL LETTER
     O          E    51      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //***             51      LCICD1    25
      //***             51      LCIDS1    78
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCICD1              42
     O               51      LCIDS1              95
      //******          51      LCICD1    32
      //******          51      LCIDS1    85
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Injured Employee Name  DENIAL LETTER
      //** Beg Chg *** 03/05/13 *************************************************
      //******   E    54      HEAD             20
     O          E    54      HEAD             20
      //** End Chg *** 03/05/13 *************************************************
      //** Beg Chg *** 02/19/13 *******************************
      //****            54      LCCLNM    61
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCCLNM              75
      //******          54      LCCLNM    65
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Employer Name          DENIAL LETTER
     O          E    54      HEAD        1
      //** Beg Chg *** 02/19/13 *******************************
      //***             54      LCEMNM    61
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCEMNM              75
      //******          54      LCEMNM    65
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Social Security Number DENIAL LETTER
     O          E    54      HEAD        1
      //** Beg Chg *** 02/19/13 *******************************
      //****            54      LCSS#3    20
      //****            54                21 '-'
      //****            54      LCSS#2    23
      //****            54                24 '-'
      //****            54      LCSS#4    28
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCSS#3              31
     O               54                          32 '-'
     O               54      LCSS#2              34
     O               54                          35 '-'
     O               54      LCSS#4              39
      //******          54      LCSS#3    21
      //******          54                22 '-'
      //******          54      LCSS#2    24
      //******          54                25 '-'
      //******          54      LCSS#4    29
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Date of Injury         DENIAL LETTER
     O          E    54      HEAD        1
      //** Beg Chg *** 02/19/13 *******************************
      //***             54      XXDAT6Y   24
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      XXDAT6        Y     35
      //******          54      XXDAT6Y   25
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //File Number            DENIAL LETTER
     O          E    54      HEAD        1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           54      LCSUB#    22
      //*****           54                23 '-'
      //*****           54      LCFND     26
      //*****           54                27 '-'
      //*****           54      LCFYR     30
      //*****           54                31 '-'
      //*****           54      LCCASE    38
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCSUB#              33
     O               54                          34 '-'
     O               54      LCFND               37
     O               54                          38 '-'
     O               54      LCFYR               41
     O               54                          42 '-'
     O               54      LCCASE              49
      //******          54      LCSUB#    23
      //******          54                24 '-'
      //******          54      LCFND     27
      //******          54                28 '-'
      //******          54      LCFYR     31
      //******          54                32 '-'
      //******          54      LCCASE    39
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Preauthorization NumberDENIAL LETTER
     O          E    54      HEAD        1
      //** Beg Chg *** 02/19/13 *******************************
      //****            54      LCCERT    43
      //****            54      LCLIAB    45
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCCERT              63
     O               54      LCLIAB              64
      //******          54      LCCERT    53
      //******          54      LCLIAB    54
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Diagnosis              DENIAL LETTER
     O          E    54      HEAD        1
      //** Beg Chg *** 02/19/13 *******************************
      //***             54      LCICD1    25
      //***             54      LCIDS1    78
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCICD1              40
     O               54      LCIDS1              93
      //******          54      LCICD1    30
      //******          54      LCIDS1    83
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Injured Employee Name  COMP   LETTER
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   20   56      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
      //****** E   19   56      HEAD
     O          E    56      HEAD             20
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 ********************************
      //****            56      LCCLNM    61
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCCLNM              74
      //******          56      LCCLNM    64
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Employer Name          COMP   LETTER
     O          E    56      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //****            56      LCEMNM    61
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCEMNM              74
      //******          56      LCEMNM    64
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Social Security Number COMP   LETTER
     O          E    56      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //****            56      LCSS#3    20
      //****            56                21 '-'
      //****            56      LCSS#2    23
      //****            56                24 '-'
      //****            56      LCSS#4    28
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCSS#3              31
     O               56                          32 '-'
     O               56      LCSS#2              34
     O               56                          35 '-'
     O               56      LCSS#4              39
      //******          56      LCSS#3    21
      //******          56                22 '-'
      //******          56      LCSS#2    24
      //******          56                25 '-'
      //******          56      LCSS#4    29
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Date of Injury         COMP   LETTER
     O          E    56      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //***             56      XXDAT6Y   24
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      XXDAT6        Y     35
      //******          56      XXDAT6Y   25
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //File Number            DENIAL LETTER
     O          E    56      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //****            56      LCSUB#    22
      //****            56                23 '-'
      //****            56      LCFND     26
      //****            56                27 '-'
      //****            56      LCFYR     30
      //****            56                31 '-'
      //****            56      LCCASE    38
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCSUB#              33
     O               56                          34 '-'
     O               56      LCFND               37
     O               56                          38 '-'
     O               56      LCFYR               41
     O               56                          42 '-'
     O               56      LCCASE              49
      //******          56      LCSUB#    23
      //******          56                24 '-'
      //******          56      LCFND     27
      //******          56                28 '-'
      //******          56      LCFYR     31
      //******          56                32 '-'
      //******          56      LCCASE    39
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Preauthorization NumberDENIAL LETTER
     O          E    56      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //***             56      LCCERT    43
      //***             56      LCLIAB    45
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCCERT              63
     O               56      LCLIAB              64
      //******          56      LCCERT    53
      //******          56      LCLIAB    55
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Diagnosis              DENIAL LETTER
     O          E    56      HEAD        1
      //** Beg Chg *** 02/19/13 ********************************
      //***             56      LCICD1    25
      //***             56      LCIDS1    78
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCICD1              42
     O               56      LCIDS1              95
      //******          56      LCICD1    32
      //******          56      LCIDS1    85
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //
      //Injured Employee (Footer Section), Approval Letter.
      //** Beg Chg *** 02/19/13 *******************************
      //****   E   53   51      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
      //****** E   54   51      HEAD
     O          E    51      HEAD             55
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //******          51      LCCLNM    64
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCCLNM              88
      //******          51      LCCLNM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Injured Employee (Footer Section), Decline Letter.
      //** Beg Chg *** 02/19/13 *******************************
      //*****  E   57   54      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
     O          E    54      HEAD             57
      //****** E   56   54      HEAD
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //****            54      LCCLNM    64
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCCLNM              88
      //******          54      LCCLNM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Injured Employee (Footer Section), Comp Letter.
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   56   56      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
     O          E    56      HEAD             57
      //****** E   56   56      HEAD
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //***             56      LCCLNM    63
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCCLNM              88
      //******          56      LCCLNM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //
      //Treating Doctor (Footer Section), Approval letter.
      //** Beg Chg *** 02/19/13 *******************************
      //*****  E   54   51      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
      //****** E   55   51      HEAD
     O          E    51      HEAD             56
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //*****           51      LCTPNM    68
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCTPNM              88
      //******          51      LCTPNM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Adjuster Representative (Footer Section)
      //** Beg Chg *** 02/19/13 *******************************
      //****   E   56   51      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
      //****** E   56   51      HEAD
     O          E    51      HEAD             57
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //*****           51      LCCANM    63
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCCANM              88
      //******          51      LCCANM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Treating Doctor (Footer Section), Decline letter.
      //** Beg Chg *** 02/19/13 *******************************
      //****   E   58   54      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
     O          E    54      HEAD             58
      //****** E   57   54      HEAD
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //****            54      LCTPNM    68
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCTPNM              88
      //******          54      LCTPNM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Adjuster Representative (Footer Section)
      //** Beg Chg *** 02/19/13 *******************************
      //****   E   60   54      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
     O          E    54      HEAD             59
      //****** E   58   54      HEAD
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //*****           54      LCCANM    63
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCCANM              88
      //******          54      LCCANM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Treating Doctor (Footer Section), Comp letter.
      //** Beg Chg *** 02/19/13 *******************************
      //****   E   57   56      HEAD
      //** Beg Chg *** 03/05/13 *************************************************
     O          E    56      HEAD             58
      //****** E   57   56      HEAD
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //***             56      LCTPNM    68
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCTPNM              88
      //******          56      LCTPNM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Adjuster Representative (Footer Section)
      //** Beg Chg *** 03/05/13 *************************************************
     O          E    56      HEAD             59
      //****** E   58   56      HEAD
      //** End Chg *** 03/05/13 *************************************************
      //** Beg Chg *** 02/19/13 *******************************
      //***             56      LCCANM    63
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCCANM              88
      //******          56      LCCANM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //
      //    ********************************
      //   ******Ratification Letter*******
      //    ********************************
      //
      //Current Date
     O          E            FOOTER            4
      //** Beg Chg *** 02/19/13 *******************************
      //*****                   DSDATEY   20
      //** Beg Chg *** 03/05/13 *************************************************
     O                       DSDATE        Y     27
      //******                  DSDATEY   16
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //Injured Employee Name
      //** Beg Chg *** 02/19/13 *******************************
      //***    E    9   51      FOOTER
      //****            51      LCCLNM    62
     O          E    51      FOOTER            7
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCCLNM              75
      //******          51      LCCLNM    65
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //***    E    8   56      FOOTER
     O          E    56      FOOTER            7
      //****            56      LCCLNM    62
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCCLNM              75
      //******          56      LCCLNM    65
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
     O          E    54      FOOTER            9
      //** Beg Chg *** 02/19/13 *******************************
      //*****           54      LCCLNM    62
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCCLNM              75
      //******          54      LCCLNM    65
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Preauthorization Number
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   10   51      FOOTER
     O          E    51      FOOTER            8
      //****            51      LCCERT    43
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      LCCERT              63
      //******          51      LCCERT    53
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
     O          E    54      FOOTER           10
      //** Beg Chg *** 02/19/13 *******************************
      //*****           54      LCCERT    43
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      LCCERT              63
      //******          54      LCCERT    53
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //****   E    9   56      FOOTER
     O          E    56      FOOTER            8
      //***             56      LCCERT    43
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      LCCERT              63
      //******          56      LCCERT    53
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Date of Injury
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   11   51      FOOTER
     O          E    51      FOOTER            9
      //****            51      XXDAT6Y   24
      //** Beg Chg *** 03/05/13 *************************************************
     O               51      XXDAT6        Y     36
      //******          51      XXDAT6Y   26
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
     O          E    54      FOOTER           11
      //** Beg Chg *** 02/19/13 *******************************
      //****            54      XXDAT6Y   24
      //** Beg Chg *** 03/05/13 *************************************************
     O               54      XXDAT6        Y     36
      //******          54      XXDAT6Y   26
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   10   56      FOOTER
     O          E    56      FOOTER            9
      //****            56      XXDAT6Y   24
      //** Beg Chg *** 03/05/13 *************************************************
     O               56      XXDAT6        Y     36
      //******          56      XXDAT6Y   26
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A1, print Only if not blank.
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   16   71 51   FOOTER
     O          E    71 51   FOOTER           13
      //*****           71 51   LC70A1    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               71 51   LC70A1              91
      //******          71 51   LC70A1    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
     O          E    71 54   FOOTER           16
      //** Beg Chg *** 02/19/13 *******************************
      //****            71 54   LC70A1    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               71 54   LC70A1              91
      //******          71 54   LC70A1    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //****   E   15   71 56   FOOTER
     O          E    71 56   FOOTER           13
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 ********************************
      //***             71 56   LC70A1    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               71 56   LC70A1              91
      //******          71 56   LC70A1    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 ********************************
      //
      //Recommendations Field LC70A2, print Only if not blank.
     O          E    72      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            72      LC70A2    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               72      LC70A2              91
      //******          72      LC70A2    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A3, print Only if not blank.
     O          E    73      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //********        73      LC70A3    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               73      LC70A3              91
      //******          73      LC70A3    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A4, print Only if not blank.
     O          E    74      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           74      LC70A4    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               74      LC70A4              91
      //******          74      LC70A4    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A5, print Only if not blank.
     O          E    75      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            75      LC70A5    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               75      LC70A5              91
      //******          75      LC70A5    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A6, print Only if not blank.
     O          E    76      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           76      LC70A6    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               76      LC70A6              91
      //******          76      LC70A6    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A7, print Only if not blank.
     O          E    77      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            77      LC70A7    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               77      LC70A7              91
      //******          77      LC70A7    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70A8, print Only if not blank.
     O          E    78      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            78      LC70A8    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               78      LC70A8              91
      //******          78      LC70A8    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //RecommenDations Field LC70A9, print Only if not blank.
     O          E    79      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           79      LC70A9    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               79      LC70A9              91
      //******          79      LC70A9    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Recommendations Field LC70AA, print Only if not blank.
     O          E    70      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            70      LC70AA    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               70      LC70AA              91
      //******          70      LC70AA    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AB, print Only if not blank.
      //** Beg Chg *** 02/19/13 *******************************
      //*****  E   42   80 51   FOOTER
     O          E    80 51   FOOTER           39
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //*****           80 51   LC70AB    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               80 51   LC70AB              91
      //******          80 51   LC70AB    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //***    E   41   80 56   FOOTER
     O          E    80 56   FOOTER           39
      //** End Chg *** 02/19/13 *******************************
      //** Beg Chg *** 02/19/13 *******************************
      //****            80 56   LC70AB    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               80 56   LC70AB              91
      //******          80 56   LC70AB    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
     O          E    80 54   FOOTER           30
      //** Beg Chg *** 02/19/13 *******************************
      //****            80 54   LC70AB    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               80 54   LC70AB              91
      //******          80 54   LC70AB    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AC, print Only if not blank.
     O          E    81      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            81      LC70AC    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               81      LC70AC              91
      //******          81      LC70AC    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AD, print Only if not blank.
     O          E    82      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            82      LC70AD    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               82      LC70AD              91
      //******          82      LC70AD    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AE, print Only if not blank.
     O          E    83      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //******          83      LC70AE    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               83      LC70AE              91
      //******          83      LC70AE    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AF, print Only if not blank.
     O          E    84      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            84      LC70AF    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               84      LC70AF              91
      //******          84      LC70AF    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AG, print Only if not blank.
     O          E    85      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            85      LC70AG    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               85      LC70AG              91
      //******          85      LC70AG    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AH, print Only if not blank.
     O          E    86      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //******          86      LC70AH    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               86      LC70AH              91
      //******          86      LC70AH    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AI, print Only if not blank.
     O          E    87      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           87      LC70AI    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               87      LC70AI              91
      //******          87      LC70AI    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AJ, print Only if not blank.
     O          E    88      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            88      LC70AJ    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               88      LC70AJ              91
      //******          88      LC70AJ    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AK, print Only if not blank.
     O          E    89      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           89      LC70AK    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               89      LC70AK              91
      //******          89      LC70AK    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AL, print Only if not blank.
      //** Beg Chg *** 02/19/13 *******************************
      //***    E 2      90      FOOTER
     O          E    90      FOOTER      1
      //*****           90      LC70AL    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               90      LC70AL              91
      //******          90      LC70AL    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AM, print Only if not blank.
     O          E    91      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //******          91      LC70AM    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               91      LC70AM              91
      //******          91      LC70AM    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AN, print Only if not blank.
     O          E    92      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //********        92      LC70AN    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               92      LC70AN              91
      //******          92      LC70AN    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AO, print Only if not blank.
     O          E    93      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           93      LC70AO    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               93      LC70AO              91
      //******          93      LC70AO    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AP, print Only if not blank.
     O          E    94      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           94      LC70AP    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               94      LC70AP              91
      //******          94      LC70AP    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AQ, print Only if not blank.
     O          E    95      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           95      LC70AQ    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               95      LC70AQ              91
      //******          95      LC70AQ    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AR, print Only if not blank.
     O          E    96      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //***             96      LC70AR    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               96      LC70AR              91
      //******          96      LC70AR    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AS, print Only if not blank.
     O          E    97      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //****            97      LC70AS    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               97      LC70AS              91
      //******          97      LC70AS    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AT, print Only if not blank.
     O          E    98      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*******         98      LC70AT    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               98      LC70AT              91
      //******          98      LC70AT    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
      //Rationale Considered  LC70AU, print Only if not blank.
     O          E    99      FOOTER      1
      //** Beg Chg *** 02/19/13 *******************************
      //*****           99      LC70AU    82
      //** Beg Chg *** 03/05/13 *************************************************
     O               99      LC70AU              91
      //******          99      LC70AU    78
      //** End Chg *** 03/05/13 *************************************************
      //** End Chg *** 02/19/13 *******************************
      //
