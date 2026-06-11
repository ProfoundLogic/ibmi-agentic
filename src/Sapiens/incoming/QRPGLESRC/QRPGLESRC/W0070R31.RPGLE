     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H* ***************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0070R31 - NOI Reprint Confirmation Screen
     H* ***************************************************************
     H* Description:                                                  *
     H*                                                               *
     H* Called from: see WTCLMTWRP - HKEALY 10/07/13                  *
     H* Calls to:    W8070Cxx where xx is the FROI HARDCOPY FORMAT CD *
     H*              such as 'A1' or a state abreviation.             *
     H*                                                               *
     H*****************************************************************
     H* Created on 01/07/2005 by c:62 for Project 132DAW              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*
     H* 04/26/23 C:333 Support: 110196
     H*                Reprint FROI does nothing if Scriptura only output
     H*                is enabled.
     H*
     H* 08/29/22 C:340 Project: WC2823
     H*                Bypass EDI 148 Check
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     H*
     H* 07/19/17 C:053  Project: WC2499
     H*                 Implement Scriptura output control window
     H*                 when process is flagged to generate Scriptura
     H*                 output.  The settings are stored/passed via
     H*                 a local data area which is picked up by
     H*                 API's and the printing program.
     H*                                                               *
     H* 10/04/13  C:267  SUPPORT: Red 3116     (COMPILED ONLY)        *
     H*                  Removed HELP botton and 'press ENTER'        *
     H*                  instructions from UI.                        *
     H*                                                               *
     H*****************************************************************
     FW0070F31  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     INFDS(DBFDS)
      *** Beg add *****  08/29/22  ***********************************
     FWMIXL     IF   E           K DISK
      *** Beg add *****  08/29/22  ***********************************
      *** begin add ***  07/19/17  ************************************
     FSTSPL     IF   E           K DISK    rename(psp01:psp00)
     FSMUPL     IF   E           K DISK    rename(pup01:pup00)
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d isUseSpoolOut   s              1n   inz
     d processType     c                   const('First Report of Injury')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
      *
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add *****  07/19/17  ************************************
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQMODE            1
     C     XXRRN         PARM                    QQRRN             9 0
     C     XXFROI        PARM                    QQFROI            2
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
    C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*MDY'        srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*  Define and initialize work fields
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    DBRRN         XXRRN
     C     *LIKE         DEFINE    QQFROI        XXFROI
      *
      *** begin add ***  07/19/17  ***********************************
      /Free
       isUseScript = UseScriptura(processType);
       isUseSpoolOut  = UseSpoolOut(processType);
       @@SCRP = isUseScript;
       @@SPOL = isUseSpoolOut;
      /end-free
      *** end add *****  07/19/17  ***********************************
      *
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  Retrieve the claim record.
     C*
     C     XXRRN         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C*  Retrieve the company name
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDATA
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDATA           80
     C                   PARM      40            SRLENG            3 0
     C                   MOVEL     SRDATA        SCCONM
      *
     C                   MOVE      CMFND         XXFND             3
     C                   MOVE      CMFYR         XXFYR             3
     C                   MOVE      CMCASE        XXCASE            7
     C                   MOVEL     XXFND         SCCASE
     C     SCCASE        CAT(P)    '-':0         SCCASE
     C     SCCASE        CAT(P)    XXFYR:0       SCCASE
     C     SCCASE        CAT(P)    '-':0         SCCASE
     C     SCCASE        CAT(P)    XXCASE:0      SCCASE
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*****
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *OFF
     C*****
     C*  PROCESS REQUEST SCREEN 1
     C*
      *** begin add ***  07/19/17  ***********************************
      /free
       xxSubmit = *on;

       If isUseScript;

          xxProcType = processType;

          In *Lock *dtaara;

          Clear svLDA;

       // Chain(n) exky pex01;
       // If %found and EXMICD <> ' ';
       //    xxDart = 'Y';
       //    xxRMCD = EXMICD;
       // Else;
       //    xxDart = 'N';
       //    xxRMCD = ' ';
       // Endif;

          Out *dtaara;

          //*** begin chg ***  08/13/21  ***********************************
          //***** xxSubmit = OutputWindow(xxProcType:xxMass);
          If xxRRN <> 0;
             xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':xxRRN);
          Else;
             xxSubmit = OutputWindow(xxProcType:xxMass);
          Endif;
          //*** end chg *****  08/13/21  ***********************************

       Endif;
      /end-free

     C                   If        xxSubmit = *off
     C                   Goto      EOJ
     C                   Endif

      /free
      //***** Beg Chg *** 04/26/23 ***********************************
       //***If isUseSpoolOut;
       If isUseSpoolOut or isUseScript;
      //***** End Chg *** 04/26/23 ***********************************
      /end-free
      *** end add *****  07/19/17  ***********************************
     C*
     C*  If ENTER is pressed, call the corresponding NOI print program.
     C*
      *** Beg add *****  08/29/22  ***********************************
         Chain(n) (kkCo#:qqFroi:'148') wmixl;
         If Not %Found(wmixl);
           xxFroi = 'A1';
         EndIf;
      *** End add *****  08/29/22  ***********************************
     C                   MOVE      CMCO#         @@CO#
     C                   MOVE      CMFND         @@FND
     C                   MOVE      CMFYR         @@FYR
     C                   MOVE      CMCASE        @@CASE
     C                   MOVE      *BLANKS       @@FROI
     C                   MOVE      *BLANKS       @@JRST
     C                   MOVE      *ZEROS        @@DATE
     C                   MOVEL     'W8070C'      XXPGM             8
     C     XXPGM         CAT(P)    XXFROI:0      XXPGM
      *** Beg Add *****  07/19/17  ***********************************
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtmdy2
     C                   movel     srdto         @@DATE
     C                   MOVEL     XXPGM         @@PGM
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    @@PGM            10
     C                   PARM      '*PGM   '     @@TYPE            7
     C                   PARM                    @EXIST            1
     C     @EXIST        IFNE      '1'
     C*
      *** End Add *****  07/19/17  ***********************************
      *** Beg chg *****  07/19/17  ***********************************
     C***********        CALL      XXPGM                                99
     C                   CALL      'W8070C01'                           99
      *** End chg *****  07/19/17  ***********************************
     C                   PARM                    @@CO#             3
      *** Beg chg *****  07/19/17  ***********************************
     C************       PARM                    @@FROI            2
     C************       PARM                    @@JRST            2
     C                   PARM      XXFROI        @@FROI            2
     C                   PARM      CMJRST        @@JRST            2
      *** End chg *****  07/19/17  ***********************************
     C                   PARM                    @@DATE            8
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
      *** Beg add *****  07/19/17  ***********************************
     C                   PARM                    @@SCRP            1
     C                   PARM                    @@SPOL            1
     C                   PARM                    @@PGM
      *
     C                   ELSE
      *** End add *****  07/19/17  ***********************************
     C     *IN99         IFEQ      *ON
     C                   EXFMT     SCR02
     C                   ENDIF
      *** Beg add *****  07/19/17  ***********************************
     C                   ENDIF
      *
      /free
       Endif;
      /end-free
      *** End add *****  07/19/17  ***********************************
      *
     C                   ENDIF                                                  F12=*ON/*OFF
     C                   ENDIF                                                  CLAIM RECORD?
     C*
     C*  END OF PROGRAM
     C*
      *** Beg add *****  07/19/17  ***********************************
     C     EOJ           TAG
      *** End add *****  07/19/17  ***********************************
     C                   MOVE      *ON           *INLR
     C*
