     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')

     H/TITLE W7281R - PREMIUM DUE (SELF-REPORTING)
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/07/17 C:65  Project: SY371                                 *
     H*                Update to write workfile data using the        *
     H*                template XML file instead of flow file name.   *
     H*                                                               *
     H* 06/22/16 C:136 Project: SY371                                 *
     H*                Add Scriptura Phase 2 processing               *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 Load alternate company name from fund master  *
     H*                                                               *
     H*  05/24/99 C:118 PROJECT:  WC763                               *
     H*                 ADD 'CANCEL DATE' TO REPORT.  MODIFIED 'DATE  *
     H*                 OF LAST REPORT' SO IF ZERO, IT DOESN'T PRINT. *
     H*                                                               *
     H*  10/31/97 C:87 PROJECT WC763                                  *
     H*                CHANGE PGM TO READ BUILT WORKFILE (WXEBL/PEB01)*
     H*                  AND ADD KLIST(EBKEY) FOR CHAIN.              *
     H*                CHANGE QSYSPRT TO PRT OUT 198 COLUMNS WIDE.    *
     H*                ADD PARMS FOR PASS IN PSRT AND SSRT.           *
     H*                DEL READ OF AGENT HISTORY FILE AND KLIST(AHKEY)*
     H*                ADD CODE TO CENTER COMPANY NAME, REPORT TITLE. *
     H*                CHANGE ON ROUTING PAGE TO SHOW PRIMARY SORT    *
     H*                  AND SECONDARY SORT VALUES.                   *
     H*                ADD TO OUTPUT: FIELDS FOR EFFECTIVE AND EXPIR- *
     H*                  ATION DATES, CONTACT NAME AND PHONE NUMBER.  *
     H*                REFORMAT OUTPUT PAGE FOR NEW FIELDS AND WIDTH. *
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE.                            *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     FWDELL1    IF   E           K DISK
     FWASRL2    IF   E           K DISK
     FWTABL     IF   E           K DISK
      *** BEGIN CHG *** 10/31/97 ***********************************
      ***FWMAHL   IF  E           K        DISK
     FWXEBL     IF   E           K DISK
      ***FQSYSPRT O   F      85     OA     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
      *** END   CHG *** 10/31/97 ***********************************
      *** beg add ***** 06/22/16 ******************
     fwxpdp1    o    e             disk    usropn
      *** beg add ***** 06/22/16 ******************
     d*** begin add ***  08/07/17  ****************************************
     FXMLPREL1  IF   E           K DISK
     d*** end add *****  08/07/17  ****************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** beg add ***** 06/21/16 ******************************************
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d processType     s            100    inz
     d strCommand      s            255
     d xmlFile         s             60    inz
     d*** begin add ***  08/07/17  ****************************************
     d xmlTemplFile    s             60    inz
     d*** end add *****  08/07/17  ****************************************
     d/copy sPrModule
     d/copy sPrXML
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const
     d S4401C          pr                  extpgm('S4401C')
     d  xmlFile                      61
      *** end add ***** 06/21/16 ******************************************

     C*
     C     *LIKE         DEFINE    EMDT          XXCDAT
     C     *LIKE         DEFINE    EMDT          XXYMD
     C     *LIKE         DEFINE    SRRPMY        XXRPYM
     C***** BEGIN ADD **** 05/24/99 ***********************************
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C***** END ADD ****** 05/24/99 ***********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQQUE            10            MLS 1/18/88
      *** BEGIN ADD *** 10/31/97 *************************************
     C                   PARM                    QQPSRT            2
     C                   PARM                    QQSSRT            2
      *** END   ADD *** 10/31/97 *************************************
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
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQRPMY        XXRPMY            6 0
     C                   MOVE      QQRPMY        XXRMY1            6 0
     C*
     C                   Z-ADD     XXRMY1        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C*
     C                   MOVEL     XXRPYM        XXYMD
     C                   MOVE      '01'          XXYMD                          |
     C*                                                    MLS 1/18/88
     C*   SET-UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*                                                         "
     C*   KEY LIST FOR THE USER PARAMETER FILE                  "
     C*                                                         "
     C     UPKEY         KLIST                                                       "
     C                   KFLD                    Q1USER                              "
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *** BEGIN ADD *** 10/31/97 *********************************
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*
     C*   KEY LIST FOR THE REPORT WORKFILE
     C*
     C     EBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** END   ADD *** 10/31/97 *********************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXRPYM
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE FILE
     C*
     C     ABKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
      *** BEGIN DEL *** 10/31/97 **********************************
     C* Key List for the Agent History Master File
     C*
     C********** AHKEY     KLIST                           JJJ 3-1-89
     C******************** KFLD           XXCO#            |
     C******************** KFLD           XXFND            |
     C******************** KFLD           ELEMP#           |
     C******************** KFLD           ELDIV            |
     C******************** KFLD           XXFYR            |
      *** END   DEL *** 10/31/97 **********************************
     C*** beg add ***** 06/22/16 ******************************************
      /Free
       processType = 'Premium Due Report - Self Reporting';
       outReturn   = processType;
       outFldrPrefix  = *blanks;
       isUseSpoolOut  = UseSpoolOut(processType);
       isUseScriptura = UseScriptura(processType);
       isUse3rdParty  = Use3rdXml(processType);
       isUseXML = isUseScriptura or isUse3rdParty;
       If isUseXML;
          Open WXPDP1;
          outPreString = 'PremDueRpt';
          xmlFile = GetXMLFileName(' ':*zeros:outCo#:outFnd:
           outFldrPrefix:outPreString:outReturn);
          //*** begin add ***  08/07/17  **********************************
          Chain (xmlFile) xp100;
          xmlTemplFile = XPXmlT;
          //*** end add *****  08/07/17  **********************************
       Endif;
      /End-free
     C*** end add ***** 06/22/16 ******************************************
     C*
     C                   TIME                    DSDTTM
     C*                                                    MLS 1/18/88
     C*   CHAIN TO THE USER PARAMETER FILE                      "
     C*                                                         "
     C     UPKEY         CHAIN     PUP01                              60             "
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Y                 2 0
     C     *BLANK        LOOKUP    CONM(Y)                            60
     C                   MOVEA     CONM(Y)       XXCONM           40
     C*
     C***** BEGIN DEL **** 04/10/00 ***********************************
      *** BEGIN ADD *** 10/31/97 ************************************
      *   CENTER COMPANY NAME IN FIELD
     C**********           MOVELCONAME    SRDFLD    P
     C**********           CALL 'SRCENTER'
     C**********           PARM           SRDFLD 80
     C**********           PARM 40        SRDLEN  30
     C**********           MOVELSRDFLD    CONAME
      *** END   ADD *** 10/31/97 ************************************
     C***** END   DEL **** 04/10/00 ***********************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 04/10/00 *********************************
     C*   CENTER COMPANY NAME IN FIELD
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        CONAME
     C***** END   ADD **** 04/10/00 *********************************
     C*
      *** BEGIN ADD *** 10/31/97 ************************************
      *   CENTER COMPANY NAME IN FIELD
     C                   MOVEL(P)  ORTITL        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        ORTITL
      *** END   ADD *** 10/31/97 ************************************
     C*
     C***** BEGIN ADD **** 05/24/99 ***********************************
     C*  LEFT JUSTIFY TITLE
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 05/24/99 ***********************************
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *******************************
     C*******            EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *******************************
     C                   END
     C*
      *** beg chg ***** 06/22/16 *****************
     C***********        EXCEPT    HEAD
      /free
       If isUseSpoolOut;
           Except Head;
       Endif;
      /end-free
      *** end chg ***** 06/22/16 *****************
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     0             XXECNT            5 0
     C*
     C*   START READING THE EMPLOYER DETAIL FILE
     C*
      *** BEGIN ADD *** 10/31/97 *********************************
     C     EBKEY         CHAIN     PEB01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      EBCO#         XXCO#
     C                   MOVE      EBFND         XXFND
     C                   MOVE      EBFYR         XXFYR
      *
      *** END   ADD *** 10/31/97 *********************************
      *
     C     ELKEY         CHAIN     PEL01                              60
      *** BEGIN CHG *** 10/31/97 *********************************
     C*********  *IN60     DOWEQ'0'
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDT             8 0
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDT             8 0
      *
     C*
      *** END   CHG *** 10/31/97 *********************************
     C*
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     ELBSCD        XXKEY
     C     ABKEY         CHAIN     PBS01                              60
     C     BSSEFL        IFEQ      'S'
     C*
     C*   GET THE EMPLOYER PAYROLL FILE
     C*
     C     SRKEY         CHAIN     PSR01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             SRRPMY
     C*
     C*   FIND THE LATEST DATE
     C*
     C     SRKEY         SETGT     PSR01
     C                   READP     PSR01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     SRCO#
     C     XXFND         ANDEQ     SRFND
     C     XXFYR         ANDEQ     SRFYR
     C     ELEMP#        ANDEQ     SREMP#
     C     ELDIV         ANDEQ     SRDIV
     C     XXRPYM        ANDGE     SRRPMY
     C*
     C     *IN60         OREQ      '0'
     C     XXCO#         ANDNE     SRCO#
     C*
     C     *IN60         OREQ      '0'
     C     XXFND         ANDNE     SRFND
     C*
     C     *IN60         OREQ      '0'
     C     XXFYR         ANDNE     SRFYR
     C*
     C     *IN60         OREQ      '0'
     C     ELEMP#        ANDNE     SREMP#
     C*
     C     *IN60         OREQ      '0'
     C     ELDIV         ANDNE     SRDIV
     C*
     C     *IN60         OREQ      '1'
     C*
     C     XXCO#         IFNE      SRCO#
     C     XXFND         ORNE      SRFND
     C     XXFYR         ORNE      SRFYR
     C     ELEMP#        ORNE      SREMP#
     C     ELDIV         ORNE      SRDIV
     C                   Z-ADD     0             SRRPMY
     C                   END
     C*
     C*   GET THE NAME OF THE EMPLOYER AND PRINT THE REPORT
     C*
     C                   ADD       1             XXECNT
     C     EMKEY         CHAIN     PEM01                              60
      *** beg chg ***** 06/22/16 ****************
     C******INOA         IFEQ      '1'
     C*****              EXCEPT    HEAD
     C*****              MOVE      '0'           *INOA
     C*****              END

      /free
       If *inoa and isUseSpoolOut;
          Except Head;
          *inoa = *off;
       Endif;
      /end-free
      *** end chg ***** 06/22/16 ****************
     C*
     C***** BEGIN ADD **** 05/24/99 ***********************************
     C     SRRPMY        IFEQ      *ZERO
     C                   SETON                                        22
     C*
     C                   ELSE
     C***** END ADD ****** 05/24/99 ***********************************
     C*
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         XXMMYY            6 0
     C*
     C***** BEGIN ADD **** 05/24/99 ***********************************
     C                   ENDIF                                                  SRRPMY=ZERO
     C***** END ADD ****** 05/24/99 ***********************************
     C*
     C*   CHECK THE EFFCTIVE AND CANCELLATION DATES
     C*
     C                   Z-ADD     1             Y
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     5             Y
     C*
     C                   ELSE
     C*
     C                   SUB       1             Y
     C*
     C                   ENDIF
     C*
     C     Y             DOWNE     0
     C*
     C     ELPTDT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C*
     C  N60              ENDDO
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     Y             IFNE      *ZERO
     C*
     C     CD(Y)         IFGE      XXCDAT
     C     CD(Y)         OREQ      0
     C*
     C                   MOVE      '0'           *IN21
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN21
     C*
     C                   ENDIF                                                  CD,Y
     C*
     C                   ELSE
     C*
     C                   SETON                                        21
     C*
     C                   ENDIF                                                  Y NE 0
     C*
     C***** BEGIN ADD **** 05/24/99 ***********************************
     C                   MOVE      *ZERO         XXCDT
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         IFNE      0
     C*
     C                   MOVE      CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCDT             8 0
     C                   ELSE
     C                   SETON                                        23
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 05/24/99 ***********************************
      *** BEGIN DEL *** 10/31/97 ************************************
     C* Get the Agent #                                    JJJ 3-1-89
     C*                                                    |
     C********** AHKEY     SETGTPAH01                      |
     C**********           READPPAH01                    50|
     C********** *IN50     DOWEQ'0'                        |
     C********** AHSTAT    ANDEQ'I'                        |
     C**********           READPPAH01                    50|
     C**********           END                             |
     C*                                                    |
     C********** *IN50     IFEQ '1'                        |
     C********** XXCO#     ORNE AHCO#                      |
     C********** XXFND     ORNE AHFND                      |
     C********** ELEMP#    ORNE AHEMP#                     |
     C********** ELDIV     ORNE AHDIV                      |
     C********** XXFYR     ORNE AHFYR                      |
     C***********          Z-ADD0         AHAGT#           |
     C**********           END                             |
      *** END   DEL *** 10/31/97 ************************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
      *** beg chg ***** 06/22/16 *****************
     C***********        EXCEPT    DET

      /free
       If isUseSpoolOut;
          Except Det;
       Endif;
       If isUseXML;
          Exsr WrtXMLFile;
       Endif;
      /end-free
      *** end chg ***** 06/22/16 *****************

     C***** BEGIN ADD **** 05/24/99 ***********************************
     C                   SETOFF                                       22
     C                   SETOFF                                       23
     C***** END ADD ****** 05/24/99 ***********************************
     C*
     C                   END
     C                   END
     C                   END
     C                   END
      *** BEGIN CHG *** 10/31/97 ***********************************
     C*********  ELKEY     READEPEL01                    60
     C     EBKEY         READE     PEB01                                  60
      *** BEGIN CHG *** 10/31/97 ***********************************
      *
     C                   ENDDO                                                  *IN50=*OFF
     C*
      *** beg chg ***** 06/22/16 ****************
     C***********        EXCEPT    TOTAL
      ****xxPrt*= '0';
      ****Chain*orKey POR01;
      ****If*%found;
      *******Except Route;
      ****Endif;*

      /free
       If isUseSpoolOut;
          Except Total;
          xxPrt = '0';
          Chain orKey POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       If isUseXML;
          Close WXPDP1;
          outXMLFile = xmlFile + '$';
          Callp S4401C(outXMLFile);
       Endif;
      /end-free
      *** end chg ***** 06/22/16 ****************
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/31/02 **********************************
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      ***** END ADD ******* 01/31/02 **********************************
      *** beg add ***** 06/22/16 *****************
      /free
       Begsr WrtXMLFile;
       Clear pdp001;
       //*** begin chg ***  08/07/17  ****************************************
       //*****DPXmlKey = xmlFile;
       DPXmlKey = xmlTemplFile;
       //*** end chg *****  08/07/17  ****************************************
       DPCO#    = xxCo#;
       DPCNAM   = CoName;
       DPFND    = xxFnd;
       DPFNAM   = FNname;
       DPRNAM   = ORtitl;
       DPPNAM   = Q1pgm;
       DPRNDT   = DSdate;
       DPRNTM   = DStime;
       DPSRMY   = xxRpMy;
       DPEMP#   = EMemp#;
       DPDIV    = EMdiv;
       DPENAM   = EMname;
       DPAGT#   = EBagt#;
       DPLRPT   = xxmmyy;
       DPPFDT   = xxfdt;
       DPPTDT   = xxtdt;
       DPCXDT   = xxcdt;
       DPAREA   = EMarea;
       DPPHN#   = EMphn#;
       DPCONT   = EMcont;
       Write pdp001;
       Endsr;
      /end-free
      *** end add ***** 06/22/16 *****************

     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END   CHG ******** 05/24/99 *******************************
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 35
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********                         49 'SELECTION CRITERIA:'
     O                                           25 'SELECTION CRITERIA:'
     O***** END   CHG ******** 05/24/99 *******************************
      *** BEGIN DEL *** 10/31/97 ***********************************
     O******  E  2             ROUTE
     O*********************              21 'YEAR  . . . . . . . :'
     O*********************    XXFYR X   25
      *** BEGIN DEL *** 10/31/97 ***********************************
     O          E            ROUTE          2
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********                         21 'REPORTING MONTH/YEAR:'
     O**********               XXRPMY    29 '0  /    '
     O                                           27 'REPORTING MONTH/YEAR:'
     O                       XXRPMY              35 '0  /    '
     O***** END   CHG ******** 05/24/99 *******************************
      *
      *** BEGIN CHG *** 10/31/97 **********************************
     O******* E  2             ROUTE
     O************                       21 'SORT  . . . . . . . :'
     O*******************                23 'E'
     O          E            ROUTE          2
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********                         21 'PRIMARY SORT  . . . :'
     O**********               QQPSRT    24
     O                                           27 'PRIMARY SORT  . . . :'
     O                       QQPSRT              30
     O          E            ROUTE          2
     O**********                         21 'SECONDARY SORT  . . :'
     O**********               QQSSRT    24
     O                                           27 'SECONDARY SORT  . . :'
     O                       QQSSRT              30
     O***** END   CHG ******** 05/24/99 *******************************
      *** END   CHG *** 10/31/97 **********************************
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
      *** BEGIN CHG *** 10/31/97 **********************************
     O************************ CONAME    62
     O                       CONAME             118
     O************************ DSDATEY   84
     O                       DSDATE        Y    196
     O          E            HEAD           1
     O                       Q1PGM               10
     O************************ ORTITL    72
     O                       ORTITL             128
     O************************           75 'PAGE:'
     O************************ PAGE  Z   84
     O                                          187 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
     O********************************   75 'RUN DATE:'
     O                                          187 'REPORTING MONTH/YEAR:'
     O***********************  XXRPMY    84 '0  /    '
      *** END   CHG *** 10/31/97 ***********************************
     O                       XXRPMY             196 '0  /    '
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********               FNFND Z   10
     O**********               FNNAME    71
     O                       FNFND         Z     11
     O                       FNNAME              73
     O***** END   CHG ******** 05/24/99 *******************************
      *** BEGIN CHG *** 10/31/97 **********************************
     O************************           79 'YEAR:'
     O***********************  XXFYR Z   84
     O          E            HEAD           1
     O***** BEGIN CHG ***** 05/24/99 **********************************
     O**********                         97 'DATE OF'
     O**********                        112 'EFF.'
     O**********                        127 'EXP.'
     O                                           89 'DATE OF'
     O                                          102 'EFF.'
     O                                          117 'EXP.'
     O***** END   CHG ***** 05/24/99 **********************************
     O***** BEGIN ADD ****** 05/24/99 *********************************
     O                                          134 'CANCEL'
     O***** END ADD ******** 05/24/99 *********************************
     O          E            HEAD           2
     O********************************** 23 'EMPLOYER   DIV    NAME'
     O                                           20 'EMPLOYER        DIV'
     O                                           30 'NAME'
     O                                           78 'AGENT#'
     O*********************************  84 'DT OF LAST REPORT'
     O***** BEGIN CHG ****** 05/24/99 *********************************
     O**********                         98 'LAST RPT'
     O**********                        112 'DATE'
     O**********                        127 'DATE'
     O**********                        142 'DATE'
     O**********                        142 'PHONE#'
     O**********                        162 'CONTACT'
     O**********                        153 'PHONE#'
     O**********                        173 'CONTACT'
     O                                           90 'LAST RPT'
     O                                          102 'DATE'
     O                                          117 'DATE'
     O                                          132 'DATE'
     O                                          146 'PHONE#'
     O                                          165 'CONTACT'
     O***** END CHG ******** 05/24/99 *********************************
      *** END   CHG *** 10/31/97 **********************************
     O          E            DET            1
     O                       EMEMP#        Z     09
     O               21                          11 'CX'
     O                       EMDIV         Z     20
      *** BEGIN CHG *** 10/31/97 **********************************
     O************************ EMNAME    59
     O                       EMNAME              66
     O************************ AHAGT#Z   64
     O                       EBAGT#        Z     78
     O************************ XXMMYY    78 '0  /    '
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********               XXMMYY    97 '0  /    '
     O              N22      XXMMYY              90 '0  /    '
     O***** END CHG ********** 05/24/99 *******************************
     O***** BEGIN CHG ******** 05/24/99 *******************************
     O**********               XXFDT Y  115
     O**********               XXTDT Y  130
     O**********                        136 '('
     O**********               EMAREA   139
     O**********                        140 ')'
     O**********               EMPHN#   149 '0   -    '
     O**********               EMCONT   195
     O                       XXFDT         Y    105
     O                       XXTDT         Y    120
     O              N23      XXCDT         Y    135
     O                                          141 '('
     O                       EMAREA             144
     O                                          145 ')'
     O                       EMPHN#             154 '0   -    '
     O                       EMCONT             198
     O***** END CHG ********** 05/24/99 *******************************
      *** END   CHG *** 10/31/97 **********************************
     O          E            TOTAL       1  1
     O                                           12 'EMPLR TOTAL:'
     O                       XXECNT        J     18
