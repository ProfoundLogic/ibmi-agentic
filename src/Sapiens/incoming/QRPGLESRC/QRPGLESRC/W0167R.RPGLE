     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/TITLE W0167R - REINSTATEMENT INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE CX/REIN FILE.  A         *
     H*                STATE REQUIRED NOTIFICATION FORM IS PRINTED    *
     H*                ALONG WITH AN OPTIONAL LETTER TO CERTIFICATE   *
     H*                HOLDERS.                                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0165R - WORK WITH CANCELLATIONS                           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/16/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
      * REVISIONS:
     H*                                                                        *
     H* 06/21/12 C:282 PROJECT: WC2086                                         *
     H*                Modify to load the new screen field                     *
     H*                Require Reinstatement Reason Code.                      *
     H*                                                                        *
     H* 05/29/07 C:136 SUPPORT: WA114 58478                                    *
     H*               -IF PROCESSING REINSTATEMENT FOR RENEWAL YEARS,          *
     H*                USE THE CX EFFECTIVE DATE IN THE RENEWAL YEAR AS        *
     H*                THE EFFECTIVE DATE OF THE REINSTATEMENT TO PREVENT      *
     H*                THE CEU FROM MIS-REPORTING PREMIUM.                     *
     H*               -DO NOT ALLOW RENEWAL REINSTATEMENT TO PROCESS IF        *
     H*                WDW06 PROMPT IS FLAGGED NOT TO.                         *
     H*                                                                        *
     H* 01/03/07 C:33  Seq#: WA099/44610                                       *
     H*                Release record lock                                     *
      *
      * 11/07/06 C:61  SUPPORT WA129 53118
      *          C:33  Write Quote Status (WAQTP) record with current
      *                agent number.
      *
      * 07/25/05 C:61  PROJECT 133WBV
      *                If a 'Y' is entered in the Cancelled in error
      *                field, set CX count in CX detail (CXCNC#) and
      *                CX history (XHCNC#) to zero.  If an 'N' is
      *                entered and the field previously contained a
      *                'Y', the recalculate the CX count.
      *                If number of cancellations exceeds limit,
      *                do not allow reinstatement.
      *
      * 08/03/04 C:61  PROJECT WC966
      *              - Change "Reinstatement Date" entry to
      *                non-display and default the value to
      *                CX effective date.
      *              - Add information windows to display/prompt when
      *                automatic reinstatement of renewal years will
      *                be performed.
      *              - Add processing to Reinstate renewal year
      *                policies previously cancelled as Automatic
      *                Flat CX of renewal years.
      *
     H* 03/01/04 C:61  SUPPORT WA081 40327                            *
     H*                Seton *INLR at pgm end.  Eliminate possible    *
     H*                carry over of values from previous pgm run.    *
     H*                                                               *
     H* 07/17/03 C:61  WC577                                          *
     H*                Allow Lower case entry in comment lines SCR01  *
     H*                                                               *
     H* 12/18/00 C:61  SUPPORT WA033 25416                            *
     H*                Update WDCXP with current Agent number.        *
     H*                                                               *
     H* 11/13/00  C:35   PROJECT: SUPP WA999 #24603                   *
     H*                  REVISE ALL CANCELLED DIVISIONS WHEN          *
     H*                  PROCESSING A REVISED REINSTATEMENT.          *
     H*                                                               *
     H* 11/01/00  C:33  PROJECT: WC1008                               *
     H*                 WHEN LOOKING FOR CX QUOTE STATUS RECORD, CHECK*
     H*                 FOR TRANSFER QUOTE STATUS WITH QTXFER = 'C'   *
     H*                                                               *
     H* 02/06/00  C:87  PROJECT: SUPP WA107 #21736                    *
     H*                 Check to see if canx has a confirmation date  *
     H*                 pending (if so, display prompt window to make *
     H*                 sure cancellation is appropriate).          *
     H*                                                               *
     H* 10/12/99  C:87 PROJECT: SUPP WA101 #20065                     *
     H*                Array index error caused by not looking at the *
     H*                specific emp dtl rec for each div.             *
     H*                                                               *
     H* 09/02/99 C:61  PROJECT WC920                                  *
     H*                Add two comment fields to Cancellation Detail  *
     H*                WDCXP.                                         *
     H*                                                               *
     H* 07/29/99  C:87  PROJECT: WC757                                *
     H*                 Display pol eff date and pol exp date in      *
     H*                 screen header.  If there are other divs that  *
     H*                 have matching cancellation record then dsply  *
     H*                 prompt window advising all divs will be re-   *
     H*                 instated.  Process all divs for reinstatement *
     H*                 in PGMEND.  Remove code which sets indicators *
     H*                 66-69.  After processing screen info (end of  *
     H*                 ERRCK1) check if someone else has updated     *
     H*                 record.  Make 'Address label type' optional.  *
     H*                                                               *
     H* 07/22/98  C:108 PROJECT SUPPORT: WA093/12777                  *
     H*                 CHANGE CLAIMS-ONLY FLAG FROM "Y" TO "C".      *
     H*                                                               *
     H* 12/24/97  C:35  PROJECT: WC735                                *
     H*                 CHANGED ORDER OF PARAMETERS IN CALL TO W7167C *
     H*                 WHICH WAS CHANGED TO MATCH W7165C.            *
     H*                                                               *
     H* 12/03/97  C:80  PROJECT: WC395                                *
     H*                 WRITE AN 'R' TYPE RECORD TO WAQTP (QUOTE      *
     H*                   FILE)                                       *
     H*                 IF THE LAST QUOTE RECORD IS A 'C' (CANCEL)    *
     H*                   TYPE                                        *
     H*                                                               *
     H* 11/06/97  C:85  PROJECT: WC764                                *
     H*                 VERSION 3 REPORT STANDARDS.                   *
     H*                 ADDED DIVISION COUNT TO SCR01.                *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - ENROLLED IN OV/400                                      *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  66 - COMMERCIAL APPLICATION INSTALLED (WCFL07)               *
     H*  67 - CLAIMS ONLY INSTALLED (WCFL08)                          *
     H*  68 - RETRO/LOSS PGM INSTALLED (WCFL10)                       *
     H*  69 - DEDUCTIBLES INSTALLED (WCFL06)                          *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H* N45 - Display reinstatement flag                              *
     H*  ERROR:                                                       *
     H*  20 - (WCR0500) Invalid cancel date                           *
     H*  21 - (WCR0501) Invalid certificate holder flag               *
     H*  22 - (WCR0513) Invalid table (Cancel Reason)                 *
     H*  23 - (WCR0501) Invalid revision flag                         *
     H*  27 -           Account has additional divisions              *
     H*  37 - (WCR0930) Date not between policy dates                 *
     H*  POSITION CURSOR:                                             *
     H*  30 - (SCRSN ) Cancel Reason                                  *
     H*  31 - (SCCOM1) Comment 1                                      *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0167F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDCXP     UF A E             DISK
     F                                     RENAME(PCX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDCXL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
      *
     FWDCXL10   IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
      *
      *  Cancellation maintenance
     FWTXCL     IF   E           K DISK
      *
      *
      * Rating State
     FWMRSL     IF   E           K DISK
      *
      * Cancellation reason table
     FWTCRL     IF   E           K DISK
      *
     FWDXHL     IF A E           K DISK
     FWDELL1    IF   E           K DISK
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL04)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMUPL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*
     FWAQTL16   IF A E           K DISK
     F*
     D***  BEG ADD  ***  06/21/12  **********************************
      *
      * System table file - Co #, ID, Key, Desc, Date. Tim
     FWTGTL1    IF   E           K DISK
     C***  End ADD  ***  06/21/12  **********************************
     D*
     D*
      ***** Beg Del ***** 06/21/12 ***************************************
     D*****/COPY SCOPYMT01I
      ***** End Del ***** 06/21/12 ***************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDSA          DS
     D  X@RRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Reinstatement Inform-
     D                                     ation')
     D MSG1            C                   CONST('Policy already under-
     D                                      cancellation effect-
     D                                     ive')
     D MSG2            C                   CONST('Policy already cance-
     D                                     lled')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
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
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D***  BEGIN ADD  ***  06/21/12  **********************************
     dstrRrCd          s              4A
     d##PrDs           s             40A
     d##Call           s              1A
     d##TbId           s              4A
     d@@Key            s             10A
     d@@RtCd           s              1A
      *
     DW2493R           PR                  Extpgm('W2493R')
     D##PrDs                         40A   CONST
     D##Call                          1A   CONST
     D##TbId                          4A   CONST
     D@@Key                          10A
     D@@RtCd                          1A
     D***  END   ADD  ***  06/21/12  **********************************
      ***** Beg Add ***** 06/21/12 ***************************************
     C/COPY SCOPYMT05C
      ***** End Add ***** 06/21/12 ***************************************
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCEMP#        ##EMP#            9 0
     C                   PARM      SCDIV         ##DIV             5 0
     C                   PARM      SCFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
      *
      *
      *==============================================================
      *  KLIST
      *==============================================================
      *
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKYX         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C*
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCFYR
      *
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      *
     C     ELKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      *
     C     ELKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C*
     C*  KEY LIST FOR THE CANCELLATION DETAIL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCREQ#
     C*
     C     CXKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     CXKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    X3DATE
      *
     C     CXKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      *
      * Rating State
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
      *  CANCELLATION/REINSTATEMENT STATE FILE
      *
     C     XCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXJRST
     C                   KFLD                    KKEFDT
      *
     C     XCKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXJRST
      *
      *  Cancellation reason
      *
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CXRST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CXRST
     C                   KFLD                    KKKEY
      *
     C*
     C*  KEY LIST FOR THE CANCELLATION HISTORY FILE
     C*
     C     XHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCREQ#
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   QUOTE FILE
     C*
     C     QTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXAGT#
      *
     C     QTKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      ***** BEG ADD ******* 06/21/12 **********************************
      *
     C     key1WTGTL1    Klist
     C                   Kfld                    CXco#
     C                   Kfld                    strRrCd
      ***** END ADD ******* 06/21/12 **********************************
     C*
      ***** Beg Del ***** 06/21/12 ***************************************
     C*****              MOVE      'W'           XXSTAT            1            ACTIVE ENVIRONMENT
      ***** End Del ***** 06/21/12 ***************************************
      *
      *
      *
      *===============================================================
      *  PLIST
      *===============================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WDCXP
     C                   PARM                    @@RRN2            9 0
      *
      *
      *
      *===============================================================
      *  *LIKE
      *===============================================================
     C     *LIKE         DEFINE    CXAGT#        XXAGT#
     C     *LIKE         DEFINE    XCEFDT        KKEFDT
     C     *LIKE         DEFINE    CRKEY         KKKEY
     C     *LIKE         DEFINE    SCERR         SCOERR
     C     *LIKE         DEFINE    CXCFDT        X3DATE
     C     *LIKE         DEFINE    XCST          XXJRST
     C     *LIKE         DEFINE    CXFYR         S1FYR
     C     *LIKE         DEFINE    CXREQ#        S1REQ#
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CXCXDT        XXRNDT
     C     *LIKE         DEFINE    CXREDT        XXREDT
     C     *LIKE         DEFINE    CXCXDT        XXCXDT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    EMDIV         SAVDIV
     C     *LIKE         DEFINE    CXREQ#        SVREQ#
     C     *LIKE         DEFINE    CXCDAT        SVCDAT
     C     *LIKE         DEFINE    CXCXDT        SVCXDT
     C     *LIKE         DEFINE    CXREIN        SVREIN
     C*
     C*
     C*
      *******Beg Del********06/21/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********06/21/12**********************************
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  VALIDATE REINSTATEMENT DATE
     C*
     C                   Z-ADD     SCREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCREDT
     C*
     C*
     C*  REINSTATEMENT DATE MUST BE > OR = CANCEL DATE
     C*
     C                   Z-ADD     SCREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXREDT
     C*
      *  Get rate set code
      *
     C     ELKY          CHAIN     PEL01                              60
      *
      *  Get Rating State
      *
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVEL(P)  RSJRST        XXJRST
     C   61              MOVEL(P)  *BLANKS       XXJRST
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C     XCKY2         SETGT     PXC01
     C     XCKY3         READPE    PXC01                                  60
      *
     C***  BEGIN ADD  ***  06/21/12  **********************************
      /Free
       If *In90 = *Off And XCrrCd = 'Y' And
          scRrCd = *Blanks;
          *IN90 = *ON;
          *IN39 = *ON;
       Endif ;

       If scRrCd <> *Blanks And *In90 = *Off;
          // XCrrCd = 'Y'
          strRrCd = 'RECD';
          *In90 = *On;
          *In38 = *On;

          Setll  key1WTGTL1 WTgtl1;
          Reade  key1WTGTL1 WTgtl1;
          Dow  Not %Eof(WTgtl1);
             If scRrCd = GTkey;
                *In90 = *Off;
                *In38 = *Off;
                Leave;
             EndIf;                                   // IF  SCrrcd = GTKEY
          Reade  key1WTGTL1 WTgtl1;
          EndDo;                                      // DOW  NOT %EOF(WTGTL1)

       EndIf;                                         // IF SCrrcd <> *BLANKS AND *IN90 = *OFF

      /End-Free
      ***** END ADD ******* 06/21/12 **********************************
      *
      * CANCELLED IN ERROR MUST BE 'Y' OR 'N'
      *
     C  N90SCERR         IFNE      'Y'
     C     SCERR         ANDNE     'N'
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCERR IFNE 'Y'
     C*
     C*  GENERATE CERTIFICATE HOLDERS LETTERS MUST BE 'Y' OR 'N'
     C*
     C  N90
     CAN 41SCRCRT        IFNE      'Y'
     C     SCRCRT        ANDNE     'N'
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCCCRT<>Y/N
     C*
     C*  REVISION FLAG MUST BE 'Y' OR 'N'
     C*
     C  N90SCREV         IFNE      'Y'
     C     SCREV         ANDNE     'N'
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCREV<>Y/N
      *
      * ADDRESS TYPE MUST BE 'R' OR 'L'
      *
     C     SCTYPE        IFNE      *BLANK
     C  N90SCTYPE        IFNE      'R'
     C     SCTYPE        ANDNE     'L'
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCTYPE IFNE 'R'
     C                   ENDIF                                                  SCTYPE<>*BLANK
      *
     C     *IN90         IFEQ      *OFF
      *
      *  If a 'Y' is entered in 'CX in error' field, move zero CX count.
      *
     C     SCERR         IFEQ      'Y'
     C                   Z-ADD     *ZERO         SCCNC#
     C                   ENDIF                                                  SCERR
      *
      *  Get rate set code
      *
     C     ELKY          CHAIN     PEL01                              60
      *
      *  Get Rating State
      *
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVEL(P)  RSJRST        XXJRST
     C   61              MOVEL(P)  *BLANKS       XXJRST
      *
      *  If a 'N' is entered in 'CX in error' field and field previously
      *  contained a 'Y', then recalculate cancellation count.
      *
     C     SCERR         IFEQ      'N'
     C     SCOERR        ANDEQ     'Y'
     C                   EXSR      SRCNC#
     C                   ENDIF                                                  SCERR
      *
      *  if CX number greater than limit and not CX in error, disallow
      *  reinstatement.
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C     XCKY2         SETGT     PXC01
     C     XCKY3         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             XCCXLM
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C     XCCXLM        IFNE      *ZERO
     C     SCCNC#        ANDGE     XCCXLM
     C     SCERR         ANDNE     'Y'
     C                   SETON                                        90  71
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90
      *
     C*
     C* Check if confirmation date exists
     C  N90CXCFDT        IFNE      0
     C                   EXSR      SRCONF
     C                   ENDIF
     C*
     C* Check for other divs eligible for reinstatement
     C  N90              EXSR      SROTHD
      *
      *  Display window if renewal year exists that was
      *  processed as part of automatic flat cx of renewal year.
      *
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C     SCREV         ANDNE     'Y'
     C     ELKY3         SETGT     PEL03
     C     ELKY4         READE     PEL03                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CXKY2         SETGT     PCX01
     C     CXKY2         READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     CXAFLT        ANDEQ     'Y'
     C                   EXFMT     WDW05
     C   KL              SETON                                        90
     C   KL              SETOFF                                       KL
     C                   LEAVE
     C                   ENDIF                                                  *IN90
      *
     C     ELKY4         READE     PEL03                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  *IN90
      *
      *  Display prompt window if renewal year exists that was NOT
      *  processed as part of automatic flat cx of renewal year.
      *
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C     ELKY3         SETGT     PEL03
     C     ELKY4         READE     PEL03                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CXKY2         SETGT     PCX01
     C     CXKY2         READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     CXAFLT        ANDNE     'Y'
     C                   Z-ADD     ELEMP#        S5EMP#
     C                   Z-ADD     ELDIV         S5DIV
     C                   Z-ADD     ELFYR         S5FYR
     C     EMKY1         CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAME        S5ENAM
     C   60              MOVEL(P)  *BLANKS       S5ENAM
     C                   MOVE      'N'           S5ANSW
      *
     C     S5ANSW        DOUEQ     'Y'
     C     S5ANSW        OREQ      'N'
     C     *INKL         OREQ      *ON
     C                   EXFMT     WDW06
     C                   SETOFF                                       71
      *
     C                   SELECT
      *
     C     S5ANSW        WHENEQ    'N'
     C                   MOVE      *ON           *IN90
     C                   LEAVE
      *
     C     S5ANSW        WHENEQ    'Y'
     C     X@RRN         CHAIN     DBRCDF                             99
     C                   MOVE      'Y'           CXAFLT
      *
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   Z-ADD     DSTIME        CXTIME
     C                   UPDATE    DBRCDF
     C                   LEAVE
      *
     C                   OTHER
     C                   SETON                                        71
      *
     C                   ENDSL
     C                   ENDDO
      *
     C   KL              SETON                                        90
     C   KL              SETOFF                                       KL
     C                   ENDIF                                                  CXKY2
      *
     C     ELKY4         READE     PEL03                                  60
     C                   ENDDO                                                  ELKY4
     C***  BEGIN ADD  ***  06/21/12  **********************************
      /Free
       If *In90 = *Off And (scRrCd = '02' Or scRrCd = '03');

          Exfmt  WDW07;

          If *InKL = *On;
             *In90 = *On;
             *InKl = *Off;
          EndIf;

       EndIf;

      /End-Free
      ***** END ADD ******* 06/21/12 **********************************
      *
     C                   ENDIF                                                  *IN90
      *
      *  Reset
      *
     C     CXKY1         CHAIN     PCX01                              60
     C     EMKY          CHAIN     PEM01                              60
     C     ELKY          CHAIN     PEL01                              60
      *
     C*
     C* Check to be sure record has not been updated since entry
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90CXDT          IFGT      SAVEDT
     C     CXDT          OREQ      SAVEDT
     C     CXTIME        ANDGT     SAVETM
     C                   SETON                                        86  90
     C                   ENDIF                                                  AUDT IFGT SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
      *** Beg Add ***** 06/21/12 **************************************
      ****************************************************************
      *  ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04                *
      ****************************************************************
      /Free
       BegSr  Errck4;
       Endsr;
      /End-Free
      ****************************************************************
      *  ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR03                *
      ****************************************************************
      /Free
       BegSr  Errck5;
       Endsr;
      /End-Free
      *** End Add ***** 06/21/12 **************************************
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C*** BEGIN ADD *** 06/21/12 **************************************
      /Free

       Select ;

       // REINSTATEMENT REASON CODE
       When HFLD = 'SCRRCD';
          W2493R(SCRRCD : '2' : 'RECD' : @@Key : @@Rtcd);
          If @@KEY <> *Blanks;
             scRrCd = @@Key;
             *In31 = *On;
          Else ;                       // @@KEY <> *BLANKS
             *In30 = *On;
          EndIf;                      // @@KEY <> *BLANKS

       Other;

       EndSl;

      /End-Free
      *** End Add ***** 06/21/12 **************************************
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     CXCO#         XXCO#
     C                   Z-ADD     CXFND         SCFND
     C                   Z-ADD     CXFYR         SCFYR
     C                   Z-ADD     CXEMP#        SCEMP#
     C                   Z-ADD     CXDIV         SCDIV
     C*
     C*
     C*  BEGIN CALCULATION FOR DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 = OFF
     C*
     C*  END CALCULATION FOR DIVISION COUNT
     C*
     C*
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C*  CHECK FOR ADDITIONAL DIVISIONS
     C*
     C                   MOVE      *OFF          *IN27
     C*
     C     EMKYX         CHAIN     PEM01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     *IN27         ANDEQ     *OFF
     C*
     C     SCDIV         IFNE      EMDIV
     C                   MOVE      *ON           *IN27
     C                   ENDIF                                                  SCDIV<>EMDIV
     C*
     C     EMKYX         READE     PEM01                                  60
     C*
     C                   ENDDO                                                  *IN60/27=*OFF
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET AGENT RECORDS
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  REINSTATEMENT INFORMATION
     C*
     C                   MOVE      CXREQ#        SCREQ#
     C*
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C*
     C                   Z-ADD     SCCXDT        SCREDT
     C*
     C                   MOVE      CXREIN        SCREIN
     C                   MOVE      'N'           SCRCRT
     C*
     C     CXRDAT        IFEQ      *ZERO
     C                   MOVE      'N'           SCREV
     C                   MOVE      'N'           SCERR
     C                   MOVE      *ON           *IN45
     C                   ELSE
     C                   MOVE      'Y'           SCREV
     C                   MOVE      CXERR         SCERR
     C                   MOVE      *OFF          *IN45
     C                   ENDIF                                                  CXRDAT
      *
     C                   MOVE      SCERR         SCOERR
     C                   Z-ADD     CXCNC#        SCCNC#
     C*
     C                   MOVEL(P)  CXRCM1        SCRCM1
     C                   MOVEL(P)  CXRCM2        SCRCM2
      ***** BEG ADD ******* 06/21/12 **********************************
      /Free
       scRrCd = CXreCd;  // Moving Reinstatement Code to Screen Field
      /End-Free
      ***** END ADD ******* 06/21/12 **********************************
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXREDT
     C*
     C                   MOVE      SCRCRT        CXRCRT
     C                   MOVE      *BLANK        CXRDRT
     C                   MOVE      'Y'           CXREIN
     C                   MOVE      SCERR         CXERR
     C*
     C                   MOVEL(P)  SCRCM1        CXRCM1
     C                   MOVEL(P)  SCRCM2        CXRCM2
     C                   Z-ADD     0             CXCFDT
     C                   Z-ADD     SCAGT#        CXAGT#
      *** BEG ADD ***** 06/21/12 **********************************
      /Free
       CXreCd = scRrCd ;  // Moving Reinstatement Code to Database
      /End-Free
      *** END ADD ***** 06/21/12 **********************************
     C*
      *
      * Renewal Year Processing
      *
     C     XXRNEW        IFEQ      'Y'
     C                   MOVE      CXCXDT        CXREDT
     C                   MOVE      'N'           CXRCRT
     C                   ELSE                                                   XXRNEW
     C                   Z-ADD     SCCNC#        CXCNC#
     C                   ENDIF                                                  XXRNEW
      *
     C                   ENDSR                                                  MVSCDB
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C***  BEGIN CHG **** 06/21/12 *********************************************
     C***  *INKJ         CASEQ     *ON           KJDO
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C***  END   CHG **** 06/21/12 *********************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C*
     C                   EXSR      PGMENX
      *
      * Process Renewal years
      *
     C                   EXSR      SRRNEW
      *
     C*
     C*    Process other divs if they exist
     C     *IN87         IFEQ      *ON
     C                   EXSR      SROTHW
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
     C*===============================================================
     C* PGMENX - Processing to be done for each reinstated emp/div
     C*===============================================================
     C     PGMENX        BEGSR
     C*
     C*  Only do if not in inquiry and enter was pressed
     C     *IN09         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C*  Find out if this is a reprint or not
     C*   -reprint=N if revise flag=Y or original reinstatement
     C     SCREV         IFEQ      'Y'
     C     CXREDT        ORNE      *ZERO
     C                   MOVE      'N'           QQREPR
     C                   ENDIF                                                  SCREV=Y
     C*
     C*  Write a record to the cancellation history file if this is
     C*  the initial entry or if this is a revision
     C     QQREPR        IFEQ      'N'
     C*
     C     XHKY          CHAIN     PXH01                              60
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHDT
     C                   Z-ADD     DSTIME        XHTIME
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     SCREV         IFEQ      'Y'
     C                   MOVE      'R'           XHRVFL
     C                   ELSE
     C                   MOVE      *BLANK        XHRVFL
     C                   ENDIF                                                  SCREV=Y
     C*
     C                   MOVEL     CXRSN         XHRSN
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C                   MOVE      CXERR         XHERR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHTRDT
     C*
     C                   Z-ADD     CXCNC#        XHCNC#
      ***** BEG ADD ******* 06/21/12 **********************************
      /Free
       // Moving Reinstatement Code to Cancellation History File.
        XHreCd = CXreCd;
      /End-Free
      ***** END ADD ******* 06/21/12 **********************************
     C                   WRITE     PXH01
     C*
     C                   ENDIF                                                   *IN60=*OFF
     C*
     C                   ENDIF                                                   QQREPR=N
     C*
      *
      *  Write an 'R' type Quote Status record.
      *
      *  Get current agent
      *
     C                   CALL      'WA002R'                             99
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM      *ZERO         #1FYR             3 0
     C     XXAGT#        PARM      *ZERO         #1AGT#            5 0
     C                   PARM      *BLANK        #1ANAM           60
      *
     C     QTKY1         SETGT     PQT01
     C     QTKY1         READPE    PQT01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     QTTYPE        IFEQ      'C'                                          CANCELLATION
     C     QTTYPE        OREQ      'T'                                          TRANSFER
     C     QTXFER        ANDEQ     'C'                                          XFER/CX
      *
     C                   Z-ADD     XXAGT#        QTAGT#
     C                   MOVE      'R'           QTTYPE
     C                   MOVE      *BLANKS       QTXFER
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     DSDATE        XXWKDT            8 0
     C*
     C                   Z-ADD     XXWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTWKDT
     C*
     C                   Z-ADD     QTWKDT        QTDT
     C                   Z-ADD     DSTIME        QTTIME
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   WRITE     PQT01
     C*
     C                   ENDIF                                                  QTTYPE=C
     C*
     C                   END                                                    QTKY
     C*
     C*  Submit reinstatement print program
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCREQ#        QQREQ#            5
     C                   MOVE      *BLANKS       QQOUTQ           10
     C                   MOVE      SCTYPE        QQTYPE            1
     C*
     C*  Get user outq
     C     Q1USER        CHAIN     PUP01                              60
     C  N60              MOVEL     UPOUTQ        QQOUTQ
     C*
     C                   CALL      'W7167C'                             99
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQREQ#
     C                   PARM                    SCRCRT
     C                   PARM                    QQREPR
     C                   PARM                    QQOUTQ
     C                   PARM                    QQTYPE
     C*
     C     XXRNEW        IFNE      'Y'
     C   87              CALL      'WA007R'                             99
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN09,KC,KL=*OFF
     C*
     C                   ENDSR                                                  PGMENX
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C                   MOVE      'N'           XXRNEW
      ***** Beg ADD ***** 06/21/12 ***************************************
     C                   MOVE      'W'           XXSTAT            1            ACTIVE ENVIRONMENT
      ***** End ADD ***** 06/21/12 ***************************************
     C*
     C*
     C*
     C*  DEFINE DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
     C*
     C                   Z-ADD     @@RRN         X@RRN
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*
     C*  FLAG IF CLIENT IS ENROLLED IN OV/400
     C*
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     SYOFIF        COMP      'Y'                                    41
     C*
     C                   MOVE      'N'           SCRCRT
     C*
     C*  SET REPRINT FLAG TO 'Y'
     C*
     C                   MOVE      'Y'           QQREPR            1
     C*
     C*  DISPLAY THE REINSTATEMENT FLAG
     C*
     C                   MOVE      *OFF          *IN45
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
      *
      *
      *===============================================================
      * SRCNC# - RE-Calculate cancellation count
      *===============================================================
     C     SRCNC#        BEGSR
      *
      *  Get Cancellation limit
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C     XCKY2         SETGT     PXC01
     C     XCKY3         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             XCCXMN
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
      *  Get number of months back to look for CX
      *
     C                   Z-ADD     *ZERO         X3DATE
      *
     C     XCCXMN        IFNE      *ZERO
     C                   Z-ADD     XCCXMN        ##MTHS
      *
     C                   CALL      'SRPMTH8'
     C                   PARM      DSDATE        ##DATE            8 0
     C                   PARM                    ##MTHS            3 0
     C                   PARM      *OFF          ##ERR             1
      *
     C     ##ERR         IFEQ      *OFF
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X3DATE
     C                   ENDIF
      *
     C                   ENDIF                                                  XCCXMN
      *
      *  Count CX
      *
     C                   Z-ADD     *ZERO         XX#CX            11 0
      *
      *
      *   IF CALLED FOR RENEWAL YEAR, SAVE OFF CURRENT YEAR VALUES
      *
     C     *LIKE         DEFINE    SCCNC#        SVCNC#
     C     XXRNEW        IFEQ      'Y'
     C                   Z-ADD     SCCNC#        SVCNC#
     C                   ENDIF
      *
     C     CXKY3         SETLL     PCX02
     C     CXKY4         READE     PCX02                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     CXERR         IFNE      'Y'
     C                   EXSR      SRNCCI
      *
     C     CRNCCI        IFEQ      '05'                                         NON-PAY
     C                   ADD       1             XX#CX
     C                   ENDIF                                                  CRNCCI
      *
     C                   ENDIF                                                  CXERR
      *
     C     CXKY4         READE     PCX02                                  60
     C                   ENDDO
      *
     C                   Z-ADD     XX#CX         SCCNC#
      *
     C     XXRNEW        IFEQ      'Y'
     C                   Z-ADD     SCCNC#        CXCNC#
     C                   Z-ADD     SVCNC#        SCCNC#
     C                   ELSE
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C                   EXSR      SRNCCI
      *
     C     CRNCCI        IFEQ      '05'                                         NON-PAY
     C                   ADD       1             SCCNC#
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  SRCNC#
      *
      *
      *===============================================================
      * SRNCCI - GET NCCI CX REASON CODE
      *===============================================================
     C     SRNCCI        BEGSR
      *
     C                   CLEAR                   CRNCCI
     C                   MOVEL(P)  CXRSN         KKKEY
     C     CRKY          SETGT     PCR01
      *
     C     *IN60         DOUEQ     *ON
     C     CRSTAT        ORNE      'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
     C                   ENDSR                                                  SRNCCI
      *
      *
     C*===============================================================
     C* SROTHD - Checks for other divs to reinstate
     C*===============================================================
     C     SROTHD        BEGSR
     C*
     C                   Z-ADD     SCDIV         SAVDIV
     C                   Z-ADD     CXREQ#        SVREQ#
     C                   Z-ADD     CXCDAT        SVCDAT
     C                   Z-ADD     CXCXDT        SVCXDT
     C                   MOVE      CXREIN        SVREIN
     C*
     C                   SETOFF                                       87
     C*
     C*   Cycle through all divisions
     C     EMKYX         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     *IN87         ANDEQ     *OFF
     C*
     C     EMDIV         IFNE      SAVDIV
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*     Set indicator if another div is found for cancellation
     C                   EXSR      SRGETX
     C     CD(X)         IFEQ      SVCXDT
     C     CD(X)         OREQ      0
     C     SCREV         ANDEQ     'Y'
     C*
     C                   Z-ADD     *HIVAL        SCREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CXREQ#        SCREQ#
     C*
     C     SCREIN        IFEQ      CXREIN
     C     SCREIN        ORNE      'Y'
     C     CXREIN        ANDNE     'Y'
     C     CXCDAT        IFEQ      SVCDAT
     C     CXCXDT        ANDEQ     SVCXDT
     C                   SETON                                        87
     C                   ENDIF                                                  CXCDAT=SVCDAT
     C                   ENDIF                                                  SCREIN=CXREIN
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CD,X=*ZERO
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EMDIV<>SAVDIV
     C*
     C     EMKYX         READE     PEM01                                  60
     C                   ENDDO
     C*
     C*   If there are multiple divs for cancellation then output msg
     C     *IN87         IFEQ      *ON
     C                   EXFMT     WDW03
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN87=*ON
     C*
     C*  Reset emp master and canx detail files
     C                   Z-ADD     SAVDIV        SCDIV
     C                   Z-ADD     SVREQ#        SCREQ#
     C     EMKY          CHAIN     PEM01                              60
     C     CXKY1         CHAIN     PCX01                              60
     C*
     C*  Reset emp detail file
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   ENDSR                                                  SROTHD
     C*===============================================================
     C* SRCONF - Checks if confirmation date is valid
     C*===============================================================
     C     SRCONF        BEGSR
     C*
     C                   Z-ADD     CXCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCFDT
     C*
     C                   EXFMT     WDW04
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                            90
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRCONF
     C*===============================================================
     C* SROTHW - Writes/Updates reinstatement records for other divs
     C*===============================================================
     C     SROTHW        BEGSR
     C*
     C                   SETOFF                                       87
     C                   Z-ADD     SCDIV         SAVDIV
     C*
     C*   Cycle through all divisions
     C     EMKYX         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMDIV         IFNE      SAVDIV
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*     Get appropriate effective and cancel dates from emp master
     C                   EXSR      SRGETX
     C     CD(X)         IFEQ      SVCXDT
     C     CD(X)         OREQ      0
     C     SCREV         ANDEQ     'Y'
     C*
     C                   Z-ADD     *HIVAL        SCREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CXREQ#        SCREQ#
     C*
     C     SCREIN        IFEQ      'Y'
     C     CXREIN        ANDEQ     'Y'
     C*
     C     CXCDAT        IFEQ      SVCDAT
     C     CXCXDT        ANDEQ     SVCXDT
     C*
     C                   CALL      AGTAPI        AGTPL
     C                   EXSR      UPDATR
     C                   EXSR      PGMENX
      *
      * Process Renewal years
      *
     C                   EXSR      SRRNEW
      *
     C*
     C                   ENDIF                                                  CXCDAT=SVCDAT
     C                   ENDIF                                                  @@MODE='C'
     C*
     C     SCREIN        IFNE      'Y'
     C     CXREIN        ANDNE     'Y'
     C*
     C     CXCDAT        IFEQ      SVCDAT
     C     CXCXDT        ANDEQ     SVCXDT
     C*
     C                   CALL      AGTAPI        AGTPL
     C                   EXSR      UPDATR
     C                   EXSR      PGMENX
      *
      * Process Renewal years
      *
     C                   EXSR      SRRNEW
      *
     C*
     C                   ENDIF                                                  CXCDAT=SVCDAT
     C                   ENDIF                                                  @@MODE='A'
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  CD,X=SVCXDT
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EMDIV<>SAVDIV
     C*
     C     EMKYX         READE     PEM01                                  60
     C                   ENDDO
     C*
     C*  Reset emp master and canx detail files
     C                   Z-ADD     SAVDIV        SCDIV
     C                   Z-ADD     SVREQ#        SCREQ#
     C     EMKY          CHAIN     PEM01                              60
     C     CXKY1         CHAIN     PCX01                              60
     C*
     C*  Reset emp detail file
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   ENDSR                                                  SROTHW
     C*===============================================================
     C* SRGETX - Gets appropriate emp master eff and canx dates
     C*===============================================================
     C     SRGETX        BEGSR
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  70
     C*
     C     ED(X)         IFNE      *ZERO
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  ED,X<>*ZERO
     C*
     C     X             DOWGT     *ZERO
     C     ED(X)         IFGT      ELPFDT
     C                   SUB       1             X
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  ED,X>ELPFDT
     C                   ENDDO                                                  X=*ZERO
     C*
     C*  Determine if cancellation date exists, set canx indicator
     C     X             IFEQ      *ZERO
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C     CD(X)         IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CD,X=*ZEROS
     C                   ENDIF                                                  X=*ZERO
     C*
     C                   ENDSR                                                  SRGETX
      *
      *
      *
      *===============================================================
      * SRRNEW - Process Renewal Years
      *===============================================================
     C     SRRNEW        BEGSR
     C                   MOVE      'Y'           XXRNEW            1
      *
      *  If not Reprint
      *
     C     QQREPR        IFNE      'Y'
     C     S5ANSW        ANDNE     'N'
     C                   Z-ADD     SCFYR         S1FYR
     C                   Z-ADD     SCREQ#        S1REQ#
      *
      *  For each renewal policy year for employer division
      *
     C     ELKY5         SETGT     PEL04
     C     ELKY6         READE     PEL04                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELFYR         SCFYR
      *
      *    If not Reinstated, process reinstatement of renewal year
      *
     C                   Z-ADD     *HIVAL        SCREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *OFF                                         CANCELLED BUT
     C     CXREIN        ANDNE     'Y'                                          NOT REINSTATED
     C                   MOVE      CXREQ#        SCREQ#
     C                   CALL      AGTAPI        AGTPL
     C                   EXSR      SRCNC#
     C                   EXSR      UPDATR
     C                   EXSR      PGMENX
     C                   ENDIF                                                  CXKY
      *
     C     ELKY6         READE     PEL04                                  60
     C                   ENDDO                                                  *IN90
      *
      * Reset
      *
     C                   Z-ADD     S1FYR         SCFYR
     C                   Z-ADD     S1REQ#        SCREQ#
     C     ELKY          CHAIN     PEL01                              60
     C                   CALL      AGTAPI        AGTPL
     C     CXKY1         CHAIN     PCX01                              60
     C                   ENDIF
      *
     C                   MOVE      'N'           XXRNEW            1
     C                   ENDSR                                                  SRRNEW
      *
      *
      *
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     X@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXRDAT
     C*
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   Z-ADD     DSTIME        CXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     XXCO#         CXCO#
     C                   Z-ADD     SCFND         CXFND
     C                   Z-ADD     SCEMP#        CXEMP#
     C                   Z-ADD     SCDIV         CXDIV
     C                   Z-ADD     SCFYR         CXFYR
     C                   Z-ADD     SCREQ#        CXREQ#
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CXIUSR
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXIPGM
     C                   MOVEL     Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXIDT
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXITM
     C                   MOVEL     DSTIME        CXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDCXP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
