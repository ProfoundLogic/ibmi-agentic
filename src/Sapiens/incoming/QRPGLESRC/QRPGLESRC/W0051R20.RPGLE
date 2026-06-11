     H Copyright('(c) 2021 Sapiens. All Rights Reserved.')
     H
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R20 - DELETION OF DIVISION ENDORSEMENT
     H*****************************************************************
     H* DESCRIPTION:  This program processes a Deletion of Division   *
     H*                Midterm Endorsement.                           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*    W0051R1 - WORK WITH MIDTERM ENDORSEMENTS                   *
     H*                                                               *
     H* CALLS TO:   NONE                                              *
     H*                                                               *
     H* CREATED:    05/27/99                                          *
     H* PROGRAMMER: C:113                                             *
     H* PROJECT:    CM027                                             *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
      * 06/29/22 C:340 Support: WC2854                                                             *
      *                Invoice Cloud:  Retain Payment Ref#                                         *
      *                                                                                            *
     H* 06/01/09 C:189 SUPPORT: 68663                                 *
     H*                6TH & 7TH ACCESSEMENST ARE NOT BEING HANDELED  *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*                                                               *
     H* 04/07/09 C:220 SUPPORT: 66684                                 *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP             *
     H*                                                               *
     H* 09/20/06 C:65  PROJECT: Seq# 54128/54913/WC1288               *
     H*                POPULATE NEW XARDIV FIELD IN WMXAP.            *
     H*
     H* 07/21/06 C:61  SUPPORT WA129 53118                            *
     H*                Write WAQTP record for most current agent.     *
     H*                                                               *
     H* 05/31/06 C:65  PROJECT: Seq# 52949                            *
     H*                When deleting division, also delete future A/R *
     H*                FOR 4TH AND 5TH ASSESSMENTS.                   *
     H*                                                               *
     H* 09/21/05 C:136 SUPPORT: WA091 46505                           *
     H*                ENSURE THAT SRDBIL DOESN'T DELETE A/R IN THE   *
     H*                PAST                                           *
     H*                                                               *
     H* 06/24/05 C:61  SUPPORT WA135 47842                            *
     H*                Do not use user parameter value ##FND for CHAIN*
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: Seq# 45714                            *
     H*                When deleting division, also delete future A/R *
     H*                FOR INSTALLMENT FEES.                          *
      *
      * 08/03/04 C:61  PROJECT WC966
      *                Add renewal year processing.
     H*                                                               *
     H* 03/28/02 C:90  PROJECT: Seq# 31442                            *
     H*                When deleting division, also delete future A/R *
     H*                                                               *
     H* 01/30/02 C:90  PROJECT: SEQ# 29367                            *
     H*                Update Quote Tracking when performing a        *
     H*                deletion of division.                          *
     H*                                                               *
     H* 01/09/02 C:90  PROJECT: SEQ# 28493                            *
     H*                Update the "Summarize A/R" flag to blanks when *
     H*                deleting a division.                           *
     H*                                                               *
     H* 10/23/00 C:90  PROJECT: SEQ# 23934                            *
     H*                When deleting a division, write records to     *
     H*                to CECFDT, CXCFFL,AND CXCFPD.                  *
     H*                                                               *
     H* 10/20/99 C:87  PROJECT: WC577                                 *
     H*                Assign initial maintenance fields when         *
     H*                writing to WDCXP.                              *
     H*                                                               *
     H* 08/10/99 C:113 PROJECT: CM046                                 *
     H*                When Deletion of division endorsement is       *
     H*                processed, create a Cancellation Detail record *
     H*                (WDCXP), and a Cancellation History record     *
     H*                (WDXHP).                                       *
     H*                                                               *
     H* 07/16/99 C:09 PROJECT: CM027                                  *
     H*               NEED TO BE LOADING THE EN**1 FIELDS WITH DATA   *
     H*               SINCE THESE ARE USED IN W7418R TO REPORT TO     *
     H*               NCCI THE ENDORSEMENT INFORMATION.               *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (       )                                               *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*****************************************************************
     FW0051F20  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
      **** BEGIN CHG **** 01/09/02 ************************************
     F****WDELL   IF  E           K        DISK
     FWDELL     UF   E           K DISK
      **** END   CHG **** 01/09/02 ************************************
     F                                     RENAME(PEL01:PEL02)
     FWDELP     IF   E             DISK
     FWDF2L     IF   E           K DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     FWMFNL     IF   E           K DISK
     FWTMYP     IF   E             DISK
     F*
     F*** BEGIN ADD *** 08/10/99 ***********************************************
     F** Cancellation Detail File
     FWDCXP     O    E             DISK
     F                                     RENAME(PCX01:PCX02)
     FWDCXL     IF   E           K DISK
     F** Cancellation History File
     FWDXHP     O    E             DISK
     F*** END   ADD *** 08/10/99 ***********************************************
      **** BEGIN ADD **** 01/30/02 ************************************
     FWAQTL7    IF A E           K DISK
      **** END   ADD **** 01/30/02 ************************************
      **** BEGIN ADD **** 03/28/02 ************************************
     F*  Employer Accounts Receivable
     FWMEAL6    UF   E           K DISK
     F***  Begin Add  ***  11/19/09  **************************************
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     F***  End   Add  ***  11/19/09  **************************************
     F*
     F*  Transaction Type Code Table  (WTTTP)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
     F*
     F* Audit Tracking Control File
     FWMXAP     O    E             DISK
     F*
     F* ADMIN/DARTS Control File
     FWMEXL     IF   E           K DISK
      **** END   ADD **** 03/28/02 ************************************
      **** BEGIN ADD **** 03/07/05 ************************************
     FWMIFL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      **** END   ADD **** 03/07/05 ************************************
     F*
      /COPY SCOPYMT01D
     D*=========================================================================
     D*  Array & Table definitions
     D*=========================================================================
     D*=========================================================================
     D*  Data Structure and Constant Definitions
     D*=========================================================================
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANC DATES
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*=========================================================================
     D*  /COPY required constants
     D*=========================================================================
     D OPTNM           C                   CONST('DELETION OF DIVISION')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)

     C*=========================================================================
     C*  PLIST definitions
     C*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0          ENDORSE DATE
     C*
     C*  Company file/fund (group) retrieve program parameter list
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
      ***** BEG CHG ******* 06/24/05 **********************************
     C*********            PARM           ##FND
     C                   PARM                    XXFND
      ***** END CHG ******* 06/24/05 **********************************
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*=========================================================================
     C*  KLIST definitions
     C*=========================================================================
      **** BEGIN ADD **** 01/30/02 ************************************
     C*
     C     QTKY          KLIST
     C                   KFLD                    ##CO#
      ***** BEG CHG ******* 06/24/05 **********************************
     C*********            KFLD           ##FND
     C                   KFLD                    XXFND
      ***** END CHG ******* 06/24/05 **********************************
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKSTAT            1
      ***** BEG ADD ******* 07/21/06 **********************************
     C                   KFLD                    KKAGT#
      ***** END ADD ******* 07/21/06 **********************************
     C*
      **** END   ADD **** 01/30/02 ************************************
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*
     C* KEY LIST FOR THE GROUP AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      ***** BEG ADD ***** 08/03/04 ************************************
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      ***** END ADD ***** 08/03/04 ************************************
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN ADD *** 08/10/99 ***********************************************
     C*
     C*  Cancellation Detail file
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
      **** BEGIN ADD **** 03/28/02 ************************************
     C*  Employer Accounts Receivable
     C*
     C     EAKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C                   KFLD                    XXKDAT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C*
     C*  Transaction Type Code Table
     C     TTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*  ADMIN/DARTS Control File
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** END   ADD **** 03/28/02 ************************************
      **** BEGIN ADD **** 03/07/05 ************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  INSTALLMENT FEE FILE
     C*
     C     IFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXEFDT            7 0
     C*
     C     IFKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
      **** END   ADD **** 03/07/05 ************************************
     C*=========================================================================
     C*  *LIKE DEFN statements
     C*=========================================================================
      ***** BEG ADD ******* 07/21/06 **********************************
     C     *LIKE         DEFINE    QTAGT#        KKAGT#
      ***** END ADD ******* 07/21/06 **********************************
      ***** BEG ADD ***** 08/03/04 ************************************
     C     *LIKE         DEFINE    ELFYR         SVFYR
      ***** END ADD ***** 08/03/04 ************************************
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         XSRDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*** BEGIN ADD *** 08/10/99 ***********************************************
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C*** END   ADD *** 08/10/99 ***********************************************
      **** BEGIN ADD **** 03/28/02 ************************************
     C     *LIKE         DEFINE    TTCO#         KKCO#
     C     *LIKE         DEFINE    TTKEY         KKKEY
     C     *LIKE         DEFINE    CXCXDT        XXKDAT
      **** END   ADD **** 03/28/02 ************************************
     C*=========================================================================
     C*  Main
     C*=========================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*=========================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*=========================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*=========================================================================
     C* DSPLYR - Processing after displaying information
     C*=========================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*=========================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*=========================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*=========================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*=========================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*=========================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*=========================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     EATRDT        IFLT      SYSDT
     C*
     C                   ADD       1             SYSTM
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1USER        EAUSER
     C                   Z-ADD     SYSDT         EADT
     C                   Z-ADD     SYSTM         EATIME
     C*
     C                   WRITE     PEA0#
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  11/19/09  **************************************
     C*=========================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*=========================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      *BLANKS       SCDELR
     C                   ENDSR
     C*=========================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*=========================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*=========================================================================
     C* OTHERR - Processing for other modes
     C*=========================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *
      *
      *
     C*=========================================================================
     C* OTHFKS - Processing for other functions keys
     C*=========================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
      ***** BEG ADD ***** 08/03/04 ************************************
      *
      *
      *
      *=========================================================================
      * SRRNEW - Process Renewal Years
      *=========================================================================
     C     SRRNEW        BEGSR
      *
     C                   Z-ADD     XXFYR         SVFYR
     C     ELKY          SETGT     PEL02
     C     ELKY1         READE     PEL02                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *    Get Agent info
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM                    #1AGT#            5 0
     C                   PARM                    #1ANAM           60
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
      *
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT            7 0
      *
      *  Write Cancellation Detail and History records
      *
      *  Get the next Request#
      *
     C                   MOVE      *HIVAL        XXREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
     C  N60CXREQ#        ADD       1             XXREQ#
     C   60              Z-ADD     1             XXREQ#
      *
      *  Load WDCXP fields
      *
     C                   MOVE      ELCO#         CXCO#
     C                   MOVE      ELFND         CXFND
     C                   MOVE      ELEMP#        CXEMP#
     C                   MOVE      ELDIV         CXDIV
     C                   MOVE      ELFYR         CXFYR
     C                   MOVE      XXREQ#        CXREQ#
     C                   Z-ADD     ELPFDT        CXCXDT
     C                   Z-ADD     XXSYDT        CXCDAT
     C                   Z-ADD     #1AGT#        CXAGT#
     C                   MOVE      *BLANKS       CXRSN
     C                   MOVE      *BLANKS       CXCOM1
     C                   MOVE      *BLANKS       CXCOM2
     C                   MOVE      *ZEROS        CXAMTD
     C                   MOVE      'N'           CXCCRT
     C                   MOVE      *BLANKS       CXCDRT
     C                   MOVE      'N'           CXREIN
     C                   MOVE      'N'           CXRCRT
     C                   MOVE      *ZEROS        CXREDT
     C                   MOVE      *ZEROS        CXRDAT
     C                   MOVE      *BLANKS       CXRDRT
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      *BLANK        CXREVL
     C                   MOVE      'Y'           CXCFFL
     C                   Z-ADD     XXSYDT        CXCFDT
     C                   MOVE      *BLANK        CXCEMN
     C                   MOVE      *ZEROS        CXDOI#
     C                   MOVE      *ZEROS        CXCNC#
     C                   MOVE      *BLANK        CXERR
     C                   MOVE      *ZEROS        CXRFEE
     C                   MOVE      *BLANKS       CXRFDS
     C                   MOVE      *BLANKS       CXCAP#
     C                   MOVE      *ZEROS        CXRPMY
     C                   Z-ADD     XXSYDT        CXCFPD
     C                   MOVE      *BLANKS       CXCARR
     C                   MOVE      'Y'           CXENFL
     C*
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
      *
     C                   MOVEL     DSTIME        CXTIME
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
      *
     C                   MOVE      Q1USER        CXIUSR
     C                   MOVE      Q1PGM         CXIPGM
     C                   Z-ADD     DSTIME        CXITM
     C                   Z-ADD     CXDT          CXIDT
     C                   MOVE      'Y'           CXAFLT
      *
      *  Load WDXHP fields
      *
     C                   MOVE      ELCO#         XHCO#
     C                   MOVE      ELFND         XHFND
     C                   MOVE      ELEMP#        XHEMP#
     C                   MOVE      ELDIV         XHDIV
     C                   MOVE      ELFYR         XHFYR
     C                   MOVE      XXREQ#        XHREQ#
     C                   Z-ADD     ELPFDT        XHCXDT
     C                   Z-ADD     XXSYDT        XHTRDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   MOVE      *BLANKS       XHRSN
     C                   MOVE      *ZEROS        XHREDT
     C                   MOVE      *ZEROS        XHRDAT
     C                   MOVE      *ZEROS        XHCNC#
     C                   MOVE      *BLANK        XHERR
     C                   MOVE      'Y'           XHENFL
     C                   Z-ADD     XXSYDT        XHDT
     C                   MOVEL     SRDTO         XHCDAT
      *
     C                   MOVEL     DSTIME        XHTIME
     C                   MOVE      *BLANK        XHSTAT
     C                   MOVEL     Q1USER        XHUSER
     C                   MOVEL     Q1PGM         XHPGM
      *
      *  Write WDCXP and WDXHP records
      *
     C                   WRITE     PCX02
     C                   WRITE     PXH01
     C                   MOVE      *BLANKS       ELARSM
     C                   UPDATE    PEL02
      *
      *  Delete billings which are posted on or after the effective
      *  date of cancellation (CXCXDT) if admin/DARTS flag = 'Y'
      *
     C     EXDLAR        IFEQ      'Y'
     C                   EXSR      SRDBIL
     C                   ENDIF
      *
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  ELKY
      *
     C                   ENDSR                                                  SRRNEW
      *
      ***** END ADD ***** 08/03/04 ************************************
      *
      *
     C*=========================================================================
     C* PGMEND - End of program processing
     C*=========================================================================
     C     PGMEND        BEGSR
      ***** BEG DEL ***** 08/03/04 ************************************
      *****BEGIN ADD **** 03/28/02 ************************************
     C***Delete billings which are posted on or after the effective
     C***date*of cancellation (CXCXDT) if admin/DARTS flag = 'Y'
     C*********
     C*********  EXDLAR    IFEQ 'Y'
     C*********            EXSR SRDBIL
     C*********            ENDIF
      *****END*  ADD **** 03/28/02 ************************************
      ***** END DEL ***** 08/03/04 ************************************
     C                   ENDSR
      *
      *
      *
     C*=========================================================================
     C* PGMSTR - Start of program processing
     C*=========================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C***  Begin Add  ***  11/19/09  **************************************
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C***  End   Add  ***  11/19/09  **************************************
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C                   MOVE      *ON           FIRST             1
     C*
     C                   MOVE      'C'           @@MODE            1
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  Get Employer Detail record
     C*
     C     @@RRN1        CHAIN     PEL01                              60
     C*
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELFYR         XXFYR
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C*
     C     @@RRN         CHAIN     WMEMP                              99
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C*
     C*  Compress EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C*  Get the most recent cancel date, if any
     C*
     C                   MOVEL     XXEDN         SCEDN
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1                1 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C* Move endorsement effective date to screen
     C*
     C                   Z-ADD     @@ENDT        SCENED
     C*
     C*  Get Group information
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  Get Agent/Producer information
     C*
     C                   Z-ADD     *ZEROS        ##FYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVE      *OFF          *IN42
     C*
     C     SCANAM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C*  Get Endors/DARTS information
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX01
     C                   ENDIF
     C*
      **** BEGIN ADD **** 03/28/02 ************************************
     C     EXKY          CHAIN     PEX01                              60
      **** END   ADD **** 03/28/02 ************************************
     C                   ENDSR
      *
      *
     C*=========================================================================
     C* PREOTH - Pre-processing for other modes
     C*=========================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
      **** BEGIN ADD **** 03/28/02 ************************************
     C*================================================================
     C*  SRDBIL - For billings posted on or after cancellation date:
     C*           write record to WMXAP and then delete from WMEAP
     C*================================================================
     C     SRDBIL        BEGSR
     C*
     C*** BEGIN ADD ***  03/07/05  ************************************
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   MOVE      ELPFDT        XXEFDT
     C  N60IFKY          SETGT     PIF01
     C  N60IFKY1         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   ENDIF
     C*** END ADD *****  03/07/05  ************************************
     C     CXCXDT        IFNE      *ZERO
     C     CXCXDT        IFLT      XXDATE
     C                   Z-ADD     XXDATE        XXKDAT
     C                   ELSE
     C                   Z-ADD     CXCXDT        XXKDAT
     C                   ENDIF                                                  CXCXDT IFLT XXDATE
     C*
     C     EAKY1         SETLL     PEA01
     C     EAKY2         READE     PEA01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAPODT        IFGE      CXCXDT
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'H'
     C*           Write record then delete
     C                   EXSR      SRWRIT
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA01
     C                   ENDIF                                                  EAARTY
     C*** BEGIN ADD ***  03/07/05  ************************************
     C     EAARCD        IFEQ      IFIFBL
     C*           Write record then delete
     C                   EXSR      SRWRIT
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA01
     C                   ENDIF                                                  EAARCD IFEQ
     C*** END ADD *****  03/07/05  ************************************
     C*
     C     EAARTY        IFEQ      'F'
     C                   MOVE      EACO#         KKCO#
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C*
     C  N60TTCTYP        IFEQ      'T'
     C     TTCTYP        OREQ      'M'
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C*** BEGIN ADD ***  05/31/06  ********************************************
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C*** END ADD *****  05/31/06  ********************************************
     C*** BEGIN ADD ***  06/01/09  ********************************************
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C*** END ADD *****  06/01/09  ********************************************
     C*           Write record then delete
     C                   EXSR      SRWRIT
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA01
     C                   ENDIF                                                  TTCTYP
     C*
     C                   ENDIF                                                  EAARTY = F
     C*
     C                   ENDIF                                                  EAPODT
     C*
     C     EAKY2         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C                   ENDIF                                                  CXCXDT
     C*
     C                   ENDSR                                                  SRDBIL
     C*================================================================
     C*  SRWRIT - Write WMEAP record to WMAXP
     C*================================================================
     C     SRWRIT        BEGSR
     C*
     C                   MOVE      EACO#         XACO#
     C                   MOVE      EACO#         XACO#
     C                   MOVE      EAFND         XAFND
     C                   MOVE      EAFYR         XAFYR
     C                   MOVE      EAEMP#        XAEMP#
     C                   MOVE      EADIV         XADIV
     C*** BEGIN ADD ***  09/20/06  *********************************************
     C                   Z-ADD     EARDIV        XARDIV
     C*** END ADD *****  09/20/06  *********************************************
     C                   MOVE      EAARCD        XAARCD
     C                   MOVE      EAARTY        XAARTY
     C                   MOVE      EADESC        XADESC
     C                   MOVE      EAAMT         XAAMT
     C                   MOVE      EATRDT        XATRDT
     C                   MOVE      EAPODT        XAPODT
     C                   MOVE      EAEDIT        XAEDIT
     C                   MOVE      EASTAT        XASTAT
     C                   MOVE      EAUSER        XAUSER
     C                   MOVE      EAPGM         XAPGM
     C                   MOVE      EATIME        XATIME
     C                   MOVE      EADT          XADT
     C                   MOVE      EABUL1        XABUL1
     C                   MOVE      EACHK#        XACHK#
     C                   MOVE      EANINV        XANINV
     C                   MOVE      EALATE        XALATE
     C                   MOVE      EAFL01        XAFL01
     C                   MOVE      EAFL02        XAFL02
     C                   MOVE      EADT01        XADT01
     C                   MOVE      EADT02        XADT02
     C*** BEGIN ADD ***  04/07/09  *********************************************
     C                   MOVE      EACCTP        XACCTP
     C                   MOVE      EACNUM        XACNUM
     C                   MOVE      EACEXP        XACEXP
     C                   MOVE      EACNAM        XACNAM
     C*** END   ADD ***  04/07/09  *********************************************
     C*
          //*** Beg Add ***** 06/29/22 ***************************
     C                   MOVE      EAref#        XAref#
          //*** Beg Add ***** 06/29/22 ***************************
     C*              Maintenance fields
     C                   MOVE      Q1USER        XAIUSR
     C                   MOVE      Q1PGM         XAIPGM
     C                   MOVE      DSTIME        XAITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XAIDT
     C*
     C                   WRITE     PXA01
     C*
     C                   ENDSR                                                  SRWRIT
      *
      **** END   ADD **** 03/28/02 ************************************
     C*=========================================================================
     C* UPDATR - Update data base record and after processing
     C*=========================================================================
     C     UPDATR        BEGSR
     C*
     C                   EXSR      MVSCDB
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C*  Get the cancel date that corresponds to the most recent
     C*  effective date.
     C*
     C                   Z-ADD     1             I1                1 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C*  Set the cancel date to the endorsement effective date.
     C*
     C     CX(I1)        IFEQ      *ZERO
     C                   Z-ADD     @@ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CX(I1)
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      WRTPEN
     C*
      ***** BEG ADD ***** 08/03/04 ************************************
      *
      *  Process renewal years
      *
     C                   EXSR      SRRNEW
      ***** END ADD ***** 08/03/04 ************************************
      *
     C                   ENDSR
      *
      *
      *
     C*=========================================================================
     C* WRITER - Write data base record and after processing
     C*=========================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WMEMP
     C                   ENDSR
      *
      *
      *
     C*=========================================================================
     C* WRTPEN - Write data base record to Endorsement Print Workfile
     C*=========================================================================
     C     WRTPEN        BEGSR
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C*
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         ENCO#
     C                   MOVE      XXFND         ENFND
     C                   MOVE      XXEMP#        ENEMP#
     C                   MOVE      XXDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C*** BEGIN CHG ***  07/16/99  ************************************
     C*
     C*  CHANGED EN**2 FIELDS TO EN**1 FIELDS
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C                   MOVEL     EMCNTY        ENCNT1
     C                   MOVE      F2CAR#        ENCAR1
     C*
     C     ELKY          CHAIN     PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
     C*
     C     ELKY          READE     PEL02                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60
     C*** END CHG *****  07/16/99  ************************************
     C*
     C                   MOVE      'Y'           ENDLDF
     C                   MOVEL(P)  DXDVDL        ENRMCD
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     @@RRN2        CHAIN     PMY01                              60
     C*
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C*** BEGIN CHG ***  07/16/99  ************************************
     C*
     C*  CHANGED EN**1 FIELDS TO EN**2 FIELDS
     C*
     C                   MOVEL     *BLANKS       ENNAM2
     C                   MOVEL     *BLANKS       ENFDN2
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVE      *BLANKS       ENANM1
     C                   MOVE      *BLANKS       ENANM2
     C*
     C                   MOVE      *BLANKS       ENAFN1
     C                   MOVE      *BLANKS       ENAFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     *BLANKS       ENCON2
     C                   MOVEL     *BLANKS       ENATT2
     C                   MOVEL     *BLANKS       ENCN32
     C                   MOVE      *ZEROS        ENFEN2
     C                   MOVEL     *BLANKS       ENTOB2
     C                   MOVEL     *BLANKS       ENMA12
     C                   MOVEL     *BLANKS       ENMA22
     C                   MOVEL     *BLANKS       ENMCT2
     C                   MOVEL     *BLANKS       ENMST2
     C                   MOVEL     *BLANKS       ENMZP2
     C                   MOVEL     *BLANKS       ENPA12
     C                   MOVEL     *BLANKS       ENPA22
     C                   MOVEL     *BLANKS       ENPCT2
     C                   MOVEL     *BLANKS       ENPST2
     C                   MOVEL     *BLANKS       ENPZP2
     C                   MOVEL     *BLANKS       ENCNT2
     C*** BEGIN ADD ***  07/16/99  ************************************
     C                   Z-ADD     *ZEROS        ENCAR2
     C                   Z-ADD     *ZEROS        ENPFD2
     C*** END ADD *****  07/16/99  ************************************
     C*** END CHG *****  07/16/99  ************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C*  Build up Before/After lines & write WMENP records.
     C*
     C     EMPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C*
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C*
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C*
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C*
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   LOAD MAIL ADDR
     C*
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C*
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C*
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C*
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       ENBLIN
     C*
     C*  Employer name
     C*
     C                   MOVEL     EMNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C*  Employer federal name
     C*
     C                   MOVEL     EMFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C*  Employer address 1
     C*
     C                   MOVEL     XXADR1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C*  Employer Address 2
     C*
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C*  Employer city/ST/ZIP
     C*
     C                   MOVEL     XXADR3        ENALIN
     C*
     C                   WRITE     PEN01
     C*
     C                   ADD       1             ENSEQ#
     C*
     C* Reason for deletion
     C*
     C                   MOVEL     SCDELR        ENALIN
     C*
     C                   WRITE     PEN01
     C*
     C                   ADD       1             ENSEQ#
     C*** BEGIN ADD *** 08/10/99 ***********************************************
     C*
     C*  Write Cancellation Detail and History records
     C*
     C*  Get the next Request#
     C*
     C                   MOVE      *HIVAL        XXREQ#
     C*
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXREQ#        ADD       1             XXREQ#
     C                   ELSE
     C                   Z-ADD     1             XXREQ#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Load WDCXP fields
     C*
     C                   MOVE      XXCO#         CXCO#
     C                   MOVE      XXFND         CXFND
     C                   MOVE      XXEMP#        CXEMP#
     C                   MOVE      XXDIV         CXDIV
     C                   MOVE      LDFYR         CXFYR
     C                   MOVE      XXREQ#        CXREQ#
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCXDT
     C                   MOVE      *BLANKS       CXRSN
     C                   MOVE      *BLANKS       CXCOM1
     C                   MOVE      *BLANKS       CXCOM2
     C                   MOVE      *BLANK        CXCCRT
     C                   MOVE      *BLANKS       CXCDRT
     C                   MOVE      'N'           CXREIN
     C                   MOVE      *ZEROS        CXREDT
     C                   MOVE      *ZEROS        CXRDAT
     C                   MOVE      *BLANK        CXRCRT
     C                   MOVE      *BLANKS       CXRDRT
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVE      *BLANK        CXREVL
     C**** BEGIN CHG **** 10/23/00 ************************************
     C******************** MOVE *BLANK    CXCFFL
     C******************** MOVE *ZEROS    CXCFDT
     C                   MOVE      'Y'           CXCFFL
     C                   Z-ADD     @@ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCFDT
     C**** END   CHG **** 10/23/00 ************************************
     C                   MOVE      *BLANK        CXCEMN
     C                   MOVE      *ZEROS        CXDOI#
     C                   MOVE      *ZEROS        CXCNC#
     C                   MOVE      *BLANK        CXERR
     C                   MOVE      *ZEROS        CXAMTD
     C                   MOVE      *ZEROS        CXRFEE
     C                   MOVE      *BLANKS       CXRFDS
     C                   MOVE      *BLANKS       CXCAP#
     C                   MOVE      *ZEROS        CXRPMY
     C**** BEGIN CHG **** 10/23/00 ************************************
     C*******************  MOVE *ZEROS    CXCFPD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCFPD
     C**** END   CHG **** 10/23/00 ************************************
     C                   MOVE      *BLANKS       CXCARR
     C                   MOVE      'Y'           CXENFL
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     SRDTO         CXCDAT
      *** BEG ADD ***** 09/21/05 *****************************************
     C                   MOVEL     SRDTO         XXDATE
      *** END ADD ***** 09/21/05 *****************************************
     C*
     C                   MOVEL     DSTIME        CXTIME
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C*
     C***  BEGIN ADD  ***  10/20/99  **********************************
     C                   MOVE      Q1USER        CXIUSR
     C                   MOVE      Q1PGM         CXIPGM
     C                   Z-ADD     DSTIME        CXITM
     C                   Z-ADD     CXDT          CXIDT
     C***  END   ADD  ***  10/20/99  **********************************
     C*
     C*  Load WDXHP fields
     C*
     C                   MOVE      XXCO#         XHCO#
     C                   MOVE      XXFND         XHFND
     C                   MOVE      XXEMP#        XHEMP#
     C                   MOVE      XXDIV         XHDIV
     C                   MOVE      LDFYR         XHFYR
     C                   MOVE      XXREQ#        XHREQ#
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHCXDT
     C                   MOVE      *BLANKS       XHRSN
     C                   MOVE      *ZEROS        XHREDT
     C                   MOVE      *ZEROS        XHRDAT
     C                   MOVE      *ZEROS        XHCNC#
     C                   MOVE      *BLANK        XHERR
     C                   MOVE      'Y'           XHENFL
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHDT
     C                   MOVEL     SRDTO         XHCDAT
     C*
     C                   MOVEL     DSTIME        XHTIME
     C                   MOVEL     Q1USER        XHUSER
     C                   MOVEL     Q1PGM         XHPGM
      **** BEGIN ADD **** 01/30/02 ************************************
      *
     C                   MOVE      'W'           KKSTAT
      ***** BEG ADD ******* 07/21/06 **********************************
     C                   MOVE      ##AGT#        KKAGT#
      ***** END ADD ******* 07/21/06 **********************************
     C     QTKY          SETGT     PQT01
     C     QTKY          READPE    PQT01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   Z-ADD     XXDATE        QTWKDT
     C                   MOVE      'C'           QTTYPE
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      DSTIME        QTTIME
     C                   MOVE      Q1PGM         QTPGM
     C                   Z-ADD     XXDATE        QTDT
     C*
     C                   MOVE      QTUSER        QTIUSR
     C                   MOVE      QTTIME        QTITM
     C                   MOVE      QTPGM         QTIPGM
     C                   MOVE      QTDT          QTIDT
     C*
     C                   WRITE     PQT01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
      **** END   ADD **** 01/30/02 ************************************
     C*
     C*  Write WDCXP and WDXHP records
     C*
     C                   WRITE     PCX02
     C                   WRITE     PXH01
     C*** END   ADD *** 08/10/99 ***********************************************
      **** BEGIN ADD **** 01/09/02 ************************************
     C     ELKY          CHAIN     PEL02                              60
     C                   MOVE      *BLANKS       ELARSM
     C                   UPDATE    PEL02
      **** END   ADD **** 01/09/02 ************************************
      ***** BEG ADD ***** 08/03/04 ************************************
      *
     C*  Delete billings which are posted on or after the effective
     C*  date of cancellation (CXCXDT) if admin/DARTS flag = 'Y'
     C*
     C     EXDLAR        IFEQ      'Y'
     C                   EXSR      SRDBIL
     C                   ENDIF
      ***** END ADD ***** 08/03/04 ************************************
     C*
     C                   ENDSR
