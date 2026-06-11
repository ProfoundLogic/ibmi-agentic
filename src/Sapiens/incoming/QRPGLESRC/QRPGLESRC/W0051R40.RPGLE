     H Copyright('(c) 2021 Sapiens. All Rights Reserved.')
     H
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R40 - Rescission of DELETION OF DIVISION ENDORSEMENT
     H*****************************************************************
     H* DESCRIPTION:  This program reverses a Deletion of Division    *
     H*                Midterm Endorsement.                           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*    W0051R1 - WORK WITH MIDTERM ENDORSEMENTS                   *
     H*                                                               *
     H* CALLS TO:   NONE                                              *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
      * 06/29/22 C:340 Support: WC2854                                                             *
      *                Invoice Cloud:  Retain Payment Ref#                                         *
      *                                                                                            *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -A/R records restored with holding records      *
     H*                 written on the same day will delete holding   *
     H*                 records.                                      *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records with new posting dates.       *
     H*                                                               *
     H* 04/07/09 C:220 SUPPORT: 66684                                 *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP             *
     H*                                                               *
     H* 02/05/07 C:180 SUPPORT WA129 53118                            *
     H*                Write WAQTP record for most current agent.     *
     H*
     H* 10/06/06 C:61  SUPPORT WA136  54913                           *
     H*                POPULATE NEW XARDIV FIELD IN WMXAP.            *
     H*                                                               *
     H* 08/17/05 C:61  PROJECT WC1266                                 *
     H*                Created.                                       *
     H*                                                               *
     H*****************************************************************
      *
     FW0051F40  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
      *
      *  Employer Detail
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELP     IF   E             DISK
      *
      *  Group Auxiliary
     FWDF2L     IF   E           K DISK
      *
      *  Endorsement/DART Control Definition
     FWMDXL     IF   E           K DISK
      *
      *  Endorsement
     FWMENP     O    E             DISK
      *
      *  Group Master
     FWMFNL     IF   E           K DISK
      *
      *  Mid-Year Endorsement Table
     FWTMYP     IF   E             DISK
      *
      *  Cancellation Detail File
     FWDCXL     UF   E           K DISK
      *
      *  Cancellation History File
     FWDXHL     IF A E           K DISK
      *
      *  Quote Tracking
     FWAQTL7    IF A E           K DISK
      *
      *  Employer Accounts Receivable
     FWMEAL14   UF A E           K DISK
      *
      *  Accounts Receivable Deleted
     FWMXAL     UF   E           K DISK
     F***  Begin Add  ***  11/19/09  **************************************
     FWMA#L1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     F***  End   Add  ***  11/19/09  **************************************
      *
      *  ADMIN/DARTS Control File
     FWMEXL     IF   E           K DISK
      *
      *
      *
      *=========================================================================
      *  Array & Table definitions
      *=========================================================================
      *
      *
      *=========================================================================
      *  Data Structure and Constant Definitions
      *=========================================================================
      *
     D***PY SCOPYMT01I
      *
      /COPY SCOPYMT01D
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      *
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANC DATES
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
      *
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
     D OPTNM           C                   CONST('RECISSION OF DIVISIO-
     D                                     N DELETION')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
      *
      *
      *=========================================================================
      *  PLIST definitions
      *=========================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          WMEMP
     C                   PARM                    @@RRN1            9 0          WDELP
     C                   PARM                    @@RRN2            9 0          WTMYP
     C                   PARM                    @@ENDT            8 0          ENDORSE DATE
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM      ##CO#         #1CO#
     C                   PARM      XXFND         #1FND
     C                   PARM      XXEMP#        #1EMP#
     C                   PARM      XXDIV         #1DIV
     C                   PARM      *ZERO         #1FYR             3 0
     C                   PARM                    #1AGT#            5 0
     C                   PARM                    #1ANAM           60
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
      *
      *
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      *  Cancellation A/R detail file
      *
     C     XAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
      *  CANCELLATION HISTORY FILE
      *
     C     XHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXREQ#
      *
      * Quote Tracking
      *
     C     QTKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    KKSTAT            1
      ***** BEG CHG ******* 02/05/07 **********************************
     C******               KFLD           CXAGT#
     C                   KFLD                    KKAGT#
      ***** END CHG ******* 02/05/07 **********************************
      *
      * Endorsement Request Workfile
      *
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *
      * Group Auxiliary
      *
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * Endors/DARTS Control Definition
      *
     C     DXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * Group
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * Cancellation Detail
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
      *
      * Employer Accounts Receivable
      *
     C     EAKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
     C                   KFLD                    XXARCD
     C                   KFLD                    XXPODT
      *
      * Admin/DARTS Control File
      *
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *
      *=========================================================================
      *  *LIKE DEFN statements
      *=========================================================================
     C     *LIKE         DEFINE    EAARCD        XXARCD
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EAPODT        ZZPODT
     C     *LIKE         DEFINE    ##CO#         #1CO#
     C     *LIKE         DEFINE    XXFND         #1FND
     C     *LIKE         DEFINE    XXEMP#        #1EMP#
     C     *LIKE         DEFINE    XXDIV         #1DIV
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
     C     *LIKE         DEFINE    EMNAME        ##NAME
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
      ***** BEG ADD ******* 02/05/07 **********************************
     C     *LIKE         DEFINE    QTAGT#        KKAGT#
      ***** END ADD ******* 02/05/07 **********************************
      *
      *
      *=========================================================================
      *  Main
      *=========================================================================
      *
     C/COPY SCOPYMT01C
      *
      *=========================================================================
      * DLTPRE - Pre-processing before delete occurs
      *=========================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *=========================================================================
      * DSPLYR - Processing after displaying information
      *=========================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *=========================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *=========================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
      *
      *
      *=========================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *=========================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *=========================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *=========================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
     C***  Begin Add  ***  11/19/09  **************************************
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    XACO#         #ICO#
     C     *LIKE         DEFINE    XAFND         #IFND
     C     *LIKE         DEFINE    XAEMP#        #IEMP#
     C     *LIKE         DEFINE    XADIV         #IDIV
     C     *LIKE         DEFINE    XAFYR         #IFYR
     C     *LIKE         DEFINE    XAPODT        #IDT
     C     *LIKE         DEFINE    XANINV        #IINV#
     C*
     C     CLTID         IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      XACO#         #ICO#
     C                   PARM      XAFND         #IFND
     C                   PARM      XAEMP#        #IEMP#
     C                   PARM      XADIV         #IDIV
     C                   PARM      XAFYR         #IFYR
     C                   PARM      XAPODT        #IDT
     C     XANINV        PARM                    #IINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Delete from A/R holding file when holding record was written today
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C     A#KEY         KLIST
     C                   KFLD                    XACO#
     C                   KFLD                    XAFND
     C                   KFLD                    XAEMP#
     C                   KFLD                    XADIV
     C                   KFLD                    XAFYR
     C                   KFLD                    SYSDT
     C*
     C     A#KEY         CHAIN     PEA0#                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     XATRDT        IFEQ      EATRDT
     C     XAPODT        ANDEQ     EAPODT
     C     XAAMT         ANDEQ     EAAMT
     C     XANINV        ANDEQ     EANINV
     C                   DELETE    PEA0#
     C                   LEAVE
     C                   ENDIF
     C*
     C     A#KEY         READE     PEA0#                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C***  End   Add  ***  11/19/09  **************************************
      *
      *=========================================================================
      * MVDBSC - Move data base fields to screen fields
      *=========================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      *BLANKS       SCRSCR                         REASON
     C                   ENDSR                                                  MVDBSC
      *
      *
      *=========================================================================
      * MVSCDB - Move screen fields to data base fields
      *=========================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR                                                  MVSCDB
      *
      *
      *=========================================================================
      * OTHERR - Processing for other modes
      *=========================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *=========================================================================
      * OTHFKS - Processing for other functions keys
      *=========================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *=========================================================================
      * SRRNEW - Process Renewal Years
      *=========================================================================
     C     SRRNEW        BEGSR
      *
     C                   MOVE      'Y'           XXRNEW            1
     C                   Z-ADD     XXFYR         SVFYR
     C     ELKY          SETGT     PEL02
     C     ELKY1         READE     PEL02                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELFYR         XXFYR
      *
      *  Update Cancellation Detail
      *
     C                   EXSR      UPDCX
      *
     C     CXAFLT        IFEQ      'Y'
      *
      *  Write CX History record
      *
     C                   EXSR      WRTXH
      *
      *  Restore A/R Billing records from WMXAP
      *
     C                   EXSR      RSTAR
      *
     C                   ENDIF                                                  CXAFLT
      *
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  ELKY
      *
     C                   Z-ADD     SVFYR         XXFYR
     C                   MOVE      'N'           XXRNEW            1
      *
     C                   ENDSR                                                  SRRNEW
      *
      *
      *=========================================================================
      * PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *=========================================================================
      * PGMSTR - Start of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
     C                   MOVE      'N'           XXRNEW            1
     C                   TIME                    DSDTTM
     C***  Begin Add  ***  11/19/09  **************************************
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  11/19/09  **************************************
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C                   MOVE      'C'           @@MODE            1
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
      *  Get Employer Detail record
      *
     C     @@RRN1        CHAIN     PEL01                              60
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELFYR         XXFYR
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C     @@RRN         CHAIN     WMEMP                              99
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
      *
      *  Compress EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME
     C                   PARM                    ##EDN            80
      *
      *  Get the most recent cancel date, if any
      *
     C                   MOVEL     ##EDN         SCEDN
     C                   MOVE      *OFF          *IN41                          NOT CANCELLED
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      *
      *  Move endorsement effective date to screen
      *
     C                   Z-ADD     @@ENDT        SCENED
      *
      *  Get Group information
      *
     C     FNKY          CHAIN     PFN01                              60
     C     F2KY          CHAIN     PF201                              60
     C                   MOVEL     FNNAME        SCNAME
      *
      *  Get Agent/Producer information
      *
     C                   CALL      AGTAPI        AGTPL
     C                   MOVEL     #1ANAM        SCANAM
     C                   MOVEL     #1PNAM        SCPNAM
     C                   MOVE      *OFF          *IN42
      *
     C     SCANAM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN42
     C                   ENDIF
      *
      *  Get Endors/DARTS information
      *
     C     DXKY          CHAIN     PDX01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX01
     C                   ENDIF
      *
     C     EXKY          CHAIN     PEX01                              60
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *=========================================================================
      * PREOTH - Pre-processing for other modes
      *=========================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
      *
      *
      *=========================================================================
      * UPDATR - Update data base record and after processing
      *=========================================================================
     C     UPDATR        BEGSR
     C                   EXSR      MVSCDB
     C     @@RRN         CHAIN     DBRCDF                             99
      *
      *  Get the cancel date that corresponds to the most recent
      *  effective date.
      *
     C                   Z-ADD     1             I1                1 0
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
      *  Clear the cancel date
      *
     C                   CLEAR                   CX(I1)
     C                   ENDIF                                                  I1 IFNE 0
      *
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMTIME
      *
     C                   UPDATE    DBRCDF
      *
      *  Write Endorsement and Reinstatement Records
      *
     C                   EXSR      WRTPEN
      *
      *  Process renewal years
      *
     C                   EXSR      SRRNEW
     C                   ENDSR                                                  UPDATR
      *
      *
      *=========================================================================
      * WRITER - Write data base record and after processing
      *=========================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WMEMP
     C                   ENDSR                                                  WRITER
      *
      *
      *=========================================================================
      * WRTPEN - Write data base record to Endorsement Print Workfile
      *=========================================================================
     C     WRTPEN        BEGSR
      *
     C                   CLEAR                   PEN01
     C                   Z-ADD     *ZERO         ENSEQ#
     C                   MOVE      XXCO#         ENCO#
     C                   MOVE      XXFND         ENFND
     C                   MOVE      XXEMP#        ENEMP#
     C                   MOVE      XXDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
      *
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
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
     C     ELKY          CHAIN     PEL02                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60
      *
     C                   MOVE      'Y'           ENRDDF
     C                   MOVEL(P)  DXDVDL        ENRMCD
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      #1AGT#        ENAGT#
     C                   MOVEL(P)  #1ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C     @@RRN2        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
      *
      *  CHANGED EN**1 FIELDS TO EN**2 FIELDS
      *
     C                   MOVEL     *BLANKS       ENNAM2
     C                   MOVEL     *BLANKS       ENFDN2
     C                   MOVE      *BLANKS       ENANM1
     C                   MOVE      *BLANKS       ENANM2
     C                   MOVE      *BLANKS       ENAFN1
     C                   MOVE      *BLANKS       ENAFN2
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
     C                   Z-ADD     *ZEROS        ENCAR2
     C                   Z-ADD     *ZEROS        ENPFD2
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      *
      *  Build up Before/After lines & write WMENP records.
      *
     C     EMPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
      *
     C                   ELSE                                                   LOAD MAIL ADDR
      *
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
      *
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       ENBLIN
      *
      *  Employer name
      *
     C                   MOVEL     EMNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  Employer federal name
      *
     C                   MOVEL     EMFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  Employer address 1
      *
     C                   MOVEL     XXADR1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  Employer Address 2
      *
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *
      *  Employer city/ST/ZIP
      *
     C                   MOVEL     XXADR3        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      * Reason for Rescission
      *
     C                   MOVEL     SCRSCR        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
      *  Update Cancellation Detail
      *
     C                   EXSR      UPDCX
      *
      *  Write CX History record
      *
     C                   EXSR      WRTXH
      *
      *  Update QUOTE Tracking
      *
     C                   EXSR      UPDQT
      *
      *  Restore A/R Billing records from WMXAP
      *
     C                   EXSR      RSTAR
      *
     C                   ENDSR                                                  WRTPEN
      *
      *
      *=========================================================================
      * WRTXH - Write CX History Record
      *=========================================================================
     C     WRTXH         BEGSR
      *
      *  Load WDXHP fields
      *
     C     XHKY          CHAIN     PXH01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANK        XHRVFL
     C                   MOVEL     CXRSN         XHRSN
     C                   Z-ADD     CXCXDT        XHCXDT
     C                   Z-ADD     CXCDAT        XHCDAT
     C                   Z-ADD     CXREDT        XHREDT
     C                   Z-ADD     CXRDAT        XHRDAT
     C                   MOVE      CXERR         XHERR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHTRDT
      *
     C                   Z-ADD     CXCNC#        XHCNC#
      *
      ****
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XHDT
     C                   Z-ADD     DSTIME        XHTIME
      *
     C                   WRITE     PXH01
     C                   ENDIF                                                   XHKY
      *
     C                   ENDSR                                                  WRTXH
      *
      *
      *=========================================================================
      * UPDCX - Update CX Detail Record
      *=========================================================================
     C     UPDCX         BEGSR
      *
     C                   MOVE      *HIVAL        XXREQ#
     C     CXKY1         SETGT     PCX01
     C     CXKY          READPE    PCX01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     XXRNEW        IFEQ      'N'
     C     XXRNEW        OREQ      'Y'
     C     CXAFLT        ANDEQ     'Y'
     C     CXREIN        ANDNE     'Y'                                          NOT REINSTATED
      *
      *  Load WDCXP fields
      *
     C                   Z-ADD     CXCXDT        CXREDT
     C                   MOVE      *BLANK        CXRCRT
     C                   MOVE      *BLANKS       CXRDRT
     C                   MOVE      'Y'           CXREIN
     C                   MOVE      'N'           CXERR
     C                   Z-ADD     0             CXCFDT
     C                   Z-ADD     #1AGT#        CXAGT#
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXRDAT
      *
     C                   MOVE      Q1USER        CXUSER
     C                   MOVE      Q1PGM         CXPGM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
      *
     C                   Z-ADD     DSTIME        CXTIME
      *
     C                   UPDATE    PCX01
     C                   ENDIF                                                  XXRNEW
      *
     C                   ENDIF                                                  CXKY
      *
     C                   ENDSR                                                  UPDCX
      *
      *
      *=========================================================================
      * UPDQT - Update QUOTE Tracking Record
      *=========================================================================
     C     UPDQT         BEGSR
      *
     C                   MOVE      'W'           KKSTAT
      ***** BEG ADD ******* 02/05/07 **********************************
     C                   MOVE      #1AGT#        KKAGT#
      ***** END ADD ******* 02/05/07 **********************************
     C     QTKY          SETGT     PQT01
     C     QTKY          READPE    PQT01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     QTTYPE        IFEQ      'C'                                          CANCELLATION
     C     QTTYPE        OREQ      'T'                                          TRANSFER
     C     QTXFER        ANDEQ     'C'                                          XFER/CX
     C                   MOVE      'R'           QTTYPE
     C                   MOVE      *BLANKS       QTXFER
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTWKDT
     C*
     C                   Z-ADD     QTWKDT        QTDT
     C                   Z-ADD     DSTIME        QTTIME
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   WRITE     PQT01
     C                   ENDIF                                                  QTTYPE
      *
     C                   ENDIF                                                  QTKY
      *
     C                   ENDSR                                                  UPDQT
      *
      *
      *=========================================================================
      * RSTAR - Restore A/R from WMAXP
      *=========================================================================
     C     RSTAR         BEGSR
      *
      *    Transfer any previously deleted
      *    a/r records to WMEAP determining new posting dates if necessary.
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
      *
      *      Read cancellation a/r detail file
      *
     C     XAKY          CHAIN     PXA01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  Get ZZPODT Date
      *
     C                   EXSR      GTZZDT
      *
      *      If posting date is greater than calc date then write record as
      *      is, otherwise write/update record with calc date as posting date
      *
     C     XAPODT        IFGT      ZZPODT
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   EXSR      WRTREC
     C                   DELETE    PXA01
      *
     C                   ELSE
      *
     C                   MOVE      XAARCD        XXARCD
     C                   Z-ADD     ZZPODT        XXPODT
     C     EAKY          CHAIN     PEA01                              61
      *
     C     *IN61         IFEQ      *OFF
     C                   ADD       XAAMT         EAAMT
     C                   MOVE(P)   Q1PGM         EAPGM
     C                   Z-ADD     SYSDT         EATRDT
     C                   UPDATE    PEA01
     C                   DELETE    PXA01
      *
     C                   ELSE
      *
     C                   Z-ADD     ZZPODT        XAPODT
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      GETINV
     C***  End   Add  ***  11/19/09  **************************************
     C                   EXSR      WRTREC
     C                   DELETE    PXA01
     C                   ENDIF                                                  *IN61=*OFF
      *
     C                   ENDIF                                                  XAPODT>ZZPODT
      *
     C     XAKY          READE     PXA01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  RSTAR
      *
      *
      *=========================================================================
      * GTZZDT - Get ZZPODT
      *=========================================================================
     C     GTZZDT        BEGSR
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     XXCO#         #2CO#
     C                   Z-ADD     XXFND         #2FND
     C                   Z-ADD     XXEMP#        #2EMP#
     C                   Z-ADD     XXDIV         #2DIV
     C                   Z-ADD     XXFYR         #2FYR
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #2DATE
      *
     C                   MOVEL     '*'           #2PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    #2CO#             3 0
     C                   PARM                    #2FND             3 0
     C                   PARM                    #2EMP#            9 0
     C                   PARM                    #2DIV             5 0
     C                   PARM                    #2FYR             3 0
     C                   PARM                    #2AGT#            5 0
     C                   PARM                    #2ANAM           60
     C                   PARM                    #2PROD            5 0
     C                   PARM                    #2PNAM           60
     C                   PARM                    #2AREA            3 0
     C                   PARM                    #2PHON            7 0
     C                   PARM                    #2PLAN            1
     C                   PARM                    #2DATE            8 0
      *
     C     #2PLAN        IFEQ      'A'
     C     EXKY          CHAIN     PEX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EXAOID        XXDAYS            5 0
      *
      * POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
      *
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        #3DT              8 0
     C                   PARM      XXDAYS        #3DAYS            5 0
     C                   PARM                    #3ERR             1
     C                   PARM                    #3TERM            1
      *
     C     #3ERR         IFNE      '1'
     C                   MOVE      #3DT          XXACDT            8 0
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  @@PLAN IFEQ
      *
      *  Determine post date for a/r (revised interim days + system dt)
      *
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        #3DT              8 0
     C                   PARM      EXRVID        #3DAYS            5 0
     C                   PARM                    #3ERR             1
     C                   PARM                    #3TERM            1
      *
     C     #3ERR         IFNE      '1'
     C                   Z-ADD     #3DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF
      *
     C     XXACDT        IFNE      *ZERO
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZPODT
     C                   ENDIF                                                  XXACDT IFNE
      *
     C                   ENDSR                                                  GTZZDT
      *
      *
      *================================================================
      *  WRTREC - writes record to a/r file
      *================================================================
     C     WRTREC        BEGSR
     C*
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XACO#         EACO#
     C                   MOVE      XAFND         EAFND
     C                   MOVE      XAFYR         EAFYR
     C                   MOVE      XAEMP#        EAEMP#
     C                   MOVE      XADIV         EADIV
      ******* BEG ADD **** 10/06/06 ***********************************
     C                   Z-ADD     XARDIV        EARDIV
      ******* END ADD **** 10/06/06 ***********************************
     C                   MOVE      XAARCD        EAARCD
     C                   MOVE      XAARTY        EAARTY
     C                   MOVE      XADESC        EADESC
     C                   MOVE      XAAMT         EAAMT
     C                   MOVE      XAPODT        EAPODT
     C                   MOVE      XAEDIT        EAEDIT
     C                   MOVE      XASTAT        EASTAT
     C                   MOVE      XABUL1        EABUL1
     C                   MOVE      XACHK#        EACHK#
     C                   MOVE      XANINV        EANINV
     C                   MOVE      XALATE        EALATE
     C                   MOVE      XAFL01        EAFL01
     C                   MOVE      XAFL02        EAFL02
     C                   MOVE      XADT01        EADT01
     C                   MOVE      XADT02        EADT02
     C                   MOVE      XACTYP        EACTYP
      ******* BEG ADD **** 04/07/09 ***********************************
     C                   MOVE      XACCTP        EACCTP
     C                   MOVE      XACNUM        EACNUM
     C                   MOVE      XACEXP        EACEXP
     C                   MOVE      XACNAM        EACNAM
      ******* END ADD **** 04/07/09 ***********************************
      *
      *    Update these fields with reinstatement info
      *
     C                   Z-ADD     SYSDT         EATRDT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   MOVE      DSTIME        EATIME
     C                   Z-ADD     SYSDT         EADT
      *
          //*** Beg Add ***** 06/29/22 ***************************
     C                   MOVE      XAref#        EAref#
          //*** Beg Add ***** 06/29/22 ***************************
     C                   WRITE     PEA01
     C                   ENDSR                                                  WRTREC
      *
      *
