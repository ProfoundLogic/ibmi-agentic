     H*  (C) Copyright Progressive Data Solutions, Inc., 2008
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H/TITLE W0074R02 - EDI Payments
     H*****************************************************************
     H* Description:  This pgm is used to enter EDI payment information
     H*   This entries will be associated with specific A49 Log records.
     H*                                                               *
     H* Called from:  W0074R                                          *
     H*                                                               *
     H* Created:  10/19/07      C:53      Project: WC1584
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*     06/09/20 C:318  Project: SY420
     H*                     Mask SSN
     H*
     H*     07/06/09 C:180 SUPPORT 68701 - Modify Payment reason code
     H*                                    edit (Use check #) so user
     H*                                    can link more than 1 payment.
     H*
     H*
     H*     03/04/09 C:53  Support: 67460  for 139WGE
     H*                    Change link for 139
     H*                                                               *
     H*     12/10/08 C:53  Project: WC1177
     H*                    Change option name
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0074F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDPTP     UF A E             DISK
     F                                     RENAME(PP001:DBRCDF)
      ***** BEG CHANGE ***** 07/06/09 *******************************
     F**WDPTL     IF   E           K DISK
     FWDPTL1    IF   E           K DISK
      ***** End CHANGE ***** 07/06/09 *******************************
     F*
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK    INFDS(CMLFDS)
     FWTGTL     IF   E           K DISK
     FWDPAP     IF   E             DISK
     FWDRPL3    IF   E           K DISK
     FWACPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F                                     IGNORE(PPA01)
     D CMLFDS          ds
     D  CMRRN                397    400B 0
     D*
     D                 DS
     D  SS#                    1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D*****************/COPY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEG CHG *** 12/10/08 *********************************
     D*OPTNM********** C                   CONST('EDI PTD/Reduced Ea-
     D****************                     rnings/Recoveries')
     D OPTNM           C                   CONST('EDI Link Payment')
     D*** END CHG *** 12/10/08 *********************************
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@SEQ#            5
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key list for the PTD/Reduced Earning/Recovery File
     C*
     C     P0KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C                   KFLD                    KKPRCD
      ***** BEG ADD *** 07/06/09 ***************************************
     C                   KFLD                    KKCHK#
      ***** END ADD *** 07/06/09 ***************************************
     C*
     C*  Key list for the Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Key list for the Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  Key list for the Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*
     C     CPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PATRDT
     C                   KFLD                    PAFND
     C*
     C**** BEG ADD *** 03/04/09 ***************************************
     C*
     C     CPKY1         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAPODT
     C                   KFLD                    PAFND
     C*
     C**** END ADD *** 03/04/09 ***************************************
     C*
     C     TBKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    P0CO#         KKCO#
     C     *LIKE         DEFINE    P0FND         KKFND
     C     *LIKE         DEFINE    P0FYR         KKFYR
     C     *LIKE         DEFINE    P0CASE        KKCASE
     C     *LIKE         DEFINE    P0SEQ#        KKSEQ#
     C     *LIKE         DEFINE    P0PRCD        KKPRCD
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    B2KEY         KKKEY
     C     *LIKE         DEFINE    P0STDT        XXSTDT
     C     *LIKE         DEFINE    P0ENDT        XXENDT
     ****** BEGIN ADD ******** 07/06/09 ***********************
     C     *LIKE         DEFINE    P0CHK#        KKCHK#
     ****** END ADD ********** 07/06/09 ***********************
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     Q1USER        P0USER
     C                   MOVEL     Q1PGM         P0PGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0DT
     C                   MOVEL     DSTIME        P0TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      CLEAR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Payment reason code cannot be blank.
     C*
     C     SCPRCD        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   LEAVESR
     C                   ENDIF
     C*
     C*  In 'add' mode, process code edits.
     C*
     C     *IN40         IFEQ      *ON
     C*  If the code has already been used, send an error.
     C*
     C                   MOVEL     SCPRCD        KKPRCD
     C*** BEGIN ADD *** 07/06/09 **************************************
     C                   MOVE      SCCHK#        KKCHK#
     C*** EBD   ADD *** 07/06/09 **************************************
     C     P0KY          CHAIN     PP001                              99
     C                   IF        *IN99 = *OFF
     C                   SETON                                        78  90
     C                   LEAVESR
     C                   ENDIF                                                  SCDODD<>0
     C*
     C                   ENDIF                                                  SCDODD<>0
     C*
     C*  Payee can not be blank.
     C*
     C     SCPAYE        IFEQ      *BLANKS
     C                   SETON                                        77  90
     C                   LEAVESR
     C                   ENDIF
     C*
     C*  Earnings amount can not be 0.
     C*
     C  N90SCAMT         IFEQ      0
     C                   SETON                                        72  90
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   MOVE      SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C                   IF        SRERR = *ON
     C                   SETON                                        7390
     C                   LEAVESR
     C                   ENDIF                                                  SCDODD<>0
     C*
     C                   MOVE      SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C                   IF        SRERR = *ON
     C                   SETON                                        7490
     C                   LEAVESR
     C                   ENDIF                                                  SCDODD<>0
     C*
     C                   MOVE      SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISDT
     C                   IF        SRERR = *ON
     C                   SETON                                        7590
     C                   LEAVESR
     C                   ENDIF                                                  SCDODD<>0
     C*
     C*** Ascending
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSTDT
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C                   IF        XXSTDT > XXENDT
     C                   SETON                                        7690
     C                   LEAVESR
     C                   ENDIF                                                  SCDODD<>0
     C*
     C*  All edits have been passed.
     C*
     C     ENDCK1        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - Process F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCPRCD'
     C*
     C                   MOVE      SCPRDS        QQPRDS           40
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C                   PARM      'PADJ'        KKTBID
     C                   PARM      *BLANKS       KKTKEY
     C                   PARM      ' '           QQRTN             1
     C*
     C                   SELECT
     C     QQRTN         WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTN         WHENEQ    'B'
     C                   MOVE      *ON           *INKL
     C     QQRTN         WHENEQ    '*'
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  KKTKEY        SCPRCD
     C  N99              MOVEL(P)  GTDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C                   MOVE      *ON           *IN89
     C                   ENDSL
     C*
     C     HFLD          WHENEQ    'SCPAYE'
     C*
     C                   CALL      'W0620R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      CMRRN         ##RRN             9 0
     C                   PARM      ' '           ##TYPE            1
     C*
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C*
     C     ##RRN         CHAIN     WDPAP
     C**** BEG ADD *** 07/06/09 ***************************************
     C                   EVAL      SCCHK# = *ZEROS
     C                   EVAL      SCCHK# = PACHK#
     C**** END ADD *** 07/06/09 ***************************************
     C* Retrieve Payee name
     C                   EVAL      SCPAYE = *BLANKS
     C                   EVAL      SCISDT = *ZEROS
     C     RPKY          CHAIN     PRP01                              60
     C                   IF        *IN60 = *OFF
     C                   EVAL      SCPAYE = RPPNAM
     C                   ENDIF                                                  *IN60 = *OFF
     C* Retrieve Payment Issue Date
     C**** BEG ADD *** 03/04/09 ***************************************
     C     CLTID         IFEQ      '139'
     C     CPKY1         CHAIN     PCP01                              60
     C                   ELSE                                                    *IN60=*OFF
     C**** END ADD *** 03/04/09 ***************************************
     C     CPKY          CHAIN     PCP01                              60
     C**** BEG ADD *** 03/04/09 ***************************************
     C                   ENDIF                                                   *IN60=*OFF
     C**** END ADD *** 03/04/09 ***************************************
     C*
     C                   IF        *IN60 = *OFF
     C                   Z-ADD     CPCMDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCISDT
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C* No Payment Issue Date use Transaction date
     C*
     C                   IF        *IN60 = *ON or CPCMDT = *ZEROS
     C**** BEG ADD *** 03/04/09 ***************************************
     C     CLTID         IFEQ      '139'
     C* No Payment Issue Date use Post date for client 139
     C                   Z-ADD     PAPODT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCISDT
     C                   ELSE                                                    *IN60=*OFF
     C**** END ADD *** 03/04/09 ***************************************
     C                   Z-ADD     PATRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCISDT
     C**** BEG ADD *** 03/04/09 ***************************************
     C                   ENDIF                                                   *IN60=*OFF
     C**** END ADD *** 03/04/09 ***************************************
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   EVAL      SCAMT  = PAAMT
     C                   Z-ADD     PAFRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCSTDT
     C                   Z-ADD     PATODT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCENDT
     C*
     C                   MOVEL(P)  PABR2         KKKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      '0'
     C     B2PYCD        ANDNE     *BLANKS
     C                   MOVEL     'PADJ'        KKTBID
     C                   MOVEL(P)  B2PYCD        SCPRCD
     C                   MOVEL(P)  B2PYCD        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL     GTDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C                   ENDIF
     C                   ENDIF
     C***
     C***  QQRTN         IFEQ      '*'
     C***                MOVEL     KKTKEY        SCPRCD
     C***  GTKY          CHAIN     PGT01                              99
     C**N99              MOVEL(P)  GTDESC        SCDESC
     C***                MOVE      *ON           *IN74
     C***                ENDIF
     C*
     C                   ENDSL
     C*
     C*** FORMAT DATES
     C                   IF        SCSTDT <> *ZEROS
     C                   MOVE      SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C                   ENDIF                                                  SCDODD<>0
     C*
     C                   IF        SCENDT <> *ZEROS
     C                   MOVE      SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C                   ENDIF                                                  SCDODD<>0
     C*
     C                   IF        SCISDT <> *ZEROS
     C                   MOVE      SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISDT
     C                   ENDIF                                                  SCDODD<>0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     P0PRCD        SCPRCD
     C                   MOVEL     P0PAYE        SCPAYE
     C                   MOVE      P0AMT         SCAMT
     C                   Z-ADD     P0STDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   Z-ADD     P0ENDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCENDT
     C*
     C                   Z-ADD     P0ISDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCISDT
     C*
     C                   MOVEL     'PADJ'        KKTBID
     C                   MOVEL(P)  SCPRCD        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL     GTDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C****** BEG ADD *** 07/06/09 ****************************************
     C                   MOVE      P0CHK#        SCCHK#
     C****** END ADD *** 07/06/09 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCPRCD
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *ZEROS        SCAMT
     C****** BEG ADD *** 07/06/09 ****************************************
     C                   MOVE      *ZEROS        SCCHK#
     C****** END ADD *** 07/06/09 ****************************************
     C*****              MOVE      'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     KKCO#         P0CO#
     C                   MOVE      KKFND         P0FND
     C                   MOVE      KKFYR         P0FYR
     C                   MOVE      KKCASE        P0CASE
     C                   MOVE      KKSEQ#        P0SEQ#
     C***** BEGIN ADD **** 07/06/09 ***************************************
     C                   MOVE      SCCHK#        P0CHK#
     C***** END ADD ****** 07/06/09 ***************************************
     C                   MOVEL     SCPRCD        P0PRCD
     C                   MOVEL     SCPAYE        P0PAYE
     C                   MOVE      SCAMT         P0AMT
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0STDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0ENDT
     C*
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0ISDT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@SEQ#        KKSEQ#
     C*
     C*  Load the screen headings.
     C*
     C                   Z-ADD     KKFND         SCFND
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL(P)  FNNAME        SCFNAM
     C*
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      CMSS#         SS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'SS#'         QQNAME           25
     C     SS#           PARM      SS#           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C                   CAT       SS2:0         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       SS3:0         SCSS#
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM
     C                   CAT       CMFSN:1       SCCNAM
     C                   CAT       CMMID:1       SCCNAM
     C*
     C                   MOVEL(P)  CMFYR         SCCS#
     C                   CAT       '-':0         SCCS#
     C                   MOVE      CMCASE        SCCS#
     C*
     C                   MOVEL(P)  CMACL#        SCSCL#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCACDT
     C*
     C                   ENDIF
     C*
     C                   EXSR      CLEAR
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        P0USER
     C                   MOVEL     Q1PGM         P0PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0DT
     C*
     C                   MOVEL     DSTIME        P0TIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        P0USER
     C                   MOVEL     Q1PGM         P0PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0DT
     C                   MOVEL     DSTIME        P0TIME
     C*
     C                   MOVEL     Q1USER        P0USER
     C                   MOVEL     Q1PGM         P0PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P0DT
     C                   MOVEL     DSTIME        P0TIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   EXSR      CLEAR
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR
     C*
     C                   ENDSR
     C*================================================================
     C* CLEAR  - Clear the screen fields
     C*================================================================
     C     CLEAR         BEGSR
     C                   CLEAR                   SCPRCD
     C                   CLEAR                   SCPAYE
     C                   CLEAR                   SCAMT
     C                   CLEAR                   SCSTDT
     C                   CLEAR                   SCENDT
     C                   CLEAR                   SCISDT
     C                   CLEAR                   SCDESC
     C*
     C                   ENDSR
