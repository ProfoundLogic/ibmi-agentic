     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W9300R02 - Trial Information Maintenance
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W9300R                                             *
     H*                                                               *
     H* CALLS TO:  none                                               *
     H*                                                               *
     H* CREATED    02/19/01  C:61  114WBC                             *
     H* IN BASE    06/20/07  C:53  114WMF                             *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H* 09/29/09 C:62  SUPPORT WC135 67535                            *
     H*                Change search.                                 *
     H*                                                               *
     H* 06/28/02 C:61  SUPPORT WC114 33010                            *
     H*                Verify all codes entered are "active".         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  64       CONVERTED CLAIM                                     *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       WCR0500 - INVALID DATE - SUIT DATE                  *
     H*  72       WCR0513 - INVALID TABLE VALUE - TRIAL TYPE          *
     H*  73       WCR0513 - INVALID TABLE VALUE - JUDGE               *
     H*  74       WCR0513 - INVALID TABLE VALUE - LEGAL ISSUE         *
     H*  75       WCR0513 - INVALID TABLE VALUE - COURT LEVEL         *
     H*  76       WCR0513 - INVALID TABLE VALUE - VERDICT             *
     H*  77       WCR0500 - INVALID DATE - VERDICT DATE               *
     H*  78       WC31364 - INVALID PETITION INDICATOR                *
     H*  79       WCR0544 - FIELDS MUTUALLY INCLUSIVE - VERD, VEDT    *
     H*                                                               *
     H*****************************************************************
      *
     FW9300F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWMTIP     UF A E             DISK
     F                                     RENAME(PTI01:DBRCDF)
      *
      *  Claim Master
     FWMCMP     IF   E             DISK
      *
      *  Fund Master
     FWMFNL     IF   E           K DISK
      *
      *  Employer Master
     FWMEML     IF   E           K DISK
      *
      *  Claimaint Demographics
     FWMCDL     IF   E           K DISK
      *
      *  Client Table
     F*** Begin Chg *** 09/29/09 **************************************
     F**********WTVLL   IF  E           K        DISK
     FWTGTL     IF   E           K DISK
     F*** End   Chg *** 09/29/09 **************************************
      *
      *
      *
     D***PY SCOPYMT01I
      *
      *
      *
      /COPY SCOPYMT01D
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D                 DS                  INZ
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Trial Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WMTIP
     C                   PARM                    @@RRN1            9 0          WMCMP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     SRCHPL        PLIST
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM                    ##ID              4            Table ID
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANK        ##RTCD            1
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Client Table Value
      *
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      KLIST
     C**********           KFLD           CMCO#
     C**********           KFLD           KKTBID
     C**********           KFLD           KKKEY
     C     GTKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** End   Chg *** 09/29/09 **************************************
      *
      *  Fund Master
      *
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *  Claimant Demographics
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C*** Begin Chg *** 09/29/09 **************************************
     C********** *LIKE     DEFN VLTBID    KKTBID
     C********** *LIKE     DEFN VLKEY     KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** End   Chg *** 09/29/09 **************************************
      *
      *
      *
     C/COPY SCOPYMT01C
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      *  Suit date must be valid date
      *
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C                   Z-ADD     SCSUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSUDT
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        90  71
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN40 = *ON
      *
      *  Trial Type must be valid and active
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  'TRTP'        KKTBID
     C                   MOVEL(P)  SCTRTP        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
      ***** BEG ADD ******* 06/28/02 **********************************
     C**N60***** VLSTAT    COMP 'A'                  6060
      ***** END ADD ******* 06/28/02 **********************************
     C**N60*****           MOVELVLDESC    SCTRDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60GTSTAT        COMP      'A'                                6060
     C  N60              MOVEL(P)  GTDESC        SCTRDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCTRDS
     C   60              SETON                                        90  72
     C                   ENDIF
      *
      *  Judge must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  'JUDG'        KKTBID
     C                   MOVEL(P)  SCJUDG        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
      ***** BEG ADD ******* 06/28/02 **********************************
     C**N60***** VLSTAT    COMP 'A'                  6060
      ***** END ADD ******* 06/28/02 **********************************
     C**N60*****           MOVELVLDESC    SCJUDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60GTSTAT        COMP      'A'                                6060
     C  N60              MOVEL(P)  GTDESC        SCJUDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCJUDS
     C   60              SETON                                        90  73
     C                   ENDIF
      *
      *  Legal Issue must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  'ISSU'        KKTBID
     C                   MOVEL(P)  SCISSU        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
      ***** BEG ADD ******* 06/28/02 **********************************
     C**N60***** VLSTAT    COMP 'A'                  6060
      ***** END ADD ******* 06/28/02 **********************************
     C**N60*****           MOVELVLDESC    SCISDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60GTSTAT        COMP      'A'                                6060
     C  N60              MOVEL(P)  GTDESC        SCISDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCISDS
     C   60              SETON                                        90  74
     C                   ENDIF
      *
      *  Court Level must be valid
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  'CLVL'        KKTBID
     C                   MOVEL(P)  SCCLVL        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
      ***** BEG ADD ******* 06/28/02 **********************************
     C**N60***** VLSTAT    COMP 'A'                  6060
      ***** END ADD ******* 06/28/02 **********************************
     C**N60*****           MOVELVLDESC    SCCLDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60GTSTAT        COMP      'A'                                6060
     C  N60              MOVEL(P)  GTDESC        SCCLDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCCLDS
     C   60              SETON                                        90  75
     C                   ENDIF
      *
      *  Verdict must be valid if entered
      *
     C     *IN90         IFEQ      *OFF
     C     SCVERD        ANDNE     *BLANKS
     C                   MOVEL(P)  'VERD'        KKTBID
     C                   MOVEL(P)  SCVERD        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
      ***** BEG ADD ******* 06/28/02 **********************************
     C**N60***** VLSTAT    COMP 'A'                  6060
      ***** END ADD ******* 06/28/02 **********************************
     C**N60*****           MOVELVLDESC    SCVEDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60GTSTAT        COMP      'A'                                6060
     C  N60              MOVEL(P)  GTDESC        SCVEDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCVEDS
     C   60              SETON                                        90  76
     C                   ENDIF
      *
      *  Verdict Date must be valid if entered
      *
     C     *IN90         IFEQ      *OFF
     C     SCVEDT        ANDNE     *ZERO
     C                   Z-ADD     SCVEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVEDT
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        90  77
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN90
      *
      *  Verdict and Verdict Date are mutually inclusive
      *
     C     *IN90         IFEQ      *OFF
      *
     C     SCVERD        IFNE      *BLANKS
     C     SCVEDT        ANDEQ     *ZERO
     C                   SETON                                        90  79
     C                   ENDIF
      *
     C     SCVERD        IFEQ      *BLANKS
     C     SCVEDT        ANDNE     *ZERO
     C                   SETON                                        90  79
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Petition indicator (Y/N/P)
      *
     C     *IN90         IFEQ      *OFF
     C     SCPETI        ANDNE     ' '
     C     SCPETI        ANDNE     'Y'
     C     SCPETI        ANDNE     'N'
     C     SCPETI        ANDNE     'P'
     C                   SETON                                        90  78
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *================================================================
      * KJDO - Searches
      *================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * Trial Type search
      *
     C     HFLD          WHENEQ    'SCTRTP'
     C                   MOVE      'TRTP'        ##ID
     C*** Begin Chg *** 09/29/09 **************************************
     C**********           CALL 'W2497R'  SRCHPL
     C                   CALL      'W2493R'      SRCHPL
     C*** End   Chg *** 09/29/09 **************************************
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        82
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ##KEY         SCTRTP
     C                   MOVEL(P)  'TRTP'        KKTBID
     C                   MOVEL(P)  SCTRTP        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCTRDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCTRDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCTRDS
     C                   SETON                                        83
     C                   ENDIF                                                  ##KEY
      *
      * Judge search
      *
     C     HFLD          WHENEQ    'SCJUDG'
     C                   MOVE      'JUDG'        ##ID
     C*** Begin Chg *** 09/29/09 **************************************
     C**********           CALL 'W2497R'  SRCHPL
     C                   CALL      'W2493R'      SRCHPL
     C*** End   Chg *** 09/29/09 **************************************
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        83
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ##KEY         SCJUDG
     C                   MOVEL(P)  'JUDG'        KKTBID
     C                   MOVEL(P)  SCJUDG        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCJUDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCJUDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCJUDS
     C                   SETON                                        84
     C                   ENDIF                                                  ##KEY
      *
      * Legal Issue
      *
     C     HFLD          WHENEQ    'SCISSU'
     C                   MOVE      'ISSU'        ##ID
     C*** Begin Chg *** 09/29/09 **************************************
     C**********           CALL 'W2497R'  SRCHPL
     C                   CALL      'W2493R'      SRCHPL
     C*** End   Chg *** 09/29/09 **************************************
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        84
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ##KEY         SCISSU
     C                   MOVEL(P)  'ISSU'        KKTBID
     C                   MOVEL(P)  SCISSU        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCISDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCISDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCISDS
     C                   SETON                                        85
     C                   ENDIF                                                  ##KEY
      *
      * Court Level
      *
     C     HFLD          WHENEQ    'SCCLVL'
     C                   MOVE      'CLVL'        ##ID
     C*** Begin Chg *** 09/29/09 **************************************
     C**********           CALL 'W2497R'  SRCHPL
     C                   CALL      'W2493R'      SRCHPL
     C*** End   Chg *** 09/29/09 **************************************
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        85
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ##KEY         SCCLVL
     C                   MOVEL(P)  'CLVL'        KKTBID
     C                   MOVEL(P)  SCCLVL        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCCLDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCCLDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCCLDS
     C                   SETON                                        86
     C                   ENDIF                                                  ##KEY
      *
      * Verdict
      *
     C     HFLD          WHENEQ    'SCVERD'
     C                   MOVE      'VERD'        ##ID
     C*** Begin Chg *** 09/29/09 **************************************
     C**********           CALL 'W2497R'  SRCHPL
     C                   CALL      'W2493R'      SRCHPL
     C*** End   Chg *** 09/29/09 **************************************
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        86
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ##KEY         SCVERD
     C                   MOVEL(P)  'VERD'        KKTBID
     C                   MOVEL(P)  SCVERD        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCVEDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCVEDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCVEDS
     C                   SETON                                        87
     C                   ENDIF                                                  ##KEY
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14 = DARTS
      *===============================================================
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      ##PGM            10
      *
     C                   ENDSR                                                  KNDO
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     TISUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSUDT
      *
      * Trial Type
      *
     C                   MOVEL(P)  TITRTP        SCTRTP
     C                   MOVEL(P)  'TRTP'        KKTBID
     C                   MOVEL(P)  SCTRTP        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCTRDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCTRDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCTRDS
      *
      * Judge
      *
     C                   MOVEL(P)  TIJUDG        SCJUDG
     C                   MOVEL(P)  'JUDG'        KKTBID
     C                   MOVEL(P)  SCJUDG        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCJUDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCJUDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCJUDS
      *
      * Issue
      *
     C                   MOVEL(P)  TIISSU        SCISSU
     C                   MOVEL(P)  'ISSU'        KKTBID
     C                   MOVEL(P)  SCISSU        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCISDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCISDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCISDS
      *
      * Court Level
      *
     C                   MOVEL(P)  TICLVL        SCCLVL
     C                   MOVEL(P)  'CLVL'        KKTBID
     C                   MOVEL(P)  SCCLVL        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCCLDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCCLDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCCLDS
      *
      * Verdict
      *
     C                   MOVEL(P)  TIVERD        SCVERD
     C                   MOVEL(P)  'VERD'        KKTBID
     C                   MOVEL(P)  SCVERD        KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    SCVEDS    P
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCVEDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL(P)  *BLANKS       SCVEDS
      *
      *
     C                   Z-ADD     TIVEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVEDT
      *
     C                   MOVEL(P)  TIPETI        SCPETI
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     CMCO#         TICO#
     C                   Z-ADD     CMFND         TIFND
     C                   Z-ADD     CMFYR         TIFYR
     C                   Z-ADD     CMCASE        TICASE
      *
     C                   Z-ADD     SCSUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TISUDT
      *
     C                   MOVEL(P)  SCTRTP        TITRTP
     C                   MOVEL(P)  SCJUDG        TIJUDG
     C                   MOVEL(P)  SCISSU        TIISSU
     C                   MOVEL(P)  SCCLVL        TICLVL
     C                   MOVEL(P)  SCVERD        TIVERD
      *
     C                   Z-ADD     SCVEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TIVEDT
      *
     C                   MOVEL(P)  SCPETI        TIPETI
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90     SEARCH
     C     *INKN         CASEQ     *ON           KNDO                     90     DARTS
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'A'                                    40     ADD
     C     @@MODE        COMP      'D'                                    09     INQUIRY
      *
      * Get Claim master record
      *
     C     @@RRN1        CHAIN     PCM01                              60
     C   60              SETON                                        LR
     C   60              RETURN
      *
      *  Get Fund Name
      *
     C                   Z-ADD     CMFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
     C   60              MOVE(P)   *BLANK        SCFNAM
      *
      *  Get Employer Name
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      CMEMP#        ##EMP#            9 0
     C                   PARM      CMDIV         ##DIV             5 0
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    ##EDN            80
     C                   END                                                    EMKY
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
      ******  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ******  End Chg  ***  06/09/20  **********************
      *
      *  CLAIMANT NAME
      *
     C     CDKY          CHAIN     PCD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
      *
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   ENDIF                                                  CDMID NE BLANK
      *
     C                   ENDIF                                                  CDKY
      *
     C                   ENDIF                                                  SS# <> BLANK
      *
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *
      *  FYR-CASE
      *
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        TIUSER
     C                   MOVEL     Q1PGM         TIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TIDT
     C                   MOVEL     DSTIME        TITIME
      *
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        TIIUSR
     C                   MOVEL     Q1USER        TIUSER
     C                   MOVEL     Q1PGM         TIIPGM
     C                   MOVEL     Q1PGM         TIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TIIDT
     C                   MOVEL     SRDTO         TIDT
     C                   MOVEL     DSTIME        TIITM
     C                   MOVEL     DSTIME        TITIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMTIP                                        WRITER
     C                   ENDSR
      *
      *
      *
