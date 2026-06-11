     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA006R - API to build 2 line Claim Screen Header
     H* ***************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program will access the Claim Master, Claimant        *
     H*    demographics to build 2 lines of Claim information to be   *
     H*    used as a screen heading.                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070Rnn - Any Claim Master Maint pgm                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    WA001R                                                     *
     H*                                                               *
     H*  CREATED: 06/04/97  C:30  Project:  Version 3                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:318 Project: SY420
     H*                  Mask SSN
     H*
     H*  04/06/98  C:33  SEQ#: WC060/10878                            *
     H*                  GET GROUP (FND) NAME FROM VALUE PASSED IN    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*
     H*****************************************************************
     F*================================================================
     F*          FILE DEFINITIONS
     F*================================================================
     F*________________________________________________________________
     FWMCML1    IF   E           K DISK
     F*      Claim Master  --Co/Fnd/FYR/Case
     F*________________________________________________________________
     FWMCDL     IF   E           K DISK
     F*      Claim Demographics --SS#                            -CDKY-
     F*________________________________________________________________
     F***  BEGIN ADD  ***  04/06/98  *********************************
     FWMFNL     IF   E           K DISK
     F***  END   ADD  ***  04/06/98  *********************************
      *

     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS
     D  LN1                    1     79
     D  LN1A                   1      6
     D  LN1B                   8     10
     D  LN1C                  12     71
      *
     D                 DS
     D  LN2                    1     79
     D  LN2CSH                 1      6
     D  LN2CS                  8     18
     D  LN2A                  21     24
     D  LN2BB                 26     37
     D  LN2B1                 26     28
     D  LN2B2                 29     29
     D  LN2B3                 30     31
     D  LN2B4                 32     32
     D  LN2B5                 33     36
     D  LN2C                  39     60
     D  LN2D                  63     66
     D  LN2E                  69     78
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D HEAD1           C                   CONST('Group:')
     D HEAD3           C                   CONST('SS#:')
     D HEAD4           C                   CONST('DOI:')
     D HEAD5           C                   CONST('Case#:')
     D COMAPI          C                   CONST('WA001R')
     D DIGITS          C                   CONST('0123456789')
      *
      *===============================================================
      *       FILE KEY LISTS
      *===============================================================
      *
      *
      *    Claim Demographics

     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *    Claim Master  --Co/Fund/FundYear/Case
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***  BEGIN ADD  ***  04/06/98  *********************************
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  END   ADD  ***  04/06/98  *********************************
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C***  BEGIN ADD  ***  04/06/98  *********************************
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMFND         ##FND
     C***  END   ADD  ***  04/06/98  *********************************
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *===============================================================
      *       ENTRY PARM LIST
      *===============================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@LN1            79
     C                   PARM                    @@LN2            79
      *
      *  Load Claim Master data to keys
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C                   MOVE      *BLANK        @@LN1
     C                   MOVE      *BLANK        @@LN2
      *
      *  Access Claim Master
      *
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *___________________________________________________________________
      *          B U I L D     L I N E   #   1
      *
      *  Call API to obtain Fund Name
      *
     C                   CALL      'WA001R'
     C***  BEGIN CHG  ***  04/06/98  *********************************
     C**********           PARM           CMCO#
     C**********           PARM           CMFND
     C                   PARM      CMCO#         ##CO#
     C                   PARM      CMFND         ##FND
     C***  END   CHG  ***  04/06/98  *********************************
     C                   PARM                    ##CONM           60
     C                   PARM                    ##FNAM           60
      *
     C                   MOVE      HEAD1         LN1A
     C                   MOVE      XXFND         LN1B
     C*
     C***  BEGIN CHG  ***  04/06/98  *********************************
     C**********           MOVEL##FNAM    LN1C
     C                   CLEAR                   LN1C
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        LN1C
     C***  END   CHG  ***  04/06/98  *********************************
     C*
     C                   MOVE      LN1           @@LN1
      *___________________________________________________________________
      *          B U I L D     L I N E   #   2
      *
      *  Claimant SS#
      *
     C     CMSS#         IFNE      *BLANK
     C                   MOVE      HEAD3         LN2A
     C                   MOVE      HEAD4         LN2D
      *
     C     DIGITS        CHECK     CMSS#         X                 3 0    42
      *
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
     C     *IN42         IFEQ      *OFF
     C                   MOVE      SS1           LN2B1
     C                   MOVE      '-'           LN2B2
     C                   MOVE      SS2           LN2B3
     C                   MOVE      '-'           LN2B4
     C                   MOVE      SS3           LN2B5
     C                   ELSE
     C                   MOVEL     CMSS#         LN2BB
     C                   ENDIF                                                  END *IN42
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
      *     Claimant Name
      *
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM           20            LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   ENDIF
     C                   MOVEL     SCCNAM        LN2C
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Accident date
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT            8
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   ENDIF
      *
     C                   MOVEL     SCACDT        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
      *
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
     C                   MOVEL     XXNUM         LN2E
      *
      *  FYR-CASE
      *
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS           11
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      HEAD5         LN2CSH
     C                   MOVE      SCYRCS        LN2CS
     C                   MOVE      LN2           @@LN2
      *___________________________________________________________________
      *
     C                   MOVE      *ON           *INLR
      *
