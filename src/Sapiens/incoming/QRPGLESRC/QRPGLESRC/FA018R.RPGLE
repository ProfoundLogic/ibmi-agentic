     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA018R - Update the EOMB billed amount
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program updates the EOMB billed amount.               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 10/24/97  C:15  Project:                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/02/00  C:53  Project: MC221
     H*            REMOVE UPDATE TO EOUSER.
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     F*  WCMC Group Master file
     FFMFNL1    IF   E           K DISK
     FFAEOL4    UF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Call code
     C                   PARM                    @@CO#             3            Company #
     C                   PARM                    @@WCF#            3            WCMS Fund
     C                   PARM                    @@FYR             3            Policy Year
     C                   PARM                    @@CASE            7            Claim #
     C                   PARM                    @@EOB#            9            EOMB #
     C                   PARM                    @@BAMT            9            EOMB billing amt
     C                   PARM                    @@RC              1            Return Code
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  Key list for the WCMC Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C*
     C*  Key list for the EOMB File
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
      *
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFND         XXWCF#
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@WCF#        XXWCF#
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C                   MOVE      @@EOB#        XXEOB#
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXSUB#
     C*
     C*================================================================
     C*  Program Start
     C*================================================================
     C*
     C*  Translate the WCMS fund into WCMC Sub# and fund.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      FNSUB#        XXSUB#
     C                   MOVE      FNFND         XXFND
     C                   ENDIF
     C*
     C*  Update the EOMB billing amount.
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      @@BAMT        EOBAMT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVE      Q1PGM         EOPGM
     C*******************  MOVE Q1USER    EOUSER           DEL 02/02/00
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C*  End of program
     C*
     C     ENDPGM        TAG
     C                   RETURN
