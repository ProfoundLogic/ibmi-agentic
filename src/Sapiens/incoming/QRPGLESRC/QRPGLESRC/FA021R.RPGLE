     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA021R - Check for deleted EOMB.
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program determines if an EOMB exists in the EOMB      *
     H*    Delete file based on the entry parms.                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7004R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 10/30/97  C:15  Project:                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/18/00 C:62 Project: WC044 22882                           *
     H*                BRING IN FA015R LOGIC.                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H* PARAMETER USAGE:
     H*
     H*    RETURNS:
     H*      @@RRN  <=== Relative record number of FAEOP record.
     H*      @@SUB# <=== FNSUB#
     H*      @@GRP# <=== FNFND
     H*      @@FEIN <=== EOFEIN
     H*      @@MOD  <=== EOMOD
     H*      @@AST  <=== EOAST
     H*      @@RC   <=== 'F' = Unable to get WCMC group.
     H*                  'N' = Unable to find EOMB.
     H*                  '1' = Unable to find deleted EOMB
     H*                  'D' = Deleted EOMB
     H*                  ' ' = Return Sub#, Grp#, FEIN, Mod, AST, & RRN
     H*
     H*****************************************************************
     FFMFNL1    IF   E           K DISK
     FFDEOL1    IF   E           K DISK
     F*** BEGIN ADD *** 05/18/00 **************************************
     F*  WCMC EOB Master file
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 05/18/00 **************************************

     D*
     D*** BEGIN ADD *** 05/18/00 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I*** END   ADD *** 05/18/00 **************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Call code
     C                   PARM                    @@CO#             3            Company #
     C                   PARM                    @@WCF#            3            WCMS Fund
     C                   PARM                    @@FYR             3            Policy Year
     C                   PARM                    @@CASE            7            Claim #
     C                   PARM                    @@EOB#            9            EOMB #
     C                   PARM                    @@RC              1            Return Code
     C*** BEGIN ADD *** 05/18/00 **************************************
     C                   PARM                    @@SUB#                         Subscriber #
     C                   PARM                    @@GRP#                         WCMC Group #
     C                   PARM                    @@AST
     C                   PARM                    @@FEIN
     C                   PARM                    @@MOD
     C                   PARM                    @@RRN             9 0
     C*** END   ADD *** 05/18/00 **************************************
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
     C*** BEGIN ADD *** 05/18/00 **************************************
     C     *LIKE         DEFINE    EOSUB#        @@SUB#
     C     *LIKE         DEFINE    EOFND         @@GRP#
     C     *LIKE         DEFINE    EOFEIN        @@FEIN
     C     *LIKE         DEFINE    EOMOD         @@MOD
     C     *LIKE         DEFINE    EOAST         @@AST
     C*** END   ADD *** 05/18/00 **************************************
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
     C*** BEGIN ADD *** 05/18/00 **************************************
     C                   MOVEL     *BLANKS       @@RC
     C*** END   ADD *** 05/18/00 **************************************
     C*
     C*  Translate the WCMS fund into WCMC Sub# and fund.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      FNSUB#        XXSUB#
     C                   MOVE      FNFND         XXFND
     C*** BEGIN ADD *** 05/18/00 **************************************
     C                   Z-ADD     FNSUB#        @@SUB#
     C                   Z-ADD     FNFND         @@GRP#
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RC
     C                   Z-ADD     *ZEROS        @@RRN
     C                   Z-ADD     *ZEROS        @@SUB#
     C                   Z-ADD     *ZEROS        @@GRP#
     C                   RETURN
     C*** END   ADD *** 05/18/00 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 05/18/00 **************************************
     C*
     C* Does the eomb exist
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'N'           @@RC
     C                   Z-ADD     *ZEROS        @@RRN
     C*** END   ADD *** 05/18/00 **************************************
     C*
     C*  Check for a record in the EOMB file.
     C*
     C*** BEGIN DEL *** 05/18/00 **************************************
     C**********           MOVE *BLANK    @@RC
     C*** END   DEL *** 05/18/00 **************************************
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      '1'           @@RC
     C*** BEGIN ADD *** 05/18/00 **************************************
     C                   ELSE
     C*
     C                   MOVEL     'D'           @@RC
     C                   RETURN
     C*** END   ADD *** 05/18/00 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 05/18/00 **************************************
     C                   ELSE
     C*
     C                   Z-ADD     DBRRN         @@RRN
     C                   MOVEL     EOAST         @@AST
     C                   MOVE      EOFEIN        @@FEIN
     C                   MOVE      EOMOD         @@MOD
     C*
     C                   ENDIF
     C*** END   ADD *** 05/18/00 **************************************
     C*
     C*  End of program
     C*
     C*** BEGIN DEL *** 05/18/00 **************************************
     C********** ENDPGM    TAG
     C*** END   DEL *** 05/18/00 **************************************
     C                   RETURN
