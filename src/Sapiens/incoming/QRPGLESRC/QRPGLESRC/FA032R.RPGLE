     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA032R - EDIT EOMB INFORMATION - BILL ENTRY
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program edits information from the EOMB file.         *
     H*    Delete file based on the entry parms.                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 06/07/00  C:62  Project:MC230                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*  RETURN VALUES: (@@RC)                                        *
     H*                                                               *
     H*  '1' - Failed to retrieve WCMC Subscriber/Group               *
     H*  '2' - Deleted EOMB                                           *
     H*  '5' - EOMB does not exist                                    *
     H*                                                               *
     H*****************************************************************
     FFMFNL1    IF   E           K DISK
     FFDEOL1    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0          Company #
     C                   PARM                    @@WCF#            3 0          WCMS Fund
     C                   PARM                    @@FYR             3 0          Policy Year
     C                   PARM                    @@CASE            7 0          Claim #
     C                   PARM                    @@EOB#            9 0          EOMB #
     C                   PARM                    @@RC              1            Return Code
     C*
     C*  Key list for the WCMC Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C*
     C*  Key list for the EOMB File
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
      *
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFND         KKWCF#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@WCF#        KKWCF#
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKSUB#
     C*
     C*================================================================
     C*  Program Start
     C*================================================================
     C*
     C*  Translate the WCMS fund into WCMC Sub# and fund.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     FNFND         KKFND
     C*
     C* Does the EOMB exist?
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     '5'           @@RC
     C*
     C* Has the EOMB been deleted?
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      '2'           @@RC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* Unable to retrieve WCMC Subscriber/Group.
     C*
     C                   MOVEL     '1'           @@RC
     C*
     C                   ENDIF
     C*
     C*  End of program
     C*
     C                   RETURN
