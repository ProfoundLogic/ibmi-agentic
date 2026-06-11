     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA031R - EDITS EOMB INFORMATION - PBILLS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program edits information from the EOMB file.         *
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
     H*  '3' - Invalid Reconsideration                                *
     H*  '4' - Not Current Reconsideration                            *
     H*  '5' - EOMB does not exist                                    *
     H*                                                               *
     H*****************************************************************
     FFMFNL1    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFAEAL1    IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
1    C                   PARM                    @@CO#             3 0          Company #
2    C                   PARM                    @@WCF#            3 0          WCMS Fund
3    C                   PARM                    @@FYR             3 0          Policy Year
4    C                   PARM                    @@CASE            7 0          Claim #
5    C                   PARM                    @@EOB#            9 0          EOMB #
6    C                   PARM                    @@RCN             3 0          RECON #
7    C                   PARM                    @@RC              1            Return Code
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
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFND         KKWCF#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@WCF#        KKWCF#
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C                   Z-ADD     @@RCN         KKRCN
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKSUB#
     C*================================================================
     C*  Program Start
     C*================================================================
     C*
     C* Translate the WCMS fund into WCMC Sub# and fund.
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
     C     EOKY          CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* If the EOMB exists, then is it a deleted EOMB?
     C*
     C     EOBLST        IFEQ      'D'
     C*
     C                   MOVEL     '2'           @@RC
     C*
     C                   ELSE
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C* Is the reconsideration valid?
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     '3'           @@RC
     C*
     C                   ELSE
     C*
     C* Is the reconsideration the current reconsideration?
     C*
     C     @@RCN         IFNE      EORCN
     C*
     C                   MOVEL     '4'           @@RC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* The EOMB does not exist?
     C*
     C                   MOVEL     '5'           @@RC
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* Unable to retrieve the WCMC Subscriber/Group?
     C*
     C                   MOVEL     '1'           @@RC
     C*
     C                   ENDIF
     C*
     C* End of program
     C*
     C                   RETURN
