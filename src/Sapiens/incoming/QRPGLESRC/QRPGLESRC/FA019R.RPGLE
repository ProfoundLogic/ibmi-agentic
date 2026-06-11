     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA019R - Retrieve EOMB Auxiliary data
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program retrieves information from the EOMB Aux file. *
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
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     F*  WCMC Group Master file
     FFMFNL1    IF   E           K DISK
     FFAE1L     IF   E           K DISK

     I*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Call code
     C                   PARM                    @@CO#             3            Company #
     C                   PARM                    @@WCF#            3            WCMS Fund
     C                   PARM                    @@FYR             3            Policy Year
     C                   PARM                    @@CASE            7            Claim #
     C                   PARM                    @@EOB#            9            EOMB #
     C                   PARM                    @@FLG1            1            EOMB billing amt
     C                   PARM                    @@RC              1            Return Code
     C*
     C*  Key list for the WCMC Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C*
     C*  Key list for the EOMB File
     C*
     C     E1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
      *
     C     *LIKE         DEFINE    E1CO#         XXCO#
     C     *LIKE         DEFINE    E1SUB#        XXSUB#
     C     *LIKE         DEFINE    E1FND         XXFND
     C     *LIKE         DEFINE    E1FND         XXWCF#
     C     *LIKE         DEFINE    E1FYR         XXFYR
     C     *LIKE         DEFINE    E1CASE        XXCASE
     C     *LIKE         DEFINE    E1EOB#        XXEOB#
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
     C*  Retrieve the reconsideration flag from the EOMB Aux File.
     C*
     C     E1KY          CHAIN     PE101                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      E1FLG1        @@FLG1
     C                   ENDIF
     C*
     C*  End of program
     C*
     C     ENDPGM        TAG
     C                   RETURN
