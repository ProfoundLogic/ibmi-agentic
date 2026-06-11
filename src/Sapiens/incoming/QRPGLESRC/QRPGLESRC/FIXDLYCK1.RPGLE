     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *****************************************************************
      * FIXDLYCK1
      *   Prepare files in library PDSCHECKS for reprint of daily
      *   checks.
      *
      * CALLED FROM:
      *   FIXDLYCHK - Reprint Daily Batch Checks
      *
      *****************************************************************
      * REVISIONS:
      *
      * 10/06/10 C:061 SUPPORT WA039 74438
      *                Created.  Copied from FIXDCK1.
      *
      *****************************************************************

     FWACPL     UF   E           K DISK
     FWARPLU    UF   E           K DISK


     D PSDS          ESDS                  EXTNAME(SZQ1P)


     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPRDT            7            CYYMMDD


      ********************************
      *  KLIST
      ********************************

     C     CPKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXPRDT            7 0

     C     RPKY          KLIST
     C                   KFLD                    XXCO#


      ********************************
      *  BEGIN
      ********************************

     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQPRDT        XXPRDT            7 0
     C     CPKY          CHAIN     PCP01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       CPSTA0
     C                   MOVE      *BLANKS       CPSTA1
     C                   UPDATE    PCP01
     C     CPKY          READE     PCP01                                  60
     C                   ENDDO

      *****
     C     RPKY          CHAIN     PRP01                              60

     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       RPSTAT
     C                   UPDATE    PRP01
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO

     C                   SETON                                        LR

