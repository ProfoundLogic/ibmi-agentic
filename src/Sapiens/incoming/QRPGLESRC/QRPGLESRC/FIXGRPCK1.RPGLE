     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      ****************************************************************
      * FIXGRPCK1
      *   Prepare files in library PDSCHECKS for reprint of GROUP
      *   checks.
      *
      * CALLED FROM:
      *   FIXGRPCHK - Reprint GROUP Checks
      *
      ****************************************************************
      * REVISIONS:
      *
      * 10/06/10 C:061 SUPPORT WA039 74438
      *                Created.
      *
      *****************************************************************

     FWAC3L     UF   E           K DISK
     FWARPLU    UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPRDT            7            CYYMMDD


      ********************************
      *  KLIST
      ********************************

     C     C3KY          KLIST
     C                   KFLD                    kkCO#             3 0
     C                   KFLD                    kkPRDT            7 0

     C     RPKY          KLIST
     C                   KFLD                    kkCO#


      ********************************
      *  BEGIN
      ********************************

     C                   MOVE      QQCO#         kkCO#             3 0
     C                   MOVE      QQPRDT        kkPRDT            7 0
     C     C3KY          CHAIN     PCP01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       CPSTA0
     C                   MOVE      *BLANKS       CPSTA1
     C                   UPDATE    PCP01
     C     C3KY          READE     PCP01                                  60
     C                   ENDDO

      *****
     C     RPKY          CHAIN     PRP01                              60

     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       RPSTAT
     C                   UPDATE    PRP01
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO

     C                   SETON                                        LR

