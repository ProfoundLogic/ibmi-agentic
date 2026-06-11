      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006J - Fix MC upload data
      *****************************************************************
      *
      * DESCRIPTION:  This pgm fixes reserves in files WMCMP and WMRHP
      *               for claims found in file  BADPV3
      *
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV3    IP   E           K DISK


     C     PL06          PLIST
     C                   PARM                    @@CO              3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM      'Y'           @@UPD             1

     C     PL06A         PLIST
     C                   PARM                    @@CO              3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM      'Y'           @@UPD             1
     C                   PARM      'Y'           @@PRT             1
     C                   PARM                    @@EXIT            1

     C                   MOVE      P3CO          @@CO
     C                   MOVE      P3FND         @@FND
     C                   MOVE      P3FYR         @@FYR
     C                   MOVE      P3CASE        @@CASE
     C                   MOVE      *OFF          @@EXIT
     C*
      *
      * Fix released payments
      *
     C                   CALL      'FIXD006C'    PL06A

      *
      * Fix remaining reserves for payment requests
      *
     C                   IF        P3TYPE = 'R' or P3TYPE ='B'
     C                   CALL      'FIXD006D'    PL06A
     C                   ENDIF
      *
      * Fix reserve history to match incurred.
      *
     C                   CALL      'FIXD006B'    PL06A
      *
     C* Fix outstanding reserves for closed claims
      *
     C                   CALL      'FIXD006F'    PL06A

     CLR                 MOVE      *ON           @@EXIT
     CLR                 CALL      'FIXD006B'    PL06A
     CLR                 CALL      'FIXD006C'    PL06A
     CLR                 CALL      'FIXD006D'    PL06A
     CLR                 CALL      'FIXD006F'    PL06A
