      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD10R  - Create BADPV3.
      *****************************************************************
      *
      * DESCRIPTION:  This pgm creates file BADPV3.
      *
      * CREATED:      08/13/12  C:180    SUPPORT 80084
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV2    IF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV3    IF A E           K DISK

     D XXCO            S              3  0 INZ(777)
     D XXTYPE          S              1    INZ
     D X               S              3  0 INZ
     D J               S              3  0 INZ

     D @CLM            S             13    DIM(500) ASCEND

     D D1CLM           DS            13
     D   XXFND                        3  0
     D   XXFYR                        3  0
     D   XXCASE                       7  0

     C     PV3KY         KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE

      *
      *
      * Extract fixable claim number for payments
      *
     C                   READ      BADPV2

     C                   DOW       NOT %EOF(BADPV2)
     C                   MOVE      xPAFND        XXFND
     C                   MOVE      xPAFYR        XXFYR
     C                   MOVE      xPACASE       XXCASE
     C                   MOVE      'P'           XXTYPE
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EXSR      WRTPV3
     C                   ENDIF
     C                   READ      BADPV2
     C                   ENDDO


     C                   MOVE      *ON           *INLR
     C**********************************************************************
     C* WRITE BADPV3 RECORD
     C**********************************************************************
     C     WRTPV3        BEGSR
     C     PV3KY         CHAIN     BADPV3
     C                   IF        NOT %FOUND(BADPV3)
     C                   CLEAR                   BADPV3R
     C                   MOVE      XXCO          P3CO
     C                   MOVE      XXFND         P3FND
     C                   MOVE      XXFYR         P3FYR
     C                   MOVE      XXCASE        P3CASE
     C                   MOVE      XXTYPE        P3TYPE
     C                   WRITE     BADPV3R
     C                   ENDIF
     C                   ENDSR
