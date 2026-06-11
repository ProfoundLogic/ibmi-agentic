      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006K - Update missing payment data
      *****************************************************************
      *
      * DESCRIPTION:  This pgm populates the missing data in the
      *               payment file.
      *
      * CREATED:      08/15/12  C:180    SUPPORT 80084
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2    IF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FWDPAL     UF   E           K DISK

     D XXCO            S              3  0 INZ(777)
     D XXFND           S              3  0 INZ
     D XXFYR           S              3  0 INZ
     D XXCASE          S              7  0 INZ


      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XPASEQ#

      *
      *
      * Extract fixable claim number for payments
      *
     C                   READ      BADPV2

     C                   DOW       NOT %EOF(BADPV2)
     C                   MOVE      xPAFND        XXFND
     C                   MOVE      xPAFYR        XXFYR
     C                   MOVE      xPACASE       XXCASE
     C                   EXSR      UPDPAY
     C                   READ      BADPV2
     C                   ENDDO


     C                   MOVE      *ON           *INLR
     C**********************************************************************
     C* UPDATE PAYMENT DATA
     C**********************************************************************
     C     UPDPAY        BEGSR
     C     PAKY          CHAIN     WDPAL
     C                   IF        %FOUND(WDPAL)
     C                   IF        PABR2 = *BLANKS
     C                   MOVE      xPABR2        PABR2
     C                   UPDATE    PPA01
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
