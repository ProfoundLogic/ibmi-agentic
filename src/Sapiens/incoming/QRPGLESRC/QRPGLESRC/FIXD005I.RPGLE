      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005I - Create BADPV5 -
      *****************************************************************
      *
      * DESCRIPTION:  This pgm creates file BADPV5 to check for
      *               multiple resv codes in a claim.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2L   IF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV2RL  IF   E           K DISK    RENAME(BADPV2R:BADPVRR) PREFIX(x)
     FBADPV5    IF A E           K DISK

     D XXCO            S              3  0 INZ(777)
     D X               S              3  0 INZ
     D J               S              3  0 INZ

     D D1CLM           DS            15
     D   XXFND                        3  0
     D   XXFYR                        3  0
     D   XXCASE                       7  0
     D   XXRECD                       2

     C     PV5KY         KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXRECD

      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XPASEQ#

     C     RPKY          KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XRPSEQ#
      *
      *
      * Extract fixable claim number for payments
      *
     C     *LOVAL        SETLL     BADPV2L
     C                   READ      BADPV2L

     C                   DOW       NOT %EOF(BADPV2L)
     C                   MOVE      xPAFND        XXFND
     C                   MOVE      xPAFYR        XXFYR
     C                   MOVE      xPACASE       XXCASE
     C                   MOVE      xPARECD       XXRECD
     C                   EXSR      WRTPV5
     C                   READ      BADPV2L
     C                   ENDDO

      *
      * Extract fixable claim number for payment requests
      *
     C     *LOVAL        SETLL     BADPV2RL
     C                   READ      BADPV2RL

     C                   DOW       NOT %EOF(BADPV2RL)
     C                   MOVE      xRPFND        XXFND
     C                   MOVE      xRPFYR        XXFYR
     C                   MOVE      xRPCASE       XXCASE
     C                   MOVE      xRPRECD       XXRECD
     C                   EXSR      WRTPV5
     C                   READ      BADPV2RL
     C                   ENDDO

     C                   MOVE      *ON           *INLR
     C**********************************************************************
     C* WRITE BADPV5 RECORD
     C**********************************************************************
     C     WRTPV5        BEGSR
     C     PV5KY         CHAIN     BADPV5
     C                   IF        NOT %FOUND(BADPV5)
     C                   CLEAR                   BADPV5R
     C                   MOVE      XXCO          P5CO
     C                   MOVE      XXFND         P5FND
     C                   MOVE      XXFYR         P5FYR
     C                   MOVE      XXCASE        P5CASE
     C                   MOVE      XXRECD        P5RECD
     C                   WRITE     BADPV5R
     C                   ENDIF
     C                   ENDSR
