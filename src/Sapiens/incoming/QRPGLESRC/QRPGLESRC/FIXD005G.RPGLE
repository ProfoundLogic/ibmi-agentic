      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005G - Create BADPV4.
      *****************************************************************
      *
      * DESCRIPTION:  This pgm creates file BADPV4.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV2L   IF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV2RL  IF   E           K DISK    RENAME(BADPV2R:BADPVRR) PREFIX(x)
     FBADPV4    IF A E           K DISK

     D XXCO            S              3  0 INZ(777)
     D X               S              3  0 INZ
     D J               S              3  0 INZ

     D @CLM            S             15    DIM(500) ASCEND

     D D1CLM           DS            15
     D   XXFND                        3  0
     D   XXFYR                        3  0
     D   XXCASE                       7  0
     D   XXRECD                       2

     C     PV4KY         KLIST
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
     C                   EXSR      DONTFIX
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
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EXSR      WRTPV4
     C                   ENDIF
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
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EXSR      WRTPV4
     C                   ENDIF
     C                   READ      BADPV2RL
     C                   ENDDO

     C                   MOVE      *ON           *INLR
     C**********************************************************************
     C* WRITE BADPV4 RECORD
     C**********************************************************************
     C     WRTPV4        BEGSR
     C     PV4KY         CHAIN     BADPV4
     C                   IF        NOT %FOUND(BADPV4)
     C                   CLEAR                   BADPV4R
     C                   MOVE      XXCO          P4CO
     C                   MOVE      XXFND         P4FND
     C                   MOVE      XXFYR         P4FYR
     C                   MOVE      XXCASE        P4CASE
     C                   MOVE      XXRECD        P4RECD
     C                   WRITE     BADPV4R
     C                   ENDIF
     C                   ENDSR
     C**********************************************************************
     C* FIND CLAIMS THAT WILL NOT BE FIXED
     C**********************************************************************
     C     DONTFIX       BEGSR

      *
      * Extract non-auto-fixable claims from payments
      *
     C                   READ      BADPV2L

     C                   DOW       NOT %EOF(BADPV2L)
     C                   MOVE      xPAFND        XXFND
     C                   MOVE      xPAFYR        XXFYR
     C                   MOVE      xPACASE       XXCASE
     C                   Z-ADD     1             X
     C                   IF        xPARQST = 'DONTFIX'
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EVAL      J = J +1
     C                   MOVE      D1CLM         @CLM(J)
     C                   SORTA     @CLM
     C                   ENDIF
     C                   ENDIF
     C
     C                   READ      BADPV2L
     C                   ENDDO

      *
      * Extract non-auto-fixable claims from payment requests
      *
      *
     C                   READ      BADPV2RL

     C                   DOW       NOT %EOF(BADPV2RL)
     C                   MOVE      xRPFND        XXFND
     C                   MOVE      xRPFYR        XXFYR
     C                   MOVE      xRPCASE       XXCASE
     C                   Z-ADD     1             X
     C                   IF        xRPRQST = 'DONTFIX'
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EVAL      J = J +1
     C                   MOVE      D1CLM         @CLM(J)
     C                   SORTA     @CLM
     C                   ENDIF
     C                   ENDIF

     C                   READ      BADPV2RL
     C                   ENDDO
     C                   ENDSR
