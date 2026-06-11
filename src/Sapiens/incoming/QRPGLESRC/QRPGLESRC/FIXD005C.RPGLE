      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005C - Create BADPV3.
      *****************************************************************
      *
      * DESCRIPTION:  This pgm creates file BADPV3.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV2L   IF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV2RL  IF   E           K DISK    RENAME(BADPV2R:BADPVRR) PREFIX(x)
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

     C     PV2RKY        KLIST
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
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
     C     PV2RKY        CHAIN     BADPV2RL
     C                   IF        %FOUND(BADPV2RL)
     C                   MOVE      'B'           XXTYPE
     C                   ELSE
     C                   MOVE      'P'           XXTYPE
     C                   ENDIF
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EXSR      WRTPV3
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
     C                   MOVE      'R'           XXTYPE
     C     D1CLM         LOOKUP    @CLM                                   50
     C                   IF        *IN50 = *OFF
     C                   EXSR      WRTPV3
     C                   ENDIF
     C                   READ      BADPV2RL
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
