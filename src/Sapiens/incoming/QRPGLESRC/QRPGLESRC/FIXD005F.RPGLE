      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005F - Identify records to be omitted.
      *****************************************************************
      *
      * DESCRIPTION:  This pgm identifies records to be omitted.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2L   UF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV2RL  UF   E           K DISK    RENAME(BADPV2R:BADPVRR) PREFIX(x)

     C     RPKY          KLIST
     C                   KFLD                    xPAFND
     C                   KFLD                    xPAFYR
     C                   KFLD                    xPACASE

     C     PAKY          KLIST
     C                   KFLD                    xRPFND
     C                   KFLD                    xRPFYR
     C                   KFLD                    xRPCASE

     C                   READ      BADPV2L
     C                   DOW       NOT %EOF(BADPV2L)
     C                   IF        xPARQST = 'DONTFIX'
     C                   EXSR      UPDREQ
     C                   ENDIF
     C                   READ      BADPV2L
     C                   ENDDO

     C     *LOVAL        SETLL     BADPV2L
     C     *LOVAL        SETLL     BADPV2RL

     C                   READ      BADPV2RL
     C                   DOW       NOT %EOF(BADPV2RL)
     C                   IF        xRPRQST = 'DONTFIX'
     C                   EXSR      UPDPAY
     C                   ENDIF
     C                   READ      BADPV2RL
     C                   ENDDO

     C                   EVAL      *INLR =  *ON
     C**********************************************************************
     C* UPDATE PAYMENT REQUEST
     C**********************************************************************
     C     UPDREQ        BEGSR
     C     RPKY          SETLL     BADPV2RL
     C     RPKY          READE     BADPV2RL
     C                   DOW       NOT %EOF(BADPV2RL)
     C                   MOVEL(P)  'DONTFIX'     xRPRQST
     C                   UPDATE    BADPVRR
     C     RPKY          READE     BADPV2RL
     C                   ENDDO
     C                   ENDSR
     C**********************************************************************
     C* UPDATE PAYMENTS
     C**********************************************************************
     C     UPDPAY        BEGSR
     C     PAKY          SETLL     BADPV2L
     C     PAKY          READE     BADPV2L
     C                   DOW       NOT %EOF(BADPV2L)
     C                   MOVEL(P)  'DONTFIX'     xPARQST
     C                   UPDATE    BADPVR
     C     PAKY          READE     BADPV2L
     C                   ENDDO
     C                   ENDSR
