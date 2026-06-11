      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006I - Update missing payment and payment request data/
      *****************************************************************
      *
      * DESCRIPTION:  This pgm populates the missing data in the
      *               payment and payment request files.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV2L   IF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV2RL  IF   E           K DISK    RENAME(BADPV2R:BADPVRR) PREFIX(x)
     FWDPAL     UF   E           K DISK
     FWARPL     UF   E           K DISK

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
     C                   IF        xPARQST <> 'DONTFIX'
     C                   MOVE      xPAFND        XXFND
     C                   MOVE      xPAFYR        XXFYR
     C                   MOVE      xPACASE       XXCASE
     C                   EXSR      UPDPAY
     C                   ENDIF
     C                   READ      BADPV2L
     C                   ENDDO

      *
      * Extract fixable claim number for payment requests
      *
     C     *LOVAL        SETLL     BADPV2RL
     C                   READ      BADPV2RL

     C                   DOW       NOT %EOF(BADPV2RL)
     C                   IF        xPARQST <> 'DONTFIX'
     C                   MOVE      xRPFND        XXFND
     C                   MOVE      xRPFYR        XXFYR
     C                   MOVE      xRPCASE       XXCASE
     C                   EXSR      UPDREQ
     C                   ENDIF
     C                   READ      BADPV2RL
     C                   ENDDO

     C                   MOVE      *ON           *INLR
     C**********************************************************************
     C* UPDATE PAYMENT DATA
     C**********************************************************************
     C     UPDPAY        BEGSR
     C     PAKY          CHAIN     WDPAL
     C                   IF        %FOUND(WDPAL)
     C                   IF        PABR2 = *BLANKS or
     C                             (PABR2 <> *BLANKS and patrcd = *blanks and
     C                              patype = *blanks and parecd = *blanks and
     C                              paretp = *blanks)
     C                   MOVE      xPABR2        PABR2
     C                   MOVE      xPATRCD       PATRCD
     C                   MOVE      xPATYPE       PATYPE
     C                   MOVE      xPARECD       PARECD
     C                   MOVE      xPARETP       PARETP
     C                   MOVE      xPADESC       PADESC
     C                   UPDATE    PPA01
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C**********************************************************************
     C* UPDATE PAYMENT REQUEST DATA
     C**********************************************************************
     C     UPDREQ        BEGSR
     C     RPKY          CHAIN     WARPL
     C                   IF        %FOUND(WARPL)
     C                   IF        RPBR2 = *BLANKS or
     C                             (RPBR2 <> *BLANKS and rptrcd = *blanks and
     C                              rptype = *blanks and rprecd = *blanks and
     C                              rpretp = *blanks)
     C                   MOVE      xRPBR2        RPBR2
     C                   MOVE      xRPTRCD       RPTRCD
     C                   MOVE      xRPTYPE       RPTYPE
     C                   MOVE      xRPRECD       RPRECD
     C                   MOVE      xRPRETP       RPRETP
     C                   MOVE      xRPDESC       RPDESC
     C                   UPDATE    PRP01
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
