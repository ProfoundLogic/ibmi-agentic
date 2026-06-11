     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA205R - VERIFIY EOB CODE FOR CHARGE NOT EQUAL PAYMENT   DATA
     H*                FOR ENTIRE BILL
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA205R                                            *
     H*    CREATED: 12/08/99  C:56  Project: MC152                    *
     H*    DESCRIPTION: THIS API WILL CHECK TO SEE IF EACH LINE       *
     H*                 ITEM HAS AN EOB CODE ASSIGNED IF THE PAID     *
     H*                 AMOUNT IS LESS THAN THE CHARGE AMOUNT.        *
     H*                 RETURN 'F' IF ANY LINE ITEM DOES NOT SATISFY  *
     H*                 THE ABOVE                                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  07/08/04 C:79  SUPPORT: MC025 43092                          *
     H*                 1. Apply edit if Paid < Charged only          *
     H*                 2. Check for inactive codes                   *
     H*                                                               *
     H*  04/05/01 C:79  SUPPORT: MC025 25456                          *
     H*                 1. Do not include if CADELF='X'.  This is     *
     H*                    system generated codes, i.e. FSMRA         *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     F* Managed Care Detail Charge File
     FFDCAL13   IF   E           K DISK
     F*
     F* EOMB Transaction File
     FFAEOP     IF   E             DISK
     F*
     F* EOMB Detail EOB/Notes Transaction File
     FFDDEL     IF   E           K DISK

     C*
     C*  Define Program Variables
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*
     C* Key list for Detail Charge File
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Detail EOB/Notes Transaction File
     C*
     C     DEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTN             1
     C                   PARM                    @@RRN1            9 0
     C*
     C* Get bill and set key fields
     C*
     C     @@RRN1        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 04/05/01 ****************************************
     C* Skip system generated codes, i.e. FSMRA
     C     CADELF        IFEQ      'X'
     C                   GOTO      NXTRCD
     C                   ENDIF
     C*** END ADD *** 04/05/01 ****************************************
     C*
     C*** BEG CHG *** 07/08/04 ****************************************
     C********** CACHRG    IFNE CAPAID
     C     CAPAID        IFLT      CACHRG
     C*** END CHG *** 07/08/04 ****************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   MOVE      *BLANKS       EOBFND            1
     C     DEKY          CHAIN     PDE01                              98
     C     *IN98         DOWEQ     *OFF
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*** BEG ADD *** 07/08/04 ****************************************
     C     DESTAT        ANDNE     'I'
     C*** END ADD *** 07/08/04 ****************************************
     C                   MOVE      'S'           EOBFND
     C                   LEAVE
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C*
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO
     C*
     C     EOBFND        IFEQ      *BLANKS
     C                   MOVE      'F'           @@RTN
     C                   LEAVE
     C                   ENDIF                                                  EOFND = '1'
     C*
     C                   ENDIF                                                  CACHRG NE CAPAID
     C*
     C*** BEG ADD *** 04/05/01 ****************************************
     C     NXTRCD        TAG
     C*** END ADD *** 04/05/01 ****************************************
     C*
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C*  End of program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
