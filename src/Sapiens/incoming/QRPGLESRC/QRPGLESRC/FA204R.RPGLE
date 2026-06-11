     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA204R - VERIFIY EOB CODE FOR CHARGE NOT EQUAL PAYMENT   DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA204R                                            *
     H*    CREATED: 12/08/99  C:56  Project: MC152                    *
     H*    DESCRIPTION: THIS API WILL CHECK TO SEE IF THE LINE        *
     H*                 ITEM HAS AN EOB CODE ASSIGNED IF THE PAID     *
     H*                 AMOUNT DOES NOT EQUAL THE CHARGE AMOUNT.      *
     H*                 RETURN 'F' IF ABOVE NOT SATIFIED.             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  07/08/04 C:79  SUPPORT: MC025 43092                          *
     H*                 1. Apply edit if Paid < Charged only          *
     H*                 2. Check for inactive codes                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     F* EOMB Transaction File
     FFAEOP     IF   E             DISK
     F*
     F* EOMB Detail  EOB/Notes Transaction File
     FFDDEL     IF   E           K DISK

     C*
     C* Define Program Variables
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    DEDSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEDSEQ        @@DSEQ
     C*
     C* Key list for EOB/Notes Transaction File
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
     C                   PARM                    @@RRN1            9 0          faeop
     C                   PARM                    @@DSEQ                         from fdcap
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
     C                   Z-ADD     @@DSEQ        KKDSEQ
     C                   SETON                                        50
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
     C                   SETOFF                                       50        eob code found
     C                   LEAVE
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C*
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     *IN50         IFEQ      *ON                                          no eob code found
     C                   MOVE      'F'           @@RTN
     C                   ENDIF                                                  *INFO = *ON
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*  End of program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
