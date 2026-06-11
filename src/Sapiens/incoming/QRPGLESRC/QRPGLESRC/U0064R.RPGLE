     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0064R - PRE/RE-CERTIFICATION EDIT PROGRAM
      * ***************************************************************
      *  PROGRAM DESCRIPTION:                                         *
      *    AFTER CHARGES ARE ADJUDICATED, IF A PRE-CERT WAS SELECTED  *
      *    AND THE LIABILITY IS 'A', BILL ENTRY WILL CALL THIS PROGRAM*
      *    TO PROCESS THE CHARGES AGAINST THE PRE-CERT RECORD.        *
      *                                                               *
      *  CALLED FROM:                                                 *
      *   F0070R                                                      *
      *                                                               *
      *  CALLS TO:                                                    *
      *   NONE                                                        *
      *                                                               *
      *  PROGRAMMER: 47                 PROJECT: 87MCB                *
      *  CREATED:  3/24/98                                            *
      * ***************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *  03/24/98 C:47   87MCB                                        *
      *    Copied U0015R & added these edits:                         *
      *    1. Edit Re-Certs as well as Pre-Certs.                     *
      *    2. Use Procedure code file (UDPOP) instead of Pre-Cert     *
      *       Master (UMPCP) to edit procedure codes.                 *
      *    3. Edit #4 - the proc code in the detail charge            *
      *       will be edited against the proc codes in the            *
      *       new procedure code file (UDPOP).                        *
      *    4. Edit #5 - the total number of visits for the particular *
      *       procedure will be compared to the total number of       *
      *       certified visits in the Proc Code file (UDPOP).         *
      *       If the from & to service dates of the Proc Code file    *
      *       are not entered, the total number of visits will be for *
      *       the life of the claim.                                  *
      *    5. If the from & to service dates have been entered in the *
      *       Proc Code file (UDPOP), the detail charge service date  *
      *       must be within that specified date range.               *
      *       The total number of visits must also be within the      *
      *       certified date range.                                   *
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      * ***************************************************************
     FUMPCL     IF   E           K DISK
     FUMPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FFDCAL7    IF   E           K DISK
     FFDCALD    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFMCML1    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFADCL     IF   E           K DISK
     FUDPOL     IF   E           K DISK
     FFDHCL     IF   E           K DISK
      *

     D QQS             S              5    DIM(50)                              DETAIL SEQ #
     D QQC             S              5    DIM(50)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  DCDC                   1     20
     D                                     DIM(4)                               DIAG CODES FADCP
     D  DCDIA1                 1      5
     D  DCDIA2                 6     10
     D  DCDIA3                11     15
     D  DCDIA4                16     20
     D*
     D                 DS
     D  PCDC                   1     25
     D                                     DIM(5)                               DIAG CODES UMPCP
     D  PCICD1                 1      5
     D  PCICD2                 6     10
     D  PCICD3                11     15
     D  PCICD4                16     20
     D  PCICD5                21     25
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     IPCA02
     I              CACPT4                      X2CPT4
     I              CACO#                       X2CO#
     I              CASUB#                      X2SUB#
     I              CAFND                       X2FND
     I              CAFYR                       X2FYR
     I              CACASE                      X2CASE
     I              CACSCD                      X2CSCD
     I              CAFDAT                      X2FDAT
     I              CATDAT                      X2TDAT
     I              CARDAT                      X2RDAT
     I              CAIPOP                      X2IPOP
     I              CAFEIN                      X2FEIN
     I              CAMOD                       X2MOD
     I              CAPRTY                      X2PRTY
     I              CATRCD                      X2TRCD
     I              CASVCD                      X2SVCD
     I              CAICD9                      X2ICD9
     I              CAANMN                      X2ANMN
     I              CAUNTS                      X2UNTS
     I              CATOTA                      X2TOTA
     I              CATOTC                      X2TOTC
     I              CATOTD                      X2TOTD
     I              CATOTP                      X2TOTP
     I              CADSEQ                      X2DSEQ
     I              CADELF                      X2DELF
     I              CAFLAG                      X2FLAG
     I              CASTAT                      X2STAT
     I              CAP1                        X2P1
     I              CAPMOD                      X2PMOD
     I              CAUSER                      X2USER
     I              CAPGM                       X2PGM
     I              CADT                        X2DT
     I              CATIME                      X2TIME
     I              CACHK#                      X2CHK#
     I              CAEOB#                      X2EOB#
     I              CAPART                      X2PART
     I              CAEOMB                      X2EOMB
     I              CARCFL                      X2RCFL
     I              CAOTHR                      X2OTHR
     I              CANDIS                      X2NDIS
     I              CARDTP                      X2RDTP
     I              CABRSV                      X2BRSV
     I              CABRPM                      X2BRPM
     I              CABRPC                      X2BRPC
     I              CABRAM                      X2BRAM
     I              CABRUN                      X2BRUN
     I              CABRTC                      X2BRTC
     I              CAZIPA                      X2ZIPA
     I              CAAST                       X2AST
     I              CADIFF                      X2DIFF
     I*
     IPPC02
     I              PCCO#                       P2CO#
     I              PCSUB#                      P2SUB#
     I              PCFND                       P2FND
     I              PCFYR                       P2FYR
     I              PCCASE                      P2CASE
     I              PCLIAB                      P2LIAB
     I              PCSTAT                      P2STAT
     I              PCADDT                      P2ADDT
     I              PCRVDT                      P2RVDT
     I              PCCERT                      P2CERT
     I              PCEXT                       P2EXT
     I              PCFEIN                      P2FEIN
     I              PCMOD                       P2MOD
     I              PCICD1                      P2ICD1
     I              PCICD2                      P2ICD2
     I              PCICD3                      P2ICD3
     I              PCICD4                      P2ICD4
     I              PCICD5                      P2ICD5
     I              PCINTR                      P2INTR
     I              PCPASG                      P2PASG
     I              PCREQD                      P2REQD
     I              PCCERD                      P2CERD
     I              PCRECD                      P2RECD
     I              PCPOS                       P2POS
     I              PC10A1                      P210A1

     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C     CAKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*   KEY LIST FOR THE DIAGNOSIS CODE WORKFILE
     C*
     C     DCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXDCS#
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE AUXILARY FILE
     C*
     C     HCKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CADSEQ
     C*
     C*   KEY LIST FOR THE PRE-CERT MASTER FILE
     C*
     C     PCKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    XXCERT
     C*
     C     PCKEY2        KLIST
     C                   KFLD                    XXCERT
     C*
     C*   KEY LIST FOR THE PROCEDURE CODE FILE
     C*
     C     POKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    PCCERT
     C                   KFLD                    PCEXT
     C*
     C     POKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    PCCERT
     C                   KFLD                    PCEXT
     C                   KFLD                    CACPT4
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM                    QQDCS#            5
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C     *LIKE         DEFINE    CAFEIN        XXFEIN
     C     *LIKE         DEFINE    CAMOD         XXMOD
     C     *LIKE         DEFINE    PCCERT        XXCERT
     C     *LIKE         DEFINE    PCEXT         XXEXT
     C     *LIKE         DEFINE    UPDT          XXYMD
     C     *LIKE         DEFINE    UPDT          XXMXDY
     C*
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C                   MOVEL     QQCERT        XXCERT
     C                   MOVE      QQEXT         XXEXT
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)                         INVALID USER PARM
     C                   GOTO      TERM
     C                   END
     C*
     C** GET THE CLAIM MASTER RECORD.
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)                         INVALID CASE #
     C                   GOTO      TERM
     C                   END
     C*
     C** DO THE EDIT CHECK FOR EACH POSSIBLE DATE ENTERED
     C*
     C                   DO        42            S                 3 0
     C                   MOVEL     QQF(S)        XXDATE            6 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     CAKEY1        CHAIN     PCA01                              54
     C*                                                                +*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C** GET THE PRE-CERT RECORD
     C*
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         XXMOD
     C                   Z-ADD     CADCS#        XXDCS#
     C     PCKEY1        SETLL     PPC01
     C     *IN60         DOUEQ     *ON
     C     PCKEY1        READE     PPC01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C** CHECK THE PLACE OF SERVICE CODE FOR A MATCH
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     PC10A1        IFNE      *BLANK                                       PCPOS NE *BLANKS
     C     PC10A1        IFNE      HCPOS
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC001'       QQC(Q)                         INVALID POS CODE
     C                   END
     C                   ENDIF
     C*
     C* TAKE ADMIT DATE AND THE RECOMMENDED DAYS = MAX DAYS
     C*
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     PCRECD        SRFUTR            5 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMXDY
     C*
     C                   Z-ADD     *ZEROS        SRFUTR
     C*
     C* IF THIS IS A RE-CERT WE NEED TO ADD ALL RECOMMENDED DAYS FROM
     C* THE PRE-CERT AND ALL RE-CERT PRIOR TO AND INCLUDING THIS ONE
     C* TO GET CORRECT MAX DAYS RANGE
     C*
     C     XXEXT         IFNE      *ZEROS
     C     PCKEY2        CHAIN     PPC02                              60
     C     *IN60         DOWEQ     '0'
     C     P2EXT         ANDLT     XXEXT
     C                   ADD       P2RECD        SRFUTR
     C     PCKEY2        READE     PPC02                                  60
     C                   ENDDO
     C*
     C*
     C                   Z-ADD     XXMXDY        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMXDY
     C*
     C                   END
     C*
     C** NO CHARGES ARE ALLOWED OUT OF THE MAX. DATE RANGE
     C*
     C     PCADDT        IFNE      *ZEROS                                       PCADDT NE *ZEROS
     C     XXYMD         IFLT      PCADDT
     C     XXYMD         ORGT      XXMXDY
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC002'       QQC(Q)                         INVALID POS CODE
     C                   END
     C                   ENDIF
     C*
     C* VERIFY THE DIAGNOSIS CODE OF THE DETAIL CHARGE IS IN THE PRE-
     C* CERT RECORD
     C*
     C     DCKEY         CHAIN     PDC01                              40
     C     *IN40         IFEQ      '0'
     C                   Z-ADD     CADXR         X                 5 0
     C                   Z-ADD     1             Y                 5 0
     C                   MOVEA     PCDC          XXPCDC           25
     C     XXPCDC        IFNE      *BLANKS                                      PCICDX NE *BLANKS
     C     DCDC(X)       LOOKUP    PCDC(Y)                                60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC003'       QQC(Q)
     C                   END                                                    END *IN60
     C                   ENDIF
     C                   END                                                    END *IN40
     C*
     C* VERIFY THAT DETAIL CHARGE PROCEDURE IS IN THE PRE-CERT RECORD
     C*
     C     CAPRTY        IFEQ      '1'
     C*
     C     POKY1         CHAIN     PPO01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC006'       QQC(Q)
     C                   END                                                    *IN60 = ON
     C*
     C**         *IN60     IFEQ *OFF                       CHAIN PPO01
     C*
     C* CHECK TO SEE IF PRE-CERT FROM AND TO SERV DATES WERE ENTERED
     C*
     C     POFRDT        IFNE      *ZEROS
     C     POTODT        ANDNE     *ZEROS
     C*
     C* IF THE PRE-CERT DATES HAVE BEEN ENTERED MAKE SURE CHARGE IS
     C* WITHIN DATE RANGE
     C*
     C     CAFDAT        IFGE      POFRDT
     C     CAFDAT        ANDLE     POTODT
     C*
     C* IF CHARGE IS WITHIN PRE-CERT DATES READ DETAIL CHARGE FILE FOR
     C* ANY OTHER CHARGES FOR PROVIDER WITHIN PRE-CERT DATES AND MAKE
     C* SURE THAT TOTAL IS NOT GREATER THAN TOTAL # OF VISITS FROM
     C* PRE-CERT FILE
     C*
     C                   Z-ADD     *ZEROS        XX#VST            5 0
     C     CAKEY2        CHAIN     PCA02                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
     C*
     C     CACPT4        IFEQ      X2CPT4
     C*
     C* NOW WE MUST MAKE SURE CHARGE IS WITHIN PRE-CERT FROM AND TO DATES
     C*
     C     X2FDAT        IFGE      POFRDT
     C     X2FDAT        ANDLE     POTODT
     C                   ADD       1             XX#VST
     C                   END                                                    END X2FDAT
     C                   END                                                    END CACPT4
     C     CAKEY2        READE     PCA02                                  50
     C                   END                                                    END *IN50
     C*
     C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
     C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
     C* THE PRE/RE-CERT # OF VISITS
     C*
     C     POVST         IFNE      *ZEROS                                       PCA#VX NE *ZEROS
     C     XX#VST        IFGT      POVST
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC004'       QQC(Q)
     C                   END                                                    END XX#VST
     C                   ENDIF
     C*
     C                   ELSE                                                   ELSE CAFDAT
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC005'       QQC(Q)
     C                   END                                                    END CAFDAT
     C*
     C                   ELSE                                                   ELSE SP/EP
     C*
     C* IF THE FROM AND TO DATES IN PRE-CERT RECORD ARE ZEROS ALLOW
     C* ONLY THE TOTAL NUMBER OF VISITS FOUND IN THE PRE-CERT RECORD
     C* FOR THE LIFE OF THE CLAIM
     C*
     C                   Z-ADD     *ZEROS        XX#VST            5 0
     C     CAKEY2        CHAIN     PCA02                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
     C*
     C     CACPT4        IFEQ      X2CPT4
     C                   ADD       1             XX#VST
     C                   END                                                    END CACPT4
     C     CAKEY2        READE     PCA02                                  50
     C                   END                                                    END *IN50
     C*
     C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
     C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
     C* THE PRE/RE-CERT # OF VISITS
     C*
     C     POVST         IFNE      *ZEROS                                       PCA#VX NE *ZEROS
     C     XX#VST        IFGT      POVST
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC004'       QQC(Q)
     C                   END                                                    END XX#VST
     C                   ENDIF
     C*
     C                   END                                                    END SP/EP
     C*
     C**                   END                             END CHAIN PPO01
     C                   END                                                    END CAPRTY=1
     C*
     C                   END                                                    END PPC01
     C                   ENDDO                                                  ENDDO PPC01
     C*
     C     CAKEY1        READE     PCA01                                  54
     C                   ENDDO                                                  ENDDOW EQ 54
     C                   END                                                    END QQF,S
     C                   ENDDO                                                  ENDDO 42
     C* ----------------------------------------------------------------
     C* END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C*
