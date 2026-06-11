     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *TITLE W6560R - RETURN FUND CHECK PRINTING PROGRAM
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  07/18/07 C:136 PROJECT: WC1491                               *
      *                 IMPLEMENT IND CASH AND RETURN DEPOSIT         *
      *                                                               *
      *  05/24/05  C:33  Project: WC1033                              *
      *                  Include calculations for Agency Billing.     *
      *                                                               *
      *  04/14/03 C:136 Project:  WC1033                              *
      *           RETURN PREMIUM CHECK PROGRAM                        *
      *                                                               *
      *****************************************************************
      * CHECK REQUEST FILE (CO/FND/CKTP)
     FWAR4LA    UF   E           K DISK
      * CHECK SCHEDULE FILE (CO/PRDT/FND/CKTP)
     FWAC6L3    UF   E           K DISK
      * TRANSACTION CODE TABLE (CO/ARCD)
     FWTABL     IF   E           K DISK
     F***  Begin Add  ***  05/24/05  *************************************
     FWMAAL     IF   E           K DISK
     F***  End   Add  ***  05/24/05  *************************************
      * A/R FILE
     FWMEAP     O    E             DISK
     F***  Begin Add  ***  05/24/05  *************************************
     FWDABP     O    E             DISK
     F                                     RENAME(PAB01:PAB0P)
     FWDABL     IF   E           K DISK
     F                                     RENAME(PAB01:PAB00)
     FWSASL     UF   E           K DISK
     F                                     RENAME(PAS01:PAS00)
     F***  End   Add  ***  05/24/05  *************************************
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *

     C     *LIKE         DEFINE    R4DATE        KKCKDT
     C     *LIKE         DEFINE    R4FND         SVFND
     C     *LIKE         DEFINE    R4ACCT        SVACCT
     C     *LIKE         DEFINE    R4CHK#        SVCHK#
     C     *LIKE         DEFINE    TTKEY         KKKEY
      *
      *  CHECK SCHEDULE FILE
      *
     C     C6KEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCKDT
      *
      *  REQUEST COMMISSION PAYMENT KEY LIST
      *
     C     R4KEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    C6FND
     C                   KFLD                    C6CKTP
      *
      *  REQUEST COMMISSION PAYMENT KEY LIST
      *
     C     TTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C***  Begin Add  ***  05/24/05  *************************************
     C     AAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    ABFEIN
     C                   KFLD                    ABMOD
     C***  End   Add  ***  05/24/05  *************************************
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
      *
      *  CHAIN TO THE CHECK SCHEDULE FILE, WE MUST HAVE A RECORD
      *  FOR THIS DATE
      *
     C                   MOVE      QQCO#         KKCO#             3 0
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKCKDT
      *
     C     C6KEY         CHAIN     PC601                              60
     C     *IN60         DOWEQ     *OFF
      *
      *  THE CHECK PROCESSING FLAG MUST BE BLANK
      *
     C     C6STA1        IFEQ      ' '
      *
      *  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
      *
     C     R4KEY         CHAIN     PR401                              60
     C     *IN60         DOWEQ     *OFF
      *
      *  UPDATE THE TRANSACTION AS PROCESSED
      *
     C                   Z-ADD     KKCKDT        R4CKDT
     C                   MOVE      'Y'           R4STAT
     C                   UPDATE    PR401
      *
     C***  Begin Add  ***  05/24/05  *************************************
     C                   SELECT
     C*
     C*** BEG ADD ***** 07/18/07 *****************************************
     C     R4BTYP        WHENEQ    'I'
     C*** END ADD ***** 07/18/07 *****************************************
     C*__________
     C*
     C     R4ACFL        WHENEQ    'N'
     C***  End   Add  ***  05/24/05  *************************************
      *  WRITE TO THE A/R FILE
      *
     C                   CLEAR                   PEA01
     C                   MOVE      R4CO#         EACO#
     C                   MOVE      R4FND         EAFND
     C                   MOVE      R4EMP#        EAEMP#
     C                   MOVE      R4DIV         EADIV
     C                   MOVE      R4FYR         EAFYR
     C                   MOVE      R4AMT         EAAMT
     C                   MOVE      R4CHK#        EACHK#
     C                   MOVE      R4ARCD        EAARCD
     C                   MOVEL(P)  R4ARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      R4CKDT        EAPODT
     C                   TIME                    DSDTTM
     C                   MOVE(P)   DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      EATRDT        EADT
     C                   MOVE      DSTIME        EATIME
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   WRITE     PEA01
     C***  Begin Add  ***  05/24/05  *************************************
     C*__________
     C*
     C     R4ACFL        WHENEQ    'Y'
      *  Write to the Agency Billing File
      *
     C                   CLEAR                   PAB0P
     C                   MOVE      R4CO#         ABCO#
     C*
     C                   MOVE      R4FEIN        ABFEIN
     C                   MOVE      R4MOD         ABMOD
     C                   CLEAR                   AAAGT#
     C     AAKY          CHAIN     PAA01                              60
     C                   MOVE      AAAGT#        ABAGT#
     C*
     C                   MOVE      R4STDT        ABSTDT
     C                   MOVE      R4FND         ABFND
     C                   MOVE      R4EMP#        ABEMP#
     C                   MOVE      R4DIV         ABDIV
     C                   MOVE      R4FYR         ABFYR
     C                   MOVE      R4AMT         ABAMT
     C                   MOVE      R4CHK#        ABCHK#
     C*
     C                   MOVE      R4ARCD        ABARCD
     C                   MOVEL(P)  R4ARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        ABARTY
     C                   MOVEL     TTDESC        ABDESC
     C*
     C                   MOVE      R4CKDT        ABPODT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE(P)   DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABTRDT
     C*
     C                   MOVE      ABTRDT        ABDT
     C                   MOVE      DSTIME        ABTIME
     C                   MOVEL     Q1USER        ABUSER
     C                   MOVEL     Q1PGM         ABPGM
     C                   MOVE      ABTRDT        ABIDT
     C                   MOVE      DSTIME        ABITIM
     C                   MOVEL     Q1USER        ABIUSR
     C                   MOVEL     Q1PGM         ABIPGM
     C*
     C                   WRITE     PAB0P
     C*
     C                   EXSR      UPDTAS
     C*_________
     C*
     C                   ENDSL
     C***  End   Add  ***  05/24/05  *************************************
      *
     C     R4KEY         READE     PR401                                  60
     C                   END
      *
     C                   END
      *
      *  DELETE THE CHECK SCHEDULE RECORD
      *
     C                   DELETE    PC601
      *
     C     C6KEY         READE     PC601                                  60
     C                   END
      *
     C                   MOVE      *ON           *INLR
     C*
     C***  Begin Add  ***  05/24/05  *************************************
     C*===================================================================
     C* UPDTAS - Update WSASP record
     C*===================================================================
     C*
     C     UPDTAS        BEGSR
     C*
     C     ABKY          KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABSTDT
     C                   KFLD                    ABAGT#
     C*
     C     ASKY          KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABSTDT
     C                   KFLD                    ABAGT#
     C*
     C     *LIKE         DEFINE    ASBILL        XXBILL
     C     *LIKE         DEFINE    ASPAID        XXPAID
     C*
     C                   CLEAR                   XXBILL
     C                   CLEAR                   XXPAID
     C*
     C     ABKY          CHAIN     PAB00                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   SELECT
     C     ABARTY        WHENEQ    'A'
     C     ABARTY        OREQ      'B'
     C     ABARTY        OREQ      'D'
     C     ABARTY        OREQ      'F'
     C     ABARTY        OREQ      'H'
     C     ABARTY        OREQ      'N'
     C                   ADD       ABNET         XXBILL
     C     ABARTY        WHENEQ    'P'
     C     ABARTY        OREQ      'I'
     C     ABARTY        OREQ      'Q'
     C     ABARTY        OREQ      'T'
     C                   ADD       ABAMT         XXPAID
     C     ABARTY        WHENEQ    'R'
     C     ABARTY        OREQ      'V'
     C                   SUB       ABAMT         XXPAID
     C                   ENDSL
     C*
     C     ABKY          READE     PAB00                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     ASKY          CHAIN     PAS00                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     XXBILL        ASBILL
     C                   Z-ADD     XXPAID        ASPAID
     C     ASBILL        SUB       ASPAID        ASBAL
     C*
     C                   MOVEL     Q1PGM         ASPGM
     C                   MOVEL     Q1USER        ASUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ASDT
     C*
     C                   Z-ADD     DSTIME        ASTIME
     C*
     C                   UPDATE    PAS00
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDTAS
     C***  End   Add  ***  05/24/05  *************************************
