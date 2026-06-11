     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W3060R - UPDATE AGT COMMISSION & CHECK TRANSACTION FILES
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/03/15 C:136 Project: MC396                                 *
     H*                Populate next available EFT sequence number    *
     H*                to check # field for EFT                       *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Load R2eftc to ACeftc, like R2cktp             *
     H*                                                               *
     H*  06/02/08 C:191 PROJECT: 132WKE                               *
     H*                 BUSINESS OBJ LINK TO CARRIER POL#             *
     H*                                                               *
     H*  09/20/07 C:136 PROJECT: WC997                                *
     H*                 IMPLEMENT LOGIC FOR EFT - AGENT COMMISSION    *
     H*                 CHECKS                                        *
     H*                                                               *
     H*  12/21/06 C:183  PROJECT: 114WND                              *
     H*                  Add a record to WMPHP.                       *
     H*                                                               *
     H*  11/01/05 C:33  Project: 109WDZ   WA109/49597                 *
     H*                 Be sure to load DSDTTM so that it doesn't     *
     H*                  have blanks.                                 *
     H*                                                               *
     H*  12/02/97 C:30 Project:  WC500                                *
     H*           Change DATE PARM from 6 to 8 chars.                 *
     H*           Update WSCKP with "Last Used" WAR2P Check# + 1      *
     H*****************************************************************
     FWAR2L     UF   E           K DISK
     FWAC2L     UF   E           K DISK
     FWMACL7    UF A E           K DISK
      *________________________________________________________________
      *
      *  Bank Account Master
     FWSCKL     UF   E           K DISK
     FWMACL21   UF   E           K DISK
     F                                     RENAME(PAC01:PAC21)
     FWMPHP     O    E             DISK
      *________________________________________________________________
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *** beg add ***** 06/03/15 *************************
     D/copy WPRBANK
      *** end add ***** 06/03/15 *************************
     C     *LIKE         DEFINE    ACPODT        XXDT
     C     *LIKE         DEFINE    R2FND         SVFND
     C     *LIKE         DEFINE    R2ACCT        SVACCT
     C     *LIKE         DEFINE    R2CHCK        SVCHK#
     C     *LIKE         DEFINE    ACSEQ#        SVSEQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
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
      *
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C*
     C*  REQUEST COMMISSION PAYMENT KEY LIST
     C*
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AGENT COMMISSION FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    R2BUL2
     C*
     C*  CHECK SCHEDULE FILE
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
      *
      *
      *  Bank Account Master File
      *
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVACCT
      *
     C     ACKY2         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
     C                   KFLD                    R2CHCK
     C*
     C     ACKY3         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZERO         SVFND                          Save Fund
     C                   MOVE      *BLANK        SVACCT                         Save Account
     C                   Z-ADD     *ZERO         SVCHK#                         Save Check#
     C*
     C*  CHAIN TO THE CHECK SCHEDULE FILE, WE MUST HAVE A RECORD
     C*  FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     C2KEY         CHAIN     PC201                              60
     C     *IN60         IFEQ      '0'
     C                   CLEAR                   PPH01
     C                   MOVE      QQCO#         PHCO#
     C                   MOVE      QQFND         PHFND
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '     '       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHDT
      *
     C                   MOVE      DSTIME        PHTIME
     C                   MOVE      Q1USER        PHUSER
     C                   MOVE      Q1PGM         PHPGM
     C                   Z-ADD     Q1DT          SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHIDT
      *
     C                   MOVE      Q1TIME        PHITIM
     C                   MOVE      Q1USER        PHIUSR
     C                   MOVE      Q1PGM         PHIPGM
     C                   MOVE      'P'           PHTYPE
     C                   MOVE      QQDATE        PHFRDT
     C                   MOVE      QQFYR         PHFYR
     C                   MOVE      C2CHK#        PHCHK#
     C                   WRITE     PPH01
     C*
     C*  THE CHECK PROCESSING FLAG MUST BE BLANK
     C*
     C     C2STA1        IFEQ      ' '
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
     C     R2KEY         CHAIN     PR201                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  MOVE THE TRANSACTION RECORD TO THE AGENT COMMISSION RECORD
     C*
     C     ACKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '1'
     C     R2AMT         ANDNE     *ZERO
     C*
     C* Before writing payment record to agent commission file
     C*   retrieve greatest seq# for CO#/FND/AGT#/CHCK
     C                   Z-ADD     0             SVSEQ#
     C     ACKY2         SETGT     PAC21
     C     ACKY2         READPE    PAC21                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ACSEQ#        SVSEQ#
     C                   ENDIF
     C                   MOVE      R2CO#         ACCO#
     C                   MOVE      R2FND         ACFND
     C                   MOVE      XXFYR         ACFYR
     C                   MOVE      *ZERO         ACEMP#
     C                   MOVE      *ZERO         ACDIV
     C                   MOVE      R2AGT#        ACAGT#
     C                   MOVE      *ZERO         ACPROD
     C                   MOVE      XXDT          ACPODT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACTRDT
     C*
     C                   MOVE      *ZERO         ACTOCL
     C                   MOVE      *ZERO         ACRATE
     C                   MOVE      R2AMT         ACAMT
      *** beg chg ***** 06/03/15 *************************************
     C***********        MOVE      R2CHCK        ACCHCK
      /free
       If R2eftc = 'E';
          ACchck = GetNextEFT#(R2co#:R2acct);
       Else;
          ACchck = R2chck;
       Endif;
      /end-free
      *** end chg ***** 06/03/15 *************************************
     C                   MOVE      R2COCD        ACCOCD
     C                   MOVE      R2COTP        ACCOTP
     C                   MOVE      R2DESC        ACDESC
     C                   MOVE      R2CKTP        ACCKTP
      *******Beg Add********04/17/12**********************************
      /Free
       ACeftc = R2eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVE      R2ACCT        ACACCT
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVE      R2USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C*
     C                   Z-ADD     DSTIME        ACTIME
     C                   Z-ADD     R2BUL2        ACBUL2
     C                   MOVE      Q1USER        ACIUSR
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     ACDT          ACIDT
     C                   Z-ADD     ACTIME        ACITM
     C*
     C                   WRITE     PAC01
     C*
     C* Read all records from WMACP for CO#/FND/AGT#
     C*   Update records where ACCOTP = 'C' and ACCCHK = *BLANKS
     C     ACKY3         CHAIN     PAC21                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ACCOTP        IFEQ      'C'
     C     ACCCHK        ANDEQ     *BLANK
     C                   Z-ADD     R2CHCK        ACCHCK
     C                   Z-ADD     R2AMT         ACCKAM
     C                   ADD       1             SVSEQ#
     C                   Z-ADD     SVSEQ#        ACSEQ#
     C                   MOVE      'Y'           ACCCHK
     C*
     C                   MOVE      R2USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   Z-ADD     DSTIME        ACTIME
     C*
     C                   Z-ADD     ACDT          ACCKDT
     C*
     C                   UPDATE    PAC21
     C                   ENDIF                                                  ACCOTP = 'C'
     C*
     C     ACKY3         READE     PAC21                                  60
     C                   ENDDO
     C                   END
      *
      *
      *  When Fund breaks occur, update the Save Fund Bank Account Master with
      *  the "Last" Check# + 1 (Starting Check#).
      *
     C     SVFND         IFNE      R2FND
     C     SVFND         ANDNE     *ZERO
      *
     C                   EXSR      CHECK#
      *
     C                   ELSE
      *
     C                   Z-ADD     R2FND         SVFND                          Save Fund
      *
      * Only save the Check Nbr and Bank Account if trans is not a pre-note
      *
     C     R2CHCK        IFGT      *ZEROS
     C                   MOVEL     R2ACCT        SVACCT                         Save Account
     C                   Z-ADD     R2CHCK        SVCHK#                         Save Check#
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*
     C*  UPDATE THE TRANSACTION AS PROCESSED
     C*
     C                   MOVE      'Y'           R2STAT
     C                   UPDATE    PR201
     C*
     C     R2KEY         READE     PR201                                  60
     C                   END
     C*
     C                   END
     C*
     C*  UPDATE THIS RECORD AS PROCESSED
     C*
     C                   MOVE      'Y'           C2STA1
     C                   UPDATE    PC201
      *
      *
      *  Update the Last Fund's Bank Account Master with
      *  the "Last" Check# + 1 (Starting Check#).
      *
     C                   EXSR      CHECK#
      *
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
      *
      **************************************************************************
      *  CHECK# - Access/update Bank Account Master with "New" Starting Check#
      **************************************************************************
     C     CHECK#        BEGSR
      *
     C     CKKY          CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *OFF
      * Only increment the Check Number if saved Check Number > CKLCKH
     C     SVCHK#        ANDGE     CKLCHK
      *
     C     SVCHK#        ADD       1             CKLCHK                         Starting Check#
     C                   MOVE      Q1USER        CKUSER
     C                   MOVE      Q1PGM         CKPGM
     C                   Z-ADD     DSTIME        CKTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CKDT
      *
     C                   UPDATE    PCK01
      *
     C                   ENDIF
      *
     C                   ENDSR
