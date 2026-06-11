     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F0156R - RECONSIDERATION/POST-AUDIT PROCESSING
     H**************************************************************************
     H* PROGRAM DESCRIPTION:                                                   *
     H* RESET EOMB AND DETAIL CHARGES SO RECON OR POST AUDIT MAY BE PROCESSED  *
     H*                                                                        *
     H* CREATED 07/28/95 C:20   REQ/PRJ: 25MAE                                 *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*
     H*  03/09/99  C:94  PROJECT: MC152
     H*                  CHANGE FDCAL2 TO FDCALL AND CHANGE KEYLIST
     H*
     H**************************************************************************
     FFMC1L     IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     FFAE1L     IF   E           K DISK
     F*** BEG CHG *** 03/09/99 **************************************
     FFDCALL    UF   E           K DISK
     F**********FDCAL2  UF  E           K        DISK
     F*** END CHG *** 03/09/99 **************************************
     FFADUP     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* INITIALIZE SECTION
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    CASS#         XXSS#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C********** *LIKE     DEFN CATRCD    XXTRCD           DEL 03/09/99
     C     *LIKE         DEFINE    CACSCD        XXCSCD
     C*
     C                   MOVE      *BLANKS       XXAR              1
     C                   Z-ADD     *ZEROS        X                 5 0
     C*
     C* KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* KEY LIST FOR THE EOB WORK FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C* KEY LIST FOR THE EOB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* KEY LIST FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C**********           KFLD           XXTRCD           DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C* KEY LIST FOR THE POSSIBLE DUPLICATE FILE
     C*
     C     DUKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQAR              1
     C                   PARM                    QQAUDF            2
     C                   PARM                    QQTOB             3
     C                   PARM                    QQDHR             2
     C                   PARM                    QQPMTH            1
     C                   PARM                    QQTOS             3
     C                   PARM                    QQPRC1            5
     C                   PARM                    QQPRC2            5
     C                   PARM                    QQPRC3            5
     C                   PARM                    QQPRC4            5
     C                   PARM                    QQADDT            8
     C                   PARM                    QQCFRM            8
     C                   PARM                    QQCTO             8
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQSS#         XXSS#
     C                   MOVE      QQACDT        X1ACDT            8 0
     C                   Z-ADD     X1ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C                   MOVE      QQEOB#        XXEOB#
     C                   TIME                    DSDTTM
     C*
     C* GET THE COMPANY AUXILIARY FILE
     C*
     C                   MOVE      *BLANKS       XXCSCD
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C     QQAR          IFEQ      'R'
     C                   MOVEL     C1CSC1        XXCSCD
     C                   ELSE
     C                   MOVEL     C1CSC3        XXCSCD
     C                   END
     C                   END
     C*
     C* READ EOMB FILE
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           QQRTCD                         NO RECORD
     C                   GOTO      TERM
     C                   END
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO GET # OF AUDITS
     C*
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     9             X
     C     1             SUBST     E120A1:X      XXAR
     C     XXAR          IFEQ      'A'
     C                   Z-ADD     10            X
     C     2             SUBST     E120A1:X      QQAUDF
     C                   ELSE                                                   ELSE XXAR EQ 'A'
     C                   MOVE      *ZEROS        QQAUDF
     C                   END                                                    END XXAR EQ 'A'
     C*
     C* RETRIEVE VALUES FROM EOMB AUX RECORD TO RETURN TO BILL ENTRY
     C*
     C                   Z-ADD     15            X
     C     3             SUBST     E120A1:X      QQTOB                          TYPE OF BILL
     C                   Z-ADD     18            X
     C     2             SUBST     E120A1:X      QQDHR                          DISCHARGE HOUR
     C                   Z-ADD     20            X
     C     1             SUBST     E120A1:X      QQPMTH                         PROC CODE METHOS
     C                   Z-ADD     7             X
     C     3             SUBST     E120A2:X      QQTOS                          HOS TYPE OF SERV
     C                   MOVEL     E17A01        QQPRC1
     C                   MOVEL     E17A02        QQPRC2
     C                   MOVEL     E17A03        QQPRC3
     C                   MOVEL     E17A04        QQPRC4
     C                   Z-ADD     E1DT6         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQADDT                         ADDMISSION DATE
     C                   Z-ADD     E1DT7         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQCFRM                         COV FROM DATE
     C                   Z-ADD     E1DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQCTO                          COV TO DATE
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* ZERO OUT CHK FIELD IN CASE DETAIL FILE
     C* UPDATE WITH NEW CLAIM STATUS CODE (FROM COMP AUXILIARY FILE)
     C*
     C**********           MOVE *BLANKS   XXTRCD           DEL 03/09/99
     C     CAKEY         CHAIN     PCA01                              70
     C     *IN70         DOWEQ     '0'
     C*
     C* BLANK PRINTED FLAG IN POSSIBLE DUPLICATE FILE
     C*
     C     DUKEY         CHAIN     PDU01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVE      *BLANK        DUFLG1
     C                   UPDATE    PDU01
     C                   END                                                    ENDIF *IN81
     C*
     C                   Z-ADD     *ZERO         CACHK#
     C                   MOVEL     XXCSCD        CACSCD
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C                   UPDATE    PCA01
     C     CAKEY         READE     PCA01                                  70
     C                   END                                                     END DOW 70
     C*
     C* BLANK PRINTED FLAG/CHECK#  IN EOB FILE
     C*
     C                   MOVE      *BLANKS       EOSTAT
     C                   Z-ADD     *ZEROS        EOCHK#
     C                   UPDATE    PEO01
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
