     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX PROGRAM - FIXWC1182
     H*****************************************************************
     H* Description:  THIS PROGRAM WILL LOAD ELGOVC FIELD.            *
     H*                                                               *
     H*   PGMR: C:65                                                  *
     H*   SUPP: 42021                                                 *
     H*   DATE: 4/20/04                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/03/05 C:65  PROJECT: SUPPORT#45632                         *
     H*                CHANGE TO USE AUDITED PAYROLL IF THE ACCOUNT   *
     H*                IS AUDITED.  CHANGE CHECK ON PAYROLL TOTAL TO  *
     H*                GREATER THAN OR EQUAL TO INCLUDE 0 PAYROLL     *
     H*                CLASSES WHICH ARE VALID.  ALSO ONLY RUN IF     *
     H*                POLICY HAS ALREADY BEEN BILLED TO UPDATE.      *
     H*                                                               *
     H* 10/26/04 C:65  PROJECT: SUPPORT/BASE CD                       *
     H*                POPULATE XXEFDT TO ENSURE PROPER RATE CHAIN.   *
     H*                                                               *
     H* 06/17/04 C:65  PROJECT: SUPPORT/BASE CD                       *
     H*                ADD IN EXTRA PROGRAMMING TO HELP AVOID LOOPING *
     H*                ON INVALID/BAD DATA (DUPLICATE WDELP RECORDS). *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMEPL1    IF   E           K DISK
     FWMPRL1    IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D XPARA           DS                  OCCURS(255)
     D  XPCLAS                 1      5  0
     D  XPPAY                  6     20  0
     D  XPFLAG                21     21

     I*

     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    EPEFDT        XXEFDT
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  PAYROLL FILE
     C*
     C     EPKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C                   TIME                    DSDTTM
     C*
     C*  Start main loop
     C*
     C                   READ      PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C*
     C*** BEGIN ADD ***  03/03/05  ************************************
     C********   ELGOVC    IFEQ *ZERO
     C     ELFAF         IFNE      *BLANK
     C*** END ADD *****  03/03/05  ************************************
     C*
     C                   Z-ADD     0             XXGOVC            5 0
     C                   MOVE      *BLANK        XXGOVF            1
     C                   Z-ADD     0             XXGOVP           15 0
     C*
     C* SET THE GOVERNING CLASS FIELD ON STATE RPT DIVISION ONLY.
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDEQ     'Y'
     C*
     C     1             DO        255           Z                 4 0
     C     Z             OCCUR     XPARA
     C                   CLEAR                   XPARA
     C                   ENDDO
     C*
     C* CHECK ALL PAYROLL RECORDS FOR POLICY YEAR.
     C*
     C                   Z-ADD     XXDIV         SVDIV
     C*
     C     EPKEY3        CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     EPDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      ELPFDT        XXEFDT
     C*
     C*  SET MULTI OCCR DS TO MATCHING CLASS ELEMENT OR NEXT AVAILABLE.
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C* NEXT AVAILABLE
     C     XPCLAS        IFEQ      *ZERO
     C                   MOVE      EPCLAS        XPCLAS
     C                   LEAVE
     C                   ENDIF                                                  XPCLAS IFEQ
     C* MATCHING CLASS
     C     XPCLAS        IFEQ      EPCLAS
     C                   LEAVE
     C                   ENDIF                                                  XPCLAS IFEQ
     C*
     C                   ENDDO                                                  DO Z:1-255
     C*
     C* TOTAL PAYROLL FIELD.
     C*
     C*** BEGIN CHG ***  03/03/05  ************************************
     C*********            ADD  EPEAPR    XPPAY
     C     ELFAF         IFNE      'A'
     C                   ADD       EPEAPR        XPPAY
     C                   ELSE
     C                   ADD       EPAAPR        XPPAY
     C                   ENDIF
     C*** END CHG *****  03/03/05  ************************************
     C*
     C* CHECK GOVERNING CLASS EXCLUSION FLAG.
     C*
     C                   Z-ADD     XPCLAS        XXCLAS
     C*
     C     PRKEY         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PRGVEX        XPFLAG
     C                   ENDIF
     C*
     C     XPFLAG        IFEQ      *BLANK
     C                   MOVE      'Y'           XXGOVF
     C                   ENDIF
     C*
     C                   ENDIF                                                  EPSTAT IFEQ
     C*
     C     EPKEY3        READE     PEP01                                  60
     C                   ENDDO
     C*
     C* CHECK FOR CLASS WITH HIGHEST PAYROLL TOTAL.  IGNORE EXCLUDED
     C* CLASSES IF ANY CLASS ON THE POLICY IS NOT EXCLUDED.
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C*
     C     XXGOVF        IFEQ      *BLANK
     C     XPFLAG        OREQ      *BLANK
     C*
     C*** BEGIN CHG ***  03/03/05  ************************************
     C*********  XPPAY     IFGT XXGOVP
     C     XPPAY         IFGE      XXGOVP
     C     XPCLAS        ANDNE     *ZERO
     C*** END CHG *****  03/03/05  ************************************
     C                   Z-ADD     XPCLAS        XXGOVC
     C                   Z-ADD     XPPAY         XXGOVP
     C                   ENDIF                                                  XPPAY IFGT
     C*
     C                   ENDIF                                                  XXGOVF IFEQ
     C*
     C                   ENDDO                                                  Z: 1-255
     C*
     C* RESET EMPLOYER INFORMATION.
     C*
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C* UPDATE EMPLOYER DETAIL
     C*
     C     XXGOVC        IFNE      *ZERO
     C                   Z-ADD     XXGOVC        ELGOVC
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   ENDIF                                                  XXGOVC IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELGOVC IFEQ
     C*
     C* READ NEXT POLICY.
     C*
     C                   READ      PEL01                                  60
     C*
     C*** BEGIN ADD ***  06/17/04  ************************************
     C     *IN60         IFEQ      *OFF
     C     ELCO#         ANDEQ     XXCO#
     C     ELFND         ANDEQ     XXFND
     C     ELEMP#        ANDEQ     XXEMP#
     C     ELDIV         ANDEQ     XXDIV
     C     ELFYR         ANDEQ     XXFYR
     C*
     C     ELKEY         SETGT     PEL01
     C                   READ      PEL01                                  60
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  06/17/04  ************************************
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
