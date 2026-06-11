     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXPRTD - POPULATE ELPRTD WITH PRORATED EXPENSE CONSTANT.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**   POPULATE THE ELPRTD FIELD WITH PRORATED EXPENSE CONSTANT.  *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    01/28/02            PROJECT: WC1031               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDECL     IF   E           K DISK
     FWDMPP     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  EXPENSE CONSTANT FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      *ON           *INOF
     C                   TIME                    DSDTTM
     C*
     C* READ ALL RECORDS.
     C*
     C                   READ      PMP01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     MPCO#         XXCO#
     C                   Z-ADD     MPFND         XXFND
     C                   Z-ADD     MPFYR         XXFYR
     C                   Z-ADD     MPEMP#        XXEMP#
     C                   Z-ADD     MPDIV         XXDIV
     C*
     C* ONLY UPDATE RECORDS WITH EXPENSE CONSTANT.
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C  N60EMKEY         CHAIN     PEM01                              60
     C*
     C     MPPRTD        IFEQ      *ZERO
     C     EMSTRP        ANDEQ     'Y'
     C     *IN60         ANDEQ     *OFF
     C     MPPRTD        OREQ      *ZERO
     C     F1EXDV        ANDEQ     'Y'
     C     *IN60         ANDEQ     *OFF
     C*
     C                   Z-ADD     0             XXEXCO           11 2
     C                   Z-ADD     0             XXMPEC           11 2
     C                   MOVE      *BLANK        XXSFUL            1
     C*
     C* FIND HIGHEST EXPENSE CONSTANT.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   Z-ADD     ECMPEC        XXMPEC
     C                   MOVE      ECSFUL        XXSFUL
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C     XXSFUL        IFNE      'Y'
     C     ELPROR        MULT(H)   XXEXCO        XXEXCN           15 4
     C                   ELSE
     C                   Z-ADD(H)  XXEXCO        XXEXCN           15 4
     C                   ENDIF                                                  XXSFUL IFEQ
     C*
     C     XXEXCN        IFLT      XXMPEC
     C                   Z-ADD     XXMPEC        XXEXCN
     C                   END
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD(H)  XXEXCN        XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXEXCN
     C                   Z-ADD     XXEXCN        MPPRTD
     C*
     C                   MOVEL     Q1USER        MPUSER
     C                   MOVEL     Q1PGM         MPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MPDT
     C                   Z-ADD     DSTIME        MPTIME
     C*
     C                   UPDATE    PMP01
     C*
     C                   ENDIF                                                  MPPRTD IFNE
     C*
     C* READ NEXT EMPLOYER DETAIL.
     C*
     C                   READ      PMP01                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
