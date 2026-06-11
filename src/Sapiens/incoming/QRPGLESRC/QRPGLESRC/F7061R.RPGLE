     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F7061R - CASE TRANSFER FILE MAINTENANCE
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* SCR02                                                         *
     H*  31  -  (PC) AT TO SUBSCRIBER                                 *
     H*  32  -  (PC) AT TO GROUP                                      *
     H*  33  -  (PC) AT TO YEAR                                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/08/99 C:56 Support: MC095 #17568                          *
     H*               1. Add CO#/SUB#/FND TO CALL OF F0575R           *
     H*  KWK 04/18/90 - CASE NUMBER IS REQUIRED IN ORDER TO USE       *
     H*                 CASE TRANSFER HISTORY SEARCH (CMD 12)         *
     H*                                                               *
     H*  08/06/91 TMN - REMOVE FUNCTION 24 (F23 ON SCREEN) TO DELETE  *
     H*                 RECORD ON SCREEN TWO. THIS FUNCTION IS MOVED  *
     H*                 TO THE CASE TRANSFER SEARCH PGM F7560R.       *
     H*                                                               *
     H*  07/23/92 KRR - ADDED SUB# TO ALL PROCESSIG                   *
     H*                                                               *
     H*  03/03/93 C:20  1.ADDED X2FND TO PARM LIST FOR F0570R         *
     H*                 2.REMOVED PTL ARRAY FOR PASSING TITLES TO     *
     H*                   CALLED PROGRAMS                             *
     H*                                                               *
     H*  03/23/93 C:20  1.ADDED SS#/ACDT TO WRITE OF FACTP FILE       *
     H*                                                               *
     H*  04/07/93 C:20  1.ADDED CODE TO NOT ALLOW TRANSFER IF FND IS  *
     H*                   NOT AN OUTSIDE ACCOUNT                      *
     H*                                                               *
     H*  11/04/93 C:05  Project: MCMNT                                *
     H*                   Fixed to that the TO SS#/ACDT are written   *
     H*                   to FACTP on a SUB/GRP transfer (with same case#)
     H*                                                              **
     H* 05/19/94  C:20     REQ/PROJ: 141/MC110                       **
     H*                 1. CHANGE F9 ALPHA SEARCH TO CALL NEW F0578R1/F1
     H*                 IF A RECORD IS SELECTED FROM THIS SEARCH     **
     H*                 PGM WILL AUTOMATICALLY CALL F0006R1 TO UPDATE**
     H*                 THE USER PARAMETER RECORD TO THE SAME SUB/GRP**
     H*                 AS THE RECORD THAT HAS BEEN SELECTED.        **
     H*                                                              **
     H*  06/29/94 C:05  REQ/PROJ: 141/MC110 (FIX)                    **
     H*                 1. SCR01 - chg use of F0006R1 to F0006R2     **
     H*                    so that Sub# is properly updated when     **
     H*                    same Grp# is used by more than 1 Sub      **
     H*****************************************************************
     FF7061F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFACTP     UF A E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMCDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFMF1L     IF   E           K DISK

     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D NR              S             38    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CASE TRANSFER FILE
     C*
     C     CTKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    SCFFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCFCAS
     C*
     C*  CASE MASTER FILE (FMCML1 - PCM01)
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C*  CASE MASTER FILE (FMCMLA - PCM02)
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*   KEY LIST FOR THE FUND AUXILLARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    CMDT          XXACDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN71
     C                   END
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPCO#         SCFCO#
     C                   MOVE      UPSUB#        SCFSUB
     C                   MOVE      UPFND         SCFFND
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     *BLANKS       SCEM
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C**************************************************************************
     C     CMDKEY        TAG
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   F3 = EXIT (SCR01/SCR02)     *
     C*********************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*********************************
     C*   F12 = PREVIOUS (SCR02/SCR03)*
     C*********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*********************************
     C* F6 = SS#/ACC DT SEARCH (SCR01)*
     C*********************************
     C     *INKF         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0573R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8 0
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQSS#         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     QQSS#         XXSS#
     C*
     C                   Z-ADD     QQACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C     CMKEY2        CHAIN     PCM02                              55
     C     *IN55         IFEQ      '0'
     C                   MOVE      CMCO#         SCFCO#
     C                   MOVE      CMSUB#        SCFSUB
     C                   MOVE      CMFND         SCFFND
     C                   MOVE      CMFYR         SCFFYR
     C                   MOVE      CMCASE        SCFCAS
     C                   GOTO      SCR01T
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END                                                    SCR01
     C*********************************
     C* SCR02 - SUBSCRIBER SEARCH    *
     C*********************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCTSUB
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCTSUB        OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END SCR02
     C*
     C                   END                                                    *INKF
     C*********************************
     C* F7 = CASE SEARCH (SCR01)      *
     C*********************************
     C     *INKG         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVE      *ZEROS        X2FND             3 0          ADD 03/03/93
     C                   CALL      'F0570R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFFYR
     C                   PARM                    SCFCAS
     C                   PARM                    QQSS#             9
     C                   PARM                    QQRTCD            1
     C                   PARM                    X2FND                          ADD 03/03/93
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFFYR        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END
     C                   END
     C*********************************
     C* F8 = SS#/CASE SEARCH (SCR01)  *
     C*********************************
     C     *INKH         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     SRTAG         TAG
     C                   CALL      'F0571R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSS#
     C                   PARM                    SCFFYR
     C                   PARM                    SCFCAS
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQSS#         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END                                                    END EQ 'B'/BLANK
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     SCFFYR        XXFYR
     C                   Z-ADD     SCFCAS        XXCASE
     C                   Z-ADD     QQFND         XXFND
     C     CMKEY2        CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         SCSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   GOTO      SCR01T
     C                   ELSE
     C                   GOTO      STRT
     C                   END                                                    END EQ '0'
     C                   END                                                    END EQ '*'
     C                   END                                                    END *INKH
     C*********************************
     C* F9 = ALPHA SEARCH (SCR01)     *
     C*********************************
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        SCSS#
     C                   Z-ADD     *ZEROS        XXACDT
     C                   CALL      'F0578R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXSS#
     C                   PARM                    QQACDT
     C                   PARM                    SCFSUB
     C                   PARM                    SCFFND
     C                   PARM                    SCFFYR
     C                   PARM                    SCFCAS
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFFYR        OREQ      *ZEROS
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFSUB        Q2SUB#            5            ADD 06/29/94
     C                   MOVE      SCFFND        Q2FND             3
     C                   CALL      'F0006R2'                                    CHG 06/29/94
     C                   PARM                    QQCO#
     C                   PARM                    Q2SUB#                         ADD 06/29/94
     C                   PARM                    Q2FND
     C                   PARM                    QQRTCD
     C                   GOTO      SCR01T
     C                   END
     C*
     C                   END
     C                   END
     C*********************************
     C* F10 = TRANSFER SEARCH (SCR01) *
     C*********************************
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C                   MOVE      *ZERO         X1CO#             3 0
     C                   MOVE      *ZERO         X1SUB#            5 0
     C                   MOVE      *ZERO         X1FND             3 0
     C                   MOVE      *ZERO         X1FYR             3 0
     C                   MOVE      *ZERO         X1CASE            7 0
     C                   MOVE      SCFCO#        X1CO#
     C                   MOVE      SCFSUB        X1SUB#
     C                   MOVE      SCFFND        X1FND
     C                   MOVE      SCFFYR        X1FYR
     C                   MOVE      SCFCAS        X1CASE
     C                   CALL      'F7560R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    X1CO#
     C                   PARM                    X1SUB#
     C                   PARM                    X1FND
     C                   PARM                    X1FYR
     C                   PARM                    X1CASE
     C                   PARM                    QQRTCD
     C     X1CO#         IFEQ      *ZERO
     C                   MOVE      UPCO#         SCFCO#
     C                   MOVE      UPSUB#        SCFSUB
     C                   MOVE      UPFND         SCFFND
     C                   ELSE
     C                   MOVE      X1CO#         SCFCO#
     C                   MOVE      X1SUB#        SCFSUB
     C                   MOVE      X1FND         SCFFND
     C                   MOVE      X1FYR         SCFFYR
     C                   MOVE      X1CASE        SCFCAS
     C                   END
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     X1CO#         OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           SCR01T
     C*
     C                   END
     C*********************************
     C* F10 = FUND SEARCH (SCR02)     *
     C*********************************
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C     SCTSUB        IFEQ      *ZEROS
     C                   MOVE      UPSUB#        SCTSUB
     C                   END
     C                   MOVE      *ZERO         X1TSUB            5 0
     C                   MOVE      SCTSUB        X1TSUB
     C                   MOVE      *ZERO         X1TFND            3 0
     C                   MOVE      SCTFND        X1TFND
     C                   CALL      'F0508R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    X1TSUB
     C                   PARM                    X1TFND
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     X1TFND        OREQ      *ZERO
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      UPCO#         SCTCO#
     C                   MOVE      X1TSUB        SCTSUB
     C                   MOVE      X1TFND        SCTFND
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C************************************************************
     C* F16- HISTORY SEARCH - PREVIOUSLY PAID CHARGES (SCR01)    *
     C************************************************************
     C     *INKQ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C     SCFCAS        IFEQ      *ZERO
     C                   MOVE      '1'           *IN27                          CASE REQD
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCFCO#        XXCO#
     C                   MOVE      SCFSUB        XXSUB#
     C                   MOVE      SCFFND        XXFND
     C                   MOVE      SCFFYR        XXFYR
     C                   MOVE      SCFCAS        XXCASE
     C     CMKEY         CHAIN     PCM01                              65
     C     *IN65         CABEQ     '1'           STRT
     C*
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQACDT
     C*
     C                   MOVE      CMSS#         QQSS#
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   MOVE(P)   SCFCO#        Q1CO#
     C                   MOVE(P)   SCFSUB        Q1SUB#
     C                   MOVE(P)   SCFFND        Q1FND
     C*** END ADD *** 04/08/99 ****************************************
     C                   CALL      'F0575R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    QQSS#
     C                   PARM                    QQACDT
     C                   PARM                    QQRTCD
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C************************************
     C*   *IN10 - PROCESS HELP REQUESTS  *
     C************************************
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C************************************
     C* F13 - CHG USER PARMS (SCR01)     *
     C************************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         SCFCO#
     C                   MOVE      *ZERO         XXSUB#
     C                   MOVE      *ZERO         SCFSUB
     C                   MOVE      *ZERO         SCFFND
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPCO#         SCFCO#
     C                   MOVE      UPSUB#        XXSUB#
     C                   MOVE      UPSUB#        SCFSUB
     C                   MOVE      UPFND         SCFFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END                                                    END CMDKEYS
     C*******************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  SUBSCRIBER NUMBER MUST BE VALID
     C*
     C     SCFSUB        IFEQ      *ZERO
     C                   MOVE      '1'           *IN28                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C                   MOVE      SCFCO#        XXCO#
     C                   MOVE      SCFSUB        XXSUB#
     C     SMKEY         CHAIN     PSM01                              35
     C     *IN35         IFEQ      '1'
     C                   MOVE      '1'           *IN29                          INVLD GROUP
     C                   GOTO      STRT
     C                   END
     C*
     C*  GROUP NUMBER MUST BE VALID
     C*
     C     SCFFND        IFEQ      *ZERO
     C                   MOVE      '1'           *IN22                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCFCO#        XXCO#
     C                   MOVE      SCFFND        XXFND
     C     FNKEY         CHAIN     PFN01                              35
     C     *IN35         IFEQ      '1'
     C                   MOVE      '1'           *IN23                          INVLD GROUP
     C                   GOTO      STRT
     C                   END
     C     *IN71         IFEQ      '1'
     C     F1KEY         CHAIN     PF101                              35
     C     *IN35         IFEQ      '0'
     C     F1OUTA        IFNE      'Y'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*  YEAR MUST BE VAILD
     C*
     C     SCFFYR        IFEQ      *ZERO
     C                   MOVE      '1'           *IN24                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C*  CASE MUST BE VALID
     C*
     C     SCFCAS        IFEQ      *ZERO
     C                   MOVE      '1'           *IN25                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCFFYR        XXFYR
     C                   MOVE      SCFCAS        XXCASE
     C*
     C     CMKEY         CHAIN     PCM01                              26
     C     *IN26         CABEQ     '1'           STRT
     C*
     C                   MOVE      CMSS#         XXSS#
     C*
     C     *IN26         IFEQ      '0'
     C     CDKEY         CHAIN     PCD01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM           15
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM
     C                   MOVE      *BLANKS       SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        SCNAME
     C*
     C                   MOVE      CMSS#         SCSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVEL     NR(1)         SCNAME
     C                   MOVE      *BLANK        QQSS#
     C                   MOVE      *ZERO         SCACDT
     C                   END
     C                   END
     C*
     C*  CHECK TO SEE IF CASE IS ALREADY IN WORK FILE TO BE TRANSFERRED
     C*
     C                   MOVE      SCFFYR        XXFYR
     C     CTKEY1        CHAIN     PCT01                              45
     C     *IN45         IFEQ      '1'
     C                   MOVE      UA(1)         SCMODE                         NOT FOUND
     C                   MOVE      *ZEROS        SCTCO#
     C                   MOVE      *ZEROS        SCTSUB
     C                   MOVE      *ZEROS        SCTFND
     C                   MOVE      SCFFYR        SCTFYR
     C                   MOVE      SCFCAS        SCTCAS
     C                   ELSE
     C                   MOVE      UA(2)         SCMODE                         ALREADY IN FILE
     C                   MOVE      CTTCO#        SCTCO#
     C                   MOVE      CTTSUB        SCTSUB
     C                   MOVE      CTTFND        SCTFND
     C                   MOVE      CTTFYR        SCTFYR
     C                   MOVE      CTTCAS        SCTCAS
     C                   END
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  PROCESS SECOND SCREEN - SCR02
     C*
     C     SCR02T        TAG
     C*
     C*  COMPANY #  MUST BE VALID
     C*
     C     SCTCO#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     COKEY         CHAIN     PCO01                              30
     C     *IN30         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVLD CO#
     C                   GOTO      STRT
     C                   END
     C*
     C*  SUBSCRIBER NUMBER MUST BE VALID
     C*
     C     SCTSUB        IFEQ      *ZERO
     C                   MOVE      '1'           *IN29                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCTCO#        XXCO#
     C                   MOVE      SCTSUB        XXSUB#
     C     SMKEY         CHAIN     PSM01                              35
     C     *IN35         IFEQ      '1'
     C                   MOVE      '1'           *IN30                          INVLD SUB#
     C                   GOTO      STRT
     C                   END
     C*
     C*  GROUP NUMBER MUST BE VAILD
     C*
     C     SCTFND        IFEQ      *ZERO
     C                   MOVE      '1'           *IN22                          CANT BE ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCTCO#        XXCO#
     C                   MOVE      SCTFND        XXFND
     C     FNKEY         CHAIN     PFN01                              35
     C     *IN35         IFEQ      '1'
     C                   MOVE      '1'           *IN23                          INVLD GROUP
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCTFYR        XXFYR
     C                   MOVE      SCTCAS        XXCASE
     C*
     C*   CHECK TO SEE IF THE "TO" CASE ALREADY EXISTS IN CLAIM FILE
     C*
     C*   ... IF THE FUND YEAR OR CASE HAVE NOT CHANGED,
     C*       THIS IS A COMPANY/FUND TRANSFER - SO THE
     C*       "NEW" CASE NUMBER MUST NOT BE ON FILE.
     C*
     C*   ... IF THE FUND YEAR OR CASE HAVE CHANGED,
     C*       THIS IS A CASE/YEAR TRANSFER - SO THE
     C*       THE "NEW" CASE NUMBER MUST BE ON FILE.
     C*
     C     CMKEY         CHAIN     PCM01                              50
     C*
     C                   SETOFF                                       2827
     C*** DETERMINE IF CO #  OR FUND HAVE CHANGED - IF YES, THE "TO"
     C*    RECORD CAN NOT ALREADY EXIST FOR A CO#/FUND TRANSFER ...
     C*
     C     SCTCO#        IFNE      SCFCO#
     C     SCTSUB        ORNE      SCFSUB
     C     SCTFND        ORNE      SCFFND
     C  N50              SETON                                        28
     C   28              GOTO      STRT                                         CO/SUB/FND TRANS
     C                   END
     C*
     C*** DETERMINE IF FUND YEAR & CASE HAVE CHANGED - IF YES, THE "TO"
     C*    RECORD MUST ALREADY EXIST FOR A YEAR/CASE TRANSFER ...
     C*
     C     SCTFYR        IFNE      SCFFYR
     C     SCTCAS        ORNE      SCFCAS
     C   50              SETON                                        27
     C   27              GOTO      STRT                                         CASE/YR TRANSFER
     C                   END
     C*
     C** ALL EDITS HAVE BEEN PASSED NOW, DISPLAY "TO CASE" INFO...
     C*
     C** DISPLAY THE NAME, SS# AND ACCIDENT DATE OF THE 'TO' CASE
     C*   FOR A YEAR/CASE TRANSFER
     C*
     C     SCTFYR        IFNE      SCFFYR
     C     SCTCAS        ORNE      SCFCAS
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C*
     C                   MOVE      CMSS#         XXSS#
     C*
     C     *IN51         IFEQ      '0'
     C     CDKEY         CHAIN     PCD01                              52
     C     *IN52         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM           15
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM
     C                   MOVE      *BLANKS       SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        SCNAM2
     C*
     C                   MOVE      CMSS#         SCSS#2
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACD2
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCNAM2
     C                   MOVEL     NR(1)         SCNAM2
     C                   MOVE      *BLANK        SCSS#2
     C                   MOVE      *ZERO         SCACD2
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  PROCESS THIRD SCREEN - SCR03
     C*
     C     SCR03T        TAG
     C*
     C                   MOVE      SCFCO#        CTFCO#
     C                   MOVE      SCFSUB        CTFSUB
     C                   MOVE      SCFFND        CTFFND
     C                   MOVE      SCFFYR        CTFFYR
     C                   MOVE      SCFCAS        CTFCAS
     C                   MOVE      SCSS#         CTFSS#
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTFACD
     C*
     C                   MOVE      SCTCO#        CTTCO#
     C                   MOVE      SCTSUB        CTTSUB
     C                   MOVE      SCTFND        CTTFND
     C                   MOVE      SCTFYR        CTTFYR
     C                   MOVE      SCTCAS        CTTCAS
     C     SCSS#2        IFEQ      *BLANKS
     C                   MOVE      SCSS#         CTTSS#
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTTACD
     C*
     C                   ELSE
     C                   MOVE      SCSS#2        CTTSS#
     C*
     C                   Z-ADD     SCACD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTTACD
     C*
     C                   END
     C                   MOVE      Q1USER        CTUSER
     C                   MOVE      Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CTTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*
     C     *IN45         IFEQ      '1'
     C                   MOVE      Q1USER        CTIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTIDT
     C*
     C                   WRITE     PCT01
     C                   ELSE
     C                   UPDATE    PCT01
     C                   END
     C*
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPCO#         SCFCO#
     C                   MOVE      UPSUB#        XXSUB#
     C                   MOVE      UPSUB#        SCFSUB
     C                   MOVE      UPFND         SCFFND
     C                   MOVE      *BLANKS       SCFFYR
     C                   MOVE      *BLANKS       SCFCAS
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE NR
*** NO CASE RECORD EXISTS ***
Case Exists, Press ENTER to continue.
