     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7322R - PREMIUM NOTICES (INVOICES) - BUILD WORKFILE
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*     THIS PROGRAM BUILDS A WORKFILE FOR THE INVOICE PRINT      *
     H*     PROGRAM (W7321R) TO USE FOR SORTING BY EMP#/DIV OR        *
     H*     EMPLOYER NAME                                             *
     H*                                                               *
     H*  CALLED FROM: W7321C - INVOICES - SUBMIT                      *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*         60  -  GENERAL CHAINING                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWXM4P     O    E             DISK
     FWMEML     IF   E           K DISK
     FWDELL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Employer Master File
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Employer Detail File - Single employer
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    EMDT          XXDTE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQSRT             1
     C                   PARM                    QQENFR            2
     C                   PARM                    QQENTO            2
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQENFR        XXENFR            2
     C                   MOVE      QQENTO        XXENTO            2
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*
     C*****************************************************************
     C*    SET UP AND INITALIZE
     C*****************************************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE
     C*
     C     QQSRT         IFEQ      'E'
     C                   MOVE      *BLANKS       XXENFR
     C                   MOVE      *HIVAL        XXENTO
     C                   END
     C*
     C     QQSRT         IFEQ      'N'
     C     XXENFR        IFEQ      *BLANKS
     C     XXENTO        ANDEQ     *BLANKS
     C                   MOVE      *HIVAL        XXENTO
     C                   END
     C                   END
     C*
     C* Get the Employer Detail Record
     C*
     C     XXEMP#        IFEQ      *ZERO
     C     ELKY          CHAIN     PEL01                              60
     C*
     C* For Single Employer Invoice, chain directly to the requested
     C* Employer Detail Record
     C*
     C                   ELSE
     C     ELKY1         CHAIN     PEL01                              60
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Get the Employer Master Record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXNM              2
     C                   MOVEL     EMNAME        XXNM
     C*
     C     XXNM          IFGE      XXENFR
     C     XXNM          ANDLE     XXENTO
     C*
     C* Move Information to Work File Fields
     C*
     C                   Z-ADD     EMCO#         M4CO#
     C                   Z-ADD     EMFND         M4FND
     C                   Z-ADD     EMEMP#        M4EMP#
     C                   Z-ADD     EMDIV         M4DIV
     C                   MOVEL     EMNAME        M4ENAM
     C                   MOVE      *BLANKS       M4STAT
     C                   MOVE      Q1USER        M4USER
     C                   MOVE      Q1PGM         M4PGM
     C                   MOVE      DSTIME        M4TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M4DT
     C                   WRITE     PM401
     C*
     C                   END
     C                   END
     C*
     C     XXEMP#        IFEQ      *ZERO
     C     ELKY          READE     PEL01                                  60
     C*
     C* For Single Employer Invoice, no more reads are needed
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
