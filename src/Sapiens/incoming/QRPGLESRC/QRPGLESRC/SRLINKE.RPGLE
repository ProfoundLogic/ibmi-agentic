     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRLINKE - WCMS/WCMC EMPLOYER MASTER LINK PGM
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* GENERAL:                                                      *
     H*  60    GENERAL CHAIN INDICATOR                                *
     H*  61    GENERAL CHAIN INDICATOR                                *
     H*  62    GENERAL CHAIN INDICATOR                                *
     H*****************************************************************
     F*
     FFMEML     UF A E           K DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:WEM01)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IWEM01
     I              EMEMP#                      E2EMP#
     I              EMDIV                       E2DIV
     I              EMFEIN                      E2FEIN
     I              EMDBA                       E2DBA
     I              EMNAME                      E2NAME
     I              EMMAD1                      E2MAD1
     I              EMMAD2                      E2MAD2
     I              EMMCTY                      E2MCTY
     I              EMMST                       E2MST
     I              EMMZIP                      E2MZIP
     I              EMPAD1                      E2PAD1
     I              EMPAD2                      E2PAD2
     I              EMPCTY                      E2PCTY
     I              EMPST                       E2PST
     I              EMPZIP                      E2PZIP
     I              EMAREA                      E2AREA
     I              EMPHN#                      E2PHN#
     I              EMVIP                       E2VIP
     I              EMMPFL                      E2MPFL
     I              EMSTRP                      E2STRP
     I              EMSTAT                      E2STAT
     I              EMUSER                      E2USER
     I              EMDT                        E2DT
     I              EMNAOB                      E2NAOB

     C*
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE (FEE SCHEDULE)
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV                                      95
     C*
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE (WCMS)
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV                                      95
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMSUB#        XXSUB#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            5
     C                   PARM                    QQDIV             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QWFND             3
     C                   PARM                    QQRTCD            1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQSUB#        XXSUB#
     C*
     C     TOP           TAG
     C*
     C*** ADD/UPDATE EMPLOYEE MASTER FILE
     C*
     C* GET WMEMP FOR EMPLOYER INFO
     C     EMKEY1        CHAIN     WEM01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     EMKY          CHAIN     PEM01                              62
     C     *IN62         IFEQ      '1'
     C                   CLEAR                   PEM01
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EMCO#
     C                   Z-ADD     XXSUB#        EMSUB#
     C                   MOVE      XXFND         EMFND
     C                   MOVE      XXEMP#        EMEMP#
     C                   MOVE      XXDIV         EMDIV                                      95
     C                   MOVE      E2NAME        EMDBA                          FLIP FR WCMS
     C                   MOVE      E2MAD1        EMMAD1
     C                   MOVE      E2MAD2        EMMAD2
     C                   MOVE      E2MCTY        EMMCTY
     C                   MOVE      E2MST         EMMST
     C                   Z-ADD     E2MZIP        EMMZIP
     C                   MOVE      E2PAD1        EMPAD1
     C                   MOVE      E2PAD2        EMPAD2
     C                   MOVE      E2PCTY        EMPCTY
     C                   MOVE      E2PST         EMPST
     C                   MOVEL     E2PZIP        EMPZIP
     C                   MOVE      E2AREA        EMAREA
     C                   MOVEL     E2PHN#        EMPHN#
     C                   Z-ADD     *ZEROS        EMRGN#
     C                   Z-ADD     E2FEIN        EMFEIN
     C                   MOVE      E2VIP         EMVIP
     C                   MOVE      E2MPFL        EMMPFL
     C                   MOVE      E2STRP        EMSTRP
     C                   MOVE      E2DBA         EMNAME                         FLIP FR WCMS
     C*
     C                   MOVE      E2NAOB        EMNOB
     C                   MOVE      *BLANK        EMSTAT
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVE      Q1USER        EMIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMIDT
     C                   Z-ADD     DSTIME        EMITIM
     C                   WRITE     PEM01
     C                   ELSE
     C                   UPDATE    PEM01
     C                   END
     C                   END
     C*
     C****************************************************************
     C* RETURN TO CALLING PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
