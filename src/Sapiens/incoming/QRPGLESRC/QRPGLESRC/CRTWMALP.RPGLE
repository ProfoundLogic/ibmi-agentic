     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CRTWDRPP = Create WDRPP Records
     H*****************************************************************
     H* Created on 02/17/2011 by c:62 for Support 75852               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FWMEMP     IF   E             DISK
     FWMALL     IF A E           K DISK
     FWMFNL     IF   E           K DISK

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D TONE            C                   CONST('0000000001')

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     ALKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C                   READ      PEM01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      WRTALP
     C*
     C                   READ      PEM01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C***************************************************************
     C* WRTRPP - WRITES WMALP FIELDS
     C***************************************************************
     C     WRTALP        BEGSR
     C*
     C     ALKY          CHAIN     PAL01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      EMCO#         ALCO#
     C                   MOVE      EMFND         ALFND
     C                   MOVE      EMEMP#        ALEMP#
     C                   MOVE      EMDIV         ALDIV
     C*
     C                   MOVE      'L'           FNACLR
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVE      FNACLR        ALELTP
     C*
     C                   MOVE      TONE          ALELID
     C                   MOVE      0             ALDLDT
     C                   MOVE      *BLANKS       ALDLRS
     C                   MOVE      EMEFD1        ALEFDT
     C                   MOVE      EMNAME        ALNAME
     C                   MOVE      EMNAM2        ALNAM2
     C                   MOVE      *BLANKS       ALDBA
     C                   MOVE      EMMAD1        ALMAD1
     C                   MOVE      EMMAD2        ALMAD2
     C                   MOVE      EMMCTY        ALMCTY
     C                   MOVE      EMMST         ALMST
     C                   MOVE      EMMZIP        ALMZIP
     C                   MOVE      EMPAD1        ALPAD1
     C                   MOVE      EMPAD2        ALPAD2
     C                   MOVE      EMPCTY        ALPCTY
     C                   MOVE      EMPST         ALPST
     C                   MOVE      EMPZIP        ALPZIP
     C                   MOVE      EMTPOB        ALTPOB
     C                   MOVE      EMFEIN        ALFEIN
     C                   MOVE      *BLANKS       ALRMCD
     C                   MOVE      'N'           ALNCCI
     C                   MOVE      EMAREA        ALAREA
     C                   MOVE      EMPHN#        ALPHN#
     C                   MOVE      EMFAXA        ALFARA
     C                   MOVE      EMFAX#        ALFAX#
     C                   MOVE      EMCNTY        ALCNTY
     C                   MOVE      EMFDNM        ALFDNM
     C                   MOVE      EMFDN2        ALFDN2
     C                   MOVE      EMSEMP        ALSEMP
     C                   MOVE      *BLANKS       ALGSI#
     C                   MOVE      'A'           ALSTAT
     C                   MOVE      EMUSER        ALUSER
     C                   MOVE      EMPGM         ALPGM
     C                   MOVE      EMTIME        ALTIME
     C                   MOVE      EMDT          ALDT
     C                   MOVE      EMIUSR        ALIUSR
     C                   MOVE      EMIDT         ALIDT
     C                   MOVE      EMIPGM        ALIPGM
     C                   MOVE      EMITIM        ALITM
     C                   MOVE      *BLANKS       ALCNT1
     C                   MOVE      *BLANKS       ALCNT2
     C                   MOVE      *BLANKS       ALCNT3
     C                   MOVE      EMSIC         ALSIC
     C                   MOVE      EMUE#         ALUE#
     C                   MOVE      EMNAOB        ALNAOB
     C                   MOVE      EMSHFT        ALSHFT
     C                   MOVE      EMNAIC        ALNAIC
     C                   MOVE      EMNAMP        ALNAMP
     C                   MOVE      EMFDNP        ALFDNP
     C                   MOVE      *BLANKS       ALSTEM
     C*
     C                   WRITE     PAL01
     C                   ENDIF
     C*
     C                   ENDSR                                                  WRTALP
