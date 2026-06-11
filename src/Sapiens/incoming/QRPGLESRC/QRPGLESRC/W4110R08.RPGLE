     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110R08 - C-20  FIRST REPORT OF WORK INJURY
     H*****************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for additional input necessary  *
     H* to print a department letter.                                 *
     H*                                                               *
     H*  CALLED FROM:  W4110R                                         *
     H*                                                               *
     H*  CALLS TO:     NONE                                           *
     H*                                                               *
     H*  PROGRAMMER:   C:02                                           *
     H*                                                               *
     H*  CREATED:    09/24/98               PROJECT: WC859            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/18/00 C:53  PROJECT: WC962
     H*                Added jurisdiction state to WMNIP chain.
     H*                                                               *
     H* 04/02/99 C:90  PROJECT: WC889                                 *
     H*          IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1.     *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTOCL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWDCLL     UF   E           K DISK
     FWDC2L     O    E             DISK
     FWMNIL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CMSS#                  1      9
     D  X1SS1                  1      3
     D  X1SS2                  4      5
     D  X1SS3                  6      9
     D*
     D*   ADDITIONAL PROGRAM PARMS
     D                 DS
     D  PARMS                  1    136
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQREQ#                 7     11
     D  QQDPCD                12     16
     D  QQDCID                17     28
     D  QQDESC                29     56
     D  QQFLDR                57    136
     D                 DS
     D  XXDATE                 1     10
     D  XXMM                   1      2
     D  XXSLH1                 3      3
     D  XXDD                   4      5
     D  XXSLH2                 6      6
     D  XXCCYY                 7     10
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1CCYY                 5      8
     D                 DS
     D  XXPHN#                 1      7
     D  XXPRE                  1      3
     D  XXSUF                  4      7
     D                 DS
     D  XXTIM                  1      6
     D  XXHR                   1      2
     D  XXMN                   3      4
     D  XXSC                   3      4
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
      *
      *   KEY LIST FOR OCCUPATION CODE FILE
      *
     C     OCKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*  PAYEE MASTER
     C*
     C     PDKY          KLIST
     C                   KFLD                    CMHOSF
     C                   KFLD                    CMHOSM
     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*  WMNIL KEY LIST
     C*
     C     NIKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C                   KFLD                    CMJRST                         ADD 05/18/00
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  EMPLOYER MASTER KEY LIST
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY FOR LETTER WORKFILE
     C*
     C     CLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQPRMS          136
     C                   PARM                    QQDEPT            2
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
     C                   MOVE      QQPRMS        PARMS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQREQ#        XXREQ#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C                   TIME                    DSDTTM
      *
      *   DISPLAY THE APPROPRIATE SCREEN
      *
     C     STRT          TAG
      *
      *  UPDATE THE CLAIM DEPT LETTER FILE
      *
     C                   EXSR      SRUPD
      *
      ******************************************************************
      *   END OF PROGRAM
      ******************************************************************
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
      * ===============================================================
      *   UPDATE THE FILE
      * ===============================================================
     C     SRUPD         BEGSR
     C*
     C*  GET THE DEPT LETTER WORKFILE RECORD
     C*
     C     CLKEY         CHAIN     PCL01                              50
      *
      *  CLAIMANT INFO
      *
     C     CMKEY         CHAIN     PCM01                              60
     C  N60CDKEY         CHAIN     PCD01                              61
     C  N60
     CANN61NIKY          CHAIN     PNI01                              62
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     '('           CL20A8
     C                   MOVE      CDAREA        XXAREA            3
     C     CL20A8        CAT       XXAREA:0      CL20A8
     C     CL20A8        CAT       ')':0         CL20A8
     C                   MOVE      CDPHN#        XXPHN#
     C     CL20A8        CAT       XXPRE:1       CL20A8
     C     CL20A8        CAT       '-':0         CL20A8
     C     CL20A8        CAT       XXSUF:0       CL20A8
      *
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A1
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A2
      *
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A5
      *
     C     CMDTLW        IFNE      0
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A6
     C                   ELSE
     C                   MOVE      *BLANKS       CL10A6
     C                   ENDIF
     C*
     C                   MOVEL     CDFSN         XXFSN             7            FIRST NAME
     C                   MOVEL     CDLSN         XXLSN            16            LAST
     C     XXFSN         CAT       CDMID:1       XXCLNM           27
     C     CDMID         IFNE      *BLANK                                       DON'T PRT '.'
     C     XXCLNM        CAT       '.':0         XXCLNM                          IF NO MID INTIAL
     C                   END
     C     XXCLNM        CAT       XXLSN:1       CL40A5
      *
     C     X1SS1         CAT       '-':0         CL20A2                         CLAIMANT SS#
     C     CL20A2        CAT       X1SS2:0       CL20A2
     C     CL20A2        CAT       '-':0         CL20A2
     C     CL20A2        CAT       X1SS3:0       CL20A2
      *
     C                   MOVEL     CDADR1        XXCDAD           22            ADDRESS
     C                   MOVEL     XXCDAD        CL40A6
      *
     C     CDCTY         CAT       ',':0         XXLCSZ           30            CITY, ST
     C     XXLCSZ        CAT       CDST:1        CL40A7
      *
     C                   MOVEL     CDZIP         XXZIP1
     C                   MOVE      CDZIP         XXZIP2
     C                   MOVEL     XXZIP1        CL20AB                         CLAIMANT ZIP
     C     XXZIP2        IFNE      '0000'
     C     CL20AB        CAT       '-':0         CL20AB
     C     CL20AB        CAT       XXZIP2:0      CL20AB
     C                   END
      *
      * CALCULATE CLAIMANT'S AGE
      *
     C     CMACDT        SUB       CDDTOB        XXAGE             6 0
     C                   MOVEL     XXAGE         CL2A3
      *
     C     CDSEX         IFEQ      'M'                                          MALE
     C                   MOVE      'X'           CL1A6
     C                   MOVE      *BLANKS       CL1A7
     C                   END
     C     CDSEX         IFEQ      'F'                                          FEMALE
     C                   MOVE      'X'           CL1A7
     C                   MOVE      *BLANKS       CL1A6
     C                   END
     C     CDMSTS        IFEQ      'M'
     C                   MOVE      'X'           CL1A8
     C                   MOVE      *BLANKS       CL1A9
     C                   ELSE
     C                   MOVE      *BLANKS       CL1A8
     C                   MOVE      'X'           CL1A9
     C                   ENDIF
     C     CMOCDS        IFNE      *BLANKS
     C                   MOVEL(P)  CMOCDS        CL40A1
     C                   ELSE
     C                   MOVEL(P)  CMOCCD        XXKEY
     C     OCKY          CHAIN     POC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OCDESC        CL40A1
     C                   END
     C                   END
      *
     C                   MOVEL     CMDAYS        CL1A10                         DAYS WORKD/WEEK
     C                   MOVE      CMLTFL        CL1A5
      *
     C                   END
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        XX31A
     C                   ELSE
     C                   MOVEL(P)  CONAME        XX31A            31
     C                   ENDIF
     C                   MOVEL     XX31A         CL40A8
      *
      *   EMPLOYER INFO
      *
     C     EMKEY         CHAIN     PEM01                              61
     C     *IN61         IFEQ      *OFF
      *
     C                   MOVE      *BLANKS       XX30A            30            EMPLOYER NAME
     C     EMNAME        IFNE      *BLANKS
     C                   MOVEL     EMNAME        XX30A
     C                   ELSE
     C                   MOVEL     EMDBA         XX30A
     C                   END
     C                   MOVEL     XX30A         CL40A9
      *
     C     EMMAD1        IFNE      *BLANKS
      *
     C                   MOVEL     EMMAD1        XXEMAD           19            ADDRESS, CITY
     C                   MOVEL     EMMCTY        XXEMCY           10
     C     XXEMAD        CAT       ',':0         CL40A4
     C     CL40A4        CAT       XXEMCY:1      CL40A4
      *
     C                   MOVEL     EMMZIP        XXZIP1            5
     C                   MOVE      EMMZIP        XXZIP2            4
     C                   MOVEL     XXZIP1        CL20A4                         EMP ZIP
     C     XXZIP2        IFNE      '0000'
     C     CL20A4        CAT       '-':0         CL20A4
     C     CL20A4        CAT       XXZIP2:0      CL20A4
     C                   END
      *
     C                   ELSE
      *
     C                   MOVEL     EMPAD1        XXEMAD                         ADDRESS, CITY
     C                   MOVEL     EMPCTY        XXEMCY
     C     XXEMAD        CAT       ',':0         CL40A4
     C     CL40A4        CAT       XXEMCY:1      CL40A4
      *
     C                   MOVEL     EMPZIP        XXZIP1
     C                   MOVE      EMPZIP        XXZIP2
     C                   MOVEL     XXZIP1        CL20A4                         EMP ZIP
     C     XXZIP2        IFNE      '0000'
     C     CL20A4        CAT       '-':0         CL20A4
     C     CL20A4        CAT       XXZIP2:0      CL20A4
     C                   END
      *
     C                   END
      *
     C                   MOVEL     EMNAOB        CL20A9                         NAT OF BUSINESS
     C                   MOVE      EMAREA        XXEARE            3            EMP PH#
     C                   MOVEL     EMPHN#        XXEPH1            3            (123) 456-7890
     C                   MOVE      EMPHN#        XXEPH2            4            (123) 456-7890
     C     '('           CAT       XXEARE:0      CL20A0
     C     CL20A0        CAT       ')':0         CL20A0
     C     CL20A0        CAT       XXEPH1:1      CL20A0
     C     CL20A0        CAT       '-':0         CL20A0
     C     CL20A0        CAT       XXEPH2:0      CL20A0
     C                   END                                                    *IN61
     C     CMRTW         IFNE      0                                            MARRIED
     C                   MOVE      'Y'           CL1A4
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A3
     C                   ELSE
     C                   MOVE      'N'           CL1A4
     C                   END
     C*
     C     CMDTH         IFEQ      'Y'                                          MARRIED
     C                   MOVE      'X'           CL1A5
     C                   MOVE      ' '           CL1A6
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      X1MM          XXMM
     C                   MOVE      X1DD          XXDD
     C                   MOVE      X1CCYY        XXCCYY
     C                   MOVE      '/'           XXSLH1
     C                   MOVE      '/'           XXSLH2
     C                   MOVE      XXDATE        CL10A4
     C                   ELSE
     C                   MOVE      ' '           CL1A5
     C                   MOVE      'X'           CL1A6
     C                   MOVE      *BLANKS       CL10A4
     C                   END
     C                   MOVE      CMACTM        XXTIM
     C                   MOVEL     XXHR          CL10AA
     C     CL10AA        CAT       ':':0         CL10AA
     C     CL10AA        CAT       XXMM:0        CL10AA
     C     CL10AA        CAT       ':':0         CL10AA
     C     CL10AA        CAT       XXSC:0        CL10AA
     C                   MOVE      CMPOI         CL1A3
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      'Y'           CL1A5
     C                   ELSE
     C                   MOVE      *BLANKS       CL1A5
     C                   ENDIF
     C*
     C                   Z-ADD     CMHRSD        CL5N1
     C     CMHRSD        MULT      CMDAYS        CL5N2
     C*
     C                   MOVE      *BLANK        ZZKEY             4
     C                   MOVE      *BLANK        XXKEY
     C     EMPST         IFNE      *BLANK
     C     EMPST         CAT       EMCNTY:0      ZZKEY
     C                   ELSE
     C     EMMST         CAT       EMCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   MOVEL(P)  ZZKEY         XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CNDESC
     C                   END
     C                   MOVEL(P)  CMACDR        CL20A6
     C                   MOVEL(P)  CMACTY        CL20A7
     C                   MOVEL(P)  CNDESC        CL20A5
     C                   MOVE      CMEADR        CL1A2
     C     18            SUBST(P)  CMACDS:1      CL60A4
     C     42            SUBST(P)  CMACDS:19     CL60A5
     C     CL60A5        CAT       CMACD2:0      CL60A5
     C*
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  TPPHYN        CL60A0
     C     CL60A0        CAT       TPPHYA:1      CL60A0
     C     CL60A0        CAT       TPPHYC:1      CL60A0
     C     CL60A0        CAT       TPPHYS:1      CL60A0
     C                   MOVEL     TPPHYZ        XXZIP1
     C                   MOVE      TPPHYZ        XXZIP2
     C     CL60A0        CAT       XXZIP1:1      CL60A0
     C     XXZIP2        IFNE      '0000'
     C     CL60A0        CAT       '-':0         CL60A0
     C     CL60A0        CAT       XXZIP2:0      CL60A0
     C                   END
     C                   END
     C*
     C     CMHOSF        IFNE      *ZEROS
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        CL60AA
      **** BEGIN ADD **** 04/02/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C     CL60AA        CAT       PDADR2:1      CL60AA
     C                   ELSE
      **** END   ADD **** 04/02/99 ************************************
     C     CL60AA        CAT       PDADR1:1      CL60AA
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C     CL60AA        CAT       PDCTY:1       CL60AA
     C     CL60AA        CAT       PDST:1        CL60AA
     C                   MOVEL     PDZIP         XXZIP1
     C                   MOVE      PDZIP         XXZIP2
     C     CL60AA        CAT       XXZIP1:1      CL60AA
     C     XXZIP2        IFNE      '0000'
     C     CL60AA        CAT       '-':0         CL60AA
     C     CL60AA        CAT       XXZIP2:0      CL60AA
     C                   END
     C                   END
     C                   ELSE
     C                   MOVEL(P)  CMHOSN        CL60AA
     C     CL60AA        CAT       CMHOSA:1      CL60AA
     C     CL60AA        CAT       CMHOSC:1      CL60AA
     C                   MOVEL     CMHOSZ        XXZIP1
     C                   MOVE      CMHOSZ        XXZIP2
     C     CL60AA        CAT       XXZIP1:1      CL60AA
     C     XXZIP2        IFNE      '0000'
     C     CL60AA        CAT       '-':0         CL60AA
     C     CL60AA        CAT       XXZIP2:0      CL60AA
     C                   END
     C                   END
      *
      *   EDI CLAIM STATE INFO
      *
     C                   MOVEL     NI3N1         CL20A1
     C                   MOVEL     NI10A4        CL10A9
     C                   MOVEL     NI3N2         CL20AA
     C                   MOVEL     NI10A5        CL10A0
     C                   MOVEL     NI10A2        CL10A7
     C                   MOVEL     NI10A3        CL10A8
     C                   MOVEL     NI80A1        CL60A8
     C                   MOVEL     NI80A4        CL60A6
     C                   MOVEL     NI80A5        CL60A7
     C                   MOVEL     NI80A2        CL60A1
     C                   MOVEL     NI80A3        CL60A2
     C                   MOVEL     NI20A1        CL40A2
      *
      *
     C     *IN50         IFEQ      *OFF
     C                   UPDATE    PCL01
     C                   WRITE     PC201
     C                   END
     C*
     C                   ENDSR
     C**************************************************************************
     C*
