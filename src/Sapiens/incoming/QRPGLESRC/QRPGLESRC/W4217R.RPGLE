     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4217R - Prompt - CLEAR TRANSACTION STATUS
     H*                                                               *
     H*****************************************************************
     H* 04/13/92 JEI  THIS PGM ALLOWS THE USER TO CLEAR THE STATUS ON *
     H* PROJ:         TRANSACTIONS THAT HAVE HAD MORE THAN TWO ACTIVI-*
     H*               TY TYPES SO THEY CAN BE CLEARED.
     H*****************************************************************
     H* REVISIONS
     H*
     H*  05/20/04  C:053  PROJECT: WC1139
     H*                   CLEAR PARCDT IF 'X' IS CLEARED.
     H*
     H*  12/17/99  C:118  PROJECT: WC577
     H*                   CHANGE DATE FORMAT FOR XXCKDT, SHOULD BE
     H*                   CYMD.
     H*****************************************************************
     FW4217F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDCRL3    UF   E           K DISK
     F*** BEG ADD *** 05/20/04 ****************************
     FWDPAL4    UF   E           K DISK
     F*** END ADD *** 05/20/04 ****************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*
     C*
     C*  CHECK RECONCILIATION FILE
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQACCT
     C                   KFLD                    QQRTP
     C                   KFLD                    XXCHK#
     C                   KFLD                    XXCKDT
     C*
     C*** BEG ADD *** 05/20/04 ****************************
     C*
     C     PAKY          KLIST
     C                   KFLD                    CRCO#
     C                   KFLD                    CRFND
     C                   KFLD                    CRCHK#
     C*
     C*** END ADD *** 05/20/04 ****************************
     C*
     C     *LIKE         DEFINE    CRDT          XXCKDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQACCT           20
     C                   PARM                    QQRTP             1
     C                   PARM                    QQCHK#            7
     C                   PARM                    QQCKDT            8
     C                   PARM                    QQCALL            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQCHK#        XXCHK#            7 0
     C                   MOVE      QQCKDT        X1CKDT            8 0
     C*
     C                   Z-ADD     X1CKDT        SRDTI
     C***** BEGIN CHG **** 12/17/99 ***********************************
     C**********           CALL 'SRCVTDAT'DTMDY1
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***** END   CHG **** 12/17/99 ***********************************
     C                   MOVEL     SRDTO         XXCKDT
     C*
     C     STRT01        TAG
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
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
     C*
     C*  DISPLAY SCREEN TO VERIFY USER WANTS TO CLEAR FIELDS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C*   PROCESS HELP REQUESTS
     C*
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
     C                   GOTO      STRT
     C                   END
     C                   END
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE CHECK SELECTED
     C*
     C     CRKY1         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '1'
     C                   SETON                                        20
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF STATUS OR CHECK IS ALREADY CLEAR, FLASH MESSAGE
     C*
     C     CRSTAT        IFEQ      *BLANK
     C     CRSTAT        OREQ      'X'
     C                   SETON                                        21
     C                   GOTO      STRT
     C                   END
     C*
     C*  CLEAR STATUS FIELDS IF ENTER IS PRESSED
     C*
     C                   MOVE      *BLANK        CRSTAT
     C                   MOVE      *BLANK        CRRVST            1
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   Z-ADD     DSTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   UPDATE    PCR01
     C*
     C*** BEG ADD *** 05/20/04 ****************************
     C*
     C**** ACCESS THE PAYMENT FILE
     C*
     C     PAKY          CHAIN     PPA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C  N99PAACCT        IFEQ      CRACCT
     C*
     C**** UPDATE THE PAYMENT FILE
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     *ZEROS        PARCDT
     C                   MOVE      CRUSER        PAUSER
     C                   MOVE      CRPGM         PAPGM
     C                   Z-ADD     CRTIME        PATIME
     C                   Z-ADD     CRDT          PADT
     C*
     C                   UPDATE    PPA01
     C*
     C     PAKY          READE     PPA01                                  99
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*** END ADD *** 05/20/04 ****************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
