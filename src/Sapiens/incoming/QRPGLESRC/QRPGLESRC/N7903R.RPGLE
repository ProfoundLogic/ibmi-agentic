     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE N7903R - EOB EDIT REPORT - PROMPT SCREEN PROGRAM
     H*
     H*****************************************************************
     H* Created on 09/10/2001 by C:62 for Project:MC239               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FN7903F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*****************************************************************
     D* Data Structures
     D*****************************************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*****************************************************************
     D* Constants
     D*****************************************************************
     D OPTNM           C                   CONST('EOB Edit Report')

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
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
     C*****************************************************************
     C* Key Lists
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    SMSUB#        KKSUB#
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    SMDT          XXFDT
     C     *LIKE         DEFINE    SMDT          XXTDT
     C*****************************************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   ENDIF
     C*
     C* GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* LOAD PROGRAM TITLE
     C*
     C                   MOVE(P)   OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD
     C                   PARM      40            SRDLEN
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C* INITIALIZE SCREEN FIELDS
     C*
     C                   Z-ADD     UPSUB#        SCSUB#
     C                   Z-ADD     SCSUB#        KKSUB#
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSNAM
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             SCTDT
     C                   Z-ADD     0             SCFDT
     C                   MOVE      *BLANKS       SCBSTA
     C                   MOVE      *BLANKS       SCBTYP
     C                   MOVE      Q1USER        SCUSER
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   SETOFF                                       90
     C*
     C* PROGRAM LOOP
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   SETOFF                                       90
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '00000'       *IN(36)
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   COMMAND 03 = EXIT          *
     C*********************************
     C*
     C     *INKC         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C*********************************
     C*      F10 = SEARCH             *
     C*********************************
     C     *INKJ         IFEQ      *ON
     C*
     C* CONVERT DATES TO MDYY
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C                   SELECT
     C*
     C* SUBSCRIBER SEARCH
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C     ##RRN         CHAIN     PSM02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   SETON                                        32
     C                   ENDIF                                                  IFEQ *IN99
     C                   ENDIF                                                  IFEQ *INKL
     C*
     C* SYSTEM SEARCH
     C*
     C     HFLD          WHENEQ    'SCBTYP'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'SYS'         ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        33
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCBTYP
     C                   SETON                                        34
     C                   ENDIF                                                  IFEQ *IN99
     C                   ENDIF                                                  IFEQ *INKL
     C*
     C* REPORT TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRTYP'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'RPTY'        ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        34
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCRTYP
     C                   MOVEL(P)  GTDESC        SCRTDS
     C                   ENDIF                                                  IFEQ *IN99
     C                   ENDIF                                                  IFEQ *INKL
     C*
     C                   ENDSL                                                  HFLD SCRTYP
     C                   SETON                                        90
     C                   ENDIF                                                  IFEQ *INKJ
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
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
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF                                                  *IN11
     C*
     C************ END OF COMMAND KEY SECTION *************************
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C                   DO
     C*
     C*   SUBSCRIBER NUMBER MUST BE VALID, IF NOT BLANK
     C*
     C     SCSUB#        IFNE      *ZEROS
     C                   Z-ADD     SCSUB#        KKSUB#
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCFDT         IFNE      *ZEROS
     C     SCTDT         ORNE      *ZEROS
     C*
     C*   FROM DATE MUST BE VALID IF ENTERED
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   LEAVE
     C                   ENDIF
     C*
     C* TO DATE MUST BE VALID IF ENTERED
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C*
     C* IF ANY DATE IS ENTERED THEN DATES MUST BE ASCENDING
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C     XXFDT         IFGT      XXTDT
     C                   SETON                                        7490
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  USE DATE
     C*                                                    RANGE?
     C*
     C*   THE BILL STATUS MUST BE 'I'. 'U', OR 'B'
     C*
     C     SCBSTA        IFNE      'I'
     C     SCBSTA        ANDNE     'U'
     C     SCBSTA        ANDNE     'B'
     C                   SETON                                        7590
     C                   LEAVE
     C                   ENDIF
     C*
     C*   THE BILL TYPE MUST BE 'H','P','N','U','M' OR 'A'
     C*
     C     SCBTYP        IFNE      'A'
     C     SCBTYP        ANDNE     'B'
     C                   MOVEL(P)  'SYS'         KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7690
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*   THE REPORT TYPE MUST BE 'D', 'S', OR 'B'
     C*
     C                   MOVEL(P)  'RPTY'        KKTBID
     C                   MOVEL(P)  SCRTYP        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7790
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C                   ENDDO
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C     *INKC         IFEQ      *OFF
     C*
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      SCSUB#        ##SUB#
     C*
     C     SCFDT         IFEQ      *ZEROS
     C                   Z-ADD     01011940      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##FDT
     C                   ELSE
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##FDT
     C                   ENDIF                                                  IFEQ *ZEROS
     C*
     C     SCTDT         IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##TDT
     C                   ELSE
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##TDT
     C                   ENDIF                                                  IFEQ SCFRDT
     C*
     C                   CALL      'N7903C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FDT             7
     C                   PARM                    ##TDT             7
     C                   PARM      SCBSTA        ##BSTA            1
     C                   PARM      SCBTYP        ##BTYP            1
     C                   PARM      SCRTYP        ##RTYP            1
     C                   PARM      UPOUTQ        ##OUTQ           10
     C                   PARM      'D'           ##MODE            1
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C     SCBTYP        IFEQ      'P'
     C     SCBTYP        OREQ      'H'
     C     SCBTYP        OREQ      'N'
     C     SCBTYP        OREQ      'A'
     C*
     C                   CALL      'N7152C'
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FDT
     C                   PARM                    ##TDT
     C                   PARM      SCBTYP        ##BTYP
     C                   PARM      SCRTYP        ##RTYP
     C                   PARM      UPOUTQ        ##OUTQ
     C                   PARM      'D'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
