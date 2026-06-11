     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3010R - AGENT COMMISSIONS MAINTENANCE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/17/10 C:246  SUPPORT: 75220 CLT:099                       *
     H*                  Fix the issue with the commission  process   *
     H*                  for accepting the without the group number   *
     H*                                                               *
     H*  12/21/06 C:183  PROJECT: 114WND                              *
     H*                  Add a record to WMPHP.                       *
     H*                                                               *
     H*  12/01/04 C:132  PROJECT: WC1244                              *
     H*                  Add Informational message that commission    *
     H*                  records exist in the date range entered.     *
     H*                                                               *
     H*  12/15/03 C:33   Seq#: WA072/40377                            *
     H*                  Back out 10/22/03 changes.                   *
     H*                                                               *
     H*  10/22/03 C:132  PROJECT: WA107   SEQ: 39325                  *
     H*                Make the Information Msg for PAYMENT HAS BEEN  *
     H*                POSTED AFTER OR ON THE 'TO DATE' to be a hard  *
     H*                Error and not continue processing.             *
     H*                                                               *
     H*  06/28/00 C:33 PROJECT: WC577                                 *
     H*                DISPLAY INFORMATION MSG IF A PAYMENT HAS BEEN  *
     H*                POSTED AFTER OR ON THE 'TO DATE'               *
     H*                                                               *
     H* 05/19/00 C:61  PROJECT WC577                                  *
     H*                Correct display of TO date.                    *
     H*                                                               *
     H*  07/23/98 C:33 PROJECT: WC577                                 *
     H*                CHANGED KLIST TO CURRENT STANDARDS.            *
     H*                (xxKEY -> xxKY, UPCO# -> XXCO#, UPFND -> XXFND)*
     H*                                                               *
     H*  06/12/98 C:90 PROJECT WC577                                  *
     H*                CHANGED SCREENS TO CURRENT STANDARDS.          *
     H*                                                               *
     H*  05/22/98 C:91 PROJECT WC577                                  *
     H*                CHANGED SCREENS TO CURRENT STANDARDS.          *
     H*                                                               *
     H*  04/06/98 C:65 PROJECT CM038                                  *
     H*                REMOVE FYR FOR COMMERICAL PROCESSING.          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USAGE:                                            *
     H*   27    DATE RANGE MUST BE IN ASCENDING ORDER                 *
     H*****************************************************************
     FW3010F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*** BEGIN CHG ***  04/06/98  ************************************
     F*****WDF1L   IF  E           K        DISK
     F*** END CHG *****  04/06/98  ************************************
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F***  BEGIN ADD  ***  06/28/00  *********************************
     FWMACL13   IF   E           K DISK
     F***  END   ADD  ***  06/28/00  *********************************
     F*
     F***** BEG ADD *****  12/01/04  *********************************
     FWMACL1    IF   E           K DISK
     F                                     RENAME(PAC01:PAC02)
     F***** END ADD *****  12/01/04  *********************************
     F***** BEG ADD *****  12/21/06  *********************************
     FWMPHP     O    E             DISK
     F***** END ADD *****  12/21/06  *********************************
     F*
     FWTABL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXCKDT                 1      8  0
     D  DSMM                   1      2  0
     D  DSDD                   3      4  0
     D  DSYY                   5      8  0
     D                 DS
     D  X1CKDT                 1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   RESET IN30 FOR 10/09/91 REVISION
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    UPFND
     C*
     C***  BEGIN ADD  ***  06/28/00  *********************************
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  END   ADD  ***  06/28/00  *********************************
      *
      ***** BEG ADD *****  12/01/04  *********************************
     C     ACKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZPODT            7 0
      ***** END ADD *****  12/01/04  *********************************
      *
     C*** BEGIN DEL ***  04/06/98  ************************************
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C*****      F1KEY     KLIST
     C*****                KFLD           UPCO#
     C*****                KFLD           UPFND
     C*****                KFLD           XXFYR   30
     C*** END DEL *****  04/06/98  ************************************
     C*
     C***  BEGIN ADD  ***  07/23/98  *********************************
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C***  END   ADD  ***  07/23/98  *********************************
     C     *LIKE         DEFINE    FNDT          XXSCDT
     C     *LIKE         DEFINE    FNDT          XXDT
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C***  BEGIN ADD  ***  07/23/98  *********************************
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C***  END   ADD  ***  07/23/98  *********************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      FNFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C***  BEGIN ADD  ***  06/28/00  *********************************
     C                   MOVE      *OFF          *IN31
     C***  END   ADD  ***  06/28/00  *********************************
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***  BEGIN ADD  ***  07/23/98  *********************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C***  END   ADD  ***  07/23/98  *********************************
     C*
      **** BEGIN ADD **** 06/12/98 ************************************
     C     HFLD          IFNE      'SCCOCD'
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      **** END   ADD **** 06/12/98 ************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVEL     SCCOCD        XX2               2
     C     XX2           IFEQ      '? '
     C     HFLD          OREQ      'SCCOCD'
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      *BLANK        XXKEY            10
     C                   CALL      'W2201R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXKEY
     C*
     C                   MOVEL     XXKEY         SCCOCD
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCCOCD        OREQ      *BLANK
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      **** BEGIN ADD **** 06/12/98 ************************************
     C                   MOVE      '1'           *IN28
      **** END   ADD **** 06/12/98 ************************************
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C***  BEGIN ADD  ***  07/23/98  *********************************
     C                   Z-ADD     *ZERO         UPFND
     C***  END   ADD  ***  07/23/98  *********************************
     C                   END
     C*
     C***  BEGIN ADD  ***  07/23/98  *********************************
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C***  END   ADD  ***  07/23/98  *********************************
     C*
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   VALIDATE THE "FROM DATE"
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
      ***** BEG DEL ******* 05/19/00 **********************************
     C*********
     C*********  *IN30     IFEQ '0'
     C*********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY1
     C*********            MOVELSRDTO     XXCKDT
     C*********
     C*********            Z-ADDDSDD      XXDD
     C*********            Z-ADDDSMM      XXMM
     C*********            Z-ADDDSYY      XXYY
     C*********  XXMM      IFNE 1
     C*********            SUB  1         XXMM
     C*********            ELSE
     C*********            Z-ADD12        XXMM
     C*********            SUB  1         XXYY
     C*********            END
     C*********
     C*********            Z-ADD01        XXDD
     C*********
     C*********            Z-ADDSCFRDT    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     XXSCDT
     C*********
     C*********            Z-ADDX1CKDT    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     XXDT
     C*********
     C*********  XXSCDT    IFLT XXDT
     C*********            MOVE '1'       *IN30
     C*********            GOTO STRT
     C*********            END
     C*********
     C*********            END
     C*********
      ***** END DEL ******* 05/19/00 **********************************
     C*
     C*   VALIDATE THE "TO DATE"
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   FROM AND TO DATES MUST BE IN ASCENDING ORDER
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C     XXFRDT        CABGT     XXTODT        STRT                 27
     C*
      ***** BEG ADD ******* 05/19/00 **********************************
      *
     C     *IN30         IFEQ      '0'
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXCKDT
      *
     C                   Z-ADD     DSDD          XXDD
     C                   Z-ADD     DSMM          XXMM
     C                   Z-ADD     DSYY          XXYY
      *
     C     XXMM          IFNE      1
     C                   SUB       1             XXMM
      *
     C                   ELSE
      *
     C                   Z-ADD     12            XXMM
     C                   SUB       1             XXYY
     C                   ENDIF                                                  XXMM
      *
     C                   Z-ADD     01            XXDD
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSCDT
      *
     C                   Z-ADD     X1CKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
      ***** BEG ADD *****  12/01/04  *********************************
      * Check for commission records within the date range entered
     C*
     C     *IN32         IFEQ      *OFF
     C                   MOVE      *HIVAL        ZZPODT
     C     ACKY2         SETLL     PAC02
     C     ACKY          READPE    PAC02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ACPODT        IFLE      XXTODT
     C     ACPODT        ANDGT     XXFRDT
     C     ACCOTP        ANDEQ     'C'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF                                                  ACCOTP = C
     C     ACKY          READPE    PAC02                                  60
     C                   ENDDO
     C                   ENDIF                                                  *IN32=*OFF
      *
      ***** END ADD *****  12/01/04  *********************************
      *
     C     XXSCDT        IFLT      XXDT
     C                   MOVE      '1'           *IN30
     C***  BEGIN ADD  ***  06/28/00  *********************************
     C                   MOVE      *OFF          *IN31
     C***  END   ADD  ***  06/28/00  *********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  XXSCDT
      *
     C                   ENDIF                                                  *IN30
      *
      ***** END ADD ******* 05/19/00 **********************************
     C***  BEGIN ADD  ***  06/28/00  *********************************
      *
      ***** BEG DEL *****  10/22/03  *********************************
     C********   *IN31     IFEQ *OFF
      ***** END DEL *****  10/22/03  *********************************
     C***  Begin Add  ***  12/15/03  **************************************
     C     *IN31         IFEQ      *OFF
     C***  End   Add  ***  12/15/03  **************************************
     C*
     C     ACKY          SETGT     PAC01
     C     ACKY          READPE    PAC01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     ACPODT        ANDGE     XXTODT
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*OFF
     C*
      ***** BEG DEL *****  10/22/03  *********************************
     C*************        ENDIF                           *IN31=*OFF
      ***** END DEL *****  10/22/03  *********************************
     C***  Begin Add  ***  12/15/03  **************************************
     C                   ENDIF                                                  *IN31=*OFF
     C***  End   Add  ***  12/15/03  **************************************
      *
     C***  END   ADD  ***  06/28/00  *********************************
     C*** BEGIN DEL ***  04/06/98  ************************************
     C*
     C*   VALIDATE THE FUND YEAR
     C*
     C*****                Z-ADDSCYR      XXFYR
     C*****      F1KEY     CHAINPF101                25
     C*****      *IN25     IFEQ '1'
     C*****                MOVE *BLANK    Q2RECF
     C*****                MOVEL'SCR01'   Q2RECF
     C*****                GOTO STRT
     C*****                END
     C*** END DEL *****  04/06/98  ************************************
     C*
     C*   VALIDATE THE COMMISSION CODE
     C*
     C                   MOVEL     SCCOCD        XXKEY            10
     C     TBKY          CHAIN     PCS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE CODE TYPE - IT MUST BE A "C" TYPE CODE
     C*
     C                   MOVEL     CSTYPE        XXTYPE            1            *LJQ 03-10-88
     C*
     C     XXTYPE        IFNE      'C'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C     CSSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      ****BEG ADD *****  12/17/10  ************************************
     C     SCFND         IFEQ      0
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
      ****END ADD *****  12/17/10  ************************************
      ****BEG ADD *****  12/21/06  ************************************
     C                   CLEAR                   PPH01
     C                   MOVE      UPCO#         PHCO#
     C                   MOVE      SCFND         PHFND
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '     '       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHDT
      *
     C                   MOVE      DSTIME        PHTIME
     C                   MOVE      Q1USER        PHUSER
     C                   MOVE      Q1PGM         PHPGM
      *
     C                   Z-ADD     Q1DT          SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHIDT
     C                   MOVE      Q1TIME        PHITIM
     C                   MOVE      Q1USER        PHIUSR
     C                   MOVE      Q1PGM         PHIPGM
     C                   MOVE      'E'           PHTYPE
     C                   MOVE      SCFRDT        PHFRDT
     C                   MOVE      SCTODT        PHTODT
     C                   MOVE      SCCOCD        PHCOCD
     C                   MOVEL     SCCOCD        XXKEY            10
     C     TBKY          CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CSTYPE        PHCOTP
     C                   MOVE      CSDESC        PHCODS
     C                   ELSE
     C                   MOVE      ' '           PHCOTP
     C                   MOVE      ' '           PHCODS
     C                   ENDIF
     C                   WRITE     PPH01
      ****END ADD *****  12/21/06  ************************************
     C*
     C*  CALL THE CLP TO SUBMIT THE LIST.
     C*
     C***  BEGIN CHG  ***  07/23/98  *********************************
     C**********           MOVE UPCO#     QQCO#   3
     C                   MOVE      XXCO#         QQCO#             3
     C***  END   CHG  ***  07/23/98  *********************************
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C*** BEGIN CHG ***  04/06/98  ************************************
     C*****                MOVE SCYR      QQFYR   3
     C                   MOVE      '000'         QQFYR             3
     C*** END CHG *****  04/06/98  ************************************
     C                   MOVE      SCCOCD        QQCOCD            2
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W3010C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQFYR
     C                   PARM                    QQCOCD
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
