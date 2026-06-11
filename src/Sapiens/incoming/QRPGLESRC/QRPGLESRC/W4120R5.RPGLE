     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4120R5 - PROMPT FOR EXPERIENCE BY PAYROLL CLASS REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/23/05 C:33  Project: WC1252                                *
     H*                Use Q1PRMS.                                    *
     H*                                                               *
     H* 07/29/04 C:61  PROJECT WC741                                  *
     H*              - Add selection by "Group range".                *
     H*                                                               *
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 03/16/00 C:61  PROJECT WC741                                  *
     H*              - Policy effective date range instead of         *
     H*                policy year.                                   *
     H*              - Prompt to use S/R payroll (or not).            *
     H*                                                               *
     H* 11/29/99  C:118 PROJECT: WC577                                *
     H*                 V3 screen fixes.                              *
     H*                                                               *
     H* 04/28/99  C:90 PROJ: 96WBK                                    *
     H*           ADD ENTRY OF NEW FIELD 'REPORT OUTPUT'. ONLY ALLOW  *
     H*           ENTRY OF TYPES 'P', 'F', OR 'B'.                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW4120F5   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      ***** BEG DEL ******* 03/16/00 **********************************
     F*WDF1L   IF  E           K        DISK
      ***** END DEL ******* 03/16/00 **********************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMPRL     IF   E           K DISK
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PPR02)

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***  Begin Add  ***  05/23/05  **************************************
     D OPTNM           C                   CONST('Experience by -
     D                                     Payroll Class Report')
     D***  End   Add  ***  05/23/05  **************************************
     D***  BEGIN ADD  ***  05/14/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/14/01  *********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ******* 07/29/04 **********************************
      *
      *
      *
      *==================================================================
      * KLIST
      *==================================================================
      *
      *  Group Master
      *

     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      ***** END ADD ******* 07/29/04 **********************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      ***** BEG DEL ******* 03/16/00 **********************************
     C*********
     C****KEY*LIST FOR THE FUND DETAIL
     C*********
     C*********  F1KEY     KLIST
     C*********            KFLD           UPCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXFYR
      ***** END DEL ******* 03/16/00 **********************************
     C*
     C*   KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCCLAS
     C*
      ***** BEG ADD ******* 03/16/00 **********************************
     C     *LIKE         DEFINE    FNEFDT        XXFRDT
     C     *LIKE         DEFINE    FNEXDT        XXTODT
      ***** END ADD ******* 03/16/00 **********************************
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
      ***** BEG DEL ******* 07/29/04 **********************************
     C*********            MOVE UPFND     SCFND
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG DEL ******* 03/16/00 **********************************
     C******************   MOVE UPFYR     SCFYR
      ***** END DEL ******* 03/16/00 **********************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C***  Begin Add  ***  05/23/05  **************************************
     C     Q1PRMS        IFGE      1
     C     QQPRDS        ANDNE     *BLANKS
     C***  End   Add  ***  05/23/05  **************************************
     C                   MOVE      QQPRDS        SCPRDS
     C***  Begin Add  ***  05/23/05  **************************************
     C                   ELSE
     C                   MOVEL     OPTNM         SCPRDS
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPRDS        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C***  End   Add  ***  05/23/05  **************************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
      *
      ***** BEG DEL ******* 07/29/04 **********************************
     C*********
     C***LOAD*FUND NAME
     C*********
     C*********            MOVE *BLANK    SCNAME
     C*********            MOVE SCFND     XXFND
     C*********  FNKEY     CHAINPFN01                60
     C*********  *IN60     IFEQ '0'
     C*********            MOVELFNNAME    SCNAME
     C*********            END
      ***** END DEL ******* 07/29/04 **********************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEG ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/14/01 ********************************************
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
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     *INKJ         IFEQ      *ON
      ***** BEG ADD ******* 07/29/04 **********************************
      *
      * FROM Group
      *
     C     HFLD          IFEQ      'SCFFND'
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
      *
     C     ##FND         IFNE      *BLANKS
     C                   MOVE      ##FND         SCFFND
     C                   SETON                                        32
     C                   ELSE
     C                   SETON                                        31
     C                   ENDIF                                                  ##FND
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFFND
      *
      *  TO Group
      *
     C     HFLD          IFEQ      'SCTFND'
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
      *
     C     ##FND         IFNE      *BLANKS
     C                   MOVE      ##FND         SCTFND
     C                   SETON                                        33
     C                   ELSE
     C                   SETON                                        32
     C                   ENDIF                                                  ##FND
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTFND
      *
      *
      ***** END ADD ******* 07/29/04 **********************************
     C*
      ***** BEG DEL ******* 03/16/00 **********************************
     C*********  HFLD      IFEQ 'SCFYR'
     C*********
     C*********            CALL 'W0011R'
     C*********  *INKC     PARM           @@KC    1
     C*********  *INKL     PARM           @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM DBRRN     @@RRN   90
     C*********            PARM *ZEROS    @@YR    30
     C*********
     C*********  *INKL     IFEQ *ON
     C*********  @@YR      OREQ *ZEROS
     C*********
     C*********            SETON                     70
     C*********
     C*********            ELSE
     C*********
     C*********            MOVE @@YR      SCFYR
     C*********            SETON                     71
     C*********
     C*********            ENDIF                           *INKL EQ *ON
     C*********
     C*********            GOTO STRT
     C*********
     C*********            ENDIF                           HFLD SCFYR
      ***** END DEL ******* 03/16/00 **********************************
     C*
     C     HFLD          IFEQ      'SCCLAS'
     C*
     C                   CALL      'W0630R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      *BLANKS       @@RTCD            3
     C                   PARM      2             @@VIEW            1 0
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        71
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     PPR02                              99
     C*
     C                   MOVE      PRCLAS        SCCLAS
     C                   SETON                                        72
     C*
     C                   ENDIF                                                  *INKL *ON
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD SCCLAS
     C*
      **** BEGIN ADD **** 04/28/99 ************************************
      ***** BEG DEL ******* 03/16/00 **********************************
     C*********  HFLD      IFNE 'SCFYR'
     C*********  HFLD      ANDNE'SCCLAS'
      ***** END DEL ******* 03/16/00 **********************************
      ***** BEG ADD ******* 03/16/00 **********************************
     C     HFLD          IFNE      'SCCLAS'
      ***** END ADD ******* 03/16/00 **********************************
     C                   SETON                                        74
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD NE SCFYR
      **** END   ADD **** 04/28/99 ************************************
     C                   ENDIF                                                  *INKJ
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
      ***** BEG DEL ******* 07/29/04 **********************************
     C*********            MOVE UPFND     SCFND
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG DEL ******* 03/16/00 **********************************
     C******************   MOVE UPFYR     SCFYR
      ***** END DEL ******* 03/16/00 **********************************
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      ***** BEG ADD ******* 07/29/04 **********************************
      *
      *  Validate group range
      *
     C     SCFFND        IFNE      *ZERO
     C                   MOVE      SCFFND        XXFND
     C     FNKY          CHAIN     PFN01                              51
     C   51              GOTO      STRT
     C                   ENDIF
      *
     C     SCTFND        IFNE      *ZERO
     C                   MOVE      SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              52
     C   52              GOTO      STRT
     C                   ENDIF
      *
     C     SCTFND        IFLT      SCFFND
     C                   MOVE      *ON           *IN53
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 07/29/04 **********************************
      ***** BEG DEL ******* 03/16/00 **********************************
     C*********
     C***VALIDATE THE FUND YEAR FOR THE FUND
     C*********
     C*********            MOVE SCFYR     XXFYR   30
     C*********  F1KEY     CHAINPF101                21
     C*********  *IN21     CABEQ'1'       STRT
      ***** END DEL ******* 03/16/00 **********************************
      ***** BEG ADD ******* 03/16/00 **********************************
      *
      *  FROM DATE MUST BE VALID
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
      *
      *  TO DATE MUST BE VALID
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
      *
      *  DATES MUST BE ASCENDING
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C     XXTODT        CABLT     XXFRDT        STRT                   77
      *
      ***** END ADD ******* 03/16/00 **********************************
     C*
     C*  VALIDATE PAYROLL CLASS IF NOT EQUAL TO 0
     C*
     C     SCCLAS        IFNE      0
     C     PRKEY         CHAIN     PPR01                              22
     C     *IN22         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      ***** BEG ADD ******* 03/16/00 **********************************
      *
      * 'S/R Used' must be Y or N
      *
     C     SCSRPR        IFNE      'Y'
     C     SCSRPR        ANDNE     'N'
     C                   SETON                                        78
     C                   ENDIF                                                  SCSRPR
      *
      ***** END ADD ******* 03/16/00 **********************************
      **** BEGIN ADD **** 04/28/99 ************************************
      *
      * VALIDATE REPORT OUTPUT SELECTION
      *
     C     SCRPT         IFNE      'P'
     C     SCRPT         ANDNE     'F'
     C     SCRPT         ANDNE     'B'
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPT
      **** END   ADD **** 04/28/99 ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
      ***** BEG DEL ******* 07/29/04 **********************************
     C*********            MOVE SCFND     QQFND   3
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG ADD ******* 07/29/04 **********************************
      *
     C     SCTFND        IFEQ      *ZERO
     C                   MOVE      999           SCTFND
     C                   ENDIF
      *
     C                   MOVE      SCFFND        QQFFND            3
     C                   MOVE      SCTFND        QQTFND            3
      ***** END ADD ******* 07/29/04 **********************************
      ***** BEG DEL ******* 03/16/00 **********************************
     C****************     MOVE SCFYR     QQFYR   3
      ***** END DEL ******* 03/16/00 **********************************
     C                   MOVE      SCCLAS        QQCLAS            5
     C                   MOVE      SCDTE         QQDTE             8
      **** BEGIN ADD **** 04/28/99 ************************************
     C                   MOVE      SCRPT         QQRPT             1
      **** END   ADD **** 04/28/99 ************************************
      ***** BEG ADD ******* 03/16/00 **********************************
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
      *
      ***** END ADD ******* 03/16/00 **********************************
     C                   CALL      'W4120C5'
     C                   PARM                    QQCO#
      ***** BEG DEL ******* 07/29/04 **********************************
     C*************        PARM           QQFND
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG ADD ******* 07/29/04 **********************************
     C                   PARM                    QQFFND            3
     C                   PARM                    QQTFND            3
      ***** END ADD ******* 07/29/04 **********************************
      ***** BEG DEL ******* 03/16/00 **********************************
     C**********           PARM           QQFYR
      ***** END DEL ******* 03/16/00 **********************************
     C                   PARM                    QQCLAS
     C                   PARM                    QQDTE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
      **** BEGIN ADD **** 04/28/99 ************************************
     C                   PARM                    QQRPT
      **** END   ADD **** 04/28/99 ************************************
      ***** BEG ADD ******* 03/16/00 **********************************
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    SCSRPR
      ***** END ADD ******* 03/16/00 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
