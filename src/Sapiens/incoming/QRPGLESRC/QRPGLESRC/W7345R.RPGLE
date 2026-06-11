     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7345R - PROMPT FOR AGENT-EMPLOYER LISTING
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM PROMPTS THE USER FOR INFO TO GENERATE A      *
     H*     LISTING OF ALL EMPLOYERS ACTIVELY ASSIGNED TO AN AGENT.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     MENU OPTION                                               *
     H*  CALLS TO:                                                    *
     H*     W7345C                                                    *
     H*                                                               *
     H*  CREATED:    01/11/93       PROGRAMMER: 11     PROJECT: WC027 *
     H*  CREATED:    01/15/98       PROGRAMMER: 33     PROJECT: CM030 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     F*                                                               *
     F*  05/06/10 C:235 PROJECT: 133WIP                               *
     F*                 Agent Employee List Report-Add Agent Status   *
     F*                 date                                          *
     H*                                                               *
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*  06/08/99 C:118 SUPPORT:  WA096/18374                         *
     H*                 MADE MODIFICATIONS TO KJDO SO WHEN F10=SEARCH *
     H*                 IS DONE ON AGENT NUMBER RANGE AND EMPLOYER    *
     H*                 NUMBER RANGE THE 'TO' VALUE WILL BE RETURNED. *
     H*                                                               *
     H*  06/04/98 C:33 SUPPORT                                        *
     H*                CORRECT F13 PROCESSING.                        *
     H*                                                               *
     H*  03/12/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FW7345F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMAAP     IF   E             DISK
     FWMEMP     IF   E             DISK

     D*
     D***  BEGIN ADD  ***  05/14/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/14/01  *********************************
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXRANG                 1     15
     D  XXREMP                 2     10  0
     D  XXRDIV                11     15  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D OPTNM           C                   CONST('Agent-Employer Listi-
     D                                     ng')

     I*_______________________________________________________________

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*** BEGIN ADD *** 03/12/98 **************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*** END   ADD *** 03/12/98 **************************************
     C*_______________________________________________________________
     C*
     C     *ENTRY        PLIST
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
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    Q2RECF        SVRECF
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    AADT          XXFRDT
     C     *LIKE         DEFINE    AADT          XXTODT
     C*_______________________________________________________________
     C*
     C*  LOAD THE PROGRAM TITLE
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        SCPRDS
     C*
     C***  BEG   ADD ***  05/06/10  **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '133'
     C                   MOVE      *ON           *IN45
     C                   ELSE
     C                   MOVE      *OFF          *IN45
     C                   ENDIF
     C***  END   ADD ***  05/06/10  **********************************
     C***  BEGIN ADD ***  06/04/98  **********************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C                   MOVE      'Y'           SCCXAG
     C*
     C     TOP           TAG
     C***  END   ADD ***  06/04/98  **********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         SCFND
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         SCFND
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C***  BEGIN DEL ***  06/04/98  **********************************
     C**********           TIME           DSDTTM
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCRNDT
     C**********
     C**********           MOVE 'Y'       SCCXAG
     C***  END   DEL ***  06/04/98  **********************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
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
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*===============================================================
     C*  F3=EXIT
     C*===============================================================
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*===============================================================
     C*  F10=SEARCH
     C*===============================================================
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   EXSR      KJDO
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   *INKJ=*ON
     C*
     C*===============================================================
     C*  F12=CANCEL
     C*===============================================================
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   GOTO      TERM
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*** BEGIN ADD *** 03/12/98 **************************************
     C*===============================================================
     C*  F13=CHANGE USER PARAMETERS
     C*===============================================================
     C*
     C*  F13 = PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'
     C*
     C***  BEGIN CHG ***  06/04/98  **********************************
     C*****GET*THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C**********
     C********** *IN60     IFEQ *ON
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *BLANK    SCCONM
     C**********           MOVEL*ZERO     SCFND
     C**********           MOVEL*BLANK    SCNAME
     C**********
     C**********           ENDIF                           *IN60=ON
     C                   GOTO      TOP
     C***  END   CHG ***  06/04/98  **********************************
     C*
     C                   ENDIF                                                  *INKM=ON
     C*** END   ADD *** 03/12/98 **************************************
     C*
     C*===============================================================
     C*  PROCESS HELP REQUESTS
     C*===============================================================
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*______________________________________________________________
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  FROM DATE MUST BE VALID
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  TO DATE MUST BE VALID
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE IN ORDER
     C*
     C     XXTODT        CABLT     XXFRDT        STRT                   22
     C*
     C*  EMPLOYER SELECTION MUST BE 'A', 'C' OR 'B'
     C*
     C     SCEMSL        IFNE      'A'
     C     SCEMSL        ANDNE     'C'
     C     SCEMSL        ANDNE     'B'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMSL<>A/C/B
     C*
     C*  EFFECTIVE-AS-OF DATE MUST BE VALID
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C******BEG ADD******05/06/10**********************************************
     C*  AGENT STATUS AS-OF DATE MUST BE VALID
     C*
     C     *IN45         IFEQ      *ON
     C     SCASDT        IFNE      *ZEROS
     C                   Z-ADD     SCASDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCASDT
     C                   END
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C******BEG ADD******05/06/10**********************************************
     C*
     C*  PRIMARY SORT MUST BE 'EN', 'EA', 'AN' OR 'AA'
     C*
     C     SCSORT        IFNE      'EN'
     C     SCSORT        ANDNE     'EA'
     C     SCSORT        ANDNE     'AN'
     C     SCSORT        ANDNE     'AA'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSORT<>EN/EA/AN/AA
     C*
     C*  FIELD MUST NOT BE ENTERED UNLESS PRIMARY SORT IS 'AA' OR 'AN'
     C*
     C     SCSORT        IFNE      'AN'
     C     SCSORT        ANDNE     'AA'
     C*
     C     SCCXAG        IFEQ      *BLANKS
     C                   MOVE      'Y'           SCCXAG
     C                   ENDIF                                                  SCCXAG=*BLANKS
     C*
     C     SCSRT2        IFNE      *BLANKS
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCXAG<>*BLANKS
     C*
     C                   ENDIF                                                  SCSORT<>AN/AA
     C*
     C*  SECONDARY SORT MUST BE 'EN' OR 'AA', IF ENTERED
     C*
     C     SCSRT2        IFNE      'EN'
     C     SCSRT2        ANDNE     'EA'
     C     SCSRT2        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSRT2<>EN/EA
     C*
     C*  INCLUDE CX AGENTS MUST BE 'Y' OR 'N'
     C*
     C     SCCXAG        IFNE      'Y'
     C     SCCXAG        ANDNE     'N'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCXAG<>Y/N
     C*
     C*  ONLY ONE EMPLR RANGE CAN BE SELECTED AND IT MUST BE IN ORDER
     C*
     C     SCEMNT        IFNE      *BLANKS
     C     SCEM#T        ANDNE     *ZEROS
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMNT<>*BLANKS
     C*
     C     SCEMNT        CABLT     SCEMNF        STRT                   28
     C*
     C     SCEM#T        CABLT     SCEM#F        STRT                   29
     C*
     C*  ONLY ONE AGENT RANGE CAN BE SELECTED AND IT MUST BE IN ORDER
     C*
     C     SCAGNT        IFNE      *BLANKS
     C     SCAG#T        ANDNE     *ZEROS
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   ENDIF                                                  SCAGNT<>*BLANKS
     C*
     C     SCAGNT        CABLT     SCAGNF        STRT                   30
     C*
     C     SCAG#T        CABLT     SCAG#F        STRT                   31
     C*
     C*  TYPE OF OUTPUT MUST BE 'P', 'F' OR 'B'
     C*
     C     SCTYPO        IFNE      'P'
     C     SCTYPO        ANDNE     'F'
     C     SCTYPO        ANDNE     'B'
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTYPO<>P/F/B
     C*_______________________________________________________________
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   MOVE      SCRNDT        QQRNDT
     C                   MOVE      SCEM#F        QQEM#F
     C                   MOVE      SCEM#T        QQEM#T
     C                   MOVE      SCAG#F        QQAG#F
     C                   MOVE      SCAG#T        QQAG#T
     C*
     C                   MOVEL     *BLANKS       QQTP1
     C                   MOVE      *BLANKS       QQFR1
     C                   MOVE      *BLANKS       QQTO1
     C                   MOVEL     *BLANKS       QQTP2
     C                   MOVE      *BLANKS       QQFR2
     C                   MOVE      *BLANKS       QQTO2
     C                   MOVEL     *BLANKS       XXTP              2
     C                   MOVE      *BLANKS       XXFR             15
     C                   MOVE      *BLANKS       XXTO             15
     C*
     C                   SELECT
     C     SCAGNT        WHENNE    *BLANKS
     C                   MOVEL(P)  'AA'          QQTP1
     C                   MOVEL(P)  SCAGNF        QQFR1
     C                   MOVEL(P)  SCAGNT        QQTO1
     C     SCAG#T        WHENNE    *ZEROS
     C                   MOVEL(P)  'AN'          QQTP1
     C                   MOVE      SCAG#F        QQFR1
     C                   MOVE      SCAG#T        QQTO1
     C                   ENDSL
     C*
     C                   SELECT
     C     SCEMNT        WHENNE    *BLANKS
     C                   MOVE      'EA'          XXTP
     C                   MOVEL(P)  SCEMNF        XXFR
     C                   MOVEL(P)  SCEMNT        XXTO
     C     SCEM#T        WHENNE    *ZEROS
     C                   MOVE      'EN'          XXTP
     C                   MOVE      *BLANKS       XXRANG
     C                   MOVE      SCEM#F        XXREMP
     C                   Z-ADD     0             XXRDIV
     C                   MOVE      XXRANG        XXFR
     C                   MOVE      SCEM#T        XXREMP
     C                   Z-ADD     99999         XXRDIV
     C                   MOVE      XXRANG        XXTO
     C                   ENDSL
     C*
     C     XXTP          IFNE      *BLANKS
     C*
     C     QQTP1         IFEQ      *BLANKS
     C                   MOVEL(P)  XXTP          QQTP1
     C                   MOVEL(P)  XXFR          QQFR1
     C                   MOVEL(P)  XXTO          QQTO1
     C                   ELSE
     C                   MOVEL(P)  XXTP          QQTP2
     C                   MOVEL(P)  XXFR          QQFR2
     C                   MOVEL(P)  XXTO          QQTO2
     C                   ENDIF                                                  QQTP1=*BLANKS
     C*
     C                   ENDIF                                                  XXTP=*BLANKS
     C*******BEG ADD********05/06/10*******************************************
     C     SCASDT        IFEQ      *ZEROS
     C                   Z-ADD     *HIVAL        SCASDT
     C                   ENDIF
     C     CLTID         IFEQ      '133'
     C                   MOVE      SCASDT        QQASDT
     C                   ENDIF
     C*******BEG ADD********05/06/10*******************************************
     C*
     C                   CALL      'W7345C'                             99
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRNDT            8
     C                   PARM      SCEMSL        QQEMSL            1
     C                   PARM      'B'           QQEMTP            1
     C                   PARM      'E'           QQACSL            1
     C                   PARM      SCSORT        QQSORT            2
     C                   PARM      SCSRT2        QQSRT2            2
     C                   PARM      'M'           QQEMFL            1
     C                   PARM      'M'           QQAGFL            1
     C                   PARM      'EI'          QQPRTP            2
     C                   PARM      *BLANKS       QQEMP#            9
     C                   PARM      *BLANKS       QQDIV             5
     C                   PARM      *BLANKS       QQFYR             3
     C                   PARM                    QQTP1             2
     C                   PARM                    QQFR1            15
     C                   PARM                    QQTO1            15
     C                   PARM                    QQTP2             2
     C                   PARM                    QQFR2            15
     C                   PARM                    QQTO2            15
     C                   PARM      SCEMNF        QQEMNF            2
     C                   PARM      SCEMNT        QQEMNT            2
     C                   PARM                    QQEM#F            9
     C                   PARM                    QQEM#T            9
     C                   PARM      SCEMNF        QQAGNF            2
     C                   PARM      SCEMNT        QQAGNT            2
     C                   PARM                    QQAG#F            5
     C                   PARM                    QQAG#T            5
     C                   PARM      SCTYPO        QQTYPO            1
     C                   PARM      SCCXAG        QQCXAG            1
     C                   PARM      UPOUTQ        QQOUTQ           10
     C*******BEG ADD********05/06/10*******************************************
     C                   PARM                    QQASDT            8
     C*******END ADD********05/06/10*******************************************
     C*_______________________________________________________________
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*===============================================================
     C*  KJDO - SUBROUTINE TO PROCESS SEARCHES
     C*===============================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*  AGENT SEARCH
     C*
     C     HFLD          WHENEQ    'SCAGNF'
     C     HFLD          OREQ      'SCAGNT'
     C     HFLD          OREQ      'SCAG#F'
     C     HFLD          OREQ      'SCAG#T'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCAG#F'
     C     HFLD          OREQ      'SCAG#T'
     C                   Z-ADD     2             ##VIEW
     C                   OTHER
     C                   Z-ADD     1             ##VIEW
     C                   ENDSL
     C*
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM                    ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##RRN         IFEQ      *ZERO
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCAGNF'
     C                   MOVE      *ON           *IN74
     C     HFLD          WHENEQ    'SCAGNT'
     C                   MOVE      *ON           *IN75
     C     HFLD          WHENEQ    'SCAG#F'
     C                   MOVE      *ON           *IN76
     C     HFLD          WHENEQ    'SCAG#T'
     C                   MOVE      *ON           *IN77
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     WMAAP                              99
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCAGNF'
     C                   MOVE      *ON           *IN75
     C                   MOVEL(P)  AANAME        SCAGNF
     C     HFLD          WHENEQ    'SCAGNT'
     C                   MOVE      *ON           *IN76
     C                   MOVEL(P)  AANAME        SCAGNT
     C     HFLD          WHENEQ    'SCAG#F'
     C                   MOVE      *ON           *IN77
     C                   Z-ADD     AAAGT#        SCAG#F
     C***** BEGIN CHG **** 06/08/99 ***********************************
     C********** HFLD      WHEQ 'SCAG#F'
     C     HFLD          WHENEQ    'SCAG#T'
     C**********           MOVE *ON       *IN70
     C                   MOVE      *ON           *IN78
     C**********           Z-ADDAAAGT#    SCAG#F
     C                   Z-ADD     AAAGT#        SCAG#T
     C***** END CHG ****** 06/08/99 ***********************************
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##RRN=*ZERO
     C*_______________________________________________________________
     C*
     C*  EMPLOYER SEARCH
     C*
     C     HFLD          WHENEQ    'SCEMNF'
     C     HFLD          OREQ      'SCEMNT'
     C     HFLD          OREQ      'SCEM#F'
     C     HFLD          OREQ      'SCEM#T'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCEM#F'
     C     HFLD          OREQ      'SCEM#T'
     C                   Z-ADD     2             ##VIEW
     C                   OTHER
     C                   Z-ADD     1             ##VIEW
     C                   ENDSL
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM                    ##RRN
     C                   PARM      DBRRNF        ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##RRN         IFEQ      *ZERO
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCEMNF'
     C                   MOVE      *ON           *IN70
     C     HFLD          WHENEQ    'SCEMNT'
     C                   MOVE      *ON           *IN71
     C     HFLD          WHENEQ    'SCEM#F'
     C                   MOVE      *ON           *IN72
     C     HFLD          WHENEQ    'SCEM#T'
     C                   MOVE      *ON           *IN73
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     WMEMP                              99
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCEMNF'
     C                   MOVE      *ON           *IN71
     C                   MOVEL(P)  EMNAME        SCEMNF
     C     HFLD          WHENEQ    'SCEMNT'
     C                   MOVE      *ON           *IN72
     C                   MOVEL(P)  EMNAME        SCEMNT
     C     HFLD          WHENEQ    'SCEM#F'
     C                   MOVE      *ON           *IN73
     C                   Z-ADD     EMEMP#        SCEM#F
     C***** BEGIN CHG **** 06/08/99 ***********************************
     C********** HFLD      WHEQ 'SCEM#F'
     C     HFLD          WHENEQ    'SCEM#T'
     C**********           MOVE *ON       *IN78
     C                   MOVE      *ON           *IN74
     C**********           Z-ADDEMEMP#    SCEM#F
     C                   Z-ADD     EMEMP#        SCEM#T
     C***** END   CHG **** 06/08/99 ***********************************
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##RRN=*ZERO
     C*
     C                   ENDSL
     C*
     C                   ENDSR
