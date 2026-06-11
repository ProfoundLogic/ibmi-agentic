     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7195R1 - PROMPT FOR MEMBER EXPERIENCE REPORT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM PROMPTS THE USER FOR VALUES NEEDED TO RUN THE    *
     H* MEMBER EXPERIENCE REPORT.  VALUES PROMPTED FOR ARE ACCIDENT   *
     H* DATE RANGE, RUN DATE, ONE OR ALL EMPLOYERS/DIV, ONE OR ALL    *
     H* REGIONS, ONE OR ALL COUNTIES, ONE OR ALL AGENTS, ONE OR ALL   *
     H* ZIP CODES, OPEN/CLOSED CASES, AND SORT SEQUENCE FOR THE       *
     H* REPORT. SORT SEQUENCES INCLUDE EMPLOYER NUMBER, EMPLOYER      *
     H* NAME, REGION, COUNTY, AGENT AND ZIP CODE.                     *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*   W7195C1                                                     *
     H*   W2217C                                                      *
     H*                                                               *
     H* PROGRAMMER: C:33   CREATED: 05/17/98     PROJECT: WC701       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/11/07 C: 132 81WYS        (WC1318 in W7190R thru W7193R)  *
     H*                  Add parameter 'Print SS#.' (also is WC1318)  *
     H*                  Print SS# will default to "N" and SS# will   *
     H*                  not appear on report unless changed to a "Y" *
     H*                                                               *
     H*  08/15/05  C:15  Support: WC102/48047                         *
     H*                  Change program name + suffix '1'             *
     H*                                                               *
     H*  10/16/03  C:132 PROJECT: 121WAS                              *
     H*                  Add additional fields and selections so      *
     H*                  repot is compliant with Houstion Casualty's  *
     H*                  requirements.                                *
     H*                                                               *
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*  11/24/99  C:33  PROJECT: WC960                                *
     H*                  USE SCDIV AS AN ALPHA FIELD TO VERIFY THAT THE*
     H*                  USER WOULD LIKE ONE OR ALL DIVISIONS.         *
     H*****************************************************************
     F***  BEGIN CHG  ***  08/15/05  *********************************
     F***************W7195F  CF  E                    WORKSTN
     FW7195F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F***  END CHG  *****  08/15/05  *********************************
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEML     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM01P)
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA01P)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D***  BEGIN ADD  ***  05/14/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/14/01  *********************************
     D*
     D DBFDSF          DS
     D  FNRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Member Experience Re-
     D                                     port')
     D COMAPI          C                   CONST('WA001R')
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D***  BEGIN ADD  ***  11/24/99  *********************************
     D                 DS
     D  SCDIV                  1      5
     D  XXDIV1                 1      1
     D  XXDIV2                 2      2
     D  XXDIV3                 3      3
     D  XXDIV4                 4      4
     D  XXDIV5                 5      5
     D***  END   ADD  ***  11/24/99  *********************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C***  BEGIN CHG  ***  11/24/99  *********************************
     C**********           KFLD           SCDIV
     C                   KFLD                    XXDIV
     C***  END   CHG  ***  11/24/99  *********************************
     C*
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C     EMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C*  REGION TABLE FILE
     C*
     C     REKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCRGN#
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C*
     C     *LIKE         DEFINE    Q2RECF        SVRECF
     C     *LIKE         DEFINE    *INKC         ##KC
     C     *LIKE         DEFINE    *INKL         ##KL
     C*******    *LIKE     DEFN *IN90     OUTFLG
     C     *LIKE         DEFINE    EMDT          X7DAT1
     C     *LIKE         DEFINE    EMDT          X7DAT2
     C     *LIKE         DEFINE    CNKEY         ##KEY
     C     *LIKE         DEFINE    RERGN#        #@KEY
     C     *LIKE         DEFINE    FNMST         ##ST
     C     *LIKE         DEFINE    SCSELO        ##SELO
     C     *LIKE         DEFINE    SCSELC        ##SELC
     C     *LIKE         DEFINE    SCACFL        ##ACFL
     C     *LIKE         DEFINE    SCDPLX        ##DPLX
     C     *LIKE         DEFINE    SCTYPO        ##TYPO
     C     *LIKE         DEFINE    SCSEQ         ##SEQ
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    SCEMP#        SVEMP#
     C     *LIKE         DEFINE    SCDIV         SVDIV
     C     *LIKE         DEFINE    *IN36         XXSAME
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
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
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@CALL        XXCALL            1
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   MOVE      '0'           QQCRTF            1
     C                   MOVE      *HIVAL        SCTPRM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SCRNDT
     C*
     C                   MOVE      '0'           ##CRTF            1
     C*_______________________________________________________________
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  GET USER PARAMETER VALUES
     C*
     C                   EXSR      GETCOF
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
      /Free
       // Intialising the Check-Box Value.
       scACFL = 'N';
       scSELO = 'N';
       scSELC = 'N';
       scSELN = 'N';
       scDPLX = ' ';
       scPRSS = ' ';
      /End-Free
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     *IN90         DOUEQ     *OFF
     C     OUTFLG        ANDEQ     *OFF
     C*
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *OFF          OUTFLG            1
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL F KEYS
     C*
     C                   EXSR      FKEYS
     C*
     C     *INKC         IFNE      *ON
     C     *IN90         ANDNE     *ON
     C*
     C*  PROCESS REQUEST SCREENS
     C*
     C     Q2RECF        CASEQ     'SCR01'       SCR01T
     C                   ENDCS
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*  CALL THE CLP TO SUBMIT THE LIST
     C*
     C                   EXSR      CALLCL
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDIF                                                  *INKC<>*ON/*IN90<>*O
     C*
     C                   ENDDO                                                  *IN90=*OFF
     C*
     C     *INKC         IFEQ      *ON
     C     ##CRTF        ANDEQ     '1'
     C                   MOVE      'D'           ##CRTF
     C                   EXSR      KPDO
     C                   ENDIF                                                  *INKC=*ON/##CRTF=1
     C*_______________________________________________________________
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
     C*
     C*================================================================
     C*  FKEYS  - Process all F keys
     C*================================================================
     C*
     C     FKEYS         BEGSR
     C*
     C     *IN11         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C     *IN10         CASEQ     *ON           HELP                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO                     90
     C     *INKP         CASEQ     *ON           KPDO                     90
     C                   ENDCS
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*
     C                   ENDSR                                                  FKEYS
     C*
     C*================================================================
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*================================================================
     C*
     C     GETCOF        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
B01  C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  PDSNAM        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        ##CONM
     C*
E01  C                   ENDIF                                                  *IN99=*ON
     C*
     C                   MOVEL(P)  ##CONM        SCCONM
     C*
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCNAME
     C*
     C*  GET GROUP INFO
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   ENDSR                                                  GETCOF
     C*
     C*================================================================
     C*  HELP   - Help Processing
     C*================================================================
     C*
     C     HELP          BEGSR
     C*
     C                   MOVE      Q2RECF        XXRECF           10
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
     C                   ENDSR                                                  HELP
     C*
     C*================================================================
     C*  KJDO   - F10 Processing
     C*================================================================
     C*
     C*  F10=Search
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C*  EMPLOYER SEARCH
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      FNRRN         ##RRN2            9 0
     C                   PARM      ' '           ##QTE             1
     C*
     C     ##RRN         CHAIN     WMEMP                              60
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN40
     C                   ELSE
     C                   Z-ADD     EMEMP#        SCEMP#
     C***  BEGIN CHG  ***  11/24/99  *********************************
     C**********           Z-ADDEMDIV     SCDIV
     C                   MOVE      EMDIV         SCDIV
     C***  END   CHG  ***  11/24/99  *********************************
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  ##RRN=*ZEROS
     C*_______________________________________________________________
     C*
     C*  REGION SEARCH
     C*
     C     HFLD          WHENEQ    'SCRGN#'
     C*
     C                   CALL      'W0520R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      FNRRN         ##RRN2
     C                   PARM      *ZEROS        #@KEY
     C*
     C     #@KEY         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C                   Z-ADD     #@KEY         SCRGN#
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  #@KEY=*ZEROS
     C*_______________________________________________________________
     C*
     C*  COUNTY SEARCH
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C*
     C                   CALL      'W2215R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      *BLANKS       ##KEY
     C                   PARM      FNMST         ##ST
     C*
     C     ##KEY         IFEQ      *BLANK
     C                   MOVE      *ON           *IN42
     C                   ELSE
     C     2             SUBST     ##KEY:3       SCCNTY
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  ##KEY=*BLANK
     C*_______________________________________________________________
     C*
     C*  AGENT SEARCH
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C*
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      FNRRN         ##RRN
     C*
     C     ##RRN         CHAIN     WMAAP                              60
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN43
     C                   ELSE
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN44
     C                   ENDIF                                                  ##RRN=*ZEROS
     C*_______________________________________________________________
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN71
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C*  KMDO   - F13 Processing
     C*================================================================
     C*
     C*  F13=Change user parms
     C*
     C     KMDO          BEGSR
     C*
     C                   CALL      'W0006R'                             99
     C*
     C                   EXSR      GETCOF
     C*
     C                   ENDSR                                                  KMDO
     C*
     C*================================================================
     C*  KPDO   - F15 Processing
     C*================================================================
     C*
     C*  F15=Select county codes
     C*
     C     KPDO          BEGSR
     C*
     C                   CALL      'W2217C'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      FNMST         ##ST
     C                   PARM                    ##MBR            10
     C                   PARM                    ##CRTF
     C*
     C     ##CRTF        IFEQ      '0'
     C                   MOVE      *ON           *IN42
     C                   ELSE
     C                   MOVEL(P)  '*S'          SCCNTY
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  ##CRTF='0'
     C*
     C                   MOVE      *INKC         OUTFLG
     C*
     C                   ENDSR                                                  KPDO
     C*
     C*================================================================
     C*  CALLCL - Call the submitter CL
     C*================================================================
     C*
     C     CALLCL        BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#             3
     C                   MOVE      ##FND         #@FND             3
     C                   MOVE      SCAFDT        ##AFDT            8
     C                   MOVE      SCATDT        ##ATDT            8
     C                   MOVE      SCRNDT        ##RNDT            8
     C                   MOVE      SCEMP#        ##EMP#            9
     C                   MOVE      SCDIV         ##DIV             5
     C                   MOVE      SCRGN#        ##RGN#            3
     C                   MOVE      SCCNTY        ##CNTY            2
     C                   MOVE      SCAGT#        ##AGT#            5
     C                   MOVE      SCZIP         ##ZIP             5
     C                   MOVE      SCMNTH        ##MNTH            3
     C                   MOVE      SCFPRM        ##FPRM           11
     C                   MOVE      SCTPRM        ##TPRM           11
      ***** BEG ADD ***** 10/16/03 **************************************
     C                   MOVE      SCPFDT        ##PFDT            8
     C                   MOVE      SCPTDT        ##PTDT            8
      ***** END ADD ***** 10/16/03 **************************************
     C*
      ***** BEG ADD ***** 01/11/07 ***********************************
     C                   MOVE      SCPRSS        ##PRSS            1
      ***** END ADD ***** 01/11/07 ***********************************
     C*
     C***** BEG CHG ***** 08/15/05 **************************************
     C****************     CALL 'W7195C'
     C                   CALL      'W7195C1'
     C***** END CHG ***** 08/15/05 **************************************
     C                   PARM                    #@CO#
     C                   PARM                    #@FND
     C                   PARM                    ##AFDT
     C                   PARM                    ##ATDT
     C                   PARM                    ##RNDT
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##RGN#
     C                   PARM                    ##CNTY
     C                   PARM                    ##AGT#
     C                   PARM                    ##ZIP
     C                   PARM      SCSELO        ##SELO
     C                   PARM      SCSELC        ##SELC
     C                   PARM                    ##MNTH
     C                   PARM      SCSEQ         ##SEQ
     C                   PARM                    ##MBR
     C                   PARM      SCACFL        ##ACFL
     C                   PARM                    ##FPRM
     C                   PARM                    ##TPRM
     C                   PARM                    ##OUTQ
     C                   PARM      SCDPLX        ##DPLX
     C                   PARM      SCTYPO        ##TYPO
      ***** BEG ADD ***** 10/16/03 **************************************
     C                   PARM                    ##PFDT            8
     C                   PARM                    ##PTDT            8
     C                   PARM      SCSELN        ##SELN            1
      ***** END ADD ***** 10/16/03 **************************************
      ***** BEG ADD ***** 01/11/07 ***********************************
     C                   PARM                    ##PRSS            1
      ***** END ADD ***** 01/11/07 ***********************************
     C*
     C                   ENDSR                                                  CALLCL
     C*
     C*================================================================
     C*  SCR01T - SCR01 Editing                               ERRCK1
     C*================================================================
     C*
     C     SCR01T        BEGSR
     C*
      ***** BEG ADD ***** 10/16/03 **************************************
      *  EDIT FROM DATE OF POLICY DATE RANGE
     C     SCPFDT        IFGT      *ZEROS
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DTE1            7 0
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EDIT TO DATE OF POLICY DATE RANGE
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DTE2            7 0
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C  N90X7DTE2        IFLT      X7DTE1
     C                   SETON                                        75  90
     C                   ENDIF                                                  X7DAT2<X7DAT1
      *
     C                   ENDIF                                                  SCPFDT > *ZEROS
      ***** END ADD ***** 10/16/03 **************************************
     C*
     C*  EDIT FROM DATE OF ACCIDENT DATE RANGE
     C*
     C                   Z-ADD     SCAFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DAT1
     C*
     C  N90SRERR         IFEQ      *ON
      ***** BEG ADD ***** 10/16/03 **************************************
     C* If policy date range entered, accident date range is not required
     C*
     C     SCPFDT        ANDEQ     *ZEROS
      ***** END ADD ***** 10/16/03 **************************************
     C                   SETON                                        20  90
     C                   ENDIF                                                  SRERR=*ON
     C*
      ***** BEG ADD ***** 10/16/03 **************************************
     C*  EDIT TO DATE OF ACCIDENT DATE RANGE
     C     *IN90         IFEQ      *OFF
     C     SCAFDT        ANDNE     *ZEROS
     C*
      ***** END ADD ***** 10/16/03 **************************************
     C*
     C                   Z-ADD     SCATDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCATDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DAT2
      ***** BEG ADD ***** 10/16/03 **************************************
     C                   ENDIF                                                  SCPFDT > *ZEROS
      ***** END ADD ***** 10/16/03 **************************************
     C*
     C  N90SRERR         IFEQ      *ON
      ***** BEG ADD ***** 10/16/03 **************************************
     C     SCPFDT        ANDEQ     *ZEROS
     C     SCAFDT        ANDNE     *ZEROS
      ***** END ADD ***** 10/16/03 **************************************
     C                   SETON                                        21  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C  N90X7DAT2        IFLT      X7DAT1
     C                   SETON                                        22  90
     C                   ENDIF                                                  X7DAT2<X7DAT1
     C*
      ***** BEG ADD ***** 10/16/03 **************************************
     C*
     C* VALIDATE ACCIDENT FROM DATE IS NOT LESS THAN POLCIY FROM DATE
     C*
     C  N90SCPFDT        IFGT      *ZEROS
     C     SCAFDT        ANDGT     *ZEROS
     C     X7DAT1        ANDLT     X7DTE1
     C                   SETON                                        397990
     C                   ENDIF
      ***** END ADD ***** 10/16/03 **************************************
     C*
     C*  VALIDATE RUN DATE
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        23  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  VALIDATE THE PREMIUM RANGE
     C*
     C  N90SCTPRM        IFLE      *ZERO
     C                   SETON                                        45  90
     C                   ENDIF                                                  SCTRPM<=*ZERO
     C*
     C  N90SCFPRM        IFGT      SCTPRM
     C                   SETON                                        46  90
     C                   ENDIF                                                  SCTRPM>SCTPRM
     C*
     C*  VALIDATE SELECT ACTIVE ACCOUNTS ONLY FLAG
     C*
     C     SCACFL        IFNE      'Y'
     C     SCACFL        ANDNE     'N'
     C                   SETON                                        32  90
     C                   ENDIF                                                  SCACFL<>Y/N
     C*
     C*  VALIDATE EMPLOYER IF ENTERED
     C*
     C  N90SCEMP#        IFNE      *ZERO
     C*
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C     SCDIV         IFNE      *BLANKS
     C*
     C*  BE TO LOAD THE CORRECT VALUE IF FIELD EXIT WAS NOT PRESSSED
     C*
     C     XXDIV5        DOWEQ     *BLANK
     C                   MOVE      XXDIV4        XXDIV5
     C                   MOVE      XXDIV3        XXDIV4
     C                   MOVE      XXDIV2        XXDIV3
     C                   MOVE      XXDIV1        XXDIV2
     C                   CLEAR                   XXDIV1
     C                   ENDDO                                                  XXDIV5=*BLANK
     C*
     C                   MOVE      SCDIV         XXDIV
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C                   ELSE
     C*
     C     EMKY2         CHAIN     PEM01                              60
     C*
     C                   ENDIF                                                  SCDIV<>*BLANKS
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        24  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCEMP#<>*ZERO
     C*
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C  N90SCEMP#        IFEQ      *ZEROS
     C     SCDIV         ANDNE     *BLANKS
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCEMP#=*ZEROS
     C*
     C*  CHECK FOR DIV=*BLANK FIRST TIME THRU AND SEND MESSAGE
     C*
     C  N90SCEMP#        IFNE      *ZEROS
     C*
     C  N90SCDIV         IFNE      SVDIV
     C     SCEMP#        ORNE      SVEMP#
     C                   MOVE      *OFF          XXSAME
     C                   MOVE      SCDIV         SVDIV
     C                   MOVE      SCEMP#        SVEMP#
     C                   ENDIF                                                  SCDIV<>SVDIV
     C*
     C  N90XXSAME        IFEQ      *OFF
     C     SCDIV         ANDEQ     *BLANKS
     C                   SETON                                        36  90
     C                   MOVE      *ON           XXSAME
     C                   MOVE      SCDIV         SVDIV
     C                   MOVE      SCEMP#        SVEMP#
     C                   ENDIF                                                  XXSAME=*OFF
     C*
     C                   ENDIF                                                  SCEMP#<>*ZERO
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C*  VALIDATE THE REGION IF ENTERED
     C*
     C  N90SCRGN#        IFNE      *ZERO
     C*
     C     REKY          CHAIN     WMREL                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        25  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCRGN#<>*ZERO
     C*
     C*  VALIDATE THE COUNTY CODE IF ENTERED
     C*
     C  N90SCCNTY        IFNE      *BLANK
     C     SCCNTY        ANDNE     '*S'
     C*
     C     FNMST         CAT(P)    SCCNTY        ##KEY
     C*
     C     TBKY          CHAIN     PCN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        26  90
     C*
     C                   ELSE
     C*
     C     CNSTAT        IFEQ      'I'
     C                   SETON                                        27  90
     C                   ENDIF                                                  CNSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCCNTY<>*BLANK/*S
     C*
     C*  VALIDATE AGENT NUMBER IF ENTERED
     C*
     C  N90SCAGT#        IFNE      *ZERO
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        28  90
     C                   ENDIF                                                  *IN60=*ON/AASTAT=I
     C*
     C                   ENDIF                                                  SCAGT#<>*ZERO
     C*
     C*  VALIDATE OPEN CLAIMS FLAG
     C*
     C  N90SCSELO        IFNE      'Y'
     C     SCSELO        ANDNE     'N'
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCSELO<>Y/N
     C*
     C*  VALIDATE CLOSED CLAIMS FLAG
     C*
     C  N90SCSELC        IFNE      'Y'
     C     SCSELC        ANDNE     'N'
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCSELC<>Y/N
     C*
      ***** BEG ADD ***** 10/16/03 **************************************
     C*  VALIDATE Empployers with NO Claims Flag
     C*
     C  N90SCSELN        IFNE      'Y'
     C     SCSELN        ANDNE     'N'
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCSELN<>Y/N
     C* If Policy from date blank, Employers w/No Claim flag cannot be Y
     C  N90SCPFDT        IFEQ      *ZEROS
     C     SCSELN        ANDEQ     'Y'
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCPFDT> BLANKS
     C* If Employer is entered, force SCSELN to "N"
     C**N90      SCEMP#    IFNE *ZEROS
     C***        SCSELN    ANDEQ'Y'
     C***                  MOVEL'N'       SCSELN
     C***                  ENDIF                           SCEMP# <> ZEROS
      ***** END ADD ***** 10/16/03 **************************************
     C*
     C*  VALIDATE DUPLEX PRINTING FLAG
     C*
     C  N90SCDPLX        IFNE      'Y'
     C     SCDPLX        ANDNE     *BLANK
     C                   SETON                                        34  90
     C                   ENDIF                                                  SCDPLX<>Y/*BLANK
     C*
     C*  TYPE OF OUTPUT MUST BE 'P', 'F' OR 'B'
     C*
     C  N90SCTYPO        IFNE      'P'
     C     SCTYPO        ANDNE     'F'
     C     SCTYPO        ANDNE     'B'
     C                   SETON                                        35  90
     C                   ENDIF                                                  SCTYPO<>P/F/B
     C*
     C*  VALIDATE SEQUENCE CRITERIA
     C*
     C  N90SCSEQ         IFNE      'A'
     C     SCSEQ         ANDNE     'N'
     C     SCSEQ         ANDNE     'R'
     C     SCSEQ         ANDNE     'C'
     C     SCSEQ         ANDNE     'G'
     C     SCSEQ         ANDNE     'Z'
     C     SCSEQ         ANDNE     '1'
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCSEQ<>A/N/R/C/G/Z/1
     C*
      ***** BEG ADD ***** 01/11/07 ***********************************
     C*  VALIDATE PRINT SS FLAG
     C  N90SCPRSS        IFNE      'Y'
     C     SCPRSS        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN90
     C                   END
      ***** END ADD ***** 01/11/07 ***********************************
     C*
     C*  CALL COUNTY CODE SELECT IF *S AND NO SELECTION HAS BEEN MADE
     C*
     C  N90SCCNTY        IFEQ      '*S'
     C     ##CRTF        ANDEQ     '0'
     C*
     C                   EXSR      KPDO
     C*
     C                   MOVE      OUTFLG        *IN90
     C*
     C     ##CRTF        IFEQ      '0'
     C                   MOVEL     *BLANKS       SCCNTY
     C                   ENDIF                                                  ##CRTF=0
     C*
     C                   ENDIF                                                  SCCNTY=*S
     C*
     C*  DLT COUNTY CODE SELECT IF NOT *S AND SELECTION HAS BEEN MADE
     C*
     C  N90SCCNTY        IFNE      '*S'
     C     ##CRTF        ANDEQ     '1'
     C                   MOVE      'D'           ##CRTF
     C                   EXSR      KPDO
     C                   ENDIF                                                  SCCNTY<>*S
     C*
     C                   ENDSR                                                  SCR01T
