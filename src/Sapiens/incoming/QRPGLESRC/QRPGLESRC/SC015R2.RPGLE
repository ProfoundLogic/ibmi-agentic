     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*****************************************************************
     H/TITLE SC015R2 - WORK WITH CLAIMS DIARY ITEMS                   *
     H*****************************************************************
     H* PROFRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM SHOWS ALL NON-COMPLETED DIARY ITEMS FOR A CASE.*
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*        W0070R                                                 *
     H* CALLS TO:                                                     *
     H*        NONE                                                   *
     H* PROGRAMMER:  C:15                                             *
     H* CREATED:    02/17/93        PROJECT: AMN                      *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  10/17/11 C:277 Project: SY195                                *
     H*                 QDLS migration to IFS                         *
     H*                 Add two parameters with command               *
     H*                                                               *
     H*  02/25/09 C:220 67339/ 71WJN                                  *
     H*                 W/W DIARY ITEM NOT SHOWING 'Q' FOR QUOTE      *
     H*                                                               *
     H*  11/14/08 C:220 PROJECT: 71WJN                                *
     H*                 -Add opt to complete diary in DARTS           *
     H*                                                               *
     H*  05/14/08 C:183 SUPPORT: WC135/62709                          *
     H*                 -Changed first view to include Diary User     *
     H*                 -F9 to show the IMG and Document ID .         *
     H*                                                               *
     H***  04/17/07 C:02 PJCT: 89AIH
     H*      IF DRFL13 IS NOT ' ', AND THE DOC BEING VIEWED IS OTHER THAN OV
     H*        (CMIMGF = 'O'), USE RUNRMTCMD TO SEND THE ITEM TO A PC FOR
     H****     EXECUTION UNLESS DRFL13 IS AN "S" THEN USE STRPCCMD
     H*                                                               *
     H*  01/08/07 C:053 SUPPORT: 55026                                *
     H*                 -Check authority for options                  *
     H*                                                               *
     H*  12/10/04 C:062 SUPPORT: IM129 44967                          *
     H*                 -Change edit doc to data entry only           *
     H*                 -Change display doc to image only             *
     H*                                                               *
     H*  11/08/04 C:136 SUPPORT: DR065 43386                          *
     H*                 Restrict user access if SMSLP/SLRACC = 'Y'    *
     H*                                                               *
     H*  06/24/04 C:136 SUPPORT: WA051 42968                          *
     H*                 The diary completion date is being cleared    *
     H*                 before the update of SDCMLH.                  *
     H*                                                               *
     H*  11/26/03 C:128 Project: WC129  Seq#: 40122
     H*                 Diary completion in error. Writing remark date
     H*                 not todays date. Changed to write todays date.
     H*
     H*
     H*  12/18/02 C:128 Project: DR099  Seq#: 35160
     H*                 Date selection field not displaying correctly
     H*                 after user enters date.
     H*
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SMARP with API DA701R
     H*
     H*  04/16/99 C:02 SUPPORT: DR098 #17398                          *
     H*                CORRECTED DISPLAY WHEN USING 0 FOR DATE        *
     H*                                                               *
     H*  09/28/98 C:02 IMAGE : USE  IMAGE DOC ID (IDOC) FOR SCIMGP CALL
      *               ADDED PARM FROM SYSCTL TO THE DSPDOC, PRTDOC, AND
     H*                                                               *
     H*  07/25/98 C:02 Support: 12611
      *               ADDED PARM FROM SYSCTL TO THE DSPDOC, PRTDOC, AND
      *               EDTDOC CALLS FOR MICROSOFT PRODUCTS.
     H*                                                               *
     F*  07/20/98 C:69 Support: 12666
     H*                Changed the way the check for a recursive call
     H*                is done
     H*                                                               *
     H*  02/05/98 C:15 Project: WC577                                 *
     H*                Chgd pgm to load QQKEY to SCNMKY if there are  *
     H*                no open diaries for the claim selected.        *
     H*                                                               *
     H*  02/02/98 C:15 Project: WC577                                 *
     H*                Fixed pgm to prevent error when returning to   *
     H*                the pgm after an option is selected.           *
     H*                                                               *
     H*  05/10/93 02    - Proj:OPT                                    *
     H*  08/15/95 C:15  - PROJ:                                       *
     H*  08/31/95 C:15  - PROJ: 25WAR                                 *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  03    OV/400 NOT INSTALLED - DO NOT DISPLAY CMD 1            *
     H* N03    OV/400 IS INSTALLED - DISPLAY CMD 1                    *
     H*  06    DON'T DISPLAY COLUMN SEPARATOR                         *
     H*  20    SCM0515-NOT AUTHORIZED TO EDIT DOCUMENT                *
     H*  21    SCM0514-NOT AUTHORIZED TO DISPLAY DOCUMENT             *
     H*  22    SCM0510-INVALID VALUE KEYED                            *
     H*  23    SCM0516-DOCUMENT NOT FOUND IN SPECIFIED FOLDER         *
     H*  24    SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER           *
     H*  25    SCM0518-USER NOT ENROLLED IN OFFICE 400                *
     H*  26    SCM0518-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT    *
     H*  30    SCM0500-INVALID DATE                                   *
     H*  31    SCM0550-OV/400 NOT INSTALLED                           *
     H*  38    KEY is displayed (F22)                                 *
     H* N38    NAME is displayed (F22)                                *
     H*  40    GENERAL CHAIN                                          *
     H*  41    CHAIN ON PCM01 FOR SELECTIONS                          *
     H*  42    SCM0622-OfficeVision/400 is already active             *
     H*  43    SCM0532-Default Folder required if Document ID indicated
     H*  44    SCM0519-Unable to process request for this document    *
     H*  50    READ ON SCR01                                          *
     H*  51                                                           *
     H*  52                                                           *
     H*  53                                                           *
     H*  59                                                           *
     H*  60    READ ON PCM01 FOR LOADING SUBFILE                      *
     H*  61    CHAIN ON PAR01                                         *
     H*  71    GLOBAL - DATA ENTRY PROGRAM IS INSTALLED               *
     H*  72    CMFL04 = 'Y' -- DEPGM Diary Item is highlighted        *
     H* N72    CMFL04 = ' ' -- DEPGM Diary Item is not highlighted    *
     H*  80    GLOBAL - Used on call to Data Entry Program (DEPGM)    *
     H*  81    SCM0619-Must find alternative access to this data entry*
     H*                program.                                       *
     H*  82    SCM0620-Data entry program is not available at this    *
     H*                time.                                          *
     H*****************************************************************
     FSC015F2   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     FSDCMLH03  UF   E           K DISK
     FSMSLL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*** BEGIN DEL *** 04/25/00 **************************************
     F*****SMARL   UF  E           K        DISK                      A
     FSMARL     IF   E           K DISK
     F*** END DEL *** 04/25/00 ****************************************
     F*** BEGIN ADD *** 12/10/04 **************************************
     FSDAIL     IF   E           K DISK
     F*** END   ADD *** 12/10/04 **************************************

     D*
     D MSG             S             10    DIM(1) CTDATA PERRCD(1)              SCREEN MESSAGE
     D*** BEG CHG ***** 05/14/08 **************************************
     D***************     CK1     1   4 79               COMMAND KEYS - SCR01
     D CK1             S             79    DIM(6) CTDATA PERRCD(1)              COMMAND KEYS - SCR01
     D*** END CHG ***** 05/14/08 **************************************
     D CMT             S             75    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D  XXSM                   7      8
     D  XXSD                   9     10
      *** Begin Change 11/26/03 ***************************************
     D**********************                 11  12 XXSY
     D  XXSY                  13     14
      *** End   Change 11/26/03 ***************************************
     D                 DS
     D  CMCDAT                 1      7  0
     D  XXCC                   1      1  0
     D  XXCY                   2      3  0
     D  XXCM                   4      5  0
     D  XXCD                   6      7  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEG ADD ***** 05/14/08 **************************************
     D                 DS
     D  DSUSR                  1     16
     D  @@IMGF                 2      2
     D  @@DCID                 5     16
     D*** END ADD ***** 05/14/08 **************************************
     D*    COMMENT STRING
     D                 DS
     D  @@CMT                  1     75
     D  @@USER                21     30
     D  @@CM                  42     43
     D  @@CD                  45     46
     D  @@CY                  48     49
     D  @@SM                  68     69
     D  @@SD                  71     72
     D  @@SY                  74     75
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMMENT DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    SCKEY
     C                   KFLD                    XXCDAT            7 0
     C                   KFLD                    SCSEQ#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    SCKEY
     C*
     C*   KEY LIST FOR THE DARTS SECURITY FILE
     C*
     C     SLKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SYSTEM CTL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEGIN ADD *** 12/10/04 **************************************
     C*
     C     AIKY          KLIST
     C                   KFLD                    SCKEY
     C                   KFLD                    SCSEQ#
     C*** END   ADD *** 12/10/04 **************************************
     C*
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   ARKY2     KLIST
     C********             KFLD           CMKEY
     C********             KFLD           CMSEQ#
     C********             KFLD           XXASQ#
     C*** END DEL *** 04/25/00 ****************************************
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
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
     C**** ADD 2/11/98 *** IMAGE PARM LIST ****************
     C     IMAGE         PLIST
     C                   PARM                    SCKEY
     C                   PARM                    SCSEQ#
     C                   PARM                    SCIDOC
     C                   PARM                    SCFOLD
     C                   PARM                    SCFL05
     C                   PARM                    SCIRTN            6
     C*
     C**** END ADD 2/11/98 *****************************
     C*
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    CMCDAT        XXDATE                         SCREEN DATE
     C     *LIKE         DEFINE    SCDATE        XHDATE                         HOLD DATE
     C     *LIKE         DEFINE    CMPNAM        #7PNAM
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    CMKEY         DEKEY
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   *LIKE     DEFN ARASQ#    XXNXT#           ADD 01/11/93
     C********   *LIKE     DEFN ARASQ#    XXASQ#           ADD 01/11/93
     C*** END DEL *** 04/25/00 ****************************************
     C                   Z-ADD     0             C1                5 0
     C*** BEG ADD ***** 05/14/08 **************************************
     C                   Z-ADD     0             C2                5 0
     C*** END ADD ***** 05/14/08 **************************************
     C                   MOVEL     QQKEY         SCKEY
     C*** BEGIN ADD *** 12/10/04 **************************************
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 12/10/04 **************************************
     C*
     C                   MOVE      *BLANKS       DERTCD            1
     C                   MOVE      *BLANKS       DECOM1          256
     C                   MOVE      *BLANKS       DECOM2           19
     C                   MOVE      *BLANKS       DEKEY
     C*
      *** BEG ADD ***** 11/08/04 *************************************
     C     SLKEY         CHAIN     PSL01                              99
     C     SLRACC        IFEQ      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     C*** END ADD ***** 11/08/04 **************************************
     C*
     C*------------------------*
     C*** RESET POINT IN PROGRAM
     C*------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*  If Officevision/400 is not installed, use first CMD line
     C*
     C     *IN03         IFEQ      '1'
     C                   Z-ADD     1             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   ELSE
     C*
     C*  If Officevision/400 is installed, use third CMD line
     C*
     C                   Z-ADD     3             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C*** BEG ADD ***** 05/14/08 *************************************
     C                   Z-ADD     5             C2
     C                   MOVEL     CK1(C2)       SCCML2
     C*** END ADD ***** 05/14/08 *************************************
     C                   END
     C*
     C* SET INITIAL DISPLAY DATE TO SYSTEM DATE
     C*
     C                   Z-ADD     DSDATE        SCDATE
     C                   Z-ADD     SCDATE        XHDATE
     C*
     C*   This is called initially in order to allow a test for
     C*   claimant name = blank.  If this condition exists, the key is
     C*   initially displayed; otherwise the name is.
     C*
     C     CMKY1         CHAIN(N)  PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     *BLANKS       X#CNAM
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#CNAM           79
     C                   PARM                    X#RTCD            1
     C                   END
     C*
     C     X#CNAM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN38
     C                   MOVEL     X#KEY         SCNMKY
     C                   ELSE
     C                   MOVE      '0'           *IN38
     C                   MOVEL     X#CNAM        SCNMKY
     C                   END
     C*
     C*** BEGIN ADD *** 02/05/98 ***********************************************
     C     SCNMKY        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN38
     C                   MOVEL     QQKEY         SCNMKY
     C                   END
     C*** END ADD ***** 02/05/98 ***********************************************
     C*
     C                   EXSR      LOAD
     C*
     C     STRT          TAG
     C*
     C*** BEG ADD *** 11/14/08 ***********************************************
     C*** BEG CHG *** 02/25/09 ***********************************************
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W0050R'      XXPGM1           10
     C                   PARM      *BLANKS       SRERR1            1
     C*********  ' '       CHEKRQQKEY     #EMFLG  20
     C*********  #EMFLG    IFEQ 20
     C     SRERR1        IFEQ      *ON
     C*** END CHG *** 02/25/09 ***********************************************
     C                   SETON                                            77      FOR EMPLOYER SCRN
     C                   ENDIF
     C*** END ADD *** 11/14/08 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C     *IN11         IFEQ      '1'
     C*
     C****************************************
     C*   F1 = OV/400                        *
     C****************************************
     C     *INKA         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      *BLANKS       OFCPGM
     C                   MOVEL     Q2RECF        HLRECF
     C*
     C* Get Office CLP Program Name from SDRPF/DRAL02
     C*
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   END
     C*
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
     C*
     C                   WRITE     SCR99
     C                   MOVEL     HLRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKA
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
     C*
     C                   EXSR      LOAD
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKE
     C*** BEG ADD ***** 05/14/08 ***********************************************
     C****************************************
     C*   F9 =  CHANGE INFO                  *
     C****************************************
     C     *INKI         IFEQ      '1'
      *
     C                   SELECT
     C     *IN73         WHENEQ    '0'
     C                   MOVE      '1'           *IN73
     C     *IN73         WHENEQ    '1'
     C                   MOVE      '0'           *IN73
     C                   ENDSL
      *
     C                   GOTO      TOP
     C                   END                                                    END *INKI
     C*** END ADD ***** 05/14/08 ***********************************************
     C****************************************
     C*   F12 = PREVIOUS                     *
     C****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C****************************************
     C*   F19 = PRINT DIARY ITEM REMARKS     *
     C****************************************
     C     *INKT         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QRKEY            40
     C                   MOVEL     SCUSER        QRUSER           10
     C                   MOVE      *ZEROS        QRFRDT            6
     C                   MOVE      *ZEROS        QRTODT            6
     C                   MOVE      *BLANKS       QRRMTY            2
     C                   MOVE      *BLANKS       QRDCID           12
     C                   MOVE      'D'           QRTYPE            1
     C                   MOVE      'N'           QRDITY            1
     C                   MOVE      'UD'          QRSORT            2
     C                   MOVE      *BLANKS       QRRTCD            1
     C                   CALL      'SC711R'
     C                   PARM                    QRKEY
     C                   PARM                    QRUSER
     C                   PARM                    QRFRDT
     C                   PARM                    QRTODT
     C                   PARM                    QRRMTY
     C                   PARM                    QRDCID
     C                   PARM                    QRTYPE
     C                   PARM                    QRDITY
     C                   PARM                    QRSORT
     C                   PARM                    QRRTCD
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKT
     C*****************************************
     C*   F22 = TOGGLE BETWEEN REMARK AND KEY *
     C*****************************************
     C     *INKW         IFEQ      '1'
     C*
     C     *IN38         IFEQ      '1'
     C                   MOVE      '0'           *IN38
     C                   MOVEL     X#CNAM        SCNMKY
     C                   ELSE
     C                   MOVE      '1'           *IN38
     C                   MOVEL     X#KEY         SCNMKY
     C                   END                                                    END *IN38
     C*
     C     C1            IFEQ      1
     C                   Z-ADD     2             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   ELSE
     C     C1            IFEQ      2
     C                   Z-ADD     1             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   END
     C                   END
     C*
     C     C1            IFEQ      3
     C                   Z-ADD     4             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C*** BEG ADD ***** 05/14/08 ***********************************************
     C                   Z-ADD     6             C2
     C                   MOVEL     CK1(C2)       SCCML2
     C*** END ADD ***** 05/14/08 ***********************************************
     C                   ELSE
     C     C1            IFEQ      4
     C                   Z-ADD     3             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C*** BEG ADD ***** 05/14/08 ***********************************************
     C                   Z-ADD     5             C2
     C                   MOVEL     CK1(C2)       SCCML2
     C*** END ADD ***** 05/14/08 ***********************************************
     C                   END
     C                   END
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END *INKW
     C**************************************************************************
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C*
     C* VALIDATE DATE ON SCREEN
     C*
     C     SCDATE        IFNE      0
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         CABEQ     '1'           STRT                     30
     C*** Begin Chg. 12/18/02 *****************************************
     C                   MOVEL     SRDTO         SCDATE
     C*** End   Chg. 12/18/02 *****************************************
     C                   END
     C*
     C*  IF THE USER CHANGED THE DATE ON THE SCREEN, RELOAD THE SUBFILE.
     C*
     C     SCDATE        IFNE      XHDATE
     C                   EXSR      LOAD
     C                   Z-ADD     SCDATE        XHDATE
     C                   GOTO      STRT
     C                   END
     C*
     C*  PROCESS ALL SELECTIONS
     C*
     C                   MOVE      '0'           ERR
     C                   READC     SCR01S                                 50
     C   50              GOTO      TERM
     C     *IN50         DOWEQ     '0'
     C*****
     C*  PROCESS FOR 4=MARK COMPLETED
     C*****
     C     SCSEL         IFEQ      '4'
     C*
     C*** BEG ADD *** 01/08/07 **************************************
     C                   EXSR      AUTHCK
     C*** END ADD *** 01/08/07 **************************************
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     CMKY          CHAIN     PCM01                              40
     C     *IN40         IFEQ      '0'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDCDT
     C                   MOVEL     Q1PGM         CMPGM                          MAINT INFO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVE      *BLANK        CMFL04
     C                   MOVEL     Q1USER        CMUSER
      *** BEG ADD ***** 06/24/04 **********************************
     C                   UPDATE    PCM01
      *** END ADD ***** 06/24/04 **********************************
     C*
     C*  MOVE VALUES INTO COMMENT STRING TO ADD TO ADDITIONAL REMARK
     C*  FILE. (SPECIFYING DIARY COMPLETION)
     C*
     C                   MOVEL     CMT(1)        @@CMT
     C                   MOVEL     Q1USER        @@USER
     C                   MOVEL     XXCM          @@CM
     C                   MOVEL     XXCD          @@CD
     C                   MOVEL     XXCY          @@CY
     C                   MOVEL     XXSM          @@SM
     C                   MOVEL     XXSD          @@SD
     C                   MOVEL     XXSY          @@SY
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********
     C***GET*NEXT AVAILABLE SEQUENCE NUMBER
     C********
     C********             EXSR ASEQ
     C********
     C***MOVE*VALUES INTO FIELDS FOR WRITE TO ADDITIONAL REMARKS RCD
     C********
     C********             MOVEL@@CMT     ARCM75
     C********
     C********             MOVELCMKEY     ARKEY
     C********             Z-ADDCMSEQ#    ARSEQ#
     C********             Z-ADDXXNXT#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             Z-ADDCMCDAT    ARCDAT
     C********             MOVELCMCODE    ARCODE
     C********             MOVELCMPNAM    ARPNAM
     C********             TIME           DSDTTM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********             Z-ADDDSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             Z-ADDDSTIME    ARTIME
     C********             Z-ADDARTRDT    ARDT
     C********             MOVELQ1USER    ARIUSR
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARIDT
     C********             MOVELCMRMCD    ARRMCD
     C********             WRITEPAR01
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C*          Call API to write a DARTS Additional Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  CMKEY         Q#KEY            40
     C                   MOVE      CMSEQ#        Q#SEQ#            5
     C                   MOVEL(P)  @@CMT         Q#CM75           75
     C                   MOVEL(P)  CMPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#ASQ#            5
     C                   MOVEL(P)  CMCSID        Q#CSID            2
     C                   MOVE(P)   CMCDAT        Q#CDAT            7
     C                   MOVE(P)   CMCODE        Q#CODE            2
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*** END ADD *** 04/25/00 ****************************************
     C*
     C*** BEG DEL **** 06/24/04 ***************************************
     C****************     UPDATPCM01
     C*** END DEL **** 06/24/04 ***************************************
     C                   END                                                    END 0 *IN40
     C*
     C                   MOVE      *BLANKS       SCCO45
     C                   MOVEL     MSG(1)        SCCO45
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      '0'           *IN72
     C                   UPDATE    SCR01S
     C*
     C                   GOTO      NXT1
     C                   END                                                    END SEL EQ '1'
     C*****
     C*  PROCESS FOR 2=EDIT DOCUMENT
     C*****
     C     SCSEL         IFEQ      '2'
     C*
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX02R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX02T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C* PERFORM THIS CODE ONLY IF DATA ENTRY PROGRAM OPTION EXISTS IN
     C*    SDRPF (DRFL03 = 'Y')
     C*   NOTE: DERTCD = *BLANK is 'Add'
     C*         DERTCD = '1'    is 'View'
     C*         DERTCD = '2'    is 'Update'
     C*
     C     *IN71         IFEQ      '1'
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     CMKY          CHAIN(N)  PCM01                              41
     C     *IN41         IFEQ      '0'
     C     CMFL01        IFEQ      'Y'
     C*
     C                   MOVE      '0'           *IN72
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C*  Check to see if a recursive call was made.
     C*
     C                   MOVE      *BLANKS       ##PGM
     C*** BEGIN ADD *** 12/10/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     CMDCID        ##PGM
     C                   ELSE
     C                   MOVEL     AIDPGM        ##PGM
     C                   ENDIF
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGM            10
     C                   PARM                    ##ERR             1
     C                   ELSE
     C*** END   ADD *** 12/10/04 **************************************
     C                   CALL      'SRCHKPGM'
     C                   PARM      CMDCID        ##PGM            10
     C                   PARM                    ##ERR             1
     C*** BEGIN ADD *** 12/10/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/10/04 **************************************
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN81
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C*** BEGIN ADD *** 12/10/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C                   ELSE
     C                   MOVEL     AIDPGM        DEPGM
     C                   MOVEL     AIDKEY        DEKEY
     C                   ENDIF
     C                   ELSE
     C*** END   ADD *** 12/10/04 **************************************
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C*** BEGIN ADD *** 12/10/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/10/04 **************************************
     C                   MOVEL     CMCOMM        DECOM1
     C                   MOVE      CMCOMM        DECOM2
     C                   MOVE      ' '           DERTCD                         ' ' = change mode
     C*
     C                   CALL      DEPGM                                80
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C*
     C     *IN80         IFEQ      '1'
     C                   EXSR      ERROR
     C                   GOTO      STRT
     C                   END
     C*
     C     DERTCD        IFEQ      'A'
     C     DERTCD        OREQ      'B'
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** 12/10/04 **************************************
     C     ##DEIF        IFNE      'Y'
     C*** END   ADD *** 12/10/04 **************************************
     C     CMKY          CHAIN     PCM01                              41
     C     *IN41         IFEQ      '0'
     C                   MOVE      *BLANKS       CMFOLD
     C                   MOVEL     DEKEY         CMFOLD
     C                   MOVE      *BLANKS       CMCOMM
     C                   MOVEL     DECOM1        CMCOMM
     C                   MOVE      DECOM2        CMCOMM
     C                   MOVEL     'Y'           CMFL04
     C                   MOVE      '1'           *IN72
     C                   MOVEL     Q1USER        CMUSER
     C                   UPDATE    PCM01
     C                   END
     C*** BEGIN ADD *** 12/10/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/10/04 **************************************
     C*
     C                   END
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    CMFL01 = 'Y'
     C*
     C                   END                                                    *IN41 = '0'
     C                   END                                                    *IN71 = '1'
     C*
     C                   MOVE      '0'           *IN72
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEA     '000'         *IN(20)
     C                   MOVEA     '0000'        *IN(23)
     C*
     C     SLKEY         CHAIN     PSL01                              40
     C     *IN40         IFEQ      '0'
     C     SLEDC         IFNE      'Y'
     C     SCIMGF        ANDNE     'Y'                                          not if image
     C                   MOVE      '1'           *IN20                          NOT ALLOWED TO EDIT
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE 'Y'
     C                   END                                                    END 0 *IN40
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SEDTDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    DRFL06
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     IEX02T        TAG
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    END SEL EQ '2'
     C*****
     C*  PROCESS FOR 3=DIARY ANOTHER USER
     C*****
     C     SCSEL         IFEQ      '3'
     C*
     C*** BEG ADD *** 01/08/07 **************************************
     C                   EXSR      AUTHCK
     C*** END ADD *** 01/08/07 **************************************
     C*
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX03R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX03T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C                   MOVEL     SCKEY         #3KEY            40
     C                   MOVE      SCSEQ#        #3SEQ#            5
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC016R'
     C                   PARM                    #3KEY
     C                   PARM                    #3SEQ#
     C                   PARM                    QQRTCD
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C                   MOVE      '0'           *IN72
     C     CMKY          CHAIN(N)  PCM01                              41
     C     *IN41         IFEQ      '0'
     C     CMFL04        ANDEQ     'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C*** ADD 2/11/98 **********
     C     IEX03T        TAG
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    END SEL EQ '3'
     C*****
     C*  PROCESS 5=DISPLAY DOCUMENTS
     C*****
     C     SCSEL         IFEQ      '5'
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX05R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX05T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*** BEGIN ADD *** 12/10/04 **************************************
     C     ##DEIF        IFNE      'Y'
     C*** END   ADD *** 12/10/04 **************************************
     C*
     C*
     C*   PERFORM THIS CODE ONLY IF DATA ENTRY PROGRAM OPTION EXISTS IN
     C*    SDRPF (DRFL03 = 'Y')
     C*   NOTE: DERTCD = *BLANK is 'Add'
     C*         DERTCD = '1'    is 'View')
     C*         DERTCD = '2'    is 'Update')
     C*
     C     *IN71         IFEQ      '1'
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     CMKY          CHAIN(N)  PCM01                              41
     C     *IN41         IFEQ      '0'
     C     CMFL01        IFEQ      'Y'
     C                   MOVE      '0'           *IN72
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C*  Check to see if a recursive call was made.
     C*
     C                   MOVE      *BLANKS       ##PGM
     C                   CALL      'SRCHKPGM'
     C                   PARM      CMDCID        ##PGM            10
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN81
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C                   MOVEL     CMCOMM        DECOM1
     C                   MOVE      CMCOMM        DECOM2
     C                   MOVE      '1'           DERTCD                         '1' = display mode
     C*
     C                   CALL      DEPGM                                80
     C                   PARM                    DEKEY
     C                   PARM                    DECOM1
     C                   PARM                    DECOM2
     C                   PARM                    DERTCD
     C     *IN80         IFEQ      '1'
     C                   EXSR      ERROR
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    CMFL01 = 'Y'
     C*
     C                   END                                                    *IN41 = '0'
     C                   END                                                    *IN71 = '1'
     C*** BEGIN ADD *** 12/10/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/10/04 **************************************
     C*
     C                   MOVE      '0'           *IN72
     C*
     C*   If AS/400 Office NOT Installed - Don't allow this function
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEA     '000'         *IN(20)
     C                   MOVEA     '0000'        *IN(23)
     C*
     C     SLKEY         CHAIN     PSL01                              40
     C     *IN40         IFEQ      '0'
     C     SLDDC         IFNE      'Y'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCIMGF        IFNE      'Y'                                          IF NOT AN IMAGE TYPE
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     SCKEY         XXKY             40
     C                   MOVEL     *BLANK        QQRTCD            1
     C     SCIMGF        IFEQ      'F'
     C                   MOVEL     SCIMGF        $MFL06            1
     C                   ELSE
     C                   MOVEL     DRFL06        $MFL06
     C                   ENDIF
     C                   CALL      'SDSPDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    $MFL06
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       XXUNCN          255
     C                   PARM      *BLANKS       XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C****** ADD START 04/17/07 ***************************************
     C     SKPDC         TAG
     C****** END ADD   04/17/07 ****************************************
     C                   ELSE
     C*
     C*** START CHANGES *** 1/05/98 ***************** ISSIMG **********
     C                   MOVEL     SCIDOC        QVDCTB          120
     C                   MOVEL     Q1USER        QVUSER           10
     C                   MOVEL     Q1JOB         QVTRID           10
     C                   MOVEL     *BLANK        QVRTCD            6
     C*
     C                   MOVEL     SCIDOC        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD           80
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     SCKEY         XXKY             40
     C                   MOVEL     *BLANK        QVRTCD
     C*
     C*
     C                   CALL      'SCIMGV'                             60
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QVRTCD
     C*** END CHANGES 1/05/98 **************************
     C*
     C*
     C     QVRTCD        IFNE      '     0'
     C     QVRTCD        ANDNE     '000000'
     C     QVRTCD        ANDNE     '      '
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*
     C     IEX05T        TAG
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    END SEL EQ '5'
     C*****
     C*  OPTION 6=PRINT DOCUMENT OR IMAGE
     C*****
     C     SCSEL         IFEQ      '6'
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX06R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX06T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C*
     C                   MOVE      '0'           *IN72
     C     SCIMGF        IFNE      'Y'                                          NOT AN IMAGE
     C*
     C*   If AS/400 Office NOT Installed - Don't allow this function
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDCID        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END SCDCID EQ *BLANK
     C*
     C                   MOVEL     SCDCID        XXDCID
     C                   MOVEL     SCFOLD        XXFOLD
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     SCKEY         XXKY             40
     C                   MOVEL     *BLANK        QQRTCD
     C     SCIMGF        IFEQ      'F'
     C                   MOVEL     SCIMGF        $MFL06            1
     C                   ELSE
     C                   MOVEL     DRFL06        $MFL06
     C                   ENDIF
     C                   CALL      'SPRTDOC'                            60
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    $MFL06
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN52
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN53
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C**** ADD 2/11/98 *** INFINITY IMAGING **********************
     C*
     C***** ADD 9/28/98 *** USE IMAGE DOC ID
     C                   MOVEL     SCIDOC        XXDCID
     C***** DEL 9/28/98**  MOVELSCDCID    XXDCID
     C                   MOVEL     SCFOLD        XXFOLD
     C                   MOVE      SCSEQ#        XXSQ#             5
     C                   MOVEL     SCKEY         XXKY             40
     C                   MOVEL     *BLANK        QPRTCD            6
     C*
     C                   CALL      'SCIMGP'                             60
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C*
     C                   PARM                    QPRTCD
     C*
     C****************     MOVELSCIDOC    QPDCID 12
     C*                    MOVELSCUSER    QPUSER 10
     C*                    MOVELQ1JOB     QPTRID 10
     C**                   MOVEL*BLANK    QPRTCD  6
     C*
     C************         CALL 'SCIMGP'               60
     C**                   PARM           QPDCID
     C***                  PARM           QPUSER
     C***                  PARM           QPTRID
     C****************     PARM           QPRTCD
     C*** END ADD 2/11/98 ****************************************
     C*
     C     QPRTCD        IFNE      '000000'                                       ERROR OCCURED
     C     QPRTCD        ANDNE     '      '                                       ERROR OCCURED
     C     QPRTCD        ANDNE     '     0'                                       ERROR OCCURED
     C                   MOVE      '1'           *IN26                              IMAGE
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                      IMGF <> 'Y'
     C*
     C     IEX06T        TAG
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                      OPTION 6
     C*****
     C*  OPTION 7=DISPLAY/ADD ADDITIONAL REMARKS
     C*****
     C     SCSEL         IFEQ      '7'
     C*
     C*** BEG ADD *** 01/08/07 **************************************
     C                   EXSR      AUTHCK
     C*** END ADD *** 01/08/07 **************************************
     C*
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX07R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX07T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C                   MOVEL     SCKEY         #7KEY            40
     C                   Z-ADD     SCSEQ#        #7SEQ#            5 0
     C                   MOVEL     SCPNAM        #7PNAM
     C                   CALL      'SC014R'
     C                   PARM                    #7KEY
     C                   PARM                    #7SEQ#
     C                   PARM                    #7PNAM
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     CMKY          CHAIN(N)  PCM01                              41
     C     *IN41         IFEQ      '0'
     C                   MOVE      '0'           *IN72
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C                   END
     C*
     C     IEX07T        TAG
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END
     C*****
     C*  PROCESS FOR 8=DISPLAY REMARK DETAIL
     C*****
     C     SCSEL         IFEQ      '8'
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX08R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX08T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C*
     C                   MOVEL     SCKEY         #8KEY            40
     C                   Z-ADD     SCSEQ#        #8SEQ#            5 0
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC012R'
     C                   PARM                    #8KEY
     C                   PARM                    #8SEQ#
     C                   PARM                    QQRTCD
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C                   MOVE      '0'           *IN72
     C     CMKY          CHAIN(N)  PCM01                              41
     C     *IN41         IFEQ      '0'
     C     CMFL04        ANDEQ     'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C     IEX08T        TAG
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    END SEL EQ '8'
     C*****
     C*  OPTION 9=APPEND/DISPLAY IMAGE NOTES
     C*****
     C     SCSEL         IFEQ      '9'
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX09R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX09T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C*
     C     SCIMGF        IFEQ      'Y'
     C                   MOVE      '0'           *IN72
     C*
     C                   MOVEL     SCIDOC        #9KEY            40
     C                   MOVEL     'IMAGENOT'    #9PGM            10
     C                   MOVE      'ES'          #9PGM
     C                   CALL      'SCN10R'
     C                   PARM                    #9KEY
     C                   PARM                    #9PGM
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C     IEX09T        TAG
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END
     C*****
     C*  OPTION 10=SEND DOCUMENT TO OTHER USER(S)
     C*****
     C     SCSEL         IFEQ      '10'
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX10R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX10T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C*
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     CMKY          CHAIN(N)  PCM01                              41
     C     *IN41         IFEQ      '0'
     C*
     C*   IF ENTERED, DOC ID MUST BE VALID (EXISTING)
     C*
     C                   MOVEL     CMDCID        XXDCID
     C                   MOVEL     CMFOLD        XXFOLD
     C                   MOVEL     *BLANK        QQRTCD
     C*
     C     CMDCID        IFNE      *BLANK
     C*
     C*   If AS/400 Office NOT Installed - Don't allow this function
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C*   Must have a Default Folder entered for this function
     C*
     C     XXFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN43
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN44
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN44
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    DCID <> *BLANK
     C*
     C                   MOVE      ' '           QQRTCD
     C                   CALL      'SSNDDOC'
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN42
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    *IN41 = 0
     C*
     C*
     C     IEX10T        TAG
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C                   END                                                    SCSEL = '10'
     C*****
     C*  INVALID SELECTION
     C*****
     C*
     C                   MOVEA     '000'         *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         ANDNE     '2'
     C     SCSEL         ANDNE     '3'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '6'
     C     SCSEL         ANDNE     '7'
     C     SCSEL         ANDNE     '8'
     C     SCSEL         ANDNE     '9'
     C     SCSEL         ANDNE     '10'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C****
     C     NXT1          TAG
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C*  POSITION CURSOR AT THE LAST RECORD SELECTED
     C*
     C                   Z-ADD     RRN2          #@$2
     C*** BEGIN DLT *** 02/02/98 ***********************************************
     C******************** UPDATSCR01S
     C*** END DLT ***** 02/02/98 ***********************************************
     C                   GOTO      STRT
     C*
     C** END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*====================================================================*
     C* * *   INIT SUBROUTINE                                    I N I T ***
     C*====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C* LOOK AT DARTS SYSTEM CONTROL FILE TO DETERMINE WHAT'S INSTALLED
     C*
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** IF IMAGE IS INSTALLED, SET ON IMAGE INDICATOR.
     C*
     C     DRFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN99
     C                   END
     C*
     C*** IF DATA ENTRY PROGRAM OPTION IS INSTALLED, SET ON INDICATOR.
     C*      (DRFL03 = 'Y', *IN71 = 1)
     C*
     C     DRFL03        IFEQ      'Y'
     C                   MOVE      '1'           *IN71
     C                   END
     C                   END
     C*
     C*** Check System Control File Install Flags for this Client:
     C*
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C*
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   MOVE      '0'           *IN03
     C                   ELSE
     C                   MOVE      'N'           XXOFIF                         Not Installed
     C                   MOVE      '1'           *IN03
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C*  CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   MOVE      '0'           *IN05
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     1             #@$2
     C*
     C*  CONVERT THE SCREEN DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     XXDATE        IFEQ      0
     C**** CHANGE NXT LIN FROM 999999 TO 9999999 **** 4/16/99
     C                   Z-ADD     9999999       XXDATE
     C                   END
     C*
     C*  LOAD THE SUBFILE
     C*
     C     CMKY1         CHAIN(N)  PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   EXSR      RMSLCT
     C     CMKY1         READE(N)  PCM01                                  60
     C                   END
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS WERE SELECTED
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCCDAT
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       SCCO45
     C                   MOVE      *BLANKS       SCSEQ#
     C                   MOVE      *BLANKS       SCFOLD
     C* * * IMAGE FIELDS
     C                   MOVEL     *BLANK        SCIQUE                             HIDDEN
     C                   MOVEL     *BLANK        SCIPTH                             HIDDEN
     C                   MOVE      *ZEROS        SCPIDX                             HIDDEN
     C                   MOVEL     *BLANK        SCAPID                             HIDDEN
     C                   MOVEL     *BLANK        SCIDOC                             HIDDEN
     C                   MOVEL     *BLANK        SCIMGF                             HIDDEN
     C                   MOVEL     *BLANK        SCQPTY                             HIDDEN
     C                   MOVEL     *BLANK        SCDCTP                             HIDDEN
     C                   MOVEL     *BLANK        SCPNAM                             HIDDEN
     C*
     C                   MOVEL     *BLANK        SCFL05                             HIDDEN
     C*
     C                   ADD       1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *   REMARK SELECT - SELECT REMARKS TO BE DISPLAYED             ***
     C*====================================================================*
     C     RMSLCT        BEGSR
     C*
     C     CMCDAT        IFLE      XXDATE
     C*
     C                   ADD       1             RRN2
     C*
     C                   MOVEL     CMKEY         SCKEY
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C                   MOVEL     CMRMCD        SCCODE
     C                   MOVEL     CMDCID        SCDCID
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   ELSE
     C                   MOVE      '0'           *IN72
     C                   END
     C*** BEG ADD ***** 05/14/08 ***********************************************
     C                   MOVE      *BLANKS       SCUSR
     C     *IN73         IFEQ      '1'
      *
      * Code for DSPATR(ND) as it was removed from DSPF
      *
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  SCDCID        @@DCID
     C                   MOVEL(P)  *BLANKS       @@IMGF
     C                   ELSE
     C                   MOVEL(P)  SCDCID        @@DCID
     C                   MOVEL(P)  SCIMGF        @@IMGF
     C                   ENDIF                                                  *IN99 IFEQ '0'
      *
     C                   MOVE(P)   DSUSR         SCUSR
      *
     C                   ELSE
     C                   MOVEL(P)  CMUSER        SCUSR
     C                   ENDIF                                                  *IN73 IFEQ '1'
     C*** END ADD ***** 05/14/08 ***********************************************
     C*
     C                   MOVEL     *BLANKS       SCCO45
     C                   MOVEL     CMCOMM        SCCO45
     C*
     C*** IF ADDITIONAL COMMENTS EXIST, THEN SPECIFY "MORE..." FOR COMMENT.
     C*                    .
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   END
     C*
     C                   MOVEL     CMSEQ#        SCSEQ#
     C                   MOVEL     CMUSER        SCUSER
     C                   MOVEL     CMFOLD        SCFOLD
     C*
     C     CMIMGF        IFEQ      'Y'
     C                   MOVE      '1'           *IN02                          IMAGE TYPE INDICATOR
     C                   MOVE      'Y'           SCIMGF                         IMAGE FLAG
     C                   ELSE
     C                   MOVE      '0'           *IN02
     C     CMIMGF        IFNE      *BLANKS
     C                   MOVEL     CMIMGF        SCIMGF                         IMAGE FLAG
     C                   ELSE
     C                   MOVE      'N'           SCIMGF
     C                   ENDIF
     C                   END
     C*
     C*  FILL IMAGE FIELDS
     C*
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C                   MOVEL     CMPNAM        SCPNAM                             HIDDEN
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *   ERROR SUBROUTINE - EXCEPTION/ERROR SUBROUTINE    * P S S R ***
     C*====================================================================*
     C     ERROR         BEGSR
     C*
     C*  IF FILE IS NOT FOUND IN USERS LIBL
     C*
     C     Q1STCD        IFEQ      211
     C                   MOVE      '1'           *IN82
     C                   MOVE      '1'           ERR
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   END
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 01/08/07 **************************************
     C*====================================================================*
     C* * *   CHECK SECURITY LEVEL SUBROUTINE USER - REMARK            R ***
     C*====================================================================*
     C     AUTHCK        BEGSR
     C*
     C*  SMSLP Record has already been retreived.
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     CMKY          CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      '0'
     C*
     C     SLSECL        IFLT      CMSECL
     C                   MOVE      '1'           *IN45
     C                   MOVE      '1'           ERR
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*** END ADD *** 01/08/07 **************************************
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C*====================================================================*
     C********SUBROUTINE FOR GETTING THE NEXT AVAILABLE ASEQ#            ***
     C*====================================================================*
     C********   ASEQ      BEGSR
     C********
     C********             Z-ADD*ZEROS    XXASQ#
     C********   ARKY2     SETLLPAR01
     C********   ARKY      READEPAR01                    60
     C********
     C********   *IN60     DOWEQ'0'
     C********             Z-ADDARASQ#    XXNXT#
     C********   ARKY      READEPAR01                    60
     C********             END
     C********
     C********             ADD  1         XXNXT#
     C********
     C********             ENDSR
     C*** END DEL *** 04/25/00 ****************************************
     C*====================================================================*
** MSG - SCREEN MESSAGE
*COMPLETED
**  CK1 - Command Keys - SCR01
F3=Exit  F5=Refresh  F9=Change Info  F12=Prev  F19=Prt Diary Rpt  F22=Disp Key
F3=Exit  F5=Refresh  F9=Change Info  F12=Prev  F19=Prt Diary Rpt  F22=Disp Name
F1=OV/400   F3=Exit   F5=Refresh   F9=Change Info   F12=Prev  F19=Prt Diary Rpt
F1=OV/400   F3=Exit   F5=Refresh   F9=Change Info   F12=Prev  F19=Prt Diary Rpt
F22=Disp Key
F22=Disp Name
** ADDITIONAL COMMENT TABLE
DIARY COMPLETED BY:            DIARY DT:   /  /   DIARY COMPLETION:  /  /
