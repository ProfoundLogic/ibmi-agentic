     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H*****************************************************************
     H/TITLE SC015R - WORK WITH DIARY ITEMS                           *
     H*****************************************************************
     H*                                                               *
     H*  SCR01 - Work with Diary Items                                *
     H*     Accessed from F6 of SC010R (SCR02)                        *
     H*       Command Keys:                                           *
     H*         Disp Rmk/Key/Name(F6)  Name    F22        (CK Array)  *
     H*  OV/03    Rmk/69  Name/68     Disp/38  Avail/39   Cmd Key Order
     H*    Y-0    Y-0     N-0          Y-0     Y-1           4-7-10   *
     H*    Y-0    Y-0     N-0          N-1     Y-1           4-8-10   *
     H*    Y-0    Y-0     Y-1          Y-0     Y-1           5-7-10   *
     H*    Y-0    Y-0     Y-1          N-1     Y-1           5-8-10   *
     H*    Y-0    N-1     N-0          Y-0     Y-1           6-7-10   *
     H*    Y-0    N-1     N-0          N-1     Y-1           6-8-10   *
     H*    Y-0    N-1     Y-1          Y-0     Y-1           5-7-10   *
     H*    Y-0    N-1     Y-1          N-1     Y-1           5-8-10   *
     H*
     H*    N-1    Y-1     N-0          Y-0     Y-1           1-7-10   *
     H*    N-1    Y-1     N-0          N-1     Y-1           1-8-10   *
     H*    N-1    Y-1     Y-1          Y-0     Y-1           2-7-10   *
     H*    N-1    Y-1     Y-1          N-1     Y-1           2-8-10   *
     H*    N-1    N-0     N-0          Y-0     Y-1           3-7-10   *
     H*    N-1    N-0     N-0          N-1     Y-1           3-8-10   *
     H*    N-1    N-0     Y-1          Y-0     Y-1           2-7-10   *
     H*    N-1    N-0     Y-1          N-1     Y-1           2-8-10   *
     H*                                                               *
     H*    Y-0    Y-0     N-0          Y-0     N-0           4-9-10   *
     H*    Y-0    Y-0     N-0          N-1     N-0           4-9-10   *
     H*    Y-0    Y-0     Y-1          Y-0     N-0           5-9-10   *
     H*    Y-0    Y-0     Y-1          N-1     N-0           5-9-10   *
     H*    Y-0    N-1     N-0          Y-0     N-0           6-9-10   *
     H*    Y-0    N-1     N-0          N-1     N-0           6-9-10   *
     H*    Y-0    N-1     Y-1          Y-0     N-0           5-9-10   *
     H*    Y-0    N-1     Y-1          N-1     N-0           5-9-10   *
     H*                                                               *
     H*    N-1    Y-1     N-0          Y-0     N-0           1-9-10   *
     H*    N-1    Y-1     N-0          N-1     N-0           1-9-10   *
     H*    N-1    Y-1     Y-1          Y-0     N-0           2-9-10   *
     H*    N-1    Y-1     Y-1          N-1     N-0           2-9-10   *
     H*    N-1    N-0     N-0          Y-0     N-0           3-9-10   *
     H*    N-1    N-0     N-0          N-1     N-0           3-9-10   *
     H*    N-1    N-0     Y-1          Y-0     N-0           2-9-10   *
     H*    N-1    N-0     Y-1          N-1     N-0           2-9-10   *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/03/13  C:33  Implement GS (no comments)
     H*                                                               *
     H*  10/17/11 C:277 Project: SY195                                *
     H*                 QDLS migration to IFS                         *
     H*                 Add two parameters with command               *
     H*                                                               *
     H***  04/17/07 C:02 PJCT: 89AIH
     H*      IF DRFL13 IS NOT ' ', AND THE DOC BEING VIEWED IS OTHER THAN OV
     H*        (CMIMGF = 'O'), USE RUNRMTCMD TO SEND THE ITEM TO A PC FOR
     H****     EXECUTION UNLESS DRFL13 IS AN "S" THEN USE STRPCCMD
     H*                                                               *
     H*  11/08/04 C:136 SUPPORT: DR065 43386                          *
     H*                 Restrict user access if SMSLP/SLRACC = 'Y'    *
     H*                                                               *
     H*  06/21/04 C:136 SUPPORT: WA034 42943                          *
     H*                 The diary completion date is being cleared    *
     H*                 before the update of SDCMLA. See previous     *
     H*                 support item. Moved update statement to       *
     H*                 position prior to call of DA701R.             *
     H*
     H* 06/14/04 C:062 Project: 129WCS
     H*                GET THE DE PGM FROM SDAIP IF EXISTS
     H*                                                               *
     H*  04/13/04 C:128 Support:                                      *
     H*                 Because of changes made to DA701R that updated*
     H*                 SDCMLI. SDCMLA must be released before the    *
     H*                 update.                                       *
     H*                                                               *
     H*  06/05/03 C:128 Project: DRMNT                                *
     H*                 Allow program to execute Visual Darts if from *
     H*                 Claims Diary Summary - Client requests        *
     H*                                                               *
     H*  09/25/01 C:128 Project: DR152                                *
     H*                 Set QQRTCD to 'G' to indicate to Visual Darts *
     H*                 to display in Group Summary mode.             *
     H*                                                               *
     H*  05/31/01 C:02  Project:
     H*                 IF FROM CLAIMS DIARY SUMMARY, DONT EXECUTE
     H*                 VDARTS
     H*
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SMARP with API DA701R
     H*
     H* 01/25/00 C:113 PROJECT: SUPPORT #21566                        *
     H*                Diary item not printing when 'P' is selected.  *
     H*                Changed program to pass the key of the         *
     H*                selected item into the print program.          *
     H*                                                               *
     H* 01/11/00 C:113 PROJECT: SUPPORT #20332/#21224                 *
     H*                Correct parms for SC711R to account for change *
     H*                of Remark Type from 2a to 5a, and correct date *
     H*                size.                                          *
     H*                                                               *
     H* 12/28/99 C:062 PROJECT: PR001                                 *
     H*                MAKE SURE ' 7' PROCESSES THE SAME AS '7 '; ETC.*
     H*                                                               *
     H* 11/19/99 C:113 PROJECT: DR079                                 *
     H*                Changed to check for installation of VDARTS    *
     H*                and check users security access to same.       *
     H*                Provide F23=Visual DARTS for users with        *
     H*                access.                                        *
     H*                Also, correct security flag usage.             *
     H*                                                               *
     H*  10/27/99 C:15 Project:                                       *
     H*                *IN99 is the image indicator but subsequent    *
     H*                code changes use this indicator for general    *
     H*                usage.  Changed the general usage to *IN60.    *
     H*                                                               *
     H*  04/16/99 C:02 SUPPORT: DR098 #17398                          *
     H*                CORRECTED DISPLAY WHEN USING 0 FOR DATE        *
     H*                                                               *
     H*  12/09/98 C:94 SUPPORT: DR095 #15470                          *
     H*                CORRECTED DISPLAY OF DATE                      *
     H*                                                               *
     H*  10/20/98 C:53 PROJECT: DRMNT                                 *
     H*                CORRECT RECURSIVE CALL FOR WCMC PGM            *
     H*                                                               *
     H*  10/06/98 C:53 PROJECT: DR051 # 14150                         *
     H*                PROHIBIT USE OF OPT 3 IF NO RECORD EXISTS      *
     H*                                                               *
     H*  09/28/98 C:02 IMAGE: USE IDOC (IMAGE DOC) ON CALL TO SCIMGP  *
     H*                                                               *
     H*  08/25/98 C:98 PROJECT: DR095                                 *
     H*               CORRECT ROLLUP ISSUE                            *
     H*                                                               *
     H*  08/18/98 C:98 PROJECT: DR064                                 *
     H*               ADD P=PRINT RMK TO WORK WITH DIARY ITEMS SCREEN *
     H*                                                               *
     H*  08/13/98 C:98 PROJECT: DR065                                 *
     H*               APPLY DIARY USER SECURITY IN WORK WITH DIARY    *
     H*               ITEMS                                           *
     H*                                                               *
     H*  07/25/98 C:02 Support: 12611                                 *
     H*               ADDED PARM FROM SYSCTL TO THE DSPDOC, PRTDOC, AND
     H*               EDTDOC CALLS FOR MICROSOFT PRODUCTS.            *
     H*                                                               *
     H*  07/21/98 C:69 Support: 12666                                 *
     H*                Changed the way the check for a recursive call *
     H*                is done                                        *
     H*                                                               *
     H*  6/25/98 C:69 Support: 12377                                  *
     H*               Corrected two items: 1. When F7 was pressed,    *
     H*               options would not be accessed the first time    *
     H*               afterwards. 2. After an option is entered and   *
     H*               the subfile screen is redisplayed, the cursor   *
     H*               will stay at the option instead of returning to *
     H*               the first item.                                 *
     H*                                                               *
     H* 4/28/98 C:62 Project: DR048  Support #: 11318                 *
     H*              MAKE SURE THAT WHEN RETURNING FROM USER SEARCH,  *
     H*              THE SCREEN REFRESHES TO TAKE INTO ACCOUNT ANY    *
     H*              CHANGE IN USER.                                  *
     H*                                                               *
     H* 2/11/98 C:02 Project: DR044  Support #: 9716                  *
     H*               Added a default entry into the comment if it is *
     H*               determined to be blanks to allow functions to   *
     H*               be performed                                    *
     H*                                                               *
     H*  01/05/98 C:02    PROJ: ISSIMG                                *
     H*   MODIFIED THE PARAMETERS TO PASS TO THE SCIMGV PROGRAM       *
     H*   PASSING IN THE KEY, SEQ#, DCID , AND FOLD                   *
     H*                                                               *
     H* 10/21/97 C:15 Project: WC577                                  *
     H*               Changed length of QQDATE to correct displaying  *
     H*               of the reposition date when called from SC015R1.*
     H*                                                               *
     H*  09/12/97 C:30    Project:  DR015  Support#:  8565            *
     H*           Change QUE assignment to use User Parm OUTQ.        *
     H*                                                               *
     H* 07-18-91 LJQ                                                  *
     H* 12/18/91 LJD    - Proj: SC083                                 *
     H* 02/08/92 LJD    - Proj: SC090                                 *
     H*  02/19/92 LJD   - Proj: SC088                                 *
     H*  03/18/92 WLD   - Proj: ANE                                   *
     H*  03/23/92 WLD   - Proj: AJP                                   *
     H*  04/06/92 LJD   - Proj: ANI                                   *
     H*  04/07/92 WLD   - Proj: AJZ                                   *
     H*  06/18/92 WLD   - Proj:                                       *
     H*  06/24/92 WLD   - Proj: AOF                                   *
     H*  07/22/92 WLD   - Proj: AOF                                   *
     H*  07/29/92 WLD   - Proj: AJG                                   *
     H*  08/13/92 WLD   - Proj: AMN                                   *
     H*  08/20/92 WLD   - Proj: ANG                                   *
     H*  08/25/92 WLD   - Proj: ANG                                   *
     H*  08/25/92 WLD   - Proj: ANJ                                   *
     H*  08/28/92 WLD   - Proj:                                       *
     H*  09/09/92 WLD   - Proj: AMN                                   *
     H*  09/10/92 WLD   - Proj: ANG                                   *
     H*  10/20/92 JJJ   - Proj: AOA                                   *
     H*  11/16/92 C:10  - Proj:                                       *
     H*  11/17/92 C:10  - Proj:                                       *
     H*  12/11/92 C:10  - Proj: AMN                                   *
     H*  01/11/93 C:10  - Proj: ASQ                                   *
     H*  03/02/93 C:15  PROJ: AMN                                     *
     H*  05/10/93 02    - Proj:OPT                                    *
     H*  08/18/94 C:09  PROJ: ANG                                     *
     H*  02/07/95 C:20  - PROJ: DR002                                 *
     H*  08/15/95 C:15  - PROJ:                                       *
     H*  08/31/95 C:15  - PROJ: 25WAR                                 *
     H*  08/01/96 C:59  - PROJ: 25MES                                 *
     H*                                                               *
     H*  01/22/98 C:85  - PROJ: DR010                                 *
     H*           ADDED TEST FOR SUCCESSFUL CHAIN TO RECORD FORMAT    *
     H*           PCM0A IN FILE SDCMLA.                               *
     H*                                                               *
     H*  ADD NEW REVISION REMARKS AT TOP OF REVISON SECTION           *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  03    (Office NOT installed) Nondisplay of F1 = OV/400       *
     H* N03    (Office IS installed)  Display/Use of F1 = OV/400      *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON OPT FLAG FIELDS                                     *
     H*  20    SCM0515-NOT AUTHORIZED TO EDIT DOCUMENT                *
     H*  21    SCM0514-NOT AUTHORIZED TO DISPLAY DOCUMENT             *
     H*  22    SCM0510-INVALID VALUE KEYED                            *
     H*  23    SCM0516-DOCUMENT NOT FOUND IN SPECIFIED FOLDER         *
     H*  24    SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER           *
     H*  25    SCM0518-USER NOT ENROLLED IN OFFICE 400                *
     H*  26    SCM0518-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT    *
     H*  29    Used to allow End of file on roll message to display   *
     H*        at the correct time.                                   *
     H*  30    SCM0500-INVALID DATE                                   *
     H*  31      SCM0550-AS/400 Officevision is not installed         *
     H*  32    ROLL UP INDICATOR                                      *
     H*  35                                                           *
     H*  38 39 KEY is displayed (F22)                                 *
     H* N38 39 NAME is displayed (F22)                                *
     H* N39    NAME/KEY toggle is not available (F22)                 *
     H*  40    GENERAL CHAIN                                          *
     H*  41    CHAIN ON PCM0A                                         *
     H*  42    SCM0622-OfficeVision/400 is already active             *
     H*  43    SCM0532-Default Folder required if Document ID indicated
     H*  44    SCM0519-Unable to process request for this document    *
     H*  49    Used to indicate to the LOAD routine that a REFRESH (F5)
     H*        is being performed.                                    *
     H*  50    READ ON SCR01                                          *
     H*  51                                                           *
     H*  52                                                           *
     H*  53                                                           *
     H*  57    SCM0503-Value cannot be equal to blanks                *
     H*  58    SCM0612-User is not enrolled in DARTS                  *
     H*  59                                                           *
     H*  60    READ ON PCM01                                          *
     H*  61    CHAIN ON PAR01                                         *
     H*  68    NAME Column is displayed in subfile (F6)               *
     H* N68 69 KEY is displayed in subfile (F6)                       *
     H* N68N69 REMARK is displayed in subfile (F6)                    *
     H*  70    Set on if QQRMCD NE BLANK.  (i.e. pgm was called from  *
     H*        SC015R1 - Claims Diary Summary)                        *
     H*  71    GLOBAL - DATA ENTRY PROGRAM IS INSTALLED               *
     H*  72    CMFL04 = 'Y' -- DEPGM Diary Item is highlighted        *
     H* N72    CMFL04 = ' ' -- DEPGM Diary Item is not highlighted    *
     H*  80    GLOBAL - Used on call to Data Entry Program (DEPGM)    *
     H*  81    SCM0619-Must find alternative access to this data entry*
     H*                program.                                       *
     H*  82    SCM0620-Data entry program is not available at this    *
     H*                time.                                          *
     H*  83    Set on if User has security level to change to another *
     H*                user's diary.                                  *
     H*****************************************************************
     FSC015F    CF   E             WORKSTN usrOpn
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSDCML4    IF   E           K DISK
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)
     FSDCMLG    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0G)
     FSMSLL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*** BEGIN CHG *** 04/25/00 **************************************
     F*****SMARL   UF  E           K        DISK                      A
     FSMARL     IF   E           K DISK
     F*** END CHG *** 04/25/00 ****************************************
     F*** BEGIN DLT *** 09/12/97 ***********************************************
     F***SMUPL   IF  E           K        DISK
     F*** END   DLT *** 09/12/97 ***********************************************
     F*** BEGIN ADD *** 08/18/98 ***********************************************
     FSMUPL     IF   E           K DISK
     F*** END   ADD *** 08/18/98 ***********************************************
     F*** BEGIN ADD *** 06/14/04 **************************************
     FSDAIL     IF   E           K DISK
     F*** END   ADD *** 06/14/04 **************************************
      *=========================================================================
      *  Array & Table Definitions
      *=========================================================================

     D*
     D MSG             S             10    DIM(1) CTDATA PERRCD(1)              SCREEN MESSAGE
      *** BEGIN CHG *** 11/19/99 ***********************************************
     D******************* CK1     1   9 70               COMMAND KEYS - SCR01
     D CK1             S             70    DIM(10) CTDATA PERRCD(1)             COMMAND KEYS - SCR01
      *** BEGIN CHG *** 11/19/99 ***********************************************
     D MSG2            S             40    DIM(1) CTDATA PERRCD(1)              TOP SCREEN MSG
     D*
     D*    COMMENT USED TO WRITE DIARY COMPLETION INFORMATION TO
     D*       ADDITIONAL REMARKS FILE
     D*
     D CMT             S             75    DIM(1) CTDATA PERRCD(1)
      *=========================================================================
      *  Data Structure and Constant Definitions
      *=========================================================================
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  XXSM                   7      8
     D  XXSD                   9     10
     D  XXSY                  11     12
     D                 DS
     D  XXCDAT                 1      7  0
     D  XXCC                   1      1  0
     D  XXCY                   2      3  0
     D  XXCM                   4      5  0
     D  XXCD                   6      7  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*    COMMENT STRING
     D*
     D                 DS
     D  @@CMT                  1     75
     D  @@USER                21     30
     D  @@CM                  42     43
     D  @@CD                  45     46
     D  @@CY                  48     49
     D  @@SM                  68     69
     D  @@SD                  71     72
     D  @@SY                  74     75
     DisUseGS          s              1n   inz
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D/copy sPrAtrium

      /Free
       isUseGS = not AtriumJob;

       Select;
       When AtriumJob;
          Open SC015F;
       Endsl;
      /End-free

      *=========================================================================
      *  KLIST Definitions
      *=========================================================================

     C*** BEGIN ADD *** 06/14/04 **************************************
     C     AIKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*** END   ADD *** 06/14/04 **************************************
     C*
     C*   KEY LIST FOR THE COMMENT DETAIL FILE
     C*
     C     CMKEY4        KLIST
     C                   KFLD                    SCUSER
     C*
     C     CMKEYA        KLIST
     C                   KFLD                    SCKEY
     C                   KFLD                    SCSEQ#
     C*
     C     CMKYA2        KLIST
     C                   KFLD                    SCDKEY
     C*
     C     CMKYG         KLIST
     C                   KFLD                    SCUSER
     C                   KFLD                    QQRMCD
     C*
     C*   KEY LIST FOR THE DARTS SECURITY FILE
     C*
     C     SLKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     SLKY          KLIST
     C                   KFLD                    KKUSER
     C*
     C*
     C*   KEY LIST FOR THE SYSTEM CTL FILE
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
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
      *=========================================================================
      *  PLIST Definitions
      *=========================================================================
      *** BEGIN ADD *** 09/12/97 **************************************
      *
      *  User Parameter file
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *** END   ADD *** 09/12/97 **************************************
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQRMCD
     C                   PARM                    QPDATE            8
     C                   PARM                    QQUSER           10
      *
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
      *** BEGIN ADD *** 11/19/99 ***********************************************
      *
      ** Visual DARTS call program
     C     VBDPL         PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQRMCD
     C                   PARM                    QPDATE            8
     C                   PARM                    QQUSER           10
      *** END   ADD *** 11/19/99 ***********************************************
      *=========================================================================
      *  *LIKE DEFN Definitions
      *=========================================================================
     C     *LIKE         DEFINE    RRN2          SRRN2
     C     *LIKE         DEFINE    CMCDAT        XXDATE                         SCREEN DATE
     C     *LIKE         DEFINE    CMPNAM        #7PNAM                         ADD 02/08/92
     C     *LIKE         DEFINE    DRAL02        OFCPGM                         ADD 04/06/92
     C     *LIKE         DEFINE    CMKEY         XHDKEY                         ADD 04/07/92
     C     *LIKE         DEFINE    CMRMCD        QQRMCD                         REMARK CODE
     C     *LIKE         DEFINE    SCDATE        QQDATE
     C     *LIKE         DEFINE    SCDATE        XHDATE                         HOLD DATE
     C     *LIKE         DEFINE    CMKEY         DEKEY
     C     *LIKE         DEFINE    CMKEY         ZZKEY
     C     *LIKE         DEFINE    CMSEQ#        ZZSEQ#
     C     *LIKE         DEFINE    CMCDAT        ZZCDAT
     C     *LIKE         DEFINE    Q1USER        XHUSER                         ADD 01/11/93
     C     *LIKE         DEFINE    SLUSRP        ##USER
     C     *LIKE         DEFINE    SLUSRP        KKUSER
     C     *LIKE         DEFINE    SLSECL        XXSECL
     C**** BEGIN ADD **** 08/13/98 ************************************
     C     *LIKE         DEFINE    SLSECL        YYSECL
     C**** END   ADD **** 08/13/98 ************************************
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   *LIKE     DEFN ARASQ#    XXNXT#           ADD 01/11/93
     C********   *LIKE     DEFN ARASQ#    XXASQ#           ADD 01/11/93
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 06/14/04 **************************************
     C*
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
     C*** END   ADD *** 06/14/04 **************************************
      *** BEGIN ADD *** 11/19/99 ***********************************************
     C                   MOVE      ' '           XFVDTA            1
     C                   MOVEL     'SC015RVB'    VBDART            8
      *** END   ADD *** 11/19/99 ***********************************************
      *
      *** Begin Add *** 09/25/01 ***********************************************
     C                   MOVE      QQRTCD        XXRTCD            1
     C                   MOVE      *BLANK        QQRTCD
      *** End   Add *** 09/25/01 ***********************************************
      *=========================================================================
      *  Main
      *=========================================================================
     C     QQUSER        IFEQ      *BLANKS
     C                   MOVEL     Q1USER        QQUSER
     C                   END
     C*
     C                   MOVE      QPDATE        QQDATE
     C*
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVE      '1'           *IN70
     C                   ELSE
     C                   MOVE      '0'           *IN70
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANKS       DERTCD            1
     C                   MOVE      *BLANKS       DECOM1          256
     C                   MOVE      *BLANKS       DECOM2           19
     C                   MOVE      *BLANKS       FOUND1            1
     C                   MOVE      *BLANKS       FOUND2            1
     C                   Z-ADD     0             C1                5 0          ADD 04/06/92
     C*
     C*   Check System Control File Install Flags for this Client:
     C*
     C                   MOVEL     'SC'          XXSYK
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C*
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      '0'           *IN03                          Installed
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      '1'           *IN03                          Not Installed
     C                   END                                                    END 'Y' SYOFIF
     C*
     C                   END                                                    END 0 *IN60
     C*
     C**** BEGIN ADD **** 08/13/98 ************************************
     C*
     C* GET MINIMUM SECURITY LEVEL TO BE ABLE TO CHANGE TO ANOTHER
     C*     USER'S DIARY.
     C*
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** 'SC'      CHAINSDR01                99
     C     'SC'          CHAIN     SDR01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C                   MOVE      DRDT02        YYSECL
     C*
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKEY     CHAINPSL01                99
     C     SLKEY         CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C                   MOVE      SLSECL        XXSECL
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C**** END   ADD **** 08/13/98 ************************************
     C*
     C*** BEG ADD ***** 11/08/04 **************************************
     C     SLRACC        IFEQ      'Y'
     C     *IN60         ANDEQ     *OFF
     C                   GOTO      TERM
     C                   ENDIF
     C*** END ADD ***** 11/08/04 **************************************
     C*
      *** BEGIN ADD *** 11/19/99 ***********************************************
      *
      ** Check for the installation of VDARTS.
     C     DRFL10        IFEQ      'Y'
     C                   EXSR      SRVDAR
     C                   ELSE
     C                   MOVE      ' '           XFVDTA
     C                   MOVE      *OFF          *IN02
     C                   ENDIF                                                  DRFL07 = 'Y'
      *** END   ADD *** 11/19/99 ***********************************************
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*  If Officevision/400 is not installed, use first CMD line if
     C*     QQRMCD is blanks, otherwise, use sixth CMD line
     C*
     C     *IN03         IFEQ      '1'
     C     QQRMCD        IFNE      *BLANKS                                      ADD 08/13/92
     C                   Z-ADD     2             C1                             ADD 08/13/92
     C                   MOVEL     CK1(C1)       SCCML1                         ADD 08/13/92
     C                   ELSE                                                   ADD 08/13/92
     C                   Z-ADD     1             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   END                                                    ADD 08/13/92
     C                   ELSE
     C*
     C*  If Officevision/400 is installed, use fourth CMD line if
     C*     QQRMCD is blanks, otherwise, use sixth CMD line
     C*
     C     QQRMCD        IFNE      *BLANKS                                      ADD 08/13/92
     C                   Z-ADD     5             C1                             ADD 08/13/92
     C                   MOVEL     CK1(C1)       SCCML1                         ADD 08/13/92
     C                   ELSE                                                   ADD 08/13/92
     C                   Z-ADD     4             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   END                                                    ADD 08/13/92
     C*
     C                   END
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C* SET INITIAL DISPLAY DATE TO SYSTEM DATE
     C*  IF SCREEN DATE = 0 THEN DISPLAY EVERY THING
     C*
     C*  IF A REMARK CODE WAS PASSED INTO THE PROGRAM, INITIALIZE
     C*     THE DISPLAY DATE TO THE DATE PASSED IN
     C*
     C     QQRMCD        IFEQ      *BLANKS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C                   ELSE
     C                   Z-ADD     QQDATE        SCDATE
     C                   END
     C                   Z-ADD     SCDATE        XHDATE
     C*
     C*  SET INITIAL DISPLAY KEY TO VALUE THAT WAS PASSED INTO THE
     C*  PROGRAM
     C*
     C                   MOVEL     QQKEY         SCDKEY
     C                   MOVEL     QQKEY         XHDKEY
     C*
     C*  SET INITIAL DISPLAY USER TO VALUE THAT WAS PASSED INTO THE
     C*  PROGRAM (IF QQUSER WAS BLANK, Q1USER WAS MOVED IN UP ABOVE)
     C*
     C                   MOVE      *BLANKS       SCUSER
     C                   MOVEL     QQUSER        SCUSER
     C                   MOVEL     QQUSER        XHUSER
     C*
     C*** BUILD INITIAL KEY DISPLAY HERE
     C*   This is called initially in order to allow a test for
     C*   claimant name = blank.  If this condition exists, the key is
     C*   initially displayed; otherwise the name is.
     C*
     C     CMKYA2        CHAIN(N)  PCM0A                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     *BLANKS       X#CNAM
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#CNAM           79
     C                   PARM                    X#RTCD            1
     C*
     C                   END
     C*
     C* INITIALIZE INDICATORS
     C*
     C     QQRMCD        IFNE      *BLANKS                                      ADD 08/13/92
     C                   MOVE      '1'           *IN68                          ADD 08/13/92
     C                   MOVE      '0'           *IN69
     C                   ELSE                                                   ADD 08/13/92
     C                   MOVE      '0'           *IN68
     C                   MOVE      '0'           *IN69
     C                   END                                                    ADD 08/13/92
     C*
     C     SCDKEY        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN38
     C                   ELSE
     C     X#CNAM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN38
     C                   ELSE
     C                   MOVE      '0'           *IN38
     C                   END
     C                   END
     C*
     C  N70CMKEY4        SETLL     PCM01                              60
     C   70CMKYG         SETLL     PCM0G                              60
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVE      '0'           *IN39
     C                   ELSE                                                   QQRMCD EQ *BLAN
     C*
     C* This test is performed each time the user presses enter.  If
     C* the key field at the top of the screen is blanked out, then
     C* F22 (the Name/Key toggle) is not available.
     C*
     C     SCDKEY        IFEQ      *BLANKS
     C                   MOVE      '0'           *IN39
     C*
     C     C1            IFEQ      7
     C     C1            OREQ      8
     C                   Z-ADD     9             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   ENDIF                                                  C1 EQ 7 OR 8
     C*
     C                   ELSE                                                   SCDKEY NE *BLAN
     C                   MOVE      '1'           *IN39
     C*
     C     C1            IFEQ      9
     C     *IN38         IFEQ      '1'
     C                   Z-ADD     8             C1
     C                   ELSE                                                   *IN38 NE '1'
     C                   Z-ADD     7             C1
     C                   ENDIF                                                  *IN38 EQ '1'
     C                   MOVEL     CK1(C1)       SCCML1
     C                   ENDIF                                                  C1 EQ 9
     C*
     C*** BUILD KEY DISPLAY HERE
     C*
     C     CMKYA2        CHAIN(N)  PCM0A                              60
     C*** BEGIN ADD *** 01/22/98 **************************************
     C     *IN60         IFEQ      *OFF
     C*** END   ADD *** 01/22/98 **************************************
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     *BLANKS       X#CNAM
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#CNAM           79
     C                   PARM                    X#RTCD            1
     C*** BEGIN ADD *** 01/22/98 **************************************
     C                   ENDIF                                                  *IN60=OFF
     C*** END   ADD *** 01/22/98 **************************************
     C*
     C                   MOVE      *BLANKS       SCNMKY
     C     *IN38         IFEQ      '0'
     C     X#CNAM        IFEQ      *BLANKS
     C                   MOVEL     MSG2(1)       SCNMKY
     C                   ELSE                                                   X#CNAM NE *BLAN
     C                   MOVEL     X#CNAM        SCNMKY
     C                   ENDIF                                                  X#CNAM EQ *BLAN
     C                   ELSE                                                   *IN38 NE '0'
     C                   MOVEL     X#KEY         SCNMKY
     C                   ENDIF                                                  *IN38 EQ '0'
     C                   ENDIF                                                  SCDKEY EQ *BLAN
     C                   ENDIF                                                  QQRMCD NE *BLAN
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   ENDIF                                                  Q2RECF EQ 1 C S
     C*
     C*   THIS WAS ADDED TO KEEP SCREEN FROM "FLIPPING OVER" AT THE END
     C*      OF A "ROLL-UP" AFTER RETURN FROM A SELECTION.
     C*
     C                   MOVE      XXRRN2        #@$2
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C****************************************
     C*   F1 = OV/400                        *
     C****************************************
     C     *INKA         IFEQ      '1'
     C*
     C* Get Office CLP Program Name from SDRPF/DRAL02
     C*
     C                   MOVE      *BLANKS       OFCPGM
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   END
     C*
     C                   CALL      OFCPGM
     C                   PARM      *BLANKS       QQRTCD
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
     C*** BEGIN ADD *** 04/28/98 **************************************
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKEY     CHAINPSL01                99
     C     SLKEY         CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C*
     C                   MOVEL     SLSECL        XXSECL
     C*
     C                   MOVEL(P)  SCUSER        KKUSER
     C*
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKY      CHAINPSL01                99
     C     SLKY          CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C**** BEGIN ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C*
     C**** END   ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      SLSECL
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** *IN99     ANDEQ*OFF
     C     *IN60         ANDEQ     *OFF
     C*** END CHG ***** 10/27/99 **************************************
     C*
     C                   EXSR      KEDO
     C*
     C                   ELSE
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 04/28/98 **************************************
     C*
     C                   END                                                    END *INKE
     C****************************************
     C*   F6 = TOGGLE BETWEEN REMARK AND KEY *
     C****************************************
     C     *INKF         IFEQ      '1'
     C*** BEGIN ADD *** 04/28/98 **************************************
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKEY     CHAINPSL01                99
     C     SLKEY         CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C*
     C                   MOVE      SLSECL        XXSECL
     C*
     C                   MOVEL(P)  SCUSER        KKUSER
     C*
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKY      CHAINPSL01                99
     C     SLKY          CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C*
     C**** BEGIN ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C*
     C**** END   ADD **** 08/13/98 ************************************
     C     XXSECL        IFGE      SLSECL
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** *IN99     ANDEQ*OFF
     C     *IN60         ANDEQ     *OFF
     C*** END CHG ***** 10/27/99 **************************************
     C*** END   ADD *** 04/28/98 **************************************
     C*
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C     C1            IFEQ      1
     C                   Z-ADD     3             C1
     C                   MOVE      '0'           *IN68
     C                   MOVE      '1'           *IN69
     C                   ELSE
     C*
     C     C1            IFEQ      2
     C                   Z-ADD     1             C1
     C                   MOVE      '0'           *IN68
     C                   MOVE      '0'           *IN69
     C                   ELSE
     C*
     C     C1            IFEQ      3
     C                   Z-ADD     2             C1
     C                   MOVE      '1'           *IN68
     C                   ELSE
     C*
     C     C1            IFEQ      4
     C                   Z-ADD     6             C1
     C                   MOVE      '0'           *IN68
     C                   MOVE      '1'           *IN69
     C                   ELSE
     C*
     C     C1            IFEQ      5
     C                   Z-ADD     4             C1
     C                   MOVE      '0'           *IN68
     C                   MOVE      '0'           *IN69
     C                   ELSE
     C*
     C     C1            IFEQ      6
     C                   Z-ADD     5             C1
     C                   MOVE      '1'           *IN68
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     C1            IFEQ      7
     C     C1            OREQ      8
     C     C1            OREQ      9
     C*
     C     *IN68         IFEQ      '1'
     C                   MOVE      '0'           *IN68
     C                   MOVE      '0'           *IN69
     C                   ELSE
     C     *IN69         IFEQ      '1'
     C                   MOVE      '1'           *IN68
     C                   ELSE
     C                   MOVE      '0'           *IN68
     C                   MOVE      '1'           *IN69
     C                   END
     C                   END
     C                   END
     C*
     C  N70CMKEY4        SETLL     PCM01                              60
     C   70CMKYG         SETLL     PCM0G                              60
     C                   EXSR      LOAD
     C*
     C                   MOVEL     CK1(C1)       SCCML1
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ELSE
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF
     C*
     C                   END                                                    END *INKF
     C****************************************
     C*  F7 = USER PROFILE SEARCH           *
     C****************************************
     C     *INKG         IFEQ      '1'
     C*
     C     SLKEY         CHAIN     PSL01                              60
     C     *IN60         IFEQ      '0'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C********** SLF07     IFEQ 'Y'
     C     SLF09         IFEQ      'Y'
      *** END   CHG *** 11/19/99 ***********************************************
     C*
     C                   MOVEL     SLSECL        XXSECL
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C*
     C                   CALL      'SC530R'
     C                   PARM      *BLANKS       ##USER
     C                   PARM      *BLANKS       QQRTCD
     C*
     C*** BEGIN ADD *** 04/28/98 **************************************
     C     QQRTCD        IFEQ      '*'
     C     ##USER        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  ##USER        KKUSER
     C*
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKY      CHAINPSL01                99
     C     SLKY          CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C*
     C     XXSECL        IFGE      SLSECL
     C*
     C                   MOVEL     ##USER        SCUSER
     C                   MOVEL     ##USER        XHUSER
     C*
     C                   EXSR      KEDO
     C*
     C                   ELSE
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 04/28/98 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END SLF09=Y
     C                   END                                                    END *IN60=0
     C                   GOTO      STRT
     C                   END                                                    END *INKG
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
     C*** BEGIN CHG *** 01/11/00 ***********************************************
     C**********           MOVE *ZEROS    QRFRDT  6
     C**********           MOVE *ZEROS    QRTODT  6
     C                   MOVE      *ZEROS        QRFRDT            8
     C                   MOVE      *ZEROS        QRTODT            8
     C                   MOVE      *BLANKS       QRSMCD            4            ADD 02/07/95
     C**********           MOVE *BLANKS   QRRMTY  2
     C                   MOVE      *BLANKS       QRRMTY            5
     C*** END   CHG *** 01/11/00 ***********************************************
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
     C                   PARM                    QRSMCD                         ADD 02/07/95
     C                   PARM                    QRRMTY
     C                   PARM                    QRDCID
     C                   PARM                    QRTYPE
     C                   PARM                    QRDITY
     C                   PARM                    QRSORT
     C                   PARM                    QRRTCD
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKT
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C*
     C     *IN29         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '0'           *IN29
     C                   END
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C*****************************************
     C*   F22 = TOGGLE BETWEEN REMARK AND KEY *
     C*****************************************
     C     *INKW         IFEQ      '1'
     C*
     C* * *     SETON OR OFF *IN38
     C     *IN38         IFEQ      '1'
     C                   MOVE      '0'           *IN38
     C                   ELSE
     C                   MOVE      '1'           *IN38
     C                   END                                                    END *IN38
     C*
     C     C1            IFEQ      7
     C                   Z-ADD     8             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   ELSE
     C     C1            IFEQ      8
     C                   Z-ADD     7             C1
     C                   MOVEL     CK1(C1)       SCCML1
     C                   END
     C                   END
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END *INKW
      *** BEGIN ADD *** 11/19/99 ***********************************************
     C****************************************
     C*   F23=Visual DARTS                   *
     C****************************************
     C     *INKX         IFEQ      *ON
      *
     C     XFVDTA        IFEQ      'Y'
     C                   CALL      VBDART        VBDPL
     C                   ENDIF                                                  XFVDTA = *ON
      *
     C                   ENDIF                                                  *INKV = *ON
      *** END   ADD *** 11/19/99 ***********************************************
     C****************************************
     C*   F24=More Keys                      *
     C****************************************
     C     *INKY         IFEQ      '1'
     C*
     C* Ofc Installed, F6=Disp Key, Rmk Disp, F22=Disp Key, Name Disp
     C*
     C     *IN03         IFEQ      '0'
     C     *IN69         ANDEQ     '0'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '0'
     C     *IN39         ANDEQ     '1'
      *** BEGIN CHG *** 11/19/99 ***********************************************
     C                   SELECT
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     4             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     4             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Key, Rmk Disp, F22=Disp Name, Key Disp
     C*
     C     *IN03         IFEQ      '0'
     C     *IN69         ANDEQ     '0'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '1'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     8             C1
      *
     C     C1            WHENEQ    8
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     4             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     4             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Rmk, Name Disp, F22=Disp Key, Name Disp
     C*
     C     *IN03         IFEQ      '0'
     C     *IN68         ANDEQ     '1'
     C     *IN38         ANDEQ     '0'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     5             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     5             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Rmk, Name Disp, F22=Disp Name, Key Disp
     C*
     C     *IN03         IFEQ      '0'
     C     *IN68         ANDEQ     '1'
     C     *IN38         ANDEQ     '1'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     8             C1
      *
     C     C1            WHENEQ    8
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     5             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     5             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Name, Key Disp, F22=Disp Key, Name Disp
     C*
     C     *IN03         IFEQ      '0'
     C     *IN69         ANDEQ     '1'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '0'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     6             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     6             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Name, Key Disp, F22=Disp Name, Key Disp
     C*
     C     *IN03         IFEQ      '0'
     C     *IN69         ANDEQ     '1'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '1'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     8             C1
      *
     C     C1            WHENEQ    8
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     6             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     6             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Key, Rmk Disp, F22=Disp Key, Name Disp
     C*
     C     *IN03         IFEQ      '1'
     C     *IN69         ANDEQ     '0'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '0'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    1
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Key, Rmk Disp, F22=Disp Name, Key Disp
     C*
     C     *IN03         IFEQ      '1'
     C     *IN69         ANDEQ     '0'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '1'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    1
     C                   Z-ADD     8             C1
      *
     C     C1            WHENEQ    8
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Rmk, Name Disp, F22=Disp Key, Name Disp
     C*
     C     *IN03         IFEQ      '1'
     C     *IN68         ANDEQ     '1'
     C     *IN38         ANDEQ     '0'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     2             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Rmk, Name Disp, F22=Disp Name, Key Disp
     C*
     C     *IN03         IFEQ      '1'
     C     *IN68         ANDEQ     '1'
     C     *IN38         ANDEQ     '1'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     8             C1
      *
     C     C1            WHENEQ    8
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     2             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Name,Key Disp, F22=Disp Key, Name Disp
     C*
     C     *IN03         IFEQ      '1'
     C     *IN69         ANDEQ     '1'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '0'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     7             C1
      *
     C     C1            WHENEQ    7
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     3             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     3             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Name,Key Disp, F22=Disp Name, Key Disp
     C*
     C     *IN03         IFEQ      '1'
     C     *IN69         ANDEQ     '1'
     C     *IN68         ANDEQ     '0'
     C     *IN38         ANDEQ     '1'
     C     *IN39         ANDEQ     '1'
     C                   SELECT
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     8             C1
      *
     C     C1            WHENEQ    8
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     3             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     3             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Key, Rmk Disp, F22 Not Available
     C*
     C     *IN03         IFEQ      '0'
     C     *IN69         ANDEQ     '0'
     C     *IN68         ANDEQ     '0'
     C     *IN39         ANDEQ     '0'
     C                   SELECT
      *
     C     C1            WHENEQ    4
     C                   Z-ADD     9             C1
      *
     C     C1            WHENEQ    9
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     4             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     4             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Rmk, Name Disp, F22 Not Available
     C*
     C     *IN03         IFEQ      '0'
     C     *IN68         ANDEQ     '1'
     C     *IN39         ANDEQ     '0'
     C                   SELECT
      *
     C     C1            WHENEQ    5
     C                   Z-ADD     9             C1
      *
     C     C1            WHENEQ    9
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     5             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     5             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc Installed, F6=Disp Name, Key Disp, F22 Not Available
     C*
     C     *IN03         IFEQ      '0'
     C     *IN69         ANDEQ     '1'
     C     *IN68         ANDEQ     '0'
     C     *IN39         ANDEQ     '0'
     C                   SELECT
      *
     C     C1            WHENEQ    6
     C                   Z-ADD     9             C1
      *
     C     C1            WHENEQ    9
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     6             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     6             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Key, Rmk Disp, F22 Not Available
     C*
     C     *IN03         IFEQ      '1'
     C     *IN69         ANDEQ     '0'
     C     *IN68         ANDEQ     '0'
     C     *IN39         ANDEQ     '0'
     C                   SELECT
      *
     C     C1            WHENEQ    1
     C                   Z-ADD     9             C1
      *
     C     C1            WHENEQ    9
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     1             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     1             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Rmk, Name Disp, F22 Not Available
     C*
     C     *IN03         IFEQ      '1'
     C     *IN68         ANDEQ     '1'
     C     *IN39         ANDEQ     '0'
     C                   SELECT
      *
     C     C1            WHENEQ    2
     C                   Z-ADD     9             C1
      *
     C     C1            WHENEQ    9
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     2             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     2             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C* Ofc NOT Instld, F6=Disp Name,Key Disp, F22 Not Available
     C*
     C     *IN03         IFEQ      '1'
     C     *IN69         ANDEQ     '1'
     C     *IN68         ANDEQ     '0'
     C     *IN39         ANDEQ     '0'
     C                   SELECT
      *
     C     C1            WHENEQ    3
     C                   Z-ADD     9             C1
      *
     C     C1            WHENEQ    9
     C     XFVDTA        IFEQ      'Y'
     C                   Z-ADD     10            C1
     C                   ELSE
     C                   Z-ADD     3             C1
     C                   ENDIF                                                  XFVDTA = 'Y'
      *
     C     C1            WHENEQ    10
     C                   Z-ADD     3             C1
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
      *** END   CHG *** 11/19/99 ***********************************************
     C                   MOVEL     CK1(C1)       SCCML1
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKY = *ON
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  CMDKEYS
     C**************************************************************************
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C* * * EDIT FIELDS
     C*
     C                   MOVE      '0'           ERR
     C*
     C*  User name can not be blank and must be valid.
     C*
     C     SCUSER        CABEQ     *BLANK        STRT                     57
     C*
     C                   MOVEL(P)  SCUSER        KKUSER
     C*
     C     SLKY          CHAIN     PSL01                              58
     C     *IN58         CABEQ     '1'           STRT
     C*
     C                   MOVEL     SLSECL        XXSECL
     C*
     C* USER MUST HAVE SUFFICIENT SECURITY LEVEL TO ACCESS THESE RMKS.
     C*
     C*** BEGIN CHG *** 10/27/99 **************************************
     C********** SLKEY     CHAINPSL01                99
     C     SLKEY         CHAIN     PSL01                              60
     C*** END CHG ***** 10/27/99 **************************************
     C*
     C**** BEGIN ADD **** 08/13/98 ************************************
     C*
     C     XXSECL        IFGE      YYSECL
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  ENDIF GE YYSECL
     C*
     C**** END   ADD **** 08/13/98 ************************************
     C     XXSECL        IFGT      SLSECL
     C*
     C                   SETON                                        73
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  Date must be valid if entered.
     C*
     C     SCDATE        IFNE      0
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*** BEG ADD *** 12/09/98 ****************************************
     C                   MOVEL     SRDTO         SCDATE
     C*** END ADD *** 12/09/98 ****************************************
     C     SRERR         CABEQ     '1'           STRT                     30
     C                   END
     C*
     C*  Key must be valid if entered.
     C*
     C     SCDKEY        IFNE      *BLANKS
     C     CMKYA2        CHAIN(N)  PCM0A                              35
     C     *IN35         CABEQ     '1'           STRT
     C                   END
     C*
     C*  If the date, user or key were changed, redisplay the screen
     C*    using the new values.
     C*
     C     SCDATE        IFNE      XHDATE
     C     SCDKEY        ORNE      XHDKEY
     C     SCUSER        ORNE      XHUSER
     C*
     C                   MOVE      '1'           *IN04                             CLEAR SUBFILE
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C  N70CMKEY4        SETLL     PCM01                              60           Reset file pointe
     C   70CMKYG         SETLL     PCM0G                              60
     C                   EXSR      LOAD
     C*
     C                   Z-ADD     SCDATE        XHDATE
     C                   MOVEL     SCDKEY        XHDKEY
     C                   MOVEL     SCUSER        XHUSER
     C                   GOTO      STRT
     C                   END
     C*
     C*  SAVE RRN SO THAT ROLLING CAN PROPERLY FUNCTION.
     C*
     C                   MOVE      RRN2          SRRN2
     C                   READC     SCR01S                                 50
     C   50              GOTO      TERM
     C     *IN50         DOWEQ     '0'
     C*****
     C*  PROCESS FOR 2=EDIT DOCUMENT
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '2'
     C     SCSEL         IFEQ      '2 '
     C     SCSEL         OREQ      ' 2'
     C*** END   CHG *** 12/28/99 **************************************
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
     C*  If the data entry option exists (SDRPF/DRFL03 = 'Y') and the
     C*  DARTS record indicates a data entry pgm exists (CMFL01 = 'Y'),
     C*  process process the data entry pgm.
     C*
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     *IN71         ANDEQ     '1'
     C     CMFL01        ANDEQ     'Y'
     C*
     C                   MOVE      '0'           *IN72
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      *OFF          ADDINF            1
     C     ##DEIF        IFEQ      'Y'
     C     AIKY          CHAIN     PAI01                              99
     C                   ELSE
     C                   SETON                                        99
     C                   ENDIF
     C     *IN99         IFEQ      *OFF
     C     AIDPGM        ANDNE     *BLANKS
     C                   MOVE      *ON           ADDINF
     C                   MOVEL     AIDPGM        ##PGM
     C                   MOVEL(P)  AIDPGM        XXPART            5
     C*
     C     XXPART        IFEQ      'W0050'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   ENDIF                                                  XXPART=W0050
     C*
     C     ##PGM         IFEQ      'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C                   ENDIF
     C                   ELSE
     C*** END   ADD *** 06/14/04 **************************************
     C*** BEG ADD *** 10/20/98 *****************************
     C                   MOVEL     CMDCID        ##PGM
     C***  BEGIN ADD  ***  08/16/99  *********************************
     C                   MOVEL(P)  CMDCID        XXPART            5
     C*
     C     XXPART        IFEQ      'W0050'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   MOVEL(P)  'W0050C01'    CMDCID
     C                   ENDIF                                                  XXPART=W0050
     C***  END   ADD  ***  08/16/99  *********************************
     C*
     C     ##PGM         IFEQ      'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C                   ENDIF
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C*** END ADD *** 10/20/98 *****************************
     C***********          MOVE *BLANKS   ##PGM            DEL 10/20/98
     C                   CALL      'SRCHKPGM'
     C*** BEG CHG *** 10/20/98 *****************************
     C***********          PARM CMDCID    ##PGM  10
     C                   PARM                    ##PGM            10
     C*** END CHG *** 10/20/98 *****************************
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN81
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ADDINF        IFEQ      *OFF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ELSE
     C                   MOVEL     AIDPGM        DEPGM
     C                   MOVEL     AIDKEY        DEKEY
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
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
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C*
     C     DERTCD        CABEQ     'A'           STRT
     C     DERTCD        CABEQ     'B'           STRT
     C*
     C     CMKEYA        CHAIN     PCM0A                              41
     C     *IN41         IFEQ      '0'
     C                   MOVE      '1'           *IN72
     C                   MOVE      *BLANKS       CMFOLD
     C                   MOVEL     DEKEY         CMFOLD
     C                   MOVE      *BLANKS       CMCOMM
     C                   MOVEL     DECOM1        CMCOMM
     C                   MOVE      DECOM2        CMCOMM
     C                   MOVEL     'Y'           CMFL04
     C                   MOVEL     SCUSER        CMUSER
     C                   UPDATE    PCM0A
     C                   END
     C*
     C                   GOTO      NXT1
     C                   END                                                    *IN41 = '0'
     C*
     C** If not a data entry pgm, process AS/400 Office document if
     C**  Office is installed.
     C*
     C                   MOVE      '0'           *IN72
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEA     '000'         *IN(20)
     C                   MOVEA     '0000'        *IN(23)
     C*
     C     SLKEY         CHAIN     PSL01                              40
     C     *IN40         IFEQ      '0'
     C     SLEDC         ANDNE     'Y'
     C     SCIMGF        ANDNE     'Y'                                          not if image
     C                   MOVE      '1'           *IN20                          NOT ALLOWED TO EDIT
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
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
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     IEX02T        TAG
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    END SEL EQ '2'
     C*****
     C*  PROCESS FOR 3=DIARY ANOTHER USER
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '3'
     C     SCSEL         IFEQ      '3 '
     C     SCSEL         OREQ      ' 3'
     C*** END   CHG *** 12/28/99 **************************************
     C*
     C     SCCDAT        IFNE      *ZEROS                                       ADD 10/06/98
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
     C*
     C                   MOVEL     SCKEY         #3KEY            40
     C                   MOVE      SCSEQ#        #3SEQ#            5
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC016R'
     C                   PARM                    #3KEY
     C                   PARM                    #3SEQ#
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      '0'           *IN72
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     CMFL04        ANDEQ     'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*** ADD 2/11/98 **********
     C     IEX03T        TAG
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   ENDIF                                                  ADD 10/06/98
     C                   END                                                    END SEL EQ '3'
     C*****
     C*  PROCESS FOR 4=MARK COMPLETED
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '4'
     C     SCSEL         IFEQ      '4 '
     C     SCSEL         OREQ      ' 4'
     C*** END   CHG *** 12/28/99 **************************************
     C*
     C*** ADD 2/11/98 **************************************
     C*** CALL THE IMAGE EXIT PROGRAM ***
     C     SCIMGF        IFEQ      'Y'
     C*
     C                   CALL      'IEX04R'      IMAGE                  99
     C*
     C     SCIRTN        CABEQ     'BYPASS'      IEX04T
     C*
     C                   ENDIF
     C******* 2/11/98 *******8 END ADD************************
     C*
     C*  Mark diary item 'Complete'.
     C*
     C*** 04/13/04 End   Change ***************************************
      * Release this record before call to DA701R
     C******     CMKEYA    CHAINPCM0A                40
     C*** beg chg ***** 06/21/04 **************************************
     C********** CMKEYA    CHAINPCM0A               N40
     C     CMKEYA        CHAIN     PCM0A                              40
     C*** end chg ***** 06/21/04 **************************************
     C*** 04/13/04 Begin Change ***************************************
     C     *IN40         IFEQ      '0'
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDCDT
     C                   MOVEL     Q1PGM         CMPGM                          MAINT INFO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVE      *BLANK        CMFL04
     C                   MOVEL     SCUSER        CMUSER
     C*
      *** beg add ***** 06/21/04 **********************************
     C                   UPDATE    PCM0A
      *** beg add ***** 06/21/04 **********************************
     C*
     C*  ALL EDITS HAVE PASSED, MOVE VALUES INTO COMMENT STRING TO ADD
     C*     TO ADDITIONAL REMARKS FILE. (SPECIFYING DIARY COMPLETION)
     C*
     C                   MOVEL     CMT(1)        @@CMT
     C                   MOVEL     Q1USER        @@USER
     C                   MOVE      CMCDAT        XXCDAT
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
     C********             MOVE CMSEQ#    ARSEQ#
     C********             MOVE XXNXT#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMCODE    ARCODE
     C********             MOVELCMPNAM    ARPNAM
     C********             TIME           DSDTTM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
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
     C*** 04/13/04 Begin Change ***************************************
     C******               UPDATPCM0A
     C*** deg del **** 06/21/04 ***************************************
      * Re-Chain to this record for update
     C********** CMKEYA    CHAINPCM0A                40
     C********** *IN40     IFEQ '0'
     C**********           UPDATPCM0A
     C**********           END
     C*** end del **** 06/21/04 ***************************************
     C*** 04/13/04 End   Change ***************************************
     C                   END                                                    *IN40 = '0'
     C*
     C     IEX04T        TAG
     C                   MOVE      *BLANKS       SCCO45
     C                   MOVEL     MSG(1)        SCCO45
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   MOVE      '0'           *IN72
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    END SEL EQ '4'
     C*****
     C*  PROCESS 5=DISPLAY DOCUMENTS
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '5'
     C     SCSEL         IFEQ      '5 '
     C     SCSEL         OREQ      ' 5'
     C*** END   CHG *** 12/28/99 **************************************
     C*
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
     C*
     C*  If the data entry option exists (SDRPF/DRFL03 = 'Y') and the
     C*  DARTS record indicates a data entry pgm exists (CMFL01 = 'Y'),
     C*  process process the data entry pgm.
     C*
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     *IN71         ANDEQ     '1'
     C     CMFL01        ANDEQ     'Y'
     C*
     C                   MOVE      '0'           *IN72
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C*  Check to see if a recursive call was made.  If so... error msg
     C*
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   MOVE      *OFF          ADDINF            1
     C     ##DEIF        IFEQ      'Y'
     C     AIKY          CHAIN     PAI01                              99
     C                   ELSE
     C                   SETON                                        99
     C                   ENDIF
     C     *IN99         IFEQ      *OFF
     C     AIDPGM        ANDNE     *BLANKS
     C                   MOVE      *ON           ADDINF
     C                   MOVEL     AIDPGM        ##PGM
     C                   MOVEL(P)  AIDPGM        XXPART            5
     C*
     C     XXPART        IFEQ      'W0050'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   ENDIF                                                  XXPART=W0050
     C*
     C     ##PGM         IFEQ      'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     CMDCID        ##PGM
     C                   MOVEL(P)  CMDCID        XXPART            5
     C*
     C     XXPART        IFEQ      'W0050'
     C                   MOVEL(P)  'W0050R'      ##PGM
     C                   MOVEL(P)  'W0050C01'    CMDCID
     C                   ENDIF                                                  XXPART=W0050
     C*
     C     ##PGM         IFEQ      'F0269C1'
     C                   MOVEL(P)  'F0269R'      ##PGM
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C*** BEGIN CHG *** 06/14/04 **************************************
     C***********          MOVE *BLANKS   ##PGM
     C***********          CALL 'SRCHKPGM'
     C***********          PARM CMDCID    ##PGM  10
     C***********          PARM           ##ERR   1
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGM            10
     C                   PARM                    ##ERR             1
     C*** END   CHG *** 06/14/04 **************************************
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           *IN81
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       DEPGM             8
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ADDINF        IFEQ      *OFF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMDCID        DEPGM
     C                   MOVEL     CMFOLD        DEKEY
     C*** BEGIN ADD *** 06/14/04 **************************************
     C                   ELSE
     C                   MOVEL     AIDPGM        DEPGM
     C                   MOVEL     AIDKEY        DEKEY
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C                   MOVEL     CMCOMM        DECOM1
     C                   MOVE      CMCOMM        DECOM2
     C                   MOVE      '1'           DERTCD                         '1' = display mode
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
     C     DERTCD        CABEQ     'A'           STRT
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    *IN41 = '0'
     C*
     C** If not a data entry pgm, process AS/400 Office document if
     C**  Office is installed.
     C*
     C                   MOVE      '0'           *IN72
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEA     '000'         *IN(20)
     C                   MOVEA     '0000'        *IN(23)
     C*
     C     SLKEY         CHAIN     PSL01                              40
     C     *IN40         IFEQ      '0'
     C     SLDDC         ANDNE     'Y'
     C                   MOVE      '1'           *IN21                          NOT ALLOWED TO DISP
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN40
     C*
     C*  If not an image type.
     C*
     C     SCIMGF        IFNE      'Y'
     C*
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
     C***** 4/17/07 - ADD START ***********************************
     C*
     C     DRFL13        IFNE      *BLANKS
     C     SCIMGF        ANDEQ     'O'
     C                   MOVEL     DRFL13        QQRTCD
     C                   CALL      'SDSPDC2'
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C                   PARM                    QQRTCD
     C                   PARM                    $MFL06
     C*** CHECK FOR ERRORS HERE......
     C***
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C*
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C*
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C*
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C*
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*** BYPASS OTHER ...
     C                   GOTO      SKPDC
     C*
     C                   ENDIF
     C*
     C***** 4/17/07  -  ADD END ************************************
     C                   CALL      'SDSPDOC'
     C                   PARM                    XXDCID           12
     C                   PARM                    XXFOLD
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    SCFL05
     C                   PARM                    QQRTCD
     C*** ADD 7/25/98 *** SYS CTL FLAG FROM DRTS FOR MICROSOFT PRDS
     C                   PARM                    $MFL06
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      SCUNCN        XXUNCN          255
     C                   PARM      SCIFSP        XXFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C****** ADD START 04/17/07 ***************************************
     C     SKPDC         TAG
     C****** END ADD   04/17/07 ****************************************
     C*
     C                   ELSE                                                   If an Image type
     C*
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
     C*** END ADD/CHANGE
     C*
     C     QVRTCD        IFNE      '     0'                                     *** Error ***
     C     QVRTCD        ANDNE     '000000'
     C     QVRTCD        ANDNE     '      '
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C     IEX05T        TAG
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    END SEL EQ '5'
     C*****
     C*  OPTION 6=PRINT DOCUMENT OR IMAGE
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '6'
     C     SCSEL         IFEQ      '6 '
     C     SCSEL         OREQ      ' 6'
     C*** END   CHG *** 12/28/99 **************************************
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
     C     SCIMGF        IFNE      'Y'                                          NOT AN IMAGE
     C*
     C*   If AS/400 Office NOT Installed - Don't allow this function
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDCID        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
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
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN52
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN53
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN59
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE                                                   IS AN IMAGE
     C**** ADD 2/11/98 *** INFINITY IMAGING **********************
     C*
     C* * * ADD 9/28/98 *** USE IDOC(IMAGE DOC ID) FOR CALL
     C                   MOVEL     SCIDOC        XXDCID
     C**** DEL 9/28/98 **  MOVELSCDCID    XXDCID
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
     C     QPRTCD        IFNE      '000000'                                       *** Error ***
     C     QPRTCD        ANDNE     '      '
     C     QPRTCD        ANDNE     '     0'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                      IMGF <> 'Y'
     C     IEX06T        TAG
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                      End Opt 6
     C*****
     C*  OPTION 7=DISPLAY/ADD ADDITIONAL REMARKS
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '7'
     C     SCSEL         IFEQ      '7 '
     C     SCSEL         OREQ      ' 7'
     C*** END   CHG *** 12/28/99 **************************************
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
     C*
     C                   MOVEL     SCKEY         #7KEY            40
     C                   MOVE      SCSEQ#        #7SEQ#            5 0
     C                   MOVEL     SCPNAM        #7PNAM
     C                   CALL      'SC014R'
     C                   PARM                    #7KEY
     C                   PARM                    #7SEQ#
     C                   PARM                    #7PNAM
     C*
     C                   MOVE      '0'           *IN72
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     CMFL04        ANDEQ     'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C     IEX07T        TAG
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    END SEL EQ '7'
     C*****
     C*  PROCESS FOR 8=DISPLAY REMARK DETAIL
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '8'
     C     SCSEL         IFEQ      '8 '
     C     SCSEL         OREQ      ' 8'
     C*** END   CHG *** 12/28/99 **************************************
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
     C                   MOVE      SCSEQ#        #8SEQ#            5 0
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   CALL      'SC012R'
     C                   PARM                    #8KEY
     C                   PARM                    #8SEQ#
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      '0'           *IN72
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     CMFL04        ANDEQ     'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C     IEX08T        TAG
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    END SEL EQ '8'
     C*****
     C*  OPTION 9=APPEND/DISPLAY IMAGE NOTES
     C*****
     C*** BEGIN CHG *** 12/28/99 **************************************
     C********** SCSEL     IFEQ '9'
     C     SCSEL         IFEQ      '9 '
     C     SCSEL         OREQ      ' 9'
     C*** END   CHG *** 12/28/99 **************************************
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
     C     IEX09T        TAG
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN26                          ??? RIGHT IND??
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END                                                    END SEL EQ '9'
     C*****
     C*  OPTION 10=SEND DOCUMENT TO OTHER USER(S)
     C*****
     C*
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
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C*
     C*  IF ENTERED, DOC ID MUST BE VALID (EXISTING)
     C*
     C                   MOVEL     CMDCID        XXDCID
     C                   MOVEL     CMFOLD        XXFOLD
     C                   MOVEL     *BLANK        QQRTCD
     C*
     C     CMDCID        IFNE      *BLANK
     C*
     C*  If AS/400 Office NOT Installed - Don't allow this function
     C*
     C     XXOFIF        IFEQ      'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C*  Must have a Default Folder entered for this function
     C*
     C     XXFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN43
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
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
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN44
     C                   MOVE      '1'           ERR
     C                   MOVE      RRN2          XRRN2
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
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*
     C     IEX10T        TAG
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   END                                                    *IN41 = 0
     C*
     C                   END                                                    SCSEL = '10'
     C*
     C**** BEGIN ADD **** 08/18/98 ************************************
     C*****
     C*  OPTION P=PRINT RMK
     C*****
     C     SCSEL         IFEQ      'P'
     C*
     C*
     C*** BEGIN CHG *** 01/25/00 ***********************************************
     C**********           MOVELQQKEY     QQKEY
     C                   MOVEL     SCKEY         QQKEY
     C*** BEGIN CHG *** 01/25/00 ***********************************************
     C                   MOVE      SCSEQ#        QQSEQ#            5
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQQUE            10
     C                   MOVEL     UPOUTQ        QQQUE
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'SC741C'
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ#
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      NXT1
     C*
     C                   ENDIF                                                  END SEL EQ 'P'
     C**** END   ADD **** 08/18/98 ************************************
     C*
     C*****
     C*  Invalid selection entered.
     C*****
     C                   MOVEA     '000'         *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         ANDNE     '2'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '6'
     C     SCSEL         ANDNE     '7'
     C     SCSEL         ANDNE     '8'
     C     SCSEL         ANDNE     '9'
     C     SCSEL         ANDNE     '10'                                         12/03/92 ADD
     C**** BEGIN ADD **** 08/18/98 ************************************
     C     SCSEL         ANDNE     'P'                                          08/18/98 ADD
     C**** END   ADD **** 08/18/98 ************************************
     C*
     C                   MOVE      '0'           *IN72
     C     CMKEYA        CHAIN(N)  PCM0A                              41
     C     *IN41         IFEQ      '0'
     C     CMFL04        ANDEQ     'Y'
     C                   MOVE      '1'           *IN72
     C                   END
     C*
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END
     C*****
     C     NXT1          TAG
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C*** RESTORE RRN SO THAT ROLLING CAN PROPERLY FUNCTION
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      SRRN2         RRN2
     C*
     C     ERR           IFEQ      '1'
     C                   MOVE      XRRN2         #@$2
     C                   MOVEL     SCKEY         ZZKEY
     C                   MOVEL     SCSEQ#        ZZSEQ#
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZCDAT
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C*** END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVEL     SCUSER        QQUSER
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C*
     C**** BEGIN ADD **** 08/18/98 ************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     'QPRINT'      UPOUTQ
     C                   ENDIF                                                  *IN60 EQ *ON
     C*
     C**** END   ADD **** 08/18/98 ************************************
     C*
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             XRRN2             5 0
     C                   Z-ADD     0             XXRRN2            5 0
     C*
     C                   MOVE      '0'           *IN29
     C*
     C* LOOK AT DARTS SYSTEM CONTROL FILE TO DETERMINE WHAT'S INSTALLED
     C*
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
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      'N'           XXOFIF                         Installed
     C                   END                                                    END 'Y' SYOFIF
     C*
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 04/28/98 **************************************
     C*================================================================
     C* * *   KEDO SUBROUTINE
     C*================================================================
     C     KEDO          BEGSR
     C*
     C*** RELOAD ALL SUBFILE FIELDS DO LOAD THROUGH THE CURRENT RRN
     C                   Z-ADD     XXRRN2        SVRRN             5 0
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   MOVEL     *BLANKS       SCSEL
     C**** BEGIN ADD **** 08/25/98 ************************************
     C                   MOVE      *ZEROS        ZZCDAT
     C**** END   ADD **** 08/25/98 ************************************
     C*
     C*
     C  N70CMKEY4        SETLL     PCM01                              60
     C   70CMKYG         SETLL     PCM0G                              60
     C                   MOVE      '1'           *IN49
     C                   MOVE      'N'           FOUND1
     C                   MOVE      'N'           FOUND2
     C*
     C                   Z-ADD     0             OVRF
     C     NXTONE        TAG
     C     RRN2          IFLT      SVRRN
     C                   EXSR      LOAD
     C                   ADD       1             OVRF              5 0
     C**** BEGIN DEL **** 08/25/98 ************************************
     C********** OVRF      CABGT900       OUT
     C**********           GOTO NXTONE
     C**** END   DEL **** 08/25/98 ************************************
     C                   ELSE
     C                   GOTO      OUT
     C                   END
     C*
     C     OUT           TAG
     C*
     C     FOUND1        IFEQ      'N'
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     RRN2          XXRRN2
     C                   Z-ADD     RRN2          XRRN2
     C                   MOVE      '1'           *IN29
     C                   MOVE      '1'           *IN05
     C                   END
     C*
     C                   MOVE      '0'           *IN49
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*** END   ADD *** 04/28/98 **************************************
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C**** BEGIN ADD **** 08/25/98 ************************************
     C                   MOVE      '0'           *IN29
     C**** END   ADD **** 08/25/98 ************************************
     C                   MOVE      '0'           *IN32
     C*
     C*  Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C     *IN49         IFEQ      '0'
     C                   MOVE      'Y'           PCFL              1
     C                   END
     C*
     C* LOAD DATE IN CASE IT HAS BEEN CHANGED BY THE USER
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     XXDATE        IFEQ      0
     C**** CHANGE NXT LIN FROM 999999 TO 9999999 **** 4/16/99
     C                   Z-ADD     9999999       XXDATE
     C                   END
     C                   MOVE      'N'           XXHIT             1
     C*
     C**** BEGIN CHG **** 08/25/98 ************************************
     C*********** LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 999 LINES
     C                   Z-ADD     1             X                 5 0
     C***********X         DOWLE13
     C     X             DOWLE     999
     C**** END   CHG **** 08/25/98 ************************************
     C     *IN60         ANDEQ     '0'
     C*
     C  N70CMKEY4        READE     PCM01                                  60
     C   70CMKYG         READE     PCM0G                                  60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN29
     C                   END
     C*
     C*  IF REMARK CODE WAS PASSED INTO PROGRAM, ONLY DISPLAY THOSE
     C*      REMARKS THAT HAVE AN EQUIVALENT REMARK CODE
     C*
     C     *IN70         IFEQ      '1'
     C*
     C     CMRMCD        IFEQ      QQRMCD
     C                   EXSR      RMSLCT                                       ADD 11/16/92
     C*
     C*  This code was moved into subroutine RMSLCT (Remark Select)
     C*
     C                   ELSE                                                   *IN70 EQ 0
     C                   MOVE      '1'           *IN05
     C                   MOVE      '1'           *IN29
     C                   END                                                    QQRMCD END
     C*
     C                   ELSE
     C                   EXSR      RMSLCT                                       ADD 11/16/92
     C*
     C*  This code was moved into subroutine RMSLCT (Remark Select)
     C*
     C                   END                                                    *IN70 = 1
     C*
     C                   END                                                    DO 13 END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C     XXHIT         OREQ      'N'
     C     RRN2          ANDEQ     0
     C                   MOVEL     *BLANKS       SCSEL
     C                   MOVE      *BLANKS       SCCDAT
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       SCCO45
     C                   MOVE      *BLANKS       SCKEY
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
     C                   Z-ADD     1             XXRRN2
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C     *IN49         IFEQ      '0'
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     XXRRN2        #@$2
     C                   Z-ADD     XXRRN2        XRRN2
     C                   END
     C*
     C*  IF NO MORE RECORDS TO DISPLAY ON ROLL-UP, DISPLAY EOF MSG
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN29
     C                   MOVE      '1'           *IN05
     C                   END
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   REMARK SELECT - SELECT REMARKS TO BE DISPLAYED             ***
      *====================================================================*
     C     RMSLCT        BEGSR
     C*
     C     SCDKEY        IFEQ      *BLANKS
     C     SCDKEY        OREQ      CMKEY
     C*
     C     CMCDAT        IFLE      XXDATE
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      'Y'           XXHIT
     C                   ADD       1             RRN2
     C*
     C*  Save 1st subfile RRN of the current page
     C*
     C     *IN49         IFEQ      '0'
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   MOVEL     CMKEY         ZZKEY
     C                   MOVEL     CMSEQ#        ZZSEQ#
     C                   Z-ADD     CMCDAT        ZZCDAT
     C                   END
     C                   END
     C*
     C     *IN49         IFEQ      '1'
     C     FOUND1        ANDEQ     'N'
     C     CMCDAT        IFGE      ZZCDAT
     C                   Z-ADD     RRN2          SVRRN
     C                   Z-ADD     RRN2          XXRRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      'Y'           FOUND1
     C                   END
     C                   END
     C*
     C     *IN49         IFEQ      '1'
     C     FOUND1        ANDEQ     'Y'
     C     FOUND2        ANDEQ     'N'
     C     CMCDAT        ANDEQ     ZZCDAT
     C*
     C     CMKEY         IFEQ      ZZKEY
     C     CMSEQ#        ANDGE     ZZSEQ#
     C                   Z-ADD     RRN2          SVRRN
     C                   Z-ADD     RRN2          XXRRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      'Y'           FOUND2
     C                   ELSE
     C     CMKEY         IFGT      ZZKEY
     C                   Z-ADD     RRN2          SVRRN
     C                   Z-ADD     RRN2          XXRRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      'Y'           FOUND2
     C                   END                                                    CMKEY>ZZKEY
     C                   END                                                    CMKEY=ZZKEY
     C                   END                                                    *IN49
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
     C                   MOVEL     CMRMCD        SCCODE
     C                   MOVEL     CMDCID        SCDCID
     C*** BEGIN ADD *** 06/14/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     SCDCID        ANDEQ     *BLANKS
     C     AIKY          CHAIN     PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIDPGM        SCDCID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/14/04 **************************************
     C     CMFL04        IFEQ      'Y'
     C                   MOVE      '1'           *IN72
     C                   ELSE
     C                   MOVE      '0'           *IN72
     C                   END
     C*
     C     *IN69         IFEQ      '0'
     C     *IN68         ANDEQ     '0'
     C                   MOVEL     *BLANKS       SCCO45
     C                   MOVEL     CMCOMM        SCCO45
     C*** BEG ADD *** 12/23/97 *************************
     C     SCCO45        IFEQ      *BLANKS
     C                   MOVEL     'No Entr'    XXMORE
     C                   MOVE      'y'           XXMORE
     C                   MOVEL     XXMORE        SCCO45
     C                   END
     C*** END ADD *** 12/23/97 *************************
     C*
     C*** IF ADDITIONAL COMMENTS EXIST, THEN SPECIFY "MORE..." FOR COMMENT.
     C*
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     'More...'    XXMORE            9
     C                   MOVE      ''           XXMORE
     C                   MOVE      XXMORE        SCCO45
     C                   END
     C*
     C                   ELSE
     C*
     C*
     C*** BUILD KEY DISPLAY HERE
     C*
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      ' '           X#RTCD
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     *BLANKS       X#CNAM                         ADD 06/24/92
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#CNAM           79            ADD 06/24/92
     C                   PARM                    X#RTCD            1
     C*
     C     *IN68         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCO45
     C     X#CNAM        IFEQ      *BLANKS
     C                   MOVEL     MSG2(1)       SCCO45
     C                   ELSE
     C                   MOVEL     X#CNAM        SCCO45
     C                   END
     C                   ELSE
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     *BLANKS       SCCO45
     C                   MOVEL     X#KEY         SCCO45
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCO45
     C                   MOVEL     CMKEY         SCCO45
     C                   END
     C                   END                                                    *IN68
     C                   END                                                    *IN69
     C*
     C                   MOVEL     CMKEY         SCKEY
     C                   MOVEL     CMSEQ#        SCSEQ#
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
     C***** FILL HIDDEN FIELDS ****
     C*
     C* * * IMAGE FIELDS
     C                   MOVEL     CMIQUE        SCIQUE                             HIDDEN
     C                   MOVEL     CMIPTH        SCIPTH                             HIDDEN
     C                   MOVE      CMPIDX        SCPIDX                             HIDDEN
     C                   MOVEL     CMAPID        SCAPID                             HIDDEN
     C                   MOVEL     CMIDOC        SCIDOC                             HIDDEN
     C                   MOVEL     CMQPTY        SCQPTY                             HIDDEN
     C                   MOVEL     CMDCTP        SCDCTP                             HIDDEN
     C                   MOVEL     CMPNAM        SCPNAM                             HIDDEN
     C                   MOVEL     *BLANKS       SCSEL
     C*
     C                   MOVEL     CMFL05        SCFL05                             HIDDEN
     C*** Begin Add *** 10/17/11 **************************************
     C                   MOVEL     CMUNCN        SCUNCN
     C                   MOVEL     CMIFSP        SCIFSP
     C*** END   Add *** 10/17/11 **************************************
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ADD       1             X
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   MOVE      '1'           *IN29
     C                   END                                                    *IN60 END
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN05
     C                   MOVE      '1'           *IN60
     C                   MOVE      '1'           *IN29
     C                   END                                                    DATE CHK END
     C*
     C                   END                                                    KEY CHK END
     C*
     C                   ENDSR
     C*
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
     C                   MOVE      RRN2          XRRN2
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   END
     C*
     C                   ENDSR
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
      *=========================================================================
      *  SRVDAR - Subroutine to check access to VDARTS and call VDARTS program
      *=========================================================================
     C     SRVDAR        BEGSR
      *
      *** Chg 06/05/03 **************************(***
      *** ADD 05/31/01 **************************(***
      * IF DEFAULT TO VDARTS IS SPECIFIED, BUT THIS IS FROM CLAIMS DIARY SUM
      *  THEN WE DONT WANT TO EXECUTE VDARTS
      *
     C******     SLVDAD    IFEQ 'D'
     C******     *IN70     ANDEQ*ON
     C******               MOVEL'Y'       SLVDAD
     C******               ENDIF
     C*** END ADD 05/31/01 ************************************8
     C*** End Chg 06/05/03 ************************************8
      *
      *** Begin Add *** 09/25/01 ***********************************************
     C                   MOVE      XXRTCD        QQRTCD
      *** End   Add *** 09/25/01 ***********************************************
     C                   SELECT
     C     SLVDAD        WHENEQ    'D'
      ** Call the VDARTS program and goto TERM
     C                   CALL      VBDART        VBDPL
     C                   GOTO      TERM
      *
     C     SLVDAD        WHENEQ    ' '
      ** Deactivate the VDARTS Fkey
     C                   MOVE      ' '           XFVDTA
     C                   MOVE      *OFF          *IN02
      *
     C     SLVDAD        WHENEQ    'Y'
      ** Activate the VDARTS Fkey
     C                   MOVE      'Y'           XFVDTA
     C                   MOVE      *ON           *IN02
      *
     C                   ENDSL                                                  SLVDAD
      *
     C                   ENDSR                                                  SRVDAR
      *** END   ADD *** 11/19/99 ***********************************************
      *====================================================================*
** MSG - SCREEN MESSAGE
*COMPLETED
**  CK1 - Command Keys - SCR01
F3=Exit   F5=Refresh   F6=Display Key Col   F24=More keys
F3=Exit   F5=Refresh   F6=Display Rmk Col   F24=More keys
F3=Exit   F5=Refresh   F6=Display Name Col   F24=More keys
F1=OV/400   F3=Exit   F5=Refresh   F6=Display Key Col    F24=More keys
F1=OV/400   F3=Exit   F5=Refresh   F6=Display Rmk Col    F24=More keys
F1=OV/400   F3=Exit   F5=Refresh   F6=Display Name Col    F24=More keys
F7=Usr Srch   F12=Prev   F19=Prt Diary Rpt   F22=Disp Key   F24=More keys
F7=Usr Srch   F12=Prev   F19=Prt Diary Rpt   F22=Disp Name  F24=More keys
F7=Usr Srch   F12=Prev   F19=Prt Diary Rpt   F24=More keys
F23=Visual DARTS   F24=More keys
** MSG2 - TOP SCREEN MSG
*** NAME FUNCTION NOT AVAILABLE ***
** ADDITIONAL COMMENT TABLE
DIARY COMPLETED BY:            DIARY DT:   /  /   DIARY COMPLETION:  /  /
