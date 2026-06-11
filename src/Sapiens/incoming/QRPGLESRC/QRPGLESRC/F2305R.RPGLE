     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
    H**********************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
     H*******************************************************************
     H/TITLE F2305R - WORK WITH CPT4 CODES
     H*****************************************************************
     H*  REVISIONS:
     H*                                                                 *
     H*  07/09/19 C:079 Project: MC433                                  *
     H*                 Call new CompSuite program                      *
     H*                                                                 *
     H*  10/20/15 C:212 Project: ME009                                  *
     H*                 ADD ME TO CALL OF F2306R                        *
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Disable function code F17 and option 5 for GS
     H*                                                                 *
     H*  04/21/10 C:198 Project: TN014                                  *
     H*                 ADD TN TO CALL OF F2306R                        *
     H*                                                                 *
     H*  02/09/09 C:198 Project: TX055                                  *
     H*                 ADD TX TO CALL OF F2306R                        *
     H*                                                                 *
     H*  08/25/08 C:212 PROJECT:OK019                                   *
     H*                ADDED STATE CODE ALSO AS A PARAMETER WHILE       *
     H*                CALLING F2306R.                                  *
     H*                                                                 *
     H*  07/09/08 C:198 PROJECT:MI004                                   *
     H*                ADDED OPTION 7 FOR DMEPOS MODIFIER AMOUNTS FOR   *
     H*                STATE MI.                                        *
     H*                                                                 *
     H*  06/20/08 C:198 PROJECT:VT007                                   *
     H*                ADDED OPTION 7 FOR DMEPOS MODIFIER AMOUNTS       *
     H*                                                                 *
     H*  09/11/00 C:62 PROJECT:MC205                                    *
     H*                ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.   *
     H*                                                                 *
     H*  05/25/00 C:62 PROJECT:MC205                                    *
     H*                ADD F21=PRINT LIST                               *
     H*
     H*  09/24/98 C:79 PROJ:AL034
     H*                Add option 2=Copy which rolls over CPT-4 data to
     H*                a new effective date.
     H*****************************************************************
     FF2305F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMPRL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPR01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMPRL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPR01:DBRCDF2)
     FSTABL     IF   E           K DISK
     F*** BEG ADD *** 06/20/08 ****************************************
     F*** BEG CHG *** 07/09/08 ****************************************
     F****FMDML   IF  E           K        DISK
     FFMDML     IF   E           K DISK    USROPN
     F*** END CHG *** 07/09/08 ****************************************
     F*** END ADD *** 06/20/08 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
      *** BEG CHG *** 06/20/08 ****************************************
      *** BEG CHG *** 09/24/98 ****************************************
     D******************* OF      1  11  1   OFD    35   Options, Function
     D******************* OF      1  13  1   OFD    35   Options, Function
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
      *** BEG CHG *** 09/24/98 ****************************************
      *** END CHG *** 06/20/08 ****************************************
     D*** BEG ADD *** 07/09/08 ****************************************
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)              CMD EXEC QCMDEXC
     D*** END ADD *** 07/09/08 ****************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  XXEFDT                 1     10
     D  XXDAY                  1      2
     D  XXSL1                  3      3
     D  XXMNTH                 4      5
     D  XXSL2                  6      6
     D  XXYEAR                 7     10
     D                 DS
     D  X1EFDT                 1      8  0
     D  X1DAY                  1      2  0
     D  X1MNTH                 3      4  0
     D  X1YEAR                 5      8  0
     D***BEG ADD *** 07/09/08 ***************************************
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D***END ADD *** 07/09/08 ***************************************
    D*** BEG ADD *** 10/27/99 ****************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Proc Codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2305R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     d*** Beg Add *** 07/09/19 ****************************************
     d*===============================================================*
     d* Standalone fields
     d*===============================================================*
     d OutMode         s              1
     d OutRrn          s              9s 0
     d OutState        s              2
     d isMC433         s               N   INZ('0')
     d*===============================================================*
     d* prototype definitions
     d*===============================================================*
     d ProcCodes       pr                  ExtPgm('MTPROCCODE')
     d inMode                         1    const
     d inRrn                          9  0 const
     d inState                        2    const
     D*** End Add *** 07/09/19 *******************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@CPT4           15
     C                   PARM                    @@DESC           40
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  CONVERTS DATE FROM MDYY TO CYMD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** BEG ADD ***** 06/20/08 ********************************
     C*
     C     DMKY          PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CO#
     C                   PARM                    ##CPT4            5
     C                   PARM                    ##EFFD            7 0
     C*
     C*** BEG ADD ***** 08/25/08 ********************************
     C     DMKY2         PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CO#
     C                   PARM                    ##CPT4            5
     C                   PARM                    ##EFFD            7 0
     C                   PARM                    @@ST
     C*** END ADD ***** 08/25/08 ********************************
     C*** END ADD ***** 06/20/08 ********************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDSC1
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*** BEG ADD ***** 06/20/08 ********************************
     C*
     C     DMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##CPT4
     C                   KFLD                    ##EFFD
     C*
     C*** END ADD ***** 06/20/08 ********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    PREFDT        S1EFDT
     C     *LIKE         DEFINE    PRDSC1        KKDSC1
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    PRDSC1        U1PDSC
     C     *LIKE         DEFINE    PRDSC1        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        ORNE      *BLANKS
     C     SSTOCD        ORNE      *BLANKS
     C*** END   ADD *** 05/25/00 **************************************
B01  C     SSEFDT        ORNE      *ZEROS
B01  C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        8890
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        IFGT      SSTOCD
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/00 **************************************
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*================================================================
     C* ERRCKP - Error checking for the prompt screen
     C*================================================================
     C     ERRCKP        BEGSR
     C*
     C     SPFRCD        IFGT      SPTOCD
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SPEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SPEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/11/00 **************************************
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCPT4
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKDSC1
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKCPT4
     C*
     C   21SCPSTD        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCPSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   ENDIF
     C*
     C   22              MOVEL     SCPST2        KKDSC1
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL     SCCPT4        KKCPT4
     C   21              MOVE      SCEFDT        KKEFDT
     C   22              MOVEL     SCDSC1        KKDSC1
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/25/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   MOVEL(P)  *BLANKS       SPFRCD
     C                   MOVEL(P)  *BLANKS       SPTOCD
     C                   Z-ADD     *ZEROS        SPEFDT
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   EXSR      ERRCKP
     C*
     C     *IN90         IFEQ      *OFF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   MOVE      ##CO#         ##CO
     C*** BEGIN CHG *** 09/11/00 **************************************
     C**********           MOVE SSEFDT    ##EFDT
     C                   MOVE      SPEFDT        ##EFDT
     C*** END   CHG *** 09/11/00 **************************************
     C*
     C                   CALL      'F7030C'
     C                   PARM                    ##CO              3
     C                   PARM      SCST          ##ST              2
     C*** BEGIN CHG *** 09/11/00 **************************************
     C**********           PARM SSFRCD    ##FROM  5
     C**********           PARM SSTOCD    ##TO    5
     C                   PARM      SPFRCD        ##FROM            5
     C                   PARM      SPTOCD        ##TO              5
     C*** END   CHG *** 09/11/00 **************************************
     C                   PARM                    ##EFDT            8
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C                   PARM      ' '           ##RTCD            1
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 05/25/00 **************************************
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL     STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     PRCPT4        SCCPT4
     C                   MOVE      PREFDT        SCEFDT
     C                   MOVEL     PRDSC1        SCDSC1
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT
     C*
     C                   MOVE      X1DAY         XXDAY
     C                   MOVE      '/'           XXSL1
     C                   MOVE      X1MNTH        XXMNTH
     C                   MOVE      '/'           XXSL2
     C                   MOVE      X1YEAR        XXYEAR
     C*
     C                   MOVEL     PRDSC1        XXDSC1           47
     C*
     C                   MOVEL(P)  PRCPT4        XXC4DS           17
     C                   MOVE      XXEFDT        XXC4DS
     C*
     C   21              MOVEL(P)  XXC4DS        SCDATA
     C   21              MOVE      XXDSC1        SCDATA
     C*
     C   22              MOVEL(P)  XXDSC1        SCDATA
     C   22              MOVE      XXC4DS        SCDATA
     C*
     C                   MOVEL     PRDSC1        SCDSC1
     C                   MOVEL     PRSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*** BEG ADD ******* 06/20/08 ***********************************
     C*================================================================
     C* OPT07 - SUBROUTINE FOR OPTION 07
     C*================================================================
     C*
     C* OPTION 7 = DMEPOS Modifier Amounts
     C*
     C     OPT07         BEGSR
     C*
     C                   MOVEL     SCCPT4        ##CPT4
     C*
     C                   MOVEL     SCEFDT        ##EFFD
     C*
     C*** BEG ADD ******* 07/09/08 ***********************************
     C*** OVERRIDE TO FMDMPXX FILE
     C                   EXSR      OVRIDE
     C                   OPEN      FMDML
     C*** END ADD ******* 07/09/08 ***********************************
     C     DMKY1         CHAIN     PDM01                              99
     C     *IN99         IFEQ      *ON
     C                   EXFMT     SCR04
     C                   ELSE
     C*** BEG ADD ******* 07/09/08 ***********************************
     C                   SELECT
     C     @@ST          WHENEQ    'VT'
     C*** END ADD ******* 07/09/08 ***********************************
     C*** BEG CHG ******* 08/25/08 ***********************************
     C*******              CALL 'F2306R'  DMKY
     C                   CALL      'F2306R'      DMKY2
     C*** END CHG ******* 08/25/08 ***********************************
     C*** BEG ADD ******* 07/09/08 ***********************************
     C     @@ST          WHENEQ    'MI'
     C                   CALL      'F2307R'      DMKY
     C*** BEG ADD ******* 08/25/08 ***********************************
     C     @@ST          WHENEQ    'OK'
     C*** BEG ADD ******* 02/09/09 ***********************************
     C     @@ST          OREQ      'TX'
     C*** END ADD ******* 02/09/09 ***********************************
     C*** BEG ADD ******* 04/21/10 ***********************************
     C     @@ST          OREQ      'TN'
     C*** END ADD ******* 04/21/10 ***********************************
     C*** BEG ADD ******* 10/20/15 ***********************************
     C     @@ST          OREQ      'ME'
     C*** END ADD ******* 10/20/15 ***********************************
     C                   CALL      'F2306R'      DMKY2
     C*** END ADD ******* 08/25/08 ***********************************
     C                   ENDSL
     C*** END ADD ******* 07/09/08 ***********************************
     C                   ENDIF
     C*** BEG ADD ******* 07/09/08 ***********************************
     C                   EXSR      DLOVRD
     C                   CLOSE     FMDML
     C*** END ADD ******* 07/09/08 ***********************************
     C                   ENDSR
     C*** END ADD ******* 06/20/08 ***********************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C*** END   ADD *** 05/25/00 **************************************
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C*** BEG ADD *** 06/20/08 **************************************
     C     SCSEL         CASEQ     '7 '          OPT07
     C*** END ADD *** 06/20/08 **************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
       //*** Beg Add *** 07/09/19 *************************************
      /Free
                      If isMC433;
                         OutMode  = @@Call;
                         OutState = @@St;
                         OutRrn   = scDbRr;

                         callp ProcCodes(OutMode:OutRrn:OutState);

                         *inlr = *on;

                         Return;
                      Endif;
      /End-free
       //*** End Add *** 07/09/19 *************************************
     C*
     C                   MOVE      *ON           *IN20
     C*** BEGIN CHG *** 05/25/00 **************************************
     C**********           Z-ADD11        SF1PAG  50
     C                   Z-ADD     10            SF1PAG            5 0
     C*** END   CHG *** 05/25/00 **************************************
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C*** BEG ADD *** 06/20/08 **************************************
     C     'FMDMP'       CAT       @@ST          ##NAME
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    ##NAME           10
     C                   PARM      '*FILE'       ##TYPE            7
     C                   PARM      ' '           ##ERR             1
     C     ##ERR         IFEQ      '1'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '07'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD *** 06/20/08 **************************************
     C*** Beg Add *** MC367 *** 11/15/12 ****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 *****************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCCPT4        @@CPT4
     C                   MOVEL     SCDSC1        @@DESC
     C*** BEG ADD ******* 06/20/08 ***********************************
     C                   MOVEL     SCCPT4        ##CPT4
     C                   MOVEL     SCEFDT        ##EFFD
     C*** END ADD ******* 06/20/08 ***********************************
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      50            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      50            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  PRDSC1        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     PREFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     PRSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        IFNE      *BLANKS
     C     SSFRCD        ANDGT     PRCPT4
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSTOCD        IFNE      *BLANKS
     C     SSTOCD        ANDLT     PRCPT4
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/00 **************************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 07/09/08 ****************************************
     C*================================================================
     C* DLOVRD - Delete file overrides
     C*================================================================
     C     DLOVRD        BEGSR
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*================================================================
     C* OVRIDE - Overide files based  on adjudication state
     C*================================================================
     C     OVRIDE        BEGSR
     C*
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL(P)  @@ST          QQST
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL(P)  @@ST          QQST
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDSR
     C*** END ADD *** 07/09/08 ****************************************
** Column headings
Proc   Eff. Date   Description                                        Sts
Desciption                                       Proc   Eff. Date     Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
12020=Change status         YNN
1077=DMEPOS Mod. Amounts    YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
**  (QCMDEX)
OVRDBF FILE(FMDML) TOFILE(*LIBL/FMDML  )
OVRDBF FILE(FMDMP) TOFILE(*LIBL/FMDMP  )
DLTOVR FILE(FMDML)
DLTOVR FILE(FMDMP)
