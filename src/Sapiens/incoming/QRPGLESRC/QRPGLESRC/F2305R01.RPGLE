     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H*                                                               *
     H* PROGRAM: F2305R01                                             *
     H* DESCRIPTION: CPT-4 CODE MAINTENANCE                           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/09/19 C:079 Project: MC433                                 *
     H*                Compsuite standards                            *
     H*                                                               *
     H* 04/03/19 C:212 Project: NY044                                 *
     H*                Activated indicator 38 for NY.                 *
     H*            079 Activated indicator 51 for NY.                 *
     H*                                                               *
     H* 02/16/18 C:274 Project: DE015                                 *
     H*                Activated indicator 36 for DE.                 *
     H*                                                               *
     H* 08/25/17 C:079 Support: 114/48786                             *
     H*                Save RRN after FEOD                            *
     H*                                                               *
     H* 01/12/17 C:079 Project: FS502                                 *
     H*                Do not lock data area                          *
     H*                                                               *
     H* 12/09/16 C:309 Project: MS032                                 *
     H*                Activate fields 92N4, 1A7 and 5A5 for MS State *
     H*                                                               *
     H* 12/28/15 C:079 Project: FS478                                 *
     H*                Add logic to default effective date to screen  *
     H*                                                               *
     H* 10/20/15 C:212 Project: ME009                                 *
     H*                Added call to F2306R when FMDMPxx exists for ME*
     H*                                                               *
     H* 09/28/15 C:212 Project: SC023                                 *
     H*                Add fields for 92N7 & 92N8 in DSPF and related *
     H*                processing                                     *
     H*                                                               *
     H* 09/09/15 C:309 Project: NC040                                 *
     H*                Activate fields 92N4 & 92N5 for NC State       *
     H*                                                               *
     H* 12/26/14 C:296 Project: FL079                                 *
     H*                1) Activate 92N3 for FL, indicator *in33       *
     H*                and move PR92NJ into PR92N3.                   *
     H*                2) Activate 92N4 for FL, indicator *in39       *
     H*                and move PR92NK into PR92N4.                   *
     H*                                                               *
     H* 06/23/14 C:280 Project: GA034  *in54                          *
     H*                Activate 92N2 for GA
     H*                Activate 92N3 for GA
     H*                Activate 5A4 for GA
     H*                Activate 5A5 for GA
     H*                                                               *
     H* 02/03/14 C:296 Project: MS027                                 *
     H*                WHEN STATE='MS' SET ON IND. 43 TO DISPLAY      *
     H*                ADD-ON FLAG.                                   *
     H*                                                               *
     H* 11/25/13 C:274 Project: CT009                                 *
     H*                Activate Add-on Flag for CT                    *
     H*                                                               *
     H* 09/19/13 C:079 Project: LA023                                 *
     H*                Activate Facility allowance for LA             *
     H*                                                               *
     H* 06/25/12 C:280 Support 80207                                  *
     H*                add code to allow fee schedul flag             *
     H*                if mra amount exists                           *
     H*                                                               *
     H* 04/12/11 C:190 Project KY019                                  *
     H*                Changed edit for KY to allow an R for the      *
     H*                assistant surgeon.                             *
     H*                                                               *
     H* 03/24/10 C:198 surgeon MD067                                  *
     H*                Changed processing for No-cut flag code        *
     H*                                                               *
     H* 09/29/09 C:227 Project: TN013                                 *
     H*                Added call for F2306R for TN state             *
     H*                                                               *
     H* 08/31/09 C:227 Project: CT005                                 *
     H*                Edit changed for SCRPT to allow F and CT state *
     H*                added for call of F2306R.                      *
     H*                                                               *
     H* 07/22/09 C:227 Project: AR038                                 *
     H*                Activated indicators 31,32,33,34,35 and added  *
     H*                EDITAR for AR state.                           *
     H*                                                               *
     H* 06/11/09 C:079 Project: DE005                                 *
     H*                Activate no cut flag                           *
     H*                                                               *
     H* 05/20/09 C:198 Project: MD066                                 *
     H*                Activated indicators 34.                       *
     H*                                                               *
     H* 03/25/09 C:198 Support: 67747                                 *
     H*                Changed the condition to F2305RF1              *
     H*                                                               *
     H* 03/04/09 C:227 Project: DE004                                 *
     H*                CALL F2305RDE WHEN EFF DATE >= 11/14/2008      *
     H*                                                               *
     H* 02/19/09 C:198 Project: FL080                                 *
     H*                CHANGE THE LOGIC WHERE F2305RFL IS BEING CALLED*
     H*                                                               *
     H* 02/09/09 C:079 Project: TX055                                 *
     H*                ADD TX TO CALL OF F2306R                       *
     H*                                                               *
     H* 01/13/09 C:220 Project: CT004                                 *
     H*                CT 2008 FS                                     *
     H*                                                               *
     H* 09/30/08 C:212 PROJECT:KY017                                  *
     H*                1.ALLOW 'R' IN SC1A2 WHEN STATE IS KY.         *
     H*                2.SETON *IN33 FOR KY IN SR-STSCRN.             *
     H*                                                               *
     H* 08/25/08 C:212 PROJECT:OK019                                  *
     H*                ADDED STATE CODE ALSO AS A PARAMETER WHILE     *
     H*                CALLING F2306R.                                *
     H*                                                               *
     H* 08/19/08 C:198 Project: MD065                                 *
     H*                Activated indicators 31, 32, 33, 39,41, 42.   *
     H*                                                               *
     H* 07/09/08 C:198 Project: MI004                                 *
     H*                Activated fields SC92N5 and SC92N6.            *
     H*                                                               *
     H* 06/20/08 C:198 Project: VT007                                 *
     H*                Added call to F2306R when FMDMPxx exists.      *
     H*                                                               *
     H* 06/16/08 C:198 Project: DE002                                 *
     H*                Activated all fields SC92N2 - SC92N5.          *
     H*                                                               *
     H* 04/15/08 C:191 Project: TN008                                 *
     H*                TN 2008 FS changes                             *
     H*                                                               *
     H* 04/29/07 C:79  Project: NC021                                 *
     H*                Activate 92N3 for NC Used allowance            *
     H*                                                               *
     H* 07/22/06 C:79  Project: NY012                                 *
     H*                Load PR1AP for VT into SC1A5                   *
     H*                                                               *
     H* 06/20/06 C:79  Project: TN003                                 *
     H*                Allow APC status indicator C for By Report     *
     H*                Allow H-Hourly for TN anes flag                *
     H*                                                               *
     H* 10/15/05 C:79  Project: FL055                                 *
     H*                Activate FL bilateral procedure field          *
     H*                                                               *
     H* 07/03/05 C:79  Project: TN002                                 *
     H*                Activate TN fields, call new screen            *
     H*                                                               *
     H* 05/18/05 C:79  Support: 46949                                 *
     H*                Allow H=Hourly for NY anes Flag                *
     H*                                                               *
     H* 04/26/05 C:79  Project: MI001                                 *
     H*                MI - activate 92N1,N2,N3,N4                    *
     H*                                                               *
     H* 03/21/05 C:79  Support: 46158                                 *
     H*                KS - activate 92N2                             *
     H*                                                               *
     H* 12/07/04 C:79  Support: 44872                                 *
     H*                NY - add exclude rule count                    *
     H*                                                               *
     H* 10/13/04 C:079 PROJECT:NH001                                  *
     H*                ADD TECH COMP                                  *
     H*                                                               *
     H* 10/12/04 C:079 PROJECT:ME001                                  *
     H*                ADD TECH COMP                                  *
     H*                                                               *
     H* 10/11/04 C:079 PROJECT:CT001                                  *
     H*                ADD TECH COMP                                  *
     H*                                                               *
     H* 09/28/04 C:079 PROJECT:TX026                                  *
     H*                1. ADD PR1A5 FOR ASC PAYMENT GROUP - TEXAS     *
     H*                                                               *
     H* 07/24/04 C:079 PROJECT:MA012                                  *
     H*                1. CHANGE FS REIMB. EDIT                       *
     H*                2. ADD SEP AND NO CUT FOR MA                   *
     H*                                                               *
     H* 07/07/04 C:079 PROJECT: NE010                                 *
     H*                POPULATE NO CUT AND SEP PROC CORRECTLY         *
     H*                                                               *
     H* 06/07/04 C:062 PROJECT:FSMNT                                  *
     H*                ALLOW "M" FOR FL ANES FLAG                     *
     H*                                                               *
     H* 12/12/03 C:079 PROJECT:FL048                                  *
     H*                CALL NEW FLORIDA SCREEN                        *
     H*                                                               *
     H* 11/02/03 C:079 PROJECT:FS265                                  *
     H*                ADD FIELD FOR KY TO HOLD CCI/CR FLAG VALUE     *
     H*                                                               *
     H* 10/30/03 C:079 PROJECT:FL047                                  *
     H*                CALL STATE SCREEN IF PREFDT < 09/30/02         *
     H*                                                               *
     H* 09/26/03 C:079 PROJECT:MS012                                  *
     H*                ACTIVATE HEADING FOR SC1A6 INCLUDE IN CASCADE  *
     H*                                                               *
     H* 08/18/03 C:079 SUPPORT:MC095 38636                            *
     H*                ACTIVATE HEADING FOR GA SC1H4                  *
     H*                                                               *
     H* 07/08/03 C:079 PROJECT:TX020                                  *
     H*                1. ALLOW C, R VALUE FOR BY REPORT IN TEXAS     *
     H*                2. ALLOW H=HOURLY FOR ANES/MI/RNT/HR FIELD     *
     H*                3. Only Call F2305RTX if PREFDT>=8/1/03        *
     H*                2. ALLOW M=Mileage FOR ANES/MI/RNT/HR FIELD    *
     H*                                                               *
     H* 07/01/03 C:079 PROJECT:MA012                                  *
     H*                1. ACTIVATE SC92N2,SC92N3, SC92N6              *
     H*                2. ADD FIELD FOR PSYCH AMOUNT                  *
     H*                3. ADD FIELD FOR REH/THER/AUDIO                *
     H*                4. ADD FIELD FOR CHIRO                         *
     H*                                                               *
     H* 05/20/03 C:079 PROJECT:FS257                                  *
     H*                1. ON INITIAL ADD DEFAULT CERTAIN FIELDS       *
     H*                                                               *
     H* 02/22/03 C:079 PROJECT:SC007                                  *
     H*                1. ACTIVATE SEPARATE PROC FOR SC               *
     H*                                                               *
     H* 12/12/02 C:079 PROJECT:FS254                                  *
     H*                ENABLE CCI EDIT, BILAT PROC, ASC GROUP FOR UC  *
     H*                                                               *
     H* 10/25/02 C:079 PROJECT:MS011                                  *
     H*                1.  ENABLE SC92N2 FOR USED AMOUNT MISSISSIPPI  *
     H*                                                               *
     H* 10/10/02 C:079 SUPPORT:MC044 34328                            *
     H*                1.  ADD FLORIDA FIELD TO EXCLUDE FROM COUNTING *
     H*                                                               *
     H* 08/27/02 C:079 PROJECT:CA001                                  *
     H*                1.  ADD SEP PROC,TECH COMP, NO CUT PROC        *
     H*                    STATE RULE USE FIELDS                      *
     H*                                                               *
     H* 08/15/02 C:079 PROJECT:NY006                                  *
     H*                1.  ADD CALL TO NEW YORK ONLY PROGRAM.         *
     H*                2.  REMOVE NY ERROR INDICATOR PROGRAM.         *
     H*                3.  NY ALLOW 'X' FOR BY REPORT FLAG            *
     H*                4.  ALLOW PRMRA = 0  BY REPORT FLAG            *
     H*                                                               *
     H* 07/30/02 C:079 PROJECT:TX014                                  *
     H*                ADD CALL TO TEXAS ONLY PROGRAM.                *
     H*                                                               *
     H* 07/30/02 C:079 PROJECT:FL042                                  *
     H*                Move sep proc into SC1A4 for Florida           *
     H*                                                               *
     H* 07/30/02 C:079 PROJECT:FL042                                  *
     H*                Move sep proc into SC1A4 for Florida           *
     H*                                                               *
     H* 07/03/02 C:079 PROJECT:NC014                                  *
     H*                Use PR1AD for Sep Proc for North Carolina      *
     H*                                                               *
     H* 05/03/02 C:079 SUPPORT:MC071 32338                            *
     H*                Allow 'R' in SC1A2 anes flag                   *
     H*                                                               *
     H* 04/08/02 C:079 PROJECT:FSMNT                                  *
     H*                Set on indicator to display sep proc and no    *
     H*                cut for UC.                                    *
     H*                                                               *
     H* 04/03/02 C:079 PROJECT:PA014                                  *
     H*                Set on indicator to display sep proc field PA  *
     H*                                                               *
     H* 02/01/02 C:079 PROJECT:KY013                                  *
     H*                Set on indicator to display no cut field KY    *
     H*                Set on indicator to display sep proc field KY  *
     H*                                                               *
     H* 12/06/01 C:079 PROJECT: NC013                                 *
     H*                Add no cut,sep proc,MRA in facility for NC     *
     H*                                                               *
     H* 12/06/01 C:079 PROJECT: FL039                                 *
     H*                Add 'D' as a valid by report value for FL      *
     H*                to indicate modifiers TC and 26 not covered    *
     H*                                                               *
     H* 09/25/01 C:079 PROJECT: VT001                                 *
     H*                Add PR92N1 as tech comp and PR1A3 as No cut for*
     H*                Vermont.                                       *
     H*                Add SC1A5 as follow up day flag (S,I,N)        *
     H*                                                               *
     H* 08/31/01 C:079 SUPPORT: MC114 28202                           *
     H*                Allow 'U' as a valid value for anes flag PR1A2 *
     H*                for all states.  Change PR1A3 to be no cut PA  *
     H*                                                               *
     H* 08/30/01 C:079 PROJECT: WV003                                 *
     H*                Activate PR92N2 to hold MRA for procedures     *
     H*                performed in facilities for WV                 *
     H*                                                               *
     H* 08/29/01 C:079 PROJECT: AL044                                 *
     H*                Add Used amount field for Alabama              *
     H*                                                               *
     H* 07/19/01 C:079 SUPPORT: MC091 28197                           *
     H*                Add No cut field SC1A3 to screen for GA        *
     H*                                                               *
     H* 07/11/01 C:079 SUPPORT: MC114 28190                           *
     H*                Add 'M' as a valid anes flag value for PA      *
     H*                                                               *
     H* 06/19/01 C:079 PROJECT: 114MCG                                *
     H*                Activate SC92N4 for Pennsylvania locality 2    *
     H*                prof comp                                      *
     H*                                                               *
     H* 04/25/01 C:062 PROJECT: MC044 27208                           *
     H*                ACTIVATE SC1A3 FOR FL.                         *
     H*                                                               *
     H* 04/23/01 C:062 PROJECT: LA017                                 *
     H*                ACTIVATE SC1A5 FOR LA.                         *
     H*                                                               *
     H* 02/09/01 C:079 Support: MC044 25693                           *
     H*                1. Allow 'T' for anes flag for Florida         *
     H*                                                               *
     H* 01/17/01 C:079 Proj. PA008                                    *
     H*                1. Allow 'R' for anes flag                     *
     H*                                                               *
     H* 01/17/01 C:079 Proj. PA008                                    *
     H*                1. Allow 'R' for anes flag                     *
     H*                                                               *
     H* 12/08/00 C:079 Proj. PA008                                    *
     H*                1. Activate SC1A3 as no cut flag field         *
     H*                2. Remove edit on SCMRA, SCPROF, SCANAM        *
     H*                                                               *
     H* 11/01/00 C:079 Proj. TXMNT 24805                              *
     H*                Added 'T' to valid values for anes flag        *
     H*                                                               *
     H* 10/13/00 C:079 Proj. OK011                                    *
     H*                Added edit to EDITOK to allow M in anes flag   *
     H*                                                               *
     H* 09/05/00 C:117 Proj. FL033
     H*                Added a field to the detail screen
     H*
     H*  08/08/00 C:79 PROJECT:OK010                                  *
     H*                1. Add Used amount to hold HCPCS value         *
     H*                                                               *
     H*  05/10/00 C:79 PROJECT:TX009                                  *
     H*                1. Add MRI slices amount fields                *
     H*                2. Add Nocut and Sep proc flags                *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H*  04/08/00 C:79 Support: MC095/22094                           *
     H*                1) Add MRI/CAT flag field                      *
     H*                                                               *
     H*  03/30/00 C:79 SUPPORT: MC044/22246                           *
     H*                1) Move ASC and Tech components to correct     *
     H*                   database fields if state=FL                 *
     H*                                                               *
     H*  03/09/00 C:79  Project: NC009                                *
     H*                 1. Allow M for Mileage to process Chiro codes *
     H*                                                               *
     H*  02/16/00 C:79 PROJ:NE007                                     *
     H*                1) Add Bundled code field for Nebraska         *
     H*                2) Add Separate and No cut fields              *
     H*                                                               *
     H*  01/04/00 C:79 PROJ:MS008                                     *
     H*                1) Allow M for mileage in Anes flag            *
     H*                                                               *
     H*  12/28/99 C:79 PROJ:65MAM                                     *
     H*                1) Add Technical component field               *
     H*                2) Add Sep Proc field                          *
     H*                3) Add no-cut field                            *
     H*                                                               *
     H*  10/25/99 C:79 PROJ:KS012                                     *
     H*                1) Correct edit on Assisting surgeon           *
     H*                2) Add Sep Proc field                          *
     H*                3) Add no-cut field                            *
     H*                                                               *
     H*  09/30/99 C:79 PROJ:SC006                                     *
     H*                1) Allow 'R' for rental in the anesthesia      *
     H*                   flag for South Carolin                      *
     H*                                                               *
     H*  09/23/99 C:79 Proj: SC006                                    *
     H*                1) Add separate procedure field for South      *
     H*                   Carolina                                    *
     H*                                                               *
     H*  08/25/99 C:79 Proj: AL036                                    *
     H*                1) Add Nocut and separate procedure fields for *
     H*                   Alabama                                     *
     H*                                                               *
     H*  04/29/99 C:79 Support:17377                                  *
     H*                1) Allow By-Report flag to = "Y" when MRA = 0  *
     H*                   for North Carolina                          *
     H*                                                               *
     H*  12/16/98 C:79 PROJ:MS005                                     *
     H*                1) Allow 'R' for rental in the anesthesia      *
     H*                   flag for Mississippi                        *
     H*                                                               *
     H*  09/24/98 C:79 PROJ:AL034                                     *
     H*                Add logic to process option 3=Copy to a new    *
     H*                effective date.                                *
     H*                                                               *
     H*  09/02/98 C:79   PROJ:AL033                                   *
     H*                  ADD "M" TO VALID VALUES FOR ANES. FLAG.      *
     H*                                                               *
     H*  07/31/98 C:110 PROJ: OH010.SP1                               *
     H*                When State='OH' display ASC Level Field        *
     H*                and Technical Component Field.                 *
     H*
     H*  07/13/98 C:79 SUPP: MC095 SEQ:12396                          *
     H*                When State='MS' set on ind. 38 to display      *
     H*                No-cut flag.
     H*                                                               *
     H*  07/01/98 C:79 PROJ: RI006                                    *
     H*                When State='RI' set on ind. 31 to display      *
     H*                Technical Component field.                     *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*  *IN31-Used to activate SC92N1,SC92H1                         *
     H*  *IN32-Used to activate SC92N2,SC92H2                         *
     H*  *IN33-Used to activate SC92N3,SC92H3                         *
     H*  *IN34-Used to activate SC1A3,SC1H3                           *
     H*  *IN35-Used to activate SC1A4,SC1H4                           *
     H*  *IN36-Used to activate SC1A5,SC1H5                           *
     H*  *IN37-Used to activate SC1A1,SC1A2                           *
     H*  *IN38-Used to activate SC1A6,SC1H6                           *
     H*  *IN39-Used to activate SC92N4,SC92H4                         *
     H*  *IN41-Used to activate SC92N5,SC92H5                         *
     H*  *IN42-Used to activate SC92N6,SC92H6                         *
     H*  *IN43-Used to activate SC1A7,SC1H7                           *
     H*  *IN51-Used to activate SC1AF,SC1HF                           *
     H*****************************************************************
     FF2305F01  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFMPRP     UF A E             DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPR01:DBRCDF)
     FFMPRL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     FFDCFL     IF   E           K DISK
     F*** BEG ADD *** 11/02/03 ****************************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END ADD *** 11/02/03 ****************************************
     F*** BEG ADD *** 06/20/08 ****************************************
     F*** BEG CHG *** 07/09/08 ****************************************
     F******FMDML   IF  E           K        DISK
     FFMDML     IF   E           K DISK    USROPN
     F*** END CHG *** 07/09/08 ****************************************
     F*** END ADD *** 06/20/08 ****************************************
      /COPY SCOPYMT01D
     d*** Beg Add *** 12/18/17 *****************************************
     d  inUCdate       s              8    dtaara
     d*** End Add *** 12/18/17 *****************************************
     D*** BEG ADD *** 07/09/08 ****************************************
     D QCA             S             56    DIM(3) CTDATA PERRCD(1)              CMD EXEC QCMDEXC
     D*** END ADD *** 07/09/08 ****************************************
     D*
     D***PY SCOPYMT01I
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***BEG ADD *** 07/09/08 ***************************************
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D***END ADD *** 07/09/08 ***************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'Proc Code Mai-       C         OPTNM
     D**********    'ntenance'
     D OPTNM           C                   CONST('Proc Code Inf-
     D                                     ormation')
     D*** END   CHG *** 05/05/00 **************************************
     D COMAPI          C                   CONST('FA007R')
     D*** Beg Add *** 12/28/15 *******************************************
     D inEfDt          s              8
     D*** End Add *** 12/28/15 *******************************************
     D*** Beg Add *** 07/09/19 *******************************************
     D outPrDs         s             40    Inz('Code Information')
     D outState        s              2
     D outMode         s              1
     D outCall         s              1
     D isMC433         s               N   INZ('0')
     d*===============================================================*
     d* prototype definitions
     d*===============================================================*
     d CodeInfo        pr
     d inPrDs                        40    const
     d inRrn                          9  0 const
     d inMode                         1    const
     d inState                        2    const
     d inCall                         1    const
     D*** End Add *** 07/09/19 *******************************************

     I*             'FSMESG'              C         SLMSGF

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*** BEG ADD *** 06/20/08 ****************************************
     C     DMKY          PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    KKCO#
     C                   PARM                    ##CPT4            5
     C                   PARM                    ##EFDT            7 0
     C*** BEG ADD *** 08/25/08 ****************************************
     C     DMKY2         PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    KKCO#
     C                   PARM                    ##CPT4            5
     C                   PARM                    ##EFDT            7 0
     C                   PARM                    @@ST
     C*** END ADD *** 08/25/08 ****************************************
     C*** END ADD *** 06/20/08 ****************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C     CFKY          KLIST
     C                   KFLD                    KKSTA             2
     C*** BEG ADD *** 11/02/03 ****************************************
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END ADD *** 11/02/03 ****************************************
     C*** BEG ADD *** 06/20/08 ****************************************
     C     DMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##CPT4
     C                   KFLD                    ##EFDT
     C*** END ADD *** 06/20/08 ****************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PRCO#         ##CO#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    STKEY         KKST
     C*** BEG ADD *** 11/02/03 ****************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTCO#         KKCO#
     C*** END ADD *** 11/02/03 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     CALLR         BEGSR
     C*
     C     SCRN#         IFEQ      #OFSCR
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   MOVE      DBRRN         ##RRN
     C                   MOVE      'C'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVE      @@RRN         ##RRN
     C                   MOVE      'D'           ##MODE
     C*
     C                   ENDIF
     C*** Beg Add *** 08/25/17 *********************************
     C                   FEOD      FMPRP
     C*** End Add *** 08/25/17 *********************************
     C*
     C*** BEG CHG *** 10/08/99 ****************************************
     C********             SELEC
     C********   @@ST      WHEQ 'FL'
     C     @@ST          IFEQ      'FL'
     C     PRFSR         ANDEQ     'Y'
     C*** BEG ADD *** 10/30/03 ****************************************
     C     PREFDT        ANDLT     1020930
     C*** END ADD *** 10/30/03 ****************************************
     C                   CALL      'F2305RFL'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*** BEG ADD *** 12/12/03 ****************************************
     C     @@ST          IFEQ      'FL'
     C     PRFSR         ANDEQ     'Y'
     C     PREFDT        ANDGE     1040101
     C*** BEG DEL *** 03/25/09 ****************************************
     C********   PRCPT4    ANDGE'10000'
     C*** BEG ADD *** 02/19/09 ****************************************
     C********   SC1A7     OREQ 'Y'
     C*** END ADD *** 02/19/09 ****************************************
     C*** END DEL *** 03/25/09 ****************************************
     C                   CALL      'F2305RF1'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*** END ADD *** 12/12/03 ****************************************
     C*
     C********   @@ST      WHEQ 'PA'
     C     @@ST          IFEQ      'PA'
     C                   CALL      'F2305RPA'    ACDPL
     C                   EXSR      CALLRT
     C********             ENDSL
     C                   ENDIF
     C*** BEG ADD *** 07/03/05 ****************************************
     C     @@ST          IFEQ      'TN'
     C     PRFSR         ANDEQ     'Y'
     C*** BEG ADD ****** 04/15/08 ************************************
     C     PREFDT        IFGE      1080304
     C                   MOVEL     'F2305RTN'    XXPGM            10
     C                   MOVE      '01'          XXPGM
     C                   CALL      XXPGM         ACDPL
     C                   EXSR      CALLRT
     C                   ELSE
     C*** END ADD ****** 04/15/08 ************************************
     C                   CALL      'F2305RTN'    ACDPL
     C                   EXSR      CALLRT
     C*** BEG ADD ****** 04/15/08 ************************************
     C                   ENDIF                                                  SCEFDT
     C*** END ADD ****** 04/15/08 ************************************
     C                   ENDIF
     C*** END ADD *** 07/03/05 ****************************************
     C*** BEGIN ADD *** 07/30/02 **************************************
     C     @@ST          IFEQ      'TX'
     C     PRFSR         ANDEQ     'Y'
     C*** BEG ADD *** 07/08/03 ****************************************
     C     PREFDT        ANDGE     1030801
     C*** END ADD *** 07/08/03 ****************************************
     C                   CALL      'F2305RTX'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*** END   ADD *** 07/30/02 **************************************
     C*** BEG ADD *** 03/04/09 ****************************************
     C     @@ST          IFEQ      'DE'
     C     PRFSR         ANDEQ     'Y'
     C     PREFDT        ANDGE     1081114
     C                   CALL      'F2305RDE'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*** END ADD *** 03/04/09 ****************************************
     C*** BEGIN ADD *** 08/15/02 **************************************
     C     @@ST          IFEQ      'NY'
     C     PRFSR         ANDEQ     'Y'
     C                   CALL      'F2305RNY'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*** END   ADD *** 08/15/02 **************************************
     C*** BEG CHG *** 10/08/99 ****************************************
     C*
     C*****                MOVE *ON       *IN90
     C*
     C*** BEG ADD *** 06/20/08 **************************************
     C     'FMDMP'       CAT       @@ST          ##NAME
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    ##NAME           10
     C                   PARM      '*FILE'       ##TYPE            7
     C                   PARM      ' '           ##ERR             1
     C     ##ERR         IFNE      '1'
     C*** BEG CHG ******* 07/09/08 ***********************************
     C********   @@RRN     CHAINDBRCDF               99
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*** END CHG ******* 07/09/08 ***********************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     PRCPT4        ##CPT4
     C                   MOVEL     PREFDT        ##EFDT
     C*** BEG ADD ******* 07/09/08 ***********************************
     C*** OVERRIDE TO FMDMPXX FILE
     C                   EXSR      OVRIDE
     C                   OPEN      FMDML
     C*
     C*** END ADD ******* 07/09/08 ***********************************
     C     DMKY1         CHAIN     PDM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEG ADD ******* 07/09/08 ***********************************
     C                   SELECT
     C     @@ST          WHENEQ    'VT'
     C*
     C*** END ADD ******* 07/09/08 ***********************************
     C*** BEG CHG ******* 08/25/08 ************************************
     C******               CALL 'F2306R'  DMKY
     C                   CALL      'F2306R'      DMKY2
     C*** END CHG ******* 08/25/08 ************************************
     C*** BEG CHG ******* 07/09/08 ***********************************
     C                   EXSR      CALLRT
     C     @@ST          WHENEQ    'MI'
     C                   CALL      'F2307R'      DMKY
     C                   EXSR      CALLRT
     C*** BEG ADD ******* 08/25/08 ***********************************
     C     @@ST          WHENEQ    'OK'
     C*** BEG ADD ******* 02/09/09 ***********************************
     C     @@ST          OREQ      'TX'
     C*** END ADD ******* 02/09/09 ***********************************
     C*** BEG ADD ******* 08/31/09 ***********************************
     C     @@ST          OREQ      'CT'
     C*** END ADD ******* 08/31/09 ***********************************
     C*** BEG ADD ******* 09/29/09 ***********************************
     C     @@ST          OREQ      'TN'
     C*** END ADD ******* 09/29/09 ***********************************
     C*** BEG ADD ******* 10/20/15 ***********************************
     C     @@ST          OREQ      'ME'
     C*** END ADD ******* 10/20/15 ***********************************
     C*
     C                   CALL      'F2306R'      DMKY2
     C                   EXSR      CALLRT
     C*** END ADD ******* 08/25/08 ************************************
     C                   ENDSL
     C*
     C**********           EXSR CALLRT
     C*** END CHG ******* 07/09/08 ***********************************
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*** BEG ADD ******* 07/09/08 ***********************************
     C                   EXSR      DLOVRD
     C                   CLOSE     FMDML
     C     @@ST          IFEQ      'MI'
     C                   CALL      'F2305RMI'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*** END ADD ******* 07/09/08 ***********************************
     C*
     C                   ENDIF                                                  FMPRPxx
     C*
     C                   ENDIF                                                  ##ERR = ' '
     C*** END   ADD *** 06/20/08 **************************************
     C*** BEG ADD *** 08/19/08 **************************************
     C*
     C     @@ST          IFEQ      'MD'
     C                   CALL      'F2305RMD'    ACDPL
     C                   EXSR      CALLRT
     C                   ENDIF
     C*
     C*** END ADD *** 08/19/08 **************************************
     C                   ENDIF
     C*
     C                   ENDSR                                                  CALLR
     C*================================================================
     C* CALLRT - RETURN FROM STATE SPECIFIC SCREEN PROGRAMS
     C*================================================================
     C     CALLRT        BEGSR
     C*
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     PRSTAT        IFEQ      'I'
     C                   MOVE      'A'           PRSTAT
     C                   ELSE
     C                   MOVE      'I'           PRSTAT
     C                   ENDIF                                                  PRSTAT
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVEL     DSTIME        PRTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* CPT4HD - Obtains variable headers by state.
     C*================================================================
     C     CPT4HD        BEGSR
     C*
     C                   MOVEL     @@ST          KKSTA
     C*
     C     CFKY          CHAIN     PCF01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL     CFHMRA        SCHMRA
     C                   MOVEL     CF92H1        SC92H1
     C                   MOVEL     CF92H2        SC92H2
     C                   MOVEL     CF92H3        SC92H3
     C                   MOVEL     CF92H4        SC92H4
     C                   MOVEL     CF92H5        SC92H5
     C*** BEG ADD *** 07/01/03 ****************************************
     C                   MOVEL     CF92H6        SC92H6
     C*** END ADD *** 07/01/03 ****************************************
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   MOVEL     CF92H7        SC92H7
     C                   MOVEL     CF92H8        SC92H8
     C*** END ADD *** 09/28/15 ****************************************
     C                   MOVEL     CF1H3         SC1H3
     C                   MOVEL     CF1H4         SC1H4
     C                   MOVEL     CF1H5         SC1H5
     C                   MOVEL     CF1H6         SC1H6
     C*** BEG ADD *** 07/31/98 ****************************************
     C                   MOVEL     CF1HF         SC1HF
     C*** END ADD *** 07/31/98 ****************************************
     C*** BEG ADD *** 02/19/09 ****************************************
     C                   MOVEL     CF1H7         SC1H7
     C*** END ADD *** 02/19/09 ****************************************
     C*
     C*** BEG ADD *** 06/23/14 ****************************************
     C*
     C                   MOVEL     CF5H4         SC5H4
     C                   MOVEL     CF5H5         SC5H5
     C*
     C*** END ADD *** 06/23/14 ****************************************
     C*
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SC92H1
     C                   MOVE      *BLANKS       SC92H2
     C                   MOVE      *BLANKS       SC92H3
     C                   MOVE      *BLANKS       SC92H4
     C                   MOVE      *BLANKS       SC92H5
     C*** BEG ADD *** 07/01/03 ****************************************
     C                   MOVE      *BLANKS       SC92H6
     C*** END ADD *** 07/01/03 ****************************************
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   MOVE      *BLANKS       SC92H7
     C                   MOVE      *BLANKS       SC92H8
     C*** END ADD *** 09/28/15 ****************************************
     C                   MOVE      *BLANKS       SC1H3
     C                   MOVE      *BLANKS       SC1H4
     C                   MOVE      *BLANKS       SC1H5
     C                   MOVE      *BLANKS       SC1H6
     C*** BEG ADD *** 07/31/98 ****************************************
     C                   MOVE      *BLANKS       SC1HF
     C*** END ADD *** 07/31/98 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C*
     C                   Z-ADD     1             SCRN#
     C*
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
     C*
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* EDITAL - State specific edits for Alabama
     C*================================================================
     C     EDITAL        BEGSR
     C*** BEG ADD *** 08/25/99 ****************************************
     C                   SETOFF                                       7879
     C*** END ADD *** 08/25/99 ****************************************
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C*** BEG ADD *** 06/25/12 ****************************************
     C*
     C     SC92N2        ANDLE     *ZERO
     C*
     C*** END ADD *** 06/25/12 ****************************************
     C*
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S OR X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A, B OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     'T'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS T AMOUNT REQUIRED
     C*
     C  N90SC1A1         IFEQ      'T'
     C     SCPROF        ANDEQ     *ZEROS
     C*
     C                   SETON                                        768590
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B, OR H
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'H'
     C*** BEG ADD *** 09/02/98 ***************************************
     C     SC1A2         ANDNE     'M'
     C*** END ADD *** 09/02/98 ***************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*** BEG ADD *** 05/03/02 ***************************************
     C     SC1A2         ANDNE     'R'
     C*** END ADD *** 05/03/02 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*** BEG ADD *** 08/25/99 ****************************************
     C*
     C*  THE NO CUT FIELD MUST BE Y OR BLANK.
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     *BLANK
     C*
     C                   SETON                                        78  90
     C*
     C                   ENDIF
     C*
     C*  THE SEPARATE PROCEDURE FIELD MUST BE Y, N, OR BLANK.
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     'N'
     C     SC1A5         ANDNE     *BLANK
     C*
     C                   SETON                                        79  90
     C*
     C                   ENDIF
     C*** END ADD *** 08/25/99 ****************************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 08/27/02 ****************************************
     C*================================================================
     C*
     C*================================================================
     C     EDITCA        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y,N,A, or X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'X'
     C     SCRPT         OREQ      'S'
     C*
     C                   SETON                                        728590
     C*
     C                   ENDIF
     C*
     C*  IF BY REPORT FIELD IS AN Y,X,S or A MRA MUST BE 0.
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCRPT         OREQ      'A'
     C     SCRPT         OREQ      'X'
     C     SCRPT         OREQ      'S'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        738590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE COUNT
     C*
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANKS
     C*
     C                   SETON                                        748590
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE CASCADING
     C*
     C  N90SC1A6         IFNE      'Y'
     C     SC1A6         ANDNE     *BLANKS
     C*
     C                   SETON                                        788590
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 08/27/02 ****************************************
     C*** BEG ADD *** 07/22/09 ****************************************
     C     EDITAR        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C                   SETON                                        768190
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y,N,A, or X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'X'
     C     SCRPT         OREQ      'S'
     C                   SETON                                        728590
     C                   ENDIF
     C*
     C*  IF BY REPORT FIELD IS AN Y,X,S or A MRA MUST BE 0.
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCRPT         OREQ      'A'
     C     SCRPT         OREQ      'X'
     C     SCRPT         OREQ      'S'
     C     SCMRA         IFGT      *ZEROS
     C                   SETON                                        738590
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 07/22/09 ****************************************
     C*** BEG ADD *** 10/11/04 ****************************************
     C*================================================================
     C* CONNECTICUT
     C*================================================================
     C     EDITCT        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y,N,A, or X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'X'
     C*** BEG ADD *** 08/31/09 ****************************************
     C     SCRPT         ANDNE     'F'
     C*** END ADD *** 08/31/09 ****************************************
     C     SCRPT         OREQ      'S'
     C*
     C                   SETON                                        728590
     C*
     C                   ENDIF
     C*
     C*  IF BY REPORT FIELD IS AN Y,X,S or A MRA MUST BE 0.
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCRPT         OREQ      'A'
     C     SCRPT         OREQ      'X'
     C     SCRPT         OREQ      'S'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        738590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE COUNT
     C*
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANKS
     C*
     C                   SETON                                        748590
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE CASCADING
     C*
     C  N90SC1A6         IFNE      'Y'
     C     SC1A6         ANDNE     *BLANKS
     C*
     C                   SETON                                        788590
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/19/07 ****************************************
     C  N90SC1A7         IFNE      'Y'
     C     SC1A7         ANDNE     *BLANKS
     C*
     C                   SETON                                        5890
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 02/19/07 ****************************************
     C                   ENDSR
     C*** END ADD *** 10/11/04 ****************************************
     C*
     C*================================================================
     C* EDITGA - State specific edits for Georgia
     C*================================================================
     C     EDITGA        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S OR X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A, B OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, or H
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'H'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITFL - State specific edits for Florida
     C*================================================================
     C     EDITFL        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR M OR S OR X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*** BEG ADD *** 12/06/01 ****************************************
     C     SCRPT         ANDNE     'D'                                          NC MOD 26/TC
     C*** END ADD *** 12/06/01 ****************************************
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A, B OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, or T.
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD 02/09/01 ********************************************
     C     SC1A2         ANDNE     'T'
     C*** END ADD 02/09/01 ********************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*** BEG ADD *** 06/07/04 ***************************************
     C     SC1A2         ANDNE     'M'
     C*** END ADD *** 06/07/04 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*** BEG ADD *** 10/10/02 ****************************************
     C*
     C*  THE EXCLUDE IN RULE COUNT FIELD MUST BE Y OR BLANK
     C*
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANK
     C*
     C                   SETON                                        798190
     C*
     C                   ENDIF
     C*** END ADD *** 10/10/02 ****************************************
     C*** BEG ADD *** 02/19/09 ***************************************
     C  N90SC1A7         IFNE      ' '
     C     SC1A7         ANDNE     'Y'
     C*
     C                   SETON                                        58  90
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 02/19/09 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* EDITKS - State specific edits for Kansas
     C*================================================================
     C     EDITKS        BEGSR
     C*
     C* THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C* THE BY REPORT FIELD MUST BE Y OR N
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C*
     C                   SETON                                        738790
     C*
     C                   ENDIF
     C*
     C* OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C* THE MRA, PROF, TECH OR ANE/RNT MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        718890
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        728890
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* THE * PROCEDURE MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C* THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C*** BEG ADD 10/25/99 ********************************************
     C******************** SETON                     748190
     C                   SETON                                        748190
     C*** END ADD 10/25/99 ********************************************
     C*
     C                   ENDIF
     C*
     C* THE ANE/MI/HR/RNT FLAG Blank, Y, B, or H.
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD 10/25/99 ********************************************
     C     SC1A2         ANDNE     'H'
     C*** END ADD 10/25/99 ********************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'B'
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG ADD *** 10/25/99 ****************************************
     C*
     C*  THE SEPARATE PROCEDURE FIELD MUST BE Y OR BLANK.
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     *BLANK
     C*
     C                   SETON                                        708490
     C*
     C                   ENDIF
     C*
     C*  THE NO-CUT FLAG FIELD MUST BE Y OR BLANK.
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANK
     C*
     C                   SETON                                        797890
     C*
     C                   ENDIF
     C*** END ADD *** 10/25/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* EDITKY - State specific edits for Kentucky
     C*================================================================
     C     EDITKY        BEGSR
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B, Y, N, P OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     'A'
     C     SC1A1         ANDNE     'P'
     C     SC1A1         ANDNE     'Y'
     C     SC1A1         ANDNE     'N'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS P AMOUNT REQUIRED
     C*
     C  N90SC1A1         IFEQ      'P'
     C     SCPROF        ANDEQ     *ZEROS
     C*
     C                   SETON                                        768590
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y,A OR B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'A'
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*** BEG ADD *** 09/30/08 ***************************************
     C     SC1A2         ANDNE     'R'
     C*** END ADD *** 09/30/08 ***************************************
     C*
     C                   SETON                                        728790
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS M ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'M'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS H ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'H'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        718690
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*** BEG ADD *** 11/02/03 ****************************************
     C*
     C*  THE CCI/CR FLAG MUST BE VALID OR BLANK
     C*
     C  N90SC1A5         IFNE      *BLANK
     C*
     C                   MOVEL(P)  'BPCC'        KKTBID
     C                   MOVEL(P)  SC1A5         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        898190
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 11/02/03 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* EDITLA - State specific edits for Louisiana
     C*================================================================
     C     EDITLA        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR B ORM OR X OR Z.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'B'
     C     SCRPT         ANDNE     'M'
     C     SCRPT         ANDNE     'X'
     C     SCRPT         ANDNE     'Z'
     C*
     C                   SETON                                        748390
     C*
     C                   ENDIF
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C     SC92N1        ANDLE     *ZEROS
     C     SC92N2        ANDLE     *ZEROS
     C*
     C                   SETON                                        758590
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778390
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'B'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778390
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PURCHASE INFO FLAG MUST BE BLANK,R,B,I,A
     C*
     C  N90SC1A4         IFNE      *BLANKS
     C     SC1A4         ANDNE     'R'
     C     SC1A4         ANDNE     'B'
     C     SC1A4         ANDNE     'I'
     C     SC1A4         ANDNE     'A'
     C*
     C                   SETON                                        728290
     C*
     C                   ENDIF
     C*
     C* IF PURCHASE INFO FLAG IS BY REPORT THE BY REPORT FLAG MUST BE Y
     C*
     C  N90SC1A4         IFEQ      'B'
     C     SCRPT         IFNE      'Y'
     C*
     C                   SETON                                        738290
     C*
     C                   ENDIF
     C*
     C  N90SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        718490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF PURCHASE INFO FLAG IS AN INV. CONSIDERATION NO MRA ALLOWED
     C*
     C  N90SC1A4         IFEQ      'I'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        718490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF PURCHASE INFO FLAG IS A RENTAL INFO NO MRA ALLOWED
     C*
     C  N90SC1A4         IFEQ      'R'
     C*
     C  N90SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        718490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE * PROCEDURE MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, M, H, R, I
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C     SC1A2         ANDNE     'R'
     C     SC1A2         ANDNE     'I'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        768390
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS M ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'M'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS H ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'H'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        718690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS R ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'R'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'B'
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS I ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'I'
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        758390
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS B NO AMOUNT ALLOWED
     C*
     C  N90SC1A1         IFEQ      'B'
     C*
     C     SCPROF        IFNE      *ZEROS
     C*
     C                   SETON                                        778490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE TECH COMP FLAG FIELD MUST BE B OR BLANK
     C*
     C  N90SC1A3         IFNE      'B'
     C     SC1A3         ANDNE     *BLANKS
     C*
     C                   SETON                                        748490
     C*
     C                   ENDIF
     C*
     C*  IF TECH COMP FLAG IS B NO TC AMOUNT ALLOWED
     C*
     C  N90SC1A3         IFEQ      'B'
     C*
     C     SC92N1        IFNE      *ZEROS
     C*
     C                   SETON                                        758490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITMA - State specific edits for Massachusetts
     C*================================================================
     C     EDITMA        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR I
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'I'
     C     SCRPT         ANDNE     'A'
     C*
     C                   SETON                                        758690
     C*
     C                   ENDIF
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  ONE OF THE AMOUNT FIELDS MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C     SC92N1        ANDLE     *ZEROS
     C*** BEG ADD *** 07/24/04 ****************************************
     C     SC92N2        ANDLE     *ZEROS
     C     SC92N3        ANDLE     *ZEROS
     C     SC92N4        ANDLE     *ZEROS
     C     SC92N5        ANDLE     *ZEROS
     C     SC92N6        ANDLE     *ZEROS
     C*** END ADD *** 07/24/04 ****************************************
     C*
     C                   SETON                                        768690
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778690
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'I'
     C*
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE * PROCEDURE MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, M, H, R, I
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C     SC1A2         ANDNE     'R'
     C     SC1A2         ANDNE     'I'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        768390
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS M ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'M'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS H ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'H'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        718690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS R ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'R'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'B'
     C*
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS I ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'I'
     C*
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS B NO AMOUNT ALLOWED
     C*
     C  N90SC1A1         IFEQ      'B'
     C     SCPROF        IFNE      *ZEROS
     C*
     C                   SETON                                        778490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITMD - State specific edits for Maryland
     C*================================================================
     C     EDITMD        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S OR X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A, B OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEG ADD *** 10/12/04 ****************************************
     C*================================================================
     C* MAINE
     C*================================================================
     C     EDITME        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y,N,A, or X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'X'
     C     SCRPT         OREQ      'S'
     C*
     C                   SETON                                        728590
     C*
     C                   ENDIF
     C*
     C*  IF BY REPORT FIELD IS AN Y,X,S or A MRA MUST BE 0.
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCRPT         OREQ      'A'
     C     SCRPT         OREQ      'X'
     C     SCRPT         OREQ      'S'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        738590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE COUNT
     C*
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANKS
     C*
     C                   SETON                                        748590
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE CASCADING
     C*
     C  N90SC1A6         IFNE      'Y'
     C     SC1A6         ANDNE     *BLANKS
     C*
     C                   SETON                                        788590
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 10/12/04 ****************************************
     C*** BEG ADD *** 04/26/05 ****************************************
     C*================================================================
     C* MICHIGAN
     C*================================================================
     C     EDITMI        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y,N
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C*
     C                   SETON                                        728590
     C*
     C                   ENDIF
     C*
     C*  IF BY REPORT FIELD IS AN Y,X,S or A MRA MUST BE 0.
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        738590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE COUNT
     C*
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANKS
     C*
     C                   SETON                                        748590
     C*
     C                   ENDIF
     C*
     C*  EDIT INCLUDE IN RULE CASCADING
     C*
     C  N90SC1A6         IFNE      'Y'
     C     SC1A6         ANDNE     *BLANKS
     C*
     C                   SETON                                        788590
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 04/26/05 ****************************************
     C*================================================================
     C* EDITMS - State specific edits for Mississippi
     C*================================================================
     C     EDITMS        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF,ANAM OR FACILITY FEE MUST BE GT *ZERO IF
     C*  FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C     SC92N3        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S OR X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A, B OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, M, or R.
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 01/04/00 ****************************************
     C     SC1A2         ANDNE     'M'
     C*** END ADD *** 01/04/00 ****************************************
     C*** BEG ADD *** 12/16/98 ****************************************
     C     SC1A2         ANDNE     'R'
     C*** END ADD *** 12/16/98 ****************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*** BEG ADD *** 04/08/00 ****************************************
     C*
     C*  THE MRI/CAT FIELD MUST BE Y OR BLANK
     C*
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     *BLANKS
     C*
     C                   SETON                                        788190
     C*
     C                   ENDIF
     C*** END ADD *** 04/08/00 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* EDITNC - State specific edits for North Carolina
     C*================================================================
     C     EDITNC        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*** BEG DEL *** 04/29/99 ****************************************
     C**********
     C***THE*MRA MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C**********
     C**N90***** SCFSR     IFEQ 'Y'
     C********** SCMRA     ANDLE*ZERO
     C**********
     C**********           SETON                     778890
     C**********
     C**********           ENDIF
     C*** END DEL *** 04/29/99 ****************************************
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C*
     C                   SETON                                        718990
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE BLANK
     C*
     C  N90SC1A1         IFNE      *BLANKS
     C*
     C                   SETON                                        728990
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 03/09/00 ****************************************
     C     SC1A2         ANDNE     'M'
     C*** END ADD *** 03/09/00 ****************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITNE - State specific edits for Nebraska
     C*================================================================
     C     EDITNE        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE (*) PROCEDURE FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE ASSISTING SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*** BEG ADD *** 02/16/00 ****************************************
     C*
     C*  THE BUNDLED CODE FIELD MUST BE M=Manipulation,
     C*  H=Work Hardening, P=Performance Testing
     C*
     C  N90SC1A3         IFNE      'M'
     C     SC1A3         ANDNE     'H'
     C     SC1A3         ANDNE     'P'
     C     SC1A3         ANDNE     *BLANKS
     C*
     C                   SETON                                        708190
     C*
     C                   ENDIF
     C*
     C*  The separate procedure field must be 'Y' or blank
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANKS
     C*
     C                   SETON                                        787990
     C*
     C                   ENDIF
     C*
     C*  The no cut procedure field must be 'Y' or blank
     C  N90SC1A6         IFNE      'Y'
     C     SC1A6         ANDNE     *BLANKS
     C*
     C                   SETON                                        698190
     C*
     C                   ENDIF
     C*** END ADD *** 02/16/00 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* EDITNH - State specific edits for New Hampshire
     C*================================================================
     C     EDITNH        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA AMOUNT ANESTHESIA FLAG IS REQUIRED
     C*
     C  N90SCANAM        IFGT      *ZEROS
     C     SC1A2         IFNE      'Y'
     C*
     C                   SETON                                        748690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITNM - State specific edits for New Mexico
     C*================================================================
     C     EDITNM        BEGSR
     C*
     C* THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C* PURCHASE INFO FLAG MUST BE BLANK A
     C*
     C  N90SC1A4         IFNE      *BLANKS
     C     SC1A4         ANDNE     'A'
     C*
     C                   SETON                                        778790
     C*
     C                   ENDIF
     C*
     C* THE BY REPORT FIELD MUST BE Y OR N
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C*
     C                   SETON                                        738790
     C*
     C                   ENDIF
     C*
     C* OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C* THE MRA, PROF, TECH OR ANE/RNT MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        718890
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        728890
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* THE * PROCEDURE MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C* THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C* THE ANE/MI/HR/RNT FLAG Blank, Y, B, R
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        738890
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'B'
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITNY - State specific edits for New York
     C*================================================================
     C     EDITNY        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*** BEG DEL *** 08/15/02 ****************************************
     C**********
     C**IF*MRA*AMOUNT ENTERED NO FAMILY, GENERAL, CONS. AMOUNTS ALLOWED
     C**********
     C**N90***** SCMRA     IFGT *ZEROS
     C********** SC92N1    IFNE *ZEROS
     C********** SC92N2    ANDNE*ZEROS
     C********** SC92N3    ANDNE*ZEROS
     C**********
     C**********           SETON                     718590
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C*** END DEL *** 08/15/02 ****************************************
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A X OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'S'
     C*** BEG ADD *** 08/20/02 ****************************************
     C     SCRPT         ANDNE     'X'
     C*** END ADD *** 08/20/02 ****************************************
     C*
     C                   SETON                                        728590
     C*
     C                   ENDIF
     C*
     C*  IF BY REPORT FIELD IS AN Y OR A OR S NO MRA ALLOWED
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCRPT         OREQ      'A'
     C     SCRPT         OREQ      'S'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        738590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C*** BEG DEL *** 08/15/02 ****************************************
     C******************** SETON                     758190
     C*** END DEL *** 08/15/02 ****************************************
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C*** BEG DEL *** 08/15/02 ****************************************
     C******************** SETON                     748190
     C*** END DEL *** 08/15/02 ****************************************
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B OR N
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'N'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*** BEG ADD *** 12/07/04 ****************************************
     C     SC1A2         ANDNE     'T'
     C*** END ADD *** 12/07/04 ****************************************
     C*** BEG ADD *** 05/18/05 ****************************************
     C     SC1A2         ANDNE     'H'
     C*** END ADD *** 05/18/05 ****************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANES AMOUNT IS NOT ALLOWED
     C*
     C     SC1A2         IFEQ      'B'
     C*
     C  N90SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B, T OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     'T'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        758390
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS T AMOUNT REQUIRED
     C*
     C  N90SC1A1         IFEQ      'T'
     C     SCPROF        ANDEQ     *ZEROS
     C*
     C                   SETON                                        768590
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS B NO AMOUNT ALLOWED
     C*
     C     SC1A1         IFEQ      'B'
     C*
     C  N90SCPROF        IFNE      *ZEROS
     C*
     C                   SETON                                        778490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG DEL *** 08/15/02 ****************************************
     C**********
     C**IF*FAMILY PRAC AMOUNT ENTERED NO MRA. GEN, CONS AMOUNTS ALLOWED
     C**********
     C**N90***** SC92N1    IFGT *ZEROS
     C**********
     C********** SCMRA     IFGT *ZEROS
     C********** SC92N2    ORGT *ZEROS
     C********** SC92N3    ORGT *ZEROS
     C**********
     C**********           SETON                     748590
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C**********
     C**IF*GEN*PRAC AMOUNT ENTERED NO MRA, FAM, CONS AMOUNTS ALLOWED
     C**********
     C**N90***** SC92N2    IFGT *ZEROS
     C**********
     C********** SCMRA     IFGT *ZEROS
     C********** SC92N1    ORGT *ZEROS
     C********** SC92N3    ORGT *ZEROS
     C**********
     C**********           SETON                     748590
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C**********
     C**IF*CON*PRAC AMOUNT ENTERED NO MRA, FAM, GEN AMOUNTS ALLOWED
     C**********
     C**N90***** SC92N3    IFGT *ZEROS
     C**********
     C********** SCMRA     IFGT *ZEROS
     C********** SC92N1    ORGT *ZEROS
     C********** SC92N2    ORGT *ZEROS
     C**********
     C**********           SETON                     748590
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C*** END DEL *** 08/15/02 ****************************************
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*** BEG DEL *** 08/15/02 ****************************************
     C********
     C***THE*MRA, PROF, ANAM, TECH AMT
     C***MUST*BE GT *ZERO IF FSREIMBURS = 'Y'
     C********
     C**N90***   SCFSR     IFEQ 'Y'
     C********   SCMRA     ANDLE*ZERO
     C********   SCPROF    ANDLE*ZERO
     C********   SCANAM    ANDLE*ZERO
     C********   SC92N1    ANDLE*ZERO
     C********** SC92N2    ANDLE*ZERO
     C********** SC92N3    ANDLE*ZERO
     C********
     C********             SETON                     728390
     C********
     C********             ENDIF
     C*** END DEL *** 08/15/02 ****************************************
     C*
     C                   ENDIF
     C*
     C* IF BY REPORT FLAG IS EQUAL TO 'Y' OR 'B' MRA AMOUNT MUST BE 0
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCRPT         OREQ      'B'
     C     SCMRA         IFNE      *ZEROS
     C*
     C                   SETON                                        738390
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITOH - State specific edits for Ohio
     C*================================================================
     C     EDITOH        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR I OR X
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'I'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C*
     C                   SETON                                        758690
     C*
     C                   ENDIF
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C  N90SCRPT         IFEQ      'Y'
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778690
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C  N90SCRPT         IFEQ      'I'
     C*
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE * PROCEDURE MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, M, H, R, I, P
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C     SC1A2         ANDNE     'R'
     C     SC1A2         ANDNE     'I'
     C     SC1A2         ANDNE     'P'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        768390
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS M ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'M'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS H ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'H'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        718690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS R ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'R'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'B'
     C*
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS I ANES AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'I'
     C*
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*IF ANESTHESIA FLAG IS P ANES AMOUNT IS ALLOWED
     C*
     C  N90SC1A2         IFEQ      'P'
     C*
     C     SCANAM        IFEQ      *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE  B OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS B NO AMOUNT ALLOWED
     C*
     C  N90SC1A1         IFEQ      'B'
     C     SCPROF        IFNE      *ZEROS
     C*
     C                   SETON                                        778490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SCRPT         IFEQ      'X'
     C*
     C     SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        778690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 07/31/98 ****************************************
     C* VALIDATING ASC LEVEL FIELD
     C*
     C  N90SC1AF         IFLT      '0'
     C     SC1AF         ORGT      '9'
     C*
     C     SC1AF         IFNE      ' '
     C*
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 07/31/98 ****************************************
     C     CKEND         ENDSR
     C*================================================================
     C* EDITOK - State specific edits for Oklahoma
     C*================================================================
     C     EDITOK        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y, B, R, M, H.
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/08/00 ****************************************
     C     SC1A2         ANDNE     'R'
     C*** END ADD *** 08/08/00 ****************************************
     C*** BEG ADD *** 10/13/00 ****************************************
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C*** END ADD *** 10/13/00 ****************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA AMOUNT ANESTHESIA FLAG IS REQUIRED
     C*
     C  N90SCANAM        IFGT      *ZEROS
     C*
     C*** BEG ADD *** 08/08/00 ****************************************
     C********** SC1A2     IFNE 'Y'
     C     SC1A2         IFEQ      ' '
     C*** END ADD *** 08/08/00 ****************************************
     C*
     C                   SETON                                        748690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  TECH COMPONENT FLAG MUST BE BLANK OR B
     C*
     C  N90SC1A3         IFNE      *BLANKS
     C     SC1A3         ANDNE     'B'
     C*
     C                   SETON                                        748490
     C*
     C                   ENDIF
     C*
     C*  IF MOD 27, AND BR = Y THEN TECH COMPONENT = B
     C*
     C  N90SC1A3         IFEQ      'B'
     C     SCRPT         ANDNE     'Y'
     C*
     C                   SETON                                        748490
     C*
     C                   ENDIF
     C*
     C*
     C*  POUND FLAG MUST BE BLANK OR Y
     C*
     C  N90SC1A5         IFNE      *BLANKS
     C     SC1A5         ANDNE     'Y'
     C*
     C                   SETON                                        768890
     C*
     C                   ENDIF
     C*
     C*
     C*  AMPERSAND FLAG MUST BE BLANK OR Y
     C*
     C  N90SC1A6         IFNE      *BLANKS
     C     SC1A6         ANDNE     'Y'
     C*
     C                   SETON                                        758890
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITPA - State specific edits for Pennsylvania
     C*================================================================
     C     EDITPA        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*** BEG DEL *** 12/08/00 ****************************************
     C*********
     C***THE*MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*********
     C**N90****  SCFSR     IFEQ 'Y'
     C*********  SCMRA     ANDLE*ZERO
     C*********  SCPROF    ANDLE*ZERO
     C*********  SCANAM    ANDLE*ZERO
     C*********
     C*********            SETON                     728190
     C*********
     C*********            ENDIF
     C*** END DEL *** 12/08/00 ****************************************
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'I'                                          IC VALUE
     C*
     C                   SETON                                        758690
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     'I'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        718790
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 01/17/01 ****************************************
     C     SC1A2         ANDNE     'R'
     C*** END ADD *** 01/17/01 ****************************************
     C*** BEG ADD *** 07/11/01 ****************************************
     C     SC1A2         ANDNE     'M'
     C*** END ADD *** 07/11/01 ****************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*** BEG ADD *** 12/08/00 ****************************************
     C*  The No cut field must be Blank, Y
     C*
     C  N90SC1A3         IFNE      *BLANKS
     C     SC1A3         ANDNE     'Y'
     C*
     C                   SETON                                        788190
     C*
     C                   ENDIF
     C*** END ADD *** 12/08/00 ****************************************
     C*
     C*  THE (*) PROCEDURE FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE ASSISTING SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITRI - State specific edits for Rhode Island
     C*================================================================
     C     EDITRI        BEGSR
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'I'                                          IC VALUE
     C*
     C                   SETON                                        758690
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     'I'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        718790
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C*  THE (*) PROCEDURE FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE ASSISTING SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITSC - State specific edits for South Carolina
     C*================================================================
     C     EDITSC        BEGSR
     C*
     C*  OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C*  THE MRA, PROF OR ANAM MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C*
     C                   SETON                                        728190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 09/30/99 ****************************************
     C     SC1A2         ANDNE     'R'
     C*** BEG ADD *** 09/30/99 ****************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA AMOUNT ANESTHESIA FLAG IS REQUIRED
     C*
     C     SCANAM        IFGT      *ZEROS
     C*
     C  N90SC1A2         IFNE      'Y'
     C*** BEG ADD *** 09/30/99 ****************************************
     C     SC1A2         ANDNE     'R'
     C*** BEG ADD *** 09/30/99 ****************************************
     C*
     C                   SETON                                        748690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/23/99 ****************************************
     C*
     C*  THE SEPARATE PROCEDURE FIELD MUST BE Y OR BLANK.
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     *BLANK
     C*
     C                   SETON                                          7890
     C*
     C                   ENDIF
     C*** END ADD *** 09/23/99 ****************************************
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITTN - State specific edits for Tennessee
     C*================================================================
     C     EDITTN        BEGSR
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*** BEG ADD *** 06/20/06 ****************************************
     C     SCRPT         ANDNE     'C'
     C*** END ADD *** 06/20/06 ****************************************
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE A OR BLANK
     C*
     C  N90SC1A1         IFNE      'A'
     C     SC1A1         ANDNE     'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y or B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*** BEG ADD *** 06/20/06 ****************************************
     C     SC1A2         ANDNE     'H'
     C*** END ADD *** 06/20/06 ****************************************
     C*
     C                   SETON                                        718290
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA AMOUNT ANESTHESIA FLAG IS REQUIRED
     C*
     C  N90SCANAM        IFGT      *ZEROS
     C*
     C     SC1A2         IFNE      'Y'
     C*** BEG ADD *** 06/20/06 ****************************************
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'U'
     C     SC1A2         ANDNE     'H'
     C*** END ADD *** 06/20/06 ****************************************
     C*
     C                   SETON                                        748690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* EDITTX - State specific edits for Texas
     C*================================================================
     C     EDITTX        BEGSR
     C*
     C* THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C* THE BY REPORT FIELD MUST BE Y OR N
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C*** BEG ADD *** 07/08/03 ****************************************
     C     SCRPT         ANDNE     'C'
     C     SCRPT         ANDNE     'R'
     C*** END ADD *** 07/08/03 ****************************************
     C*
     C                   SETON                                        738790
     C*
     C                   ENDIF
     C*
     C* OTHER FIELDS MUST BE VALID IF NOT BY REPORT
     C*
     C     SCRPT         IFEQ      'N'
     C*
     C* THE MRA, PROF, TECH OR ANE/RNT MUST BE GT *ZERO IF FSREIMBURS = 'Y'
     C*
     C  N90SCFSR         IFEQ      'Y'
     C     SCMRA         ANDLE     *ZERO
     C     SCPROF        ANDLE     *ZERO
     C     SCANAM        ANDLE     *ZERO
     C     SC92N1        ANDLE     *ZEROS
     C     SCCPT4        ANDNE     '99024'
     C*
     C                   SETON                                        748790
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     SCRPT         IFEQ      'Y'
     C*
     C  N90SCMRA         IFGT      *ZEROS
     C*
     C                   SETON                                        758790
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* THE * PROCEDURE MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C* THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*** BEG CHG *** 11/01/00 ****************************************
     C*
     C* THE ANE/MI/HR/RNT FLAG Blank, Y, B, R, P, T, H, M
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'R'
     C     SC1A2         ANDNE     'P'
     C*** BEG ADD *** 11/01/00 ****************************************
     C     SC1A2         ANDNE     'T'
     C*** END ADD *** 11/01/00 ****************************************
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*** BEG ADD *** 07/08/03 ***************************************
     C     SC1A2         ANDNE     'H'
     C     SC1A2         ANDNE     'M'
     C*** END ADD *** 07/08/03 ***************************************
     C*
     C                   SETON                                        768790
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS R ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'R'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728690
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS B ANZS AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'B'
     C*
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        738490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF RENTAL FLAG IS P AMOUNT IS NOT ALLOWED
     C*
     C  N90SC1A2         IFEQ      'P'
     C*
     C     SCANAM        IFNE      *ZEROS
     C*
     C                   SETON                                        748890
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        758390
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS B NO AMOUNT ALLOWED
     C*
     C  N90SC1A1         IFEQ      'B'
     C*
     C     SCPROF        IFNE      *ZEROS
     C*
     C                   SETON                                        778490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG DEL *** 09/01/00 ****************************************
     C**********
     C**********
     C***THE*TECH COMP FLAG FIELD MUST BE B OR BLANK
     C**********
     C**N90***** SC1A3     IFNE 'B'
     C********** SC1A3     ANDNE*BLANKS
     C**********
     C**********           SETON                     748490
     C**********
     C**********           ENDIF
     C**********
     C***IF*TECH COMP FLAG IS B NO TC AMOUNT ALLOWED
     C**********
     C**N90***** SC1A3     IFEQ 'B'
     C**********
     C********** SC92N1    IFNE *ZEROS
     C**********
     C**********           SETON                     758490
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C*** END DEL *** 09/01/00 ****************************************
     C*
     C*** BEG ADD *** 09/01/00 ****************************************
     C* Bundled code flag must be "Y" or blank
     C     SC1A3         IFNE      *BLANKS
     C     SC1A3         ANDNE     'Y'
     C*
     C                   SETON                                        748490
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEG ADD *** 09/25/01 ****************************************
     C*================================================================
     C* EDITVT - State specific edits for Vermont
     C*================================================================
     C     EDITVT        BEGSR
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR X OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B, Y, N, P OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     'A'
     C     SC1A1         ANDNE     'P'
     C     SC1A1         ANDNE     'Y'
     C     SC1A1         ANDNE     'N'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS P AMOUNT REQUIRED
     C*
     C  N90SC1A1         IFEQ      'P'
     C     SCPROF        ANDEQ     *ZEROS
     C*
     C                   SETON                                        768590
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y,A OR B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'A'
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        728790
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS M ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'M'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS H ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'H'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        718690
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE FOLLOW UP DAY FLAG SHOULD BE S,I,N OR BLANK.
     C  N90SC1A5         IFNE      'S'
     C     SC1A5         ANDNE     'I'
     C     SC1A5         ANDNE     'N'
     C     SC1A5         ANDNE     *BLANK
     C*
     C                   SETON                                        748990
     C*
     C                   ENDIF
     C*
     C*  THE NO-CUT FLAG FIELD MUST BE Y OR BLANK.
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     *BLANK
     C*
     C                   SETON                                        788190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 09/25/01 ****************************************
     C*** BEG ADD *** 10/25/99 ****************************************
     C*================================================================
     C* EDITWV - State specific edits for West Virginia
     C*================================================================
     C     EDITWV        BEGSR
     C*
     C*  THE BY REPORT FIELD MUST BE Y OR N OR A OR S.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'                                          ANE VALUE
     C     SCRPT         ANDNE     'X'                                          NC VALUE
     C     SCRPT         ANDNE     'S'
     C*
     C                   SETON                                        738190
     C*
     C                   ENDIF
     C*
     C*  THE PROF COMP FLAG FIELD MUST BE B, Y, N, P OR BLANK
     C*
     C  N90SC1A1         IFNE      'B'
     C     SC1A1         ANDNE     'A'
     C     SC1A1         ANDNE     'P'
     C     SC1A1         ANDNE     'Y'
     C     SC1A1         ANDNE     'N'
     C     SC1A1         ANDNE     *BLANKS
     C*
     C                   SETON                                        778190
     C*
     C                   ENDIF
     C*
     C*  IF PROF COMP FLAG FIELD IS P AMOUNT REQUIRED
     C*
     C  N90SC1A1         IFEQ      'P'
     C     SCPROF        ANDEQ     *ZEROS
     C*
     C                   SETON                                        768590
     C*
     C                   ENDIF
     C*
     C*  The Anesthesia Flag field must be Blank, Y,A OR B
     C*
     C  N90SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'B'
     C     SC1A2         ANDNE     'A'
     C     SC1A2         ANDNE     'M'
     C     SC1A2         ANDNE     'H'
     C*** BEG ADD *** 08/31/01 ***************************************
     C     SC1A2         ANDNE     'U'
     C*** END ADD *** 08/31/01 ***************************************
     C*
     C                   SETON                                        728790
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS Y ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'Y'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        728490
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS M ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'M'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        778590
     C*
     C                   ENDIF
     C*
     C* IF ANESTHESIA FLAG IS H ANES AMOUNT IS REQUIRED
     C*
     C  N90SC1A2         IFEQ      'H'
     C     SCANAM        ANDEQ     *ZEROS
     C*
     C                   SETON                                        718690
     C*
     C                   ENDIF
     C*
     C*  THE SUPPLIES FIELD MUST BE Y OR N.
     C*
     C  N90SCSUPP        IFNE      'N'
     C     SCSUPP        ANDNE     'Y'
     C*
     C                   SETON                                        758190
     C*
     C                   ENDIF
     C*
     C*  THE SURGEON FIELD MUST BE Y OR N.
     C*
     C  N90SCSURG        IFNE      'N'
     C     SCSURG        ANDNE     'Y'
     C*** BEG ADD 04/12/11 ********************************************
     C     @@ST          ANDNE     'KY'
     C     @@ST          OREQ      'KY'
     C     SCSURG        ANDNE     'R'
     C     SCSURG        ANDNE     'Y'
     C     SCSURG        ANDNE     'N'
     C*** END ADD 04/12/11 ********************************************
     C*
     C                   SETON                                        748190
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*  THE FEE SCHEDULE FIELD MUST BE Y OR N.
     C*
     C  N90SCFSR         IFNE      'N'
     C     SCFSR         ANDNE     'Y'
     C*
     C                   SETON                                        768190
     C*
     C                   ENDIF
     C*
     C*  THE SEPARATE PROCEDURE FIELD MUST BE Y OR BLANK.
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     *BLANK
     C*
     C                   SETON                                        708490
     C*
     C                   ENDIF
     C*
     C*  THE NO-CUT FLAG FIELD MUST BE Y OR BLANK.
     C  N90SC1A5         IFNE      'Y'
     C     SC1A5         ANDNE     *BLANK
     C*
     C                   SETON                                        797890
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 10/25/99 ****************************************
     C*** BEG ADD *** 06/16/08 ****************************************
     C*================================================================
     C     EDITDE        BEGSR
     C*
     C*  THE BY REPORT FIELD MUST BE Y,N,A, P, OR X.
     C*
     C  N90SCRPT         IFNE      'N'
     C     SCRPT         ANDNE     'Y'
     C     SCRPT         ANDNE     'A'
     C     SCRPT         ANDNE     'X'
     C     SCRPT         ANDNE     'P'
     C     SCRPT         OREQ      'S'
     C*
     C                   SETON                                        728590
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C*** END ADD *** 06/16/08 ****************************************
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SETOFF                                       52
     C*
     C* CPT-4 CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCCPT4        IFEQ      *BLANKS
     C                   SETON                                        758290
     C                   ENDIF                                                  SCKEY
     C*
     C* EFFECTIVE DATE MUST BE VALID FOR COPY/ADD
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*** BEG ADD *** 09/24/98 ****************************************
     C     *IN90         OREQ      *OFF
     C     *IN30         ANDEQ     *ON
     C*** END ADD *** 09/24/98 ****************************************
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        768290
     C*
     C                   ENDIF                                                  SRERR
     C*
     C                   ENDIF                                                  *IN90/*IN40
     C*
     C* BE SURE CPT-4 CODE HAS NOT BEEN USED PREVIOUSLY FOR COPY/ADD
     C*
     C                   MOVEL     SCCPT4        KKCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C*** BEG DEL *** 09/24/98 ****************************************
     C***40N90** PRKY      CHAINPPR01                99
     C***40N90N99          SETON                     778290
     C*** END DEL *** 09/24/98 ****************************************
     C*
     C*** BEG ADD *** 09/24/98 ****************************************
     C*
     C  N90*IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
     C*
     C     PRKY          CHAIN     PPR01                              99
     C*
     C  N99              SETON                                        778290
     C*
     C                   ENDIF                                                  *IN30=OFF
     C*
     C*** END ADD *** 09/24/98 ****************************************
     C*
     C* CPT-4 CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDSC1        IFEQ      *BLANKS
     C                   SETON                                        718190
     C                   ENDIF                                                  SCDSC1
     C*** BEG ADD *** 09/05/00 ****************************************
     C     SC1AG         IFNE      'Y'
     C     SC1AG         ANDNE     *BLANKS
     C                   SETON                                        53  90
     C                   ENDIF
     C*** END ADD *** 09/05/00 ****************************************
     C*
     C* CALL STATE SPECIFIC EDITS
     C*
     C                   EXSR      STEDTS
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90PRDT          IFGT      SAVEDT
     C     PRDT          OREQ      SAVEDT
     C     PRTIME        ANDGT     SAVETM
     C                   SETON                                        718390
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     1             SCRN#             1 0
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     2             SCRN#
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     3             SCRN#
     C*
     C                   ENDSR
     C*** BEG ADD *** 11/02/03 ****************************************
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* BYPASS COORECT CODING AND/OR CODREVIEW
     C*
     C     HFLD          IFEQ      'SC1A5'
     C     @@ST          ANDEQ     'KY'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'BPCC'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SC1A5
     C                   MOVEL(P)  GTDESC        SC1A5D
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9020
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDSR
     C*** END ADD *** 11/02/03 ****************************************
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
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     PRCPT4        SCCPT4
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      PRDSC1        SCDSC1
     C                   MOVE      PRDSC2        SCDSC2
     C                   MOVE      PRMRA         SCMRA
     C                   MOVE      PRFOLU        SCFOLU
     C                   MOVE      PRANAM        SCANAM
     C                   MOVE      PR1A2         SC1A2
     C                   MOVE      PRPROF        SCPROF
     C                   MOVE      PR1A1         SC1A1
     C                   MOVE      PRRPT         SCRPT
     C                   MOVE      PRSURG        SCSURG
     C                   MOVE      PRSUPP        SCSUPP
     C                   MOVE      PRFSR         SCFSR
     C*** BEG ADD *** 09/05/00 ****************************************
     C                   MOVE      PR1AG         SC1AG
     C*** END ADD *** 09/05/00 ****************************************
     C*
     C     @@ST          IFEQ      'FL'
     C*
     C                   Z-ADD     PR92N1        SC92N2
     C                   Z-ADD     PR92N2        SC92N1
     C***** Beg Add ***** 12/26/14 ************************************
     C                   Z-ADD     PR92NJ        SC92N3
     C                   Z-ADD     PR92NK        SC92N4
     C***** End Add ***** 12/26/14 ************************************
     C*
     C                   ELSE
     C*
     C                   Z-ADD     PR92N1        SC92N1
     C                   Z-ADD     PR92N2        SC92N2
     C*
     C***** Beg Add ***** 12/26/14 ************************************
     C                   Z-ADD     PR92N3        SC92N3
     C                   Z-ADD     PR92N4        SC92N4
     C***** End Add ***** 12/26/14 ************************************
     C                   ENDIF
     C*
     C***** Beg Del ***** 12/26/14 ************************************
     C******             Z-ADD     PR92N3        SC92N3
     C******             Z-ADD     PR92N4        SC92N4
     C***** End Del ***** 12/26/14 ************************************
     C                   Z-ADD     PR92N5        SC92N5
     C*** BEG ADD *** 07/03/05 ****************************************
     C                   Z-ADD     PR92N6        SC92N6
     C*** END ADD *** 07/03/05 ****************************************
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   Z-ADD     PR92N7        SC92N7
     C                   Z-ADD     PR92N8        SC92N8
     C*** END ADD *** 09/28/15 ****************************************
     C                   MOVE      PR1A3         SC1A3
     C                   MOVE      PR1A4         SC1A4
     C                   MOVE      PR1A5         SC1A5
     C                   MOVE      PR1A6         SC1A6
     C                   MOVEL     PRSTAT        SCSTAT
     C*** BEG ADD *** 07/31/98 ****************************************
     C                   MOVE      PR1AF         SC1AF
     C*** END ADD *** 07/31/98 ****************************************
     C*
     C*** BEG ADD *** 08/25/99 ****************************************
     C     @@ST          IFEQ      'AL'
     C                   MOVE      PR1AE         SC1A3
     C                   MOVE      PR1AD         SC1A5
     C                   ENDIF
     C*** END ADD *** 08/25/99 ****************************************
     C*** BEG ADD *** 08/27/02 ****************************************
     C     @@ST          IFEQ      'CA'
     C                   MOVE      PR1A5         SC1A3
     C                   MOVE      PR1AD         SC1A4
     C                   MOVE      PR1A3         SC1A5
     C                   MOVE      PR1A4         SC1A6
     C                   ENDIF
     C*** END ADD *** 08/27/02 ****************************************
     C*** BEG ADD *** 07/30/02 ****************************************
     C     @@ST          IFEQ      'FL'
     C                   MOVE      PR1AD         SC1A4
     C*** BEG ADD *** 10/10/02 ****************************************
     C                   MOVE      PR1A4         SC1A5
     C*** END ADD *** 10/10/02 ****************************************
     C*** BEG ADD *** 02/19/09 ****************************************
     C                   MOVE      PR1A7         SC1A7
     C*** END ADD *** 02/19/09 ****************************************
     C                   ENDIF
     C*** END ADD *** 07/30/02 ****************************************
     C*** BEG ADD *** 07/19/01 ****************************************
     C     @@ST          IFEQ      'GA'
     C                   MOVE      PR1A3         SC1A3
     C*** BEG ADD *** 06/23/14 ****************************************
     C                   MOVEl     PR5A4         SC5A4
     C                   MOVEl     PR5A5         SC5A5
     C*** END ADD *** 06/23/14 ****************************************
     C                   ENDIF
     C*** END ADD *** 07/19/01 ****************************************
     C*** BEG ADD *** 10/25/99 ****************************************
     C     @@ST          IFEQ      'KS'
     C                   MOVE      PR1AD         SC1A3
     C                   MOVE      PR1A5         SC1A5
     C                   ENDIF
     C*** END ADD *** 10/25/99 ****************************************
     C*** BEG ADD *** 02/01/02 ****************************************
     C     @@ST          IFEQ      'KY'
     C                   MOVE      PR1A5         SC1A4
     C*** BEG ADD *** 11/02/03 ****************************************
     C                   MOVE      PR1AH         SC1A5
     C*** END ADD *** 11/02/03 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/01/02 ****************************************
     C*** BEG ADD *** 07/01/03 ****************************************
     C     @@ST          IFEQ      'MA'
     C                   MOVE      PR92N2        SC92N2
     C                   MOVE      PR92N3        SC92N3
     C                   MOVE      PR92N4        SC92N4
     C                   MOVE      PR92N5        SC92N5
     C                   MOVE      PR92N6        SC92N6
     C                   MOVE      PR1A3         SC1A3
     C                   MOVE      PR1AD         SC1A4
     C                   ENDIF
     C*** END ADD *** 07/01/03 ****************************************
     C*** BEG ADD *** 05/02/00 ****************************************
     C     @@ST          IFEQ      'MS'
     C*** BEG DEL *** 10/25/02 ****************************************
     C******************** MOVE PR1AD     SC1A5
     C*** END DEL *** 10/25/02 ****************************************
     C*** BEG ADD *** 10/25/02 ****************************************
     C                   MOVE      PR1AD         SC1A4
     C                   MOVE      PR1A6         SC1A5
     C*** END ADD *** 10/25/02 ****************************************
     C*** BEG ADD *** 09/26/03 ****************************************
     C                   MOVE      PR1A4         SC1A6
     C*** END ADD *** 09/26/03 ****************************************
     C*** Beg Add *** 12/09/16 ****************************************
     C                   MOVE      PR1A7         SC1A7
     C                   MOVE      PR5A5         SC5A5
     C                   MOVE      PR92N4        SC92N4
     C*** End Add *** 12/09/16 ****************************************
     C                   ENDIF
     C*** END ADD *** 05/02/00 ****************************************
     C*** BEG ADD *** 07/03/02 ****************************************
     C     @@ST          IFEQ      'NC'
     C                   MOVE      PR1AD         SC1A4
     C***** Beg Add *** 09/09/15 **************************************
     C                   MOVE      PR92N4        SC92N4
     C                   MOVE      PR92N5        SC92N5
     C***** End Add *** 09/09/15 **************************************
     C                   ENDIF
     C*** END ADD *** 02/16/00 ****************************************
     C*** BEG ADD *** 02/16/00 ****************************************
     C     @@ST          IFEQ      'NE'
     C                   MOVE      PR1A4         SC1A3
     C                   MOVE      PR1A5         SC1A5
     C*** BEG DEL *** 07/07/04 ****************************************
     C******************** MOVE PR1A6     SC1A6
     C*** END DEL *** 07/07/04 ****************************************
     C*** BEG ADD *** 07/07/04 ****************************************
     C                   MOVE      PR1AD         SC1A4
     C*** END ADD *** 07/07/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/16/00 ****************************************
     C*** BEG ADD *** 08/15/02 ****************************************
     C     @@ST          IFEQ      'NY'
     C                   MOVE      PR1AD         SC1A4
     C                   MOVE      PR1A5         SC1A3
     C*** BEG ADD *** 12/07/04 ****************************************
     C                   MOVE      PR1A4         SC1A5
     C                   MOVE      PR92OB        SC92N2
     C*** END ADD *** 12/07/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 08/15/02 ****************************************
     C*** BEG ADD *** 09/23/99 ****************************************
     C     @@ST          IFEQ      'SC'
     C                   MOVE      PR1AD         SC1A3
     C*** BEG ADD *** 02/22/03 ****************************************
     C                   MOVE      PR1A3         SC1A4
     C*** END ADD *** 02/22/03 ****************************************
     C                   ENDIF
     C*** END ADD *** 09/23/99 ****************************************
     C*** BEG ADD *** 09/28/04 ****************************************
     C     @@ST          IFEQ      'TX'
     C                   MOVE      PR1AI         SC1A4
     C                   ENDIF
     C*** END ADD *** 09/28/04 ****************************************
     C*** BEG ADD *** 04/08/02 ****************************************
     C     @@ST          IFEQ      'UC'
     C                   MOVE      PR1A5         SC1A3
     C*** BEG ADD *** 12/12/02 ****************************************
     C                   MOVE      PR1A6         SC1A5
     C                   MOVE      PR1A7         SC1A6
     C                   MOVE      PR1A8         SC1AF
     C*** END ADD *** 12/12/02 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/08/02 ****************************************
     C*** BEG ADD *** 07/22/06 ****************************************
     C     @@ST          IFEQ      'VT'
     C                   MOVE      PR1AP         SC1A5
     C                   ENDIF
     C*** END ADD *** 08/25/99 ****************************************
     C*** BEG ADD *** 12/28/99 ****************************************
     C     @@ST          IFEQ      'WV'
     C                   MOVE      PR1AD         SC1A3
     C                   MOVE      PR1A5         SC1A5
     C                   ENDIF
     C*** END ADD *** 12/28/99 ****************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C*** Beg Add *** 12/28/15 *******************************************
     C*** Beg Del *** 12/18/17 *******************************************
     C**** *dtaara       define    inUCDate      inEfDt
     C*** End Del *** 12/18/17 *******************************************
     C*** Beg Del *** 01/12/17 *******************************************
     C**** *lock         in        inEfDt
     C*** End Del *** 01/12/17 *******************************************
     C*** Beg Add *** 12/18/17 *******************************************
     C     *lock         in        inUCDate
     C                   eval      inEfDt = inUCDate
     C*** End Add *** 12/18/17 *******************************************
     C                   if        inEfDt <> *blanks and @@St = 'UC'
     C                   eval      scEfDt = %dec(inEfDt:8:0)
     C                   Endif
     C*** End Add *** 12/28/15 *******************************************
     C*
     C                   MOVE      *BLANK        SCDSC1
     C                   MOVE      *BLANK        SCDSC2
     C                   MOVE      *BLANK        SCMRA
     C                   MOVE      *BLANK        SCFOLU
     C                   MOVE      *BLANK        SCANAM
     C                   MOVE      *BLANK        SC1A2
     C                   MOVE      *BLANK        SCPROF
     C                   MOVE      *BLANKS       SC1A1
     C                   MOVE      *BLANK        SCRPT
     C                   MOVE      *BLANK        SCSURG
     C                   MOVE      *BLANK        SCSUPP
     C                   MOVE      *BLANK        SCFSR
     C                   Z-ADD     *ZEROS        SC92N1
     C                   Z-ADD     *ZEROS        SC92N2
     C                   Z-ADD     *ZEROS        SC92N3
     C                   Z-ADD     *ZEROS        SC92N4
     C                   Z-ADD     *ZEROS        SC92N5
     C*** BEG ADD *** 07/03/05 ****************************************
     C                   Z-ADD     *ZEROS        SC92N6
     C*** END ADD *** 07/03/05 ****************************************
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   Z-ADD     *ZEROS        SC92N7
     C                   Z-ADD     *ZEROS        SC92N8
     C*** END ADD *** 09/28/15 ****************************************
     C                   MOVE      *BLANKS       SC1A3
     C                   MOVE      *BLANKS       SC1A4
     C                   MOVE      *BLANKS       SC1A5
     C                   MOVE      *BLANKS       SC1A6
     C*** BEG ADD *** 07/31/98 ****************************************
     C                   MOVE      *BLANKS       SC1AF
     C*** END ADD *** 07/31/98 ****************************************
     C*** BEG ADD *** 02/19/09 ****************************************
     C                   MOVE      *BLANKS       SC1A7
     C*** END ADD *** 02/19/09 ****************************************
     C*** BEG ADD *** 09/05/00 ****************************************
     C                   MOVE      *BLANKS       SC1AG
     C*** END ADD *** 09/05/00 ****************************************
     C                   MOVEL     'A'           SCSTAT
     C*** BEG ADD *** 05/20/03 ****************************************
     C                   MOVE      'Y'           SCFSR
     C                   MOVE      'N'           SCRPT
     C                   MOVE      'N'           SCSUPP
     C                   MOVE      'N'           SCSURG
     C*** END ADD *** 05/20/03 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         PRCO#
     C                   MOVEL     SCCPT4        PRCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PREFDT
     C*
     C                   MOVE      SCDSC1        PRDSC1
     C                   MOVE      SCDSC2        PRDSC2
     C                   MOVE      SCMRA         PRMRA
     C                   MOVE      SCFOLU        PRFOLU
     C                   MOVE      SCANAM        PRANAM
     C                   MOVE      SC1A2         PR1A2
     C                   MOVE      SCPROF        PRPROF
     C                   MOVE      SC1A1         PR1A1
     C                   MOVE      SCRPT         PRRPT
     C                   MOVE      SCSURG        PRSURG
     C                   MOVE      SCSUPP        PRSUPP
     C                   MOVE      SCFSR         PRFSR
     C                   Z-ADD     SC92N1        PR92N1
     C*** BEG ADD *** 12/07/04 ****************************************
     C     SCST          IFNE      'NY'
     C*** END ADD *** 12/07/04 ****************************************
     C                   Z-ADD     SC92N2        PR92N2
     C*** BEG ADD *** 12/07/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 12/07/04 ****************************************
     C***** Beg Del ***** 12/26/14 ************************************
     C******             Z-ADD     SC92N3        PR92N3
     C******             Z-ADD     SC92N4        PR92N4
     C***** End Del ***** 12/26/14 ************************************
     C                   Z-ADD     SC92N5        PR92N5
     C*** BEG ADD *** 07/03/05 ****************************************
     C                   Z-ADD     SC92N6        PR92N6
     C*** END ADD *** 07/03/05 ****************************************
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   Z-ADD     SC92N7        PR92N7
     C                   Z-ADD     SC92N8        PR92N8
     C*** END ADD *** 09/28/15 ****************************************
     C                   MOVE      SC1A3         PR1A3
     C                   MOVE      SC1A4         PR1A4
     C                   MOVE      SC1A5         PR1A5
     C                   MOVE      SC1A6         PR1A6
     C                   MOVEL     SCSTAT        PRSTAT
     C*** BEG ADD *** 07/31/98 ****************************************
     C                   MOVE      SC1AF         PR1AF
     C*** END ADD *** 07/31/98 ****************************************
     C*** BEG ADD *** 09/05/00 ****************************************
     C                   MOVE      SC1AG         PR1AG
     C*** END ADD *** 09/05/00 ****************************************
     C*** BEG ADD *** 08/25/99 ****************************************
     C     @@ST          IFEQ      'AL'
     C                   MOVE      SC1A3         PR1AE
     C                   MOVE      SC1A5         PR1AD
     C                   ENDIF                                                  @@ST=AL
     C*** END ADD *** 08/25/99 ****************************************
     C*** BEG ADD *** 08/27/02 ****************************************
     C     @@ST          IFEQ      'CA'
     C                   MOVE      SC1A3         PR1A5
     C                   MOVE      SC1A4         PR1AD
     C                   MOVE      SC1A5         PR1A3
     C                   MOVE      SC1A6         PR1A4
     C                   ENDIF
     C*** END ADD *** 08/27/02 ****************************************
     C*** BEG ADD *** 03/30/00 ****************************************
     C     @@ST          IFEQ      'FL'
     C                   MOVE      SC92N1        PR92N2
     C                   MOVE      SC92N2        PR92N1
     C*** BEG ADD *** 07/30/02 ****************************************
     C                   MOVE      SC1A4         PR1AD
     C*** END ADD *** 07/30/02 ****************************************
     C*** BEG ADD *** 10/10/02 ****************************************
     C                   MOVE      SC1A5         PR1A4
     C*** END ADD *** 10/10/02 ****************************************
     C*** BEG ADD *** 02/19/09 ****************************************
     C                   MOVE      SC1A7         PR1A7
     C*** END ADD *** 02/19/09 ****************************************
     C***** Beg Add ***** 12/26/14 ************************************
     C                   Z-ADD     SC92N3        PR92NJ
     C                   Z-ADD     SC92N4        PR92NK
     C*
     C                   Else
     C*
     C                   Z-ADD     SC92N3        PR92N3
     C                   Z-ADD     SC92N4        PR92N4
     C***** End Add ***** 12/26/14 ************************************
     C                   ENDIF                                                  @@ST=AL
     C*** BEG ADD *** 07/19/01 ****************************************
     C     @@ST          IFEQ      'GA'
     C                   MOVE      SC1A3         PR1A3
     C*** BEG ADD *** 06/23/14 ****************************************
     C                   MOVE      SC5A4         PR5A4
     C                   MOVE      SC5A5         PR5A5
     C*** END ADD *** 06/23/14 ****************************************
     C                   ENDIF                                                  @@ST=AL
     C*** END ADD *** 07/19/01 ****************************************
     C*** BEG ADD *** 02/01/02 ****************************************
     C     @@ST          IFEQ      'KY'
     C                   MOVE      SC1A4         PR1A5
     C*** BEG ADD *** 11/02/03 ****************************************
     C                   MOVE      SC1A5         PR1AH
     C*** END ADD *** 11/02/03 ****************************************
     C                   ENDIF                                                  @@ST=AL
     C*** END ADD *** 02/01/02 ****************************************
     C*** BEG ADD *** 07/01/03 ****************************************
     C     @@ST          IFEQ      'MA'
     C                   MOVE      SC92N2        PR92N2
     C                   MOVE      SC92N3        PR92N3
     C                   MOVE      SC92N4        PR92N4
     C                   MOVE      SC92N5        PR92N5
     C                   MOVE      SC92N6        PR92N6
     C                   MOVE      SC1A3         PR1A3
     C                   MOVE      SC1A4         PR1AD
     C                   ENDIF
     C*** END ADD *** 07/01/03 ****************************************
     C*** BEG ADD *** 05/02/00 ****************************************
     C     @@ST          IFEQ      'MS'
     C*** BEG DEL *** 10/25/02 ****************************************
     C******************** MOVE SC1A5     PR1AD
     C*** END DEL *** 10/25/02 ****************************************
     C*** BEG ADD *** 10/25/02 ****************************************
     C                   MOVE      SC1A4         PR1AD
     C                   MOVE      SC1A5         PR1A6
     C*** END ADD *** 10/25/02 ****************************************
     C*** BEG ADD *** 09/26/03 ****************************************
     C                   MOVE      SC1A6         PR1A4
     C*** END ADD *** 09/26/03 ****************************************
     C*** Beg Add *** 12/09/16 ****************************************
     C                   MOVE      SC1A7         PR1A7
     C                   MOVE      SC1A5         PR1A5
     C                   MOVE      SC92N4        PR92N4
     C*** End Add *** 12/09/16 ****************************************
     C                   ENDIF
     C*** END ADD *** 05/02/00 ****************************************
     C*** BEG ADD *** 07/03/02 ****************************************
     C     @@ST          IFEQ      'NC'
     C                   MOVE      SC1A4         PR1AD
     C***** Beg Add *** 09/09/15 **************************************
     C                   MOVE      SC92N4        PR92N4
     C                   MOVE      SC92N5        PR92N5
     C***** End Add *** 09/09/15 **************************************
     C                   ENDIF
     C*** END ADD *** 02/16/00 ****************************************
     C*** BEG ADD *** 02/16/00 ****************************************
     C     @@ST          IFEQ      'NE'
     C                   MOVE      SC1A3         PR1A4
     C                   MOVE      SC1A5         PR1A5
     C*** BEG DEL *** 07/07/04 ****************************************
     C******************** MOVE SC1A6     PR1A6
     C*** END DEL *** 07/07/04 ****************************************
     C*** BEG ADD *** 07/07/04 ****************************************
     C                   MOVE      SC1A4         PR1AD
     C*** END ADD *** 07/07/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/16/00 ****************************************
     C*** BEG ADD *** 08/15/02 ****************************************
     C     @@ST          IFEQ      'NY'
     C                   MOVE      SC1A4         PR1AD
     C                   MOVE      SC1A3         PR1A5
     C*** BEG ADD *** 12/07/04 ****************************************
     C                   MOVE      SC1A5         PR1A4
     C                   MOVE      SC92N2        PR92OB
     C*** END ADD *** 12/07/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 08/15/02 ****************************************
     C*** BEG ADD *** 09/23/99 ****************************************
     C     @@ST          IFEQ      'SC'
     C                   MOVE      SC1A3         PR1AD
     C*** BEG ADD *** 02/22/03 ****************************************
     C                   MOVE      SC1A4         PR1A3
     C*** END ADD *** 02/22/03 ****************************************
     C                   ENDIF
     C*** END ADD *** 09/23/99 ****************************************
     C*** BEG ADD *** 10/25/99 ****************************************
     C     @@ST          IFEQ      'KS'
     C                   MOVE      SC1A3         PR1AD
     C                   MOVE      SC1A5         PR1A5
     C                   ENDIF
     C*** END ADD *** 10/25/99 ****************************************
     C*** BEG ADD *** 09/28/04 ****************************************
     C     @@ST          IFEQ      'TX'
     C                   MOVE      SC1A4         PR1AI
     C                   ENDIF
     C*** END ADD *** 09/28/04 ****************************************
     C*** BEG ADD *** 04/08/02 ****************************************
     C     @@ST          IFEQ      'UC'
     C                   MOVE      SC1A3         PR1A5
     C*** BEG ADD *** 12/12/02 ****************************************
     C                   MOVE      SC1A5         PR1A6
     C                   MOVE      SC1A6         PR1A7
     C                   MOVE      SC1AF         PR1A8
     C*** END ADD *** 12/12/02 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/08/02 ****************************************
     C*** BEG ADD *** 07/22/06 ****************************************
     C     @@ST          IFEQ      'VT'
     C                   MOVE      SC1A5         PR1AP
     C                   ENDIF
     C*** END ADD *** 08/25/99 ****************************************
     C*** BEG ADD *** 12/28/99 ****************************************
     C     @@ST          IFEQ      'WV'
     C                   MOVE      SC1A3         PR1AD
     C                   MOVE      SC1A5         PR1A5
     C                   ENDIF
     C*** END ADD *** 12/28/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** BEG ADD *** 11/02/03 ****************************************
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*** END ADD *** 11/02/03 ****************************************
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
     C*
       //*** Beg Add *** 07/09/19 *************************************
      /Free
                      If isMC433 = '1';
                         OutMode  = @@Mode;
                         OutState = @@St;
                         OutCall  = 'S';

                         callp CodeInfo(outPrDs:@@Rrn:outMode:OutState:OutCall);

                         *inlr = *on;

                         Return;
                      Endif;
      /End-free
       //*** Beg Add *** 07/09/19 *************************************
     C*** BEG ADD *** 07/31/98 ****************************************
     C                   SETOFF                                       5152
     C*** END ADD *** 07/31/98 ****************************************
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*** BEG ADD *** 09/24/98 ****************************************
     C     @@MODE        COMP      'P'                                    30    COPY
     C*** END ADD *** 09/24/98 ****************************************
     C*
     C     *IN09         IFEQ      *ON
     C*
     C                   MOVEL(P)  MODED         SCMODE
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C* Gets the screen headers as determined by the state.
     C*
     C                   EXSR      CPT4HD
     C*
     C* Set indicators to create screen by state.
     C*
     C                   EXSR      STSCRN
     C*** BEG ADD *** 11/02/03 ****************************************
     C                   MOVE      ##CO#         KKCO#
     C*** END ADD *** 11/02/03 ****************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* STEDTS - Carries out state specific edits.
     C*================================================================
     C     STEDTS        BEGSR
     C*
     C     @@ST          CASEQ     'AL'          EDITAL
     C*** BEG ADD *** 08/27/02 ****************************************
     C     @@ST          CASEQ     'CA'          EDITCA
     C*** END ADD *** 08/27/02 ****************************************
     C*** BEG ADD *** 07/22/09 ****************************************
     C     @@ST          CASEQ     'AR'          EDITAR
     C*** END ADD *** 07/22/09 ****************************************
     C*** BEG ADD *** 10/11/04 ****************************************
     C     @@ST          CASEQ     'CT'          EDITCT
     C*** END ADD *** 10/11/04 ****************************************
     C     @@ST          CASEQ     'GA'          EDITGA
     C     @@ST          CASEQ     'FL'          EDITFL
     C     @@ST          CASEQ     'KS'          EDITKS
     C     @@ST          CASEQ     'KY'          EDITKY
     C     @@ST          CASEQ     'LA'          EDITLA
     C     @@ST          CASEQ     'MA'          EDITMA
     C*** BEG ADD *** 10/12/04 ****************************************
     C     @@ST          CASEQ     'ME'          EDITME
     C*** END ADD *** 10/12/04 ****************************************
     C*** BEG ADD *** 04/26/05 ****************************************
     C     @@ST          CASEQ     'MI'          EDITMI
     C*** END ADD *** 04/26/05 ****************************************
     C     @@ST          CASEQ     'MD'          EDITMD
     C     @@ST          CASEQ     'MS'          EDITMS
     C     @@ST          CASEQ     'NC'          EDITNC
     C     @@ST          CASEQ     'NH'          EDITNH
     C     @@ST          CASEQ     'NE'          EDITNE
     C     @@ST          CASEQ     'NM'          EDITNM
     C     @@ST          CASEQ     'NY'          EDITNY
     C     @@ST          CASEQ     'OH'          EDITOH
     C     @@ST          CASEQ     'OK'          EDITOK
     C     @@ST          CASEQ     'PA'          EDITPA
     C     @@ST          CASEQ     'RI'          EDITRI
     C     @@ST          CASEQ     'SC'          EDITSC
     C     @@ST          CASEQ     'TN'          EDITTN
     C     @@ST          CASEQ     'TX'          EDITTX
     C*** BEG ADD *** 09/25/01 ****************************************
     C     @@ST          CASEQ     'VT'          EDITVT
     C*** END ADD *** 09/25/01 ****************************************
     C*** BEG ADD *** 12/28/99 ****************************************
     C     @@ST          CASEQ     'WV'          EDITWV
     C*** END ADD *** 12/28/99 ****************************************
     C*** BEG ADD *** 06/16/08 ****************************************
     C     @@ST          CASEQ     'DE'          EDITDE
     C*** END ADD *** 06/16/08 ****************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* STSCRN - Screen set up by state.
     C*================================================================
     C     STSCRN        BEGSR
     C*
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   EVAL      dsp92H7 = *off
     C                   EVAL      dsp92H8 = *off
     C*** END ADD *** 09/28/15 ****************************************
     C*
     C                   MOVE      *ON           *IN37
     C*
     C                   SELECT
     C*
     C     @@ST          WHENEQ    'AL'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C*** BEG ADD *** 08/25/99 ****************************************
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN39
     C*** END ADD *** 08/25/99 ****************************************
     C*** BEG ADD *** 08/29/01 ****************************************
     C                   MOVE      '1'           *IN41
     C*** BEG ADD *** 08/27/02 ****************************************
     C     @@ST          WHENEQ    'CA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN38
     C*** END ADD *** 08/27/02 ****************************************
     C*** BEG ADD *** 10/11/04 ****************************************
     C     @@ST          WHENEQ    'CT'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN34
     C*** BEG ADD *** 01/13/09 ****************************************
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C*** Beg Add *** 11/25/13 ****************************************
     C                   MOVE      '1'           *IN35
     C*** End Add *** 11/25/13 ****************************************
     C*** END ADD *** 01/13/09 ****************************************
     C*** END ADD *** 10/11/04 ****************************************
     C*** END ADD *** 08/29/01 ****************************************
     C     @@ST          WHENEQ    'FL'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C*** BEGIN ADD *** 04/25/01 **************************************
     C                   MOVE      '1'           *IN34
     C*** END   ADD *** 04/25/01 **************************************
     C*** BEG ADD *** 07/30/02 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 07/30/02 ****************************************
     C*** BEG ADD *** 10/10/02 ****************************************
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 10/10/02 ****************************************
     C*** BEG ADD *** 02/19/09 ****************************************
     C                   MOVE      '1'           *IN43
     C*** END ADD *** 02/19/09 ****************************************
     C*** BEG ADD *** 10/15/05 ****************************************
     C                   MOVE      '1'           *IN38
     C*** END ADD *** 10/15/05 ****************************************
     C*** BEG ADD *** 12/26/14 ****************************************
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN39
     C*** END ADD *** 12/26/14 ****************************************
     C     @@ST          WHENEQ    'GA'
     C                   MOVE      '1'           *IN31
     C*** BEG ADD *** 07/19/01 ***************************************
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 07/19/01 ***************************************
     C*** BEG ADD *** 08/18/03 ***************************************
     C                   MOVE      '1'           *IN35
     C*** BEG ADD *** 06/23/14 ***************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN54
     C*** END ADD *** 06/23/14 ***************************************
     C*** END ADD *** 08/18/03 ***************************************
     C     @@ST          WHENEQ    'KS'
     C                   MOVE      '1'           *IN31
     C*** BEG ADD *** 10/25/99 ****************************************
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 10/25/99 ****************************************
     C*** BEG ADD *** 03/21/05 ****************************************
     C                   MOVE      '1'           *IN32
     C*** END ADD *** 03/21/05 ****************************************
     C     @@ST          WHENEQ    'KY'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C*** BEG ADD *** 09/30/08 ****************************************
     C                   MOVE      '1'           *IN33
     C*** END ADD *** 09/30/08 ****************************************
     C*** BEG ADD *** 02/01/02 ****************************************
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 02/01/02 ****************************************
     C*** BEG ADD *** 11/02/03 ****************************************
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 11/02/03 ****************************************
     C     @@ST          WHENEQ    'LA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C*** BEGIN ADD *** 04/23/01 **************************************
     C                   MOVE      '1'           *IN36
     C*** END   ADD *** 04/23/01 **************************************
     C*** BEGIN ADD *** 09/19/13 **************************************
     C                   MOVE      '1'           *IN33
     C*** END   ADD *** 09/19/13 **************************************
     C     @@ST          WHENEQ    'MA'
     C                   MOVE      '1'           *IN31
     C*** BEG ADD *** 07/01/03 ****************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN42
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 07/01/03 ****************************************
     C*** BEG ADD *** 10/12/04 ****************************************
     C     @@ST          WHENEQ    'ME'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 10/12/04 ****************************************
     C*** BEG ADD *** 04/26/05 ****************************************
     C     @@ST          WHENEQ    'MI'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN39
     C*** BEG ADD *** 07/09/08 ****************************************
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN42
     C*** END ADD *** 07/09/08 ****************************************
     C*** END ADD *** 04/26/05 ****************************************
     C*** BEG ADD *** 07/13/98 ****************************************
     C     @@ST          WHENEQ    'MS'
     C*** BEG DEL *** 10/25/02 ****************************************
     C******************** MOVE '1'       *IN38
     C*** END DEL *** 10/25/02 ****************************************
     C*** END ADD *** 07/13/98 ****************************************
     C*** BEG ADD *** 12/16/98 ****************************************
     C                   MOVE      '1'           *IN31
     C*** END ADD *** 12/16/98 ****************************************
     C*** BEG ADD *** 10/25/02 ****************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C*** END ADD *** 10/25/02 ****************************************
     C*** BEG ADD *** 04/08/00 ****************************************
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 04/08/00 ****************************************
     C*** BEG ADD *** 05/02/00 ****************************************
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 05/02/00 ****************************************
     C*** BEG ADD *** 10/25/02 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 10/25/02 ****************************************
     C*** BEG ADD *** 09/26/03 ****************************************
     C                   MOVE      '1'           *IN38
     C*** END ADD *** 09/26/03 ****************************************
     C*** BEG ADD *** 02/03/14 ****************************************
     C                   MOVE      '1'           *IN43
     C*** END ADD *** 02/03/14 ****************************************
     C*** Beg Add *** 12/09/16 ****************************************
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN54
     C*** End Add *** 12/09/16 ****************************************
     C     @@ST          WHENEQ    'NC'
     C                   MOVE      '1'           *IN31
     C*** BEG ADD *** 12/11/01 ****************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 12/11/01 ****************************************
     C*** BEG ADD *** 04/29/07 ****************************************
     C                   MOVE      '1'           *IN33
     C*** END ADD *** 04/29/07 ****************************************
     C*** BEG ADD *** 10/13/04 ****************************************
     C***** Beg Add *** 09/09/15 **************************************
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C***** End Add *** 09/09/15 **************************************
     C     @@ST          WHENEQ    'NH'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 10/13/04 ****************************************
     C     @@ST          WHENEQ    'NY'
     C                   MOVE      '1'           *IN31
     C*** BEG ADD *** 08/15/02 ****************************************
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 08/15/02 ****************************************
     C*** BEG ADD *** 12/07/04 ****************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 12/07/04 ****************************************
     C***** Beg Add *** 04/03/19 **************************************
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN51
     C***** End Add *** 04/03/19 **************************************
     C*** BEG DEL *** 08/15/02 ****************************************
     C******************** MOVE '1'       *IN32
     C******************** MOVE '1'       *IN33
     C******************** MOVE '1'       *IN39
     C*** END DEL *** 02/16/00 ****************************************
     C     @@ST          WHENEQ    'NE'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN36
     C*** BEG DEL *** 07/07/04 ****************************************
     C******************** MOVE '1'       *IN38
     C*** END DEL *** 07/07/04 ****************************************
     C*** BEG ADD *** 07/07/04 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 07/07/04 ****************************************
     C*** END ADD *** 02/16/00 ****************************************
     C     @@ST          WHENEQ    'NM'
     C                   MOVE      '1'           *IN35
     C*** BEG ADD *** 07/31/98 ****************************************
     C     @@ST          WHENEQ    'OH'
     C                   MOVE      '1'           *IN51
     C                   MOVE      '1'           *IN31
     C*** END ADD *** 07/31/98 ****************************************
     C     @@ST          WHENEQ    'OK'
     C                   MOVE      '1'           *IN31
     C*** BEG ADD *** 08/08/00 ****************************************
     C                   MOVE      '1'           *IN32
     C*** END ADD *** 08/08/00 ****************************************
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN38
     C     @@ST          WHENEQ    'PA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C*** BEG ADD *** 12/08/00 ****************************************
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 12/08/00 ****************************************
     C*** BEG ADD *** 06/19/01 ****************************************
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 06/19/01 ****************************************
     C*** BEG ADD *** 04/03/02 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 04/03/02 ****************************************
     C*** BEG ADD *** 07/01/98 ****************************************
     C     @@ST          WHENEQ    'RI'
     C                   MOVE      '1'           *IN31
     C*** END ADD *** 07/01/98 ****************************************
     C     @@ST          WHENEQ    'SC'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C*** BEG ADD *** 09/23/99 ****************************************
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 09/23/99 ****************************************
     C*** BEG ADD *** 03/08/03 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 03/08/03 ****************************************
     C*** BEG ADD *** 09/28/15 ****************************************
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN42
     C                   EVAL      dsp92H7 = *on
     C                   EVAL      dsp92H8 = *on
     C*** END ADD *** 09/28/15 ****************************************
     C*** BEG ADD *** 07/03/05 ****************************************
     C     @@ST          WHENEQ    'TN'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN42
     C*** BEG ADD *** 09/29/09 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 09/29/09 ****************************************
     C*** END ADD *** 07/03/05 ****************************************
     C     @@ST          WHENEQ    'TX'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN34
     C*** BEG ADD *** 05/10/00 ****************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN51
     C*** END ADD *** 05/10/00 ****************************************
     C*** BEG ADD *** 09/28/04 ****************************************
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 09/28/04 ****************************************
     C*** BEG ADD *** 04/08/02 ****************************************
     C     @@ST          WHENEQ    'UC'
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 04/08/02 ****************************************
     C*** BEG ADD *** 12/12/02 ****************************************
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN51
     C*** END ADD *** 12/12/02 ****************************************
     C*
     C*** BEG ADD *** 09/25/01 ****************************************
     C     @@ST          WHENEQ    'VT'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 09/25/01 ****************************************
     C*** BEG ADD *** 12/28/99 ****************************************
     C     @@ST          WHENEQ    'WV'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 12/28/99 ****************************************
     C*** BEG ADD *** 08/30/01 ****************************************
     C                   MOVE      '1'           *IN32
     C*** END ADD *** 08/30/01 ****************************************
     C*** END ADD *** 12/28/99 ****************************************
     C*** BEG ADD *** 06/16/08 ****************************************
     C     @@ST          WHENEQ    'DE'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C*** END ADD *** 06/16/08 ****************************************
     C*** BEG ADD *** 06/11/09 ****************************************
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 06/11/09 ****************************************
     C*** BEG ADD *** 02/16/18 ****************************************
     C                   MOVE      '1'           *IN36
     C*** END ADD *** 02/16/18 ****************************************
     C*** BEG ADD *** 08/19/08 ****************************************
     C     @@ST          WHENEQ    'MD'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN42
     C*** BEG ADD *** 05/20/09 ****************************************
     C                   MOVE      '1'           *IN34
     C*** END ADD *** 05/20/09 ****************************************
     C*** Beg Add *** 03/24/10 ****************************************
     C                   MOVE      '1'           *IN35
     C*** End Add *** 03/24/10 ****************************************
     C*** BEG ADD *** 07/22/09 ****************************************
     C     @@ST          WHENEQ    'AR'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C*** END ADD *** 07/22/09 ****************************************
     C*
     C*** END ADD *** 08/19/08 ****************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVEL     DSTIME        PRTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C* CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED
     C*
     C                   EXSR      CALLR
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   MOVEL     Q1USER        PRIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVEL     SRDTO         PRIDT
     C*
     C                   MOVEL     DSTIME        PRTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C*** Beg Del *** 08/25/17 *********************************
     C*****************  FEOD      FMPRP
     C*** End Del *** 08/25/17 *********************************
     C*
     C* CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED
     C*
     C                   EXSR      CALLR
     C*
     C                   ENDSR
     C*** BEG ADD *** 07/09/08 ****************************************
     C*================================================================
     C* DLOVRD - Delete file overrides
     C*================================================================
     C     DLOVRD        BEGSR
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH
     C                   ENDSR
     C*================================================================
     C* OVRIDE - Overide files based  on adjudication state
     C*================================================================
     C     OVRIDE        BEGSR
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL(P)  @@ST          QQST
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C                   ENDSR
     C*** END ADD *** 07/09/08 ****************************************
**  (QCMDEX)
OVRDBF FILE(FMDML) TOFILE(*LIBL/FMDML  )
DLTOVR FILE(FMDML)
