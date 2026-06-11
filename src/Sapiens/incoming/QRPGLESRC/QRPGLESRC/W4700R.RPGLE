     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4700R - WORK WITH AUDIT TRACKING
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: MENUS - WW/GROUPS AND WW/EMPLOYERS(PLCY YRS)    *
     H*                                                               *
     H*  CALLS TO: W4700R02                                           *
     H*            W4700C - SUBMIT AUDIT REPORT                       *
     H*            W4700R21 - VIEW AUDIT REPORT                       *
     H*            WA007R - 'SUBMITTED TO BATCH' WINDOW               *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*            SC010R - DARTS                                     *
     H*            W0612R - A/R INQUIRY (OPT 60)                      *
     H*                   - A/R DETAIL SUMMARY (OPT 61)               *
     H*                                                               *
     H*  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/03/10  C:246  Support: Seq 74538                           *
     H*                  Fix the issue with subset not working when   *
     H*                  if filtered by Year and then option 5.       *
     H*                                                               *
     H* 12/30/09  C:232  PROJECT: WC1838                              *
     H*                  Added plan type in subset of "Work with Audit*
     H*                   Tracking" Screen.                           *
     H*                                                               *
     H* 12/22/09 C:234 Support: SEQ70845 WA133                        *
     H*                Add 'Active/Inactive' to Subset window         *
     H*                                                               *
     H* 01/21/09 C:220 Project: WC1664                                *
     H*                AUDIT TRACKING - TRACKING FROM EXPIRATION AUDIT*
     H*                 TO CANCELLATION AUDIT                         *
     H*                                                               *
     H* 05/02/07 C:132 132WFT                                         *
     H*              - Change Call to pass in an "M" to w/w Emps      *
     H*                to allow F12=Cancel to display.                *
     H*                                                               *
     H* 04/10/07 C:132 132WFT                                         *
     H*              - Change Call to w/w Employers from display to   *
     H*                active so Audit Payrolls can be updated only   *
     H*                for client 132, all others get display only.   *
     H*              - Change Call to pass in an "M" to get F12=Cancel*
     H*                                                               *
     H* 04/03/07 C:136 SUPPORT: WA045 57848                           *
     H*                CORRECT SUBSET FUNCTION IF TRYING TO SELECT    *
     H*                ON UNDERWRITER.                                *
     H*                                                               *
     H* 03/30/07 C:132 132WFT                                         *
     H*                Add EDIT if called from w/w Employers or from  *
     H*                w/w policy years to not allow option 6.        *
     H*                                                               *
     H* 09/26/06 C:132 132WFT                                         *
     H*                Add call to w/w Employers                      *
     H*                                                               *
     H* 07/08/04 C:132 SUPPORT WA099 43128                            *
     H*                Delete code to remove option F11 when subset   *
     H*                by date range                                  *
     H*                                                               *
     H* 09/19/03 C:61  SUPPORT WA121 38813                            *
     H*                Change code to immediately identify bad data.  *
     H*                                                               *
     H*  10/18/02  C:128 PROJECT: DR099 #34333                        *
     H*                  Changed the Q1PNAM parm on the call to       *
     H*                  SC010R so that an employer name could be     *
     H*                  retreived for subsequent displays.           *
     H*                                                               *
     H*  02/18/00  C:90  PROJECT: WC577                               *
     H*                  Add Opt 60 - A/R Inquiry                     *
     H*                  Add Opt 61 - A/R Detail Summary              *
     H*                                                               *
     H*  08/25/99  C:87  PROJECT: SUPP DR048 #19350                   *
     H*                  Do not reset VLDOPT in MVDBSC.  Assign 'N'   *
     H*                  to VLDOPT in READ and READP.                 *
     H*                                                               *
     H*  06/04/99  C:87  PROJECT: SUPP WA081 #18221                   *
     H*                  Audit tracking record deletion was being     *
     H*                  restricted based upon the Employer deletion  *
     H*                  level from Admin/DARTS.  This restriction    *
     H*                  has been removed.  The client can restrict   *
     H*                  deletion through the 'Function/Option        *
     H*                  Security' menu item.                         *
     H*                                                               *
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*
     H*  03/02/99  C:87  PROJECT: SUPP DR081 #16756                   *
     H*                  Audit was shown as not contested if contested*
     H*                  date was cleared through option 2=Change yet *
     H*                  the contested flag in the file = 'Y'.  Chgd  *
     H*                  code to show as contested if flag = 'Y'      *
     H*                  regardless of contested date field.          *
     H*                                                               *
     H*  11/05/98  C:87  PROJECT: SUPP WA048 #14820                   *
     H*                  Program was not looking at correct record    *
     H*                  for option processing in some instances.     *
     H*                  Changed assignation of relative record       *
     H*                  numbers to correct.                          *
     H*                                                               *
     H*  09/21/98  C:87  PROJECT: SUPP WA048/#13810                   *
     H*                  Screen record was not closed at program end  *
     H*                  causing screen to reappear upon reentering   *
     H*                  program before subset screen SCR04.          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  21-24 - Correspond to view#. i.e. view1=*in21                *
     H*  60,61 - General chain indicators                             *
     H*  65 - *ON: subsetted on policy exp date range                 *
     H*  96-99 - Subset indicators                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  WMAUL2,3,4,5,7-26 OMIT INACTIVE RECORDS (AUSTAT='I')         *
     H*  WMAUL19-22 SELECT AUDIT OUTSTANDING RECORDS ONLY             *
     H*                (AURCVD < AURQDT)                              *
     H*                                                               *
     H*****************************************************************
     FW4700F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMAUP     IF   E             DISK
     F                                     RENAME(PAU01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
      ***** BEG CHG ***** 01/21/09 ***************************************
     F************* WMAUL2  IF  E           K        DISK
     FWMAUL6    IF   E           K DISK
      ***** END CHG ***** 01/21/09 ***************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAU01:DBRCDF)
     F*
     F*  Logical file definition (for displaying items, 2nd view)
      ***** BEG CHG ***** 01/21/09 ***************************************
     F************* WMAUL3  IF  E           K        DISK
     FWMAUL30   IF   E           K DISK
      ***** END CHG ***** 01/21/09 ***************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAU01:DBRCDF2)
     F*
     F*  Logical file definition (for displaying items, 3rd view)
      ***** BEG CHG ***** 01/21/09 ***************************************
     F************* WMAUL4  IF  E           K        DISK
     FWMAUL31   IF   E           K DISK
      ***** END CHG ***** 01/21/09 ***************************************
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PAU01:DBRCDF3)
     F*
     F*  Logical file definition (for displaying items, 4th view)
      ***** BEG CHG ***** 01/21/09 ***************************************
     F************* WMAUL5  IF  E           K        DISK
     FWMAUL32   IF   E           K DISK
      ***** END CHG ***** 01/21/09 ***************************************
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PAU01:DBRCDF4)
     F*
     F*  Logical files for subsetting
      ***** BEG DLT ***** 01/21/09 ***************************************
     F************* WMAUL7  IF  E           K        DISK
     F*************                                 KINFDS DBFDB1
     F************* PAU01                             KRENAMEDBRCDB1
     F*
     F************* WMAUL8  IF  E           K        DISK
     F*************                                 KINFDS DBFDB2
     F************* PAU01                             KRENAMEDBRCDB2
     F*
     F************* WMAUL9  IF  E           K        DISK
     F*************                                 KINFDS DBFDB3
     F************* PAU01                             KRENAMEDBRCDB3
     F*
     F************* WMAUL10 IF  E           K        DISK
     F*************                                 KINFDS DBFDB4
     F************* PAU01                             KRENAMEDBRCDB4
     F*
     F************* WMAUL11 IF  E           K        DISK
     F*************                                 KINFDS DBFDD1
     F************* PAU01                             KRENAMEDBRCDD1
     F*
     F************* WMAUL12 IF  E           K        DISK
     F*************                                 KINFDS DBFDD2
     F************* PAU01                             KRENAMEDBRCDD2
     F*
     F************* WMAUL13 IF  E           K        DISK
     F*************                                 KINFDS DBFDD3
     F************* PAU01                             KRENAMEDBRCDD3
     F*
     F************* WMAUL14 IF  E           K        DISK
     F*************                                 KINFDS DBFDD4
     F************* PAU01                             KRENAMEDBRCDD4
     F*
     F************* WMAUL15 IF  E           K        DISK
     F*************                                 KINFDS DBFDE1
     F************* PAU01                             KRENAMEDBRCDE1
     F*
     F************* WMAUL16 IF  E           K        DISK
     F*************                                 KINFDS DBFDE2
     F************* PAU01                             KRENAMEDBRCDE2
     F*
     F************* WMAUL17 IF  E           K        DISK
     F*************                                 KINFDS DBFDE3
     F************* PAU01                             KRENAMEDBRCDE3
     F*
     F************* WMAUL18 IF  E           K        DISK
     F*************                                 KINFDS DBFDE4
     F************* PAU01                             KRENAMEDBRCDE4
     F*
     F************* WMAUL19 IF  E           K        DISK
     F*************                                 KINFDS DBFDG1
     F************* PAU01                             KRENAMEDBRCDG1
     F*
     F************* WMAUL20 IF  E           K        DISK
     F*************                                 KINFDS DBFDG2
     F************* PAU01                             KRENAMEDBRCDG2
     F*
     F************* WMAUL21 IF  E           K        DISK
     F*************                                 KINFDS DBFDG3
     F************* PAU01                             KRENAMEDBRCDG3
     F*
     F************* WMAUL22 IF  E           K        DISK
     F*************                                 KINFDS DBFDG4
     F************* PAU01                             KRENAMEDBRCDG4
     F*
     F************* WMAUL23 IF  E           K        DISK
     F*************                                 KINFDS DBFDH1
     F************* PAU01                             KRENAMEDBRCDH1
     F*
     F************* WMAUL24 IF  E           K        DISK
     F*************                                 KINFDS DBFDH2
     F************* PAU01                             KRENAMEDBRCDH2
     F*
     F************* WMAUL25 IF  E           K        DISK
     F*************                                 KINFDS DBFDH3
     F************* PAU01                             KRENAMEDBRCDH3
     F*
     F************* WMAUL26 IF  E           K        DISK
     F*************                                 KINFDS DBFDH4
     F************* PAU01                             KRENAMEDBRCDH4
      ***** END DLT ***** 01/21/09 ***************************************
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F                                     INFDS(EMFDB)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F*
     F*  Employer detail file
     FWDELL     IF   E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     F*
     F*  Fund master file
     FWMFNP     IF   E             DISK
     F*
     FWTACP     IF   E             DISK
     FSMUPP     IF   E             DISK
      *** BEG ADD ***** 12/30/09 *************************************
     FWTABL     IF   E           K DISK
     F*
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      *** END ADD ***** 12/30/09 *************************************
      /COPY SCOPYWW01D
     D*
     D HDA             S             80    DIM(12) CTDATA PERRCD(1)             Column headings
     D HD              S             80    DIM(12) CTDATA PERRCD(1)             Column headings
      **** BEGIN CHG **** 02/18/00 ************************************
     D******************* OF      1  22  1   OFD    35   Options, Function
      ***** BEG CHG ***** 09/26/06 ***************************************
     D***********         OF      1  24  1   OFD    35   Options, Function
     D OF              S              1    DIM(25) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(25) ALT(OF)
      ***** END CHG ***** 09/26/06 ***************************************
      **** END   CHG **** 02/18/00 ************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D EMFDB           DS
     D  EMRRN                397    400B 0
     D DBFDB1          DS
     D  DBRRB1               397    400B 0
     D DBFDB2          DS
     D  DBRRB2               397    400B 0
     D DBFDB3          DS
     D  DBRRB3               397    400B 0
     D DBFDB4          DS
     D  DBRRB4               397    400B 0
     D*
     D DBFDD1          DS
     D  DBRRD1               397    400B 0
     D DBFDD2          DS
     D  DBRRD2               397    400B 0
     D DBFDD3          DS
     D  DBRRD3               397    400B 0
     D DBFDD4          DS
     D  DBRRD4               397    400B 0
     D*
     D DBFDE1          DS
     D  DBRRE1               397    400B 0
     D DBFDE2          DS
     D  DBRRE2               397    400B 0
     D DBFDE3          DS
     D  DBRRE3               397    400B 0
     D DBFDE4          DS
     D  DBRRE4               397    400B 0
     D*
     D DBFDG1          DS
     D  DBRRG1               397    400B 0
     D DBFDG2          DS
     D  DBRRG2               397    400B 0
     D DBFDG3          DS
     D  DBRRG3               397    400B 0
     D DBFDG4          DS
     D  DBRRG4               397    400B 0
     D*
     D DBFDH1          DS
     D  DBRRH1               397    400B 0
     D DBFDH2          DS
     D  DBRRH2               397    400B 0
     D DBFDH3          DS
     D  DBRRH3               397    400B 0
     D DBFDH4          DS
     D  DBRRH4               397    400B 0
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Audit Tracking')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W4700R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(3)
     D VIEWMX          C                   CONST(4)
     D SF1PAG          C                   CONST(7)
     D DELETD          C                   CONST('*Deleted')
     D*
      ***** BEG DEL ***** 07/08/04 ************************************
     D********      '                    -C         CLRFKY
     D********      '       NNN'
     D********      '11F11=Change view   -C         F11FKY
     D********      '       YYY'
      ***** END DEL ***** 07/08/04 ************************************
      *
     D*
     D*  Data structures for view changes
     D                 DS
     D  XXVW1                  1     31
     D  V1EMP#                 1      9
     D  V1DIV                 11     15
     D  V1FYR                 17     19
     D                 DS
     D  XXVW2                  1     31
     D  V2ACO                  1      3
     D  V2EMP#                 5     13
     D  V2DIV                 15     19
     D  V2FYR                 21     23
     D                 DS
     D  XXVW3                  1     31
     D  V3PTDT                 2      9
     D  V3EMP#                12     20
     D  V3DIV                 22     26
     D                 DS
     D  XXVW4                  1     31
     D  V4ACO                  1      3
     D  V4PTDT                 6     13
     D  V4EMP#                16     24
     D  V4DIV                 26     30
     D*
     D*  Data structures for info changes
     D                 DS
      ***** BEG CHG ***** 01/21/09 ************************************
     D*************                           1  45 XXINF1
     D  XXINF1                 1     49
      ***** END CHG ***** 01/21/09 ************************************
     D  XXNAME                 1     24
     D  I1RQDT                26     33
     D  AUREP                 36     38
     D  I1ACO                 40     42
     D  XXCXFL                45     45
      ***** BEG ADD ***** 01/21/09 ************************************
     D  XXSTS                 48     48
     D                 DS
     D  X1INF1                 5     49
     D  X1NAME                 5     28
     D  I3RQDT                30     37
     D  A1REP                 40     42
     D  X1CXFL                45     45
     D  X1STS                 48     48
      ***** END ADD ***** 01/21/09 ************************************
     D                 DS
     D  XXINF2                 1     45
     D  I2RQDT                 1      8
     D  XXRCVD                10     17
     D  XXATDT                19     26
     D  XXPRDT                28     35
     D  XXMAIL                37     44
     D                 DS
     D  XXINF3                 1     44
     D  XXCTST                 4      4
     D  AUREFL                10     10
     D  AURSGN                16     16
     D  AURVFL                22     22
     D  EMUWTR                27     29
     D  EMARVW                32     34
     D  EMAMGR                37     39
     D  EMPACC                42     44
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Canx dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*  Note : @@RRN is passed in as the RRN of WMFNP
     C*  Note : @@RRN2 is passed in as the RRN of WDELP if called from
     C*         WWEmployers/Policy yrs, otherwise it is passed *blanks
     C*  Note : If called from menu, only first three parms are passed.
     C*         This is used in subroutine KEYINZ.
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCSEL         ##OPT             2
     C*
     C*  Company file/fund (group) retrieve program parameter list
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
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     RPTPL         PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRNDT            7
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKACO
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKACO
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY2B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKACO
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY3B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY4B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKACO
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1D         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKREP
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY2D         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKREP
     C                   KFLD                    KKACO
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY3D         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKREP
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY4D         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKREP
     C                   KFLD                    KKACO
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY1E         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCNFL
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY2E         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCNFL
     C                   KFLD                    KKACO
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY3E         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCNFL
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY4E         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCNFL
     C                   KFLD                    KKACO
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY1H         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXFL
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY2H         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXFL
     C                   KFLD                    KKACO
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY3H         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXFL
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY4H         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXFL
     C                   KFLD                    KKACO
     C                   KFLD                    KKPTDT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*  Audit file key
     C     AUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Audit file key (subset on emp#/div)
     C     AUKYA         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Audit file key (subset on policy yr)
     C     AUKYB         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C*
     C*  Audit file key (subset on audit co)
     C     AUKYC         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKACO
     C*
     C*  Audit file key (subset on audit rep)
     C     AUKYD         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKREP
     C*
     C*  Audit file key (subset on contested only)
     C     AUKYE         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCNFL
     C*
     C*  Audit file key (subset on cancelled/full term)
     C     AUKYH         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXFL
     C*
     C*  Darts/admin control defn key
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  User parameter key
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Employer master key
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Employer detail key
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C*  Employer detail key
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C     BSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBSCD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    AUCO#         KKCO#
     C     *LIKE         DEFINE    AUFND         KKFND
     C     *LIKE         DEFINE    AUEMP#        KKEMP#
     C     *LIKE         DEFINE    AUDIV         KKDIV
     C     *LIKE         DEFINE    AUFYR         KKFYR
     C     *LIKE         DEFINE    AUACO         KKACO
     C     *LIKE         DEFINE    AUREP         KKREP
     C     *LIKE         DEFINE    AUCNFL        KKCNFL
     C     *LIKE         DEFINE    AUCXFL        KKCXFL
     C     *LIKE         DEFINE    AUPTDT        KKPTDT
     C     *LIKE         DEFINE    AUPTDT        XSPFDT
     C     *LIKE         DEFINE    AUPTDT        XSPTDT
     C     *LIKE         DEFINE    @@RRN2        SAVRRN
     C*
     C*  Define and init fields for CHGOFA subroutine
     C                   MOVE      *BLANKS       XXOPFN            1
     C                   MOVE      *BLANKS       XXNMBR            2
     C                   MOVE      *BLANKS       XXMIS             3
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHGOFA - SUBROUTINE TO CHANGE OPTION/FUNCTION ARRAY
     C*===============================================================
     C*  Before calling this subroutine make the following assignments:
     C*       XXOPFN = '1' if Option, '2' if Function
     C*       XXNMBR = Option/Function number to change
     C*            NOTE: Pass option/function numbers as 2 char fields.
     C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
     C*       XXMIS = 'XXX' where each 'X' is Y or N depending on change rqrd
     C*
     C*  This subroutine when called evaluates array OF for XXOPFN and checks
     C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
     C*  replaces 'XXX' with XXMIS.
     C*
     C     CHGOFA        BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C     FOUND         DOWEQ     *OFF
     C     OF(I)         ANDNE     '3'
     C*
     C     OF(I)         IFEQ      XXOPFN
     C*
     C                   MOVEL     OFD(I)        DUMNUM            2
     C*
     C     XXNMBR        IFEQ      DUMNUM
     C*
     C                   MOVEL(P)  OFD(I)        DUMOFD           30
     C                   MOVE      XXMIS         DUMOFD
     C                   MOVEL     DUMOFD        OFD(I)
     C                   MOVE      *ON           FOUND
     C*
     C                   ENDIF                                                  ZZNUM=DUMNUM
     C*
     C                   ENDIF                                                  OF,I=XXOPFN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
     C*
     C                   ENDDO                                                  FOUND DOWEQ *OFF
     C*
     C                   ENDSR                                                  CHGOFA
     C*
     C***  Begin Add  ***  01/22/08  **************************************
     C*================================================================
     C* CHKO06 - Check to see if Opt 6 should be available and turn off
     C*================================================================
     C     CHKO06        BEGSR
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0050R'      ##PGM            10
     C                   PARM                    ##IN0             1
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0051R'      ##PGM            10
     C                   PARM                    ##IN1             1
     C*
     C     ##IN0         IFEQ      *ON
     C     ##IN1         OREQ      *ON
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHKO06
     C***  End   Add  ***  01/22/08  **************************************
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSPTDT        IFNE      *ZEROS
     C     SSEMP#        ORNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSACO         ORNE      *BLANKS
     C     SSREP         ORNE      *BLANKS
     C     SSCTFL        ORNE      *BLANK
     C     SSOSFL        ORNE      *BLANK
     C     SSCXFT        ORNE      *BLANK
     C     SSUDWT        ORNE      *BLANKS
     C     SSRVWR        ORNE      *BLANKS
     C     SSMGR         ORNE      *BLANKS
     C***  BEG   ADD  ***  01/21/09  **********************************
     C     SSASTS        ORNE      *BLANK
     C***  END   ADD  ***  01/21/09  **********************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSPTDT IFNE 0
     C*
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* DSVALS - Set data structure values for views and infos
     C*================================================================
     C     DSVALS        BEGSR
     C*
     C*  Edit stuff for output
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  EMEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1EMP#
     C*
     C                   MOVEL(P)  EMDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1DIV
     C*
     C                   MOVEL(P)  AUFYR         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1FYR
     C*
     C                   MOVE      V1EMP#        V2EMP#
     C                   MOVE      V1EMP#        V3EMP#
     C                   MOVE      V1EMP#        V4EMP#
     C*
     C                   MOVE      V1DIV         V2DIV
     C                   MOVE      V1DIV         V3DIV
     C                   MOVE      V1DIV         V4DIV
     C*
     C                   MOVE      V1FYR         V2FYR
     C*
     C                   MOVE      AUACO         V2ACO
     C                   MOVE      AUACO         V4ACO
     C                   MOVE      AUACO         I1ACO
     C*
     C***  BEG   ADD  ***  01/21/09  **********************************
     C                   MOVE      AUSTAT        XXSTS
     C                   MOVE      AUREP         A1REP
     C                   MOVE      AUSTAT        X1STS
     C***  END   ADD  ***  01/21/09  **********************************
     C*
     C                   MOVE      XXPTDT        V3PTDT
     C                   MOVE      XXPTDT        V4PTDT
     C*
     C                   MOVE      XXRQDT        I1RQDT
     C                   MOVE      XXRQDT        I2RQDT
     C*
     C***  BEG   ADD  ***  01/21/09  **********************************
     C                   MOVE      XXRQDT        I3RQDT
     C***  END   ADD  ***  01/21/09  **********************************
     C*
     C                   ENDSR                                                  DSVALS
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*  Process F10=Search (otherwise process everything else)
     C     *INKJ         IFEQ      *ON
     C                   EXSR      KJDO
     C*
     C                   ELSE
     C*
     C*  Convert policy expiration from date to 8-digit date
     C                   Z-ADD     SSPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSPFDT
     C*
     C*  Convert policy expiration to date to 8-digit date
     C                   Z-ADD     SSPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSPTDT
     C*
     C*  Convert policy expiration from date to CYMD
     C                   Z-ADD     SSPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSPFDT
     C*
     C*  Convert policy expiration to date to CYMD
     C                   Z-ADD     SSPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSPTDT
     C*
     C*
     C                   SELECT
     C*
     C*  Check that from date <= to date
     C     XSPFDT        WHENGT    XSPTDT
     C                   MOVE      *ON           *IN99
     C                   MOVE      *ON           *IN90
     C*
     C*  Check contested only flag = 'Y' or 'N' or ' '
     C     SSCTFL        WHENNE    'Y'
     C     SSCTFL        ANDNE     'N'
     C     SSCTFL        ANDNE     ' '
     C                   MOVE      *ON           *IN98
     C                   MOVE      *ON           *IN90
     C*
     C*  Check outstanding only flag = 'Y' or 'N' or ' '
     C     SSOSFL        WHENNE    'Y'
     C     SSOSFL        ANDNE     'N'
     C     SSOSFL        ANDNE     ' '
     C                   MOVE      *ON           *IN97
     C                   MOVE      *ON           *IN90
     C*
     C*  Check cancelled('C'), full term('F'), or both('B') flag or ' '
     C     SSCXFT        WHENNE    'B'
     C     SSCXFT        ANDNE     'C'
     C     SSCXFT        ANDNE     'F'
     C     SSCXFT        ANDNE     ' '
     C                   MOVE      *ON           *IN96
     C                   MOVE      *ON           *IN90
     C*
     C***  BEG   ADD  ***  12/22/09  **********************************
     C     SSASTS        WHENNE    'A'
     C     SSASTS        ANDNE     'I'
     C     SSASTS        ANDNE     *BLANK
     C                   MOVE      *ON           *IN78
     C                   MOVE      *ON           *IN90
     C***  END   ADD  ***  12/22/09  **********************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C     WCFL43        WHENEQ    'Y'
     C     SSPLAN        ANDNE     *BLANKS
     C     SSPLAN        ANDNE     'P'
     C     SSPLAN        ANDNE     'I'
     C     SSPLAN        ANDNE     'L'
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
      *
     C     WCFL43        WHENNE    'Y'
     C     SSPLAN        ANDNE     *BLANKS
     C     SSPLAN        ANDNE     'P'
     C     SSPLAN        ANDNE     'I'
     C                   MOVE      *ON           *IN80
     C                   MOVE      *ON           *IN90
      *
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*  All error checks passed (turn indicators off)
     C                   OTHER
     C                   MOVE      *OFF          *IN99
     C                   MOVE      *OFF          *IN98
     C                   MOVE      *OFF          *IN97
     C                   MOVE      *OFF          *IN96
     C*
     C                   ENDSL
     C*
     C     *IN90         IFEQ      *OFF                                         no subset error
     C*
     C                   MOVE      *BLANKS       SSFLD
     C*
     C                   SELECT
     C*
     C*    Force view #3 if subsetting on policy expiration date range
     C     XSPTDT        WHENNE    *ZERO
     C                   EXSR      VIEW3
     C                   EXSR      GETHD#
     C                   EXSR      BLDLNS
     C*
     C*    Force view #1 if subsetting on employer/division
     C     SSEMP#        WHENNE    *ZERO
     C                   EXSR      VIEW1
     C                   EXSR      GETHD#
     C                   EXSR      BLDLNS
     C*
     C*    else reset F11=Change view if previously subsetted
     C                   OTHER
     C     *IN65         IFEQ      *ON
     C     *IN66         OREQ      *ON
     C*                Reset F11=Change view
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '11'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C                   EXSR      BLDLNS
     C                   MOVE      *OFF          *IN65
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C*   If not sbstd on pol exp dt or emp#: check subset on following
     C     *IN65         IFEQ      *OFF
     C     *IN66         ANDEQ     *OFF
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZERO
     C                   Z-ADD     SSFYR         KKFYR
     C                   MOVE      'FYR'         SSFLD             3
     C*
     C     SSACO         WHENNE    *BLANKS
     C                   MOVE      SSACO         KKACO
     C                   MOVE      'ACO'         SSFLD
     C*
     C     SSREP         WHENNE    *BLANKS
     C                   MOVE      SSREP         KKREP
     C                   MOVE      'REP'         SSFLD
     C*
     C     SSCTFL        WHENEQ    'Y'
     C                   MOVE      SSCTFL        KKCNFL
     C                   MOVE      'CON'         SSFLD
     C*
     C     SSOSFL        WHENEQ    'Y'
     C                   MOVE      'OUT'         SSFLD
     C*
     C     SSCXFT        WHENEQ    'C'
     C                   MOVE      'Y'           KKCXFL
     C                   MOVE      'CFT'         SSFLD
     C*
     C     SSCXFT        WHENEQ    'F'
     C                   MOVE      'N'           KKCXFL
     C                   MOVE      'CFT'         SSFLD
     C*
     C                   OTHER
     C                   MOVE      *BLANKS       SSFLD
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN65=*OFF
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*  Set main subset dates equal to entry subset dates
     C                   Z-ADD     SSFRDT        SSPFDT
     C                   Z-ADD     XXFRDT        XSPFDT
     C                   Z-ADD     SSTODT        SSPTDT
     C                   Z-ADD     XXTODT        XSPTDT
     C*
     C*  If called from ww/employers - subset on emp#
     C     SAVRRN        IFNE      *ZERO
     C     SAVRRN        CHAIN     PEL02                              60
     C                   Z-ADD     ELEMP#        SSEMP#
     C                   Z-ADD     ELDIV         SSDIV
     C                   Z-ADD     ELFYR         SSFYR
     C                   MOVE      SSEMP#        KKEMP#
     C                   MOVE      SSDIV         KKDIV
     C                   MOVE      SSFYR         KKFYR
     C*
     C                   ELSE
     C                   CLEAR                   SSEMP#
     C                   CLEAR                   SSDIV
     C                   CLEAR                   SSFYR
     C                   ENDIF
     C*
     C                   CLEAR                   SSACO
     C                   CLEAR                   SSREP
     C                   CLEAR                   SSCTFL
     C                   CLEAR                   SSOSFL
     C                   CLEAR                   SSCXFT
     C                   CLEAR                   SSUDWT
     C                   CLEAR                   SSRVWR
     C                   CLEAR                   SSMGR
     C                   CLEAR                   SSFLD
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C     'WC'          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL43        IFEQ      'Y'
     C     WCFL11        OREQ      'Y'
     C                   SETON                                        31
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*  Get fund record (if not called from main menu)
     C     Q1PRMS        IFGE      5
     C     @@RRN         ANDNE     *ZERO
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   MOVE      FNCO#         KKCO#
     C                   MOVE      FNFND         KKFND
     C*
     C                   Z-ADD     @@RRN2        SAVRRN
     C*
     C*  Disable 'F13=Chg user parms' if not called from main menu
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '13'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
     C                   ELSE
     C*
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCFNAM
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      ##FND         KKFND
     C*
     C                   Z-ADD     *ZERO         SAVRRN
     C*
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C*  Initialize keys based on values in subset or not
     C     *IN11         IFEQ      *OFF
     C*
     C                   MOVE      *LOVAL        KKACO
     C                   MOVE      *LOVAL        KKPTDT
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C                   MOVE      *LOVAL        KKFYR
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XSPFDT        KKPTDT
     C                   MOVE      SSACO         KKACO
     C                   Z-ADD     SSFYR         KKFYR
     C                   Z-ADD     SSEMP#        KKEMP#
     C                   Z-ADD     SSDIV         KKDIV
     C*
     C                   ENDIF                                                  *IN11=*OFF
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      *LOVAL        KKACO
     C                   MOVE      *LOVAL        KKPTDT
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C                   MOVE      *LOVAL        KKFYR
     C*
     C     SSFLD         IFEQ      'FYR'
     C                   Z-ADD     SSFYR         KKFYR
     C                   ELSE
     C     SSFLD         IFEQ      'ACO'
     C                   MOVE      SSACO         KKACO
     C                   ENDIF                                                  SSFLD='ACO'
     C                   ENDIF                                                  SSFLD='FYR'
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON                                          VIEW#1
     C                   MOVE      SCPST1        KKEMP#
     C                   MOVE      SCPS1A        KKDIV
     C*
     C     *IN22         WHENEQ    *ON                                          VIEW#2
     C                   MOVE      SCPST2        KKACO
     C                   MOVE      SCPS2A        KKEMP#
     C                   MOVE      SCPS2B        KKDIV
     C*
     C     *IN23         WHENEQ    *ON                                          VIEW#3
     C                   MOVE      SCPST3        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPTDT
     C*
     C     *IN24         WHENEQ    *ON                                          VIEW#4
     C                   MOVE      SCPST4        KKACO
     C                   MOVE      SCPS4A        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPTDT
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C                   MOVE      SCFYR         KKFYR
     C                   MOVEL     SCACO         KKACO
     C                   MOVE      SCPTDT        KKPTDT
     C*
     C                   ENDSR                                                  KEYSFL
     C*===============================================================
     C* KJDO - Subroutine for F10=Search
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       60
     C*
     C                   SELECT
     C*
     C*  Employer/division search
     C     HFLD          WHENEQ    'SSEMP#'
     C     HFLD          OREQ      'SSDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      EMEMP#        SSEMP#
     C                   MOVE      EMDIV         SSDIV
     C                   SETON                                        71
     C                   ELSE
     C                   SETON                                        76
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  ##RRN<>0
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C* Audit company search
     C     HFLD          WHENEQ    'SSACO'
     C*
     C                   CALL      'W2290R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      0             ##RRN             9 0
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PAC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      ACKEY         SSACO
     C                   SETON                                        62
     C                   ELSE
     C                   SETON                                        67
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  ##RRN IFNE 0
     C*
     C                   ENDIF                                                  *INKL IFEQ *OFF
     C*
     C*  Audit rep search
     C     HFLD          WHENEQ    'SSREP'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      UPSTAF        SSREP
     C                   SETON                                        63
     C                   ELSE
     C                   SETON                                        62
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ENDIF                                                  ##RRN IFNE 0
     C*
     C*  Underwriter search
     C     HFLD          WHENEQ    'SSUDWT'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      0             ##RRN
     C                   PARM      3             ##VIEW
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     UPSTAF        SSUDWT
     C                   SETON                                        64
     C                   ELSE
     C                   SETON                                        68
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C*  Audit reviewer search
     C     HFLD          WHENEQ    'SSRVWR'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      0             ##RRN
     C                   PARM      3             ##VIEW
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     UPSTAF        SSRVWR
     C                   SETON                                        75
     C                   ELSE
     C                   SETON                                        64
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C*  Audit manager search
     C     HFLD          WHENEQ    'SSMGR'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      0             ##RRN
     C                   PARM      3             ##VIEW
     C*
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     UPSTAF        SSMGR
     C                   SETON                                        69
     C                   ELSE
     C                   SETON                                        75
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C                   ENDSL                                                  HFLD WHEQ 'SCACO'
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* KSDO - Process F18: audit report
     C*================================================================
     C     KSDO          BEGSR
     C*
     C                   EXSR      $SCR04
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  After <enter> is pressed in above window, call W4700C: this
     C*  will generate a report detail/summary for all audit tracking
     C*  records within the requested date range.
     C*
     C                   MOVE      AUCO#         QQCO#
     C                   MOVE      AUFND         QQFND
     C                   MOVE      XXRNDT        QQRNDT
     C                   MOVE      XXFRDT        QQFRDT
     C*
     C     XXTODT        IFEQ      *ZERO
     C                   MOVE      *HIVAL        XXTODT
     C                   ENDIF
     C*
     C                   MOVE      XXTODT        QQTODT
     C                   MOVE      ##OUTQ        QQQUE
     C                   MOVE      'D'           QQMODE
     C*
     C                   CALL      'W4700C'      RPTPL
     C*
     C                   CALL      'WA007R'
     C*
     C                   ENDIF                                                  *INKJ = *OFF
     C*
     C                   ENDSR                                                  KSDO
     C*================================================================
     C* KVDO - Process F21: view report
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   MOVE      AUCO#         QQCO#
     C                   MOVE      AUFND         QQFND
     C*
     C*  This will display on screen the information on the last
     C*  Audit Detail Report and the date it was run
     C*
     C                   CALL      'W4700R21'
     C     *INKC         PARM                    QQKC              1
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C*
     C                   ENDSR                                                  KVDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      ***** BEG ADD ******* 09/19/03 **********************************
     C                   CLEAR                   SCDATA
     C                   MOVE      AUEMP#        SCEMP#
     C                   MOVE      AUDIV         SCDIV
     C                   MOVE      AUFYR         SCFYR
     C                   MOVEL     AUACO         SCACO
     C                   MOVE      AUPTDT        SCPTDT
     C                   CLEAR                   SCDBR2
      ***** END ADD ******* 09/19/03 **********************************
     C***  BEGIN ADD  ***  12/03/10  **********************************
     C                   MOVE      'Y'           VLDOPT
     C***  END   ADD  ***  12/03/10  **********************************
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C     VLDOPT        IFNE      'Y'
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C     SSFLD         IFNE      *BLANKS
     C                   SELECT
     C*
     C     SSFLD         WHENEQ    'FYR'
     C   21              MOVE      DBRRB1        SCDBRR
     C   22              MOVE      DBRRB2        SCDBRR
     C   23              MOVE      DBRRB3        SCDBRR
     C   24              MOVE      DBRRB4        SCDBRR
     C*
     C     SSFLD         WHENEQ    'ACO'
     C   21              MOVE      DBRRN2        SCDBRR
     C   22              MOVE      DBRRN2        SCDBRR
     C   23              MOVE      DBRRN4        SCDBRR
     C   24              MOVE      DBRRN4        SCDBRR
     C*
     C     SSFLD         WHENEQ    'REP'
     C   21              MOVE      DBRRD1        SCDBRR
     C   22              MOVE      DBRRD2        SCDBRR
     C   23              MOVE      DBRRD3        SCDBRR
     C   24              MOVE      DBRRD4        SCDBRR
     C*
     C     SSFLD         WHENEQ    'CON'
     C   21              MOVE      DBRRE1        SCDBRR
     C   22              MOVE      DBRRE2        SCDBRR
     C   23              MOVE      DBRRE3        SCDBRR
     C   24              MOVE      DBRRE4        SCDBRR
     C*
     C     SSFLD         WHENEQ    'OUT'
     C   21              MOVE      DBRRG1        SCDBRR
     C   22              MOVE      DBRRG2        SCDBRR
     C   23              MOVE      DBRRG3        SCDBRR
     C   24              MOVE      DBRRG4        SCDBRR
     C*
     C     SSFLD         WHENEQ    'CFT'
     C   21              MOVE      DBRRH1        SCDBRR
     C   22              MOVE      DBRRH2        SCDBRR
     C   23              MOVE      DBRRH3        SCDBRR
     C   24              MOVE      DBRRH4        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SSFLD<>*BLANKS
     C*
     C***  BEGIN ADD  ***  11/05/98  **********************************
     C***  BEGIN DEL  ***  08/25/99  **********************************
     C*******************  ELSE
     C*******************  MOVE 'N'       VLDOPT
     C***  END   DEL  ***  08/25/99  **********************************
     C                   ENDIF                                                  VLDOPT<>'Y'
     C***  END   ADD  ***  11/05/98  **********************************
     C*
     C                   MOVE      AUACO         KKACO
     C                   MOVE      AUPTDT        KKPTDT
     C                   MOVE      AUEMP#        KKEMP#
     C                   MOVE      AUDIV         KKDIV
     C                   MOVE      AUFYR         KKFYR
     C*  Get employer master record
      *** BEG CHG ***** 04/03/07 **************************************
     C*********  EMKY      CHAINPEM01                60
     C     EMKY          SETLL     PEM01
     C     EMKY          READE     PEM01                                  60
      *** END CHG ***** 04/03/07 **************************************
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EMNAME        XXNAME
     C*
     C***  BEG   ADD  ***  01/21/09  **********************************
     C                   MOVEL     EMNAME        X1NAME
     C***  END   ADD  ***  01/21/09  **********************************
     C*
     C     ELKY          CHAIN     PEL01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
     C*
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date exists, set canx flag
     C     XI            IFEQ      *ZERO
     C                   MOVE      'N'           XXCXFL
     C***  BEG   ADD  ***  01/21/09  **********************************
     C                   MOVE      'N'           X1CXFL
     C***  END   ADD  ***  01/21/09  **********************************
     C                   ELSE
     C     CX(XI)        IFEQ      *ZEROS
     C     CX(XI)        ORGT      XXRNDT
     C                   MOVE      'N'           XXCXFL
     C***  BEG   ADD  ***  01/21/09  **********************************
     C                   MOVE      'N'           X1CXFL
     C***  END   ADD  ***  01/21/09  **********************************
     C                   ELSE
     C                   MOVE      'Y'           XXCXFL
     C***  BEG   ADD  ***  01/21/09  **********************************
     C                   MOVE      'Y'           X1CXFL
     C***  END   ADD  ***  01/21/09  **********************************
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
     C*
     C*  Set contested flag
     C     AUCNTD        IFEQ      *ZEROS
     C***  BEGIN ADD  ***  03/02/99  **********************************
     C     AUCNFL        ANDNE     'Y'
     C***  END   ADD  ***  03/02/99  **********************************
     C                   MOVE      'N'           XXCTST
     C                   ELSE
     C                   MOVE      'Y'           XXCTST
     C                   ENDIF                                                  AUCNTD=*BLANKS
     C*
     C*  Format six digit date fields
     C*
     C                   MOVEL     'DATE6'       EDTMSK
     C*
     C*                Audit request date
     C                   Z-ADD     AURQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZRQDT            6 0
     C*
     C                   MOVEL     ZZRQDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRQDT            8
     C*
     C*                Audit received date
     C                   Z-ADD     AURCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZRCVD            6 0
     C*
     C                   MOVEL     ZZRCVD        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRCVD
     C*
     C*                Audit attempt date
     C                   Z-ADD     AUATDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZATDT            6 0
     C*
     C                   MOVEL     ZZATDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXATDT
     C*
     C*                Audit print date
     C                   Z-ADD     AUPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZPRDT            6 0
     C*
     C                   MOVEL     ZZPRDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXPRDT
     C*
     C*                Audit mail date
     C                   Z-ADD     AUMAIL        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZMAIL            6 0
     C*
     C                   MOVEL     ZZMAIL        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXMAIL
     C*
     C*                Audit policy to date
     C                   Z-ADD     AUPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZPTDT            6 0
     C*
     C                   MOVEL     ZZPTDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXPTDT            8
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      AUEMP#        SCEMP#
     C                   MOVE      AUDIV         SCDIV
     C                   MOVE      AUFYR         SCFYR
     C                   MOVEL     AUACO         SCACO
     C                   MOVE      AUPTDT        SCPTDT
     C                   MOVE      EMRRN         SCDBR2
     C*
     C*  Initialize data structure values
     C                   EXSR      DSVALS
     C*
     C*  Load fields according to VIEW# and INFO#
     C                   SELECT
     C*
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  XXVW1         SCDATA
     C                   MOVE      XXINF1        SCDATA
     C                   MOVEL     HDA(1)        SCHDLA
     C*
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  XXVW1         SCDATA
     C                   MOVE      XXINF2        SCDATA
     C                   MOVEL     HDA(2)        SCHDLA
     C*
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     3
     C                   MOVEL(P)  XXVW1         SCDATA
     C                   MOVE      XXINF3        SCDATA
     C                   MOVEL     HDA(3)        SCHDLA
     C*
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  XXVW2         SCDATA
     C                   MOVE      XXINF1        SCDATA
     C                   MOVEL     HDA(4)        SCHDLA
     C*
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  XXVW2         SCDATA
     C                   MOVE      XXINF2        SCDATA
     C                   MOVEL     HDA(5)        SCHDLA
     C*
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     3
     C                   MOVEL(P)  XXVW2         SCDATA
     C                   MOVE      XXINF3        SCDATA
     C                   MOVEL     HDA(6)        SCHDLA
     C*
     C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  XXVW3         SCDATA
     C                   MOVE      XXINF1        SCDATA
     C                   MOVEL     HDA(7)        SCHDLA
     C*
     C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  XXVW3         SCDATA
     C                   MOVE      XXINF2        SCDATA
     C                   MOVEL     HDA(8)        SCHDLA
     C*
     C     VIEW#         WHENEQ    3
     C     INFO#         ANDEQ     3
     C                   MOVEL(P)  XXVW3         SCDATA
     C                   MOVE      XXINF3        SCDATA
     C                   MOVEL     HDA(9)        SCHDLA
     C*
     C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  XXVW4         SCDATA
      ***** BEG CHG ***** 01/21/09 ***************************************
     C*************        MOVE XXINF1    SCDATA
     C                   MOVE      X1INF1        SCDATA
      ***** END CHG ***** 01/21/09 ***************************************
     C                   MOVEL     HDA(10)       SCHDLA
     C*
     C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  XXVW4         SCDATA
     C                   MOVE      XXINF2        SCDATA
     C                   MOVEL     HDA(11)       SCHDLA
     C*
     C     VIEW#         WHENEQ    4
     C     INFO#         ANDEQ     3
     C                   MOVEL(P)  XXVW4         SCDATA
     C                   MOVE      XXINF3        SCDATA
     C                   MOVEL     HDA(12)       SCHDLA
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   ENDIF                                                  *IN60=*0FF
     C*
     C                   ENDSR                                                  MVDBSC
     C*
      ***** BEG ADD ***** 09/26/06 ***************************************
     C*================================================================
     C* OPT06 - Process option 06 (W/w Employers)
     C*================================================================
     C     OPT06         BEGSR
     C*
     C***  Begin Del  ***  01/22/08  **************************************
      ******BEG*ADD ***** 03/30/07 ***************************************
     C**********           MOVE *OFF      *IN30
     C********** XXRRN     IFEQ *ZEROS
     C********** XXRRN2    ANDEQ*ZEROS
      ******END*ADD ***** 03/30/07 ***************************************
     C***  End   Del  ***  01/22/08  **************************************
     C*
      *
      ***** BEG ADD ***** 04/10/07 ************************************
     C     CLTID         IFEQ      '132'
      ***** BEG CHG ***** 05/02/07 ************************************
     C***************      MOVE ' '       ##MODE  1
     C                   MOVE      'M'           ##MODE            1
      ***** END CHG ***** 05/02/07 ************************************
     C                   ELSE
     C                   MOVE      'Z'           ##MODE
     C                   ENDIF
      ***** END ADD ***** 04/10/07 ************************************
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
      ***** BEG CHG ***** 04/10/07 ************************************
     C****************     PARM 'Z'       ##MODE  1
     C                   PARM                    ##MODE
      ***** END CHG ***** 04/10/07 ************************************
     C                   PARM      0             ##RRN             9 0
     C                   PARM      0             ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM      SCDBR2        ##RRN3            9 0
     C***  Begin Del  ***  01/22/08  **************************************
      ******BEG*ADD ***** 03/30/07 ***************************************
     C**********           ELSE
     C**********           MOVE *ON       *IN30
     C**********           ENDIF
      ******END*ADD ***** 03/30/07 ***************************************
     C***  End   Del  ***  01/22/08  **************************************
     C*
     C                   ENDSR                                                  OPT06
      ***** END ADD ***** 09/26/06 ***************************************
     C*================================================================
     C* OPT14 - Process option 14
     C*================================================================
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C                   MOVE      *BLANKS       QCQTE
     C*
     C*  CALL COMMENT PROGRAM
     C*
     C                   MOVEL     'W0050R'      PGMNAM           10
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
      *** Begin Change 10/18/02 ***************************************
     C**********           PARM           Q1PGM
     C                   PARM                    PGMNAM
      *** End   Change 10/18/02 ***************************************
     C*
     C                   ENDSR                                                  OPT14
     C*================================================================
     C* OPT21 - Process option 21 (Audit received)
     C*================================================================
     C     OPT21         BEGSR
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT21
     C*================================================================
     C* OPT22 - Process option 22 (Audit attempt)
     C*================================================================
     C     OPT22         BEGSR
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT22
     C*================================================================
     C* OPT23 - Process option 23 (Final audit mailed)
     C*================================================================
     C     OPT23         BEGSR
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C     AUPRDT        IFNE      *ZERO
     C                   CALL      ACDPGM        ACDPL
     C                   ELSE
     C                   EXFMT     SCR05
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT23
     C*================================================================
     C* OPT24 - Process option 24 (Contested)
     C*================================================================
     C     OPT24         BEGSR
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT24
     C*================================================================
     C* OPT25 - Process option 25 (Clear contested)
     C*================================================================
     C     OPT25         BEGSR
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C     AUCNTD        IFNE      *ZERO
     C***  BEGIN ADD  ***  03/02/99  **********************************
     C     AUCNFL        OREQ      'Y'
     C***  END   ADD  ***  03/02/99  **********************************
     C                   CALL      ACDPGM        ACDPL
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT25
     C*================================================================
     C* OPT26 - Process option 26 (Reassign audit rep)
     C*================================================================
     C     OPT26         BEGSR
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL
     C*
     C                   ENDSR                                                  OPT26
      **** BEGIN ADD **** 02/18/00 ************************************
     C*===============================================================
     C* OPT60 - SUBROUTINE FOR OPTION 60
     C*===============================================================
     C*
     C* OPTION 60=A/R INQUIRY
     C*
     C     OPT60         BEGSR
     C*
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCFYR         QQFYR
     C*
     C                   CALL      'W0612R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##MODE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM      *ZEROS        QQDATE            8
     C                   PARM      *ZEROS        QQAM              9
     C                   PARM      *ZEROS        QQCHK#            7
     C                   PARM                    QQRTC             1
     C*
     C                   ENDSR                                                  OPT60
     C*
     C*===============================================================
     C* OPT61 - SUBROUTINE FOR OPTION 61
     C*===============================================================
     C*
     C* OPTION 61=A/R DETAIL SUMMARY
     C*
     C     OPT61         BEGSR
     C*
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCFYR         QQFYR
     C*
     C                   CALL      'W0610R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##MODE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQRTC
     C*
     C                   ENDSR                                                  OPT61
     C*
      **** END   ADD **** 02/18/00 ************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKS         CASEQ     *ON           KSDO                           <F18>
     C     *INKV         CASEQ     *ON           KVDO                           <F21>
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
      ***** BEG ADD ***** 09/26/06 ***************************************
     C     SCSEL         CASEQ     '6'           OPT06
      ***** END ADD ***** 09/26/06 ***************************************
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '21'          OPT21
     C     SCSEL         CASEQ     '22'          OPT22
     C     SCSEL         CASEQ     '23'          OPT23
     C     SCSEL         CASEQ     '24'          OPT24
     C     SCSEL         CASEQ     '25'          OPT25
     C     SCSEL         CASEQ     '26'          OPT26
      **** BEGIN ADD **** 02/18/00 ************************************
     C     SCSEL         CASEQ     '60'          OPT60
     C     SCSEL         CASEQ     '61'          OPT61
      **** END   ADD **** 02/18/00 ************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C***  BEGIN ADD  ***  09/21/98  **********************************
     C                   MOVE      *ON           *INLR
     C***  END   ADD  ***  09/21/98  **********************************
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      ***** BEG ADD ***** 04/10/07 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ***** 04/10/07 ************************************
      *
     C***  Begin Chg  ***  01/22/08  **************************************
      ******BEG*ADD ***** 03/30/07 ************************************
      **********
      ******If*not called from WW Employers then allow option 06, else
      **Display*Error msg
      **********
     C********** SAVRRN    IFEQ *ZERO
     C********** *INKM     ANDNE*ON
     C**********           Z-ADD*ZEROS    XXRRN   90
     C**********           Z-ADD*ZEROS    XXRRN2  90
     C**********           ELSE
     C**********           Z-ADD@@RRN     XXRRN
     C**********           Z-ADD@@RRN2    XXRRN2
     C**********           ENDIF
      ******END*ADD ***** 03/30/07 ************************************
     C                   EXSR      CHKO06
     C***  End   Chg  ***  01/22/08  **************************************
     C*
     C*  Output subset screen if not called from WWEmployers nor
     C*  returning from F13=Change user parms
     C     SAVRRN        IFEQ      *ZERO
     C     *INKM         ANDNE     *ON
     C*
     C*
     C                   EXSR      $SCR04
     C*
     C     *INKL         IFEQ      *ON
     C                   RETURN
     C                   ENDIF
     C*
     C*  Force view #3 if subsetting on policy expiration date range
     C*    else reset F11=Change view if previously subsetted
     C     XXTODT        IFNE      *ZERO
     C*
     C                   EXSR      VIEW3
     C*
     C                   ELSE
     C*
     C     *IN65         IFEQ      *ON
      ***** BEG DEL ***** 07/08/04 ************************************
     C**************       MOVEAF11FKY    OFD,16
      ***** END DEL ***** 07/08/04 ************************************
     C                   MOVE      *OFF          *IN65
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXTODT=*ZERO
     C*
     C                   Z-ADD     XXFRDT        KKPTDT
     C*
     C                   ENDIF                                                  SAVRRN=*ZERO
     C*
     C*  Disable F17=Subset function when called from ww/employers
     C     SAVRRN        IFNE      *ZERO
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*                Force view 1
     C                   EXSR      VIEW1
     C                   ENDIF
     C*
     C*  Get run date
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI = %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C***  BEGIN CHG  ***  08/25/99  **********************************
     C*******************  MOVE *OFF      VLDOPT
     C                   MOVE      'N'           VLDOPT
     C***  END   CHG  ***  08/25/99  **********************************
     C***** END ADD ****** 03/30/99 ***********************************
     C*
     C*  Read file based on subset values
     C*
     C                   SELECT
     C*
     C*    Subset on policy exp date range
     C     *IN65         WHENEQ    *ON
     C   23AUKY          READE     DBRCDF3                                95
     C     AUPTDT        IFGT      XSPTDT
     C                   MOVE      *ON           *IN95
     C                   ENDIF
     C*
     C*    Subset on emp#/div
     C     *IN66         WHENEQ    *ON
     C   21AUKYA         READE     DBRCDF                                 95
     C*
     C*    Subset on fyr
     C***** BEGIN DEL **** 01/21/09 ***********************************
     C********** SSFLD     WHEQ 'FYR'
     C**********   21      AUKYB     READEDBRCDB1                  95
     C**********   22      AUKYB     READEDBRCDB2                  95
     C**********   23      AUKYB     READEDBRCDB3                  95
     C**********   24      AUKYB     READEDBRCDB4                  95
     C*
     C*    Subset on audit co
     C********** SSFLD     WHEQ 'ACO'
     C**********   21      AUKYC     READEDBRCDF2                  95
     C**********   22      AUKYC     READEDBRCDF2                  95
     C**********   23      AUKYC     READEDBRCDF4                  95
     C**********   24      AUKYC     READEDBRCDF4                  95
     C*
     C*    Subset on audit rep
     C********** SSFLD     WHEQ 'REP'
     C**********   21      AUKYD     READEDBRCDD1                  95
     C**********   22      AUKYD     READEDBRCDD2                  95
     C**********   23      AUKYD     READEDBRCDD3                  95
     C**********   24      AUKYD     READEDBRCDD4                  95
     C*
     C*    Subset on contested audits only
     C********** SSFLD     WHEQ 'CON'
     C**********   21      AUKYE     READEDBRCDE1                  95
     C**********   22      AUKYE     READEDBRCDE2                  95
     C**********   23      AUKYE     READEDBRCDE3                  95
     C**********   24      AUKYE     READEDBRCDE4                  95
     C*
     C*    Subset on outstanding audits only
     C********** SSFLD     WHEQ 'OUT'
     C**********   21      AUKY      READEDBRCDG1                  95
     C**********   22      AUKY      READEDBRCDG2                  95
     C**********   23      AUKY      READEDBRCDG3                  95
     C**********   24      AUKY      READEDBRCDG4                  95
     C*
     C*    Subset on cancelled/full term
     C********** SSFLD     WHEQ 'CFT'
     C**********   21      AUKYH     READEDBRCDH1                  95
     C**********   22      AUKYH     READEDBRCDH2                  95
     C**********   23      AUKYH     READEDBRCDH3                  95
     C**********   24      AUKYH     READEDBRCDH4                  95
     C***** END   DEL **** 01/21/09 ***********************************
     C*
     C                   OTHER
     C*
     C   21AUKY          READE     DBRCDF                                 95
     C   22AUKY          READE     DBRCDF2                                95
     C   23AUKY          READE     DBRCDF3                                95
     C   24AUKY          READE     DBRCDF4                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C***  BEGIN CHG  ***  08/25/99  **********************************
     C*******************  MOVE *OFF      VLDOPT
     C                   MOVE      'N'           VLDOPT
     C***  END   CHG  ***  08/25/99  **********************************
     C***** END ADD ****** 03/30/99 ***********************************
     C*
     C                   SELECT
     C*
     C*    Subset on policy exp date range
     C     *IN65         WHENEQ    *ON
     C   23AUKY          READPE    DBRCDF3                                95
     C     AUPTDT        IFLT      XSPFDT
     C                   MOVE      *ON           *IN95
     C                   ENDIF
     C*
     C*    Subset on emp#/div
     C     *IN66         WHENEQ    *ON
     C   21AUKYA         READPE    DBRCDF                                 95
     C*
     C*    Subset on fyr
     C***** BEGIN DEL **** 01/21/09 ***********************************
     C********** SSFLD     WHEQ 'FYR'
     C**********   21      AUKYB     REDPEDBRCDB1                  95
     C**********   22      AUKYB     REDPEDBRCDB2                  95
     C**********   23      AUKYB     REDPEDBRCDB3                  95
     C**********   24      AUKYB     REDPEDBRCDB4                  95
     C*
     C*    Subset on audit co
     C********** SSFLD     WHEQ 'ACO'
     C**********   21      AUKYC     REDPEDBRCDF2                  95
     C**********   22      AUKYC     REDPEDBRCDF2                  95
     C**********   23      AUKYC     REDPEDBRCDF4                  95
     C**********   24      AUKYC     REDPEDBRCDF4                  95
     C*
     C*    Subset on audit rep
     C********** SSFLD     WHEQ 'REP'
     C**********   21      AUKYD     REDPEDBRCDD1                  95
     C**********   22      AUKYD     REDPEDBRCDD2                  95
     C**********   23      AUKYD     REDPEDBRCDD3                  95
     C**********   24      AUKYD     REDPEDBRCDD4                  95
     C*
     C*    Subset on contested audits only
     C********** SSFLD     WHEQ 'CON'
     C**********   21      AUKYE     REDPEDBRCDE1                  95
     C**********   22      AUKYE     REDPEDBRCDE2                  95
     C**********   23      AUKYE     REDPEDBRCDE3                  95
     C**********   24      AUKYE     REDPEDBRCDE4                  95
     C*
     C*    Subset on outstanding audits only
     C********** SSFLD     WHEQ 'OUT'
     C**********   21      AUKY      REDPEDBRCDG1                  95
     C**********   22      AUKY      REDPEDBRCDG2                  95
     C**********   23      AUKY      REDPEDBRCDG3                  95
     C**********   24      AUKY      REDPEDBRCDG4                  95
     C*
     C*    Subset on cancelled/full term
     C********** SSFLD     WHEQ 'CFT'
     C**********   21      AUKYH     REDPEDBRCDH1                  95
     C**********   22      AUKYH     REDPEDBRCDH2                  95
     C**********   23      AUKYH     REDPEDBRCDH3                  95
     C**********   24      AUKYH     REDPEDBRCDH4                  95
     C***** END   DEL **** 01/21/09 ***********************************
     C*
     C                   OTHER
     C*
     C   21AUKY          READPE    DBRCDF                                 95
     C   22AUKY          READPE    DBRCDF2                                95
     C   23AUKY          READPE    DBRCDF3                                95
     C   24AUKY          READPE    DBRCDF4                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C***** BEG   CHG **** 01/21/09 ***********************************
     C*********            SELEC
     C*
     C*********  SSFLD     WHEQ 'FYR'
     C*********   21      KEY1B     SETGTDBRCDB1              96
     C*********   22      KEY2B     SETGTDBRCDB2              96
     C*********   23      KEY3B     SETGTDBRCDB3              96
     C*********   24      KEY4B     SETGTDBRCDB4              96
     C*
     C*********           SSFLD     WHEQ 'ACO'
     C*********   21      KEY2      SETGTDBRCDF2              96
     C*********   22      KEY2      SETGTDBRCDF2              96
     C*********   23      KEY4      SETGTDBRCDF4              96
     C*********   24      KEY4      SETGTDBRCDF4              96
     C*
     C*********  SSFLD     WHEQ 'REP'
     C*********   21      KEY1D     SETGTDBRCDD1              96
     C*********   22      KEY2D     SETGTDBRCDD2              96
     C*********   23      KEY3D     SETGTDBRCDD3              96
     C*********   24      KEY4D     SETGTDBRCDD4              96
     C*
     C********* SSFLD     WHEQ 'CON'
     C*********   21      KEY1E     SETGTDBRCDE1              96
     C*********   22      KEY2E     SETGTDBRCDE2              96
     C*********   23      KEY3E     SETGTDBRCDE3              96
     C*********   24      KEY4E     SETGTDBRCDE4              96
     C*
     C********* SSFLD     WHEQ 'OUT'
     C*********   21      KEY1      SETGTDBRCDG1              96
     C*********   22      KEY2      SETGTDBRCDG2              96
     C*********   23      KEY3      SETGTDBRCDG3              96
     C*********   24      KEY4      SETGTDBRCDG4              96
     C*
     C********* SSFLD     WHEQ 'CFT'
     C*********   21      KEY1H     SETGTDBRCDH1              96
     C*********   22      KEY2H     SETGTDBRCDH2              96
     C*********   23      KEY3H     SETGTDBRCDH3              96
     C*********   24      KEY4H     SETGTDBRCDH4              96
     C*
     C*********           OTHER
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C*
     C*********            ENDSL
     C***** END   CHG **** 01/21/09 ***********************************
     C*
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C***** BEG   CHG **** 01/21/09 ***********************************
     C*********            SELEC
     C*
     C*********  SSFLD     WHEQ 'FYR'
     C*********   21      KEY1B     SETLLDBRCDB1              96
     C*********   22      KEY2B     SETLLDBRCDB2              96
     C*********   23      KEY3B     SETLLDBRCDB3              96
     C*********   24      KEY4B     SETLLDBRCDB4              96
     C*
     C*********  SSFLD     WHEQ 'ACO'
     C*********   21      KEY2      SETLLDBRCDF2              96
     C*********   22      KEY2      SETLLDBRCDF2              96
     C*********   23      KEY4      SETLLDBRCDF4              96
     C*********   24      KEY4      SETLLDBRCDF4              96
     C*
     C*********  SSFLD     WHEQ 'REP'
     C*********   21      KEY1D     SETLLDBRCDD1              96
     C*********   22      KEY2D     SETLLDBRCDD2              96
     C*********   23      KEY3D     SETLLDBRCDD3              96
     C*********   24      KEY4D     SETLLDBRCDD4              96
     C*
     C*********  SSFLD     WHEQ 'CON'
     C*********   21      KEY1E     SETLLDBRCDE1              96
     C*********   22      KEY2E     SETLLDBRCDE2              96
     C*********   23      KEY3E     SETLLDBRCDE3              96
     C*********   24      KEY4E     SETLLDBRCDE4              96
     C*
     C********* SSFLD     WHEQ 'OUT'
     C*********   21      KEY1      SETLLDBRCDG1              96
     C*********   22      KEY2      SETLLDBRCDG2              96
     C*********   23      KEY3      SETLLDBRCDG3              96
     C*********   24      KEY4      SETLLDBRCDG4              96
     C*
     C********* SSFLD     WHEQ 'CFT'
     C*********   21      KEY1H     SETLLDBRCDH1              96
     C*********   22      KEY2H     SETLLDBRCDH2              96
     C*********   23      KEY3H     SETLLDBRCDH3              96
     C*********   24      KEY4H     SETLLDBRCDH4              96
     C*
     C*********            OTHER
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
     C*
     C*********            ENDSL
     C***** END   CHG **** 01/21/09 ***********************************
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*  Convert subset dates for comparison
     C*
     C                   Z-ADD     SSPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSPFDT
     C*
     C                   Z-ADD     SSPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSPTDT
     C*
     C*  The following code checks if any of the subset conditions
     C*  are not met by the current record.
     C*
      *** BEG DEL ***** 04/03/07 *****************************************
     C**********           SELEC
     C**********
     C*****Check employer expiration date within date range
     C**********
     C********** XSPTDT    WHNE *ZEROS
     C********** AUPTDT    ANDGTXSPTDT
     C**********           MOVE *OFF      MEETSS
     C**********
     C********** XSPTDT    WHNE *ZEROS
     C********** AUPTDT    ANDLTXSPFDT
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check emp# vs subset emp#
     C**********
     C********** SSEMP#    WHNE *ZEROS
     C********** AUEMP#    ANDNESSEMP#
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check employer division vs subset division
     C**********
     C********** SSEMP#    WHNE *ZEROS
     C********** AUDIV     ANDNESSDIV
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check fund year vs subset fund year
     C**********
     C********** SSFYR     WHNE *ZEROS
     C********** AUFYR     ANDNESSFYR
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check audit co# vs subset audit co#
     C**********
     C********** SSACO     WHNE *BLANKS
     C********** AUACO     ANDNESSACO
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check audit rep# vs subset audit rep
     C**********
     C********** SSREP     WHNE *BLANKS
     C********** AUREP     ANDNESSREP
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****If*'contested only' flag = Y, check contested flag is 'Y'
     C**********
     C********** SSCTFL    WHEQ 'Y'
     C********** AUCNFL    ANDNE'Y'
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****If*'outstanding only' = 'Y', check audit is outstanding
     C********** SSOSFL    WHEQ 'Y'
     C********** AURQDT    ANDLEAURCVD
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check vs. Cancelled, Full term, both
     C********** SSCXFT    WHEQ 'F'
     C********** AUCXFL    ANDEQ'Y'
     C**********           MOVE *OFF      MEETSS
     C**********
     C********** SSCXFT    WHEQ 'C'
     C********** AUCXFL    ANDEQ'N'
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check underwriter vs subset underwriter
     C**********
     C********** SSUDWT    WHNE *BLANKS
     C********** EMUWTR    ANDNESSUDWT
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check reviewer vs subset reviewer
     C**********
     C********** SSRVWR    WHNE *BLANKS
     C********** EMARVW    ANDNESSRVWR
     C**********           MOVE *OFF      MEETSS
     C**********
     C*****Check manager vs subset manager
     C**********
     C********** SSMGR     WHNE *BLANKS
     C********** EMAMGR    ANDNESSMGR
     C**********           MOVE *OFF      MEETSS
     C**********
     C**********           ENDSL
      *** END DEL ***** 04/03/07 *****************************************
     C*
      *** BEG ADD ***** 04/03/07 *****************************************
     C*
     C*    Check employer expiration date within date range
     C*
     C     XSPTDT        IFNE      *ZEROS
     C     AUPTDT        ANDGT     XSPTDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     XSPTDT        IFNE      *ZEROS
     C     AUPTDT        ANDLT     XSPFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check emp# vs subset emp#
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     AUEMP#        ANDNE     SSEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check employer division vs subset division
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     AUDIV         ANDNE     SSDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check fund year vs subset fund year
     C*
     C     SSFYR         IFNE      *ZEROS
     C     AUFYR         ANDNE     SSFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check audit co# vs subset audit co#
     C*
     C     SSACO         IFNE      *BLANKS
     C     AUACO         ANDNE     SSACO
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check audit rep# vs subset audit rep
     C*
     C     SSREP         IFNE      *BLANKS
     C     AUREP         ANDNE     SSREP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    If 'contested only' flag = Y, check contested flag is 'Y'
     C*
     C     SSCTFL        IFEQ      'Y'
     C     AUCNFL        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    If 'outstanding only' = 'Y', check audit is outstanding
     C*
     C     SSOSFL        IFEQ      'Y'
     C     AURQDT        ANDLE     AURCVD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check vs. Cancelled, Full term, both
     C*
     C     SSCXFT        IFEQ      'F'
     C     AUCXFL        ANDEQ     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCXFT        IFEQ      'C'
     C     AUCXFL        ANDEQ     'N'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSUDWT        IFNE      *BLANKS
     C     SSRVWR        ORNE      *BLANKS
     C     SSMGR         ORNE      *BLANKS
      *
     C                   MOVE      AUEMP#        KKEMP#
     C                   MOVE      AUDIV         KKDIV
     C                   MOVE      AUFYR         KKFYR
     C     EMKY          SETLL     PEM01
     C     EMKY          READE     PEM01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C*    Check underwriter vs subset underwriter
     C*
     C     SSUDWT        IFNE      *BLANKS
     C     EMUWTR        ANDNE     SSUDWT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check reviewer vs subset reviewer
     C*
     C     SSRVWR        IFNE      *BLANKS
     C     EMARVW        ANDNE     SSRVWR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*    Check manager vs subset manager
     C*
     C     SSMGR         IFNE      *BLANKS
     C     EMAMGR        ANDNE     SSMGR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
      *** END ADD ***** 04/03/07 *****************************************
      *** BEG ADD ***** 01/21/09 *****************************************
     C*
     C*    Check Audit status value
     C*
     C     SSASTS        IFNE      AUSTAT
     C     SSASTS        ANDNE     *BLANK
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *** END ADD ***** 01/21/09 *****************************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C*    Check Plan Type of the employers...
     C*
     C     SSPLAN        IFNE      *BLANKS
     C*
     C                   MOVE      KKCO#         XXCO#             3 0
     C                   MOVE      KKFND         XXFND             3 0
     C                   MOVE      AUEMP#        XXEMP#            9 0
     C                   MOVE      AUDIV         XXDIV             5 0
     C                   MOVE      AUFYR         XXFYR             3 0
     C*
     C     ELKY1         SETLL     PEL01
     C     ELKY1         READE     PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     ELBSCD        XXBSCD           10
     C*
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     SSPLAN        IFNE      BSLPGM
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C                   ENDSR                                                  SUBSET
     C*================================================================
     C* VIEW1 - Forces View#1 and removes F11=Change view from screen
     C*================================================================
     C     VIEW1         BEGSR
     C*
     C                   MOVE      *OFF          *IN65
     C                   MOVE      *ON           *IN66
     C                   Z-ADD     SSEMP#        KKEMP#
     C                   Z-ADD     SSDIV         KKDIV
     C*
     C                   Z-ADD     1             VIEW#
     C*
      ***** BEG ADD ***** 07/08/04 ************************************
     C*    Force view #1 if subsetting on employer/division
     C     SAVRRN        IFNE      *ZERO
      ***** END ADD ***** 07/08/04 ************************************
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '11'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*
      ***** BEG ADD ***** 07/08/04 ************************************
     C                   ENDIF
      ***** END ADD ***** 07/08/04 ************************************
      *
     C                   ENDSR                                                  VIEW1
     C*================================================================
     C* VIEW3 - Forces View#3 and removes F11=Change view from screen
     C*================================================================
     C     VIEW3         BEGSR
     C*
      ***** BEG DEL ***** 07/08/04 ************************************
     C**********           MOVE *ON       *IN65
     C**********           MOVE *ON       *IN66
      ***** END DEL ***** 07/08/04 ************************************
     C*
     C                   Z-ADD     3             VIEW#
     C*
      ***** BEG DEL ***** 07/08/04 ************************************
     C*********            MOVE '2'       XXOPFN
     C*********            MOVE '11'      XXNMBR
     C*********            MOVE 'NNN'     XXMIS
     C*********            EXSR CHGOFA
      ***** END DEL ***** 07/08/04 ************************************
     C*
     C                   ENDSR                                                  VIEW3
     C*================================================================
     C* $SCR04 - Entry subset screen when called from wwgroups
     C*================================================================
     C     $SCR04        BEGSR
     C*
     C     *IN99         DOUNE     '1'
     C*
     C                   EXFMT     SCR04
     C*
     C*  Convert policy expiration from date to 8-digit date
     C                   Z-ADD     SSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SSFRDT
     C                   ENDIF
     C*
     C*  Convert policy expiration to date to 8-digit date
     C                   Z-ADD     SSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SSTODT
     C                   ENDIF
     C*
     C*  Convert policy expiration from date
     C                   Z-ADD     SSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C*
     C*  Convert policy expiration to date
     C                   Z-ADD     SSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*
     C*  Check that from date <= to date
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN99
     C                   ELSE
     C                   MOVE      *OFF          *IN99
     C                   ENDIF
     C*
     C***** BEG ADD ***** 01/21/09 ************************************
     C*
     C     SSASTS        IFNE      'A'
     C     SSASTS        ANDNE     'I'
     C     SSASTS        ANDNE     *BLANK
     C                   SETON                                        57
     C                   MOVE      *ON           *IN99
     C     *INKL         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C***** END ADD ***** 01/21/09 ************************************
     C                   ENDDO                                                  *IN99 NE '1'
     C*
     C                   ENDSR                                                  $SCR04
     C*================================================================
** Column headings - first line
                                                         Audit   Adt Adt
                                  Adt Rqst   Rcvd     Audit    Print    Mail
                                    Cont-  Rev-  Rea-  Rea-  Udr  Adt  Adt  Prm
    Adt                                                  Audit       Adt
    Adt                           Adt Rqst   Rcvd     Audit    Print    Mail
    Adt                             Cont-  Rev-  Rea-  Rea-  Udr  Adt  Adt  Prm
     Policy                                              Audit   Adt
     Policy                       Adt Rqst   Rcvd     Audit    Print    Mail
     Policy                         Cont-  Rev-  Rea-  Rea-  Udr  Adt  Adt  Prm
    Adt  Policy                                             Audit    Adt
    Adt  Policy                   Adt Rqst   Rcvd     Audit    Print    Mail
    Adt  Policy                     Cont-  Rev-  Rea-  Rea-  Udr  Adt  Adt  Prm
** Column headings - second line
Opt Employer  Div   Yr        Name                      Rqst Dt  Rep Co  CX STS
Opt Co  Employer  Div   Yr    Name                      Rqst Dt      Rep CX STS
Opt  To Date   Employer  Div  Name                      Rqst Dt  Rep Co  CX STS
Opt Co   To Date   Employer  Div  Name                      Rqst Dt  Rep CX STS
Opt Employer  Div   Yr              Date     Date    Attempt   Date     Date
Opt Co  Employer  Div   Yr          Date     Date    Attempt   Date     Date
Opt  To Date   Employer  Div        Date     Date    Attempt   Date     Date
Opt Co   To Date   Employer  Div    Date     Date    Attempt   Date     Date
Opt Employer  Div   Yr              ested  ised  sgnd  udit  Wtr  Rvw  Mgr  Act
Opt Co  Employer  Div   Yr          ested  ised  sgnd  udit  Wtr  Rvw  Mgr  Act
Opt  To Date   Employer  Div        ested  ised  sgnd  udit  Wtr  Rvw  Mgr  Act
Opt Co   To Date   Employer  Div    ested  ised  sgnd  udit  Wtr  Rvw  Mgr  Act
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Employer info          YYN
11414=DARTS                 YYN
12121=Audit Received        YNN
12222=Audit Attempted       YNN
12323=Audit Mailed          YNN
12424=Contested             YNN
12525=Clear Contested Info  YNN
12626=Reassign Audit Rep    YNN
16060=A/R Inquiry           YYY
16161=A/R Detail Summ       YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
213F13=Chg user parms       YYN
217F17=Subset               YYY
218F18=Audit rpt            YYN
221F21=View report          YYN
3
