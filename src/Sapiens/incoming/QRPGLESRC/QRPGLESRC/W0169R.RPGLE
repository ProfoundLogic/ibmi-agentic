     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0169R - WORK WITH CANCELLATION BATCHES
     H**************************************************************************
     H*                                                                        *
     H*  PROGRAM DESCRIPTION:                                                  *
     H*    This program displays a list of all unprocessed Cancellation        *
     H*    batches, showing the selection criteria for each.                   *
     H*                                                                        *
     H*  CALLED FROM:                                                          *
     H*     MENU   - CANCELLATION MAINTENANCE (SELECT)                         *
     H*                                                                        *
     H*  CALLS TO:                                                             *
     H*     W0162R - MASS CANCELLATION MAINTENANCE                             *
     H*                                                                        *
     H*  CREATED:    07/22/97                                                  *
     H*  PROGRAMMER: 35                                                        *
     H*  PROJECT:    WC577                                                     *
     H*                                                                        *
     H*                                                                        *
     H**************************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/24/24 C:300 Project: WC2906                                *
     H*                This program is no longer in use.              *
     H*                Pass-through this program to new screen.       *
      *                 Add extra entry parm to avoid Pass-through of old screen
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*                Change headers to allow for entry of Prem %.   *
     H*                Add column for "Amt / Type".                            *
     H*                                                                        *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREMIUM EXPORT/IMPORT)         *
     H*                Change displays if vendor processing is enabled.        *
     H*                                                                        *
     H* 11/30/00  C:61  PROJECT: WC577                                         *
     H*                 Correct screen headings. (opt 4 delete)                *
     H*                                                                        *
     H* 12/02/99  C:90  PROJECT: WC912                                         *
     H*                 ADD NEW COLUMN 'BAL TYPE' (CCEAFL) TO SCREEN.          *
     H*                                                                        *
     H* 10/07/99  C:87  PROJECT: WC894                                         *
     H*                 Chg column 'Balance dt' to 'Billings through dt'.  Chg *
     H*                 'Reporting month/year' to 'Pymnts thr/Rpt Mth/Yr' and  *
     H*                 move this column before 'Acct Type'.  If based on A/R  *
     H*                 balance display Pymts Thru Dt; if based on missing S/R *
     H*                 monthly reports display Rptg Mth/Yr.  Create addtn'l   *
     H*                 header line for new headings.  Reduce subfile size to  *
     H*                 11 to accomodate extra heading line.                   *
     H*                                                                        *
     H* 11/10/98  C:87  PROJECT: WC852                                         *
     H*                 Add column to screen for office number to the right of *
     H*                 reason code.                                           *
     H*                 When option 25=Issue mass cx is chosen read through    *
     H*                 detail recs for selected batch. If selected for canx,  *
     H*                 verify that all detail recs for that employer have non *
     H*                 blank reason codes. Display error msg if blank.        *
     H*                                                                        *
     H**************************************************************************
     FW0169F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWXCCP     IF   E             DISK
     F                                     RENAME(PCC01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWXCCL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCC01:DBRCDF)
     F***  BEGIN ADD  ***  11/10/98  **********************************
     FWXCNL4    IF   E           K DISK
     F***  END   ADD  ***  11/10/98  **********************************
      **** BEG   ADD **** 02/18/03 *********************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      **** END   ADD **** 02/18/03 *********************************************
      /COPY SCOPYWW01D
     D*
     D***  BEGIN ADD  ***  10/07/99  **********************************
     D***  BEG   ADD  ***  02/18/03  **********************************
     D******************* HDA     1   1 78               Column heading 1
     D HDA             S             78    DIM(2) CTDATA PERRCD(1)              Column heading 1
     D***  END   ADD  ***  02/18/03  **********************************
     D***  END   ADD  ***  10/07/99  **********************************
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Batches')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0169R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D***  BEGIN CHG  ***  10/07/99  **********************************
     D************* 12                    C         SF1PAG
     D SF1PAG          C                   CONST(11)

     I***  END   CHG  ***  10/07/99  **********************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
      **** Begin Add **** 04/24/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      * EDIT MASK API PARAMETER LIST
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKBAT#
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*  Key list for cancellation detail file
     C     CNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKBAT#
     C*
     C     CNKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKEMP#
     C***  END   ADD  ***  11/10/98  **********************************
      *
      **** BEG   ADD **** 02/18/03 *********************************************
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      **** END   ADD **** 02/18/03 *********************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CCCO#         KKCO#
     C     *LIKE         DEFINE    CCFND         KKFND
     C     *LIKE         DEFINE    CCBAT#        KKBAT#
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     *LIKE         DEFINE    CNEMP#        KKEMP#
     C***  END   ADD  ***  11/10/98  **********************************

      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if 2nd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 2
     c                   Call      'WTCXBTCH'
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C*================================================================
     C* CHKREC - Checks records selected for cancellation
     C*================================================================
     C     CHKREC        BEGSR
     C*
     C*  Chain to cancellation detail
     C                   MOVE      SCBAT#        KKBAT#
     C     CNKY          CHAIN     PCN01                              60
     C     *IN60         DOWEQ     *OFF
     C     *IN80         ANDEQ     *OFF
     C*
     C*    If record is selected for cancellation then check all divs
     C     CNSELC        IFEQ      'Y'
     C*
     C                   Z-ADD     CNEMP#        KKEMP#
     C                   Z-ADD     CNFYR         SAVFYR            3 0
     C     CNKY1         CHAIN     PCN01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C*      Make sure record has canx reason code (non blank)
     C     CNRECD        IFEQ      *BLANK
     C     CNFYR         ANDEQ     SAVFYR
     C                   SETON                                        80
     C                   ENDIF
     C*
     C     CNKY1         READE     PCN01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C*      Set at next employer in canx detl file
     C     CNKY1         SETGT     PCN01
     C                   ENDIF                                                  CNSELC='Y'
     C*
     C     CNKY          READE     PCN01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKREC
     C***  END   ADD  ***  11/10/98  **********************************
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      *
     C     SCDBRR        CHAIN     PFRCDF                             99                        1
      *
     C     CCBPF         IFNE      'Y'
     C                   SETON                                        71  90
     C                   ENDIF                                                  CCBPF IFNE
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      ##FND         KKFND
     C                   Z-ADD     0             KKBAT#
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        KKBAT#
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     CCBAT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
      *
     C                   CALL      CHGNUM        CHGPL
      *
     C                   MOVEL     XXNUM         SCBAT#
     C                   MOVE      CCBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBLDT
     C                   MOVE      CCETYP        SCETYP
      **** BEGIN ADD **** 12/02/99 ************************************
     C                   MOVE      CCEAFL        SCEAFL
      **** END   ADD **** 12/02/99 ************************************
      ***** BEG CHG ******* 05/02/03 **********************************
     C*************        MOVE CCMIN     SCMIN
     C                   Z-ADD     CCMIN         SCMIN
     C                   MOVE      CCADPP        SCADPP
      ***** END CHG ******* 05/02/03 **********************************
      *
      **** BEG ADD ****** 02/18/03 *********************************************
     C     *IN41         IFEQ      *OFF
      **** END ADD ****** 02/18/03 *********************************************
     C                   MOVE(P)   CCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C*    If batch is based on A/R balance (SCRPMY=0) display the 'Payments
     C*    through date, otherwise the batch is based on missing S/R monthly
     C*    rpts - display 'Reporting month/year'.
     C*
     C     SCRPMY        IFEQ      0
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   Z-ADD     CCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCSADT
     C*
     C                   ELSE
     C                   MOVEL(P)  'RPMY'        EDTMSK
     C                   MOVEL(P)  SCRPMY        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C     ' '           CAT       XXNUM:1       SCSADT
     C*
     C                   ENDIF                                                  SCRPMY=0
     C*
      **** BEG ADD ****** 02/18/03 *********************************************
     C                   ELSE                                                   *IN41 = *OFF
     C*
     C                   MOVE(P)   CCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWEDT
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C*    If batch is based on A/R balance (SCWEDT=0) display the 'Payments
     C*    through date, otherwise the batch is based on missing S/R monthly
     C*    rpts - display 'Reporting date'.
     C*
     C     SCWEDT        IFEQ      0
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   Z-ADD     CCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCSADT
     C*
     C                   ELSE
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   MOVEL(P)  SCWEDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCSADT
     C*
     C                   ENDIF                                                  SCRPMY=0
     C*
     C                   ENDIF                                                  *IN41 = *OFF
      **** END ADD ****** 02/18/03 *********************************************
     C                   MOVE      CCRSN         SCRSN
     C                   MOVE      CCBPF         SCBPF
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVEL     CCOFF#        SCOFF#
     C***  END   ADD  ***  11/10/98  **********************************
      **** BEGIN DEL **** 12/02/99 ************************************
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C*    Move header line 1 into screen field
     C******************** MOVELHDA,1     SCHDLA
     C***  END   ADD  ***  10/07/99  **********************************
      **** END   DEL **** 12/02/99 ************************************
      *
     C                   ENDSR
      *
      *===============================================================
      * OPT8  - SUBROUTINE FOR OPTION 8
      *===============================================================
      *
      * OPTION 8=CANCELLATION BATCH DETAIL
      *
     C     OPT8          BEGSR
      *
     C                   CALL      'W0162R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      ' '           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
      *
     C                   ENDSR                                                  OPT8
      *
      *===============================================================
      * OPT25 - SUBROUTINE FOR OPTION 25
      *===============================================================
      *
      * OPTION 25 = ISSUE MASS CANCELLATION
     C*
     C     OPT25         BEGSR
      *
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   EXSR      CHKREC
     C*
     C*  Do not process cancellation if a blank reason code is found
     C     *IN80         IFEQ      *OFF
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C                   CALL      'W0169R25'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      'C'           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   ELSE
     C*
     C                   EXFMT     SCR04
     C*
     C                   ENDIF                                                  *IN80=*OFF
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C                   ENDSR                                                  OPT25
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '25'          OPT25
      *
     C                   ENDCS                                                  SCSEL CASEQ
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
      **** BEG   ADD **** 02/18/03 *********************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
      *
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        41
     C                   MOVEL     HDA(2)        SCHDLB
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF                                                  WCFL23 IFEQ
      **** END   ADD **** 02/18/03 *********************************************
      *
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCNAME
      **** BEGIN ADD **** 12/02/99 ************************************
     C*    Move header line 1 into screen field
     C                   MOVEL     HDA(1)        SCHDLA
      **** END   ADD **** 12/02/99 ************************************
      *
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL     '*Deleted'    SCBAT#
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings - 1st line
               Billings    Pymts thr/  Type Bal      Amt/    Amt
Opt    Batch#  Through Dt  Rpt date    Acct Type    Prem%    Type  Rsn Office#
** Column headings - 2nd line
Opt    Batch#  Through Dt  Rpt Mth/Yr  Acct Type    Prem%    Type  Rsn Office#
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                NNN
1088=Cancellation batch dtl YYN
12525=Issue mass CX         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Build CX batch        YNN
213F13=Chg user parms       YNN
3
