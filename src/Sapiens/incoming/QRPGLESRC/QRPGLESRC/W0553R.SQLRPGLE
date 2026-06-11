     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* ============================================================= *
     H/TITLE W0553R - WORK WITH AGENT HISTORY
     H* ============================================================= *
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL AGENT  *
     H*                HISTORY RECORDS FROM A SELECTED EMPLOYER.  EACH*
     H*                RECORD ACCESSED FOR UPDATE, DISPLAY OR         *
     H*                SELECTION DEPENDING ON THE CALLING PROGRAM.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R - WORK WITH EMPLOYERS                               *
     H*    W0051R - WORK WITH EMPLOYERS POLICY YEARS                  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0553R02 - AGENT HISTORY INFORMATION (OPT 2/5/20)          *
     H*    W4150R - QUOTE STATUS (OPT9)                               *
     H*                                                               *
     H*  CREATED: 07/22/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H* ============================================================= *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/30/11 C:65  PROJECT: SY255 PRISS 73 (SEQ 71976/69598)      *
     H*                FIX PRIOR TWO SUPPORT FIXES TO ENSURE THE      *
     H*                DUPLICATE AGENT EDIT IS INVOKED, IN EITHER     *
     H*                SINGLE YEAR MODE OR MULTIPLE YEAR MODE.        *
     H*                                                               *
     H* 01/22/10 C:246 PROJECT:139WNQ                                 *
     H*                Error window for Tertiary Agent and include    *
     H*                Tertiary Agent in SQL for Count.               *
     H*                                                               *
     H* 12/18/09 C:189 SUPPORT: 71076                                 *
     H*                Error window for duplicate agents with same    *
     H*                effective date should show for the year user   *
     H*                is accessing.                                  *
     H*                                                               *
     H* 12/09/09 C:235 SUPPORT: 70277                                 *
     H*                Need to revise the edit put into place by      *
     H*                WC1423 to include the Agent History Effective. *
     H*                                                               *
     H* 11/06/09 C:232 PROJECT: 139WLX PRISS#14                       *
     H*                Change the parameter of the programs.          *
     H*                                                               *
     H* 10/05/09 C:180 SUPPORT: 70159
     H*                ALLOW CHANGE OF RATE FOR RENEWAL TERM
     H*
     H* 09/30/09 C:232 PROJECT: 139WLX                                *
     H*                INQUIRY ACCESS TO AGENT FROM AGENT HISTORY.    *
     H*
     H* 09/28/09 C:33  Project: WC1788                                *
     H*                Do not allow for the addition of a record, if  *
     H*                 a quote transfer exist for the employer.      *
     H*                                                               *
     H* 08/25/09 C:220 SUPPORT: 69598                                 *
     H*                SHOW AGENT RECORDS YEAR WISE                   *
     H*                                                               *
     H* 04/10/08 C:136 SUPPORT: WA133 62917                           *
     H*                ALLOW USER TO EXIT IF THE 'DUPLICATE' EDIT IS  *
     H*                INVOKED BUT THE USER IS IN INQUIRY MODE        *
     H*                                                               *
     H* 12/31/07 C:136 SUPPORT: WA132 60309                           *
     H*                REBUILD EDITS FOR INACTIVATING A RECORD        *
     H*                - A RECORD MUST EXIST THAT CAN REPLACE IT      *
     H*                - DISPLAY INFO WINDOW IF COMMISSION IS POSTED  *
     H*                  TO AN AGENT HISTORY RECORD THAT IS BEING     *
     H*                  INACTIVATED                                  *
     H*                - DON'T ALLOW A RECORD TO BE ACTIVIATED IF     *
     H*                  IT HAS BEEN PREVIOUSLY INACTIVIATED PER GW   *
     H*                  IT ORDER TO PROVIDE AN AUDIT TRAIL           *
     H*                - DON'T ALLOW USER TO EXIT IF TWO ACTIVE       *
     H*                  RECORDS WITH THE SAME EFF DT EXIST AT THE    *
     H*                  SAME TIME. EDIT PUT IN PGMEND.               *
     H*                                                               *
     H* 12/11/07 C:136 SUPPORT: WA132 60309 (WC1423)                  *
     H*                TEMP DISABLE ALL EDITS FOR INACTIVIATING       *
     H*                AN AGENT PER GW                                *
     H*                                                               *
     H* 10/23/07 C:180 Support: WA146 - 60659                         *
     H*                                                               *
     H* 10/10/07 C:132 Support: WA132 - 60521                         *
     H*                For the Active/Inactive option 20, add these:  *
     H*              - If other agents exist, check for Inactive on   *
     H*                these other agent history records.             *
     H*                                                               *
     H* 09/28/07 C:65  PROJECT: SUPP# 60309                           *
     H*                PERFORM A RECORD EXCPT TO REMOVE THE LOCK ON   *
     H*                WAQTP IN THE EMPAGT SUBROUTINE, WHEN FILE IS   *
     H*                NOT BEING UPDATED.                             *
     H*                                                               *
     H* 11/28/06 C:132 Project: WC1423                                *
     H*                For the Active/Inactive option 20, add these:  *
     H*              - ADD Edits against Other Agent & Producer #'s   *
     H*              - ADD Edits for Commission Records, Inactivate   *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 03/08/02 C:132 PROJECT: WC1071                                *
     H*                Change Option 20 to check for cancel date in   *
     H*                WMAPP producer file                            *
     H*                                                               *
     H* 10/26/01 C:61  SUPPORT WA107 29472                            *
     H*                Update maintenance date and time fields        *
     H*                correctly.                                     *
     H*                                                               *
     H* 05/08/01 C:90  PROJECT: SEQ#27402                             *
     H*                Load DARTS remark if an agent change is made.  *
     H*                                                               *
     H* 03/28/01 C:90  PROJECT: CM027                                 *
     H*                PRINT A MID TERM ENDORSEMENT IF AN AGENT       *
     H*                CHANGE IS MADE FROM THAT PROCESS.              *
     H*                                                               *
     H* 11/01/00 C:33  PROJECT: WC1008                                *
     H*                WHEN TRANSFERRING, IF LAST STATUS IS 'C' OR 'R'*
     H*                LOAD TO QTXFER                                 *
     H*                                                               *
     H* 07/13/00 C:33  SEQ#: WA102/23358                              *
     H*                DO NOT WRITE QUOTE TRACKING RECORD IF NOT THE  *
     H*                LATEST AGENT.                                  *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*              - Add previous fund number to header info.       *
     H*                                                               *
     H*  08/10/98  C:61 Project: 48WLC                                *
     H*                  Display old Emp#/Div from Employer Cross     *
     H*                  Reference (WZEXP).                           *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                LOAD VALUE 'Q' OR BLANKS IN QCQTE(DARTS)       *
     H*                DEPENDING IF IT IS IN QUOTE MODE OR NOT.       *
     H*                                                               *
     H*  07/10/98  C:33  SEQ#: WA091/12598                            *
     H*                  AVOID WRITING MULTIPLE RECORDS TO AGENT QUOTE*
     H*                  TRACKING FILE.                               *
     H*                                                               *
     H*  06/07/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT ALLOW ACTIVATION OF A RECORD IF THE   *
     H*                  AGENT INFORMATION HAS CHANGED THAT WOULD     *
     H*                  CAUSE IT TO BE INVALID.                      *
     H*                                                               *
     H*  05/26/98  C:33  SEQ#: WA093/11752                            *
     H*                  LOAD CORRECT CALL PARM FOR ADDITION OF AGENT *
     H*                  HISTORY.                                     *
     H*                                                               *
     H*  04/10/98  C:33  PROJECT: WC395                               *
     H*                  ALLOW FOR TRANSFER OF AGENT IN QUOTE TRACKING*
     H*                                                               *
     H*  04/01/98  C:78  PROJECT: WC825                               *
     H*                  ADDED ACCESS TO DARTS. F14.                  *
     H*                                                               *
     H*  02/18/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT PRELOAD THE QUOTE STATUS RECORD IN    *
     H*                  INQUIRY MODE.                                *
     H*                                                               *
     H*  01/21/98  C:33  PROJECT: WC577                               *
     H*                  LOAD CORRECT RRN FOR OPT 9.                  *
     H*                                                               *
     H*  11/04/97  C:33  PROJECT: WC577                               *
     H*                 -NEW CALL PARMS FOR W4150R                    *
     H*                 -LOAD NEW FIELDS FOR WAQTP                    *
     H* ============================================================= *
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  63 - Display old Emp#/Div in header info                     *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
      *
      *
     FW0553F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWMAHP     IF   E             DISK
     F                                     RENAME(PAH01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMAHL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAH01:DBRCDF)
      *
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
      *
     FWDELP     IF   E             DISK
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      ***** END ADD ******* 08/10/98 **********************************
      *
     FWMFNL     IF   E           K DISK
      *
     FWMAAL1    IF   E           K DISK
      ***  BEGIN ADD  ***  11/04/97  *********************************
     F                                     INFDS(DBFDSA)
      ***  END   ADD  ***  11/04/97  *********************************
      *
     FWMAGL     IF   E           K DISK
      ***  BEGIN CHG  ***  04/10/98  *********************************
      ****WAQTL7  IF  E           K        DISK                      A
      *
     FWAQTL7    UF A E           K DISK
      ***  END   CHG  ***  04/10/98  *********************************
     F**** BEGIN ADD **** 03/28/01 ************************************
      *
     FWTMYP     IF   E             DISK
      *
     FWMDXL     IF   E           K DISK
      *
     FWMENL3    IF A E           K DISK
      *
     FWDF2L     IF   E           K DISK
      *
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMAHL     IF   E           K DISK
     F                                     RENAME(PAH01:PAH03)
     F**** END   ADD **** 03/28/01 ************************************
      *
     F**** BEGIN ADD **** 03/08/02 ************************************
      * Producer File - Check for cancel on Re-activate record
     F*** BEG DEL ***** 12/31/07 **************************************
      * Producer File - Check for cancel on Re-activate record
     F**********WMAPL   IF  E           K        DISK
     F*** END DEL ***** 12/31/07 **************************************
     F**** END   ADD **** 03/08/02 ************************************
     F*
      ***** BEG ADD ***** 11/28/06 ************************************
      * Agent Commission File
      ***** BEG DEL ***** 12/11/07 ************************************
      * Agent Commission File
     F*********WMACL   IF  E           K        DISK
      ***** END DEL ***** 12/11/07 ************************************
      *** BEG ADD ***** 12/31/07 **************************************
     FWMACL     IF   E           K DISK
      *** END ADD ***** 12/31/07 **************************************
     F***  Begin Add  ***  09/28/09  **************************************
     FWAQOL     IF   E           K DISK    USROPN
     F***  End   Add  ***  09/28/09  **************************************
      ***** END ADD ***** 11/28/06 ************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D**** BEGIN CHG  ***  04/01/98  *********************************
     D****************    OF      1  12  1   OFD    35   Options, Function
     D**** BEGIN CHG  ***  09/30/09  *********************************
     D***********         OF      1  13  1   OFD    35   Options, Function
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D**** END   CHG  ***  09/30/09  *********************************
     D**** END   CHG  ***  04/01/98  *********************************
     D*** BEG ADD ***** 12/31/07 *************************************
     D MSG             S             60    DIM(12) CTDATA PERRCD(1)             INFORMATION EDITS
     D*** END ADD ***** 12/31/07 *************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***  BEGIN ADD  ***  11/14/97  *********************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D***  END   ADD  ***  11/14/97  *********************************
      **** BEGIN ADD **** 04/01/98 *********************************************
     D                 DS
     D  QCKEY                  1     16
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      **** END   ADD **** 04/01/98 *********************************************
     D                 DS
     D  DATA                   1     70
     D  AHFYR                  1      3  0
     D  XXEFDT                 6     15
     D  XXAGT#                18     22
     D  XXNAME                25     44
     D  XXRT#                 48     50
     D  XXPROD                56     60
     D**** BEGIN CHG **** 09/30/09 ************************************
     D****                                   65  65 AHSTAT
     D  XXAOR#                63     67
     D  AHSTAT                70     70
     D**** END   CHG **** 09/30/09 *********************************************
     D*
     D***PY SCOPYWW01I
     D*
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D**** BEGIN ADD **** 03/28/01 ************************************
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D**** END   ADD **** 03/28/01 ************************************
     D*** BEG ADD ***** 12/31/07 **************************************
     D                 DS
     D  ACPODT                 1      7  0
     D  ACYRMN                 1      5  0
      *
     D                 DS
     D  SYSDAT                 1      7  0
     D  SYYRMN                 1      5  0
     D*** END ADD ***** 12/31/07 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent History')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0553R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)
     D**** BEGIN ADD **** 03/28/01 ************************************
     D AGTCHG          C                   CONST('AGENT CHANGE')
     D**** END   ADD **** 03/28/01 ************************************
     D***  Begin Add  ***  09/28/09  **************************************
     D OVR             C                   CONST('OVRDBF FILE(')
     D TFIL            C                   CONST(') TOFILE(')
     D GETLIB          C                   CONST('RTVENVLIB')
     D***  End   Add  ***  09/28/09  **************************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@QTE             1
     C                   PARM                    @@MODE            1
      *
      *  NOTE: -IF THIS PROGRAM IS CALLED USING THE EMPLOYER MASTER RRN
      *         THE MODE PARM SHOULD BE 'E'.
      *        -IF THIS PROGRAM IS CALLED USING THE EMPLOYER DETAIL RRN
      *         THE MODE PARM SHOULD BE 'Y'.  IF ALL RECORD SHOULD BE
      *         SHOWN (NOT JUST THE YEAR IN THE DETAIL RECORD) THE MODE
      *         PARM SHOULD BE '*'
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE             1
      ***  BEGIN CHG  ***  05/26/98  *********************************
      **********           PARM           ##CALL  1
     C                   PARM                    #@CALL            1
      ***  END   CHG  ***  05/26/98  *********************************
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     KEY1          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
     C     KEY1A         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKAGT#
     C*
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***** END ADD ******* 08/10/98 **********************************
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *  AGENT ASSIGNMENT FILE
      *
     C     AAKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHAGT#
      *
      ***  BEGIN ADD  ***  06/07/98  *********************************
     C     AAKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCAGT#
      ***  END   ADD  ***  06/07/98  *********************************
     C**** BEGIN ADD **** 03/28/01 ************************************
      *
     C     AAKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    ZZAGT#
      *
     C**** END   ADD **** 03/28/01 ************************************
      *
      *  AGENT MASTER FILE
      *
     C     AGKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      **** BEGIN ADD **** 03/08/02 ************************************
      *  AGENT PRODUCER FILE
     C     APKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    #XPROD
      *
     C     AAKY3         KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    #XAGT#
      **** END   ADD **** 03/08/02 ************************************
     C*
      ***** BEG ADD ***** 11/28/06 ***************************************
     C*  Agent Commission file
     C     ACKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    AHAGT#
     C                   KFLD                    AHPROD
     C*
     C     ACKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    AHSAGT
     C                   KFLD                    AHSPRD
     C*
      ***** END ADD ***** 11/28/06 ***************************************
      *
      *  AGENT QUOTE FILE
      *
      ***  BEGIN ADD  ***  04/10/98  *********************************
     C     QTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***  BEGIN CHG  ***  07/10/98  *********************************
      **********           KFLD           @@QTE
     C                   KFLD                    XXSTAT
      ***  END   CHG  ***  07/10/98  *********************************
      ***  END   ADD  ***  04/10/98  *********************************
      *
     C     QTKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***  BEGIN CHG  ***  07/10/98  *********************************
      **********           KFLD           @@QTE
     C                   KFLD                    XXSTAT
      ***  END   CHG  ***  07/10/98  *********************************
     C                   KFLD                    SCAGT#
     C**** BEGIN ADD **** 03/28/01 ************************************
      *
      *  ENDORS/DARTS CONTROL DEFINITION
      *
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *  ENDORSEMENT REQUEST WORKFILE
      *
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    @@EFDT
     C*
     C     AHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C**** END   ADD **** 03/28/01 ************************************
      *
      ***** BEG ADD ***** 11/28/06 ************************************
     C*
     C     AHKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
      ***** END ADD ***** 11/28/06 ************************************
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    AHFYR         KKFYR
     C     *LIKE         DEFINE    AHEFDT        KKEFDT
     C     *LIKE         DEFINE    AHAGT#        KKAGT#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XZNAME
      *** BEGIN ADD *** 04/01/98 ***********************************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *** END   ADD *** 04/01/98 ***********************************************
      ***  BEGIN ADD  ***  04/10/98  *********************************
     C     *LIKE         DEFINE    QTDT          X7DATE
      ***  END   ADD  ***  04/10/98  *********************************
      ***  BEGIN ADD  ***  07/10/98  *********************************
     C     *LIKE         DEFINE    QTSTAT        XXSTAT
      ***  END   ADD  ***  07/10/98  *********************************
     C***  BEGIN ADD  ***  07/13/00  *********************************
     C     *LIKE         DEFINE    AHFYR         ##FYR
     C     *LIKE         DEFINE    AHAGT#        ##AGT#
     C     *LIKE         DEFINE    AGNAME        ##AGNM
     C     *LIKE         DEFINE    AHAGT#        XXAGT2
     C***  END   ADD  ***  07/13/00  *********************************
     C**** BEGIN ADD **** 03/28/01 ************************************
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
     C     *LIKE         DEFINE    AHAGT#        SAVAGT
     C     *LIKE         DEFINE    AHAGT#        NEWAGT
     C     *LIKE         DEFINE    AHAGT#        ZZAGT#
     C     *LIKE         DEFINE    ELPFDT        @@EFDT
     C**** END   ADD **** 03/28/01 ************************************
     C**** BEGIN ADD **** 03/08/02 ************************************
     C     *LIKE         DEFINE    AHPROD        #XPROD
     C     *LIKE         DEFINE    AHAGT#        #XAGT#
     C**** END ADD ****** 03/08/02 ************************************
      ***** BEG ADD ***** 11/28/06 ************************************
     C     *LIKE         DEFINE    AHAGT#        SVAGT#
     C     *LIKE         DEFINE    AHSAGT        SVSAGT
      ***** BEG ADD ***** 12/09/09 ************************************
     C     *LIKE         DEFINE    AHEFDT        ##EFDT
      ***** END ADD ***** 12/09/09 ************************************
      ***** END ADD ***** 11/28/06 ************************************
      *
     C/COPY SCOPYWW01C
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
      *
     C     CHKSSM        BEGSR
      *
     C     SSFYR         IFNE      *ZEROS
     C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSFYR<>*ZEROS
      *
     C                   ENDSR                                                  CHKSSM
      *
     C**** BEGIN ADD **** 03/28/01 ************************************
      *===============================================================
      *  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      *===============================================================
     C     CKCHGS        BEGSR
      *
     C                   MOVE      *HIVAL        KKFYR
     C                   MOVE      *HIVAL        KKEFDT
     C                   MOVE      *HIVAL        KKAGT#
     C     KEY1          SETGT     PAH03
     C     KEY1          READPE    PAH03                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     KEY1          READPE    PAH03                                  60
     C                   END
     C                   Z-ADD     AHAGT#        NEWAGT
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C     @@QTE         ANDNE     'Q'
      *
     C     SAVAGT        IFNE      NEWAGT
     C*
     C     F2KY          CHAIN     PF201                              60
      *
      *  GET ENDORS/DARTS INFORMATION
      *
     C     DXKY          CHAIN     PDX01                              60
     C**** BEGIN ADD **** 05/08/01 ************************************
     C                   MOVEL     DXPANC        ENRMCD
     C**** END   ADD **** 05/08/01 ************************************
      *
      *  SETUP FOR NEXT REQUEST NUMBER
      *
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      *
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      SAVAGT        ENAGT1
     C*
     C*
     C                   MOVE      NEWAGT        ENAGT2
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
      *
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C                   MOVEL     EMCNTY        ENCNT1
     C                   MOVE      F2CAR#        ENCAR1
     C                   MOVE      F2CAR#        ENCAR2
     C     ELKY          CHAIN(N)  PEL02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
      *
     C     ELKY          READE(N)  PEL02                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60
      *
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     AGTCHG        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      *
      *  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      *
     C     EMPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
     C                   ELSE                                                   LOAD MAIL ADDR
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
     C                   ENDIF
      *
     C                   Z-ADD     SAVAGT        ZZAGT#
     C                   MOVEL     SAVAGT        @@AGT#            5
     C     AAKY2         CHAIN     PAA01                              60
     C     AGKY          CHAIN     PAG01                              60
     C     @@AGT#        CAT       AGNAME:2      ENBLIN
     C*
     C                   Z-ADD     NEWAGT        ZZAGT#
     C                   MOVEL     NEWAGT        @@AGT#
     C     AAKY2         CHAIN     PAA01                              60
     C     AGKY          CHAIN     PAG01                              60
     C     @@AGT#        CAT       AGNAME:2      ENALIN
     C                   MOVE      NEWAGT        ENAGT#
     C                   MOVEL     AGNAME        ENAGNM
     C                   MOVE      'Y'           ENCIFL
     C                   WRITE     PEN01
      *
     C                   ENDIF                                                  SVAGT# <> SCAGTE='E'
      *
     C                   ENDIF                                                  LDENDA <> *ZERO
      *
     C                   ENDSR                                                  CKCHGS
     C**** END   ADD **** 03/28/01 ************************************
     C*
     C***  Begin Add  ***  09/28/09  **************************************
     C*====================================================================
     C* Check for Quote Transfer records.  If exists, remove F6.
     C*====================================================================
     C     CKQTXF        BEGSR
     C*
     C                   CALL      GETLIB
     C                   PARM      '*OTHER'      RQSTYP           10
     C                   PARM                    XXLIB            10
     C                   PARM                    XXMLIB           10
     C*
     C                   CALL      'SRCHKOB2'
     C                   PARM                    XXLIB            10
     C                   PARM      'WAQOL'       XXOBJ            10
     C                   PARM      '*FILE'       XXTYPE            7
     C                   PARM                    XXERR             1
     C*
     C     XXERR         IFNE      *OFF
     C                   CALL      GETLIB
     C                   PARM      '*CURRENT'    RQSTYP           10
     C                   PARM                    XXLIB            10
     C                   PARM                    XXMLIB           10
     C                   ENDIF
     C*
     C                   MOVEL(P)  OVR           XXCMD
     C                   CAT       'WAQOL':0     XXCMD
     C                   CAT       TFIL:0        XXCMD
     C                   CAT       XXLIB:0       XXCMD
     C                   CAT       '/':0         XXCMD
     C                   CAT       'WAQOL':0     XXCMD
     C                   CAT       ')':0         XXCMD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD            55
     C                   PARM      55            XXLEN            15 5
     C*
     C                   OPEN      WAQOL
     C*
     C                   MOVEL     '2'           XXOPFN
     C                   MOVEL     '06'          XXNMBR
     C*
     C     KEY1          CHAIN     WAQOL                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'NNN'         XXMIS
     C                   ELSE
     C                   MOVEL     'YNN'         XXMIS
     C                   ENDIF
     C*
     C                   EXSR      UPDOFA
     C*
     C                   CLOSE     WAQOL
     C*
     C                   ENDSR
     C***  End   Add  ***  09/28/09  **************************************
     C*
      ***  BEGIN ADD  ***  04/10/98  *********************************
      *================================================================
      * EMPAGT - Check to see if this emp already has an agent assigned
      *================================================================
      *
     C     EMPAGT        BEGSR
      *
     C                   MOVE      'N'           OLDAGT            1
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE
      *
     C     QTKY          CHAIN     PQT01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      ***** BEG DEL ******* 10/26/01 **********************************
     C*********            MOVELQ1USER    QTUSER    P
     C*********            MOVELQ1PGM     QTPGM     P
     C*********            Z-ADDX7DATE    QTDT
     C*********            Z-ADDDSTIME    QTITM
      ***** END DEL ******* 10/26/01 **********************************
      *
     C     @@QTE         IFEQ      'Q'
      *
     C                   Z-ADD     SCAGT#        QTAGT#
      *
      ***** BEG ADD ******* 10/26/01 **********************************
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
      *
      ***** END ADD ******* 10/26/01 **********************************
     C                   UPDATE    PQT01
      *
     C                   ELSE
     C*
     C*** BEGIN ADD ***  09/28/07  ************************************
     C                   EXCEPT    PQT01E
     C*** END ADD *****  09/28/07  ************************************
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   SELECT
     C     QTTYPE        WHENEQ    'C'
     C     QTTYPE        OREQ      'R'
     C                   MOVE      QTTYPE        QTXFER
     C                   OTHER
     C                   MOVE      *BLANKS       QTXFER
     C                   ENDSL
     C***  END   ADD  ***  11/01/00  *********************************
      *
     C                   MOVE      'X'           QTTYPE
      *
     C                   EXSR      WRITEQ
      *
     C                   Z-ADD     SCAGT#        QTAGT#
     C                   MOVE      'T'           QTTYPE
      *
     C                   EXSR      WRITEQ
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   CLEAR                   QTXFER
     C***  END   ADD  ***  11/01/00  *********************************
      *
     C                   ENDIF                                                  @@QTE=Q
      *
     C                   MOVE      'Y'           OLDAGT
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDSR                                                  EMPAGT
      ***  END   ADD  ***  04/10/98  *********************************
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
      *
     C     ERRCK1        BEGSR
      *
      ***  BEGIN ADD  ***  06/07/98  *********************************
     C                   SELECT
      *_______________________________________________________________
      *
     C     SCSEL         WHENEQ    '20'
      ***** BEG ADD ***** 10/05/09 ************************************
     C     XXSTAT        IFEQ      'Q'
     C     ELKY          CHAIN(N)  PEL02                              60
     C   60              MOVE      *ZEROS        ELPFDT
     C                   ENDIF
      ***** END ADD ***** 10/05/09 ************************************
      *
      ***** BEG ADD ***** 11/28/06 ************************************
      ***** BEG DEL ***** 12/11/07 ************************************
      *************
     C*************        SETOF                     444546
      ******BEG*ADD ***** 10/10/07 ************************************
     C*************        MOVE ' '       DIFAGT  1
      ******END*ADD ***** 10/10/07 ************************************
      *************
     C***********SCSTAT    IFEQ 'A'
      *************
      **Check*for*active Commission Records for this year.
     C***********ACKY      CHAINWMACL                60
     C************IN60     IFEQ *OFF
     C*************        SETON                     44  90
     C*************        ENDIF                           ACKY CHAIN   D
     C**N90******AHSAGT    IFNE *ZEROS
     C***********ACKY1     CHAINWMACL                60
     C**N60********        SETON                     44  90
     C*************        ENDIF                           SCAGT2<>SVAGT2
      *************
     C************IN90     IFEQ *OFF
      **If*there*is no other agent history record with a different agt#
      **Then*don't*allow Agent History to be Inactivated.
     C***********AHKY2     SETLLWMAHL
     C*************        MOVE *OFF      *IN60
     C*************        Z-ADDAHAGT#    SVAGT#
     C*************        Z-ADDAHSAGT    SVSAGT
      ******BEG*DLT ***** 10/10/07 ************************************
     C*************        MOVE ' '       DIFAGT  1
      ******END*DLT ***** 10/10/07 ************************************
      *************
      ******BEG*ADD ***** 10/23/07 ************************************
     C*************        MOVE *ZEROS    AGTCNT  50
      ******END*ADD ***** 10/23/07 ************************************
     C***********AHKY2     READEWMAHL                    60
      *************
     C************IN60     DOWNE*ON
      **********BEG ADD ***************** 10/23/07 ********************
     C***********AHSTAT    IFEQ 'A'
     C*************        ADD  1         AGTCNT
     C*************        ENDIF
      **********END ADD ***************** 10/23/07 ********************
     C***********AHAGT#    IFNE SVAGT#
     C***********AHSAGT    ORNE SVSAGT
     C*************        MOVE 'Y'       DIFAGT
     C*************        LEAVE
     C*************        ENDIF
     C***********AHKY2     READEWMAHL                    60
     C*************        ENDDO
      *************
      ******BEG*DLT ***** 10/10/07 ************************************
      ***If*no*other agent, edit to not be inactivated changed to allow
     C**N90******DIFAGT    IFNE 'Y'
     C*************        SETON                     45  90
     C*************        ENDIF
      ******BEG*DLT ***** 10/10/07 ************************************
      ******BEG*ADD ***** 10/23/07 ************************************
     C**N90******AGTCNT    IFLE 1
     C*************        SETON                     45  90
     C*************        ENDIF
      ******END*ADD ***** 10/23/07 ************************************
      *************
     C*************        ENDIF                           *IN90 = *OFF
      *************
     C*************        ENDIF                           SCSTAT=A
      ***** END DEL ***** 12/11/07 ************************************
      ***** END ADD ***** 11/28/06 ************************************
      ***** BEG ADD ***** 12/31/07 ************************************
      *
      *  EDIT TO ENSURE THAT AN AGENT RECORD CAN'T BE DELETED UNLESS
      *  ANOTHER WITH AN EFF DT <= THE ORIGINAL HAS REPLACED IT.
      *
     C     SCSTAT        IFEQ      'A'
     C                   Z-ADD     *ZERO         AGTCNT            5 0
     C***** BEGIN ADD ****** 10/05/09 ********************************
     C                   Z-ADD     *ZERO         QTECNT            5 0
     C                   Z-ADD     *ZERO         RENCNT            5 0
     C***** END ADD   ****** 10/05/09 ********************************
     C     AHKY2         SETLL     WMAHL
     C     AHKY2         READE     WMAHL                                  60
     C     *IN60         DOWEQ     *OFF
     C     AHEFDT        IFLE      SCEFDT
     C     AHSTAT        ANDEQ     'A'
     C                   ADD       1             AGTCNT
     C                   ENDIF
      *
     C***** BEGIN ADD ****** 10/05/09 ********************************
     C     XXSTAT        IFEQ      'Q'
     C     AHSTAT        ANDEQ     'A'
     C                   ADD       1             QTECNT
     C     AHEFDT        IFGT      SCEFDT
     C     AHEFDT        ANDEQ     ELPFDT
     C     ELPFDT        ANDGT     EMEFD1
     C                   ADD       1             RENCNT
     C                   ENDIF
     C                   ENDIF
     C***** END ADD   ****** 10/05/09 ********************************
      *
     C     AHKY2         READE     WMAHL                                  60
     C                   ENDDO
      *
      ***** BEG ADD ******* 10/05/09 **********************************
     C                   SELECT
     C     XXSTAT        WHENEQ    'Q'
     C     QTECNT        ANDEQ     2
     C     RENCNT        ANDEQ     1
     C     AGTCNT        ANDEQ     1
     C                   MOVE      *OFF          *IN45
     C     XXSTAT        WHENEQ    'Q'
     C     AGTCNT        ANDLE     1
     C                   SETON                                        45  90
     C                   OTHER
      ***** END ADD ******* 10/05/09 **********************************
      *
     C     AGTCNT        IFLE      1
     C                   SETON                                        45  90
     C                   ENDIF
      ***** BEG ADD ******* 10/05/09 **********************************
     C                   ENDSL
      ***** END ADD ******* 10/05/09 **********************************
      *
      * DISPLAY INFO WINDOW IF COMMISSION HAS BEEN POSTED TO A PRIOR MONTH FOR
      * THE AGENT HISTORY RECORD BEING INACTIVATED.
      *
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     *IN90         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT
     C     ACKY          CHAIN     WMACL                              60
     C     *IN60         IFEQ      *OFF
     C     ACYRMN        ANDLT     SYYRMN
     C                   EXFMT     ACWDW
     C                   ENDIF                                                  *IN60=*OFF
     C  N90AHSAGT        IFNE      *ZEROS
     C     ACKY1         CHAIN     WMACL                              60
     C     *IN60         IFEQ      *OFF
     C     ACYRMN        ANDLT     SYYRMN
     C                   EXFMT     ACWDW
     C                   ENDIF                                                  *IN60=*OFF
      ***** BEG ADD ******* 01/22/10 **********************************
     C  N90AH3AGT        IFNE      *ZEROS
     C     ACKY1         CHAIN     WMACL                              60
     C     *IN60         IFEQ      *OFF
     C     ACYRMN        ANDLT     SYYRMN
     C                   EXFMT     ACWDW
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
      ***** END ADD ******* 01/22/10 **********************************
     C                   ENDIF                                                  AHSAGT<>*ZEROS
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  SCSTAT='A'
      ***** END ADD ***** 12/31/07 ************************************
      *
     C     SCSTAT        IFEQ      'I'
      *
      ******BEG ADD ***** 11/28/06 ************************************
      ***** BEG DEL ***** 12/11/07 ************************************
      **If*there*is other agent history records with a different agt#
      **Then*don't*allow Agent History to be re-activated.
     C***********AHKY2     SETLLWMAHL
     C*************        MOVE *OFF      *IN60
     C*************        Z-ADDAHAGT#    SVAGT#
     C*************        Z-ADDAHSAGT    SVSAGT
     C*************        MOVE ' '       DIFAGT  1
      *************
     C************IN60     DOWNE*ON
     C***********AHKY2     READEWMAHL                    60
     C***********AHAGT#    IFNE SVAGT#
      ******BEG*ADD ***** 10/10/07 ************************************
     C***********AHSTAT    ANDNE'I'
      ******END*ADD ***** 10/10/07 ************************************
     C***********AHSAGT    ORNE SVSAGT
     C*************        MOVE 'Y'       DIFAGT
     C*************        LEAVE
     C*************        ENDIF
     C***********AHKY2     READEWMAHL                    60
     C*************        ENDDO
      *************
     C***********DIFAGT    IFEQ 'Y'                        Have other agt#
     C*************        SETON                     46  90
     C*************        ENDIF
      *************
      ***** END DEL ***** 12/11/07 ************************************
      ***** END ADD ***** 11/28/06 ************************************
      **********
     C********** AAKY1     CHAINPAA01                60
      **********
     C********** *IN60     IFEQ *ON
     C********** AAEFDT    ORGT SCEFDT
     C**********           SETON                     71  90
     C**********           ENDIF                           *IN60=*ON
      **********
     C********** AACNDT    IFNE *ZEROS
     C********** AACNDT    ANDLTSCEFDT
     C**********           SETON                     71  90
     C**********           ENDIF                           AACNDT<>*ZEROS
      **********
      *****BEGIN ADD **** 03/08/02 ************************************
      **If*Activating Histry Record, use that msg (WCR3157) on Screen
      **Check*First Producer
     C**********           Z-ADDAHPROD    #XPROD
     C********** APKY      CHAINPAP01                60
     C********** *IN60     IFEQ *OFF
     C********** APCXDT    ANDGT*ZEROS
     C********** APCXDT    ANDLESCEFDT
     C**********           SETON                     72  90
     C**********           ENDIF                           *IN60=*ON
      **Check*Secondary Producer
     C********** *IN90     IFEQ *OFF
     C**********           Z-ADDAHSAGT    #XAGT#
     C**********           Z-ADDAHSPRD    #XPROD
     C********** AAKY3     CHAINPAA01                60
     C**N60***** APKY      CHAINPAP01                60
     C********** *IN60     IFEQ *OFF
     C********** APCXDT    ANDGT*ZEROS
     C********** APCXDT    ANDLESCEFDT
     C**********           SETON                     72  90
     C**********           ENDIF                           *IN60=*ON
     C**********           ENDIF
      **Check*Producer Agent of Record
     C********** *IN90     IFEQ *OFF
     C**********           Z-ADDAHAGRC    #XAGT#
     C**********           Z-ADDAHPRRC    #XPROD
     C********** AAKY3     CHAINPAA01                60
     C**N60***** APKY      CHAINPAP01                60
     C********** *IN60     IFEQ *OFF
     C********** APCXDT    ANDGT*ZEROS
     C********** APCXDT    ANDLESCEFDT
     C**********           SETON                     72  90
     C**********           ENDIF                           *IN60=*ON
     C**********           ENDIF
      **Check*Secondary Producer Agent of Record
     C********** *IN90     IFEQ *OFF
     C**********           Z-ADDAH2ARC    #XAGT#
     C**********           Z-ADDAH2RRC    #XPROD
     C********** AAKY3     CHAINPAA01                60
     C**N60***** APKY      CHAINPAP01                60
     C********** *IN60     IFEQ *OFF
     C********** APCXDT    ANDGT*ZEROS
     C********** APCXDT    ANDLESCEFDT
     C**********           SETON                     72  90
     C**********           ENDIF                           *IN60=*ON
     C**********           ENDIF
      *** END DEL ***** 12/31/07 **************************************
      **** END   ADD **** 03/08/02 ************************************
      *** BEG ADD ***** 12/31/07 **************************************
     C                   SETON                                        71  90
      *** END ADD ***** 12/31/07 **************************************
      *
     C                   ENDIF                                                  SCSTAT=I
      *_______________________________________________________________
      *
     C                   ENDSL
      ***  END   ADD  ***  06/07/98  *********************************
      *
     C                   ENDSR                                                  ERRCK1
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
      *
     C     ERRCK2        BEGSR
      *
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK2
     C*
     C***  BEGIN ADD  ***  07/13/00  *********************************
     C*================================================================
     C* GETAGT - Get the latest agent for employer
     C*================================================================
     C*
     C     GETAGT        BEGSR
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      *ZERO         ##FYR
     C                   PARM      *ZERO         ##AGT#
     C                   PARM      *BLANK        ##ANAM           60
     C*
     C                   ENDSR                                                  GETAGT
     C***  END   ADD  ***  07/13/00  *********************************
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
      *
     C     INZSST        BEGSR
      *
     C     @@MODE        IFEQ      'Y'
     C                   Z-ADD     ELFYR         SSFYR
     C                   ELSE
     C                   MOVE      *ZEROS        SSFYR
     C                   ENDIF                                                  @@MODE=Y
      *
     C     @@CALL        IFEQ      '2'
      *
     C                   MOVE      'A'           SSSTAT
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
      *
     C     KEYINZ        BEGSR
      *
     C                   MOVE      *HIVAL        KKFYR
     C                   MOVE      *HIVAL        KKEFDT
     C                   MOVE      *LOVAL        KKAGT#
      *
     C                   ENDSR                                                  KEYINZ
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
      *
     C     KEYSET        BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        KKFYR
     C                   MOVE      *HIVAL        KKEFDT
     C                   MOVE      *LOVAL        KKAGT#
     C                   ENDSL
      *
     C                   ENDSR                                                   KEYSET
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
      *
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCFYR         KKFYR
      *
     C     ##MODE        IFNE      'A'
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   ELSE
     C                   Z-ADD     *HIVAL        KKEFDT
     C                   CLEAR                   ##MODE
     C                   ENDIF                                                  ##MODE<>A
      *
     C                   Z-ADD     SCAGT#        KKAGT#
      *
     C                   ENDSR                                                  KEYSFL
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
      *
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     AHFYR         SCFYR
     C                   Z-ADD     AHEFDT        SCEFDT
     C                   Z-ADD     AHAGT#        SCAGT#
      ***  BEGIN ADD  ***  09/30/09  *********************************
     C                   MOVEL     *BLANKS       EDTMSK
     C                   MOVEL(P)  AHAGRC        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAOR#
      ***  END   ADD  ***  09/30/09  *********************************
      ***  BEGIN ADD  ***  06/07/98  *********************************
     C                   MOVE      AHSTAT        SCSTAT
      ***  END   ADD  ***  06/07/98  *********************************
      *
     C                   MOVEL     *BLANKS       EDTMSK
      *
     C                   MOVEL(P)  AHAGT#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAGT#
      *
     C                   MOVEL(P)  AHRT#         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRT#
      *
     C                   MOVEL(P)  AHPROD        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXPROD
      *
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZEFDT            8 0
      *
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   MOVEL(P)  ZZEFDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEFDT
      *
     C     AAKY          CHAIN     PAA01                              60
      ***  BEGIN ADD  ***  11/04/97  *********************************
     C                   Z-ADD     DBRRNA        SCAARR
      ***  END   ADD  ***  11/04/97  *********************************
      *
     C  N60AGKY          CHAIN     PAG01                              60
     C  N60              MOVEL(P)  AGNAME        XXNAME
      *
     C                   MOVEL     DATA          SCDATA
      *
     C                   ENDSR                                                  MVDBSC
      *
      *================================================================
      * OPT9 - Processing for OPT 9
      *================================================================
      *
      * OPTION 9=QUOTE STATUS
      *
     C     OPT9          BEGSR
      *
      *  IF IT DOESN'T ALREADY EXIST CREATE AN AGENT QUOTE RECORD
      *  BEFORE OPTION IS EXECUTED TO ENTER THE QUOTE MAINTENANCE
      *  PROGRAM
      *
     C                   MOVE      SCFND         QQFND             3 0
      *
     C     QTKY1         CHAIN(N)  PQT01                              60        CHAIN W/ AGT# TOO
     C*
     C***  BEGIN ADD  ***  07/13/00  *********************************
     C                   EXSR      GETAGT
     C                   Z-ADD     ##AGT#        XXAGT2
     C*
     C   60SCAGT#        IFNE      XXAGT2
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  SCAGT#<>XXAGT2
     C***  END   ADD  ***  07/13/00  *********************************
      *
     C     *IN60         IFEQ      *ON
      ***  BEGIN ADD  ***  02/18/98  *********************************
     C     *IN09         ANDEQ     *OFF                                         NOT INQUIRY
      ***  END   ADD  ***  02/18/98  *********************************
      *
      ***  BEGIN ADD  ***  04/10/98  *********************************
     C                   EXSR      EMPAGT
      *
     C     OLDAGT        IFEQ      'N'
      ***  END   ADD  ***  04/10/98  *********************************
      *
      ***  BEGIN ADD  ***  11/04/97  *********************************
     C                   CLEAR                   PQT01
      ***  END   ADD  ***  11/04/97  *********************************
      *
     C                   Z-ADD     EMCO#         QTCO#
     C                   Z-ADD     EMFND         QTFND
     C                   Z-ADD     EMEMP#        QTEMP#
     C                   Z-ADD     EMDIV         QTDIV
     C                   Z-ADD     SCAGT#        QTAGT#
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTWKDT
      ***  BEGIN ADD  ***  11/04/97  *********************************
     C                   MOVEL     SRDTO         QTORDT
      ***  END   ADD  ***  11/04/97  *********************************
      *
     C                   Z-ADD     EMEFD1        QTEFDT
     C                   Z-ADD     EMCHAP        QTCHAP
     C                   MOVEL     EMNAME        QTNAME
      *
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      'P'           QTTYPE
     C                   ELSE
     C                   MOVE      'W'           QTTYPE
      ***  BEGIN ADD  ***  11/04/97  *********************************
     C                   MOVEL     SRDTO         QTNBDT
      ***  END   ADD  ***  11/04/97  *********************************
     C                   ENDIF                                                  @@QTE=Q
      *
     C                   Z-ADD     *ZEROS        QTPREM
     C                   MOVE      *BLANKS       QTCARR
     C                   MOVE      *BLANKS       QTCOMM
      *
     C                   TIME                    DSDTTM
      *
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           QTSTAT
     C                   ELSE
     C                   MOVE      'W'           QTSTAT
     C                   ENDIF                                                   @@QTE=Q
      *
      ***** BEG DEL ******* 10/26/01 **********************************
     C*********            MOVE Q1USER    QTUSER
     C*********            MOVE Q1PGM     QTPGM
      *****BEGIN ADD  ***  11/04/97  *********************************
     C*********            MOVELQ1USER    QTIUSR    P
     C*********            MOVELQ1PGM     QTIPGM    P
      *****END*  ADD  ***  11/04/97  *********************************
      *********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     QTDT
      *****BEGIN ADD  ***  11/04/97  *********************************
     C*********            MOVELSRDTO     QTIDT
      *****END*  ADD  ***  11/04/97  *********************************
      *********
     C*********            Z-ADDDSTIME    QTTIME
      *****BEGIN ADD  ***  11/04/97  *********************************
     C*********            Z-ADDDSTIME    QTITM
      *****END*  ADD  ***  11/04/97  *********************************
      ***** END DEL ******* 10/26/01 **********************************
      *
      ***** BEG ADD ******* 10/26/01 **********************************
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
     C                   Z-ADD     DSTIME        QTITM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C                   MOVEL     SRDTO         QTIDT
      *
      ***** END ADD ******* 10/26/01 **********************************
     C                   WRITE     PQT01
      *
      ***  BEGIN ADD  ***  04/10/98  *********************************
     C                   ENDIF                                                  OLDAGT=N
      ***  END   ADD  ***  04/10/98  *********************************
      *
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   CALL      'W4150R'                             99
      ***  BEGIN CHG  ***  11/04/97  *********************************
      **********           PARM           SCPRDS
      **********           PARM           @@CALL
      **********           PARM           QQFND
      **********           PARM           SCAGT#
      **********           PARM           @@QTE
      ********** *INKC     PARM           ##KC    1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
      ***  BEGIN CHG  ***  01/21/98  *********************************
      **********           PARM DBRRNA    ##RRN   90
     C                   PARM      SCAARR        ##RRN             9 0
      ***  END   CHG  ***  01/21/98  *********************************
     C                   PARM      @@QTE         ##QTE             1
      ***  END   CHG  ***  11/04/97  *********************************
      *
     C                   ENDSR
      *
      *
      *===============================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *===============================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR                                                  OPT20
      *
      ***  BEGIN ADD  ***  09/30/09  *********************************
      *===============================================================
      * OPT80 - SUBROUTINE FOR OPTION 80
      *===============================================================
      *
      * OPTION 80=INQUIRY ACCESS TO AGENT
      *
     C     OPT80         BEGSR
      *
     C                   CALL      'W0040R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
      ***  BEG   CHG  ***  11/06/09  *********************************
     C****                 PARM '2'       ##CALL  1
     C                   PARM      '1'           ##CALL            1
      ***  END   CHG  ***  11/06/09  *********************************
     C                   PARM      SCDBRR        ##RRN             9 0
      ***  BEG   CHG  ***  11/06/09  *********************************
     C****                 PARM 2         ##VIEW  10
     C                   PARM      1             ##VIEW            1 0
      ***  END   CHG  ***  11/06/09  *********************************
     C                   PARM                    ##QTE             1
     C                   PARM      '*'           ##MODE            1
      *
     C                   ENDSR                                                  OPT80
      *
      *===============================================================
      ***  END   ADD  ***  09/30/09  *********************************
      ***  BEGIN ADD  ***  04/01/98  *********************************
      *===============================================================
      * F14 - SUBROUTINE FOR F14
      *===============================================================
      *
      *  F14=DARTS
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      EMEMP#        QCEMP#
     C                   MOVE      EMDIV         QCDIV
      ******* BEGIN ADD ****** 08/07/98 *******************************
     C  N31              MOVE      *BLANKS       QCQTE
     C   31              MOVEL     'Q'           QCQTE
      ******* END   ADD ****** 08/07/98 *******************************
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      ***  END   ADD  ***  04/01/98  *********************************
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
      *
     C     OTHFKS        BEGSR
      *
     C     *INKI         CASEQ     *ON           KIDO
     C     *INKN         CASEQ     *ON           KNDO                 90
     C                   ENDCS
      *
     C                   ENDSR
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
      *
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '20'          OPT20
     C**** BEGIN ADD **** 09/30/09 ************************************
     C     SCSEL         CASEQ     '80'          OPT80
     C**** END   ADD **** 09/30/09 ************************************
     C                   ENDCS
      *
     C                   ENDSR
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
      *
     C     PGMEND        BEGSR
     C**** BEGIN ADD **** 03/28/01 ************************************
      *
      *  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      *
     C                   EXSR      CKCHGS
     C**** END   ADD **** 03/28/01 ************************************
      *
     C*** BEG ADD ***** 12/31/07 **************************************
     C     *LIKE         DEFINE    AHFYR         SVFYR
     C     *LIKE         DEFINE    AHEFDT        SVEFDT
     C                   Z-ADD     *ZERO         SVEFDT
     C                   Z-ADD     *ZERO         SVFYR
     C     KEY1          SETLL     PAH03
     C     KEY1          READE     PAH03                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C*** BEGIN ADD  ***  06/30/11  ***********************************
     C     @@MODE        IFEQ      'E'
     C     AHFYR         OREQ      ELFYR
     C*** END ADD ******  06/30/11  ***********************************
     C     AHFYR         IFNE      SVFYR
     C                   Z-ADD     AHFYR         SVFYR
     C                   Z-ADD     0             CNT               3 0
     C                   ENDIF
      *
     C     AHEFDT        IFNE      SVEFDT
     C                   Z-ADD     AHEFDT        SVEFDT
     C                   Z-ADD     0             CNT               3 0
     C                   ENDIF
      *
     C     AHSTAT        IFEQ      'A'
     C*** BEG ADD ***** 08/25/09 ************************************
     C*** BEGIN DEL  ***  06/30/11  ***********************************
     C*********  AHFYR     ANDNESVFYR
     C*** END DEL ******  06/30/11  ***********************************
     C*** END DDD ***** 08/25/09 ************************************
     C                   ADD       1             CNT
     C                   ENDIF
      *
     C     CNT           IFEQ      2
      *** BEG ADD ***** 12/18/09 ************************************
     C*** BEGIN DEL  ***  06/30/11  ***********************************
     C*********  AHFYR     ANDEQELFYR
     C*** END DEL ******  06/30/11  ***********************************
      *** END ADD ***** 12/18/09 ************************************
     C                   MOVEL(P)  MSG(6)        WDMSG1
      *** BEG CHG ***** 04/10/08 ************************************
     C**********           MOVELMSG,7     WDMSG2    P
     C**********           MOVELMSG,8     WDMSG3    P
     C**********           EXFMTWDWMSG
     C**********           MOVE *ON       RSTPGM
     C**********           MOVE *OFF      OUTFLG
     C                   SELECT
     C     *IN09         WHENEQ    *ON
     C                   MOVEL(P)  MSG(9)        WDMSG2
     C                   EXFMT     WDWMSG
     C                   OTHER
     C                   MOVEL(P)  MSG(7)        WDMSG2
     C                   MOVEL(P)  MSG(8)        WDMSG3
     C                   EXFMT     WDWMSG
     C                   MOVE      *ON           RSTPGM
     C                   MOVE      *OFF          OUTFLG
     C                   ENDSL
      *** END CHG ***** 04/10/08 ************************************
     C                   LEAVE
     C                   ENDIF
     C*** BEGIN ADD  ***  06/30/11  ***********************************
     C                   ENDIF                                                  @@MODE IFEQ 'E'
     C*** END ADD ******  06/30/11  ***********************************
      *
     C     KEY1          READE     PAH03                                  60
     C                   ENDDO
      *
     C                   Z-ADD     0             CNT
      *** BEG CHG ***** 12/09/09 ************************************
     C                   MOVE      SCEFDT        ##EFDT
      *** END CHG ***** 12/09/09 ************************************
     C/EXEC SQL
     C+ SELECT COUNT(*) INTO :CNT
     C+   FROM WMAHP A JOIN WMARP ON
     C+     A.AHCO# = ARCO# AND
     C+     A.AHFND = ARFND AND
     C+     A.AHRT# = ARRT# AND
      *** BEG CHG ***** 12/09/09 ************************************
     C+     A.AHEFDT = AREFDT AND
      *** BEG CHG ***** 12/09/09 ************************************
     C+     ARSTAT <> 'I'
     C+   WHERE EXISTS(SELECT * FROM WMAHP B
     C+                  WHERE A.AHCO# = B.AHCO# AND
     C+                        A.AHFND = B.AHFND AND
     C+                        A.AHEMP# = B.AHEMP# AND
     C+                        A.AHDIV = B.AHDIV AND
     C+                        A.AHFYR = B.AHFYR AND
     C+                        (A.AHAGT# = B.AHSAGT OR
      *** BEG ADD ***** 01/22/10 ************************************
     C+                        A.AHAGT# = B.AH3AGT) AND
      *** END ADD ***** 01/22/10 ************************************
     C+                        A.AHSTAT = 'A' AND
     C+                        B.AHSTAT = 'A') AND
     C+   A.AHCO#  = :EMCO# AND
     C+   A.AHFND  = :EMFND AND
     C+   A.AHEMP# = :EMEMP# AND
     C+   A.AHDIV  = :EMDIV  AND
      *** BEG CHG ***** 12/09/09 ************************************
     C+   A.AHEFDT = :##EFDT AND
      *** END CHG ***** 12/09/09 ************************************
     C+   ARNWRT <> 0
     C/END-EXEC
      *
     C     CNT           IFGT      *ZERO
     C                   MOVEL(P)  MSG(1)        WDMSG1
     C                   MOVEL(P)  MSG(2)        WDMSG2
     C                   MOVEL(P)  MSG(3)        WDMSG3
     C                   MOVEL(P)  MSG(4)        WDMSG4
      *** BEG CHG ***** 04/10/08 **************************************
     C*****************    MOVELMSG,5     WDMSG5    P
     C*****************    EXFMTWDWMSG
     C*****************    MOVE *ON       RSTPGM
     C*****************    MOVE *OFF      OUTFLG
      *
     C                   SELECT
     C     *IN09         WHENEQ    *ON                                          INQUIRY MODE
     C     WDMSG4        CAT       '.':0         WDMSG4
     C                   EXFMT     WDWMSG
     C                   OTHER
     C                   MOVEL(P)  MSG(5)        WDMSG5
     C                   EXFMT     WDWMSG
     C                   MOVE      *ON           RSTPGM
     C                   MOVE      *OFF          OUTFLG
     C                   ENDSL
      *** END CHG ***** 04/10/08 **************************************
     C                   ENDIF
     C*** END ADD ***** 12/31/07 **************************************
      *
     C                   ENDSR
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
      *
     C     PGMSTR        BEGSR
      *
      ***  BEGIN ADD  ***  02/18/98  *********************************
     C                   MOVE      *OFF          *IN09
      *
     C     @@CALL        IFEQ      '1'
     C     @@CALL        OREQ      '2'
     C                   MOVE      *ON           *IN09
     C                   ENDIF                                                  @@CALL=1/2
      ***  END   ADD  ***  02/18/98  *********************************
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'E'
     C     @@RRN         CHAIN     WMEMP                              60
     C                   Z-ADD     @@RRN         ##RRN2
      *
      **
     C     @@MODE        WHENEQ    'Y'
     C     @@MODE        OREQ      '*'
      *
     C     @@RRN         CHAIN     WDELP                              60
     C     EMKY          CHAIN     PEM01                              60
      *
     C     @@MODE        IFNE      'Y'
     C                   Z-ADD     DBRRNE        ##RRN2
     C                   ELSE
     C                   Z-ADD     @@RRN         ##RRN2
     C                   ENDIF                                                   @@MODE<>Y
      *
      ***  BEGIN DEL  ***  05/26/98  *********************************
      **********           MOVE @@MODE    ##CALL
      ***  END   DEL  ***  05/26/98  *********************************
      *
     C                   ENDSL
      *
      ***  BEGIN ADD  ***  05/26/98  *********************************
     C                   MOVE      @@MODE        #@CALL
      ***  END   ADD  ***  05/26/98  *********************************
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XZNAME
     C                   PARM                    XXEDN            80
      *
     C                   Z-ADD     EMFND         SCFND
     C                   MOVEL(P)  XXEDN         SCEDN
      *
      *  GET GROUP INFORMATION
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
      *
     C                   CLEAR                   @@RRN
      *
      ***  BEGIN ADD  ***  07/10/98  *********************************
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           XXSTAT
     C                   ELSE
     C                   MOVE      'W'           XXSTAT
     C                   ENDIF                                                   @@QTE=Q
      ***  END   ADD  ***  07/10/98  *********************************
     C*
     C***  Begin Add  ***  09/28/09  **************************************
     C* Check for Quote Transfer records.  If exists, remove F6.
     C     @@QTE         IFNE      'Q'
     C                   EXSR      CKQTXF
     C                   ENDIF
     C***  End   Add  ***  09/28/09  **************************************
      *
      ****** BEGIN ADD ****** 08/07/98 *******************************
     C     @@QTE         COMP      'Q'                                    31    QUOTE
      ****** END   ADD ****** 08/07/98 *******************************
      *
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     EXKY          CHAIN     PEX01                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N92              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 08/10/98 **********************************
     C**** BEGIN ADD **** 03/28/01 ************************************
     C                   MOVE      *HIVAL        KKFYR
     C                   MOVE      *HIVAL        KKEFDT
     C                   MOVE      *HIVAL        KKAGT#
     C     KEY1          SETGT     PAH03
     C     KEY1          READPE    PAH03                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     KEY1          READPE    PAH03                                  60
     C                   END
     C                   Z-ADD     AHAGT#        SAVAGT
     C**** END   ADD **** 03/28/01 ************************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
      *
     C     READ          BEGSR
      *
     C                   CLEAR                   DATA
      *
     C   21KEY1          READE     DBRCDF                                 95
      *
     C                   ENDSR
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
      *
     C     READP         BEGSR
      *
     C                   CLEAR                   DATA
      *
     C   21KEY1          READPE    DBRCDF                                 95
      *
     C                   ENDSR
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
      *
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C                   ENDSR
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
      *
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C                   ENDSR
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
      *
     C     SF1UPD        BEGSR
     C                   ENDSR
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
      *
     C     SLCTRD        BEGSR
      *
     C                   Z-ADD     SCDBRR        @@RRN
      *
     C                   ENDSR
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
      *
     C     SUBSET        BEGSR
      *
     C     SSFYR         IFNE      *ZEROS
     C     SSFYR         ANDNE     AHFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     AHSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
      *
      ***  BEGIN ADD  ***  04/10/98  *********************************
      *================================================================
      * WRITEQ - Write records to the WAQTP file
      *================================================================
      *
     C     WRITEQ        BEGSR
      *
     C                   Z-ADD     X7DATE        QTWKDT
      *
      ***** BEG DEL ******* 10/26/01 **********************************
     C*********            MOVELQ1USER    QTIUSR    P
     C*********            MOVELQ1PGM     QTIPGM    P
     C*********            Z-ADDX7DATE    QTIDT
     C*********            Z-ADDDSTIME    QTITM
      ***** END DEL ******* 10/26/01 **********************************
      *
      ***** BEG ADD ******* 10/26/01 **********************************
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
     C                   Z-ADD     DSTIME        QTITM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C                   MOVEL     SRDTO         QTIDT
      *
      ***** END ADD ******* 10/26/01 **********************************
      *
     C                   WRITE     PQT01
      *
     C                   ENDSR                                                  WRITEQ
      ***  END   ADD  ***  04/10/98  *********************************
      *
     C*** BEGIN ADD ***  09/28/07  ************************************
     OPQT01     E            PQT01E
     C*** END ADD *****  09/28/07  ************************************
** Column headings
Opt Year   Eff Date   Agent  Name                  Rate#  Producer   AOR Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1099=Quote Status           YYN
12020=Activate/Inactivate   YNN
18080=Agent Inquiry         YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=Darts                YYN
217F17=Subset               YYY
3
**
The primary agent and producer are the secondary
agent and producer in another active record or the
secondary agent and producer are the primary agent and
producer on another record. This condition must be fixed
prior to exiting W/W Agent History.
Multiple active records exist with the same effective
date. This condition must be fixed prior to exiting
W/W Agent History.
date. This condition must be fixed.
