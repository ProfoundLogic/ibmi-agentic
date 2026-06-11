     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0051R1 - EMPLOYER MID-YEAR ENDORSEMENTS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF AVAILABLE  *
     H*                MID-YEAR ENDORSEMENTS THAT MAY BE PERFORMED    *
     H*                UPON AN EMPLOYER.                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/11/12 C:136 SUPPORT: WA133 79890                           *
     H*                PROCESS DELETIONS WITH MOST CURRENT YEAR       *
     H*                AS OF THE ENDORSEMENT DATE                     *
     H*                                                               *
     H* 04/04/12 C:180 SUPPORT 79400 - Prevent dup divisions.
     H*
     H* 07/11/11 C:190 Project WC1982                                 *
     H*                Removed the section of code that checks        *
     H*                user's security level to determine whether     *
     H*                they can override an error message window that *
     H*                is displayed if the policy is pending cancell- *
     H*                ation based upon the Endorsement Eff Date.     *
     H*                                                               *
     H* 05/06/11 C:180 SUPPORT 76815
     H*                Allow rescision of a deletion of a division
     H*                on policy expiration date.
     H*
     H* 10/20/10 C:220 SUPPORT: 73864                                 *
     H*                Allow deletion of division/location            *
     H*                on policy expiration date.                     *
     H*                                                               *
     H* 11/04/08 C:190 PROJECT: WC1700                                *
     H*                RELEASE RECORD FROM WMMUL6 WHEN GETTING        *
     H*                CURRENT ENDORSEMENT # FOR FYR BEING PROCESSED  *
     H*                                                               *
     H* 11/06/07 C:136 SUPPORT: WA139 60855                           *
     H*                RESET WDELP VALUES BACK TO THE DIVISION BEING  *
     H*                PROCESSED AFTER ALL DIVISIONS ARE CHECKED      *
     H*                FOR A CARRIER POLICY NUMBER.                   *
     H*                                                               *
     H* 10/31/06 C:180 114WMF - Require carrier #.
     H*
     H* 04/03/06 C:61  SUPPORT WA109 51864                            *
     H*                Position cursor to "Position to" field at      *
     H*                program start.                                 *
     H*                                                               *
     H* 01/19/06 C:61  SUPPORT WA132 50944                            *
     H*                Change so WMMUP records for mid-term process   *
     H*                aborted by users are not kept if no subsequent *
     H*                WMMUP record exists for same policy.           *
     H*                                                               *
     H* 12/15/05 C:65  PROJECT 139WBT                                 *
     H*                STORE THE NAME OF THE PROCESSING PROGRAM IN    *
     H*                WMMUP FOR EXPORT PROCESS.                      *
     H*                                                               *
     H* 10/28/05 C:33  Seq#: WA126/49343                              *
     H*               -If F3/F12 is pressed prior to completion of    *
     H*                 the endorsement, inform the user and allow    *
     H*                 override to finalize the endorsement.         *
     H*               -Write WMMUP file as soon as the endorsment #   *
     H*                 is retreived.  If the endorsment doesn't      *
     H*                 process, soft delete the record.              *
     H*                                                               *
     H* 08/17/05 C:61  PROJECT WC1266                                 *
     H*                Allow "Rescission Division Deletion" for       *
     H*                cancelled divisions only.                      *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288 (SUMMARIZE S/R POLICIES)       *
     H*                ADD CODE TO PREVENT DELETION OF DIVISION       *
     H*                IS A/R EXISTS IN STATE REPORTING DIVISION.     *
     H*                REFERENCE 43540                                *
     H*                                                               *
     H* 06/16/03 C:61  SUPPORT WA091 37520                            *
     H*                Do not allow revised bills for CX policy.      *
     H*                (Reverse change made seq# 31442)               *
     H*                                                               *
     H* 03/04/02 C:09  PROJECT: WA109/SEQ # 31425                     *
     H*                Correct program logic to properly recognize    *
     H*                when a user has pressed F3=Exit or F12=Cancel  *
     H*                before the endorsement has been completed.     *
     H*                                                               *
     H* 02/26/02 C:90  PROJECT: SEQ#WA100/31442                       *
     H*                Only run "cancelled" error checks in the ERRCK1*
     H*                subroutine if the current date is greater than *
     H*                the cancel date.                               *
     H*                                                               *
     H* 08/27/01 C:90  PROJECT: SEQ#:WC090/28478                      *
     H*                Add edits to not allow a midterm endorsement   *
     H*                on a cancelled policy. If the employer is      *
     H*                pending cancellation, allow the user to be     *
     H*                able to bypass the message.                    *
     H*                                                               *
     H* 07/05/01 C:90  SUPPORT: WA112/27471                           *
     H*                Removed special process on Deletion of Division*
     H*                mid-term endorsement to ensure that the        *
     H*                endorsement effective date corresponds with    *
     H*                the policy effective date if it falls between  *
     H*                policy years.                                  *
     H*                                                               *
     H* 06/22/01 C:90  SUPPORT: WA091/27956                           *
     H*                Corrected flag for ADDITION OF DIVISION edit.  *
     H*                                                               *
     H* 05/08/01 C:90  SUPPORT: WA112/27403                           *
     H*                Added an edit to not allow an ADDITION OF      *
     H*                DIVISION on a cancelled policy.                *
     H*                                                               *
     H* 02/02/01 C:90  PROJECT: WA112/25757                           *
     H*                Assign the next endorsement number across all  *
     H*                divisions.                                     *
     H*                                                               *
     H* 01/31/01 C:90  SUPPORT: WA095/26083                           *
     H*                Removed DIV from SHKY when determining if a    *
     H*                policy page has been generated.                *
     H*                                                               *
     H* 10/24/00 C:09  SUPPORT: WA095/#24104                          *
     H*                Need to allow user to designate an end eff dt  *
     H*                equal to the policy expiration date if         *
     H*                processing a Deletion of Division mditerm end. *
     H*                                                               *
     H* 04/07/00 C:09  SUPPORT: WA088/#22472                          *
     H*                If client does not have WCPOLS, skip the edit  *
     H*                that verifies the policy information page has  *
     H*                been produced prior to allowing a midterm end. *
     H*                                                               *
     H* 09/29/99 C:113 SUPPORT: WA048/#19848                          *
     H*                Adjusted edit for policy page generation to    *
     H*                check submission history records for 'P' types *
     H*                to determine if a policy page was generated.   *
     H*                                                               *
     H* 09/20/99 C:113 SUPPORT: WA033/#19610                          *
     H*                Move edit checking for policy page generation  *
     H*                to after the endorsment effective date has     *
     H*                been entered and use that date to check the    *
     H*                appropriate policy year.                       *
     H*                                                               *
     H* 08/23/99 C:113 SUPPORT: #19371                                *
     H*                Adjusted Policy Page edit to check for SHSTAT  *
     H*                > 2 when checking for Policy Page issuance.    *
     H*                                                               *
     H* 08/03/99 C:113 PROJECT: CM027                                 *
     H*                Added edit to not allow the processing of a    *
     H*                midterm endorsement if a Policy Page has not   *
     H*                been generated yet.                            *
     H*                                                               *
     H* 07/16/99 C:113 SUPPORT: #18882                                *
     H*                Removed edit on Deletion of Division for       *
     H*                users security level.                          *
     H*                                                               *
     H* 07/08/99 C:113 SUPPORT: #18683                                *
     H*                Changed to recognize an F3 or F12 from         *
     H*                processing program and to not print            *
     H*                endorsement or write WMMUP records.            *
     H*                                                               *
     H* 06/29/99 C:113 SUPPORT: #18651                                *
     H*                Added edit to not allow deletion of a          *
     H*                cancelled division.                            *
     H*                                                               *
     H*  05/20/99 C:113 PROJECT: CM027                                *
     H*                 Reimplement deletion of division.  Prior to   *
     H*                 calling W0051R20, make sure there are no      *
     H*                 claims with accident dates after the          *
     H*                 endorsement effective date, which will become *
     H*                 the division's cancel date.                   *
     H*                                                               *
     H*  04/13/99  C:09  PROJECT: CM027                               *
     H*                  MIDTERM OWNER/OFFICER ENDORSEMENT REWRITE    *
     H*                                                               *
     H*  01/27/99  C:09  PROJECT: SUPP (WA091/SEQ# 16259)             *
     H*                  NEED EDIT TO PREVENT A MIDTERM REVISED       *
     H*                  BILLING ON AN AUDITED POLICY.                *
     H*                                                               *
     H*  01/19/99  C:09  PROJECT: SUPP (WA081/SEQ# 15603)             *
     H*                  PROGRAM MUST ALLOW FOR ENDORSEMENTS TO       *
     H*                  FUTURE POLICY YEARS IF THE POLICY PAGE       *
     H*                  HAS ALREADY BEEN ISSUED.                     *
     H*                                                               *
     H*  09/04/98  C:65  PROJECT: WC705                               *
     H*                  DO NOT ALLOW REVISED BILLS FOR A CX'D POLICY *
     H*                                                               *
     H*  07/07/98  C:57  PROJECT: WA091 12559                         *
     H*                  DO NOT PROCESS A POLICY YEAR IF THE ENDORSE  *
     H*                  DATE IS THE POLICY END DATE.                 *
     H*                                                               *
     H*  06/10/98  C:57  PROJECT: CM027 SUPPORT                       *
     H*                  ADD SAME EDITS FOR DEL OF A DIV AS IN W0050R *
     H*                                                               *
     H*  03/18/98  C:57  PROJECT: CM027                               *
     H*                  FORCE ENDORSEMENT DATE INTO AN 8 POS FORMAT. *
     H*                                                               *
     H*****************************************************************
     F*
     FW0051F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWTMYP     IF   E             DISK
     F                                     RENAME(PMY01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWTMYL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMY01:DBRCDF)
     F*
     FWMEMP     IF   E             DISK
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS9)
      *++++++ BEGIN ADD +++ 10/31/06 ++++++++++++++++++++++++++++++++
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *** BEG ADD ***** 05/11/12 ************************************
     FWDELL2    IF   E           K DISK
     F                                     RENAME(PEL01:PELL2)
      *** END ADD ***** 05/11/12 ************************************
     FWMRPL     IF   E           K DISK
     FWMGCL     IF   E           K DISK
      *++++++ END ADD +++++ 10/31/06 ++++++++++++++++++++++++++++++++
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F**** BEGIN CHG **** 02/02/01 ************************************
     F*****WMMUL1  IF  E           K        DISK                      A
     F***  Begin Chg  ***  10/28/05  **************************************
     F**********WMMUL6  IF  E           K        DISK                      A
     FWMMUL6    UF A E           K DISK
     F***  End   Chg  ***  10/28/05  **************************************
     F**** END   CHG **** 02/02/01 ************************************
     FWMENL3    IF   E           K DISK
     F*** BEG ADD ***** 06/10/98 ***********************************************
      *** BEGIN DEL *** 07/16/99 ***********************************************
     F***** WMEXL   IF  E           K        DISK
      *** END   DEL *** 07/16/99 ***********************************************
     FWMUPL     IF   E           K DISK
     F*
     F*** BEGIN DEL *** 05/20/99 **************************************
     F***** WMCML   IF  E           K        DISK
     F***** WMEAL   IF  E           K        DISK
     F***** WDEAL5  IF  E           K        DISK
     F*****             PEA01                             KRENAMEPEA02
      *** BEGIN CHG *** 08/03/99 ***********************************************
      **** BEGIN CHG **** 01/31/01 ************************************
     F******WMSHL13 IF  E           K        DISK
     FWMSHL1    IF   E           K DISK
      **** END   CHG **** 01/31/01 ************************************
      *** END   CHG *** 08/03/99 ***********************************************
     F*** END   DEL *** 05/20/99 **************************************
     F** BEG ADD ***** 04/08/05 ***************************************
     FWMEAL     IF   E           K DISK
     F** BEG ADD ***** 04/08/05 ***************************************
     F*
     FSSYLF     IF   E           K DISK
      *** BEGIN ADD *** 04/07/00 **************************************
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *** END ADD ***** 04/07/00 **************************************
     FSDCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     F*** END ADD ***** 06/10/98 ***********************************************
     F*
     F*** BEGIN ADD *** 05/20/99 **************************************
     FWMCMLC    IF   E           K DISK
     F*** BEGIN ADD *** 05/20/99 **************************************
     F*
      *** BEGIN ADD *** 08/03/99 ***********************************************
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
      *** END   ADD *** 08/03/99 ***********************************************
     F***** BEGIN ADD ****** 04/04/12 *********************************
     FWXRIL1    IF A E           K DISK
     F***** END ADD   ****** 04/04/12 *********************************
      /COPY SCOPYWW01D
     D*================================================================
     D*  Array and Table Definitions
     D*================================================================
     D HD              S             79    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(4) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(4) ALT(OF)
     D*
     D*** BEGIN ADD *** 05/20/99 **************************************
     D*** END ADD ***** 06/10/98 ***********************************************
     D*
     D*** BEGIN ADD *** 05/20/99 **************************************
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              CLAIM PMTS
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*** END   ADD *** 05/20/99 **************************************
     D*
     D***PY SCOPYWW01I
     D*
     D*** BEG ADD ***** 04/04/12 ***********************************************
     D                 DS
     D  XSDTTM                 1     12  0
     D  XSTIME                 1      6  0
     D  XSDATE                 7     12  0
     D*** END ADD ***** 04/04/12 ***********************************************
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5)
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS                  INZ
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*** BEG ADD ***** 06/10/98 ***********************************************
     D                 DS
     D  EATRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D*
     D                 DS
     D  EAPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D*
     D                 DS
     D  XXDATE                 1      7  0
     D  XXCUYM                 1      5  0
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*** END ADD ***** 06/10/98 ***********************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Midterm End-
     D                                     orsements')
     D AUDIT           C                   CONST('Audited')
     D***  Begin Add  ***  10/28/05  **************************************
     D DELETD          C                   CONST('Endorsement not -
     D                                     completed')
     D***  End   Add  ***  10/28/05  **************************************
     D EST             C                   CONST('Estimated')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CMPRES          C                   CONST('WA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)

     I*** END   ADD *** 05/20/99 **************************************
     I*
     I***PY SCOPYWW01E
     I*
     I*================================================================
     I*  Data Structure and Constant Definitions
     I*================================================================
     I*
     I*** BEG ADD ***** 06/10/98 ***********************************************
     IPCM03
     I              CMKEY                       C3KEY
     I              CMSEQ#                      C3SEQ#
     I              CMSTAT                      C3STAT
     I              CMPGM                       C3PGM
     I              CMTIME                      C3TIME
     I              CMDT                        C3DT
     I              CMDCDT                      C3DCDT
     I              CMDIFL                      C3DIFL

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM      ELCO#         ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#
     C                   PARM      ELDIV         ##DIV
     C                   PARM      ELFYR         ##FYR
     C     SCAGT#        PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD
     C                   PARM                    ##PNAM           60
     C*
     C*  Compress the employer header information parameter list
     C*
     C     CMPRPL        PLIST
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C*  Date Conversion parameter list
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Processing Program(s) parameter list
     C*
     C     PROCPL        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@RRN         ##RRN             9 0          PEM01 RRN
     C                   PARM      DBRRN9        ##RRN1            9 0          PEL01 RRN
     C                   PARM      SCDBRR        ##RRN2            9 0          PMY01 RRN
     C                   PARM      SSENDT        ##ENDT            8 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKSORT
     C*
     C*  EMPLOYER DETAIL
     C*
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*** BEG ADD ***** 05/11/12 **************************************
     C     ELKYA         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    XXENDT
     C*** END ADD ***** 05/11/12 **************************************
     C*
     C     ELKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C******** BEGIN ADD ****** 01/27/99 ******************************
     C     ELKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYRS
      *
      *++++++ BEGIN ADD +++ 10/31/06 ++++++++++++++++++++++++++++++++
     C     ELKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *++++++ END ADD +++++ 10/31/06 ++++++++++++++++++++++++++++++++
     C*
      *** BEG ADD ***** 11/06/07 **************************************
     C     ELKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
      *** END ADD ***** 11/06/07 **************************************
     C*
     C******** END ADD ******** 01/27/99 ******************************
     C*
     C*  COMPANY MASTER
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  EMPLOYER ENDORSEMENT FORMS
     C*
     C     MUKY          KLIST
     C**** BEGIN CHG **** 02/02/01 ************************************
     C*****************    KFLD           KKCO#
     C*****************    KFLD           KKFND
     C*****************    KFLD           KKEMP#
     C*****************    KFLD           KKDIV
     C*****************    KFLD           ELFYR
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    ELFYR
     C                   KFLD                    KKEMP#
     C**** END   CHG **** 02/02/01 ************************************
     C**** BEGIN ADD **** 02/02/01 ************************************
     C     MUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    ELFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKEND#
     C**** END   ADD **** 02/02/01 ************************************
     C*
     C*  EMPLOYER ENDORSEMENT REQUESTS
     C*
     C     ENKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    XXEND#
     C*** BEG ADD ***** 06/10/98 ***********************************************
      *** BEGIN DEL *** 07/16/99 ***********************************************
     C**********
     C***KEY*LIST FOR ADMIN/DARTS FILE
     C**********
     C********** EXKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKFND
      *** END   DEL *** 07/16/99 ***********************************************
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     **** BEGIN ADD *** 05/20/99 **************************************
     C                   KFLD                    XXACDT
     C*
     C     CMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     **** END   ADD *** 05/20/99 **************************************
     C*
     *** BEG ADD ***** 04/08/05 ***************************************
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKFYR
     *** END ADD ***** 04/08/05 ***************************************
     C*
     **** BEGIN DEL *** 05/20/99 **************************************
     C**********
     C***EMPLOYER ACCOUNTS RECEIVABLE FILE
     C**********
     C********** EAKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKFND
     C**********           KFLD           KKEMP#
     C**********           KFLD           KKDIV
     C**********
      *** BEGIN CHG *** 08/03/99 ***********************************************
     C*  NCCI SUBMISSION HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C**** BEGIN DEL **** 01/31/01 ************************************
     C*******************  KFLD           XXDIV   50
     C**** END   DEL **** 01/31/01 ************************************
     C                   KFLD                    KKFYR
      *
      *** BEGIN ADD *** 09/20/99 ***********************************************
     C     SHKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C**** BEGIN DEL **** 01/31/01 ************************************
     C*******************  KFLD           XXDIV
     C**** END   DEL **** 01/31/01 ************************************
     C                   KFLD                    XXFYRS
      *** BEGIN ADD *** 09/29/99 **************************************
     C                   KFLD                    KKTYPE            1
      *** BEGIN ADD *** 09/29/99 **************************************
      *** END   ADD *** 09/20/99 ***********************************************
     C*
     C** Employer Master File
     C     EMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *** END   CHG *** 08/03/99 ***********************************************
     **** END   DEL *** 05/20/99 **************************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  DARTS REMARK DETAIL FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*** END ADD ***** 06/10/98 ***********************************************
     C*** BEGIN ADD ***  04/04/12  ************************************
     C     RIUKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*** END   ADD ***  04/04/12  ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MYCO#         KKCO#
     C     *LIKE         DEFINE    MYCO#         ##CO#
     C     *LIKE         DEFINE    MYFND         KKFND
     C     *LIKE         DEFINE    MYFND         ##FND
     C     *LIKE         DEFINE    ELEMP#        KKEMP#
     C     *LIKE         DEFINE    ELEMP#        ##EMP#
     C     *LIKE         DEFINE    ELDIV         KKDIV
     C     *LIKE         DEFINE    ELDIV         ##DIV
     C     *LIKE         DEFINE    ELFYR         KKFYR
     C     *LIKE         DEFINE    ELFYR         ##FYR
     C     *LIKE         DEFINE    MYSORT        KKSORT
     C     *LIKE         DEFINE    MYSORT        @@SORT
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C     *LIKE         DEFINE    MYDT          XXENDT
     C**** BEGIN ADD **** 02/02/01 ************************************
     C     *LIKE         DEFINE    MUEND#        KKEND#
     C**** END   ADD **** 02/02/01 ************************************
     C*
     C*** BEG DEL ***** 06/10/98 ***********************************************
     C********** *LIKE*    DEFN ELPTDT    XXDATE
     C*** END DEL ***** 06/10/98 ***********************************************
     C*
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C*
     C******** BEGIN ADD ****** 01/27/99 ******************************
     C     *LIKE         DEFINE    ELFYR         XXFYRS
     C******** END ADD ******** 01/27/99 ******************************
     C*
     **** BEGIN ADD *** 05/20/99 **************************************
     C     *LIKE         DEFINE    XXENDT        XXACDT
     **** END   ADD *** 05/20/99 **************************************
     C*** BEGIN ADD *** 10/20/10 ******************************
     C     *LIKE         DEFINE    *IN92         XXDEL
     C*** END   ADD *** 10/20/10 ******************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*
     C***  Begin Add  ***  10/28/05  **************************************
     C*====================================================================
     C* DLTRCD - Delete WMMUP record
     C*====================================================================
     C     DLTRCD        BEGSR
     C*
     C*** ***              Z-ADDXXEND#    KKEND#
     C*** ***    MUKY1     DELETPMU01                60
      ***** BEG DEL ******* 01/19/06 **********************************
     C*********            Z-ADDXXEND#    KKEND#
     C*********  MUKY1     CHAINPMU01                60
     C*********            MOVELDELETD    MUDESC    P
     C*********            MOVE 'X'       MUSTAT
     C**N60****            UPDATPMU01
      ***** END DEL ******* 01/19/06 **********************************
      ***** BEG ADD ******* 01/19/06 **********************************
      *
     C                   Z-ADD     XXEND#        KKEND#
     C     MUKY          SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     MUEND#        ANDEQ     KKEND#
     C     MUKY1         DELETE    PMU01                              60
      *
     C                   ELSE
      *
     C     MUKY1         CHAIN     PMU01                              60
     C                   MOVEL(P)  DELETD        MUDESC
     C                   MOVE      'X'           MUSTAT
     C  N60              UPDATE    PMU01
     C                   ENDIF
      *
      ***** END ADD ******* 01/19/06 **********************************
     C*
     C                   ENDSR
     C***  End   Add  ***  10/28/05  **************************************
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   Z-ADD     *ZERO         COUNT1
     C*** BEGIN ADD ***  09/04/98  ************************************
     C  N90XXCANC        IFEQ      'Y'
     C*
     C     SCPGM1        IFEQ      'W0051R21'
     C     SCPGM2        OREQ      'W0051R21'
     C     SCPGM3        OREQ      'W0051R21'
     C     SCPGM4        OREQ      'W0051R21'
     C     SCPGM5        OREQ      'W0051R21'
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPGM1 IFEQ
     C*
      *** BEGIN ADD *** 06/29/99 ***********************************************
     C     SCPGM1        IFEQ      'W0051R20'
     C     SCPGM2        OREQ      'W0051R20'
     C     SCPGM3        OREQ      'W0051R20'
     C     SCPGM4        OREQ      'W0051R20'
     C     SCPGM5        OREQ      'W0051R20'
     C                   MOVE      *ON           *IN81
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPGM1 IFEQ
      *** END   ADD *** 06/29/99 ***********************************************
     C                   ENDIF                                                  XXCANC IFEQ
     C*** END ADD *****  09/04/98  ************************************
      *++++++ BEGIN ADD +++ 10/31/06 ++++++++++++++++++++++++++++++++
      * Check if carrier # required.
      *
     C                   MOVE      *OFF          *IN80
     C     XXCAP#        IFEQ      'Y'
     C*
     C     SCPGM1        IFEQ      'W0051R21'
     C     SCPGM2        OREQ      'W0051R21'
     C     SCPGM3        OREQ      'W0051R21'
     C     SCPGM4        OREQ      'W0051R21'
     C     SCPGM5        OREQ      'W0051R21'
     C                   MOVE      *ON           *IN80
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPGM1 IFEQ
     C                   ENDIF                                                  SCPGM1 IFEQ
      *++++++ END   ADD +++ 10/31/06 ++++++++++++++++++++++++++++++++
      **** BEGIN ADD **** 05/08/01 ************************************
      *
      *  ADD DIVISION CANNOT BE RUN ON CANCELLED POLICY
      *
     C**** BEGIN ADD **** 06/22/01 ************************************
     C  N90XXCANC        IFEQ      'Y'
     C**** END   ADD **** 06/22/01 ************************************
     C*
     C     SCPGM1        IFEQ      'W0051R19'
     C     SCPGM2        OREQ      'W0051R19'
     C     SCPGM3        OREQ      'W0051R19'
     C     SCPGM4        OREQ      'W0051R19'
     C     SCPGM5        OREQ      'W0051R19'
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPGM1 IFEQ
     C                   ENDIF                                                  XXCXST IFEQ
      **** END   ADD **** 05/08/01 ************************************
      ***** BEG ADD ******* 08/17/05 **********************************
      *
      *  Allow "Rescind Division Deletion" for CX divisions only
      *
     C  N90XXCANC        IFNE      'Y'
      *
     C     SCPGM1        IFEQ      'W0051R40'
     C     SCPGM2        OREQ      'W0051R40'
     C     SCPGM3        OREQ      'W0051R40'
     C     SCPGM4        OREQ      'W0051R40'
     C     SCPGM5        OREQ      'W0051R40'
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCPGM1-5
      *
     C                   ENDIF                                                  XXCANC NE Y
      *
      ***** END ADD ******* 08/17/05 **********************************
     C*
      *** BEG ADD ***** 04/08/05 **************************************
     C  N90ELARSM        IFEQ      'Y'
     C     EMSTRP        ANDNE     'Y'
     C*
     C     SCPGM1        IFEQ      'W0051R20'
     C     SCPGM2        OREQ      'W0051R20'
     C     SCPGM3        OREQ      'W0051R20'
     C     SCPGM4        OREQ      'W0051R20'
     C     SCPGM5        OREQ      'W0051R20'
     C*
     C     EMKY1         SETLL     PEM02
     C     EMKY1         READE     PEM02                                  60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XXDIV
     C                   LEAVE
     C                   ENDIF                                                  EMSTRP = Y
     C     EMKY1         READE     PEM02                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C     EAKY          READE     PEA01                                  60
     C*
     C     EAARTY        IFEQ      'H'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C                   SETON                                        84  90
     C                   LEAVE
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  SCPGM1 = 'W0051R20'
     C*
     C                   ENDIF                                                  ELARSM = Y
      *** END ADD ***** 04/08/05 **************************************
     C*
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
     C*
     C     *IN21         IFEQ      *ON
     C                   MOVE      *LOVAL        KKSORT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C                   MOVE      SCPST1        KKSORT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C                   MOVEL     SCSORT        KKSORT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  LOAD DATA DATA FIELDS TO HIDDEN SCREEN FIELDS
     C*
     C                   MOVEL     MYPGM1        SCPGM1
     C                   MOVEL     MYPGM2        SCPGM2
     C                   MOVEL     MYPGM3        SCPGM3
     C                   MOVEL     MYPGM4        SCPGM4
     C                   MOVEL     MYPGM5        SCPGM5
     C*
     C*  LOAD THE APPROPRIATE VIEW DATA SUBFIELDS
     C*
     C                   MOVE      MYSORT        SCSORT
     C                   MOVEL     MYDESC        SCDATA
     C                   MOVEL     MYDESC        SCDESC
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
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
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C**** BEGIN ADD **** 02/02/01 ************************************
     C                   MOVE      *HIVAL        KKEND#
     C**** END   ADD **** 02/02/01 ************************************
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     ELKY1         SETGT     PEL01
     C     ELKY1         READPE    PEL01                                  60
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C                   MOVE      ELCO#         KKCO#
     C                   MOVE      ELFND         KKFND
     C                   MOVE      ELFYR         KKFYR
     C                   MOVE      ELEMP#        KKEMP#
     C                   MOVE      ELDIV         KKDIV
     C*
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELFND         ##FND
     C*
     C                   MOVEL     *BLANK        ##CONM
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVEL     FNNAME        ##FNAM
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   MOVEL     ##FNAM        SCNAME
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELEMP#        SCEMP#
     C                   MOVE      ELDIV         SCDIV
     C                   MOVE      ELFYR         SCFYR
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      CMPRES        CMPRPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C*** BEGIN ADD ***  09/04/98  ************************************
     C                   MOVE      *BLANK        XXCANC            1
     C*** END ADD *****  09/04/98  ************************************
     C                   MOVE      *OFF          *IN78                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN78                          CANCELLED
      *
      ***** BEG DEL ******* 06/16/03 **********************************
      *****BEGIN ADD **** 02/26/02 ************************************
     C*********  XXDATE    IFGE CX,I1
      *****END*  ADD **** 02/26/02 ************************************
      ***** END DEL ******* 06/16/03 **********************************
     C*** BEGIN ADD ***  09/04/98  ************************************
     C                   MOVE      'Y'           XXCANC            1
     C*** END ADD *****  09/04/98  ************************************
      ***** BEG DEL ******* 06/16/03 **********************************
      *****BEGIN ADD **** 02/26/02 ************************************
     C*********            ENDIF
      *****END*  ADD **** 02/26/02 ************************************
      ***** END DEL ******* 06/16/03 **********************************
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   END
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
      *++++++ BEGIN ADD +++ 10/31/06 ++++++++++++++++++++++++++++++++
     C     KEY1          CHAIN     PGC01                              60
     C     *IN60         IFEQ      *OFF
     C     GCCRFL        ANDEQ     'Y'
     C     MUKY          SETLL     PEL02
     C     MUKY          READE     PEL02                                  60
     C     *IN60         DOWEQ     *OFF
     C     ELKY3         CHAIN     PRP01                              99
     C     RPCAP#        IFEQ      *BLANKS
     C     *IN99         OREQ      *ON
     C                   MOVE      'Y'           XXCAP#            1
     C                   LEAVE
     C                   ENDIF                                                  RPCAP# = BLANKS
     C     MUKY          READE     PEL02                                  60
     C                   ENDDO
      *** BEG ADD ***** 11/06/07 **************************************
      *
      * RESET WDELP INFO
      *
     C     ELKY4         CHAIN     PEL01                              60
      *** END ADD ***** 11/06/07 **************************************
     C                   ENDIF
      *++++++ END   ADD +++ 10/31/06 ++++++++++++++++++++++++++++++++
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C     KEY1          READE     DBRCDF                                 95
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C     KEY1          READPE    DBRCDF                                 95
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C     KEY2          SETGT     DBRCDF                             96
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C     *IN21         IFEQ      *ON
     C     KEY2          SETLL     DBRCDF                             96
     C                   ENDIF
     C*
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
     C*** BEGIN ADD **** 04/04/12 *************************************
     C*  VALIDATIONS FOR ADDITION OF A DIVISION
     C*
B01  C     SCPGM1        IFEQ      'W0051R19'
     C     SCPGM2        OREQ      'W0051R19'
     C     SCPGM3        OREQ      'W0051R19'
     C     SCPGM4        OREQ      'W0051R19'
     C     SCPGM5        OREQ      'W0051R19'
     C                   EXSR      CHKRIU
     C                   ENDIF
     C*** END   ADD **** 04/04/12 *************************************
     C*** BEGIN ADD *** 08/03/99 ***********************************************
     C*
     C*  Check and see if policy page was issued for state reporting division
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     KKDIV         XXDIV             5 0
     C                   ELSE
     C*
     C     EMKY1         SETLL     PEM02
     C     EMKY1         READE     PEM02                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XXDIV
     C                   LEAVE
     C                   ENDIF                                                  EMSTRP = Y
     C*
     C     EMKY1         READE     PEM02                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C                   ENDIF
      *** BEGIN DEL *** 09/20/99 **************************************
     C**********           SETON                     82  90
      **********
     C********** SHKY      SETLLPSH01
      **********
     C********** *IN60     DOUEQ*ON
     C********** SHKY      READEPSH01                    60
      **********
     C********** *IN60     IFEQ *OFF
     C****BEGIN*CHG *** 08/23/99 ***********************************************
     C********** SHSTAT    ANDEQ'3'
     C********** SHSTAT    ANDGT'2'
     C****END***CHG *** 08/23/99 ***********************************************
     C**********           SETOF                     82  90
     C**********           LEAVE
     C**********           ENDIF
      **********
     C**********           ENDDO                           *IN60 = *ON
      *** END   DEL *** 09/20/99 **************************************
      *
     C*** END   ADD *** 08/03/99 ***********************************************
     C*
     C*** BEG ADD ***** 06/10/98 ***********************************************
     C*
     C*  VALIDATIONS FOR DELETION OF A DIVISION
     C*
B01  C     SCPGM1        IFEQ      'W0051R20'
     C     SCPGM2        OREQ      'W0051R20'
     C     SCPGM3        OREQ      'W0051R20'
     C     SCPGM4        OREQ      'W0051R20'
     C     SCPGM5        OREQ      'W0051R20'
      *
      *** BEGIN DEL *** 07/16/99 ***********************************************
     C********** Q1USER    CHAINPUP01                60
     C********** EXKY      CHAINPEX01                60
     C*
B02  C********** UPLVL     IFLT EXDLTL
     C********** *IN60     OREQ *ON
     C*
     C**********           SETON                     71  90
     C*
X02  C**********           ELSE
      *** END   DEL *** 07/16/99 ***********************************************
      *
     **** BEGIN DEL *** 05/20/99 **************************************
     C**********
     C***GOTO*THE CLAIMS FILE AND CHECK FOR A CLAIM
     C**********
     C********** CMKY      CHAINPCM01                60
     C**********
B03  C********** *IN60     IFEQ *OFF
     C**********
     C**********           SETON                     72  90
     C**********
X03  C**********           ELSE
     C**********
     C***CHECK*FOR PAYMENTS IN ACCOUNTS RECEIVABLE FILE
     C**********
     C********** EAKY      CHAINPEA01                60
     C**********
B04  C********** *IN60     DOWEQ*OFF
     C**********
B05  C**********           SELEC
     C**********
S01  C********** EAARTY    WHEQ 'P'
     C********** EAARTY    OREQ 'I'
     C********** EAARTY    OREQ 'Q'
     C**********           SETON                     736090
     C**********
S02  C********** XXTRYM    WHGT XXPOYM
     C********** XXTRYM    ANDLTXXCUYM
     C**********           SETON                     746090
     C**********
S03  C********** XXTRYM    WHLT XXCUYM
     C********** XXPOYM    ANDLTXXCUYM
     C**********           SETON                     746090
     C**********
S04  C**********           OTHER
     C********** EAKY      READEPEA01                    60
     C**********
E05  C**********           ENDSL
     C**********
E04  C**********           ENDDO                           *IN60=*OFF
     C**********
E03  C**********           ENDIF                           *IN60=*OFF
     C**********
     C***CHECK*TO BE SURE NO NCCI SUBMISSION HAS A STATUS OF '4'
     C**********
     C**N90***** *IN60     DOUEQ*ON
     C**********
     C********** SHKY      CHAINPSH01                60
     C**********
     C********** SHSTAT    IFEQ '4'
     C********** *IN60     ANDEQ*OFF
     C**********           SETON                     75  90
     C**********           LEAVE
     C**********           ENDIF                           SHSTAT=A
     C**********
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***CHECK*FOR NON-TRANSFERRED A/R BATCH RECORDS
     C**********
     C**N90***** *IN60     DOUEQ*ON
     C**********
     C********** EAKY      READEPEA02                    60
     C**********
     C********** EASTAT    IFNE 'T'
     C********** *IN60     ANDEQ*OFF
     C**********           SETON                     76  90
     C**********           LEAVE
     C**********           ENDIF                           EASTAT=A
     C**********
     C**********           ENDDO                           *IN60=*ON
     C**********
     **** END   DEL *** 05/20/99 **************************************
     C*
     **** BEGIN ADD *** 05/20/99 **************************************
     C     EMSTRP        IFEQ      'Y'
     C                   SETON                                        73  90
     C                   ENDIF
     **** END   ADD *** 05/20/99 **************************************
     C*
B03  C     *IN90         IFEQ      *OFF
     C*
     C*  EXECUTE THIS CODE ONLY IF DARTS IS INSTALLED
     C*
     C                   MOVEL     'SY'          XXSYK
     C*
     C     SYKY          CHAIN     SSY01                              60
     C*
B04  C     *IN60         IFEQ      *OFF
     C     SYSCIF        ANDEQ     'Y'
     C*
     C*  CHECK FOR ACTIVE DARTS REMARKS
     C*
     C*    ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      KKCO#         QCCO#
     C                   MOVE      KKFND         QCFND
     C                   MOVE      KKEMP#        QCEMP#
     C                   MOVE      KKDIV         QCDIV
     C*
     C*  CHAIN TO DARTS REMARKS DETAIL FILE
     C*
     C     C3KY          CHAIN     PCM03                              60
     C*
B05  C     *IN60         DOWEQ     *OFF
     C*
B06  C     C3DIFL        IFEQ      'Y'
     C     C3STAT        ANDNE     'I'
     C*
     C                   MOVEL     'N'           SCOVR
     C*
     C                   EXFMT     ERRWDW01
     C*
B07  C     SCOVR         IFNE      'Y'
     C                   SETON                                        77  90
E07  C                   ENDIF                                                  SCERFL<>Y
     C*
     C                   LEAVE
     C*
E06  C                   ENDIF                                                  C3DIFL=Y
     C*
     C     C3KY          READE     PCM03                                  60
     C*
E05  C                   ENDDO                                                  *IN60=*OFF
     C*
E04  C                   ENDIF                                                  *IN60=*OFF/SYSCIF=Y
     C*
E03  C                   ENDIF                                                  *IN90=*OFF
     C*
      *** BEGIN DEL *** 07/16/99 ***********************************************
E02  C**********           ENDIF                           SVLVL<EXDLTL
      *** END   DEL *** 07/16/99 ***********************************************
     C*
E01  C                   ENDIF                                                  SCPGM*=W0051R20
     C*** END ADD ***** 06/10/98 ***********************************************
     C*
     C*** BEG ADD ***** 06/10/98 ***********************************************
B0   C     *IN90         IFEQ      *OFF
     C*** END ADD ***** 06/10/98 ***********************************************
B1   C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR03
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C*
B2   C     *INKL         IFEQ      *OFF
     C*
     C*  ENDORSEMENT DATE MUST BE ENTERED
     C*
B3   C     SSENDT        IFEQ      *ZERO
     C                   SETON                                        8590
X3   C                   ELSE
     C*
     C*  ENDORSEMENT DATE MUST BE VALID
     C*
     C                   Z-ADD     SSENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
B4   C     SRERR         IFEQ      *ON
     C                   SETON                                        8590
     C*** BEG ADD ***** 03/18/98 ***********************************************
     C                   ELSE
     C                   MOVEL     SRDTO         SSENDT
     C*** END ADD ***** 03/18/98 ***********************************************
E4   C                   ENDIF                                                  SRERR=*ON
     C*
     C*  ENDORSEMENT DATE MUST BE WITHIN POLICY DATES
     C*
     C******** BEGIN DEL ****** 01/19/99 ******************************
     C********** ELKY      SETGTPEL01
     C********** ELKY      REDPEPEL01                    60
     C**********
     C**********           Z-ADDSSENDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXENDT
     C*
B4   C********** XXENDT    IFLT EMEFD1
     C********** XXENDT    ORGT ELPTDT
     C**********           SETON                     8690
E4   C**********           ENDIF
     C******** END   DEL ****** 01/19/99 ******************************
     C*
     C******** BEGIN ADD ****** 01/19/99 ******************************
     C******** BEGIN ADD ****** 10/24/00 ******************************
     C*
     C* Allow an endorsement effective date equal to the policy
     C* expiration date if processing a Deletion of Division
     C*
     C                   MOVE      '0'           *IN92
     C******** BEGIN ADD ****** 10/20/10 ******************************
     C                   MOVE      '0'           XXDEL
     C******** END   ADD ****** 10/20/10 ******************************
     C     SCPGM1        IFEQ      'W0051R20'
     C     SCPGM2        OREQ      'W0051R20'
     C     SCPGM3        OREQ      'W0051R20'
     C     SCPGM4        OREQ      'W0051R20'
     C     SCPGM5        OREQ      'W0051R20'
     C******** BEGIN ADD ****** 10/20/10 ******************************
     C     SCPGM1        OREQ      'W0051R65'
     C     SCPGM2        OREQ      'W0051R65'
     C     SCPGM3        OREQ      'W0051R65'
     C     SCPGM4        OREQ      'W0051R65'
     C     SCPGM5        OREQ      'W0051R65'
     C******** END   ADD ****** 10/20/10 ******************************
     C******** BEGIN ADD ****** 05/06/11 ******************************
     C     SCPGM1        OREQ      'W0051R40'
     C     SCPGM2        OREQ      'W0051R40'
     C     SCPGM3        OREQ      'W0051R40'
     C     SCPGM4        OREQ      'W0051R40'
     C     SCPGM5        OREQ      'W0051R40'
     C******** END   ADD ****** 05/06/11 ******************************
     C                   MOVE      '1'           *IN92
     C******** BEGIN ADD ****** 10/20/10 ******************************
     C                   MOVE      '1'           XXDEL
     C******** END   ADD ****** 10/20/10 ******************************
     C                   ENDIF
     C******** END ADD ******** 10/24/00 ******************************
     C                   SETON                                        8690
     C*** BEG CHG ***** 05/11/12 **************************************
     C***********ELKY      CHAINPEL01                60
     C************IN60     DOWEQ'0'
     C***********
     C***********          Z-ADDSSENDT    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     XXENDT
     C***********
     C***********XXENDT    IFGE ELPFDT
     C***********XXENDT    ANDLTELPTDT
     C*********BEGIN ADD ****** 10/20/10 ******************************
     C***********XXDEL     ANDEQ'0'
     C*********END   ADD ****** 10/20/10 ******************************
     C*****BEGIN*DEL **** 07/05/01 ************************************
     C*********BEGIN ADD ****** 10/24/00 ******************************
     C***********
     C**Allow*an*endorsement effective date equal to the policy
     C**expiration date if processing a Deletion of Division
     C***********
     C******** BEGIN ADD ****** 10/20/10 ******************************
     C********** XXDEL     OREQ '1'
     C********** XXENDT    ANDGEELPFDT
     C********** XXENDT    ANDLEELPTDT
     C*********END   ADD ****** 10/20/10 ******************************
     C********** *IN92     OREQ '1'
     C********** XXENDT    ANDGEELPFDT
     C********** XXENDT    ANDLEELPTDT
     C*********END ADD ******** 10/24/00 ******************************
     C*****END** DEL **** 07/05/01 ************************************
     C*********BEGIN ADD ****** 01/27/99 ******************************
     C**********           Z-ADDELFYR     XXFYRS
     C*********END ADD ******** 01/27/99 ******************************
     C**********           SETOF                     8690
     C**********           ENDIF
     C********** ELKY      READEPEL01                    60
     C**********           ENDDO
     C*********END   ADD ****** 01/19/99 ******************************
      *
     C* Allow an endorsement effective date equal to the policy
     C* expiration date if xxdel = '1' only
      *
     C                   Z-ADD     SSENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
      *
     C     ELKYA         SETGT     PELL2
     C     ELKY          READPE    PELL2                                  60
     C     *IN60         DOWEQ     '0'
     C     XXDEL         IFEQ      '0'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C     XXDEL         OREQ      '1'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLE     ELPTDT
     C                   Z-ADD     ELFYR         XXFYRS
     C                   SETOFF                                       8690
     C                   LEAVE
     C                   ENDIF
     C     ELKY          READPE    PELL2                                  60
     C                   ENDDO
     C*** END CHG ***** 05/11/12 **************************************
      *** BEGIN ADD *** 04/07/00 **************************************
     C*
     C* Only perform policy page edit if WCPOLS is installed
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL03        IFEQ      'Y'
      *** END ADD ***** 04/07/00 **************************************
      *** BEGIN ADD *** 09/20/99 **************************************
      ** Check policy year of endorsement effective date for the
      ** submission of a policy page.
     C     *IN90         IFEQ      *OFF
      *
      *** BEGIN ADD *** 09/29/99 **************************************
     C                   MOVE      'P'           KKTYPE
      *** BEGIN ADD *** 09/29/99 **************************************
     C                   SETON                                        82  90
     C     SHKY1         CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C     SHSTAT        ANDGT     '2'
     C                   SETOFF                                       82  90
     C                   ENDIF                                                  SHSTAT > 2
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *** BEGIN ADD *** 04/07/00 **************************************
     C                   ENDIF                                                  WCFL03='Y'
     C*                                                    WCPOLS INSTALLED?
      *** END ADD ***** 04/07/00 **************************************
     C*
     C******** BEGIN ADD ****** 01/27/99 ******************************
     C*
     C*  DO NOT ALLOW MIDTERM REVISED BILLING CHANGE ON AN
     C*  AUDITED ACCOUNT
     C*
     C     ELKY2         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C     ELFAF         IFEQ      'A'
     C     SCPGM1        IFEQ      'W0051R21'
     C     SCPGM2        OREQ      'W0051R21'
     C     SCPGM3        OREQ      'W0051R21'
     C     SCPGM4        OREQ      'W0051R21'
     C     SCPGM5        OREQ      'W0051R21'
     C                   SETON                                        8790
     C                   ENDIF                                                  SCPGM1-5
     C                   ENDIF                                                  ELFAF='A'
     C                   ENDIF                                                  *IN60
     C******** END ADD ******** 01/27/99 ******************************
     C*
     **** BEGIN ADD *** 05/20/99 **************************************
     C*
     C*  If processing Deletion of Division and there are any claims
     C*  that exist with an accident date greater than or equal to the
     C*  endorsement date, flag an error message due to backposting
     C*  cancelation date.  Allow user to override error message.
     C*
     C     SCPGM1        IFEQ      'W0051R20'
     C     SCPGM2        OREQ      'W0051R20'
     C     SCPGM3        OREQ      'W0051R20'
     C     SCPGM4        OREQ      'W0051R20'
     C     SCPGM5        OREQ      'W0051R20'
     C*
     C                   Z-ADD     XXENDT        XXACDT
     C*
     C     CMKY          SETLL     PCM01
     C     CMKY1         READE     PCM01                                  60
     C*
     C  N90*IN60         DOWEQ     *OFF
     C     OVRFLG        ANDNE     'Y'
     C*
     C                   XFOOT     AR            XXPMTS           13 2
     C*
     C     CMSTAT        IFNE      'C'
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
     C*
     C*  Allow override on error msg only if user has proper authority
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C     UPLVL         IFGE      WCSL03
     C*
     C                   MOVE      'N'           SCEOVR
     C*
     C                   EXFMT     WDWMSG
     C*
     C     SCEOVR        IFEQ      'Y'
     C*
     C                   MOVE      'Y'           OVRFLG            1
     C*
     C                   ELSE
     C*
     C                   MOVE      'N'           OVRFLG
     C                   SETON                                        72  90
     C                   MOVE      *ON           *INKL
     C*
     C                   ENDIF                                                  SCEOVR=Y
     C*
     C                   ELSE
     C*
     C                   MOVE      'N'           OVRFLG
     C                   SETON                                        72  90
     C                   MOVE      *ON           *INKL
     C*
     C                   ENDIF                                                  UPLVL>=WCSL03
     C*
     C                   ENDIF                                                  CMSTAT<>C
     C*
     C     CMKY1         READE     PCM01                                  60
     C*
     C  N90              ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SCPGM=W0051R20
     **** END   ADD *** 05/20/99 **************************************
      **** BEGIN ADD **** 08/27/01 ************************************
      *
      *** BEG CHG ***** 07/11/11 ******************************************
      *******Do not allow endorsement to be issued after the cancellation
      *******date. If the Employer is pending cancellation, allow the user
      ******* to override the message if they have the proper authority.
      *
      * Do not allow endorsement to be issued after the cancellation
      * date. If the Employer is pending cancellation, display a
      * window with a warming message that the account is pending cancellation
      *** END CHG ***** 07/11/11 ******************************************
      *
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    ED(I1)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             I1
     C                   ELSE
     C                   SUB       1             I1
     C                   ENDIF                                                  *IN60
     C*
     C     I1            IFEQ      *ZERO
     C     CX(I1)        OREQ      *ZERO
     C                   MOVE      *OFF          *IN78                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN78                          CANCELLED
     C                   ENDIF                                                  I1
      *
     C                   Z-ADD     SSENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
      *
     C     CX(I1)        IFGT      XXENDT
     C     CX(I1)        ANDNE     *ZEROS
      *** BEG DLT ***** 07/11/11 ******************************************
     C***********Q1USER    CHAINPUP01                60
     C***********UPLVL     IFGE WCSL03
     C***********
     C***********          MOVE 'N'       SCEOVR
      *** END DLT ***** 07/11/11 ******************************************
     C*
     C                   EXFMT     WDWMSG2
     C*
      *** BEG DLT ***** 07/11/11 ******************************************
     C***********SCEOVR    IFEQ 'Y'
     C***********
     C***********          MOVE 'Y'       OVRFLG  1
     C***********
     C***********          ELSE
     C***********
     C***********          MOVE 'N'       OVRFLG
     C***********          SETON                         90
     C***********          MOVE *ON       *INKL
     C***********
     C***********          ENDIF                           SCEOVR=Y
     C***********
     C***********          ELSE
     C***********
     C***********          MOVE 'N'       OVRFLG
     C***********          SETON                         90
     C***********          MOVE *ON       *INKL
     C***********
     C***********          ENDIF                           UPLVL>=WCSL03
      *** END DLT ***** 07/11/11 ******************************************
     C*
     C                   ENDIF                                                  XXENDT
      *
     C     XXENDT        IFGE      CX(I1)
     C     CX(I1)        ANDNE     *ZEROS
     C                   SETON                                        8990
     C                   ENDIF                                                  XXENDT
      *
      **** END   ADD **** 08/27/01 ************************************
      ***** BEG ADD ******* 08/17/05 **********************************
      *
      *  Allow "Rescind Division Deletion" for CX divisions only
      *
     C     SCPGM1        IFEQ      'W0051R40'
     C     SCPGM2        OREQ      'W0051R40'
     C     SCPGM3        OREQ      'W0051R40'
     C     SCPGM4        OREQ      'W0051R40'
     C     SCPGM5        OREQ      'W0051R40'
      *
     C     *IN89         IFEQ      *ON
     C     *IN90         ANDEQ     *ON
     C                   SETOFF                                       89  90
E3   C                   ENDIF
      *
E3   C                   ENDIF                                                  SCPGM1-5
      *
      ***** END ADD ******* 08/17/05 **********************************
     C*
E3   C                   ENDIF
     C*
     C*  EDITS PASSED
     C*
B3   C     *IN90         IFEQ      *OFF
     C                   CALL      'SRRMVMSG'                           99
     C*
B4   C     SCPGM1        IFNE      *BLANK
     C     SCPGM2        ORNE      *BLANK
     C     SCPGM3        ORNE      *BLANK
     C     SCPGM4        ORNE      *BLANK
     C     SCPGM5        ORNE      *BLANK
     C*
     C*  PROCESS FOR ALL FUND YEARS ENDORSEMENT DATE THRU CURRENT DATE
     C*
      *** BEG CHG ***** 05/11/12 **************************
     C***********ELKY      SETLLPEL01
     C***********ELKY      READEPEL01                    60
B5   C************IN60     DOWEQ*OFF
     C***********
     C*********BEGIN CHG ****** 01/19/99 ******************************
     C***********
     C***ISSUE*ENDORSEMENT FOR POLICY YEAR IN WHICH THE ENDORSEMENT
     C***EFFECTIVE DATE FALLS
     C***********
     C****BEG*CHG ***** 07/07/98 ***********************************************
B6   C***********XXENDT    IFLE ELPTDT                     ENDORSE DT> POL TO
B6   C***********XXENDT    IFLT ELPTDT                     ENDORSE DT> POL TO
     C****END*CHG ***** 07/07/98 ***********************************************
     C***********XXDATE    ANDGEELPFDT
     C***********XXENDT    IFGE ELPFDT
     C***********XXENDT    ANDLTELPTDT
     C*********BEGIN ADD ****** 10/20/10 ******************************
     C***********XXDEL     ANDEQ'0'
     C*********END   ADD ****** 10/20/10 ******************************
     C*********END CHG ******** 01/19/99 ******************************
     C*****BEGIN*DEL **** 07/05/01 ************************************
     C*********BEGIN ADD ****** 10/24/00 ******************************
     C***********
     C**Allow*an*endorsement effective date equal to the policy
     C**expiration date if processing a Deletion of Division
     C***********
     C*********BEGIN ADD ****** 10/20/10 ******************************
     C***********XXDEL     OREQ '1'
     C***********XXENDT    ANDGEELPFDT
     C***********XXENDT    ANDLEELPTDT
     C*********END   ADD ****** 10/20/10 ******************************
     C************IN92     OREQ '1'
     C***********XXENDT    ANDGEELPFDT
     C***********XXENDT    ANDLEELPTDT
     C*********END ADD ******** 10/24/00 ******************************
     C*****END***DEL **** 07/05/01 ************************************
      *
     C     ELKYA         SETGT     PELL2
     C     ELKY          READPE    PELL2                                  60
B5   C     *IN60         DOWEQ     *OFF
      *
      * Allow an endorsement effective date equal to the policy
      * expiration date only if XXDEL = '1'. See prior comments
      * for XXDEL.
      *
B6   C     XXDEL         IFEQ      '0'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C     XXDEL         OREQ      '1'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLE     ELPTDT
      *** END CHG ***** 05/11/12 **************************
     C*
     C*  GET CURRENT ENDORSEMENT # FOR FYR BEING PROCESSED
     C*
     C**** BEGIN ADD **** 02/02/01 ************************************
     C********** MUKY      SETGTPMU01
     C     MUKY1         SETGT     PMU01
     C**** END   ADD **** 02/02/01 ************************************
     C**** BEG   ADD **** 11/04/08 ************************************
     C*********  MUKY      REDPEPMU01                    60
     C     MUKY          READPE(N) PMU01                                  60
     C**** END   ADD **** 11/04/08 ************************************
B7   C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         MUEND#
E7   C                   ENDIF
     C*
     C*  LOAD ENDORSEMENT NUMBER TO LOCAL DATA AREA POSITION 1 THRU 5
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     SVLDA         SVLDAO          250
     C     MUEND#        ADD       1             XXEND#
     C                   MOVE      XXEND#        LDEND#
     C                   MOVE      SCDBRR        LDMYRR
     C                   MOVE      SSENDT        LDEEDT
     C                   MOVE      ELFYR         LDFYR
     C                   OUT       *DTAARA
     C*
     C*  CREATE AN EMPLOYER ENDORSEMENT RECORD
     C*
     C                   MOVE      ELCO#         MUCO#
     C                   MOVE      ELFND         MUFND
     C                   MOVE      ELEMP#        MUEMP#
     C                   MOVE      ELDIV         MUDIV
     C                   MOVE      ELFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      *ZERO         MUSEQ#
     C                   MOVE      *BLANKS       MUFORM
     C                   MOVEL     'MID-YEAR'    MUFORM
     C                   MOVEL     *BLANKS       MUENST
     C                   MOVEL     SCDESC        MUDESC
     C                   MOVE      XXENDT        MUEEDT
     C                   MOVEL     'I'           MUEXFL
     C                   MOVEL     'M'           MUMNAU
     C                   MOVEL     'N'           MUPRNT
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C     DSTIME        ADD       1             MUTIME
     C                   MOVE      XXDATE        MUDT
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      XXDATE        MUIDT
     C*** BEGIN ADD ***  12/15/05  ************************************
     C                   MOVEL     SCPGM1        MUPPGM
     C*** END ADD *****  12/15/05  ************************************
     C*
     C***  Begin Add  ***  10/28/05  **************************************
     C                   WRITE     PMU01
     C***  End   Add  ***  10/28/05  **************************************
      *** BEGIN DEL *** 07/08/99 ***********************************************
     C******************** WRITEPMU01
      *** END   DEL *** 07/08/99 ***********************************************
     C*
     C*  CALL THE NECESSARY PROCESSING PROGRAMS
     C*
     C     SCPGM1        IFNE      *BLANK
     C                   CALL      SCPGM1        PROCPL                 99
     C                   ENDIF
     C*
     C     SCPGM2        IFNE      *BLANK
     C                   CALL      SCPGM2        PROCPL                 99
     C                   ENDIF
     C*
     C     SCPGM3        IFNE      *BLANK
     C                   CALL      SCPGM3        PROCPL                 99
     C                   ENDIF
     C*
     C     SCPGM4        IFNE      *BLANK
     C                   CALL      SCPGM4        PROCPL                 99
     C                   ENDIF
     C*
     C     SCPGM5        IFNE      *BLANK
     C                   CALL      SCPGM5        PROCPL                 99
     C                   ENDIF
     C*
     C*  IF WMENP ENDORSEMENT REQUEST RECORDS WERE CREATED
     C*    THEN SUBMIT THE PRINT PORTION OF THE ENDORSEMENT PROCESS
     C*
     C     ENKY          CHAIN     PEN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD *** 04/13/99 ***********************************************
      *
      *** BEGIN DEL *** 07/08/99 ***********************************************
     C********** ##KC      ANDEQ'0'
     C********** *IN60     OREQ *OFF
     C********** ##KC      ANDEQ*BLANKS
      *** END   DEL *** 07/08/99 ***********************************************
      *
     C*** END ADD ***** 04/13/99 ***********************************************
     C                   MOVE      ELCO#         QQCO#             3
     C                   MOVE      ELFND         QQFND             3
     C                   MOVE      ELEMP#        QQEMP#            9
     C                   MOVE      ELDIV         QQDIV             5
     C                   MOVE      ELFYR         QQFYR             3
     C                   MOVE      Q1JOBN        QQJOBN            6
     C                   MOVE      XXEND#        QQEND#            5
     C                   MOVEL     ##OUTQ        QQOUTQ           10
      *** BEGIN DEL *** 03/04/02 ***********************************************
      *** BEGIN ADD *** 07/08/99 ***********************************************
     C************INKC     IFEQ *OFF
     C************INKL     ANDEQ*OFF
     C************IN60     OREQ *OFF
     C***********##KC      ANDEQ*BLANKS
     C************IN60     OREQ *OFF
     C***********##KL      ANDEQ*BLANKS
      *** END   ADD *** 07/08/99 ***********************************************
      *** END   DEL *** 03/04/02 ***********************************************
      *** BEGIN ADD *** 03/04/02 ***********************************************
     C*
     C*  DETERMINE IF F3=EXIT OR F12=CANCEL WAS PRESSED BEFORE
     C*  THE ENDORSEMENT COULD BE COMPLETED
     C*
     C                   MOVE      'N'           XXEXIT            1
     C     *INKC         IFNE      *BLANK
     C     *INKC         ANDNE     *ZERO
     C     *INKL         ORNE      *BLANKS
     C     *INKL         ANDNE     *ZERO
     C                   MOVE      'Y'           XXEXIT
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C     ##KC          IFNE      *BLANK
     C     ##KC          ANDNE     *ZERO
     C     ##KL          ORNE      *BLANK
     C     ##KL          ANDNE     *ZERO
     C                   MOVE      'Y'           XXEXIT
     C                   ENDIF
     C                   ENDIF
     C*
     C***  Begin Add  ***  10/28/05  **************************************
     C     XXEXIT        IFEQ      'Y'
     C                   MOVE      'N'           SCF312
     C                   EXFMT     WDWMSG3
     C     SCF312        IFEQ      'Y'
     C                   MOVE      'N'           XXEXIT
     C                   ENDIF
     C                   ENDIF
     C***  End   Add  ***  10/28/05  **************************************
     C*
     C     XXEXIT        IFNE      'Y'
      *** END   ADD *** 03/04/02 ***********************************************
     C*
     C                   CALL      'W7050C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQJOBN            6
     C                   PARM                    QQEND#            5
     C                   PARM                    QQOUTQ           10
      *** BEGIN ADD *** 07/08/99 ***********************************************
      *
     C***  Begin Del  ***  10/28/05  **************************************
      ***Only*write a WMMUP record if an endorsement request record was
      ***written.
     C********** DSTIME    ADD  1         MUTIME
     C**********           WRITEPMU01
     C***  End   Del  ***  10/28/05  **************************************
      *
     C                   ELSE
     C*
     C***  Begin Add  ***  10/28/05  **************************************
     C                   EXSR      DLTRCD
     C***  End   Add  ***  10/28/05  **************************************
      *
      ** Call this so that WMENP records won't be left hanging
      ** if the midterm endorsement was cancelled.
     C                   CALL      'W7052R1'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQJOBN
     C                   PARM                    QQEND#
      *
     C                   ENDIF                                                  ##KC = *OFF
      *** END   ADD *** 07/08/99 ***********************************************
     C*
     C***  Begin Add  ***  10/28/05  **************************************
     C                   ELSE
     C*
     C                   EXSR      DLTRCD
     C***  End   Add  ***  10/28/05  **************************************
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C*  REPLACE ORIGINAL VALUE OF *LDA
     C*
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     SVLDAO        SVLDA
     C                   OUT       *DTAARA
      *** BEG CHG ***** 05/11/12 ************************************
E6   C***********        ENDIF                                                  XXENDT <= ELPTDT
     C*****ELKY**        READE     PEL01                                  60
     C                   LEAVE
E6   C                   ENDIF                                                  XXENDT <= ELPTDT
      *
     C     ELKY          READPE    PELL2                                  60
      *** END CHG ***** 05/11/12 ************************************
E5   C                   ENDDO                                                  *IN60 DOWEQ
     C*
E4   C                   ENDIF                                                  SCPGM1 <> ' '
E3   C                   ENDIF                                                  *IN90 IFEQ *OFF
E2   C                   ENDIF                                                  *INKL IFEQ *OFF
     C*
E1   C                   ENDDO                                                  *IN90 DOUEQ *OFF
     C*** BEG ADD ***** 06/10/98 ***********************************************
E0   C                   ENDIF                                                  *IN90 = *OFF
     C*** END ADD ***** 06/10/98 ***********************************************
     C*
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *OFF          OUTFLG
     C*
     C*** BEGIN ADD **** 04/04/12 *************************************
     C*  RELEASE EMP WHEN ADDITION OF A DIVISION IS COMPLETED.
     C*
     C     SCPGM1        IFEQ      'W0051R19'
     C     SCPGM2        OREQ      'W0051R19'
     C     SCPGM3        OREQ      'W0051R19'
     C     SCPGM4        OREQ      'W0051R19'
     C     SCPGM5        OREQ      'W0051R19'
     C     @@RTCD        IFEQ      *OFF
     C                   MOVE      @@RRN         ##RRN4            9
     C                   CALL      'W0105R'
     C                   PARM      'B'           @@MODE            1
     C                   PARM                    ##RRN4
     C                   PARM      'WMEMP'       @@FNM            10
     C                   ENDIF                                                  ##KC IFEQ
     C                   ENDIF
     C*** END   ADD **** 04/04/12 *************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*  IF NOT STATE REPORTING DIVISION, ONLY SHOW ENDORSEMENTS AVAILABLE
     C*  TO ALL DIVISIONS.
     C*
     C     EMSTRP        IFNE      'Y'
     C     MYSRFL        ANDEQ     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     MYSTAT        IFNE      'A'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD **** 04/04/12 *************************************
     C*---------------------------------------------------------------
     C* CHECK RECORD IN USE
     C*---------------------------------------------------------------
     C     CHKRIU        BEGSR
     C                   MOVE      *OFF          @@RTCD            1
     C     RIUKY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RIJOBN        JOB6A             6
     C                   MOVEL(P)  RIJOB         SCJOB
     C     SCJOB         CAT       '/':0         SCJOB
     C     SCJOB         CAT       JOB6A:0       SCJOB
     C     SCJOB         CAT       '/':0         SCJOB
     C     SCJOB         CAT       RIUSER:0      SCJOB
     C                   EXFMT     WDWMSG4
     C                   MOVE      *ON           @@RTCD
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   TIME                    XSDTTM
     C                   MOVEL     'WMEMP'       RINAM
     C                   MOVE      @@RRN         RIRRN
     C                   MOVE      ##CO#         RICO#
     C                   MOVE      SCFND         RIFND
     C                   MOVE      SCEMP#        RIEMP#
     C                   MOVE      Q1JOBN        RIJOBN
     C                   MOVE      Q1JOB         RIJOB
     C                   MOVE      Q1USER        RIUSER
     C                   MOVE      XSTIME        RITIME
     C                   MOVE      XSDATE        RIDATE
     C                   WRITE     PRI01
     C                   ENDIF                                                  *IN60
     C                   ENDSR
     C***** END ADD   ****** 04/04/12 *********************************
     C*================================================================
** Column headings
Opt Seq# Description
** Description--------------MISN
1011=Select                 YYY
203F3=Exit                  YYY
212F12=Cancel               YYY
3
