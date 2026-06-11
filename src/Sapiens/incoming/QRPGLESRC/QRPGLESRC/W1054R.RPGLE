     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE W1054R - WORK WITH ENDORSEMENT FORMS
      *
      *
      * SEARCH FOR 'Continue here'
      *
      *
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                ATTACHED ENDORSEMENTS FOR A SELECTED EMPLOYER. *
     H*                EACH ENDORSEMENT CAN BE ACCESSED FOR UPDATE,   *
     H*                DISPLAY OR SELECTION DEPENDING ON THE CALLING  *
     H*                PROGRAM.                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/29/24 C:300 Project: WC2433                                *
     H*               -Chanegs for Endorsement Forms Additional Info link
     H*                                                               *
     H* 01/31/19 C:180 Support 75201                                  *
     H*                Read all divisions and allow subset by div     *
     H*                                                               *
     H* 08/20/18 C:291 Project: 133WQP                                *
     H*                Only allow Waiver Endorsements to  be added in *
     H*                Quote mode.                                    *
     H*                                                               *
     H* 09/28/17 C:33  Support: 49209                                 *
     H*                Allow for Output Window to be cancelled.       *
     H*                                                               *
     H* 01/16/17 C:136 Project: SY371                                 *
     H*                Implement Scriptura output window processing   *
     H*                                                               *
     H* 11/10/16 C:136 Project: SY371                                 *
     H*                Add processing for extended description field  *
     H*                                                               *
     H* 05/03/13 C:282 Support 82152  WA099                           *
     H*                Handling to remove the associated endorsement  *
     H*                data in WLEDP.                                 *
     H*                                                               *
     H* 06/05/08 C:197 PROJECT WC1530 PRISS#6                         *
     H*                ENDORSEMENTS - GENERIC END APPROVED/EXCLUDED   *
     H*                STATES                                         *
     H*                                                               *
     H* 11/01/07 C:184 PROJECT WC1530                                 *
     H*                ENDORSEMENTS - GENERIC END APPROVED/EXCLUDED   *
     H*                STATES                                         *
     H*                                                               *
     H* 04/01/07 C:61  SUPPORT WA099 44141                            *
     H*                Do not allow deletion of mid-term endorsement  *
     H*                records.                                       *
     H*                                                               *
     H* 04/21/06 C:33  Seq#: WA133/51998                              *
     H*                Form WC890600* is not available for reprint.   *
     H*                                                               *
     H* 10/28/05 C:33  Seq#: WA126/49343                              *
     H*                Don't display deleted records.                 *
     H*                                                               *
     H* 03/25/05 C:61  SUPPORT WA033 46131                            *
     H*              - Pass "Endorsement #" and                       *
     H*                endorsement effective date                     *
     H*                for opt6 processing.                           *
     H*
     H*  03/15/02 C:90  PROJECT: CM027/#31636                         *
     H*                 Allow Midterm endorsements to be printed if   *
     H*                 they have a valid record in WTENP. Do not     *
     H*                 allow print of WC890600* midterm endorsement. *
     H*                                                               *
     H*  06/30/00 C:90  PROJECT: CM027                                *
     H*                 Added Opt.6-Print print a single endorsement. *
     H*                                                               *
     H*  06/07/00 C:113 PROJECT: CM027
     H*           Added Fkey for reattachment of endorsements
     H*
     H*  06/01/00 C:90  PROJECT: CM027 SEQ# 22858
     H*                 ADDED COPY OPTION.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  11/01/99 C:113 PROJECT: CM027                                *
     H*                 Fixed key for rolling.                        *
     H*                                                               *
     H*  04/26/99 C:113 PROJECT: SUPPORT #17311                       *
     H*                 Added F4=Delete and added comments at top of  *
     H*                 program.                                      *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
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
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*                                                               *
     H*****************************************************************
     FW1054F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMMUP     IF   E             DISK
     F                                     RENAME(PMU01:PFRCDF)
     F*
      *** Begin Change *** 01/31/19 ***************************************
     F*  Primary logical file definition (for displaying items)
     F***WMMUL     IF   E           K DISK
     F***                                     INFDS(DBFDS)
     F***                                     RENAME(PMU01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     F***WMMUL1    IF   E           K DISK
     F***                                     INFDS(DBFDS2)
     F***                                     RENAME(PMU01:DBRCDF2)
     F*
     F*  Secondary logical file definition (for displaying items)
     F***WMMUL2    IF   E           K DISK
     F***                                     INFDS(DBFDS3)
     F***                                     RENAME(PMU01:DBRCDF3)
     F*
     F*  Secondary logical file definition (for displaying items)
     F***WMMUL3    IF   E           K DISK
     F***                                     INFDS(DBFDS4)
     F***                                     RENAME(PMU01:DBRCDF4)
      ***
      ***
      ***
     F*  Primary logical file definition (for displaying items)
     FWMMULA    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMU01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWMMULB    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PMU01:DBRCDF2)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWMMULC    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PMU01:DBRCDF3)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWMMULD    IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PMU01:DBRCDF4)
     F*  Secondary logical file definition (for displaying items)
     FWMMUL1    IF   E           K DISK
     F                                        RENAME(PMU01:PMU01L1)
      *** End   Change *** 01/31/19 ***************************************
     F*
     F*  Secondary logical file definition (for displaying items)
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      **** BEGIN ADD **** 06/30/00 ************************************
     FWTENL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      **** END   ADD **** 06/30/00 ************************************
      **** BEG ADD **** 05/03/13 ************************************
     FWledl     uf   e           k disk    rename(ped01:ped00)
      **** End ADD **** 05/03/13 ************************************
      /COPY SCOPYWW01D
     D*================================================================
     D*  Arrays & Tables
     D*================================================================
     D HD              S             79    DIM(8) CTDATA PERRCD(1)              Column headings
     D**** BEGIN CHG **** 06/30/00 ************************************
     D*** BEGIN CHG *** 06/07/00 ***********************************************
     D******************  OF      1  13  1   OFD    35   Options, Function
     D******************* OF      1  15  1   OFD    35   Options, Function
     D*** END   CHG *** 06/07/00 ***********************************************
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D**** END   CHG **** 06/30/00 ************************************
     D*================================================================
     D*  Data structures & constants
     D*================================================================
      *** beg add ***** 01/16/17 **************************************
     d/copy sPrLetter
     d/copy sPrXML
     d isUseScript     s              1n   inz
     d processType     c                   const('Policy Page')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
      *** end add ***** 01/16/17 **************************************
      *** Beg Add ***** 08/20/18 **********************************************
     D##Env            S              1a
     D@@Env            S              1a
      *** End Add ***** 08/20/18 *********************************************
      ***** BEG ADD ******* 03/25/05 **********************************
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26  0
      *** beg add ***** 02/29/24 ******************
     d  ldForm                41     55
      *** end add ***** 02/29/24 ******************
      ***** END ADD ******* 03/25/05 **********************************
      *** beg add ***** 01/14/17 **********************************************
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add ***** 01/14/17 **********************************************
      *
     D                 DS
     D  VIEW1                  1     52
     D  XXFOR1                 1     12
     D  XXDES1                14     43
     D  XXEND1                45     49
     D  XXST1                 51     52
     D*
     D                 DS
     D  VIEW2                  1     52
     D  XXEND2                 1      5
     D  XXFOR2                 7     18
     D  XXDES2                20     49
     D  XXST2                 51     52
     D*
     D                 DS
     D  VIEW3                  1     52
     D  XXST3                  1      2
     D  XXFOR3                 4     15
     D  XXDES3                17     46
     D  XXEND3                48     52
     D*
     D                 DS
     D  VIEW4                  1     52
     D  XXDES4                 1     30
     D  XXFOR4                32     43
     D  XXEND4                45     49
     D  XXST4                 51     52
     D*
     D                 DS
     D  INFO1                  1     22
     D  XXEEDT                 1     10
     D  XXMNA1                13     13
     D  XXPRN1                17     17
     D  XXEXF1                21     21
     D*
     D                 DS
     D  INFO2                  1     22
     D  XXIDT                  1     10
     D  XXMNA2                13     13
     D  XXPRN2                17     17
     D  XXEXF2                21     21
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
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Endorsement-
     D                                      Forms')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1054R02')
      **** BEGIN ADD **** 06/30/00 ************************************
     D PRTPGM          C                   CONST('W7054C')
      **** END   ADD **** 06/30/00 ************************************
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D CMPRES          C                   CONST('WA005R')
     D USPAPI          C                   CONST('W0006R')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(4)
     D SF1PAG          C                   CONST(9)
     D*** BEGIN ADD *** 06/07/00 ***********************************************
     D RAPGM           C                   CONST('W1053R3')

     I*** END   ADD *** 06/07/00 ***********************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
      *** Beg Add ***** 08/20/18 *************
     C                   PARM                    @@ENV
      *** End Add ***** 08/20/18 *************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
      *** Beg Chg ***** 02/29/24 *************
     C*****              PARM      SCDBRR        ##RRN             9 0
     C*****              PARM      @@RRN         ##RRN2            9 0
     C                   PARM                    xxRRN             9
     C                   PARM                    xxRRN2            9
      *** End Chg ***** 02/29/24 *************
      *** Beg Add ***** 08/20/18 *************
     C                   PARM                    ##ENV
      *** End Add ***** 08/20/18 *************
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
     C*  Change Numeric to Alpha program parameter list
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
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
     C*** BEGIN ADD *** 06/07/00 ***********************************************
     C     RAPL          PLIST
     C                   PARM                    #ACO#             3
     C                   PARM                    #AFND             3
     C                   PARM                    #AEMP#            9
     C                   PARM                    #AFYR             3
     C                   PARM                    RTNCD             1
     C*** END   ADD *** 06/07/00 ***********************************************
      **** BEGIN ADD **** 06/30/00 ************************************
     C*
     C*  Print Policy Page Endorsement program parameter list
     C*
     C     PRTPL         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    XXFYR
     C                   PARM                    ##JOB             6
     C                   PARM      XXPRNT        ##PRT            10
     C                   PARM                    ##OUTQ           10
     C                   PARM      SCFORM        ##FRM#           15
     C                   PARM      ENFLDR        ##FLDR           32
      **** END   ADD **** 06/30/00 ************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *** Begin Delete *** 01/31/19 *******************************
     C***  KEY1          KLIST
     C***                KFLD                    KKCO#
     C***                KFLD                    KKFND
     C***                KFLD                    KKEMP#
     C***                KFLD                    KKDIV
     C***                KFLD                    KKFYR
     C***
     C***  KEY2          KLIST
     C***                KFLD                    KKCO#
     C***                KFLD                    KKFND
     C***                KFLD                    KKEMP#
     C***                KFLD                    KKDIV
     C***                KFLD                    KKFYR
     C***                KFLD                    KKFORM
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C***                KFLD                    KKEND#
      *** END   ADD *** 11/01/99 ***********************************************
     C***
     C***  KEY3          KLIST
     C***                KFLD                    KKCO#
     C***                KFLD                    KKFND
     C***                KFLD                    KKEMP#
     C***                KFLD                    KKDIV
     C***                KFLD                    KKFYR
     C***                KFLD                    KKEND#
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C***                KFLD                    KKFORM
      *** END   ADD *** 11/01/99 ***********************************************
     C***
     C***  KEY4          KLIST
     C***                KFLD                    KKCO#
     C***                KFLD                    KKFND
     C***                KFLD                    KKEMP#
     C***                KFLD                    KKDIV
     C***                KFLD                    KKFYR
     C***                KFLD                    KKENST
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C***                KFLD                    KKFORM
      *** END   ADD *** 11/01/99 ***********************************************
     C***
     C***  KEY5          KLIST
     C***                KFLD                    KKCO#
     C***                KFLD                    KKFND
     C***                KFLD                    KKEMP#
     C***                KFLD                    KKDIV
     C***                KFLD                    KKFYR
     C***                KFLD                    KKDESC
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C***                KFLD                    KKFORM
     C***                KFLD                    KKEND#
      *** END   ADD *** 11/01/99 ***********************************************
      *** End   Delete *** 01/31/19 *******************************
      ***
      ***
      *** Begin Add    *** 01/31/19 *******************************
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKFORM
     C                   KFLD                    KKEND#
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEND#
     C                   KFLD                    KKFORM
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKENST
     C                   KFLD                    KKFORM
     C*
     C     KEY5          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKDESC
     C                   KFLD                    KKFORM
     C                   KFLD                    KKEND#
      *** End   Add    *** 01/31/19 *******************************
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C*  EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
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
      **** BEGIN ADD **** 06/30/00 ************************************
     C*
     C*  ENDORSEMENT MASTER
     C*
     C     ENKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    SCFORM
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      **** END   ADD **** 06/30/00 ************************************
      **** BEG ADD **** 05/03/13 ************************************
      * Key List on file WLEDL
     c     keyWLEDL      KLIST
     c                   KFLD                    KKCO#
     c                   KFLD                    KKFND
     c                   KFLD                    KKEMP#
     c                   KFLD                    KKDIV
     c                   KFLD                    KKFYR
     c                   KFLD                    MUFORM
      **** END ADD **** 05/03/13 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MUCO#         KKCO#
     C     *LIKE         DEFINE    MUCO#         ##CO#
     C     *LIKE         DEFINE    MUFND         KKFND
     C     *LIKE         DEFINE    MUFND         ##FND
     C     *LIKE         DEFINE    MUEMP#        KKEMP#
     C     *LIKE         DEFINE    MUEMP#        ##EMP#
     C     *LIKE         DEFINE    MUDIV         KKDIV
     C     *LIKE         DEFINE    MUDIV         ##DIV
     C     *LIKE         DEFINE    MUFYR         KKFYR
     C     *LIKE         DEFINE    MUFYR         ##FYR
     C     *LIKE         DEFINE    MUFORM        KKFORM
     C     *LIKE         DEFINE    MUFORM        @@FORM
     C     *LIKE         DEFINE    MUEND#        KKEND#
     C     *LIKE         DEFINE    MUENST        KKENST
     C     *LIKE         DEFINE    MUDESC        KKDESC
     C     *LIKE         DEFINE    MUDESC        U1PDSC
     C     *LIKE         DEFINE    MUDESC        U2PDSC
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
B01  C     SSEXFL        ORNE      *BLANKS
B01  C     SSMNAU        ORNE      *BLANKS
B01  C     SSPRNT        ORNE      *BLANKS
B01  C     SSEND#        ORNE      *ZEROS
      *** Begin Add *** 01/31/19 *************************************
B01  C     SSDIV         ORNE      *BLANKS
      *** End   Add *** 01/31/19 *************************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      //*** Beg Add ***** 02/29/24 *************
      /Free
       xxRRN = %Editc(SCDBRR:'X');
      /End-Free
      //*** End Add ***** 02/29/24 *************
      **** BEGIN ADD **** 06/01/00 ************************************
     C     SCSEL         IFEQ      '3'
     C*
     C*  Verify that the form is a "manual attach"
     C*
     C     SCMNAU        IFNE      'M'
     C                   SETON                                        30  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  Verify that the endorsement # is equal to zero.
     C*
     C  N90SCEND#        IFNE      *ZERO
     C                   SETON                                        31  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF
      **** END   ADD **** 06/01/00 ************************************
     C*
     C***  Begin Add  ***  04/21/06  **************************************
     C  N90SCSEL         IFEQ      '6'
     C                   MOVEL(P)  SCFORM        X8FORM            8
     C                   SELECT
     C     X8FORM        WHENEQ    'WC890600'
     C                   SETON                                        93  90
     C                   ENDSL
     C                   ENDIF
     C***  End   Add  ***  04/21/06  **************************************
      ***** BEG ADD ******* 04/01/07 **********************************
      *
      * Deletion of Mid-term endorsement records not allowed.
      *
     C  N90SCSEL         IFEQ      '4'
     C     SCEND#        ANDNE     *ZERO
     C                   SETON                                        71  90
     C                   ENDIF
      ***** END ADD ******* 04/01/07 **********************************
      *** BEG DEL *** 06/05/08 ********************************************
      *** BEG ADD *** 11/01/07 ********************************************
     C********
     C*******    *IN90     IFEQ *OFF                       NO ERRORS SO FAR
     C*******
     C*******              CALL 'SRCKES'
     C*******              PARM           ELCO#
     C*******              PARM           ELFND
     C*******              PARM           ELEMP#
     C*******              PARM           ELFYR
     C*******              PARM           SCFORM
     C*******              PARM *BLANK    #@RTN   1
     C*******
     C*******    #@RTN     IFEQ 'N'
     C*******              SETON                     73  90
     C*******              ENDIF
     C*******
     C*******              ENDIF                           *IN90 = *OFF
      *** END ADD *** 11/01/07 ********************************************
      *** END DEL *** 06/05/08 ********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSEXFL        IFNE      ' '
     C     SSEXFL        ANDNE     'I'
     C     SSEXFL        ANDNE     'E'
     C                   SETON                                        8590
     C                   ENDIF
     C*
     C     SSMNAU        IFNE      ' '
     C     SSMNAU        ANDNE     'A'
     C     SSMNAU        ANDNE     'M'
     C                   SETON                                        8690
     C                   ENDIF
     C*
     C     SSPRNT        IFNE      ' '
     C     SSPRNT        ANDNE     'Y'
     C     SSPRNT        ANDNE     'N'
     C                   SETON                                        8790
     C                   ENDIF
     C*
     C     SSENST        IFNE      ' '
     C                   MOVEL(P)  SSENST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        8890
     C                   ENDIF
     C*
     C     SSEND#        IFLT      *ZERO
     C                   SETON                                        8990
     C                   ENDIF
      /Free
       //*** Begin Add 01/31/19 *************************************
       *in84 = *off;
       If SSDIV <> *blanks;
          kkdiv = %INT(ssdiv);
         Setll (KKco#:KKfnd:KKemp#:KKdiv) PMU01L1;
         If Not %equal;
            *in84 = *on;
            *in90 = *on;
         Endif;
       Endif;

       //*** End   Add 01/31/19 *************************************

      /End-Free
     C*
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
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVE      *LOVAL        KKFORM
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVE      *LOVAL        KKEND#
      *** END   ADD *** 11/01/99 ***********************************************
     C     *IN22         WHENEQ    *ON
     C                   MOVE      *LOVAL        KKEND#
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVE      *LOVAL        KKFORM
      *** END   ADD *** 11/01/99 ***********************************************
     C     *IN23         WHENEQ    *ON
     C                   MOVE      *LOVAL        KKENST
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVE      *LOVAL        KKFORM
      *** END   ADD *** 11/01/99 ***********************************************
     C     *IN24         WHENEQ    *ON
     C                   MOVE      *LOVAL        KKDESC
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVE      *LOVAL        KKFORM
     C                   MOVE      *LOVAL        KKEND#
      *** END   ADD *** 11/01/99 ***********************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCPST1        KKFORM
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCPST2        KKEND#
     C     *IN23         WHENEQ    *ON
     C                   MOVEL     SCPST3        KKENST
     C     *IN24         WHENEQ    *ON
     C                   MOVEL     SCPST4        KKDESC
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCFORM        KKFORM
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVEL     SCEND#        KKEND#
      *** END   ADD *** 11/01/99 ***********************************************
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCEND#        KKEND#
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVEL     SCFORM        KKFORM
      *** END   ADD *** 11/01/99 ***********************************************
     C     *IN23         WHENEQ    *ON
     C                   MOVEL     SCENST        KKENST
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVEL     SCFORM        KKFORM
      *** END   ADD *** 11/01/99 ***********************************************
     C     *IN24         WHENEQ    *ON
     C                   MOVEL     SCDESC        KKDESC
      *** BEGIN ADD *** 11/01/99 ***********************************************
     C                   MOVEL     SCFORM        KKFORM
     C                   MOVE      SCEND#        KKEND#
      *** END   ADD *** 11/01/99 ***********************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/07/00 ***********************************************
     C*================================================================
     C* KUDO - F20=Reattach
     C*================================================================
     C     KUDO          BEGSR
     C*
     C                   SETOFF                                       72
     C*
     C                   MOVE      ELCO#         #ACO#
     C                   MOVE      ELFND         #AFND
     C                   MOVE      ELEMP#        #AEMP#
     C                   MOVE      ELFYR         #AFYR
     C                   MOVE      ' '           RTNCD
     C*
     C                   CALL      RAPGM         RAPL
     C     RTNCD         IFNE      '1'
     C                   SETON                                        72
     C                   ENDIF
     C*
     C                   ENDSR                                                  KUDO
     C*** END   ADD *** 06/07/00 ***********************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  LOAD DATA DATA FIELDS TO HIDDEN SCREEN FIELDS
     C*
     C                   MOVE      MUEND#        SCEND#
     C                   MOVEL     MUFORM        SCFORM
     C                   MOVEL     MUENST        SCENST
      *** beg chg ***** 11/10/16 ***************************
     C***********        MOVEL     MUDESC        SCDESC
      /Free
       // Use extended description if the attaching program has
       //  been updated to populate it.
       If MUdsc2 <> *blanks;
          scDesc = MUdsc2;
       Else;
          scDesc = MUdesc;
       Endif;
      /End-free
      *** end chg ***** 11/10/16 ***************************
     C*
     C                   Z-ADD     MUEEDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCEEDT
     C*
     C                   MOVEL     MUEXFL        SCEXFL
     C                   MOVEL     MUMNAU        SCMNAU
     C                   MOVEL     MUPRNT        SCPRNT
     C*
     C                   Z-ADD     MUIDT         SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCIDT
     C*
     C*  LOAD THE APPROPRIATE VIEW DATA SUBFIELDS
     C*
     C                   MOVEL(P)  SCEND#        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C     XXNUM         IFEQ      *BLANKS
     C                   MOVEL     '    0'       XXEND#            5
     C                   ELSE
     C                   MOVEL     XXNUM         XXEND#
     C                   ENDIF
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCFORM        XXFOR1
     C                   MOVEL     SCDESC        XXDES1
     C                   MOVEL     SCENST        XXST1
     C                   MOVEL     XXEND#        XXEND1
     C                   MOVEL(P)  VIEW1         SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL     SCFORM        XXFOR2
     C                   MOVEL     SCDESC        XXDES2
     C                   MOVEL     SCENST        XXST2
     C                   MOVEL     XXEND#        XXEND2
     C                   MOVEL(P)  VIEW2         SCDATA
     C     *IN23         WHENEQ    *ON
     C                   MOVEL     SCFORM        XXFOR3
     C                   MOVEL     SCDESC        XXDES3
     C                   MOVEL     SCENST        XXST3
     C                   MOVEL     XXEND#        XXEND3
     C                   MOVEL(P)  VIEW3         SCDATA
     C     *IN24         WHENEQ    *ON
     C                   MOVEL     SCFORM        XXFOR4
     C                   MOVEL     SCDESC        XXDES4
     C                   MOVEL     SCENST        XXST4
     C                   MOVEL     XXEND#        XXEND4
     C                   MOVEL(P)  VIEW4         SCDATA
     C                   ENDSL
     C*
     C*  LOAD THE APPROPRIATE INFO DATA SUBFIELDS
     C*
     C*
     C                   SELECT
     C     INFO#         WHENEQ    1
     C                   MOVEL(P)  SCEEDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEEDT
     C                   MOVEL     SCMNAU        XXMNA1
     C                   MOVEL     SCPRNT        XXPRN1
     C                   MOVEL     SCEXFL        XXEXF1
     C                   MOVE      INFO1         SCDATA
     C*
     C     INFO#         WHENEQ    2
     C                   MOVEL(P)  SCIDT         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXIDT
     C                   MOVEL     SCMNAU        XXMNA2
     C                   MOVEL     SCPRNT        XXPRN2
     C                   MOVEL     SCEXFL        XXEXF2
     C                   MOVE      INFO2         SCDATA
     C                   ENDSL
     C*
     C                   ENDSR
      **** BEGIN ADD **** 06/30/00 ************************************
     C*================================================================
     C* OPT 6 - SUBROUTINE FOR OPTION 6
     C*================================================================
     C*
     C* OPTION 6 = PRINT SINGLE ENDORSEMENT
     C*
     C     OPT6          BEGSR
     C*
     C     SCEND#        IFEQ      *ZERO
     C     SCPRNT        IFEQ      'Y'

      *** beg add ***** 01/16/17 ******************************
      /Free
       If isUseScript;
          xxProcType = processType;
          In *Lock *dtaara;
          xxDart = 'N';
          xxRmCd = *blanks;
          Out *dtaara;
        //***  Begin Chg  ***  09/28/17  **********************************
        //xxSubmit = OutputWindow(xxProcType:xxMass:'Y');
          xxSubmit = OutputWindow(xxProcType:xxMass);
          If not xxSubmit;
             Leavesr;
          Endif;
        //***  End   Chg  ***  09/28/17  **********************************
       Endif;
      /End-free
      *** end add ***** 01/16/17 ******************************

     C     ENKY          CHAIN     PEN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ENPGM2        XXPRNT           10
     C                   ENDIF
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPOUTQ        ##OUTQ
     C                   ENDIF
     C*
     C                   MOVE      SCEND#        XXEND             5
     C                   MOVE      ELCO#         XXCO#             3
     C                   MOVE      ELFND         XXFND             3
     C                   MOVE      ELEMP#        XXEMP#            9
     C                   MOVE      ELDIV         XXDIV             5
     C                   MOVE      ELFYR         XXFYR             3
      ***** BEG ADD ******* 03/25/05 **********************************
      *
     C                   EXSR      GETEND
      *
      ***** END ADD ******* 03/25/05 **********************************
     C*
     C                   CALL      PRTPGM        PRTPL
     C*
     C                   ELSE
     C*
     C                   SETON                                        1690
     C*
     C                   ENDIF                                                  SCPRNT = Y
     C*
     C                   ELSE
      **** BEGIN ADD **** 03/15/02 ************************************
     C     SCPRNT        IFEQ      'Y'

      *** beg add ***** 01/16/17 ******************************
      /Free
       If isUseScript;
          xxProcType = processType;
          In *Lock *dtaara;
          xxDart = 'N';
          xxRmCd = *blanks;
          Out *dtaara;
        //***  Begin Chg  ***  09/28/17  **********************************
        //xxSubmit = OutputWindow(xxProcType:xxMass:'Y');
          xxSubmit = OutputWindow(xxProcType:xxMass);
          If not xxSubmit;
             Leavesr;
          Endif;
        //***  End   Chg  ***  09/28/17  **********************************
       Endif;
      /End-free
      *** end add ***** 01/16/17 ******************************

     C     ENKY          CHAIN     PEN01                              60
     C     *IN60         IFEQ      *OFF
     C     ENPGM2        ANDNE     'SR89600'
     C                   MOVE      ENPGM2        XXPRNT           10
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPOUTQ        ##OUTQ
     C                   ENDIF
      *
     C                   MOVE      SCEND#        XXEND             5
     C                   MOVE      ELCO#         XXCO#             3
     C                   MOVE      ELFND         XXFND             3
     C                   MOVE      ELEMP#        XXEMP#            9
     C                   MOVE      ELDIV         XXDIV             5
     C                   MOVE      ELFYR         XXFYR             3
      *
      ***** BEG ADD ******* 03/25/05 **********************************
      *
     C                   EXSR      GETEND
      *
      ***** END ADD ******* 03/25/05 **********************************
     C                   CALL      PRTPGM        PRTPL
      *
     C                   ELSE
     C                   SETON                                        1790
      *
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   SETON                                        1690
      *
     C                   ENDIF                                                  SCPRNT = Y
      **** END   ADD **** 03/15/02 ************************************
     C*
      **** BEGIN DEL **** 03/15/02 ************************************
     C******************** SETON                     1790
      **** END   DEL **** 03/15/02 ************************************
     C*
     C                   ENDIF                                                  XXEND# = 0
     C*
     C                   ENDSR
      **** END   ADD **** 06/30/00 ************************************
      ***** BEG ADD ******* 03/25/05 **********************************
      *
      *
      *
      *================================================================
      * GET END - LOAD ENDORSEMENT NUMBER AND EFFECTIVE DATE
      *================================================================
     C     GETEND        BEGSR
      *
      *  LOAD ENDORSEMENT NUMBER TO LOCAL DATA AREA POSITION 1 THRU 5
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVE      SCEND#        LDEND#
     C                   MOVE      *ZERO         LDMYRR
      *
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         LDEEDT
      *
     C                   MOVE      ELFYR         LDFYR
     C                   OUT       *DTAARA
     C                   ENDSR                                                  GETEND
     C*
      ***** END ADD ******* 03/25/05 **********************************
     C*
     C*
     C*================================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*================================================================
     C*
     C* OPTION 10=CHANGE INCLUDE/EXCLUDE STATUS
     C*
     C     OPT10         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C*** BEGIN ADD *** 06/07/00 ***********************************************
     C     *INKU         CASEQ     *ON           KUDO
     C*** END   ADD *** 06/07/00 ***********************************************
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '10'          OPT10
      **** BEGIN ADD **** 06/30/00 ************************************
     C     SCSEL         CASEQ     '6'           OPT6
      **** END   ADD **** 06/30/00 ************************************
     C                   ENDCS
     C*
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

       //*** beg add ***** 01/16/17 ************************
      /Free
       isUseScript = UseScriptura(processType);
      /End-Free
       //*** end add ***** 01/16/17 ************************
      //*** Beg Add ***** 08/20/18 *******************
      /free
         ##Env = ' ';
         If %Parms > 4;
            If @@Env <> *blanks;
               ##Env = @@ENV;
            Endif;
         Endif;
      /End-free
      //*** End Add ***** 08/20/18 *******************
      //*** Beg Add ***** 02/29/24 *************
      /Free
       if xxRRN = *blanks;
          xxRRN  = '000000000';
       endif;
       xxRRN2 = %Editc(@@RRN:'X');
      /End-Free
      //*** End Add ***** 02/29/24 *************

     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C                   Z-ADD     2             VIEW#
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C     @@RRN         CHAIN     PEL01                              60
     C*
      //*** beg add ***** 02/29/24 ***********************
      /free
       If @@call = '6';                  // 6 - Print Form
          scend# = 0;
          scprnt = 'Y';
          // Get Form Name from LDA
          In *Lock *dtaara;
          scForm = ldform;
          Out *dtaara;

          // Print Form
          Exsr opt6;
          Return;
       Endif;
      /End-free
      //*** end add ***** 02/29/24 ***********************
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
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
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
     C                   MOVE      *OFF          *IN78                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN78                          CANCELLED
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1          READE     DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C     KEY1          READE     DBRCDF2                                95
     C     *IN23         WHENEQ    *ON
     C     KEY1          READE     DBRCDF3                                95
     C     *IN24         WHENEQ    *ON
     C     KEY1          READE     DBRCDF4                                95
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF2                                95
     C     *IN23         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF3                                95
     C     *IN24         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF4                                95
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY2          SETGT     DBRCDF                             96
     C     *IN22         WHENEQ    *ON
     C     KEY3          SETGT     DBRCDF2                            96
     C     *IN23         WHENEQ    *ON
     C     KEY4          SETGT     DBRCDF3                            96
     C     *IN24         WHENEQ    *ON
     C     KEY5          SETGT     DBRCDF4                            96
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY2          SETLL     DBRCDF                             96
     C     *IN22         WHENEQ    *ON
     C     KEY3          SETLL     DBRCDF2                            96
     C     *IN23         WHENEQ    *ON
     C     KEY4          SETLL     DBRCDF3                            96
     C     *IN24         WHENEQ    *ON
     C     KEY5          SETLL     DBRCDF4                            96
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
      **** BEG ADD **** 05/03/13 ************************************
      /Free
       Setll keyWLEDL ped00;
       Reade keyWLEDL ped00;
       Dow Not %EOF(wledl);
          Delete ped00;
       Reade keyWLEDL ped00;
       EndDo;
      /End-Free
      **** End ADD **** 05/03/13 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
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
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
      *** beg chg ***** 11/10/16 ***************************
     C*****LO:HI*        XLATE(P)  MUDESC        U1PDSC
      // Just the first 30 positions of MUdsc2 will be used
      //  in the subset logic for backwards compatibility.
      //  Might cause unexpected results in subset selection.
     c                   If        MUdsc2 <> *blanks
     c     lo:hi         Xlate(p)  MUdsc2        u1pdsc
     c                   Else
     c     lo:hi         Xlate(p)  MUdesc        u1pdsc
     c                   Endif
      *** end chg ***** 11/10/16 ***************************
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSEXFL        IFNE      *BLANKS
     C     SSEXFL        ANDNE     MUEXFL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSMNAU        IFNE      *BLANKS
     C     SSMNAU        ANDNE     MUMNAU
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSPRNT        IFNE      *BLANKS
     C     SSPRNT        ANDNE     MUPRNT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSENST        IFNE      *BLANKS
     C     SSENST        ANDNE     MUENST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSEND#        IFNE      *ZERO
     C     SSEND#        ANDNE     MUEND#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C***  Begin Add  ***  10/28/05  **************************************
     C     'X'           IFEQ      MUSTAT                                       Deleted
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C***  End   Add  ***  10/28/05  **************************************
      /Free
       // Begin Add *** 01/31/19 ******************************************
       If SSDIV <> *BLANKS and %int(SSDIV) <> MUdiv;
          MEETSS = *OFF;
       Endif;
       // End   Add *** 01/31/19 ******************************************
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
** Column headings
Opt Form         Description                    Endr# St  Endr Date M/A Prt I/E
Opt Endr# Form         Description                    St  Endr Date M/A Prt I/E
Opt St Form         Description                    Endr#  Endr Date M/A Prt I/E
Opt Description                    Form         Endr# St  Endr Date M/A Prt I/E
Opt Form         Description                    Endr# St Date Added M/A Prt I/E
Opt Endr# Form         Description                    St Date Added M/A Prt I/E
Opt St Form         Description                    Endr# Date Added M/A Prt I/E
Opt Description                    Form         Endr# St Date Added M/A Prt I/E
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Print                  YYY
11010=Change Inc/Exc        YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
220F20=Reattach             YNN
3
