     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0030R - Payee Master Maintenance - Search Program
     H*****************************************************************
     H*
     H*  Description:
     H*
     H*    This program displays Payee records.  The user has the
     H*    ability to execute the following functions:
     H*
     H*      1) Re-position at a specific record.
     H*      2) F11 to toggle between:
     H*         Payee Name order and FEIN order        -or-
     H*         Subset Payee Name order and FEIN order with
     H*         PDDBA1 equal 'Y'.
     H*      3) F6 to initiate the entry of a New Payee.
     H*      4) INQUIRY Mode:
     H*           Option '5' to Display
     H*         SELECT Mode:
     H*           Option '1' to Select
     H*           Option '5' to Display
     H*         UPDATE Mode:
     H*           Option '2' to Update
     H*           Option '5' to Display
     H*           Option '8' to Update WCMC Info only
     H*           Option '20' to Activate/Inactivate record
     H*      5) F17 to enter Subset fields to limit Display records
     H*
     H*  Called From:
     H*    Menu
     H*    Select programs
     H*
     H*  Calls To:
     H*    FA005R
     H*    F0032R
     H*    QCLSCAN
     H*    SA001R
     H*    SRCENTER
     H*    SRNAME
     H*    W0030R1
     H*
     H*  Created:  04/15/97  C:30  Project:  WC607
     H*****************************************************************
     H* Revisions:
     H*
     H* 05/22/19 C:079 Support: 152/79622
     H*                Use isModEnabled for Managed Care install flags
     H*
     H* 10/27/14 C:212 Project: 25MTT
     H*                Provider Modifier Transfer Functionality
     H*                - Add option 71
     H*
     H* 03/14/13 C:279 Project: SY266  Redmine: 2537
     H*                Position to field for FEIN should not accept
     H*                special characters.
     H*
     H* 11/02/09 C:232 PROJECT: 139WMR
     H*                OFAC Reporting-Add Pending Status Indicator.
     H*
     H* 09/07/09 C:234 SUPPORT 69726
     H*                Not implemented in V10 because issue was
     H*                related to compilation only.
     H*
     H* 07/13/09 C:180 PROJECT WC1836
     H*                Return payee/provider name
     H*
     H* 06/29/09 C:191 PROJECT: WC1806
     H*                EFT - UPDATE CALL TO W0028R
     H*
     H* 09/20/07 C:136 PROJECT: WC997
     H*                EFT - AGENT COMMISSIONS (ADD PARM FOR TYPE)
     H*
     H* 08/09/07 C:191 PROJECT WC1453
     H*              - Add option 70 = EFT Account Info, Option not
     H*                allowed if the EFT control Flag is not a 'Y'
     H*
     H* 06/19/06 C:132 PROJECT 133WDD
     H*              - Add Retrieve Client ID, and if is client 133 (LUBA)
     H*                subset to active records if Inquiry or Search Mode.
     H*
     H* 12/21/05 C:168 PROJECT 114WKX
     H*                ADDED OPTION 11 TO ALLOW THE USER TO VIEW ALL   IA THAT
     H*                THE RECORDS ON CHANGE LOG FILES
     H*
     H* 05/01/01 C:134 PROJECT WC577
     H*                ADD FEIN NUMBER TO SCREEN THREE'S LIST OF CRITERIA THAT
     H*                CAN BE USED TO SPECIFY A SUBSET LIST
     H*
     H* 05/01/01 C:134 PROJECT WC577
     H*                ADD FEIN NUMBER TO SCREEN THREE'S LIST OF CRITERIA THAT
     H*                CAN BE USED TO SPECIFY A SUBSET LIST
     H*
     H* 11/30/00 C:53  PROJECT WC577
     H*                ADD ZIP 4 TO DISPLAY
     H*
     H* 10/13/99 C:33  PROJECT WC577
     H*                DISPLAY 1099 MOD
     H*
     H* 08/18/99 C:61  PROJECT WC823
     H*              - Extensive program revision correcting numerous
     H*                issues.  Page Up, Page Down, Position to, views,
     H*                subsets, etc.  Changes not documented.
     H*
     H*  05/24/99  C:62  SEQ#: MC145
     H*                  CHANGE OPTION '8' TO CALL F0032R INSTEAD
     H*                  OF F0032R01.
     H*
     H*  07/23/98  C:33  SEQ#: WA033/12232
     H*                  RECOMPILED DUE TO CHANGE OF SCPSTC FROM
     H*                  ALPHA TO NUMERIC AND CHECK(RZ)
     H*
     H*  05/19/89  C:33  SEQ#: WA048/11542
     H*                  ALLOW PROGRAM TO HAVE ONLY ONE PARM WHEN
     H*                  CALLED FROM THE MENU
     H*
     H*  PLEASE ADD TO TOP OF REVISIONS
     H*****************************************************************
     H*
     H*  Indicators:
     H*
     H* Global
     H*   09 - Inquiry
     H*   19 - Call from other pgm - SELECT mode
     H*   21 - Name Logical sort
     H*   22 - FEIN Logical sort
     H*   23 - DBA Name / address sort
     H*   60 - General use
     H*
     H* SCR03
     H*   48 - Specialty code must be valid
     H*   71 - Subset Window - Status Flag invalid (A/I)
     H*   72 - Subset Window - DBA Flag invalid (Y/ )
     H*   73 - Subset Window - Provider Type invalid (1 - 6)
     H*   74 - Subset Window - Provider Type invalid (1 - 6)
     H*   75 - Subset Window - Provider Type invalid (1 - 6)
     H*   76 - Subset Window - Provider Type invalid (1 - 6)
     H*   77 - Subset Window - Provider Type invalid (1 - 6)
     H*   78 - Subset Window - Provider Type invalid (1 - 6)
     H*   80 - Subset Window - Must be greater that zero
     H*****************************************************************
      *
      *
      * Sub File Display Screen
     FW0030F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      * Payee Demographics Physical
     FWMPDP     IF   E             DISK
     F                                     RENAME(PPD01:PFRCDF)
      *
      * Payee Demographics   Key=NAME/FSN/FEIN/MOD
     FWMPDL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPD01:DBRCDF)
      *
      * Payee Demographics   Key=FEIN/MOD
     FWMPDL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPD01:DBRCDF2)
      *
      * Payee Demographics   Key=ADR1/FEIN/MOD
     FWMPDL3    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPD01:DBRCDF3)
      *
     F*** BEGIN ADD **** 08/09/07 ***********************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** END ADD ****** 08/09/07 ***********************************
      ***********************************************
      *
      *  State Code Table File
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      *
      *  Table File
     FWTABL     IF   E           K DISK
      *
      *
     F***** Beg Add *** 10/27/14 ************************************
      *
      *  Provider Modifier From/To File
     FWAPTP     UF A E           K DISK
     F                                     infds(waptpds)
     F***** End Add *** 10/27/14 ************************************
     D***** Beg Add *** 10/27/14 ************************************
      *
     D W7090C          pr                  extpgm('W7090C')
     D inDelMod                       1
     D inRunDate                      6
     D inRunTime                      5
     D inPtpRrn                       9
     D outRtnCode                     1
      *
     D***** End Add *** 10/27/14 ************************************
     D*** Beg Add *** 05/22/19 **************************************
      /Copy sprModule
     D*** End Add *** 05/22/19 **************************************
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(3) CTDATA PERRCD(1)              Column Headings
     D*** BEGIN CHG *** 08/09/07 ************************************
     D**********          OF      1  19  1   OFD    35   Options, Function
     D***** Beg Chg *** 10/27/14 ************************************
     D OF              S              1    DIM(21) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(21) ALT(OF)
     D*** OF              S              1    DIM(20) CTDATA PERRCD(1)          Options, Function
     D*** OFD             S             35    DIM(20) ALT(OF)
     D***** End Chg *** 10/27/14 ************************************
      *** END CHG **** 08/09/07 ************************************
      *
     D***PY SCOPYWW01E
      *
      *
      *
     D***PY SCOPYWW01I
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  TY                     1      6
     D                                     DIM(6)                               Provider Type
     D  SCSTY1                 1      1
     D  SCSTY2                 2      2
     D  SCSTY3                 3      3
     D  SCSTY4                 4      4
     D  SCSTY5                 5      5
     D  SCSTY6                 6      6
      *
     D                 DS
     D  FEINMD                 1     14
      *
     D***** Beg Add *** 10/27/14 ************************************

     D waptpds         ds
     D  ptprrn               397    400b 0

     D JobRunTmHMS     ds
     D  JobRunTmDta                   5    dtaara(PROVCHGDTA)
     D  JobRunHH                      2    overlay(JobRunTmDta:1)
     D  JobRunMM                      2    overlay(JobRunTmDta:4)
     D  TmSep                         1    inz(':')
     D  JobRunSS                      2    inz('00')
     D***** End Add *** 10/27/14 ************************************
      *
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D PGMCO#          C                   CONST('FA007R    ')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D OPTNM           C                   CONST('Payee Demographics')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(9)
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0030R1')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D***** Beg Add ****** 03/14/13 *************************************
     D conocd          C                   Const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ1234567890 abcdefgh-
     D                                     ijklmnopqrstuvwxyz')
     D x               s              3  0
     D occupationPos   s              3  0
     D***** Beg Add ****** 03/14/13 *************************************
      *
      *  W0030R1
      *
     D*** BEGIN ADD *** 08/09/07 **************************************
     D                 DS
     D  XXFN                   1     12
     D  XXFEN1                 1      9
     D  XXMOD1                10     12

     I*** END ADD *** 08/09/07 ****************************************

     D***** Beg Add *** 10/27/14 ************************************
     D inDelMod        s              1    inz
     D inRunTime       s              5    inz
     D inRunDate       s              6    inz
     D inPtpRrn        s              9    inz
     D outRtnCode      s              1    inz
     D isValTrf        s               n   inz(*off)
     D jobruntm        s             10    inz
     D***** End Add *** 10/27/14 ************************************
     D*** Beg Add *** 05/22/19 **************************************
     d isWcMc          s              1n   inz('0')
     d isWcInt         s              1n   inz('0')
     D*** End Add *** 05/22/19 **************************************

     C     ACDPL         PLIST
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
      *  F0032R01
      *
     C     ACDPL2        PLIST
     C*** BEGIN CHG *** 05/24/99 **************************************
     C********** *INKC     PARM *INKC     ##KC    1
     C********** *INKL     PARM *INKL     ##KL    1
     C**********           PARM           ##MODE  1
     C**********           PARM QQMCPD    ##RRN   90
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      *BLANKS       ##1N4             1            type 1 & 4 only
     C                   PARM      'Y'           ##1PRV            1            Y=subset on fein/mod
     C*** END   CHG *** 05/24/99 **************************************
      *
      *  Company Name
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Managed Care File Maint Plist
      *
     C*** BEGIN DEL *** 05/24/99 **************************************
     C********** MCPL      PLIST
     C*** BEGIN CHG *** 03/09/98 **************************************
     C**********           PARM           ##MODE  1        Function
     C*** END   CHG *** 03/09/98 **************************************
     C**********           PARM           QQMCPD  90       FMPDP RRN
     C**********           PARM           QQMCP1  90       FMP1P RRN
     C**********           PARM           QQRTRN  1        Return Code
     C**********           PARM           PDFEIN
     C**********           PARM           PDMOD
     C**********           PARM           PDNAME
     C**********           PARM           PDFSN
     C**********           PARM           PDTITL
     C**********           PARM           PDADR1
     C**********           PARM           PDCTY
     C**********           PARM           PDST
     C**********           PARM           PDZIP
     C**********           PARM           PDAREA
     C**********           PARM           PDPHN#
     C**********           PARM           PDSHNM
     C**********           PARM           PD1099
     C**********           PARM           PDTYPE
     C**********           PARM           PDSPCD
     C**********           PARM           PDFLIC
     C**********           PARM           PDVFLG
     C**********           PARM           PDADR2
     C**********           PARM           PDDBA1
     C**********           PARM           PDFXAR
     C**********           PARM           PDFAX#
     C**********           PARM           PDWCCE
     C**********           PARM           PDSTAT
     C**********           PARM           PDWCDT
     C**********           PARM           PDBWC#
     C**********           PARM           PDBWCT
     C*** END   DEL *** 05/24/99 **************************************
     C*** BEGIN ADD **** 08/09/07 *************************************
     C*  KEY LIST FOR THE SYSTEM CONTROL FILES
     C*
     C     SYKY          KLIST
     C                   KFLD                    XSKEY
     C*** END ADD ****** 08/09/07 *************************************
      *
     C     KEY1          KLIST
     C                   KFLD                    XXNAME
     C                   KFLD                    XXFSN
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     KEY2          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     KEY3          KLIST
     C                   KFLD                    XXADR1
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
      *  SPECIALTY CODE TABLE FILE
      *
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
      *
      * *LIKE defines
      *
     C     *LIKE         DEFINE    PDNAME        XXNAME
     C     *LIKE         DEFINE    PDADR1        XXADR1
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PDFSN         XXFSN
     C     *LIKE         DEFINE    PDADR1        U1PDSC
     C     *LIKE         DEFINE    PDADR1        U2PDSC
     C     *LIKE         DEFINE    SPKEY         KKKEY
     C     *LIKE         DEFINE    SPKEY         ##KEY
     C*** BEGIN ADD **** 08/09/07 *************************************
     C     *LIKE         DEFINE    WCKEY         XSKEY
     C*** END ADD ****** 08/09/07 *************************************
     C*
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
      **** BEGIN ADD ******* 07/13/09 *********************************
     C                   PARM                    QQNAME           40
      **** END ADD   ******* 07/13/09 *********************************
      *
     C     *LIKE         DEFINE    SCPSTB        SVPSTB
     C     *LIKE         DEFINE    SCPSTC        SVPSTC
      *
     C/COPY SCOPYWW01C
      *
      **********************************************************************
      *  CHKSSM
      **********************************************************************
     C     CHKSSM        BEGSR
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSNAM        ORNE      *BLANK
     C     SCSDBC        ORNE      *BLANK
     C     SCSADR        ORNE      *BLANK
     C     SCSCTY        ORNE      *BLANK
     C     SCSST         ORNE      *BLANK
     C     SCSZIP        ORNE      *BLANK
     C     SCSPHN        ORNE      *BLANK
     C     SCSDBA        ORNE      *BLANK
     C     SCSTY1        ORNE      *BLANK
     C     SCSTY2        ORNE      *BLANK
     C     SCSTY3        ORNE      *BLANK
     C     SCSTY4        ORNE      *BLANK
     C     SCSTY5        ORNE      *BLANK
     C     SCSTY6        ORNE      *BLANK
     C     SCSSPC        ORNE      *BLANK
     C***  BEGIN ADD  ***  05/01/01  *********************************
     C     SCSSSF        ORNE      0
     C***  END   ADD  ***  05/01/01  *********************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR                                                  CHKSSM
      *
      *
      *
      **********************************************************************
      *  ERRCK1 - Edit OPT screen
      **********************************************************************
     C     ERRCK1        BEGSR
      *
      * Refresh WW screen after delete
      *
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
      *
      * Option 8 requires Provider Type '1' or '4'
      *
     C     SCSEL         IFEQ      '8'
     C     *IN90         ANDEQ     *OFF
     C     SCTYPE        ANDNE     '1'
     C     SCTYPE        ANDNE     '4'
     C                   SETON                                        889005
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      **********************************************************************
      *  ERRCK2 - Edit SCR03 Fields
      **********************************************************************
     C     ERRCK2        BEGSR
      *
      *  Search - subset screen
      *
     C                   SETOFF                                       454647
     C                   SETOFF                                       48
     C   KJ              EXSR      KJDO
      *
      *  Status value must be 'A', 'I', or Blank
      *
     C     SCSSTA        IFNE      *BLANK
      *
     C     SCSSTA        IFNE      'A'
     C     SCSSTA        ANDNE     'I'
     C*** BEG ADD *** 11/02/09 ****************************************
     C     CLTID         IFNE      '139'
     C     CLTID         OREQ      '139'
     C     SCSSTA        ANDNE     'P'
     C     CLTID         IFNE      '139'
     C*** END ADD *** 11/02/09 ****************************************
     C                   SETON                                        9071
     C*** BEG ADD *** 11/02/09 ****************************************
     C                   ELSE
     C                   SETON                                        9065
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
     C                   ENDIF
      *
     C                   ENDIF
     C***  BEGIN ADD  ***  05/01/01  *********************************
      *
      *  SCSSSF # MUST BE BLANK OR A POSSITIVE INTEGER
      *
     C     SCSSSF        IFLT      0
     C                   SETON                                        9080
     C                   ENDIF
     C***  END   ADD  ***  05/01/01  *********************************
      *
      *  "DBA name only" must be 'Y', or Blank.
      *
     C     SCSDBA        IFNE      *BLANK
     C     SCSDBA        ANDNE     'Y'
     C                   SETON                                        9072
     C                   ENDIF                                                  SCSDBA
      *
      *  If "DBA contains" NE *BLANK, then "DBA name only" must = 'Y'
      *
     C     SCSDBC        IFNE      *BLANK
     C     SCSDBA        ANDNE     'Y'
     C                   SETON                                        9079
     C                   ENDIF                                                  SCSDBC
      *
      *  Provider Types
      *
     C                   Z-ADD     1             I                 2 0          Type Index
      *
     C                   DO        6             I
      *
     C     TY(I)         IFNE      *BLANK
      *
     C     TY(I)         IFLT      '1'
     C     TY(I)         ORGT      '6'
      *
      *  Provider Type must be '1', '2', '3', '4', '5', or '6'.
      *
     C     I             ADD       72            N                 2 0          Indicator Index
     C                   MOVE      *ON           *IN(N)
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  Specialty code must be valid if entered
      *
     C     SCSSPC        IFNE      *BLANKS
     C                   MOVEL(P)  SCSSPC        KKKEY
     C     SPKY          CHAIN     PSP01                              48
     C   48              MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSSPC
      *
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      **********************************************************************
      *  INZSST
      **********************************************************************
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
      **********************************************************************
      *  KEYINZ - Initialize Both File Key Fields to Low Values
      **********************************************************************
     C     KEYINZ        BEGSR
      *
     C                   MOVE      *LOVAL        XXNAME
     C                   MOVE      *LOVAL        XXFSN
     C                   MOVE      *LOVAL        XXADR1
     C                   Z-ADD     *ZERO         XXFEIN
     C                   Z-ADD     *ZERO         XXMOD
      *
     C                   ENDSR                                                  KEYINZ
      **********************************************************************
      *  KEYSET - Assign File Key for position to
      **********************************************************************
     C     KEYSET        BEGSR
      *
     C   21              MOVEL(P)  SCPST1        XXNAME
     C   21              MOVEL(P)  SCPSTB        XXFSN
      *
     C***** Beg Add ****** 03/14/13 *************************************
      /Free
       If SCPST2 <> *Blanks;
          For x = 1 to %len(SCPST2);
             occupationPos = 0;
             occupationPos = %Scan(%subst(SCPST2:x:1):conocd);
             If occupationPos = 0;
                SCPST2 = %XLate(%subst(SCPST2:x:1):'0':SCPST2);
             EndIf;
          Endfor;
       EndIf;
      /End-Free
     C***** Beg Add ****** 03/14/13 *************************************
     C   22              MOVE      SCPST2        XXFEIN
     C   22              MOVE      SCPSTC        XXMOD
      *
     C   23              MOVEL(P)  SCPST3        XXADR1
     C   23              Z-ADD     *ZERO         XXFEIN
     C   23              Z-ADD     *ZERO         XXMOD
      *
     C                   ENDSR                                                  KEYSET
      **********************************************************************
      *  KEYSFL - Assign Sub File Fields to File Key Fields
      **********************************************************************
     C     KEYSFL        BEGSR
      *
     C                   MOVEL(P)  SCNAME        XXNAME
     C                   MOVEL(P)  SCADR         XXADR1
     C                   MOVEL(P)  SCFSN         XXFSN
      *
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
      *
     C                   ENDSR
      *
      *
      *
      **********************************************************************
      *  KJDO - Process F10 Searches
      **********************************************************************
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCSSTA'
      *
      * SCR03 Subset - Physical State search
      *
     C     HFLD          WHENEQ    'SCSST'
      *
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        45
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PST02                              99
     C                   MOVEL(P)  STKEY         SCSST
     C                   SETON                                        46
     C                   ENDIF
      *
      * SCR03 Subset - Specialty code search
      *
     C     HFLD          WHENEQ    'SCSSPC'
     C                   CALL      'W2111R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN47
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ##KEY         SCSSPC
     C                   ENDIF                                                  ##KEY
      *
      *
     C                   ENDSL
      *
     C                   MOVE      '1'           *IN90
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      **********************************************************************
      *  MVDBSC - Move File Fields to Screen Fields
      **********************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVE      PDTYPE        SCTYPE
     C                   MOVEL(P)  PDSPCD        SCSPCD
     C                   MOVEL(P)  PDSTAT        SCSTAT
     C***  BEGIN ADD  ***  10/13/99  *********************************
     C                   Z-ADD     PD109M        SC109M
     C***  END   ADD  ***  10/13/99  *********************************
     C                   MOVEL(P)  PDADR1        SCADR1
     C                   MOVEL(P)  PDADR1        SCADR
     C                   MOVEL(P)  PDADR2        SCADR2
     C                   MOVEL(P)  PDNAME        SCNAME
     C                   MOVEL(P)  PDADR1        SCADR
     C                   MOVEL(P)  PDFSN         SCFSN
     C                   Z-ADD     PDFEIN        SCFEIN
     C                   Z-ADD     PDMOD         SCMOD
      *
      *  Build City, State, and Zip into 1 field
      *
     C                   MOVE      *BLANKS       SCCSZ
     C                   MOVEL     PDZIP         XXZIP1            5            MAKE ALPHA ZIP
     C                   MOVE      PDZIP         XXZIP2            4            ADD 11/30/00IP
      *
     C     PDCTY         IFNE      *BLANKS
     C     SCCSZ         CAT       PDCTY:0       SCCSZ                          CITY
     C                   ENDIF
     C*
     C     PDST          IFNE      *BLANKS
     C     SCCSZ         CAT       ',':0         SCCSZ
     C     SCCSZ         CAT       PDST:1        SCCSZ                          STATE
     C                   ENDIF
      *
     C     XXZIP1        IFNE      *ZERO
     C     SCCSZ         CAT       XXZIP1:2      SCCSZ                          ZIP CODE
     C                   END
     C*** BEG ADD *** 11/30/00 ****************************
     C     XXZIP2        IFNE      *ZERO
     C     SCCSZ         CAT       '-':0         SCCSZ
     C     SCCSZ         CAT       XXZIP2:0      SCCSZ                          ZIP 2
     C                   END
     C*** END ADD *** 11/30/00 ****************************
      *
      *  Format FEIN and Mod
      *
     C                   MOVE      *BLANKS       FEINMD
     C                   MOVEL     PDFEIN        FEINMD
     C                   MOVE      PDMOD         FEINMD
      *
      *  Format Name
      *
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        XXNM             37
      *
     C                   ELSE
      *
     C                   MOVEL(P)  PDNAME        QQLAST           56
     C                   MOVEL(P)  PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNM
     C                   ENDIF
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  XXNM          SCDATA
     C                   MOVE      FEINMD        SCDATA
      *
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  FEINMD        SCDATA
     C     SCDATA        CAT       XXNM:2        SCDATA
      *
     C     VIEW#         WHENEQ    3
     C                   MOVEL(P)  PDADR1        XXTMP            37
     C                   MOVEL(P)  XXTMP         SCDATA
     C                   MOVE      FEINMD        SCDATA
     C                   MOVEL(P)  PDADR2        SCADR1
     C                   MOVEL(P)  *BLANKS       SCADR2
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      **********************************************************************
      *  OPT8 - Process Option '8' (WCMC Info program call)
      **********************************************************************
     C     OPT8          BEGSR
      *
     C     SCDBRR        CHAIN     PFRCDF                             99
      *
     C*** BEGIN DEL *** 05/24/99 **************************************
     C********** *IN09     IFEQ *ON
     C**********           MOVE 'D'       ##MODE
     C**********           ELSE
     C**********           MOVE 'C'       ##MODE
     C**********           ENDIF
     C*** END   DEL *** 05/24/99 **************************************
      *
     C                   EXSR      MCINTR
      *
     C                   ENDSR                                                  OPT8
      *
      *
      *
      **********************************************************************
      *  OPT20 - Process Option '20' (Activate / Inactivate)
      **********************************************************************
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR                                                  OPT20
     C***  BEGIN ADD  ***  12/21/05  *********************************
      **********************************************************************
      *  OPT11 - Process Option '11' (Field modification log records)
      **********************************************************************
     C     OPT11         BEGSR
      *
     C                   CALL      'W0031R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    SCKEY@           40
     C                   PARM      1             SCVEW@            1 0
     C                   PARM      SCFEIN        SCFEI@            9 0
     C                   PARM      SCMOD         SCMOD@            3 0
      *
     C                   ENDSR                                                  OPT11
     C***  END   ADD  ***  12/21/05  *********************************
     C*** BEGIN ADD *** 08/09/07 *************************************
     C*===============================================================
     C* OPT70 - SUBROUTINE FOR OPTION 70 (EFT ACCOUNT INFO)
     C*===============================================================
     C     OPT70         BEGSR
     C*
     C                   MOVE      SCFEIN        XXFEN1
     C                   MOVE      SCMOD         XXMOD1
     C*
     C                   CALL      'W0028R'                             99
     C                   PARM      @@CALL        ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
      *** BEG CHG ***** 06/29/09 *************************************
     C***********          PARM           #1RRN   90
     C                   PARM      SCDBRR        #1RRN             9 0
      *** END CHG ***** 06/29/09 *************************************
     C                   PARM      *BLANK        ##CLM            13
     C                   PARM                    XXFN
     C                   PARM      *ZERO         ##SEQ#            5 0
      *** BEG ADD ***** 09/20/07 **************************************
     C                   PARM      'P'           #1TYPE            1
      *** END ADD ***** 09/20/07 **************************************
     C*
     C                   ENDSR                                                  OPT70
      ***** END ADD ***** 08/09/07 ************************************
     C***** Beg Add *** 10/27/14 *************************************
     C*===============================================================
     C* OPT71 - SUBROUTINE FOR OPTION 71 (Transfer FEIN/Mod)
     C*===============================================================
     C     OPT71         BEGSR
     C*
      /free

        // populate the Transfer-From Mod and blank out the Transfer-To Mod
         scFromMod = SCMOD;
         scToMod = *zeros;
         scDelMod = *blanks;
         erToModVal = *off;
         erToModReq = *off;
         BtnCancel = *off;
         BtnSubmit = *off;
         BtnOK = *off;
         scMsg = *blanks;
         scMsgInd = *off;
         dsbMsgInd = *on;
         DoW *inlr = *off;
           Exfmt rcdTrnMod;

           If BtnCancel = *on;
             BtnCancel = *off;
             leave;
           EndIf;

           If scMsgInd = *on;
             scMsgInd = *off;
             dsbMsgInd = *on;
             BtnOK = *off;
             leave;
           EndIf;

           If BtnSubmit = *on;
             BtnSubmit = *off;

            // validate the screen values
             isValTrf = *on;

            // verify To-Mod is entered
             // To-Mod is numeric so will contain a default value

             // verify To-Mod is valid
             if isValTrf = *on;
               erToModVal = *off;
               // verify if From Mod and To Mod are not same
               if scToMod = scFromMod;
                 erToModVal = *on;
                 isValTrf = *off;
               endif;
               // verify To Mod entered is valid for this Fein
               setll (scFein:scToMod) wmpdl;
               if not %equal(wmpdl);
                 erToModVal = *on;
                 isValTrf = *off;
               endif;
             endif;

             // verify the Mod can be deleted
             if isValTrf = *on;
               erDelMod = *off;
               if scDelMod = 'Y';
               // check all records except the one being processed,
               // if the From Mod is used as 1099 mailed to mod on any other

                 setll scfein dbrcdf2;
                 reade scfein dbrcdf2;
                 dow not %eof(wmpdl);
                   if scFromMod = pd109m;     // listed as 1099 mod
                     erDelMod = *on;
                     isValTrf = *off;
                     leave;
                   endif;
                   reade scfein dbrcdf2;
                 enddo;

                 // read back the original record being processed
                 chain scdbrr pfrcdf;

               endif;
             endif;

             If isValTrf = *on;

               // trasnfer Mods

               // get last key number from waptp
               setll *end waptp;
               readp(n) ppt01;
               if %eof(waptp);         //file empty
                 ptkey = 1;
               else;
                 ptkey = ptkey + 1;
               endif;

               // write to waptp an entry repreenting this transfer
               ptfrmfein = pdfein;
               ptfrmmod = scfrommod;
               pttofein = pdfein;
               pttomod = sctomod;
               ptstat = 'A';
               ptpgm = q1pgm;
               ptuser = q1user;
               ptdt = %dec(%char(%date():*cymd0):7:0);
               pttime = %dec(%time():*iso);
               ptipgm = q1pgm;
               ptiusr = q1user;
               ptidt = %dec(%char(%date():*cymd0):7:0);
               ptitim = %dec(%time():*iso);

               Write ppt01;

               // call program to submit transfer job
               if scdelmod = 'Y';
                 inDelMod = 'Y';
               else;
                 inDelMod = 'N';
               endif;
               // deduce transfer job run time
               monitor;
                In JobRunTmDta;
                JobRunTm = %char(%time(JobRunTmHMS:*hms):*usa);
                inRunTime = jobruntmdta;
               on-error;
                JobRunTm = '03:00 AM';
                inRunTime = '03:00';
               endmon;
               // determine job run date
               if %time(inRunTime+':00':*hms) > %time();
                 inRunDate = %char(%date():*mdy0);
               else;
                 inRunDate = %char(%date()+ %days(1):*mdy0);
               endif;

               inPtpRrn = %editc(ptprrn:'X');
               outRtnCode = *blanks;

               callp W7090C (inDelMod:inRunDate:inRunTime:inPtpRrn:outRtncode);

               // show message to user when the job will submit
               scMsg = 'This job will run at ' + %trim(JobRunTm) + ' .';
               scMsg = %trim(scMsg) + 'Please look for the results accordingly';
               scMsgInd = *on;
               dsbMsgInd = *off;
             Endif;
           EndIf;
         EndDo;
      /end-free
     C*
     C                   ENDSR                                                  OPT70
     C***** End Add *** 10/27/14 *************************************
      **********************************************************************
      *  OTHFKS - Process Other Function Keys
      **********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   EXSR      ENTRDO
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      **********************************************************************
      *  OTHOPT - Process Other Options.
      **********************************************************************
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '1'           OPT1
     C***  BEGIN ADD  ***  12/21/05  *********************************
     C     SCSEL         CASEQ     '11'          OPT11
     C***  END   ADD  ***  12/21/05  *********************************
     C*** BEGIN ADD *** 08/09/07 ***********************************
     C     SCSEL         CASEQ     '70'          OPT70
     C*** END ADD **** 08/09/07 ************************************
     C***** Beg Add *** 10/27/14 ***********************************
     C     SCSEL         CASEQ     '71'          OPT71
     C***** End Add *** 10/27/14 ***********************************
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      **********************************************************************
      *  PGMEND
      **********************************************************************
     C     PGMEND        BEGSR
      *
     C*** BEGIN ADD ****  05/19/98  *********************************
     C     Q1PRMS        IFGT      1
     C***  END   ADD  ***  05/19/98  *********************************
     C*
     C     @@CALL        IFEQ      '2'
     C     QQRTCD        ANDNE     '*'
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
      *
     C                   ELSE
      *
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
      *
     C                   ENDIF
      *
     C***  BEGIN ADD  ***  05/19/98  *********************************
     C                   ENDIF                                                  Q1PRMS>1
     C***  END   ADD  ***  05/19/98  *********************************
     C*
     C                   ENDSR                                                  PGMEND
      **********************************************************************
      *  PGMSTR - Assign Initial field values
      **********************************************************************
     C     PGMSTR        BEGSR
      *
      *  Initialize Default Indicator values
      *
     C                   MOVE      *OFF          *IN09                          NOT INQUIRY
     C                   MOVE      *OFF          *IN19                          NOT CALL FROM PGM
     C                   MOVE      *ON           *IN12                          SFLDROP
      *
      ***** BEG ADD ***** 06/19/06 ***************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     CLTID         IFEQ      '139'
     C                   MOVE      '1'           *IN69
     C                   ELSE
     C                   MOVE      '0'           *IN69
     C                   ENDIF
      ***** END ADD ***** 06/19/06 ***************************************
      *
      *  Determine what function program is performing.
      *
     C                   SELECT
      *
     C     @@CALL        WHENEQ    '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
      *
      ***** BEG ADD ***** 06/19/06 ***************************************
     C     CLTID         IFEQ      '133'
     C                   MOVE      'A'           SCSSTA
     C                   ENDIF                                                  CLTID = 133
      ***** END ADD ***** 06/19/06 ***************************************
      *
     C     @@CALL        WHENEQ    '2'
     C                   MOVE      *ON           SELMOD                         SELECT MODE
     C                   MOVE      *ON           *IN19                          CALL FROM PGM
     C                   MOVE      'A'           SCSSTA                         SUBSET ACTIVE STATUS
      *
     C                   OTHER
      *                    "Work With" Function
      *
     C                   ENDSL
      *
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*** BEGIN ADD *** 08/09/07  *************************************
     C* Disable option 70, if EFT Install Flag is not 'Y'
     C*
     C                   MOVEL(P)  'WC'          XSKEY
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL35        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '70'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD ****** 08/09/07 *************************************
       //*** Beg Add *** 05/22/19 *************************************
      /Free
                         isWcMc = isModEnabled('Managed Care');
                         isWcInt = isModEnabled('Work Comp Interface');
      /End-free
       //*** End Add *** 05/22/19 *************************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      **********************************************************************
      *  READ - Access Payee Demographic file based on *IN20
      **********************************************************************
     C     READ          BEGSR
      *
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
      *
     C                   ENDSR                                                  READ
      *
      *
      *
      **********************************************************************
      *  READP - Access Payee Demographic file based on *IN20
      **********************************************************************
     C     READP         BEGSR
      *
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
      *
     C                   ENDSR                                                  READP
      *
      *
      *
      **********************************************************************
      *  SETGT - Position Payee Demographics for Read Access
      **********************************************************************
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
      *
     C                   ENDSR                                                  SETGT
      *
      *
      *
      **********************************************************************
      *  SETLL - Position Payee Demographics for Read Access
      **********************************************************************
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
      *
     C                   ENDSR                                                  SETLL
      *
      *
      *
      **********************************************************************
      *  SF1UPD - Assign Deleted message
      **********************************************************************
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      **********************************************************************
      *  SLCTRD - Used to move fields for selected records
      **********************************************************************
     C     SLCTRD        BEGSR
      *
      *  Return to Calling Program PARMS
      *
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   MOVE      '*'           QQRTCD
     C***** BEGIN ADD ***** 07/13/09 ********************************
     C     Q1PRMS        IFGT      4
     C                   MOVEL     SCNAME        QQNAME
     C                   ENDIF
     C***** END ADD ******* 07/13/09 ********************************
      *
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
      *
     C     STRING        BEGSR
      *
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
      *
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
      *
     C                   ENDSR                                                  STRING
      *
      *
      *
      **********************************************************************
      *  SUBSET - Process record against Subset Criteria
      **********************************************************************
     C     SUBSET        BEGSR
      *
      *  Check Payee Status against Search Status
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSSTA        ANDNE     PDSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Payee name
      *
     C     SCSNAM        IFNE      *BLANKS
     C     LO:UP         XLATE     PDNAME        U1PDSC
     C     LO:UP         XLATE     SCSNAM        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF
      *
      *  Check DBA name
      *
     C     SCSDBC        IFNE      *BLANKS
      *
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      *OFF          MEETSS
      *
     C                   ELSE
      *
     C     LO:UP         XLATE     PDADR1        U1PDSC
     C     LO:UP         XLATE     SCSDBC        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF                                                  PDDBA1
      *
     C                   ENDIF                                                  SCSDBC
      *
      *  Check address
      *
     C     SCSADR        IFNE      *BLANKS
      *
     C     PDDBA1        IFEQ      'Y'
      *
      *    Check address 2
      *
     C     LO:UP         XLATE     PDADR2        U1PDSC
     C     LO:UP         XLATE     SCSADR        U2PDSC
     C                   EXSR      STRING
      *
     C                   ELSE
      *
      *    Check address 1
      *
     C     LO:UP         XLATE     PDADR1        U1PDSC
     C     LO:UP         XLATE     SCSADR        U2PDSC
     C                   EXSR      STRING
      *
      *    Check address 2
      *
     C     MEETSS        IFEQ      *OFF
     C                   MOVE      *ON           MEETSS
     C     LO:UP         XLATE     PDADR2        U1PDSC
     C     LO:UP         XLATE     SCSADR        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF                                                  MEETSS
      *
     C                   ENDIF                                                  PDDBA1
      *
     C                   ENDIF                                                  SCSADR
      *
      *  Check City
      *
     C     SCSCTY        IFNE      *BLANKS
     C     LO:UP         XLATE     PDCTY         U1PDSC
     C     LO:UP         XLATE     SCSCTY        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF
      *
      *  Check State
      *
     C     SCSST         IFNE      *BLANKS
     C     SCSST         ANDNE     PDST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Zipcode contains subset number
      *
     C     SCSZIP        IFNE      *ZERO
     C                   MOVEL     PDZIP         U1PDSC
     C                   MOVEL(P)  SCSZIP        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF                                                  SCSZIP <> BLANK
      *
      *  Check Telephone contains subset number
      *
     C     SCSPHN        IFNE      *ZERO
     C                   MOVEL     PDPHN#        U1PDSC
     C                   MOVEL     SCSPHN        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF                                                  SCSPHN <> BLANK
     C***  BEGIN ADD  ***  05/01/01  *********************************
      *
      *  Check FEIN  contains subset number
      *
     C     SCSSSF        IFNE      *ZERO
     C     SCSSSF        ANDNE     PDFEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C***  BEGIN ADD  ***  05/01/01  *********************************
      *
      *
      *  Check DBA Flag against Search DBA Flag
      *
     C     SCSDBA        IFEQ      'Y'
     C     PDDBA1        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Payee Type against Search Types
      *
     C                   MOVEA     TY            XXTY              6
      *
     C     XXTY          IFNE      *BLANK
     C                   Z-ADD     1             I
     C     PDTYPE        LOOKUP    TY(I)                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Check specialty code
      *
     C     SCSSPC        IFNE      *BLANKS
     C     SCSSPC        ANDNE     PDSPCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSR                                                  SUBSET
      *
      *
      *
     C*** END ADD ****** 08/09/07 *************************************
      ****************************************************************
      *  MCINTR - WCMC Interface                                     *
      ****************************************************************
     C     MCINTR        BEGSR
      *
      *  WCMS / WCMC Installed flags determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
      *  Maintain WCMC Provider and Provider Auxiliary files
      *  prior to executing WCMC Provider Maint.
      *
       //*** Beg Chg *** 05/22/19 *************************************
     C*****QQFBIF        IFEQ      'Y'
     C*****QQFL01        ANDEQ     'Y'
      /Free
           If isWcMc and isWcInt;
      /End-Free
       //*** End Chg *** 05/22/19 *************************************
     C*** BEGIN CHG *** 05/24/99 **************************************
     C**********           CALL 'FA005R'  MCPL
     C**********           CALL 'F0032R01'ACDPL2       99
     C                   MOVE      SCFEIN        ##FEIN
     C                   MOVE      SCMOD         ##MOD
      *
     C                   CALL      'F0032R'      ACDPL2                 99
     C*** END   CHG *** 05/24/99 **************************************
     C                   ENDIF
      *
     C                   ENDSR                                                  MCINTR
      ***** beg 10/27/14 **********************************************
      ** add option 71
      ***** end 10/27/14 **********************************************
** Headers - HD
Opt  Name                                  FEIN#      Mod Tp  Spec  Mod Sts
Opt  FEIN#      Mod  Name                                 Tp  Spec  Mod Sts
Opt  DBA Name / address                    FEIN#      Mod Tp  Spec  Mod Sts
**  Description-------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Print                  NNN
1088=Med info only          YYN
11111=View mod history      YYY
12020=Change status         YNN
17070=EFT Account Info      YYN
17171=Transfer FEIN/Mod     YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
210F10=Search               NNN
211F11=Change view          YYY
212F12=Cancel               NNY
213F13=Chg usr parm         NNN
215F15=Fold/truncate        YYY
217F17=Subset               YYY
3
