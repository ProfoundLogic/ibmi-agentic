     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2423R - Bank Information Maint - Work With Program
     H*
     H*  Description:
     H*
     H*    This program displays Bank Info records.  The user has the
     H*    ability to execute the following functions:
     H*
     H*      1) Re-position at a specific record.
     H*      2) F6 to initiate the entry of a BI record.
     H*      3) INQUIRY Mode:
     H*           Option '5' to Display
     H*         SELECT Mode:
     H*           Option '1' to Select
     H*           Option '5' to Display
     H*         UPDATE Mode:
     H*           Option '2' to Update
     H*           Option '5' to Display
     H*           Option '20' to Activate/Inactivate record
     H*      4) F17 to enter Subset fields to limit Display records
     H*
     H*  Called From:
     H*    Menu
     H*    Select programs
     H*
     H*
     H*  Calls To:
     H*    QCLSCAN
     H*    SRCENTER
     H*    SRNAME
     H*
     H*  Created:  02/13/97  C:30  Project:  87WBL
     H*
     H*****************************************************************
     H*  Revisions:
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*****************************************************************
     H*  Indicators:
     H*
     H*   09 - Inquiry
     H*   40 - LOKUP result indicator
     H*   99 - Subfile access
     H*
     H*****************************************************************
      * Sub File Display Screen
     FW2423F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *__________________________________________________________________
      * Bank Information
     FWMBIL3    IF   E           K DISK
     F                                     RENAME(PBI01:DBRCDF)
     F                                     INFDS(DBFDS)
      *__________________________________________________________________
      * Bank Information
     FWMBIL5    IF   E           K DISK
     F                                     RENAME(PBI01:DBRCDF2)
     F                                     INFDS(DBFDS2)
      *__________________________________________________________________
      * Bank Information
     FWMBIP     IF   E             DISK
     F                                     RENAME(PBI01:PFRCDF)
      *__________________________________________________________________
      * System Table file
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      *__________________________________________________________________
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(18) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(18) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D***PY SCOPYWW01I
      *
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D OPTNM           C                   CONST('Bank Information')
     D OPTNM2          C                   CONST('Select Bank Account')
     D DELETD          C                   CONST('*Deleted')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D ACDPGM          C                   CONST('W2423R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D VIEWMX          C                   CONST(2)
     D INFOMX          C                   CONST(2)
      *

     C     *LIKE         DEFINE    BICO#         XXCO#
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIFND         SVFND
     C     *LIKE         DEFINE    BIACCT        XXACCT
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C     *LIKE         DEFINE    BIEFDT        SSEFDT
     C     *LIKE         DEFINE    BIEFDT        WWEFDT
     C     *LIKE         DEFINE    BICO#         @@CO#
     C     *LIKE         DEFINE    BIACCT        @@ACCT
     C     *LIKE         DEFINE    BIAPPL        @@APPL
     C     *LIKE         DEFINE    BIAPPL        QQAPPL
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    BICKTP        QQCKTP
     C     *LIKE         DEFINE    BICKTP        @@CKTP
     C     *LIKE         DEFINE    BICKTP        XXCKTP
     C     *LIKE         DEFINE    BIACCT        QQACCT
     C     *LIKE         DEFINE    SCPST1        SVPST1
     C     *LIKE         DEFINE    BIAI01        U1PDSC
     C     *LIKE         DEFINE    BIAI01        U2PDSC
     C     *LIKE         DEFINE    STKEY         XXKEY
      *
      *  W2423R02
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##GRP             3 0
      *
      *  8 Digit Date Check
      *
     C*          DATCHK    PLIST
     C*                    PARM           SRDTI   80
     C*                    PARM *BLANKS   SRFFMT  8
     C*                    PARM *BLANKS   SRTFMT  8
     C*                    PARM           SRDTO  10
     C*                    PARM           SRERR   1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *  Company Name
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Keylist for Bank Information file
      *
     C     KEY1          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXACCT
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXFND
      *
     C     KEY2          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXCKTP
      *
      *  SYSTEM TABLE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQACCT
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQAPPL
     C                   PARM                    QQCKTP
      *
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
     C     SCSACT        ORNE      *BLANK
     C     SCSEDT        ORNE      *ZERO
     C     SCSCKT        ORNE      *BLANK
     C     SCSFND        ORNE      *ZERO
     C     SCSST         ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR
      *
      **********************************************************************
      *  ERRCK1 - Edit OPT screen field
      **********************************************************************
     C     ERRCK1        BEGSR
      *
      *  VALIDATE 'POSITION TO' EFFECTIVE DATE
      *
      *                    *** IF POSITION TO EFF DT ON WW SCREEN <> 0
     C     SCPSTB        IFNE      *ZERO
      *
     C                   Z-ADD     SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9073
     C                   ELSE
     C                   Z-ADD     SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVEFDT                         WRK WITH EFDT
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     SCPSTB        SVEFDT
     C                   ENDIF
      *
      *
     C                   ENDSR
      *
      **********************************************************************
      *  ERRCK2 - Edit SCR03 Fields
      **********************************************************************
     C     ERRCK2        BEGSR
      *
      *  Edit Search Status
      *  Search Status value must be 'A', 'I', or Blank
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSSTA        IFNE      'A'
     C     SCSSTA        ANDNE     'I'
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
      *
      *  Edit Search Effective Date.
      *
     C     SCSEDT        IFNE      *ZERO
      *
     C                   Z-ADD     SCSEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      ******               MOVELSRDTO     XXEFDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9072
     C                   ELSE
     C                   Z-ADD     SCSEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSEFDT                         SUBSET EFDT
     C                   ENDIF
     C                   ENDIF
      *
      *  Edit State Code entry
      *
     C     SCSST         IFNE      *BLANK
      *
     C                   MOVEL     SCSST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9073
     C                   ENDIF
      *
      *  Fund Restricted in Select mode
      *
     C     SVFND         IFNE      *ZERO
     C     SELMOD        ANDEQ     *ON
     C     SVFND         ANDNE     SCSFND
     C                   MOVE      *ZERO         SCSFND
     C                   MOVE      SVFND         SCSFND
     C                   ENDIF
      *
     C                   ENDSR
      *
      **********************************************************************
      *  INZSST
      **********************************************************************
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SCSSTA
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *
      **********************************************************************
      *  KEYINZ - Initialize Both File Key Fields to Low Values
      **********************************************************************
     C     KEYINZ        BEGSR
      *
     C                   MOVE      ##CO#         XXCO#
     C                   MOVE      *LOVAL        XXACCT
     C                   MOVE      *HIVAL        XXEFDT                         DESCEND ORDER
     C                   MOVE      *LOVAL        XXFND
     C                   MOVE      *LOVAL        XXCKTP
     C                   MOVE      *LOVAL        XXAPPL
      *
     C                   ENDSR
      *
      **********************************************************************
      *  KEYSET - Assign File Key Field
      **********************************************************************
     C     KEYSET        BEGSR
      *
     C                   MOVEL(P)  SCPST1        XXACCT
      *
      *                    *** IF POSITION TO EFF DT ON WW SCREEN <> 0
     C     SCPSTB        IFNE      *ZERO
      *
     C                   Z-ADD     SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C*******              SETON                     9073
     C                   MOVE      *HIVAL        XXEFDT                         DESCEND ORD
     C                   ELSE
     C                   MOVEL     SRDTO         SCPSTB
     C                   Z-ADD     SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT                         WRK WITH EFDT
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *HIVAL        XXEFDT                         DESCEND ORD
     C                   ENDIF
      *
     C                   MOVE      *LOVAL        XXFND
     C                   MOVE      *LOVAL        XXAPPL
      *
     C                   MOVEL     SCPST2        XXCKTP
      *
     C                   ENDSR
      *
      **********************************************************************
      *  KEYSFL - Assign Sub File Fields to File Key Fields
      **********************************************************************
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SVACCT        XXACCT
     C                   Z-ADD     SVEFDT        XXEFDT
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SVCKTP        XXCKTP
     C                   MOVE      SVCO#         XXCO#
     C                   MOVE      SCAPPL        XXAPPL
      *
     C                   ENDSR
      *
      **********************************************************************
      *  MVDBSC - Move File Fields to Screen Fields
      **********************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVE      BICO#         SVCO#
     C                   MOVE      BIACCT        SVACCT
     C                   MOVE      BICKTP        SVCKTP
     C                   Z-ADD     BIEFDT        SVEFDT
     C                   Z-ADD     BIFND         SCFND
     C                   MOVEL     BIACCT        SCACCT
     C                   MOVEL     BISTAT        SCSTAT
     C                   MOVEL     BIAPPL        SCAPPL
     C                   MOVEL     BIST          SCST
     C                   MOVE      *BLANK        SCDATA
      *
     C                   Z-ADD     BIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   SELECT
     C     INFO#         WHENEQ    1
     C                   MOVEL     BIACCT        SCDATA
     C                   MOVE      BICKTP        SCDATA
     C     INFO#         WHENEQ    2
     C                   MOVEL     BIAI01        SCDATA
     C                   ENDSL
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OTHFKS - Process Other Function Keys
      **********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   EXSR      ENTRDO
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OTHOPT - Process Other Options.
      **********************************************************************
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '60'          OPT60
     C                   ENDCS
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OPT20 - Process Option '20' (Activate / Inactivate)
      **********************************************************************
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OPT60 - Process Option '60'
      **********************************************************************
     C     OPT60         BEGSR
      *
      *  HERE
     C     *LIKE         DEFINE    SCPRDS        @@PRDS
     C                   CALL      'W6006R'
     C                   PARM      SCPRDS        @@PRDS
     C                   PARM      ' '           QQCALL            1
     C                   PARM      *ZERO         @@PRDT            6 0
     C                   PARM      *BLANK        @@ACCT
     C                   PARM      *BLANK        @@RTC             1
     C                   PARM      *BLANK        @@CKTP
     C*
     C     @@RTC         IFEQ      'A'
     C                   MOVE      '1'           *INKC
     C                   ENDIF                                                  RTC A
      *
      *
      *
     C                   ENDSR
      *
      **********************************************************************
      *  PGMEND
      **********************************************************************
     C     PGMEND        BEGSR
      *
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
      *
     C                   ENDSR
      *
      **********************************************************************
      *  PGMSTR - Assign Initial field values
      **********************************************************************
     C     PGMSTR        BEGSR
      *
      *  Initialize Default values
      *
     C                   MOVE      *OFF          *IN09                          NOT INQUIRY
     C***********          MOVEA'00'      *IN,21
     C                   Z-ADD     11            SF1PAG            5 0
      *
     C                   Z-ADD     *ZERO         SCSFND
     C                   MOVE      QQFND         SCSFND                         SUBSET ACTIVE STATUS
     C                   MOVE      QQFND         SVFND
      *
     C                   Z-ADD     *ZERO         ##GRP
     C                   MOVE      *OFF          *IN55
     C     SCSFND        IFGT      *ZERO
     C                   MOVE      *ON           *IN55                          Protect SCSFND
     C                   Z-ADD     SCSFND        ##GRP                          Pass Grp to Maint pg
     C                   ENDIF
      *
     C                   MOVEL     QQACCT        SCSACT                         SUBSET ACTIVE STATUS
     C                   MOVEL     QQAPPL        SCSAPL                         SUBSET ACTIVE STATUS
     C                   MOVEL     QQCKTP        SCSCKT                         SUBSET ACTIVE STATUS
      *
     C                   Z-ADD     *ZERO         SRDTI
     C                   MOVEL     QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSEDT
      *
      *  Determine what function program is performing.
      *
     C                   SELECT
     C     @@CALL        WHENEQ    '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
     C     @@CALL        WHENEQ    '2'
     C                   MOVE      *ON           SELMOD                         SELECT MODE
     C                   MOVE      *ON           *IN19                          CALL FROM PGM
     C                   MOVE      'A'           SCSSTA                         SUBSET ACTIVE STATUS
     C                   OTHER
      *                    "Work With" Function
     C                   ENDSL
      *
     C                   ENDSR
      *
      **********************************************************************
      *  READ - Access Bank Information  file
      **********************************************************************
     C     READ          BEGSR
      *
     C   21XXCO#         READE     DBRCDF                                 95
      *
     C   22XXCO#         READE     DBRCDF2                                95
      *
     C                   ENDSR
      *
      **********************************************************************
      *  READP - Access Bank Information  file
      **********************************************************************
     C     READP         BEGSR
      *
     C   21XXCO#         READPE    DBRCDF                                 95
      *
     C   22XXCO#         READPE    DBRCDF2                                95
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SETGT - Position Bank Information   for Read Access
      **********************************************************************
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
      *
     C   22KEY2          SETGT     DBRCDF2                            96
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SETLL - Position Bank Information   for Read Access
      **********************************************************************
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
      *
     C   22KEY2          SETLL     DBRCDF2                            96
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SF1UPD - Assign Deleted message
      **********************************************************************
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCAI01
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SLCTRD - Used to move fields for selected records
      **********************************************************************
     C     SLCTRD        BEGSR
      *
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVEL     SVACCT        QQACCT
     C                   MOVEL     SCAPPL        QQAPPL
     C                   MOVEL     SVCKTP        QQCKTP
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQEFDT
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SUBSET - Process record against Subset Criteria
      **********************************************************************
     C     SUBSET        BEGSR
      *
      * Check Application type against Search Application Type
      *
     C     SCSAPL        IFNE      *BLANK
     C     SCSAPL        ANDNE     BIAPPL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Check Bank Info Status against Search Status
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSSTA        ANDNE     BISTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Check Name against Search String
      *
B01  C     SCSNAM        IFNE      *BLANKS
     C     LO:UP         XLATE     BIAI01        U1PDSC
     C     LO:UP         XLATE     SCSNAM        U2PDSC
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
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF
E01  C                   ENDIF
      *
      * Check Bank Account against Search Bank Account
      *
B01  C     SCSACT        IFNE      *BLANK
     C     BIACCT        ANDNE     SCSACT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Effect Date against Search Effective Date
      *
     C     SCSEDT        IFNE      *ZERO
     C     BIEFDT        ANDNE     SSEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Check Type against Search Check Type
      *
     C     SCSCKT        IFNE      *BLANK
     C     BICKTP        ANDNE     SCSCKT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check State against Search State
      *
     C     SCSST         IFNE      *BLANK
     C     BIST          ANDNE     SCSST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Fund Number against Search Fund Number
      *  HERE
      *
     C     SCSFND        IFNE      *ZERO
     C     BIFND         ANDNE     SCSFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
      *
** Headers - HD
Eff Date  Bank Account#                 Bank Code      State Group Appl Sts
Eff Date  Bank Account#                 Bank Code      State Group Appl Sts
Eff Date  Bank Name                                    State Group Appl Sts
Eff Date  Bank Name                                    State Group Appl Sts
**  Description-------------MISN
1011=Select                 NNY
1022=Change                 NNN
1033=Copy                   NNN
1044=Delete                 NNN
1055=Display                NNN
1066=Print                  NNN
12020=Change status         NNN
16060=Claim check sched histNNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
209F09=Change info          NNY
210F10=Search               NNN
211F11=Toggle info          NNY
212F12=Cancel               NNY
213F13=Chg usr parm         NNN
217F17=Subset               NNY
3
