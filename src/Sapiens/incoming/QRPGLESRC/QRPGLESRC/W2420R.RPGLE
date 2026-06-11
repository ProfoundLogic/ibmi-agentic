     H* (c) 2012 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*
     H/TITLE W2420R - Bank Information Maint - Work With Program
     H*****************************************************************
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
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  04/17/12 C:282 PROJECT: WC2044                               *
     H*                - Added new column for EFT flag just before    *
     H*                  status column.                               *
     H*                                                               *
     H*  06/29/09 C:191 PROJECT: WC1806                               *
     H*                -REMOVE STATE FIELD (NOT USED)                 *
     H*                                                               *
     H* 04/13/01 C:61  PROJECT WC1020                                 *
     H*              - Display Bank Code on initial screen.           *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*****************************************************************
     H*
     H*  Indicators:
     H*
     H*   09 - Inquiry
     H*   40 - LOKUP result indicator
     H*   99 - Subfile access
     H*
     H*****************************************************************
      * Sub File Display Screen
     FW2420F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *__________________________________________________________________
      * Bank Information
     FWMBIL2    IF   E           K DISK
     F                                     RENAME(PBI01:DBRCDF)
     F                                     INFDS(DBFDS)
      *__________________________________________________________________
      * Bank Information
     FWMBIP     IF   E             DISK
     F                                     RENAME(PBI01:PFRCDF)
      *__________________________________________________________________
      * System Table file
      *__________________________________________________________________
      *
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(18) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(18) ALT(OF)
      *
      *** Beg Chg *** 04/17/12 **************************************
     D*****/COPY SCOPYWW01E
      *** End Chg *** 04/17/12 **************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *** Beg Chg *** 04/17/12 **************************************
     D****/COPY SCOPYWW01I
      *** End Chg *** 04/17/12 **************************************
      *
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D OPTNM           C                   CONST('Bank Information')
     D OPTNM2          C                   CONST('Select Bank Account')
     D DELETD          C                   CONST('*Deleted')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D ACDPGM          C                   CONST('W2420R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(2)
      *** Beg Chg *** 04/17/12 **************************************
     C/COPY SCOPYWW01C
      *** End Chg *** 04/17/12 **************************************
      *
     C     *LIKE         DEFINE    BICO#         XXCO#
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIACCT        XXACCT
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C     *LIKE         DEFINE    BIEFDT        XSEFDT
     C     *LIKE         DEFINE    BIACCT        ##ACCT
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    BICKTP        ##CKTP
     C     *LIKE         DEFINE    BICKTP        XXCKTP
     C     *LIKE         DEFINE    BIAI01        U1PDSC
     C     *LIKE         DEFINE    BIAI01        U2PDSC
      *
      *  W2420R02
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##GRP             3 0
      *
      *   Date Conversion parm list
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
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C                   KFLD                    XXEFDT
      *
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@ACCT           20
     C                   PARM                    @@EFDT            8
     C                   PARM                    @@APPL            2
     C                   PARM                    @@CKTP           10
      *
      *
      *** Beg Chg *** 04/17/12 **************************************
     C***/COPY SCOPYWW01C
      *** End Chg *** 04/17/12 **************************************
      *
      **********************************************************************
      *  CHKSSM
      **********************************************************************
     C     CHKSSM        BEGSR
      *
     C     SSSTAT        IFNE      *BLANK
     C     SSBKNM        ORNE      *BLANK
     C     SSACCT        ORNE      *BLANK
     C     SSEFDT        ORNE      *ZERO
     C     SSCKTP        ORNE      *BLANK
     C     SSFND         ORNE      *ZERO
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
     C     SCPS1C        IFNE      *ZERO
     C     SCPS1C        ANDNE     *BLANKS
      *
     C                   MOVE      SCPS1C        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   ENDSR
      **********************************************************************
      *  ERRCK2 - Edit SCR03 Fields
      **********************************************************************
     C     ERRCK2        BEGSR
      *
      *  Edit Search Status
      *  Search Status value must be 'A', 'I', or Blank
      *
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        IFNE      'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
      *
      *  Edit Search Effective Date.
      *
     C     SSEFDT        IFNE      *ZERO
      *
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9072
     C                   ELSE
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSEFDT                         SUBSET EFDT
     C                   ENDIF
     C                   ENDIF
      *
      *
     C                   ENDSR
      **********************************************************************
      *  INZSST
      **********************************************************************
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
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
     C                   MOVE      SSAPPL        XXAPPL
      *
     C                   ENDSR
      **********************************************************************
      *  KEYSET - Assign File Key Field
      **********************************************************************
     C     KEYSET        BEGSR
      *
      *
      *
     C                   MOVE      SCPST1        XXAPPL
     C                   MOVE      SCPS1A        XXFND
     C                   MOVE      SCPS1B        XXCKTP
     C                   MOVE      SCPS1C        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     XXEFDT        IFEQ      0
     C                   MOVE      *HIVAL        XXEFDT
     C                   ENDIF
      *
     C                   ENDSR
      **********************************************************************
      *  KEYSFL - Assign Sub File Fields to File Key Fields
      **********************************************************************
     C     KEYSFL        BEGSR
      *
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFND         XXFND
     C                   MOVEL     SCACCT        XXACCT
     C                   Z-ADD     SVEFDT        XXEFDT
     C                   MOVE      SCCKTP        XXCKTP
     C                   MOVE      SCAPPL        XXAPPL
      *
     C                   ENDSR
      **********************************************************************
      *  MVDBSC - Move File Fields to Screen Fields
      **********************************************************************
     C     MVDBSC        BEGSR
      *
     C                   MOVE      BICO#         SCCO#
     C                   MOVEL     BIAPPL        SCAPPL
     C                   Z-ADD     BIFND         SCFND
     C                   MOVE      BICKTP        SCCKTP
     C                   Z-ADD     BIEFDT        SVEFDT
     C                   MOVEL     BIACCT        SCACCT
     C                   MOVEL     BISTAT        SCSTAT
     C*** Beg Add ****** 04/17/12 ************************************
      /free
       scEFTC = BIEFTC;
      /end-free
     C*** End Add ****** 04/17/12 ************************************
     C                   MOVE      *BLANK        SCDATA
      *
     C                   Z-ADD     BIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   SELECT
      *
     C     INFO#         WHENEQ    1
     C                   MOVEL(P)  BIACCT        SCDATA
      *
     C     INFO#         WHENEQ    2
     C                   MOVEL(P)  BIAI01        SCDATA
     C                   ENDSL
      *
     C                   ENDSR
      **********************************************************************
      *  OTHFKS - Process Other Function Keys
      **********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   EXSR      ENTRDO
      *
     C                   ENDSR
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
      **********************************************************************
      *  OPT20 - Process Option '20' (Activate / Inactivate)
      **********************************************************************
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      **********************************************************************
      *  OPT60 - Process Option '60'
      **********************************************************************
     C     OPT60         BEGSR
      *
      *  HERE
     C     *LIKE         DEFINE    SCPRDS        ##PRDS
     C                   CALL      'W6006R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      ' '           ##CALL            1
     C                   PARM      *ZERO         ##PRDT            6 0
     C                   PARM      *BLANK        ##ACCT
     C                   PARM      *BLANK        ##RTC             1
     C                   PARM      *BLANK        ##CKTP
     C*
     C     ##RTC         IFEQ      'A'
     C                   MOVE      '1'           *INKC
     C                   ENDIF                                                  RTC A
      *
     C                   ENDSR
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
      **********************************************************************
      *  PGMSTR - Assign Initial field values
      **********************************************************************
     C     PGMSTR        BEGSR
      *
      *  Initialize Default values
      *
     C                   MOVE      *OFF          *IN09                          NOT INQUIRY
     C                   Z-ADD     11            SF1PAG            5 0
      *
     C                   MOVE      @@FND         SSFND                          SUBSET ACTIVE STATUS
      *
     C                   Z-ADD     *ZERO         ##GRP
     C                   MOVE      *OFF          *IN55
     C     SSFND         IFGT      *ZERO
     C                   MOVE      *ON           *IN55                          Protect SSFND
     C                   Z-ADD     SSFND         ##GRP                          Pass Grp to Maint pg
     C                   ENDIF
      *
     C     @@CKTP        IFNE      *BLANKS
     C                   MOVE      @@CKTP        SSCKTP                         SUBSET BANK CODE
     C                   ENDIF
      *
     C                   MOVEL     @@ACCT        SSACCT                         SUBSET ACTIVE STATUS
     C                   MOVEL     @@APPL        SSAPPL                         SUBSET ACTIVE STATUS
     C                   MOVEL     @@CKTP        SSCKTP                         SUBSET ACTIVE STATUS
      *
     C                   Z-ADD     *ZERO         SRDTI
     C                   MOVEL     @@EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSEFDT
      *
      *  Determine what function program is performing.
      *
     C                   SELECT
     C     @@CALL        WHENEQ    '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
     C     @@CALL        WHENEQ    '2'
     C                   MOVE      *ON           SELMOD                         SELECT MODE
     C                   MOVE      *ON           *IN19                          CALL FROM PGM
     C                   MOVE      'A'           SSSTAT                         SUBSET ACTIVE STATUS
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
     C                   ENDSR
      **********************************************************************
      *  READP - Access Bank Information  file
      **********************************************************************
     C     READP         BEGSR
      *
     C   21XXCO#         READPE    DBRCDF                                 95
      *
     C                   ENDSR
      **********************************************************************
      *  SETGT - Position Bank Information   for Read Access
      **********************************************************************
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
      *
     C                   ENDSR
      **********************************************************************
      *  SETLL - Position Bank Information   for Read Access
      **********************************************************************
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
      *
     C                   ENDSR
      **********************************************************************
      *  SF1UPD - Assign Deleted message
      **********************************************************************
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCDATA
      *
     C                   ENDSR
      **********************************************************************
      *  SLCTRD - Used to move fields for selected records
      **********************************************************************
     C     SLCTRD        BEGSR
      *
     C                   MOVE      ##CO#         @@CO#
     C                   MOVE      SCFND         @@FND
     C                   MOVEL     SCACCT        @@ACCT
     C                   MOVEL     SCAPPL        @@APPL
     C                   MOVEL     SCCKTP        @@CKTP
     C                   MOVE      SCEFDT        @@EFDT
      *
     C                   ENDSR
      **********************************************************************
      *  SUBSET - Process record against Subset Criteria
      **********************************************************************
     C     SUBSET        BEGSR
      *
      * Check Application type against Search Application Type
      *
     C     SSAPPL        IFNE      *BLANK
     C     SSAPPL        ANDNE     BIAPPL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Check Bank Info Status against Search Status
      *
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     BISTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Check Name against Search String
      *
B01  C     SSBKNM        IFNE      *BLANKS
     C     LO:UP         XLATE     BIAI01        U1PDSC
     C     LO:UP         XLATE     SSBKNM        U2PDSC
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
B01  C     SSACCT        IFNE      *BLANK
     C     BIACCT        ANDNE     SSACCT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Effect Date against Search Effective Date
      *
     C     SSEFDT        IFNE      *ZERO
     C     BIEFDT        ANDNE     XSEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Check Type against Search Check Type
      *
     C     SSCKTP        IFNE      *BLANK
     C     BICKTP        ANDNE     SSCKTP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *
      *  Check Fund Number against Search Fund Number
      *  HERE
      *
     C     SSFND         IFNE      *ZERO
     C     BIFND         ANDNE     SSFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
     C*=========================================================================
** Headers - HD
 Appl  Group  Bank Code    Eff Date    Bank Account           EFT Flag Sts
 Appl  Group  Bank Code    Eff Date    Bank Name              EFT Flag Sts
**  Description-------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Print                  NNN
12020=Change status         YNN
16060=Claim check sched histNNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change info           YYY
210F10=Search               NNN
211F11=Change view          NNN
212F12=Cancel               YYY
213F13=Chg usr parm         NNN
217F17=Subset               YNY
3
