     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*******************************************************************
     H/TITLE SA001R - RETRIEVE SYSTEM VALUES
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Access System Control file record 'SY' to obtain values for  *
     H*    SYWCIF (WCMS Installed = 'Y') and                            *
     H*    SYFBIF (WCMC BASE Installed = 'Y').                          *
     H*    SYFL05 (NON-PDS WORKERS COMP SYSTEM)                         *
     H*    SYFL06 (MULTI GROUP RESTRICTIONS)                            *
     H*    SYAL04 Pos. 8-10 - (Client Number)                           *
     H*    SYFL11 Pos. 2 - (Data Center)                                *
     H*    SYAL05 Pos. 1 - (Credit Module)                              *
     H*                                                                 *
     H*    Access Darts Control file record 'SC' to obtain values for   *
     H*    SYAL03 Pos. 1 - (Data Entry and Image Flag)                  *
     H*                                                                 *
     H*    Access System Control file record 'FB' to obtain value for   *
     H*    DRFL06 (Microsoft product support in DARTS (WORD/EXCEL))
     H*    FBAL01 Pos. 4  - (Use DARTS WCMS key).                       *
     H*    FBAL01 Pos. 9  - (MCMOD3 Installed = 'Y')
     H*    FBAL03 Pos. 2  - (Create Provider = 'Y')
     H*    FBAL04 Pos. 5  - (A/R Installed = 'Y')
     H*    FBAL04 Pos. 6  - (EDI Installed = 'Y')
     H*    FBAL04 Pos. 7  - (REHAB Module installed).                   *
     H*    FBAL04 Pos. 9  - (Fee Schedule installed).                   *
     H*    FBAL04 Pos. 10 - (Provider Bills installed).                 *
     H*    FBAL05 Pos. 3  - (Outcome/Savings installed).                *
     H*    FBAL05 Pos. 10 - (Correct Coding installed).                 *
     H*    FBFL01 (Interfaced with WCMS = 'Y').                         *
     H*    FBFL06 (NETWORK Installed = 'Y')
     H*    FBFL07 (UR Installed = 'Y')
     H*    FBST01 Pos. 1  - (ClaimCheck installed).                     *
     H*    FBST01 Pos. 2  - (No longer used).                           *
     H*    FBST02 Pos. 1  - Medicare CR Flag.                           *
     H*    FBST02 Pos. 2  - Frequency                                   *
     H*    FBST03 Pos. 1  - (No longer used).                           *
     H*    FBST03 Pos. 2  - (EOB Code/Note print flag default)          *
     H*    FBST04 Pos. 1  - (W/W Providers Par/Non-Par/Both)            *
     H*    FBST04 Pos. 2  - (Pbills Info Keep Claimant Info)            *
     H*    FBST05 Pos. 1  - (Pbills entry cursor position)              *
     H*    FBST05 Pos. 2  - (Provider check issued in batch)            *
     H*    FBST06 Pos. 1  - (UCR Percentile Module)                     *
     H*    FBST07 Pos. 1  - (NCCI Medical Data Call Flag)               *
     H*    FBST10 Pos. 1  - Include Zero dollar EOB's in EFT PDF's      *
     H*                                                                 *
     H*    Pass above values back to calling program.                   *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    Various programs                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*                                                                 *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  11/06/96  C:30  Project:  68MDI                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:
     H*
     H*  05/27/15  C:062 Project: 114MRH
     H*                  Add @@INZE as an entry parameter
     H*
     H*  10/20/14  C:274 Project: SY359
     H*                  Add @PBLT as an entry parameter
     H*
     H*  06/11/12  C:277 Project: 114MER
     H*                  Change description of CodeReview flag to ClaimCheck
     H*
     H*  12/13/11  C:191 Project: LA025
     H*                  Add LA Parish flag
     H*
     H*  02/10/11 C:136 SUPPORT: WA114 75791
     H*                 USE RTVCLTID FOR CLIENT ID INSTEAD OF SYAL04
     H*                 WHICH IS A FIELD RESERVED FOR THE PRODUCT
     H*                 LICENSE KEY
     H*
     H*  04/02/09  C:227 Project: MC331
     H*                  Corrected Medical Data Call Flag
     H*                  (FBST07 POS 1)
     H*
     H*  02/16/09  C:198 Project: MC330
     H*                  Get NCCI Medical Data Call Flag
     H*                  (FBST07 POS 1)
     H*
     H*  01/19/09  C:062 Project: MC288
     H*                  ADD SPLIT LINE PARAMETER
     H*
     H*  10/16/07  C:002 Project: ALLCLT
     H*                  GET GO VERSION INFO  (SUBST OF SYFL11 POS 4)
     H*
     H*  08/09/07  C:191 Project: WC1453
     H*                  Retrieve EFT flag.
     H*
     H*  12/12/06  C:062 Project: 144MAB
     H*                  Add Create Provider Flag  (pos 2 of FBAL03)
     H*
     H*  10/25/06  C:002 Project: DR214
     H*                  Add iDarts Installed Flag (pos 3 of SYFL11)
     H*
     H*  08/04/06  C:062 Project: 135WHU
     H*                  Add Credit module flag.
     H*
     H*  03/08/54  C:132 Project: WC1226
     H*                  Add Auto-Reinstatement flag.
     H*
     H*  11/09/04  C:62  Project: 129WCS
     H*                  Add Data Entry/Image flag.
     H*
     H*  05/28/04  C:53  Project: WC1198
     H*                  Add auto batch process flag.
     H*                                                                 *
     H*  05/19/04  C:62  Project: SY100
     H*                  Add Data Center Flag
     H*                                                                 *
     H*  01/14/04  C:62  Project: 114MGP
     H*                  Add UCR Percentile Module
     H*                                                                 *
     H*  10/31/03  C:79  Project: FS270
     H*                  Add client number
     H*                                                                 *
     H*  04/16/03  C:62  Project: MCMNT
     H*                  Add PROVIDER CHECK ISSUED IN THE BATCH FLAG
     H*                                                                 *
     H*  11/13/02  C:62  Project: FS250
     H*                  Add PBILLS entry cursor position.
     H*                                                                 *
     H*  11/11/02  C:79  Project: FS251
     H*                  Add Keep Claimant Info Flag for Pbills.
     H*                                                                 *
     H*  10/28/02  C:62  Project: fs253
     H*                  Add Multi Group Restrictions Installed Flag.
     H*                                                                 *
     H*  10/08/02  C:62  Project: 114MEP
     H*                  Add Par/Non-Par/Both Flag.
     H*                                                                 *
     H*  07/02/02  C:62  Project: fs235
     H*                  Add EOB Code/Note print flag default.
     H*                                                                 *
     H*  04/23/02  C:62  Project: 114MDC
     H*                  ADD 1) CODE REVIEW INSTALLED PARM
     H*                      2) PROMATCH INSTALLED PARM
     H*                      3) MEDICARE CODE REVIEW INSTALLED PARM
     H*                      4) FREQUENCY INSTALLED PARM
     H*                      5) CODE REVIEW AUTO COMPLETE PARM
     H*                                                                 *
     H*  01/16/02  C:62  Project: 114MAO
     H*                  ADD CORRECT CODING INSTALLED PARM
     H*                                                                 *
     H*  01/02/02  C:62  Project: 45MDF
     H*                  ADD AUTO CHECK RELEASE PARM.
     H*                                                                 *
     H*  06/18/99  C:62  Project:  101AX
     H*                  ADDED OUTCOME/SAVINGS INSTALL FLAG
     H*                                                                 *
     H*  06/02/99  C:53  Project:  MC145
     H*                  ADDED NON-PDS WORKERS COMP FLAG
     H*                                                                 *
     H*  12/16/98  C:53  Project:  MC145
     H*                  ADDED FEE SCHEDULE & PROVIDER BILLS FLAGS
     H*                                                                 *
     H*  09/28/98  C:62  Project:  MC098 14022                          *
     H*                  ADDED DRFL06 TO PARM LIST (MS SUPPORT).
     H*                  SEE DESCRIPTION FOUND ABOVE.
     H*                                                                 *
     H*  10/24/97  C:53  Project:  MC145                                *
     H*                  ADDED KIGNORES TO RCD FORMATS, RMVD LR.
     H*                                                                 *
     H*  12/19/97  C:53  Project:  MC145                                *
     H*                  ADDED ADDITIONAL PARMS
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*******************************************************************
     H*                                                                 *
     H* System Control file                                             *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C     *LIKE         DEFINE    SYKEY         KKKEY
     C*
     C*          M A I N     L O G I C
     C*
     C     *ENTRY        PLIST
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
14   C                   PARM                    @@NPWC            1            NON-PDS WC  LS
15   C                   PARM                    @@OS              1            OUTOME/SAVINGS
16   C                   PARM                    @@ARLS            1            Auto Release Flag
17   C                   PARM                    @@CCOD            1            Correct Coding Flag
     C*** Beg Chg *** 06/11/12 ****************************************
18   C*****              PARM                    @@CRVW            1            Code Review Flag
18   C                   PARM                    @@CCHK            1            Claim Check Flag
     C*** End Chg *** 06/11/12 ****************************************
19   C                   PARM                    @@PROM            1            ProMatch Flag
20   C                   PARM                    @@MCRV            1            Medicare CR Flag
21   C                   PARM                    @@FREQ            1            Frequency Flag
22   C                   PARM                    @@CRAC            1            CR Auto Complete
23   C                   PARM                    @@PREO            1            CR Auto Complete
24   C                   PARM                    @@PARN            1            Par/Non-Par/Both
25   C                   PARM                    @@MGRI            1            Multi Group Restr
26   C                   PARM                    @@KCIF            1            Keep Claimant info
27   C                   PARM                    @@CURS            1            PBILLS entry cursor
28   C                   PARM                    @@PCIB            1            Prov Check in Batch
29   C                   PARM                    @@CLT#            3            Client Number
30   C                   PARM                    @@UCPF            1            UCR Percentile
31   C                   PARM                    @@DCF             1            Data Center
32   C                   PARM                    @@AUTB            1            Auto Batch   Flag
33   C                   PARM                    @@DEIF            1            DE/Image Flag
34   C                   PARM                    @@AURE            1            Auto-Reinstate
35   C                   PARM                    @@CRED            1            Credit Module
36   C                   PARM                    @@IDRT            1            Idarts Module
37   C                   PARM                    @@CPRV            1            Create Provider
38   C                   PARM                    @@EFT             1            EFT Flag
39   C                   PARM                    @@GOVR            1            GO VERSION
40   C                   PARM                    @@NCDC            1            NCCI data
41   C                   PARM                    @@SPLT            1            SPLIT
42   C                   PARM                    @@LAST            1            LA PARISH SALES TAX
     C*** Beg Add *** 10/20/14 ****************************************
43   C                   PARM                    @@PBLT            1            Provider bill light
     C*** End Add *** 10/20/14 ****************************************
     C*** Beg Add *** 05/27/15 ****************************************
44   C                   PARM                    @@INZE            1            Include $0 EOB
     C*** End Add *** 05/27/15 ****************************************
     C*
     C*  Access System Control file 'SY' record
     C*
     C                   MOVEL(P)  'SY'          KKKEY
     C                   MOVEL     *BLANK        @@WCIF
     C                   MOVEL     *BLANK        @@MCIF
     C*
     C     KKKEY         CHAIN     SSY01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SYWCIF        @@WCIF
     C                   MOVEL     SYFBIF        @@MCIF
     C     Q1PRMS        IFGT      13
     C                   MOVEL     *BLANK        @@NPWC
     C                   MOVEL     SYFL05        @@NPWC
     C                   ENDIF
     C     Q1PRMS        IFGT      24
     C                   MOVEL(P)  SYFL06        @@MGRI
     C                   ENDIF
     C     Q1PRMS        IFGT      28
     C                   CALL      'RTVCLTID'
     C     @@CLT#        PARM                    @@CLT#
     C                   ENDIF
     C     Q1PRMS        IFGT      30
     C                   SUBST(P)  SYFL11:2      @@DCF
     C                   ENDIF
     C     Q1PRMS        IFGT      34
     C                   MOVEL(P)  SYAL05        @@CRED
     C                   ENDIF
     C     Q1PRMS        IFGT      35
     C                   SUBST(P)  SYFL11:3      @@IDRT
     C                   ENDIF
     C     Q1PRMS        IFGE      39
     C                   SUBST(P)  SYFL11:4      @@GOVR
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Access System Control file 'FB' record
     C*
     C                   MOVEL(P)  'FB'          KKKEY
     C                   MOVEL     *BLANK        @@WCIN
     C     Q1PRMS        IFGT      3
     C                   MOVEL     'N'           @@AR
     C                   MOVEL     'N'           @@NTWK
     C                   MOVEL     'N'           @@UR
     C                   MOVEL     'N'           @@MED
     C                   MOVEL     'N'           @@EDI
     C                   MOVEL     *BLANK        @@WCDK
     C                   ENDIF
     C     Q1PRMS        IFGT      9
     C                   MOVEL     'N'           @@RHAB
     C                   ENDIF
     C     Q1PRMS        IFGT      11
     C                   MOVEL     'N'           @@FSCH
     C                   MOVEL     'N'           @@PRVB
     C                   ENDIF
     C*
     C     KKKEY         CHAIN     SFB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FBFL01        @@WCIN
     C     Q1PRMS        IFGT      3
     C                   SUBST     FBAL04:5      @@AR
     C                   MOVE      FBFL06        @@NTWK
     C                   MOVE      FBFL07        @@UR
     C                   SUBST     FBAL01:9      @@MED
     C                   SUBST     FBAL04:6      @@EDI
     C                   SUBST     FBAL01:4      @@WCDK
     C                   ENDIF
     C     Q1PRMS        IFGT      9
     C                   SUBST     FBAL04:7      @@RHAB
     C                   ENDIF
     C     Q1PRMS        IFGT      11
     C                   SUBST     FBAL04:9      @@FSCH
     C                   SUBST     FBAL04:10     @@PRVB
     C                   ENDIF
     C     Q1PRMS        IFGT      14
     C                   SUBST     FBAL05:3      @@OS
     C                   ENDIF
     C     Q1PRMS        IFGT      36
     C                   SUBST     FBAL03:2      @@CPRV
     C                   ENDIF
     C     Q1PRMS        IFGT      39
     C                   MOVEL     FBST07        @@NCDC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     'SC'          KKKEY
     C*
     C     KKKEY         CHAIN     SDR01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     Q1PRMS        ANDGT     10
     C*
     C                   MOVEL(P)  DRFL06        @@MSS
     C     Q1PRMS        IFGT      32
     C     1             SUBST     DRAL03:1      @@DEIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Access WCMS Control file 'WC' record
     C*
     C     Q1PRMS        IFGT      15
     C                   MOVEL     *BLANKS       @@ARLS
     C     Q1PRMS        IFGT      31
     C                   MOVEL     *BLANKS       @@AUTB
     C                   ENDIF
     C                   MOVEL(P)  'WC'          KKKEY
     C     KKKEY         CHAIN     SWC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  WCFL22        @@ARLS
     C     Q1PRMS        IFGT      31
     C                   MOVE(P)   WCFL28        @@AUTB
     C                   ENDIF
     C     Q1PRMS        IFGT      33
     C                   MOVE(P)   WCFL29        @@AURE
     C                   ENDIF
     C     Q1PRMS        IFGT      37
     C                   MOVEL     WCFL35        @@EFT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     Q1PRMS        IFGT      16
     C                   SUBST     FBAL05:10     @@CCOD
     C                   ENDIF
     C     Q1PRMS        IFGT      17
     C*** Beg Chg *** 06/11/12 ****************************************
     C*****                   MOVEL     FBST01        @@CRVW
      /Free
         @@Cchk = Fbst01;
      /End-Free
     C*** End Chg *** 06/11/12 ****************************************
     C                   MOVE      FBST01        @@PROM
     C                   MOVEL     FBST02        @@MCRV
     C                   MOVE      FBST02        @@FREQ
     C                   MOVEL     FBST03        @@CRAC
     C                   ENDIF
     C     Q1PRMS        IFGT      22
     C                   MOVE      FBST03        @@PREO
     C                   ENDIF
     C     Q1PRMS        IFGT      23
     C                   MOVEL     FBST04        @@PARN
     C                   ENDIF
     C     Q1PRMS        IFGT      25
     C                   MOVE      FBST04        @@KCIF
     C                   ENDIF
     C     Q1PRMS        IFGT      26
     C                   MOVEL     FBST05        @@CURS
     C                   ENDIF
     C     Q1PRMS        IFGT      27
     C                   MOVE      FBST05        @@PCIB
     C                   ENDIF
     C     Q1PRMS        IFGT      29
     C                   MOVEL     FBST06        @@UCPF
     C                   ENDIF
     C     Q1PRMS        IFGT      40
     C                   MOVE      FBST07        @@SPLT
     C                   ENDIF
     C     Q1PRMS        IFGT      41
     C                   MOVEL     FBST09        @@LAST
     C                   ENDIF
     C*** Beg Add *** 10/20/14 ****************************************
     C     Q1PRMS        IFGT      42
     C                   MOVEL     FBST02        @@PBLT
     C                   ENDIF
     C*** Beg Add *** 10/20/14 ****************************************
     C*** Beg Add *** 05/27/15 ****************************************
     C     Q1PRMS        IFGT      43
     C                   MOVEL     FBST10        @@INZE
     C                   ENDIF
     C*** Beg Add *** 05/27/15 ****************************************
     C*
     C                   RETURN
