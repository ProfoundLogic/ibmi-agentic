     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SA009R - RETRIEVE ADDITIONAL WCMC INSTALL INFO
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Access System Control file record 'FB' to obtain value for   *
     H*    FBDT02 (EOMB delete security level).                         *
     H*    FBFL09 (EOMB interactive process)
     H*    FBAL01 Pos. 10 - (Audit/Recon installed)
     H*    FBAL04 Pos.  1 - (WCMC Auto Check Issuance)
     H*    FBAL04 Pos.  2 - (Print WCMC EOMB if pay > 0)
     H*    FBAL04 Pos.  3 - (MDV PPO Leasing installed)
     H*    FBAL04 Pos.  4 - (MDV Download process installed)
     H*    FBAL04 Pos.  5 - (A/R Module installed)
     H*    FBAL04 Pos.  6 - (EDI Module installed)
     H*    FBAL04 Pos.  7 - (Rehab Module installed)
     H*    FBAL04 Pos.  8 - (Allow future service dates)                *
     H*    Pass above values back to calling program.                   *
     H*    FBAL05 Pos.  1 - (Initial view of W/W Medical cases)         *
     H*    FBAL05 Pos.  2 - (Initial view of W/W Prov Bills)            *
     H*    FBAL05 Pos.  3 - (Outcome savings installed)                 *
     H*    FBAL05 Pos.  4 - (Auto Invoice Issuance)                     *
     H*    FBAL05 Pos.  5 - (Work History usage when linked)            *
     H*    FBAL05 Pos.  6 - (Initial view of W/W Providers)             *
     H*    FBAL05 Pos.  7 - (Adjudication duplicate logic criteria)     *
     H*    FBAL05 Pos.  8 - (Prov Ntwk Selection)                       *
     H*    FBTP01         - (Tape for State 01)                         *
     H*    FBTP02         - (Tape for State 02)                         *
     H*    FBTP03         - (Tape for State 03)                         *
     H*    FBTP04         - (Tape for State 04)                         *
     H*    FBTP05         - (Tape for State 05)                         *
     H*    FBTP06         - (Tape for State 06)                         *
     H*    FBTP07         - (Tape for State 07)                         *
     H*    FBTP08         - (Tape for State 08)                         *
     H*    FBTP09         - (Tape for State 09)                         *
     H*    FBTP10         - (Tape for State 10)                         *
     H*    FBST01         - (State Rule 01)                             *
     H*    FBST02         - (State Rule 02)                             *
     H*    FBST03         - (State Rule 03)                             *
     H*    FBST04         - (State Rule 04)                             *
     H*    FBST05         - (State Rule 05)                             *
     H*    FBST06         - (State Rule 06)                             *
     H*    FBST07         - (State Rule 07)                             *
     H*    FBST08         - (State Rule 08)                             *
     H*    FBST09         - (State Rule 09)                             *
     H*    FBST10         - (State Rule 10)                             *
     H*    FBFL02         - (Diary function installed)                  *
     H*    FBFL03         - (Allow provider maintenance from F0070R)    *
     H*    FBFL04         - (Claim edit installed)                      *
     H*    FBFL05         - (DME Installed)                             *
     H*    FBFL08         - (Default LOS region)                        *
     H*    FBFL10         - (Pharmacy Processing)                       *
     H*    FBDT01         - (Date of conversion to new EOMB)            *
     H*    FBDT02         - (Security Level)                            *
     H*    FBDT03         - (Date)                                      *
     H*    FBDT04         - (Date)                                      *
     H*    FBDT05         - (Not used yet)                              *
     H*    FBAL01 Pos.  2 - (Savings $ Info)                            *
     H*    FBAL01 Pos.  3 - (System Calculation for Saved Days)         *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    Various programs                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*                                                                 *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  02/11/99  C:94  Project:                             *
     H*                                                                 *
     H*******************************************************************
     H** REVISIONS:
     H*                                                                 *
     H*  08/15/00 C:62  Project: MC215
     H*                 ADD TAPE STATE PARMS, STATE RULE PARMS,
     H*                 REMAINING FLAG PARMS AND DATE FIELDS.           *
     H*                                                                 *
     H*  04/24/00 C:62  Project: MC229
     H*                 Add parm for Provider Network Selection.
     H*                                                                 *
     H*  04/18/00 C:79  Project: TX012
     H*                 Add parm for adjudication duplicate criteria
     H*                                                                 *
     H*  04/06/00 C:62  Project: MC225
     H*                 Add parm for initial view for WW providers.
     H*                                                                 *
     H*  10/13/99 C:31  Project: MC214
     H*                 Work Hist usage w/WC
     H*                 1. Add WHPG parm
     H*                                                                 *
     H*  08/05/99 C:62  Project: 44MCJ
     H*                 Add PARMS to get auto invoicing flag.
     H*
     H*  06/02/99 C:56  Project: MC152
     H*                 Add PARMS to get initial view number for
     H*                 Work with Medical Cases and Work with Provider
     H*                 Bills.
     H*
     H*  03/15/99 C:56  Project: MC152
     H*                 1. Changed EOBD parm to be 3 numeric
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*******************************************************************
     H*                                                                 *
     H* System Control file                                             *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SWC01)
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
     C*** BEG CHG *** 03/15/99 ****************************************
     C**********           PARM           @@EOBD  1        EOMB DELETE SECURITY
 1   C                   PARM                    @@EOBD            3 0          EOMB DELETE SECURITY
     C*** END CHG *** 03/15/99 ****************************************
 2   C                   PARM                    @@EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM                    @@ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM                    @@ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM                    @@PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM                    @@SDTS            1            ALLOW FUTURE SERV DT
     C*** BEG ADD *** 06/02/99 ****************************************
 7   C                   PARM                    @@MCIV            1            MED CASES INIT VIEW
 8   C                   PARM                    @@PBIV            1            PROV BILLS ININT VIE
     C*** END ADD *** 06/02/99 ****************************************
     C*** BEGIN ADD *** 08/05/99 **************************************
 9   C                   PARM                    @@AINV            1            AUTO INVOICE FLAG
     C*** END   ADD *** 08/05/99 **************************************
     C*** BEGIN ADD *** 10/13/99 **************************************
10   C                   PARM                    @@WHPG            1            Work History Usage
     C*** END ADD *** 10/13/99 ****************************************
     C*** BEGIN ADD *** 04/06/00 **************************************
11   C                   PARM                    @@PRIV            1            PROVIDER INIT VIEW
     C*** END   ADD *** 04/06/00 **************************************
     C*** BEGIN ADD *** 04/18/00 **************************************
12   C                   PARM                    @@TDUP            1            ADJUDICATION DUP CRI
     C*** END   ADD *** 04/18/00 **************************************
     C*** BEGIN ADD *** 04/24/00 **************************************
13   C                   PARM                    @@NWSL            1            PROV NTWK SELECTION
     C*** END   ADD *** 04/24/00 **************************************
     C*** BEGIN ADD *** 08/15/00 **************************************
14   C                   PARM                    @@TP01            2            TAPE STATE 01
15   C                   PARM                    @@TP02            2            TAPE STATE 02
16   C                   PARM                    @@TP03            2            TAPE STATE 03
17   C                   PARM                    @@TP04            2            TAPE STATE 04
18   C                   PARM                    @@TP05            2            TAPE STATE 05
19   C                   PARM                    @@TP06            2            TAPE STATE 06
20   C                   PARM                    @@TP07            2            TAPE STATE 07
21   C                   PARM                    @@TP08            2            TAPE STATE 08
22   C                   PARM                    @@TP09            2            TAPE STATE 09
23   C                   PARM                    @@TP10            2            TAPE STATE 10
24   C                   PARM                    @@ST01            2            STATE RULE 01
25   C                   PARM                    @@ST02            2            STATE RULE 02
26   C                   PARM                    @@ST03            2            STATE RULE 03
27   C                   PARM                    @@ST04            2            STATE RULE 04
28   C                   PARM                    @@ST05            2            STATE RULE 05
29   C                   PARM                    @@ST06            2            STATE RULE 06
30   C                   PARM                    @@ST07            2            STATE RULE 07
31   C                   PARM                    @@ST08            2            STATE RULE 08
32   C                   PARM                    @@ST09            2            STATE RULE 09
33   C                   PARM                    @@ST10            2            STATE RULE 10
34   C                   PARM                    @@DIFI            1            DIARY FUNCTION INST
35   C                   PARM                    @@APRM            1            PROVIDER MAINT
36   C                   PARM                    @@CLED            1            CLAIM EDIT INST
37   C                   PARM                    @@DMEI            1            DME INSTALLED
38   C                   PARM                    @@DLOS            1            DEFAULT LOS RGN
39   C                   PARM                    @@PHPR            1            PHARMACY PROCESSING
40   C                   PARM                    @@DT01            7 0          DATE 1
41   C                   PARM                    @@DT02            7 0          DATE 2
42   C                   PARM                    @@DT03            7 0          DATE 3
43   C                   PARM                    @@DT04            7 0          DATE 4
44   C                   PARM                    @@DT05            7 0          DATE 5
45   C                   PARM                    @@SAV             1            Saving $ Info
46   C                   PARM                    @@SAVC            1            Calc for Saved Days
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C*  Access System Control file 'FB' record
     C*
     C                   MOVEL(P)  'FB'          KKKEY
     C*** BEG CHG *** 03/15/99 ****************************************
     C**********           MOVEL*BLANK    @@EOBD
     C                   Z-ADD     0             @@EOBD
     C*** END CHG *** 03/15/99 ****************************************
     C                   MOVEL     *BLANK        @@EOBI
     C                   MOVEL     *BLANK        @@ARIN
     C                   MOVEL     *BLANK        @@ACIS
     C                   MOVEL     *BLANK        @@PAY0
     C                   MOVEL     *BLANK        @@SDTS
     C*
     C     KKKEY         CHAIN     SFB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FBDT02        @@EOBD
     C                   MOVE      FBFL09        @@EOBI
     C                   SUBST     FBAL01:10     @@ARIN
     C                   SUBST     FBAL04:1      @@ACIS
     C                   SUBST     FBAL04:2      @@PAY0
     C                   SUBST     FBAL04:8      @@SDTS
     C*** BEG ADD *** 06/02/99 ****************************************
     C*** BEGIN CHG *** 08/05/99 **************************************
     C********** Q1PRMS    IFGE 8
     C     Q1PRMS        IFGT      6
     C*** END   CHG *** 08/05/99 **************************************
     C                   SUBST     FBAL05:1      @@MCIV
     C                   SUBST     FBAL05:2      @@PBIV
     C                   ENDIF                                                  Q1PRMS GE 8
     C*** END ADD *** 06/02/99 ****************************************
     C*** BEGIN ADD *** 08/05/99 **************************************
     C     Q1PRMS        IFGT      8
     C*
     C                   SUBST     FBAL05:4      @@AINV
     C*
     C                   ENDIF
     C*** END   ADD *** 08/05/99 **************************************
     C*** BEGIN ADD *** 10/13/99 **************************************
     C     Q1PRMS        IFGT      9
     C                   SUBST     FBAL05:5      @@WHPG
     C                   ENDIF
     C*** END ADD *** 10/13/99 ****************************************
     C*** BEGIN ADD *** 04/06/00 **************************************
     C     Q1PRMS        IFGT      10
     C*
     C                   SUBST     FBAL05:6      @@PRIV
     C*
     C                   ENDIF
     C*** END   ADD *** 04/06/00 **************************************
     C*** BEGIN ADD *** 04/18/00 **************************************
     C     Q1PRMS        IFGT      11
     C*
     C                   SUBST     FBAL05:7      @@TDUP
     C*
     C                   ENDIF
     C*** END   ADD *** 04/18/00 **************************************
     C*** BEGIN ADD *** 04/24/00 **************************************
     C     Q1PRMS        IFGT      12
     C*
     C                   SUBST     FBAL05:8      @@NWSL
     C*
     C                   ENDIF
     C*** END   ADD *** 04/24/00 **************************************
     C*** BEGIN ADD *** 08/15/00 **************************************
     C     Q1PRMS        IFGT      13
     C*
     C                   MOVEL(P)  FBTP01        @@TP01
     C                   MOVEL(P)  FBTP02        @@TP02
     C                   MOVEL(P)  FBTP03        @@TP03
     C                   MOVEL(P)  FBTP04        @@TP04
     C                   MOVEL(P)  FBTP05        @@TP05
     C                   MOVEL(P)  FBTP06        @@TP06
     C                   MOVEL(P)  FBTP07        @@TP07
     C                   MOVEL(P)  FBTP08        @@TP08
     C                   MOVEL(P)  FBTP09        @@TP09
     C                   MOVEL(P)  FBTP10        @@TP10
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      23
     C*
     C                   MOVEL(P)  FBST01        @@ST01
     C                   MOVEL(P)  FBST02        @@ST02
     C                   MOVEL(P)  FBST03        @@ST03
     C                   MOVEL(P)  FBST04        @@ST04
     C                   MOVEL(P)  FBST05        @@ST05
     C                   MOVEL(P)  FBST06        @@ST06
     C                   MOVEL(P)  FBST07        @@ST07
     C                   MOVEL(P)  FBST08        @@ST08
     C                   MOVEL(P)  FBST09        @@ST09
     C                   MOVEL(P)  FBST10        @@ST10
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      33
     C*
     C                   MOVEL(P)  FBFL02        @@DIFI
     C                   MOVEL(P)  FBFL03        @@APRM
     C                   MOVEL(P)  FBFL04        @@CLED
     C                   MOVEL(P)  FBFL05        @@DMEI
     C                   MOVEL(P)  FBFL08        @@DLOS
     C                   MOVEL(P)  FBFL10        @@PHPR
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      39
     C*
     C                   Z-ADD     FBDT01        @@DT01
     C                   Z-ADD     FBDT02        @@DT02
     C                   Z-ADD     FBDT03        @@DT03
     C                   Z-ADD     FBDT04        @@DT04
     C                   Z-ADD     FBDT05        @@DT05
     C*
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      44
     C*
     C                   SUBST     FBAL01:2      @@SAV
     C                   SUBST     FBAL01:3      @@SAVC
     C*
     C                   ENDIF
     C*** END   ADD *** 08/15/00 **************************************
     C                   ENDIF
     C*
     C                   RETURN
