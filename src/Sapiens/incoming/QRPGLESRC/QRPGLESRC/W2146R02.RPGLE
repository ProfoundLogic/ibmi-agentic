     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/25/13 C:282 Project: 129WSC                                *
     H*                Add 'Apportionment' checkbox in SCR01          *
     H*                to define the types of payments that should be *
     H*                should be apportoined.                         *
     H*                                                               *
     H* 06/14/11 C:190 Project: SY255 P#13                            *
     H*                Add search to EDI Recovery Code field.         *
     H*                                                               *
     H* 10/14/10 C:190 Project: WC1953                                *
     H*                Add 'Cost Containment for WCSTAT'at SCR01.     *
     H*                                                               *
     H* 03/10/10 C:242 Project: 25WRK                                 *
     H*                 Add B2AWPY, B2ASMY, B2ACRY to SCR02           *
     H*                                                               *
     H* 07/21/09  C:198 SUPPORT:68101                                 *
     H*                 Added EDI Recovery code.                      *
     H*
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 10/11/06  C:33  Seq#: WA099/41801                             *
     H*                  Use SC305R instead of SC580R.                *
     H*
     H* 08/29/06 C:180 PROJECT: 114WMF
     H*                Added code to warn user that making changes to *
     H*                this table could impact customized exports.    *
     H*
     H* 04/25/05 C:156 PROJECT: 132WCN                                *
     H*                New field B2FLG1 - Print CA fraud statement on *
     H*                checks.                                        *
     H*                                                               *
     H* 02/18/05 C:132 Project: WC1221                                *
     H*                New field B2RCVR - flag for "Include Recovered *
     H*                Fields on Loss Ratio Reports (Y/ )"            *
     H*                                                               *
     H* 06/11/98 C:78 Project: WC577  SUPPORT#: 10410                 *
     H*               Fixed F10 on EDI Payment code (SCPYCD) which    *
     H*               was clearing the original value if nothing was  *
     H*               choosed in the search.                          *
     H*                                                               *
     H* 12/19/97 C:15 Project: WC659                                  *
     H*               Chgd the EDI Pmt/Adj code so it is not required.*
     H*                                                               *
     H* 07/11/97 C:58 PROJECT WC468                                   *
     H*               ADDED EDI FIELDS B2MINR, B2BNFT; WITH SEARCHES.
     H*                                                               *
     H* 07/08/97 C:62 PROJECT WC659                                   *
     H*               ADDED SEARCHES,EDITS AND FIELDS FOR 10 RESERVE  *
     H*               CODES.                                          *
     H*****************************************************************
     FW2146F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTB2P     UF A E             DISK
     F                                     RENAME(PB201:DBRCDF)
     FWTABL     IF   E           K DISK
     FSTCTL2    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      /COPY SCOPYMT01D
     D*
     D RCN             S             10    DIM(10) CTDATA PERRCD(1)
     D*
     D                 DS
     D  RC                     1     20
     D                                     DIM(10)
     D  SCRC01                 1      2
     D  SCRC02                 3      4
     D  SCRC03                 5      6
     D  SCRC04                 7      8
     D  SCRC05                 9     10
     D  SCRC06                11     12
     D  SCRC07                13     14
     D  SCRC08                15     16
     D  SCRC09                17     18
     D  SCRC10                19     20
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Payment Value 1 -
     D                                     Master Information')
     D COMAPI          C                   CONST('WA001R')


     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     B2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @#KEY
     C*
     C     CTKY          KLIST
     C                   KFLD                    ##STAT
     C                   KFLD                    SCRMCD
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    ##KEY
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##SKY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    B2CO#         ##CO#
     C     *LIKE         DEFINE    B2KEY         ##KEY
     C     *LIKE         DEFINE    B2KEY         @@KEY
     C     *LIKE         DEFINE    LRSTAT        ##STAT
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    SCRMCD        ##RMCD
     C     *LIKE         DEFINE    WCKEY         ##SKY
     C                   MOVEL     *BLANKS       QQRTC             1
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     B2STAT        IFEQ      'I'
     C                   MOVE      'A'           B2STAT
     C                   ELSE
     C                   MOVE      'I'           B2STAT
     C                   ENDIF                                                  B2STAT
     C*
     C                   MOVEL     Q1USER        B2USER
     C                   MOVEL     Q1PGM         B2PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B2DT
     C*
     C                   MOVEL     DSTIME        B2TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* BR-2 CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE BR-2 CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90B2KY          CHAIN     PB201                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* BR-2 CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* DART CODE MUST BE VALID
     C*
     C  N90SCRMCD        IFNE      *BLANKS
     C*
     C                   MOVE      'A'           ##STAT
     C*
     C  N90CTKY          CHAIN     PCT01                              99
     C*
     C  N90*IN99         IFEQ      *ON
     C*
     C                   SETON                                        79  90
     C*
     C                   ENDIF                                                  *IN99
     C*
     C  N90CTFL01        IFNE      'Y'
     C*
     C                   SETON                                        51  90
     C*
     C                   ENDIF                                                  CTFL01
     C*
     C                   ENDIF                                                  SCDART NE ''
     C*
     C* IF EDI IS INSTALLED, VALIDATE THE EDI FIELDS.
     C*
     C*
     C*    Validate Benefit indicator
     C     *IN90         IFEQ      *OFF                                         No error
     C     SCBNFT        ANDNE     *BLANK
     C*
     C                   MOVEL(P)  'BIND'        ##TBID
     C                   MOVEL(P)  SCBNFT        ##KEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  *IN90
     C*
     C*
     C*    Validate Minor Payment code
     C     *IN90         IFEQ      *OFF                                         No error
     C     SCMINR        ANDNE     *ZERO
     C*
     C                   MOVEL(P)  'MPMT'        ##TBID
     C                   MOVEL(P)  SCMINR        ##KEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  *IN90
     C*
     C*  Validate Payment/adjustment code, if entered.
     C*
     C     *IN90         IFEQ      *OFF                                         No error
     C*
     C     SCPYCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'PADJ'        ##TBID
     C                   MOVEL(P)  SCPYCD        ##KEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        91  90
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SCPYCD NE BLANKS
     C*
     C                   ENDIF                                                  *IN90
     C*
     C*
     C* VERIFY THAT THE RESERVE CODES ARE VALID
     C*
     C*
     C                   Z-ADD     0             XXCNT
     C  N901             DO        10            I
     C*
     C     RC(I)         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  RC(I)         @#KEY
     C*
     C     RCKY          CHAIN     PRT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     RTSTAT        ORNE      'A'
     C*
     C     I             ADD       60            X
     C*
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN(X)
     C                   LEAVE
     C*
     C                   ENDIF                                                  *IN99
     C                   ADD       1             XXCNT             2 0
     C*
     C                   ENDIF                                                  RC,I
     C*
     C                   ENDDO                                                  I EQ 10
     C*
     C*  Validate Payment/adjustment code, if entered.
     C     *IN90         IFEQ      *OFF                                         No error
     C     SCERCV        IFNE      *BLANKS
     C                   MOVEL(P)  'PADJ'        ##TBID
     C                   MOVEL(P)  SCERCV        ##KEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        44  90
     C*
     C                   ENDIF                                                  *IN44
     C                   ENDIF                                                  SCERCV NE BLANKS
     C                   ENDIF                                                  *IN90
      * LIST RECOVERIES ON LOSS REPORTS FLAG (Y/ )
     C  N90SCRCVR        IFNE      'Y'
     C     SCRCVR        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN76
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCRCVR <> 'Y'
      * LIST RECOVERIES ON LOSS REPORTS FLAG (Y/ )
     C  N90SCFLG1        IFNE      'Y'
     C     SCFLG1        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN77
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCRCVR <> 'Y'
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90B2DT          IFGT      SAVEDT
     C     B2DT          OREQ      SAVEDT
     C     B2TIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF                                         No error
     C     SCEFT         IFNE      'Y'
     C     SCEFT         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN78
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCEFT <> Y
     C                   ENDIF                                                  *IN90 = *OFF
     C     *IN90         IFEQ      *OFF                                         No error
     C     SCCCW         IFNE      'Y'
     C     SCCCW         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN59
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCCCW <> Y
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF                                         No error
     C     SCCCW         IFEQ      'Y'
     C     XXCNT         ANDGT     1
     C                   MOVE      *ON           *IN49
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCCCW =  Y
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
      * Waiting Period Diary (Y/ )
     C  N90SCAWPY        IFNE      'Y'
     C     SCAWPY        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN56
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCAWPY <> 'Y'
      * Statutory MMI Diary (Y/ )
     C  N90SCASMY        IFNE      'Y'
     C     SCASMY        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN57
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCASMY <> 'Y'
      * Change Comp Rate Diary (Y/ )
     C  N90SCACRY        IFNE      'Y'
     C     SCACRY        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN58
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCACRY <> 'Y'
      *
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     B2KEY         SCKEY
     C                   MOVEL     B2DESC        SCDESC
     C                   MOVEL     B2PLCD        SCPLCD
     C                   MOVEL     B2STAT        SCSTAT
     C                   MOVEL     B2BNFT        SCBNFT
     C                   MOVEL     B2MINR        SCMINR
     C                   MOVEL     B2PYCD        SCPYCD
     C                   MOVEL     B2NDDT        SCNDDT
     C                   MOVEL     B2RMCD        SCRMCD
     C                   MOVEL     B2RC01        SCRC01
     C                   MOVEL     B2RC02        SCRC02
     C                   MOVEL     B2RC03        SCRC03
     C                   MOVEL     B2RC04        SCRC04
     C                   MOVEL     B2RC05        SCRC05
     C                   MOVEL     B2RC06        SCRC06
     C                   MOVEL     B2RC07        SCRC07
     C                   MOVEL     B2RC08        SCRC08
     C                   MOVEL     B2RC09        SCRC09
     C                   MOVEL     B2RC10        SCRC10
     C                   MOVEL     B2FLG1        SCFLG1
     C*
     C                   MOVEL     B2RCVR        SCRCVR
     C*
     C                   MOVEL     B2EFT         SCEFT
     C                   MOVEL     B2ERCV        SCERCV
      *
     C                   MOVEL     B2AWPY        SCAWPY
     C                   MOVEL     B2ASMY        SCASMY
     C                   MOVEL     B2ACRY        SCACRY
     C                   MOVEL     B2CCW         SCCCW
      *** BEG  ADD ****** 11/25/13 **************************************
     C                   MOVEL     B2PSIG        SCPSIG
      *** END  ADD ****** 11/25/13 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPLCD
     C                   MOVEL     'A'           SCSTAT
     C                   MOVE      *BLANKS       SCBNFT
     C                   MOVE      *BLANKS       SCMINR
     C                   MOVE      *BLANKS       SCPYCD
     C                   MOVE      *BLANKS       SCNDDT
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCRC01
     C                   MOVE      *BLANKS       SCRC02
     C                   MOVE      *BLANKS       SCRC03
     C                   MOVE      *BLANKS       SCRC04
     C                   MOVE      *BLANKS       SCRC05
     C                   MOVE      *BLANKS       SCRC06
     C                   MOVE      *BLANKS       SCRC07
     C                   MOVE      *BLANKS       SCRC08
     C                   MOVE      *BLANKS       SCRC09
     C                   MOVE      *BLANKS       SCRC10
     C                   MOVEL     *BLANK        SCFLG1
     C*
     C                   MOVEL     *BLANK        SCEFT
     C                   MOVEL     *BLANKS       SCERCV
      *
     C                   MOVEL     *BLANK        SCAWPY
     C                   MOVEL     *BLANK        SCASMY
     C                   MOVEL     *BLANK        SCACRY
     C                   MOVEL     *BLANK        SCCCW
      *** BEG  ADD ****** 11/25/13 **************************************
     C                   MOVEL     *BLANK        SCPSIG
      *** END  ADD ****** 11/25/13 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         B2CO#
     C                   MOVEL     SCKEY         B2KEY
     C                   MOVEL     SCDESC        B2DESC
     C                   MOVEL     SCPLCD        B2PLCD
     C                   MOVEL     SCSTAT        B2STAT
     C                   MOVEL     SCBNFT        B2BNFT
     C                   MOVEL     SCMINR        B2MINR
     C                   MOVEL     SCPYCD        B2PYCD
     C                   MOVEL     SCNDDT        B2NDDT
     C                   MOVEL     SCRMCD        B2RMCD
     C                   MOVEL     SCRC01        B2RC01
     C                   MOVEL     SCRC02        B2RC02
     C                   MOVEL     SCRC03        B2RC03
     C                   MOVEL     SCRC04        B2RC04
     C                   MOVEL     SCRC05        B2RC05
     C                   MOVEL     SCRC06        B2RC06
     C                   MOVEL     SCRC07        B2RC07
     C                   MOVEL     SCRC08        B2RC08
     C                   MOVEL     SCRC09        B2RC09
     C                   MOVEL     SCRC10        B2RC10
     C*
     C                   MOVEL     SCRCVR        B2RCVR
     C                   MOVEL     SCFLG1        B2FLG1
     C*
     C                   MOVEL     SCEFT         B2EFT
     C                   MOVEL     SCERCV        B2ERCV
      *
     C                   MOVEL     SCAWPY        B2AWPY
     C                   MOVEL     SCASMY        B2ASMY
     C                   MOVEL     SCACRY        B2ACRY
     C                   MOVEL     SCCCW         B2CCW
      *** BEG  ADD ****** 11/25/13 **************************************
     C                   MOVEL     SCPSIG        B2PSIG
      *** END  ADD ****** 11/25/13 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C* EDI Benefit Code Search
     C*
     C     HFLD          IFEQ      'SCBNFT'
     C*
     C                   MOVE      *BLANKS       SCBNFT
     C                   MOVEL     'BIND'        QQID              5
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCPRDS        QQPRDS           40
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    @@MODE
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCBNFT
     C*
     C     QQRTC         IFEQ      'A'
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ENDIF                                                  QQRTC='A'
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        52
     C*
     C                   ELSE
     C*
     C                   SETON                                        53
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCBNFT
     C*
     C*
     C*    Minor Payment Code Search
     C*
     C     HFLD          IFEQ      'SCMINR'
     C*
     C                   MOVE      *BLANKS       SCMINR
     C                   MOVEL     'MPMT'        QQID              5
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCPRDS        QQPRDS           40
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    @@MODE
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCMINR
     C*
     C     QQRTC         IFEQ      'A'
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ENDIF                                                  QQRTC='A'
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        53
     C*
     C                   ELSE
     C*
     C                   SETON                                        54
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCMINR
     C*
     C*
     C* EDI PAYMENT ADJUSTMENT CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCPYCD'
     C*
     C                   MOVEL     'PADJ'        QQID              5
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCPRDS        QQPRDS           40
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    @@MODE
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     *BLANKS       IFNE      QQKEY
     C                   MOVEL     QQKEY         SCPYCD
     C                   END
     C*
     C     QQRTC         IFEQ      'A'
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ENDIF                                                  QQRTC='A'
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        54
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCPYCD
     C*
     C*
     C                   Z-ADD     1             I                 2 0
     C*
     C* DARTS REMARK CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       QQRMCD
     C*
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *LIKE         DEFINE    ##RMCD        QQRMCD
     C*
     C*
     C     *INKL         IFEQ      *ON
     C     QQRMCD        OREQ      *BLANKS
     C*
     C                   MOVE      *ON           *IN55
     C*
     C                   ELSE
     C*
     C                   MOVEL     QQRMCD        SCRMCD
     C                   MOVE      *ON           *IN80
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCRMCD
     C*
     C* RESERVE CODE SEARCHES
     C*
     C     HFLD          LOOKUP    RCN(I)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *BLANKS       @#KEY
     C*
     C                   CALL      'W2136R'
     C                   PARM      '2'           @#CALL            1
     C     *INKC         PARM                    @#KC              1
     C     *INKL         PARM                    @#KL              1
     C                   PARM                    @#KEY            10
     C*
     C                   MOVEL     @#KEY         RC(I)
     C*
     C     *INKL         IFEQ      *ON
     C     @#KEY         OREQ      *BLANKS
     C*
     C     I             ADD       79            X                 2 0
     C                   MOVE      *ON           *IN(X)
     C*
     C                   ELSE
     C*
     C     I             ADD       80            X
     C                   MOVE      *ON           *IN(X)
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *INKJ
     C* EDI RECOVERY CODE CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCERCV'
     C*
     C                   MOVEL     'PADJ'        QQID              5
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCPRDS        QQPRDS           40
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    @@MODE
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     *BLANKS       IFNE      QQKEY
     C                   MOVEL     QQKEY         SCERCV
     C                   END
     C*
     C     QQRTC         IFEQ      'A'
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ENDIF                                                  QQRTC='A'
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        42
     C*
     C                   ELSE
     C*
     C                   SETON                                        43
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCERCV
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* CHECK THAT EDI IS INSTALLED, OR IGNORE EDI INFO
     C*
     C                   MOVEL     'WC'          ##SKY
     C*
     C     SYKY          CHAIN     SWC01                              99
     C*
     C*
     C*++ BEG ADD +++ 08/29/06 +++++++++++++++++++++++++++++++
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     CLTID         IFEQ      '114'
     C     @@MODE        IFNE      'D'
     C     @@MODE        ANDNE     'S'
     C                   EXFMT     SCR04
     C                   ENDIF
     C                   ENDIF
     C*++ END ADD +++ 08/29/06 +++++++++++++++++++++++++++++++
     C     WCFL35        IFEQ      'Y'
     C                   MOVE      *ON           *IN94
     C                   ELSE
     C                   MOVE      *OFF          *IN94
     C                   ENDIF
     C     CLTID         IFEQ      '025'
     C                   Z-ADD     2             #OFSCR            2 0
     C                   ELSE
     C                   Z-ADD     1             #OFSCR
     C                   ENDIF
     C*** Begin Add *** 11/25/13 ***********************************************
      /Free
       If CltId = '129';
          isFCC = *On;
       Else;
          isFCC = *Off;
       EndIf;
      /End-Free
     C*** End   Add *** 11/25/13 ***********************************************
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVE      *BLANKS       B2PSCD
     C                   MOVEL     Q1USER        B2USER
     C                   MOVEL     Q1PGM         B2PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B2DT
     C*
     C                   MOVEL     DSTIME        B2TIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVE      *BLANKS       B2PSCD
     C                   MOVEL     Q1USER        B2USER
     C                   MOVEL     Q1PGM         B2PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B2DT
     C*
     C                   MOVEL     DSTIME        B2TIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTB2P
     C*
     C                   ENDSR
** RESERVE CODE SCREEN FIELDS
SCRC01
SCRC02
SCRC03
SCRC04
SCRC05
SCRC06
SCRC07
SCRC08
SCRC09
SCRC10
