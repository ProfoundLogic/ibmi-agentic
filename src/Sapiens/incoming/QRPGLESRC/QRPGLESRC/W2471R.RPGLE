     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* W2471R - Work with Rate Set Codes
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/25/25 C:65  PROJECT: WC2952/PHW#119268 (PRESCIENT)         *
     H*                ADD DATE FOR ADOPTION OF 3 DIGIT RATES.        *
     H*                                                               *
     H* 10/20/04 C:132 PROJECT: WC1136                                *
     H*                Add option 7 to display W/W screen for the     *
     H*                Loss Cost Multipliers - Program W2471R.        *
     H*                                                               *
     H* 06/30/00 C:61  SUPPORT WC999 22743                            *
     H*                Correct "Position to".                         *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768                          *
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.*
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
     FW2471F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMRSP     IF   E             DISK
     F                                     RENAME(PRS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMRSL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRS01:DBRCDF)
     FWTGTL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
      ***** BEG CHG ***** 10/20/04 ***********************************
      ***** END CHG ***** 10/20/04 ***********************************
     D************        OF      1  11  1   OFD    35   Options, Function
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      ***** END CHG ***** 10/20/04 ***********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Rate Set Codes')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2471R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RTCD
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
     C*
     C*  STATE PARM LIST
     C*
     C     STSPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKJRST
     C                   KFLD                    KKRTCD
     C*
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY            10
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    ##KEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    RSJRST        KKJRST
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    RSRTCD        @@RTCD
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    RSDESC        U1PDSC
     C     *LIKE         DEFINE    RSDESC        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSJRST        ORNE      *BLANKS
B01  C     SSDESC        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C  N90*INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C*  THE JURISDICTION STATE MUST EXIST IN STSTP OR MUST BE A
     C*  VALID TABLE ENTRY.
     C*
     C  N90SSJRST        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SSJRST        ##KEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   MOVEL     'JURS'        ##TBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   SETON                                        9088
     C*
     C                   ENDIF                                                  *IN99 GTKY
     C*
     C                   ENDIF                                                  *IN99 STKY
     C*
     C                   ENDIF                                                  SSJRST NE
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKJRST
     C                   MOVE      *LOVAL        KKRTCD
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKJRST
      ***** BEG DEL ******* 06/30/00 **********************************
     C****************     MOVELSCPST1    KKRTCD
      ***** END DEL ******* 06/30/00 **********************************
      ***** BEG ADD ******* 06/30/00 **********************************
     C                   MOVEL     SCPST2        KKRTCD
      ***** END ADD ******* 06/30/00 **********************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCJRST        KKJRST
     C                   MOVEL     SCRTCD        KKRTCD
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH SUBSET SCREEN.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSJRST'
     C*
     C                   CALL      'SY601R'      STSPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        8090
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST02                              99
     C*
     C                   MOVEL     STKEY         SSJRST
     C                   SETON                                        8190
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     RSSTAT        SCSTAT
     C                   MOVEL     RSJRST        SCJRST
     C                   MOVEL     RSRTCD        SCRTCD
     C                   MOVEL     RSCRFL        SCCRFL
     C                   MOVEL     RSJRST        SCJRST
     C*** begin add ***  06/25/25  ************************************
     C                   Z-ADD     RS3DATE       SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SC3DATE
     C*** end add *****  06/25/25  ************************************
     C*
     C                   MOVEL(P)  RSRTCD        SCDATA
     C                   MOVE      RSDESC        SCDATA
     C*
     C                   ENDSR
      ***** BEG ADD ***** 10/20/04 ***********************************
     C*================================================================
     C* OPT07 - SUBROUTINE FOR OPTION 7
     C*================================================================
     C*
     C* OPTION 7 = Call for W/W Loss cost multipliers
     C*
     C     OPT07         BEGSR
     C*
     C                   CALL      'W2472R'
     C                   PARM      ' '           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      RSJRST        ##JRST            2
     C                   PARM      RSRTCD        ##RTCD            3
     C* Hold Loss Multiplier for future display
     C                   MOVE      *BLANKS       XXLCM             3
     C     ##RTCD        IFNE      *BLANKS
     C                   MOVE      ##RTCD        XXLCM
     C                   ENDIF
     C*
     C                   ENDSR
      ***** END ADD ***** 10/20/04 ***********************************
     C*
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
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
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
      ***** BEG ADD ***** 10/20/04 ***********************************
     C     SCSEL         CASEQ     '7'           OPT07
     C                   ENDCS
      ***** END ADD ***** 10/20/04 ***********************************
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
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
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   MOVEL     HD(2)         SCHDL2
     C*
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
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
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
     C                   MOVEL     SCRTCD        @@RTCD
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
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
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  RSDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     RSSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSJRST        IFNE      *BLANKS
     C     SSJRST        ANDNE     RSJRST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Rating                                        Consent   Adoption of
State    Code Description                     to rate   3 digit rates  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1077=Multipliers            YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
213F13=User parms           YYN
217F17=Subset               YYY
3
