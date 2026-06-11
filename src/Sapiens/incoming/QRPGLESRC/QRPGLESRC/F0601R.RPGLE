     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H***************************************************************
     H* REVISIONS
     H*
     H* 06/23/16 C:212 PROJECT: NY038
     H*                Edit option 30 for State NY
     H*
     H* 02/11/16  C:307 SUPPORT:37159
     H*                 To subset non adjudication state list and
     H*                 not to show in provider bill and work with
     H*                 medical cases.
     H*
     H* 12/29/14 C:296 PROJECT: FL079
     H*                Add option 47 for Work with FL County/Rate.
     H*
     H* 06/25/13 C:274 PROJECT: NY029
     H*                Add option 44 for Work with NY Comorbidity Factors
     H*
     H* 06/14/13 C:277 PROJECT: ME005
     H*                Add option 45 and 46
     H*
     H* 02/21/12 C:277 PROJECT: IL004
     H*                Add option 42 and 43 also modify the option 6
     H*
     H* 08/11/10 C:227 PROJECT: IL001
     H*                Add option 06 for Work with IL Fee Schedule
     H*
     H* 04/19/10 C:227 PROJECT: NY020
     H*                Add option 39 for Work with APR DRG
     H*                Add option 40 for Work with Inpat Case Pay Rates
     H*                Add option 41 for Work with Inpat Exempt Unit Rates
     H*
     H* 02/26/07 C:062 PROJECT: 114MMZ
     H*                MEDICARE RATES.
     H*
     H* 11/08/05 C:163 PROJECT: 48MWK
     H*                FL EDI CORRECTION LICENSE #
     H*
     H* 09/28/04 C:79  PROJECT TX026
     H*                Add FAAGL2 to F2522R
     H*
     H* 09/07/04 C:79  PROJECT NY009
     H*                Add option 34 for Work with ASC's by PAS group
     H*                Add option 35 for Work with Hospital Rates
     H*                Add option 36 for Work with Exempt Units/Hosp
     H*                Add option 37 for Work with DRG's
     H*
     H* 04/07/03 C:79  PROJECT PA020
     H*                Add option 33 for Work with PT per visit rates
     H*
     H* 11/21/02 C:79  PROJECT NY006
     H*                Change option 30 desc to FEIN/Prov# Table
     H*
     H* 09/27/02 C:62  PROJECT CA001
     H*                Add option 32 for Work with DRG Factors/CCR
     H*
     H* 09/25/02 C:79  PROJECT CA001
     H*                Add file override for FTFEL1 to opt20
     H*
     H* 01/08/02 C:62  PROJECT WV004G
     H*                Add Option '31=Provider DRG Rates'
     H*
     H* 08/17/01 C:62  PROJECT 114MCG
     H*                Add Option '30=FEIN/Med# Table'
     H*
     H* 08/08/01 C:62  PROJECT 114MCG
     H*                Add Option '29=CORF Table'
     H*
     H* 08/03/01 C:62  PROJECT 114MCG
     H*                Add Option '28=Pharmacy RCC Table'
     H*
     H* 08/02/01 C:62  PROJECT 114MCG
     H*                Add Option '27=RCC by Prov and Rev Code'
     H*
     H* 08/02/01 C:62  PROJECT 114MCG
     H*                Add Option '26=ASC by MSA and Region'
     H*
     H* 08/01/01 C:62  PROJECT 114MCG
     H*                Add Option '25=ASC by Provider'.
     H*
     H* 06/28/01 C:61  PROJECT 114MCG
     H*                Add Option '22=Skilled Nursing'.
     H*
     H* 06/27/01 C:136 PROJECT: 114MCG
     H*                ADD OPTION 23  TO PASS STATE CODE TO PGM F2524R
     H*                FOR HOME HEALTH AGENCY TABLE PROGRAM
     H*
     H* 06/25/01 C:136 PROJECT: 114MCG
     H*                ADD OPTION 21  TO PASS STATE CODE TO PGM F2522R
     H*                FOR ASC BY REGION/GROUP PROGRAM
     H*
     H* 06/08/01 C:136 PROJECT: 114MCG
     H*                ADD OPTION 19 AND 20 TO PASS STATE CODE TO PGM F2520R
     H*                AND F2521R RESPECTIVELY
     H*
     H* 02/15/01 C:062 PROJECT: FS126
     H*                REMOVE CHKSTR.
     H*
     H* 09/26/00 C:062 PROJECT: FSMNT
     H*                -CHANGE PARM LIST ACDPL1 TO INCLUDE 3RD LOGICAL.
     H*                -LOAD THIRD LOGICAL IN CALL TO WW EOMB CODES.
     H*
     H* 09/20/00 C:117 PROJECT: FL031
     H*                ADD OPTION 18=FL PROVIDER LIC#
     H*
     H* 08/15/00 C:62  PROJECT: MC215
     H*                REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H* 05/25/00 C:062 PROJECT MC205
     H*                ADD PARMS TO F2011R.
     H*
     H* 05/25/00 C:062 PROJECT MC226
     H*                REMOVE OPTION '13' COUNTY CODES.
     H*
     H* 03/17/99 C:117 PROJECT MC152
     H*              - ADD CALL TO F2011R - WORK W/ T/P COMPONENTS
     H*                TABLE
     H*
     H* 10/01/98  C:79 PROJECT MC152
     H*              - Add call to F2390R01 - Work with State EOMB
     H*                comments
     H*
     H* 12/18/97  C:61 PROJECT MC159
     H*              - Add PARM to return facility code description
     H*
     H***************************************************************
     H*
     FF0601F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTSTL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PST01:DBRCDF)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     F*** BEGIN ADD *** 01/08/02 **************************************
     FFMSIL     IF   E           K DISK
     F*** END   ADD *** 01/08/02 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
      ***** BEG CHG ******* 06/28/01 **********************************
     D**************      OF      1  23  1   OFD    35   Options, Function
     D***** BEG CHG ******* 04/07/03 **********************************
     D******************* OF      1  32  1   OFD    35   Options, Function
     D***** BEG CHG ******* 09/07/04 **********************************
     D******************* OF      1  33  1   OFD    35   Options, Function
     D*** BEG CHG *** 04/19/10 ****************************************
     D******************* OF      1  38  1   OFD    35   Options, Function
     D*** BEG CHG *** 02/21/12 ****************************************
     D*****               OF      1  41  1   OFD    35   Options, Function
     D*** BEG CHG *** 06/14/13 ****************************************
     D*** BEG CHG *** 06/25/13 ****************************************
     D****OF              S              1    DIM(43) CTDATA PERRCD(1)             Options, Function
     D******** OFD             S             35    DIM(43) ALT(OF)
     D*** BEG CHG *** 12/29/14 ****************************************
     D****** OF              S              1    DIM(46) CTDATA PERRCD(1)  Options, Function
     D****** OFD             S             35    DIM(46) ALT(OF)
     D OF              S              1    DIM(47) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(47) ALT(OF)
     D*** END CHG *** 12/29/14 ****************************************
     D*** END CHG *** 06/25/13 ****************************************
     D*** END CHG *** 06/14/13 ****************************************
     D*** END CHG *** 02/21/12 ****************************************
     D*** BEG CHG *** 04/19/10 ****************************************
     D***** END CHG ******* 09/07/04 **********************************
     D***** END CHG ******* 04/07/03 **********************************
     D***** END CHG ******* 06/28/01 **********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*** INSTALLED ADJUDICATION STATES FROM SFBPF
     D*
     D                 DS
     D*** BEGIN CHG *** 08/15/00 **************************************
     D**********                              1  20 ST
     D**********                              1   2 FBST01
     D**********                              3   4 FBST02
     D**********                              5   6 FBST03
     D**********                              7   8 FBST04
     D**********                              9  10 FBST05
     D**********                             11  12 FBST06
     D**********                             13  14 FBST07
     D**********                             15  16 FBST08
     D**********                             17  18 FBST09
     D**********                             19  20 FBST10
     D  ST                     1     20
     D                                     DIM(10)                              ADJ ST FROM SFBPF
     D  ##ST01                 1      2
     D  ##ST02                 3      4
     D  ##ST03                 5      6
     D  ##ST04                 7      8
     D  ##ST05                 9     10
     D  ##ST06                11     12
     D  ##ST07                13     14
     D  ##ST08                15     16
     D  ##ST09                17     18
     D  ##ST10                19     20
     D*** END   CHG *** 08/15/00 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State Codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          STATE
     C                   PARM                    @@RRN2            9 0          CODE
     C*** Beg Add *** 02/11/16 ********************************************
     C                   PARM                    @@adstate         1
     C*** End Add *** 02/11/16 ********************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##PGM            10
     C                   PARM                    ##FIL1           10
     C                   PARM                    ##FIL2           10
     C                   PARM                    ##FIL3           10
     C                   PARM                    ##ST
     C                   PARM      *BLANKS       ##CODE           15
      *** BEG ADD ***** 12/18/97 ************************************
     C                   PARM      *BLANKS       ##DESC           40
      *** END ADD ***** 12/18/97 ************************************
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   PARM                    ##FIL4           10
     C*** END   ADD *** 09/26/00 **************************************
     C*
     C*  Parm list for County Codes and TOC Codes.
     C*
     C     ACDPL2        PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    #@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEG ADD *** 03/17/1999 **************************************
     C*
     C* Professional/Technical component table pgm PLIST
     C*
     C     TPTPL         PLIST
     C                   PARM                    @@CALL            1
     C*** BEGIN ADD *** 05/25/2000 ************************************
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C*** END   ADD *** 05/25/2000 ************************************
     C*** END ADD *** 03/17/1999 **************************************
     C*** BEG ADD *** 02/21/12 **************************************
     C     HNHPL         PLIST
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      SCST          ##ST
     C*** END ADD *** 02/21/12 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKKEY
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN ADD *** 01/08/02 **************************************
      *
     C     SIKY          KLIST
     C                   KFLD                    KKP1ST
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKSPCD
     C*** END   ADD *** 01/08/02 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    SCST          ##ST
     C*** BEGIN ADD *** 01/08/02 **************************************
     C     *LIKE         DEFINE    SIST          KKP1ST
     C     *LIKE         DEFINE    SIPRTY        KKTYPE
     C     *LIKE         DEFINE    SISPCD        KKSPCD
     C*** END   ADD *** 01/08/02 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 01/08/02 **************************************
     C     SCSEL         IFEQ      '31'
     C                   MOVEL     SCST          KKP1ST
     C                   MOVEL     '4'           KKTYPE
     C                   MOVEL     *BLANKS       KKSPCD
     C     SIKY          CHAIN     PSI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/08/02 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL(P)  SCPST1        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCKEY         KKKEY
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     STKEY         SCKEY
     C                   MOVEL     STKEY         SCST
     C*
     C                   MOVEL(P)  STKEY         SCDATA
     C                   MOVE      STNAME        SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - WORK WITH CPT4 CODES
     C*================================================================
     C     OPT6          BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C*** BEG ADD *** 08/11/10 ****************************************
     C     SCST          IFEQ      'IL'
     C*** BEG ADD *** 02/21/12 *****************************
     C                   MOVEL     '1'           SCRGN
     C                   MOVEL     *BLANK        SCZIP
     C                   MOVEL     *BLANK        XXFLG             1
     C                   MOVEA     '000'         *IN(74)
     C*
     C     XXFLG         DOWEQ     *BLANK
     C     *INKL         ANDEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C                   SELECT
     C*
     C     SCRGN         WHENEQ    '1'
     C     SCZIP         ANDEQ     '1'
     C                   MOVE      *ON           *IN74                          SELECTED TWO
     C*
     C     SCRGN         WHENNE    '1'
     C     SCRGN         ANDNE     ' '
     C                   MOVE      *ON           *IN75                          INVALID CODE
     C*
     C     SCZIP         WHENNE    '1'
     C     SCZIP         ANDNE     ' '
     C                   MOVE      *ON           *IN76                          INVALID CODE
     C*
     C     SCRGN         WHENEQ    '1'
     C*
     C                   MOVEL(P)  'F2547R'      ##PGM
     C                   MOVEL(P)  'FMF1P'       ##FIL1
     C                   MOVEL(P)  'FMF1L1'      ##FIL2
     C                   MOVEL(P)  'FMF1L2'      ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C                   MOVEL     '1'           XXFLG
     C*
     C     SCZIP         WHENEQ    '1'
     C*
     C*** END ADD *** 02/21/12 *****************************
     C                   MOVEL(P)  'F2544R'      ##PGM
     C                   MOVEL(P)  'FMFSP'       ##FIL1
     C                   MOVEL(P)  'FMFSL1'      ##FIL2
     C                   MOVEL(P)  'FMFSL2'      ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*** BEG ADD *** 02/21/12 *****************************
     C                   MOVEL     '1'           XXFLG
     C*
     C                   ENDSL
     C*
     C                   SETOFF                                       06
     C     *INKL         IFEQ      *ON
     C                   SETON                                        07
     C                   ELSE
     C                   SETOFF                                       07
     C                   ENDIF
     C*
     C                   ENDDO
     C*** END ADD *** 02/21/12 *****************************
     C                   ELSE
     C*** END ADD *** 08/11/10 ****************************************
     C                   MOVEL(P)  'F2305R'      ##PGM
     C                   MOVEL(P)  'FMPRP'       ##FIL1
     C                   MOVEL(P)  'FMPRL'       ##FIL2
     C                   MOVEL(P)  'FMPRL1'      ##FIL3
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*** BEG ADD *** 08/11/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 08/11/10 ****************************************
     C*
     C*** BEG ADD *** 02/21/12 *****************************
     C     *INKL         IFEQ      *OFF
     C*** END ADD *** 02/21/12 *****************************
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/21/12 *****************************
     C                   ENDIF
     C*** END ADD *** 02/21/12 *****************************
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - WORK WITH PROC CODE MOD
     C*================================================================
     C     OPT7          BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2315R'      ##PGM
     C                   MOVEL(P)  'FTPMP'       ##FIL1
     C                   MOVEL(P)  'FTABL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - WORK WITH SERVICE CODES
     C*================================================================
     C     OPT8          BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2320R'      ##PGM
     C                   MOVEL(P)  'FMSVP'       ##FIL1
     C                   MOVEL(P)  'FMSVL1'      ##FIL2
     C                   MOVEL(P)  'FMSVL2'      ##FIL3
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT9 - WORK WITH BODY AREAS
     C*================================================================
     C     OPT9          BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2325R'      ##PGM
     C                   MOVEL(P)  'FTPAP'       ##FIL1
     C                   MOVEL(P)  'FTABL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT10 - WORK WITH EOB CODES
     C*================================================================
     C     OPT10         BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2330R'      ##PGM
     C                   MOVEL(P)  'FTEBP'       ##FIL1
     C                   MOVEL(P)  'FTABL'       ##FIL2
     C*** BEGIN CHG *** 09/26/00 **************************************
     C**********           MOVEL*BLANKS   ##FIL3
     C                   MOVEL(P)  'FTEBL1'      ##FIL3
     C*** END   CHG *** 09/26/00 **************************************
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  'FTEBL2'      ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT11 - WORK WITH ZIP CODE/REGIONS
     C*================================================================
     C     OPT11         BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2345R'      ##PGM
     C                   MOVEL(P)  'FMZRP'       ##FIL1
     C                   MOVEL(P)  'FMZRL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT12 - WORK WITH TYPE OF FAC CODES
     C*================================================================
     C     OPT12         BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2355R'      ##PGM
     C                   MOVEL(P)  'FTFTP'       ##FIL1
     C                   MOVEL(P)  'FTABL'       ##FIL2
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT13 - WORK WITH COUNTY CODES
     C*================================================================
     C     OPT13         BEGSR
     C*
     C                   MOVEL     SCST          #@ST
     C*
     C                   CALL      'F2360R'      ACDPL2
     C*
     C                   ENDSR
     C*================================================================
     C* OPT14 - WORK WITH TYPE OF SERVICE CODES
     C*================================================================
     C     OPT14         BEGSR
     C*
     C                   MOVEL     SCST          #@ST
     C*
     C                   CALL      'F2365R'      ACDPL2
     C*
     C                   ENDSR
     C*================================================================
     C* OPT15 - WORK WITH PLACE OF SERVICE CODE
     C*================================================================
     C     OPT15         BEGSR
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           EXSR STVLSL
     C*
     C********** *IN90     IFNE *ON
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   MOVEL(P)  'F2370R'      ##PGM
     C                   MOVEL(P)  'FTPSP'       ##FIL1
     C                   MOVEL(P)  'FTPSL'       ##FIL2
     C                   MOVEL(P)  'FTPSL2'      ##FIL3
     C*** BEGIN ADD *** 09/26/00 **************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C*** END   ADD *** 09/26/00 **************************************
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 02/15/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 02/15/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT16 - WORK WITH EOMB COMMENTS
     C*================================================================
     C     OPT16         BEGSR
     C*
     C                   MOVEL     SCST          #@ST
     C*
     C                   CALL      'F2390R'      ACDPL2
     C*
     C                   ENDSR
     C*** BEG ADD *** 03/17/1999 **************************************
     C*
     C*================================================================
     C* OPT17 - WORK WITH T/P COMPONENT TABLE
     C*================================================================
     C     OPT17         BEGSR
     C     SCST          IFNE      'UC'
     C                   SETON                                        72
     C                   ELSE
     C                   CALL      'F2011R'      TPTPL
     C                   ENDIF
     C                   ENDSR
     C*** END ADD *** 03/17/1999 **************************************
     C*** BEG ADD *** 09/20/00 ****************************************
     C*================================================================
     C* OPT18 - WORK WITH PROVIDER LIC#
     C*================================================================
     C     OPT18         BEGSR
     C     SCST          IFNE      'FL'
     C                   SETON                                        72
     C                   ELSE
     C                   CALL      'F2395R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C*** BEG CHG *** 11/08/05 ****************************************
     C***************      PARM           @@LIC#  9
     C                   PARM                    @@LIC#           13
     C*** END CHG *** 11/08/05 ****************************************
     C*
     C                   ENDIF
     C                   ENDSR
     C*** END ADD *** 09/20/00 ****************************************
     C*
     C*** BEG ADD *** 06/08/01 ****************************************
     C*================================================================
     C* OPT19 - WORK WITH DRG PROSPECTIVE PAYMENT SYSTEM TABLE
     C*================================================================
     C     OPT19         BEGSR
     C*
     C*
     C                   MOVEL(P)  'F2520R'      ##PGM
     C                   MOVEL(P)  'FAPOP'       ##FIL1
     C                   MOVEL(P)  'FAPOL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - WORK WITH DRG FEDERAL REGISTER TABLE LISTING
     C*================================================================
     C     OPT20         BEGSR
     C*
     C*
     C                   MOVEL(P)  'F2521R'      ##PGM
     C                   MOVEL(P)  'FTFEP'       ##FIL1
     C                   MOVEL(P)  'FTFEL'       ##FIL2
     C*** BEG CHG *** 09/25/02 ****************************************
     C******************** MOVEL*BLANKS   ##FIL3
     C                   MOVEL     'FTFEL1'      ##FIL3
     C*** END CHG *** 09/25/02 ****************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 06/08/01 ****************************************
     C*** BEG ADD *** 06/25/01 ****************************************
     C*================================================================
     C* OPT21 - WORK WITH ASC BY REGION/GROUP
     C*================================================================
     C     OPT21         BEGSR
     C*
     C*
     C                   MOVEL(P)  'F2522R'      ##PGM
     C                   MOVEL(P)  'FAAGP'       ##FIL1
     C                   MOVEL(P)  'FAAGL'       ##FIL2
     C*** BEG CHG *** 09/28/04 ****************************************
     C******************** MOVEL*BLANKS   ##FIL3
     C                   MOVEL     'FAAGL1'      ##FIL3
     C*** END CHG *** 09/28/04 ****************************************
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 06/25/01 ****************************************
      ***** BEG ADD ******* 06/28/01 **********************************
      *
      *
      *
      *================================================================
      * OPT22 - WORK WITH SKILLED NURSING TABLE
      *================================================================
     C     OPT22         BEGSR
      *
     C                   MOVEL(P)  'F2523R'      ##PGM
     C                   MOVEL(P)  'FASNP'       ##FIL1
     C                   MOVEL(P)  'FASNL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
      *
     C                   CALL      'F0601C'      ACDPL1
      *
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
      *
     C                   ENDSR                                                  OPT22
      *
      *
      *
      ***** END ADD ******* 06/28/01 **********************************
     C*** BEG ADD *** 06/27/01 ****************************************
     C*================================================================
     C* OPT23 - WORK WITH HOME HEALTH AGENCY TABLE
     C*================================================================
     C     OPT23         BEGSR
     C*
     C*
     C                   MOVEL(P)  'F2524R'      ##PGM
     C                   MOVEL(P)  'FAHHP'       ##FIL1
     C                   MOVEL(P)  'FAAGL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 06/27/01 ****************************************
     C*** BEG ADD *** 06/28/01 ****************************************
     C*================================================================
     C* OPT24 - WORK WITH CHARGE MASTER SERVICE CODE TABLE
     C*================================================================
     C     OPT24         BEGSR
     C*
     C*
     C                   MOVEL(P)  'F2525R'      ##PGM
     C                   MOVEL(P)  'FACGP'       ##FIL1
     C                   MOVEL(P)  'FACGL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 06/28/01 ****************************************
     C*** BEGIN ADD *** 08/01/01 **************************************
     C*================================================================
     C* OPT25 - WORK WITH ASC BY PROVIDERS
     C*================================================================
     C     OPT25         BEGSR
     C*
     C                   MOVEL(P)  'F2526R'      ##PGM
     C                   MOVEL(P)  'FAAPP'       ##FIL1
     C                   MOVEL(P)  'FAAPL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/01/01 **************************************
     C*** BEGIN ADD *** 08/02/01 **************************************
     C*================================================================
     C* OPT26 - WORK WITH ASC BY MSA AND REGION
     C*================================================================
     C     OPT26         BEGSR
     C*
     C                   MOVEL(P)  'F2527R'      ##PGM
     C                   MOVEL(P)  'FAMRP'       ##FIL1
     C                   MOVEL(P)  'FAMRL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/02/01 **************************************
     C*** BEGIN ADD *** 08/02/01 **************************************
     C*================================================================
     C* OPT27 - WORK WITH RCC BY PROVIDER AND REV CODE
     C*================================================================
     C     OPT27         BEGSR
     C*
     C                   MOVEL(P)  'F2528R'      ##PGM
     C                   MOVEL(P)  'FARRP'       ##FIL1
     C                   MOVEL(P)  'FARRL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/02/01 **************************************
     C*** BEGIN ADD *** 08/03/01 **************************************
     C*================================================================
     C* OPT28 - WORK WITH PROVIDER RCC TABLE
     C*================================================================
     C     OPT28         BEGSR
     C*
     C                   MOVEL(P)  'F2529R'      ##PGM
     C                   MOVEL(P)  'FAPHP'       ##FIL1
     C                   MOVEL(P)  'FAPHL'       ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/02/01 **************************************
     C*** BEGIN ADD *** 08/08/01 **************************************
     C*================================================================
     C* OPT29 - WORK WITH CORF TABLE
     C*================================================================
     C     OPT29         BEGSR
     C*
     C                   MOVEL(P)  'F2530R'      ##PGM
     C                   MOVEL(P)  'FACRP'       ##FIL1
     C                   MOVEL(P)  'FACRL'       ##FIL2
     C                   MOVEL     'FACRL1'      ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/08/01 **************************************
     C*** BEGIN ADD *** 08/17/01 **************************************
     C*================================================================
     C* OPT30 - WORK WITH FEIN/MED# TABLE
     C*================================================================
     C     OPT30         BEGSR
     C*
     C***** BEG ADD *** 06/23/16 *******************************************
     C                   IF        scst <> 'NY'
     C***** END ADD *** 06/23/16 *******************************************
     C                   MOVEL(P)  'F2531R'      ##PGM
     C                   MOVEL(P)  'FMFMP'       ##FIL1
     C                   MOVEL(P)  'FMFML1'      ##FIL2
     C                   MOVEL     'FMFML2'      ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C***** BEG ADD *** 06/23/16 *******************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  'F2556R'      ##PGM
     C                   MOVEL(P)  'FMFMP'       ##FIL1
     C                   MOVEL(P)  'FMFML1'      ##FIL2
     C                   MOVEL     'FMFML4'      ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C                   ENDIF
     C***** END ADD *** 06/23/16 *******************************************
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 08/17/01 **************************************
     C*** BEGIN ADD *** 01/08/02 **************************************
     C*================================================================
     C* OPT31 - WORK WITH PROVIDER DRG RATES
     C*================================================================
     C     OPT31         BEGSR
     C*
     C                   MOVEL(P)  'F0048R'      ##PGM
     C                   MOVEL(P)  'FMP2P'       ##FIL1
     C                   MOVEL(P)  'FMP2L '      ##FIL2
     C                   MOVEL     'FMP2L1'      ##FIL3
     C                   MOVEL(P)  'FMP2L2'      ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 01/08/02 **************************************
     C*** BEGIN ADD *** 09/27/02 **************************************
     C*================================================================
     C* OPT32 - WORK WITH DRG FACTORS AND CCR'S
     C*================================================================
     C     OPT32         BEGSR
     C*
     C                   MOVEL(P)  'F2535R'      ##PGM
     C                   MOVEL(P)  'FADFP '      ##FIL1
     C                   MOVEL(P)  'FADFL '      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/27/02 **************************************
     C*** BEGIN ADD *** 04/07/03 **************************************
     C*================================================================
     C* OPT33 - WORK WITH PHYSICAL THERAPY PER VISIT RATES
     C*================================================================
     C     OPT33         BEGSR
     C*
     C                   MOVEL(P)  'F2536R'      ##PGM
     C                   MOVEL(P)  'FAPTP '      ##FIL1
     C                   MOVEL(P)  'FAPTL '      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 04/07/03 **************************************
     C*** BEGIN ADD *** 09/07/04 **************************************
     C*================================================================
     C* OPT34 - WORK WITH ASC'S BY PAS GROUP
     C*================================================================
     C     OPT34         BEGSR
     C*
     C                   MOVEL(P)  'F2537R'      ##PGM
     C                   MOVEL(P)  'FAPGP '      ##FIL1
     C                   MOVEL(P)  'FAPGL '      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT35 - WORK WITH HOSPIATAL RATES
     C*================================================================
     C     OPT35         BEGSR
     C*
     C                   MOVEL(P)  'F2540R'      ##PGM
     C                   MOVEL(P)  'FAHRP '      ##FIL1
     C                   MOVEL(P)  'FAHRL '      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT36 - WORK WITH EXEMPT UNITS/HOSPITALS
     C*================================================================
     C     OPT36         BEGSR
     C*
     C                   MOVEL(P)  'F2538R'      ##PGM
     C                   MOVEL(P)  'FMEXP '      ##FIL1
     C                   MOVEL(P)  'FMEXL '      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT37 - WORK WITH DRG'S
     C*================================================================
     C     OPT37         BEGSR
     C*
     C                   MOVEL(P)  'F2539R'      ##PGM
     C                   MOVEL(P)  'FTDGP '      ##FIL1
     C                   MOVEL(P)  'FTDGL '      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/07/04 **************************************
     C*** BEGIN ADD *** 02/26/07 **************************************
     C*================================================================
     C* OPT38 - WORK WITH MEDICARE RATES
     C*================================================================
     C     OPT38         BEGSR
     C*
     C                   MOVEL(P)  'F2545R'      ##PGM
     C                   MOVEL(P)  'FMMCP '      ##FIL1
     C                   MOVEL(P)  'FMMCL1'      ##FIL2
     C                   MOVEL     'FMMCL2'      ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 02/26/07 **************************************
     C*** BEG ADD *** 04/19/10 ****************************************
     C*================================================================
     C* OPT39 - WORK WITH APR DRG'S
     C*================================================================
     C     OPT39         BEGSR
     C*
     C                   MOVEL(P)  'F2541R'      ##PGM
     C                   MOVEL(P)  'FTADP '      ##FIL1
     C                   MOVEL(P)  'FTADL1'      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT40 - WORK WITH INPATIENT CASE PAYMENT RATES
     C*================================================================
     C     OPT40         BEGSR
     C*
     C                   MOVEL(P)  'F2542R'      ##PGM
     C                   MOVEL(P)  'FAIAP '      ##FIL1
     C                   MOVEL(P)  'FAIAL1'      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT41 - WORK WITH INPATIENT EXEMPT UNIT RATES
     C*================================================================
     C     OPT41         BEGSR
     C*
     C                   MOVEL(P)  'F2543R'      ##PGM
     C                   MOVEL(P)  'FTEMP '      ##FIL1
     C                   MOVEL(P)  'FTEML1'      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C*
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 04/19/10 ****************************************
     C*** BEG ADD *** 02/21/12 ****************************************
     C*================================================================
     C* OPT42 - ZIP TO REGION FOR HOSPITAL
     C*================================================================
     C     OPT42         BEGSR
     C*
     C     SCST          IFEQ      'IL'
     C                   CALL      'F2548R'      HNHPL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT43 - ZIP TO REGION FOR NON-HOSPITAL
     C*================================================================
     C     OPT43         BEGSR
     C*
     C     SCST          IFEQ      'IL'
     C                   CALL      'F2549R'      HNHPL
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 02/21/12 ****************************************
     C*** BEG ADD *** 06/25/13 ****************************************
     C*================================================================
     C* OPT44 - W/W NY Co morbidity Factors
     C*================================================================
     C     OPT44         BEGSR
     C*
     C                   MOVEL(P)  'F2551R'      ##PGM
     C                   MOVEL(P)  'FTCTP '      ##FIL1
     C                   MOVEL(P)  'FTCTL1'      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C*
     C                   CALL      'F0601C'      ACDPL1
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*** END ADD *** 06/25/13 ****************************************
     C*** BEG ADD *** 06/14/13 ****************************************
     C*================================================================
     C* OPT45 - W/W ME DRG Rates
     C*================================================================
     C     OPT45         BEGSR
     C*
     C                   MOVEL(P)  'F2552R'      ##PGM
     C                   MOVEL(P)  'FTDGP '      ##FIL1
     C                   MOVEL(P)  'FTDGL1'      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C
     C                   CALL      'F0601C'      ACDPL1
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT46 - W/W OPPS APC Rates
     C*================================================================
     C     OPT46         BEGSR
     C*
     C                   MOVEL(P)  'F2553R'      ##PGM
     C                   MOVEL(P)  'FTAPP '      ##FIL1
     C                   MOVEL(P)  'FTAPL1'      ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C
     C                   CALL      'F0601C'      ACDPL1
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C
     C                   ENDSR
     C*** END ADD *** 06/14/13 ****************************************
     C*** Beg Add *** 12/29/14 ****************************************
     C*================================================================
     C* OPT47 - FL County/Rate
     C*================================================================
     C     OPT47         BEGSR
     C*
     C                   MOVEL(P)  'F2555R'      ##PGM
     C                   MOVEL(P)  'ftctyp'      ##FIL1
     C                   MOVEL(P)  'ftctyl1'     ##FIL2
     C                   MOVEL     *BLANKS       ##FIL3
     C                   MOVEL(P)  *BLANKS       ##FIL4
     C                   MOVEL     SCST          ##ST
     C
     C                   CALL      'F0601C'      ACDPL1
     C     ##CALL        IFEQ      'F'
     C                   SETON                                        7290
     C                   ENDIF
     C
     C                   ENDSR
     C*** End Add *** 12/29/14 ****************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '13'          OPT13
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '15'          OPT15
      *** BEG ADD *** 10/01/98 ****************************************
     C     SCSEL         CASEQ     '16'          OPT16
      *** END ADD *** 10/01/98 ****************************************
      *** BEG ADD *** 03/17/1999 **************************************
     C     SCSEL         CASEQ     '17'          OPT17
      *** END ADD *** 03/17/1999 **************************************
     C*** BEG ADD *** 09/20/00 ****************************************
     C     SCSEL         CASEQ     '18'          OPT18
     C*** END ADD *** 09/20/00 ****************************************
     C*** BEG ADD *** 06/08/01 ****************************************
     C     SCSEL         CASEQ     '19'          OPT19
     C     SCSEL         CASEQ     '20'          OPT20
     C*** END ADD *** 06/08/01 ****************************************
     C*** BEG ADD *** 06/25/01 ****************************************
     C     SCSEL         CASEQ     '21'          OPT21
     C*** END ADD *** 06/25/01 ****************************************
      ***** BEG ADD ******* 06/28/01 **********************************
     C     SCSEL         CASEQ     '22'          OPT22                          SKILLED NURSING
      ***** END ADD ******* 06/28/01 **********************************
     C*** BEG ADD *** 06/27/01 ****************************************
     C     SCSEL         CASEQ     '23'          OPT23
     C*** END ADD *** 06/27/01 ****************************************
     C*** BEG ADD *** 06/28/01 ****************************************
     C     SCSEL         CASEQ     '24'          OPT24
     C*** END ADD *** 06/28/01 ****************************************
     C*** BEGIN ADD *** 08/01/01 **************************************
     C     SCSEL         CASEQ     '25'          OPT25
     C*** END   ADD *** 08/01/01 **************************************
     C*** BEGIN ADD *** 08/02/01 **************************************
     C     SCSEL         CASEQ     '26'          OPT26
     C*** END   ADD *** 08/02/01 **************************************
     C*** BEGIN ADD *** 08/02/01 **************************************
     C     SCSEL         CASEQ     '27'          OPT27
     C*** END   ADD *** 08/02/01 **************************************
     C*** BEGIN ADD *** 08/02/01 **************************************
     C     SCSEL         CASEQ     '28'          OPT28
     C*** END   ADD *** 08/02/01 **************************************
     C*** BEGIN ADD *** 08/08/01 **************************************
     C     SCSEL         CASEQ     '29'          OPT29
     C*** END   ADD *** 08/08/01 **************************************
     C*** BEGIN ADD *** 08/16/01 **************************************
     C     SCSEL         CASEQ     '30'          OPT30
     C*** END   ADD *** 08/16/01 **************************************
     C*** BEGIN ADD *** 01/08/02 **************************************
     C     SCSEL         CASEQ     '31'          OPT31
     C*** END   ADD *** 01/08/02 **************************************
     C*** BEGIN ADD *** 09/27/02 **************************************
     C     SCSEL         CASEQ     '32'          OPT32
     C*** END   ADD *** 09/27/02 **************************************
     C*** BEGIN ADD *** 04/07/03 **************************************
     C     SCSEL         CASEQ     '33'          OPT33
     C*** END   ADD *** 04/07/04 **************************************
     C*** BEGIN ADD *** 09/07/04 **************************************
     C     SCSEL         CASEQ     '34'          OPT34
     C     SCSEL         CASEQ     '35'          OPT35
     C     SCSEL         CASEQ     '36'          OPT36
     C     SCSEL         CASEQ     '37'          OPT37
     C*** END   ADD *** 09/07/04 **************************************
     C*** BEGIN ADD *** 02/26/07 **************************************
     C     SCSEL         CASEQ     '38'          OPT38
     C*** END   ADD *** 02/26/07 **************************************
     C*** BEG ADD *** 04/19/10 **************************************
     C     SCSEL         CASEQ     '39'          OPT39
     C     SCSEL         CASEQ     '40'          OPT40
     C     SCSEL         CASEQ     '41'          OPT41
     C*** END ADD *** 04/19/10 **************************************
     C*** BEG ADD *** 02/21/12 **************************************
     C     SCSEL         CASEQ     '42'          OPT42
     C     SCSEL         CASEQ     '43'          OPT43
     C*** END ADD *** 02/21/12 **************************************
     C*** BEG ADD *** 06/25/13 **************************************
     C     SCSEL         CASEQ     '44'          OPT44
     C*** END ADD *** 06/25/13 **************************************
     C*** BEG ADD *** 06/14/13 **************************************
     C     SCSEL         CASEQ     '45'          OPT45
     C     SCSEL         CASEQ     '46'          OPT46
     C*** END ADD *** 06/14/13 **************************************
     C*** Beg Add *** 12/29/14 ****************************************
     C     SCSEL         CASEQ     '47'          OPT47
     C*** End Add *** 12/29/14 ****************************************
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
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*** BEGIN DEL *** 02/15/01 **************************************
     C*================================================================
     C* STVLSL - State Select and Validate to get state specified code.
     C*================================================================
     C********** STVLSL    BEGSR
     C*
     C* VALID INSTALLED STATE MUST BE SELECTED BEFORE YOU CAN DO SEARCH
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C**********           MOVELSCST      QQSTT
     C*
     C**********           CALL 'CHKSTR'               99
     C**********           PARM           QQSTT   2
     C**********           PARM           QQSTRT  1
     C**********           PARM           QQFLRT  3
     C*
     C********** *IN99     IFEQ '0'
     C*
     C********** QQSTRT    IFEQ '0'
     C*
     C**********           SETON                     7190
     C*
     C**********           ENDIF                           END LOKUP ST
     C*
     C**********           ELSE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C*
     C********** FBKY      CHAINSFB01                99
     C*
     C********** *IN99     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C**********           Z-ADD1         X       20
     C********** SCST      LOKUPST,X                     99
     C*
     C********** *IN99     IFEQ '0'
     C*
     C**********           SETON                     7190
     C*
     C**********           ENDIF                           *IN99=*OFF
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ENDIF                           *IN99=*OFF
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C**********           ENDIF                           *IN99=*ON
     C*
     C**********           ENDSR
     C*** END   DEL *** 02/15/01 **************************************
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*** Beg Add *** 02/11/16 ********************************************
     C                   IF        %parms > 5
     C                   IF        @@adstate = 'A'and ST1A2 = 'Y'
     C                   EVAL      MEETSS = *OFF
     C                   Endif
     C                   Endif
     C*** End Add *** 02/11/16 ********************************************
     C*
     C                   ENDSR
** Column headings
State    Name
** Description--------------MISN
1011=Select                 NNY
1066=Proc codes             YYN
1077=Proc Code Mod          YYN
1088=Service codes          YYN
1099=Body areas             YYN
11010=EOB codes             YYN
11111=Zip/Regions           YYN
11212=Type of Fac codes     YYN
11313=County codes          NNN
11414=Type of Service codes YYN
11515=Place of Service codesYYN
11616=EOMB Comments         YYN
11717=T/P Component Table   YYN
11818=Provider Lic#         YYN
11919=DRG Pros Pay          YYN
12020=DRG Fed Reg           YYN
12121=ASC by region/group   YYN
12222=Skilled Nursing       YYN
12323=Home Health           YYN
12424=Charge Master         YYN
12525=ASC by Provider       YYN
12626=ASC by MSA and Region NNN
12727=RCC by Revenue Code   YYN
12828=Pharmacy RCC Table    YYN
12929=CORF Table            YYN
13030=Provider Table        YYN
13131=Provider DRG Rates    YYN
13232=DRG Factors and CCR's YYN
13333=Phys Ther Per Visit   YYN
13434=ASC's by PAS Group    YYN
13535=Hospital Rates        YYN
13636=Exempt Units/HospitalsYYN
13737=DRG's                 YYN
13838=Medicare Rates        YYN
13939=APR DRG'S             YYN
14040=Inpat Case Pay Rates  YYN
14141=Inpat Exempt Unit RateYYN
14242=Zip to Rgn Hospital   YYN
14343=Zip to Rgn Non-Hosp   YYN
14444=NY Comorbidity Fact   YYN
14545=ME DRG Rates          YYN
14646=OPPS APC Rates        YYN
14747=FL County/Rates       YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
212F12=Cancel               NNY
3
