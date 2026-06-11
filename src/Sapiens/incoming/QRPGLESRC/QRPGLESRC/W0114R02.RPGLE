     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) option(*nodebugio:*srcstmt)
     H BndDir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0114R02 - STATISTICAL REPORTING/RATING MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE STAT RPT/RATING FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0014R02 - STAT RPT/RATING DEFINTION MAINTENANCE           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 01/02/01  PROGRAMMER: C:65  PROJECT: WC1024         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/18/19 C:298 Support: 83030                                 *
     H*                PEO policy type not getting saved due to       *
     H*                improper profound conversion. Corrected        *
     H*                field on screen.                               *
     H*                                                               *
     H* 04/26/19 C:136 Project: SY406                                 *
     H*                Add 'Exclude State from Connect' flag          *
     H*                                                               *
     H* 01/14/18 C:136 Project: 150WCJ                                *
     H*                Add Overseeing PEO Named Insured field for     *
     H*                Sunz                                           *
     H*                                                               *
     H* 08/03/17 C:65  Project: 150WAR                                *
     F*                Add checkboxs for including assessments in     *
     H*                targeted premium.                              *
     H*                                                               *
     H* 08/01/17 C:62  Project: 150WAN                                *
     H*                Add PEO Policy Type                            *
     H*                                                               *
     H* 02/24/17 C:65  Project: WC2480                                *
     H*                Add field for holding the PEO acronym for      *
     H*                output modes blank and 3.  Default to 'L/C/F'  *
     H*                and require an entry if blank or 3 is chosen.  *
     H*                                                               *
     H* 05/11/15 C:279 Project: WC2361                                *
     H*                Added Exclude F-Class flag on all accessments. *
     H*                                                               *
     H* 01/14/14 C:136 Project: WC2034                                *
     H*                Add new value '4' to PEO output flag           *
     H*                                                               *
     A* 02/08/13 C:279 Project: SY266 Transform                       *
     A*                Initialize check box with value 'N'            *
     A*                                                               *
     A* 05/26/11 C:191 Project: WC2038                                *
     A*                Add new flag for each assessment for incresead *
     A*                limits                                         *
     A*                                                               *
     A* 06/24/10 C:227 Project: WC1873                                *
     A*              - Add fields "Include terrorism charges in assmnt*
     A*                calc" for all seven assessment calculations.   *
     A*                                                               *
     H* 04/23/10 C:61  PROJECT WC1880                                 *
     H*              - Add field SCR04 for entry of page segment used *
     H*                for signature on Certificate of Insurance.     *
     H*              - Add field SCR04 for entry of overlay used
     H*                on Certificate of Insurance.
     H*              - Add search on SCCEOV field used for entry of   *
     H*                overlay used to print Certificate of Insurance.*
     H*                                                               *
     H* 04/28/09  C:191 PROJECT WC1794                                *
     H*                 Add THE ENTRIES FOR NCCI RISK ID              *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*              - Convert to RPGIV (comments removed).           *
     H*              - Increase to 5 screens.                         *
     H*              - Implement procedure for editing.               *
     H*              - Add 6th and 7th assessments.                   *
     H*              - "PEO ouput control" to SCR05.                  *
     H*                                                               *
     H* 11/03/07 C:61  PROJECT WC1610                                 *
     H*               - Move "Fifth assessment" and "PEO ouput        *
     H*                 control" fields to SCR03.                     *
     H*               - Move "Third assessment" fields to SCR02.      *
     H*               - Add prompts for "Apply to discount on         *
     H*                 deductible policies only (Y/ )" for           *
     H*                 assessments 1 thu 5.                          *
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Move assessment by installment.                *
     H*                                                               *
     H* 12/08/05 C:61  PROJECT WC1407 139WBU                          *
     H*                Add "Dec Page / Alternate Employer Control     *
     H*                (1/2/3/ )" prompt to screen 2.                 *
     H*                                                               *
     H*  07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                   *
     H*                 Add code for 4th and 5th assessments.         *
     H*                                                               *
     H*  03/26/01 C:65 PROJECT: 107BH                                 *
     H*                ADD STAT CODE ENTRIES FOR THE ASSESSMENTS.     *
     H*                                                               *
     H*  02/16/01 C:65 PROJECT: WC1025                                *
     H*                ADD OPTION FOR THE ASSESSMENTS TO BE APPLIED   *
     H*                TO 'X' LEVEL.                                  *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD                                                     *
     H* N40 - UPDATE/INQUIRY                                          *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  51-55 3RD ASSESSMENT                                         *
     H*  58-68 1ST & 2ND ASSESSMENT                                   *
     H*  70-74 BILL BY INSTALLMENTS                                   *
     H*                                                               *
     H*****************************************************************
     FW0114F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDRRP     UF   E             DISK
     F                                     RENAME(PRR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
      * Generic Table File
     FWTGTL     IF   E           K DISK
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*
     D OPTNM           C                   CONST('Statistical Report-
     D                                     ing/Rating')
     D #OFSCR          s              1  0 inz(4)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     DCheckAssmts      pr             1n
     D #onScreen                      1  0              const
     D percentage                          like(RR1STP) const
     D fClass                              like(RR1STF) const
     D level                               like(RR1STL) const
     D billFlag                            like(RR1STB) const
     D statCode                            like(RR1STC) const
     D incDiscFlag                         like(RR1IDD) const
     D incDevFlag                          like(RR1IDV) const
     D applyDedFlag                        like(RR1ATD) const
     D inclTerr                            like(RR1ATR) const
     D incrLmtsFlg                         like(RR1ILF) const
       //*** beg add ***** 04/26/19 ****************
      /Copy SprModule
       //*** end add ***** 04/26/19 ****************
     C/COPY SCOPYMT05C
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    RRFND         XXFND
     C     *LIKE         DEFINE    RRCO#         XXCO#
     C     *LIKE         DEFINE    RRST          XXST
     C     *LIKE         DEFINE    RREFDT        XXEFDT
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *================================================================
      *  KLIST
      *================================================================
      *
      *  GENERIC TABLE
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      ***** END ADD ******* 04/23/10 **********************************
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  STAT RPT/RATING DEFINITION FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
      *
      *================================================================
      *  PLIST
      *================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C* 1st Assessment
     C  n90              eval      *in90 = CheckAssmts(1
     C                                                :SC1STP
     C                                                :SC1STF
     C                                                :SC1STL
     C                                                :SC1STB
     C                                                :SC1STC
     C                                                :SC1IDD
     C                                                :SC1IDV
     C                                                :SC1ATD
     C                                                :SC1ATR
     C                                                :SC1ILF
     C                                                )
     C*
     C* 2nd Assessment
     C  n90              eval      *in90 = CheckAssmts(2
     C                                                :SC2NDP
     C                                                :SC2NDF
     C                                                :SC2NDL
     C                                                :SC2NDB
     C                                                :SC2NDC
     C                                                :SC2IDD
     C                                                :SC2IDV
     C                                                :SC2ATD
     C                                                :SC2ATR
     C                                                :SC2ILF
     C                                                )
     C*
      /free
       er1xfc = *off;
       er2xfc = *off;

       If sc1xfc = 'Y' and sc1stf <> 0;
          er1xfc = *on;
          *in90  = *on;
       Endif;

       If sc2xfc = 'Y' and sc2ndf <> 0;
          er2xfc = *on;
          *in90  = *on;
       Endif;
      /end-free

     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C     ERRCK2        BEGSR
     C*
     C* 3rd Assessment
     C  n90              eval      *in90 = CheckAssmts(1
     C                                                :SC3RDP
     C                                                :SC3RDF
     C                                                :SC3RDL
     C                                                :SC3RDB
     C                                                :SC3RDC
     C                                                :SC3IDD
     C                                                :SC3IDV
     C                                                :SC3ATD
     C                                                :SC3ATR
     C                                                :SC3ILF
     C                                                )
     C*
     C* 4th Assessment
     C  n90              eval      *in90 = CheckAssmts(2
     C                                                :SC4THP
     C                                                :SC4THF
     C                                                :SC4THL
     C                                                :SC4THB
     C                                                :SC4THC
     C                                                :SC4IDD
     C                                                :SC4IDV
     C                                                :SC4ATD
     C                                                :SC4ATR
     C                                                :SC4ILF
     C                                                )
     C*
      /free
       er3xfc = *off;
       er4xfc = *off;

       If sc3xfc = 'Y' and sc3rdf <> 0;
          er3xfc = *on;
          *in90  = *on;
       Endif;

       If sc4xfc = 'Y' and sc4thf <> 0;
          er4xfc = *on;
          *in90  = *on;
       Endif;
      /end-free

     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C* 5th Assessment
     C  n90              eval      *in90 = CheckAssmts(1
     C                                                :SC5THP
     C                                                :SC5THF
     C                                                :SC5THL
     C                                                :SC5THB
     C                                                :SC5THC
     C                                                :SC5IDD
     C                                                :SC5IDV
     C                                                :SC5ATD
     C                                                :SC5ATR
     C                                                :SC5ILF
     C                                                )
     C*
     C* 6th Assessment
     C  n90              eval      *in90 = CheckAssmts(2
     C                                                :SC6THP
     C                                                :SC6THF
     C                                                :SC6THL
     C                                                :SC6THB
     C                                                :SC6THC
     C                                                :SC6IDD
     C                                                :SC6IDV
     C                                                :SC6ATD
     C                                                :SC6ATR
     C                                                :SC6ILF
     C                                                )
     C*
      /free
       er5xfc = *off;
       er6xfc = *off;

       If sc5xfc = 'Y' and sc5thf <> 0;
          er5xfc = *on;
          *in90  = *on;
       Endif;

       If sc6xfc = 'Y' and sc6thf <> 0;
          er6xfc = *on;
          *in90  = *on;
       Endif;
      /end-free

     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04
     C*===============================================================
     C     ERRCK4        BEGSR
     C*
     C* 7th Assessment
     C  n90              eval      *in90 = CheckAssmts(1
     C                                                :SC7THP
     C                                                :SC7THF
     C                                                :SC7THL
     C                                                :SC7THB
     C                                                :SC7THC
     C                                                :SC7IDD
     C                                                :SC7IDV
     C                                                :SC7ATD
     C                                                :SC7ATR
     C                                                :SC7ILF
     C                                                )
     C*
      /free
       er7xfc = *off;

       If sc7xfc = 'Y' and sc7thf <> 0;
          er7xfc = *on;
          *in90  = *on;
       Endif;
      /end-free

      *
     C  N90SCSRRB        IFNE      *BLANK
     C     SCSRRB        ANDNE     'Y'
     C                   MOVE      *ON           *in31
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C  N90SCSRRQ        IFNE      *BLANK
     C     SCSRRQ        ANDNE     'Y'
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C  N90SCNRRB        IFNE      *BLANK
     C     SCNRRB        ANDNE     'Y'
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C  N90SCNRRQ        IFNE      *BLANK
     C     SCNRRQ        ANDNE     'Y'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *
      *  CERT OF INS - OVERLAY - MUST EXIST ON SYSTEM
      *
     C  N90              IF        (scceov <> *blanks)
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCCEOV        #2NAME           10
     C                   PARM      '*OVL   '     #2OTYP            7
     C                   PARM      *BLANK        #2ERR             1
     C                   IF        #2err = '1'
     c                   eval      *in87 = *on
     c                   eval      *in90 = *on
     C                   endif
     C                   endif
      *
      *  CERT OF INS - OVERLAY - MUST EXIST IN SYSTEM TABLE FILE 'COVL'
      *                          IF ENTERED
      *
     C  N90              IF        (scceov <> *blanks)
     C                   MOVEL(p)  'COVL'        KKTBID
     C                   MOVEL(p)  SCCEOV        KKKEY
     C     GTKY          CHAIN     PGT01
     c                   if        not %found
     c                   eval      *in88 = *on
     c                   eval      *in90 = *on
     C                   endif
     C                   endif
      *
      *  CERT OF INS - PAGSEG MUST EXIST ON SYSTEM
      *
     C  N90              IF        scspsg <> *BLANKS
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCSPSG        #2NAME           10
     C                   PARM      '*PAGSEG'     #2OTYP            7
     C                   PARM      *BLANK        #2ERR             1
     C                   IF        #2err = '1'
     c                   eval      *in89 = *on
     c                   eval      *in90 = *on
     C                   ENDIF
     C                   ENDIF
      *
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK4
     C*
     C*===============================================================
     C* ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR05
     C*===============================================================
     C     ERRCK5        BEGSR
      *
      * Dec Page / Alternate Employer Control (1/2/3/ )
      *
      /free
       Select;
       When *in90 = *on;
       When scPgfl <> ' ' and scPgfl <> '1' and scPgfl <> '2'
        and scPgfl <> '3' and scPgfl <> '4';
          *in83 = *on;
          *in90 = *on;
       Endsl;
       //*** begin add ***  02/24/17  ******************************************
       Clear erPEOA;
       Clear msPEOA;
       If (scPgfl = '3' or scPgfl = ' ') and scPEOA = ' ';
          *in90 = *on;
          erPEOA = *on;
          msPEOA = 'An acronym must be entered for output control 3 or blank.';
       Endif;
       //*** end add *****  02/24/17  ******************************************
      /end-free
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
      *
      ****************************************************************
      *  KJDO - Process F10 "Table Search" requests.                 *
      ****************************************************************
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * CERT OF INS OVERLAY
      *
     C                   when      hfld = 'SCCEOV'
     C                   CALL      'W2493R'
     C                   PARM      *blanks       #1PRDS           40
     C                   PARM      *BLANK        #1CALL            1
     C                   PARM      'COVL'        #1ID              4
     C                   PARM      *BLANKS       #1KEY            10
     C                   PARM      *BLANK        #1RTC             1
      *
     C                   if        #1key = *BLANKS
     c                   eval      *in85 = *on
     c                   else
     c                   eval      *in86 = *on
     C                   MOVEL(p)  #1KEY         SCCEOV
     C                   endif
      *
     C                   when      hfld = 'SC1ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in61 = *on
     c                   else
     c                   eval      *in51 = *on
     C                   movel(p)  #1key         sc1atr
     C                   endif
      *
     C                   when      hfld = 'SC2ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in62 = *on
     c                   else
     C                   movel(p)  #1key         sc2atr
     C                   endif
      *
     C                   when      hfld = 'SC3ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in63 = *on
     c                   else
     c                   eval      *in53 = *on
     C                   movel(p)  #1key         sc3atr
     C                   endif
      *
     C                   when      hfld = 'SC4ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in64 = *on
     c                   else
     C                   movel(p)  #1key         sc4atr
     C                   endif
      *
     C                   when      hfld = 'SC5ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in65 = *on
     c                   else
     c                   eval      *in55 = *on
     C                   movel(p)  #1key         sc5atr
     C                   endif
      *
     C                   when      hfld = 'SC6ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in66 = *on
     c                   else
     C                   movel(p)  #1key         sc6atr
     C                   endif
      *
     C                   when      hfld = 'SC7ATR'
     C*
     C                   exsr      getter
     C*
     C                   if        #1key = *blanks
     c                   eval      *in67 = *on
     c                   else
     c                   eval      *in57 = *on
     C                   movel(p)  #1key         sc7atr
     C                   endif
      *
     c                   endsl
      *
     C                   ENDSR
     C*
     C****************************************************************
     C*  GETTER  - Search for Terrorism type                         *
     C****************************************************************
     C     GETTER        BEGSR
     C*
     C                   CALL      'W2493R'
     C                   PARM      *blanks       #1PRDS           40
     C                   PARM      *BLANK        #1CALL            1
     C                   PARM      'TERA'        #1ID              4
     C                   PARM      *BLANKS       #1KEY            10
     C                   PARM      *BLANK        #1RTC             1
     C*
     C                   ENDSR
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      RRST          SCST
     C                   Z-ADD     RREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     RR1STP        SC1STP
     C                   Z-ADD     RR1STF        SC1STF
     C                   MOVE      RR1STL        SC1STL
     C                   MOVE      RR1STB        SC1STB
     C                   MOVE      RR2NDB        SC2NDB
     C                   MOVE      RR3RDB        SC3RDB
     C                   MOVEL(P)  RR1STH        SC1STH
     C                   Z-ADD     RR1STC        SC1STC
     C                   Z-ADD     RR2NDC        SC2NDC
     C                   Z-ADD     RR3RDC        SC3RDC
     C                   Z-ADD     RR2NDP        SC2NDP
     C                   Z-ADD     RR2NDF        SC2NDF
     C                   MOVE      RR2NDL        SC2NDL
     C                   MOVEL(P)  RR2NDH        SC2NDH
     C                   Z-ADD     RR3RDP        SC3RDP
     C                   Z-ADD     RR3RDF        SC3RDF
     C                   MOVE      RR3RDL        SC3RDL
     C                   MOVEL(P)  RR3RDH        SC3RDH
     C                   Z-ADD     RR4THP        SC4THP
     C                   Z-ADD     RR4THF        SC4THF
     C                   MOVE      RR4THL        SC4THL
     C                   MOVEL(P)  RR4THH        SC4THH
     C                   Z-ADD     RR4THC        SC4THC
     C                   Z-ADD     RR5THP        SC5THP
     C                   Z-ADD     RR5THF        SC5THF
     C                   MOVE      RR5THL        SC5THL
     C                   MOVE      RR4THB        SC4THB
     C                   MOVE      RR5THB        SC5THB
     C                   MOVEL(P)  RR5THH        SC5THH
     C                   Z-ADD     RR5THC        SC5THC
     C                   Z-ADD     RR6THP        SC6THP
     C                   Z-ADD     RR6THF        SC6THF
     C                   MOVE      RR6THL        SC6THL
     C                   MOVEL(P)  RR6THH        SC6THH
     C                   Z-ADD     RR6THC        SC6THC
     C                   MOVE      RR6THB        SC6THB
     C                   Z-ADD     RR7THP        SC7THP
     C                   Z-ADD     RR7THF        SC7THF
     C                   MOVE      RR7THL        SC7THL
     C                   MOVE      RR7THB        SC7THB
     C                   MOVEL(P)  RR7THH        SC7THH
     C                   Z-ADD     RR7THC        SC7THC
      *
     C                   MOVE      RRPGFL        SCPGFL
     C*** begin add ***  02/24/17  ******************************************
     C                   Move      RRPEOA        scPEOA
     C                   If        scPEOA = ' '
     C                   Eval      scPEOA = 'L/C/F'
     C                   Endif
     C*** end add *****  02/24/17  ******************************************
     C*** Begin Add *** 08/01/17 ********************************************
     C                   Move      RRpeoF        scPeoPT
     C*** End   Add *** 08/01/17 ********************************************
      *
     C                   MOVE      RR1ATD        SC1ATD
     C                   MOVE      RR2ATD        SC2ATD
     C                   MOVE      RR3ATD        SC3ATD
     C                   MOVE      RR4ATD        SC4ATD
     C                   MOVE      RR5ATD        SC5ATD
     C                   MOVE      RR6ATD        SC6ATD
     C                   MOVE      RR7ATD        SC7ATD
     C                   MOVEL(P)  RR1ATR        SC1ATR
     C                   MOVEL(P)  RR2ATR        SC2ATR
     C                   MOVEL(P)  RR3ATR        SC3ATR
     C                   MOVEL(P)  RR4ATR        SC4ATR
     C                   MOVEL(P)  RR5ATR        SC5ATR
     C                   MOVEL(P)  RR6ATR        SC6ATR
     C                   MOVEL(P)  RR7ATR        SC7ATR
     C                   MOVE      RR1ILF        SC1ILF
     C                   MOVE      RR2ILF        SC2ILF
     C                   MOVE      RR3ILF        SC3ILF
     C                   MOVE      RR4ILF        SC4ILF
     C                   MOVE      RR5ILF        SC5ILF
     C                   MOVE      RR6ILF        SC6ILF
     C                   MOVE      RR7ILF        SC7ILF
     C                   MOVE      RRSRRB        SCSRRB
     C                   MOVE      RRSRRQ        SCSRRQ
     C                   MOVE      RRNRRB        SCNRRB
     C                   MOVE      RRNRRQ        SCNRRQ
     C                   MOVEL(p)  RRCEOV        SCCEOV
     C                   MOVEL(p)  RRSPSG        SCSPSG
     C                   IF        RR1IDD = ' '
     C                   EVAL      SC1IDD = 'N'
     C                   ELSE
     C                   MOVE      RR1IDD        SC1IDD
     C                   ENDIF

     C                   IF        RR1IDV = ' '
     C                   EVAL      SC1IDV = 'N'
     C                   ELSE
     C                   MOVE      RR1IDV        SC1IDV
     C                   ENDIF

     C                   IF        RR2IDD = ' '
     C                   EVAL      SC2IDD = 'N'
     C                   ELSE
     C                   MOVE      RR2IDD        SC2IDD
     C                   ENDIF

     C                   IF        RR2IDV = ' '
     C                   EVAL      SC2IDV = 'N'
     C                   ELSE
     C                   MOVE      RR2IDV        SC2IDV
     C                   ENDIF

     C                   IF        RR3IDD = ' '
     C                   EVAL      SC3IDD = 'N'
     C                   ELSE
     C                   MOVE      RR3IDD        SC3IDD
     C                   ENDIF

     C                   IF        RR3IDV = ' '
     C                   EVAL      SC3IDV = 'N'
     C                   ELSE
     C                   MOVE      RR3IDV        SC3IDV
     C                   ENDIF

     C                   IF        RR4IDD = ' '
     C                   EVAL      SC4IDD = 'N'
     C                   ELSE
     C                   MOVE      RR4IDD        SC4IDD
     C                   ENDIF

     C                   IF        RR4IDV = ' '
     C                   EVAL      SC4IDV = 'N'
     C                   ELSE
     C                   MOVE      RR4IDV        SC4IDV
     C                   ENDIF

     C                   IF        RR5IDD = ' '
     C                   EVAL      SC5IDD = 'N'
     C                   ELSE
     C                   MOVE      RR5IDD        SC5IDD
     C                   ENDIF

     C                   IF        RR5IDV = ' '
     C                   EVAL      SC5IDV = 'N'
     C                   ELSE
     C                   MOVE      RR5IDV        SC5IDV
     C                   ENDIF

     C                   IF        RR6IDD = ' '
     C                   EVAL      SC6IDD = 'N'
     C                   ELSE
     C                   MOVE      RR6IDD        SC6IDD
     C                   ENDIF

     C                   IF        RR6IDV = ' '
     C                   EVAL      SC6IDV = 'N'
     C                   ELSE
     C                   MOVE      RR6IDV        SC6IDV
     C                   ENDIF

     C                   IF        RR7IDD = ' '
     C                   EVAL      SC7IDD = 'N'
     C                   ELSE
     C                   MOVE      RR7IDD        SC7IDD
     C                   ENDIF

     C                   IF        RR7IDV = ' '
     C                   EVAL      SC7IDV = 'N'
     C                   ELSE
     C                   MOVE      RR7IDV        SC7IDV
     C                   ENDIF
     C*** begin add *** 08/03/17 ***********************************
     C                   Move      RR1stM        sc1stM
     C                   Move      RR2ndM        sc2ndM
     C                   Move      RR3rdM        sc3rdM
     C                   Move      RR4thM        sc4thM
     C                   Move      RR5thM        sc5thM
     C                   Move      RR6thM        sc6thM
     C                   Move      RR7thM        sc7thM
     C*** end add ***** 08/03/17 ***********************************
     C*
      /free
       er1xfc = *off;
       er2xfc = *off;
       er3xfc = *off;
       er4xfc = *off;
       er5xfc = *off;
       er6xfc = *off;
       er7xfc = *off;

       sc1xfc = RR1xfc;
       sc2xfc = RR2xfc;
       sc3xfc = RR3xfc;
       sc4xfc = RR4xfc;
       sc5xfc = RR5xfc;
       sc6xfc = RR6xfc;
       sc7xfc = RR7xfc;
       //*** beg add ***** 01/14/18 ****************
       scNmInDft = RRNmInDft;
       //*** end add ***** 01/14/18 ****************
       //*** beg add ***** 04/26/19 ****************
       scExSt = RRexSt;
       //*** end add ***** 04/26/19 ****************
      /end-free


     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SC1STP        RR1STP
     C                   Z-ADD     SC1STF        RR1STF
     C                   MOVE      SC1STL        RR1STL
     C                   MOVE      SC1STB        RR1STB
     C                   MOVE      SC2NDB        RR2NDB
     C                   MOVE      SC3RDB        RR3RDB
     C                   MOVEL(P)  SC1STH        RR1STH
     C                   MOVE      SC1IDD        RR1IDD
     C                   MOVE      SC1IDV        RR1IDV
     C                   Z-ADD     SC1STC        RR1STC
     C                   Z-ADD     SC2NDC        RR2NDC
     C                   Z-ADD     SC3RDC        RR3RDC
     C                   Z-ADD     SC2NDP        RR2NDP
     C                   Z-ADD     SC2NDF        RR2NDF
     C                   MOVE      SC2NDL        RR2NDL
     C                   MOVEL(P)  SC2NDH        RR2NDH
     C                   MOVE      SC2IDD        RR2IDD
     C                   MOVE      SC2IDV        RR2IDV
     C                   Z-ADD     SC3RDP        RR3RDP
     C                   Z-ADD     SC3RDF        RR3RDF
     C                   MOVE      SC3RDL        RR3RDL
     C                   MOVEL(P)  SC3RDH        RR3RDH
     C                   MOVE      SC3IDD        RR3IDD
     C                   MOVE      SC3IDV        RR3IDV
     C                   Z-ADD     SC4THP        RR4THP
     C                   Z-ADD     SC4THF        RR4THF
     C                   MOVE      SC4THL        RR4THL
     C                   MOVE      SC4THB        RR4THB
     C                   MOVE      SC5THB        RR5THB
     C                   MOVEL(P)  SC4THH        RR4THH
     C                   MOVE      SC4IDD        RR4IDD
     C                   MOVE      SC4IDV        RR4IDV
     C                   Z-ADD     SC4THC        RR4THC
     C                   Z-ADD     SC5THP        RR5THP
     C                   Z-ADD     SC5THF        RR5THF
     C                   MOVE      SC5THL        RR5THL
     C                   MOVEL(P)  SC5THH        RR5THH
     C                   MOVE      SC5IDD        RR5IDD
     C                   MOVE      SC5IDV        RR5IDV
     C                   Z-ADD     SC5THC        RR5THC
     C                   Z-ADD     SC6THP        RR6THP
     C                   Z-ADD     SC6THF        RR6THF
     C                   MOVE      SC6THL        RR6THL
     C                   MOVE      SC6THB        RR6THB
     C                   MOVEL(P)  SC6THH        RR6THH
     C                   MOVE      SC6IDD        RR6IDD
     C                   MOVE      SC6IDV        RR6IDV
     C                   Z-ADD     SC6THC        RR6THC
     C                   Z-ADD     SC7THP        RR7THP
     C                   Z-ADD     SC7THF        RR7THF
     C                   MOVE      SC7THL        RR7THL
     C                   MOVEL(P)  SC7THH        RR7THH
     C                   MOVE      SC7IDD        RR7IDD
     C                   MOVE      SC7IDV        RR7IDV
     C                   Z-ADD     SC7THC        RR7THC
     C                   MOVE      SC7THB        RR7THB
      *
     C                   MOVE      SCPGFL        RRPGFL
     C*** begin add ***  02/24/17  ******************************************
     C                   Move      scPEOA        RRPEOA
     C*** end add *****  02/24/17  ******************************************
     C*** Begin Add *** 08/01/17 ********************************************
     C                   Move      scPeoPT       RRpeoF
     C*** End   Add *** 08/01/17 ********************************************
      *
     C                   MOVE      SC1ATD        RR1ATD
     C                   MOVE      SC2ATD        RR2ATD
     C                   MOVE      SC3ATD        RR3ATD
     C                   MOVE      SC4ATD        RR4ATD
     C                   MOVE      SC5ATD        RR5ATD
     C                   MOVE      SC6ATD        RR6ATD
     C                   MOVE      SC7ATD        RR7ATD
     C                   MOVEL(P)  SC1ATR        RR1ATR
     C                   MOVEL(P)  SC2ATR        RR2ATR
     C                   MOVEL(P)  SC3ATR        RR3ATR
     C                   MOVEL(P)  SC4ATR        RR4ATR
     C                   MOVEL(P)  SC5ATR        RR5ATR
     C                   MOVEL(P)  SC6ATR        RR6ATR
     C                   MOVEL(P)  SC7ATR        RR7ATR
     C                   MOVE      SC1ILF        RR1ILF
     C                   MOVE      SC2ILF        RR2ILF
     C                   MOVE      SC3ILF        RR3ILF
     C                   MOVE      SC4ILF        RR4ILF
     C                   MOVE      SC5ILF        RR5ILF
     C                   MOVE      SC6ILF        RR6ILF
     C                   MOVE      SC7ILF        RR7ILF
     C                   MOVE      SCSRRB        RRSRRB
     C                   MOVE      SCSRRQ        RRSRRQ
     C                   MOVE      SCNRRB        RRNRRB
     C                   MOVE      SCNRRQ        RRNRRQ
     C                   MOVEL(p)  SCCEOV        RRCEOV
     C                   MOVEL(p)  SCSPSG        RRSPSG
     C*** begin add *** 08/03/17 ***********************************
     C                   Move      sc1stM        RR1stM
     C                   Move      sc2ndM        RR2ndM
     C                   Move      sc3rdM        RR3rdM
     C                   Move      sc4thM        RR4thM
     C                   Move      sc5thM        RR5thM
     C                   Move      sc6thM        RR6thM
     C                   Move      sc7thM        RR7thM
     C*** end add ***** 08/03/17 ***********************************
     C*
      /free
       RR1xfc = sc1xfc;
       RR2xfc = sc2xfc;
       RR3xfc = sc3xfc;
       RR4xfc = sc4xfc;
       RR5xfc = sc5xfc;
       RR6xfc = sc6xfc;
       RR7xfc = sc7xfc;
       //*** beg add ***** 01/14/18 ****************
       RRNmInDft = scNmInDft;
       //*** end add ***** 01/14/18 ****************
       //*** beg add ***** 04/26/19 ****************
       RRexSt = scExSt;
       //*** end add ***** 04/26/19 ****************
      /end-free
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
      *
     C     *inkj         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVE      *OFF          *IN92
     C                   MOVE      *OFF          *IN40
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'P'
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  @@MODE EQ A
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FND         SCFND
     C                   MOVE      RRST          XXST
     C                   Z-ADD     RREFDT        XXEFDT
       //*** begin add *** 08/03/17 ***********************************
     C                   Call      'RTVCLTID'
     C                   Parm                    cltID             3
      /free
       If cltID = '150';
          vis150 = *On;
       Else;
          vis150 = *Off;
       Endif;
      /end-free
       //*** end add ***** 08/03/17 ***********************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
      *
      * PEO Group
      *
     C     FNPEOF        COMP      *BLANK                             2121
     C                   if        FNPEOF <> *blank
     C                   eval      #ofScr = 5
     C*** Begin Add *** 08/01/17 **************************************
     C     FNpeoF        Comp      'C'                                    22
     C*** End   Add *** 08/01/17 **************************************
     C                   endif

     C*** beg add ***** 01/14/18 ***************************
      /Free
       If cltID = '150' and FNpeof <> *Blank and FNpeof <> 'S';
          is150PEO = *On;
          scNmInSlct = 'ANCO# = ' + %CHAR(FNCO#) +
           ' AND ANFND = ' + %CHAR(FNFND) +
           ' AND ANEMP# = 1 AND ANDIV = 0 AND ANSTAT = ''A''';
       Else;
          is150PEO = *Off;
       Endif;
       //*** beg add ***** 04/26/19 ****************
       hasConnect = IsModEnabled('Portal Module');
       If hasConnect = *On;
          #OfScr = 5;
       EndIf;
       //*** end add ***** 04/26/19 ****************
      /End-Free
     C*** end add ***** 01/14/18 ***************************

     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   EXSR      MVDBSC
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RRUSER
     C                   MOVEL     Q1PGM         RRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C                   MOVEL     DSTIME        RRTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER





     P*===============================================================
     P* Process assessment fields
     P*===============================================================
     PCheckAssmts      b
     DCheckAssmts      pi             1n
     D #onScreen                      1  0              const
     D percentage                          like(RR1STP) const
     D fClass                              like(RR1STF) const
     D level                               like(RR1STL) const
     D billFlag                            like(RR1STB) const
     D statCode                            like(RR1STC) const
     D incDiscFlag                         like(RR1IDD) const
     D incDevFlag                          like(RR1IDV) const
     D applyDedFlag                        like(RR1ATD) const
     D inclTerr                            like(RR1ATR) const
     D incrLmtsFlg                         like(RR1ILF) const

     C                   eval      *in(80 + #onScreen) = *on
     C*
     C                   if        percentage < *zero
     C                   eval      *in71 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        fClass < *zero
     C                   eval      *in72 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        percentage > *zero
     C*
     C                   if        level <> 'J'
     C                             and level <> 'E'
     C                             and level <> 'S'
     C                             and level <> 'D'
     C                             and level <> 'X'
     C                   eval      *in73 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        billFlag <> 'Y'
     C                             and billFlag <> ' '
     C                   eval      *in74 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        incDiscFlag <> 'Y'
     C                             and incDiscFlag <> 'N'
     C                   eval      *in76 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        incDevFlag <> 'Y'
     C                             and incDevFlag <> 'N'
     C                   eval      *in77 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*

     C                   endif                                                  SC1STP IFGT
     C
     C*
     C                   if        statCode < *zero
     C                   eval      *in75 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        applyDedFlag <> 'Y'
     C                             and applyDedFlag <> ' '
     C                   eval      *in78 = *on
     C                   return    *on
     C                   endif                                                  SC1STP IFLT
     C*
     C                   if        inclTerr <> ' '
     C                   movel(p)  'TERA'        kktbid
     C                   movel(p)  inclTerr      kkkey
     C     gtky          chain     pgt01                              99
     C     *in99         ifeq      *on
     C                   eval      *in79 = *on
     C                   return    *on
     C                   endif                                                  *in99
     C                   endif                                                  inclTerr
     C
     C                   if        incrLmtsFlg <> ' '
     C                             and incrLmtsFlg <>'I'
     C                             and incrLmtsFlg <>'W'
     C                   eval      *in80 = *on
     C                   return    *on
     C                   endif                                                  incrLmtsFlg
     C                   eval      *in81 = *off
     C                   eval      *in82 = *off
     C*
     C                   return    *off

     PCheckAssmts      e
