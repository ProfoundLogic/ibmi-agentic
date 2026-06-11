     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7092R - SINGLE CASE TRANSFER/DELETION UPDATE/DISPLAY
     H*****************************************************************
     H* DESCRIPTION: CASE TRANSFER/DELETION INFORMATION PROGRAM
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                                                               *
     H*  10/20/03 C:53 Project: MC213                                 *
     H*                Added country address info.                    *
     H*                                                               *
     H*  11/19/99 C:31  Project: MC151
     H*                 Claim Transfer/Deletion
     H*                 1. Compile only
     H*
     H*  11/30/98  C:94  PROJECT: MCMNT
     H*                  CHANGED LENGTH OF PARM
     H*
     H*  11/25/98  C:94  PROJECT: SUP15132
     H*                  DO NOT MAKE EMPLOYER REQUIRED, BUT IF
     H*                  ENTERED THEN USE CO/SUB/FND/EMP/DIV AND
     H*                  VALIDATE AGAINST FMEMP
     H*
     H*****************************************************************
     H*
     FF7092F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFTGTP     IF   E             DISK
     F*
     FSTSTP     IF   E             DISK
     F*
     FFMEML     IF   E           K DISK
     F*
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FFMFNL     IF   E           K DISK
     F*
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     F*
     FFMSML     IF   E           K DISK
     F*
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     F*  Physical file definition (for maintaining record)
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FFMCDL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Single Case Transfer-
     D                                     /Deletion Informatio-
     D                                     n')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN1            9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@TD              1
     C                   PARM                    @@FLAG            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN1            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM                    ##CO#             3 0
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM      SCEMP#        ##EMP#            9 0
     C                   PARM      SCDIV         ##DIV             5 0
     C                   PARM      SCSS#         ##SS#             9
     C*** BEG CHG *** 11/30/98 ***************************************
     C                   PARM      SCACDT        ##ACDT            8 0
     C**********           PARM SCACDT    ##ACDT  70
     C*** END CHG *** 11/30/98 ***************************************
     C                   PARM      SCLSN         ##LSN            30
     C                   PARM      SCFSN         ##FSN            15
     C                   PARM      SCMID         ##MID             1
     C                   PARM      SCADR1        ##ADR1           40
     C                   PARM      SCADR2        ##ADR2           40
     C                   PARM      SCCTY         ##CTY            25
     C                   PARM      SCST          ##ST              2
     C                   PARM      CDZIP         ##ZIP             9 0
     C                   PARM      SCDOB         ##DOB             7 0
     C                   PARM      SCSEX         ##SEX             1
     C                   PARM      @@FLAG        ##FLAG            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    SCSUB#        KKSUB#
     C     *LIKE         DEFINE    SCFND         KKFND
     C     *LIKE         DEFINE    SCEMP#        KKEMP#
     C     *LIKE         DEFINE    SCDIV         KKDIV
     C*
     C/COPY SCOPYMT01C
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
     C*
     C     *INKL         IFEQ      *OFF
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':scSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   CALL      'F7093R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           *IN90
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   SETON                                        9071
     C                   GOTO      ENDCHK
     C                   ELSE
     C                   MOVE      SCSUB#        KKSUB#
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9062
     C                   GOTO      ENDCHK
     C                   ELSE
     C                   MOVEL(P)  SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVEL     SCFND         KKFND
     C*
     C     SCFND         IFEQ      *ZEROS
     C                   SETON                                        9072
     C                   GOTO      ENDCHK
     C                   ELSE
     C                   MOVEL     SCSUB#        KKSUB#
     C                   MOVEL     SCFND         KKFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9063
     C                   GOTO      ENDCHK
     C                   ELSE
     C                   MOVEL(P)  FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG DEL *** 11/25/98 ****************************************
     C********** SCEMP#    IFEQ *ZEROS
     C**********           SETON                     9073
     C**********           GOTO ENDCHK
     C**********           ENDIF
     C*** END DEL *** 11/25/98 ****************************************
     C*** BEG ADD *** 11/25/98 ****************************************
     C*
     C     SCEMP#        IFNE      *ZEROS
     C                   MOVEL     SCSUB#        KKSUB#
     C                   MOVEL     SCFND         KKFND
     C                   MOVEL     SCEMP#        KKEMP#
     C                   MOVEL     SCDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      ENDCHK
     C                   ELSE
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/25/98 ****************************************
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':scSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C     SCSS#         IFEQ      *BLANKS
     C                   SETON                                        9074
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCACDT        IFEQ      *ZEROS
     C                   SETON                                        9075
     C                   ELSE
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT            7
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7
     C                   ENDIF
     C*
     C     XXACDT        IFGT      XXDATE
     C                   SETON                                        9076
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCLSN         IFEQ      *BLANKS
     C                   SETON                                        9077
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCFSN         IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCADR1        IFEQ      *BLANKS
     C                   SETON                                        9079
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C*** BEG ADD *** 10/20/03 **********************
     C     CDCTRY        IFEQ      'USA'
     C*** END ADD *** 10/20/03 **********************
     C*
     C     SCCTY         IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCST          IFEQ      *BLANKS
     C                   SETON                                        9081
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCZIP1        IFEQ      *ZEROS
     C                   SETON                                        9082
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C*** BEG ADD *** 10/20/03 **********************
     C                   ENDIF
     C*** END ADD *** 10/20/03 **********************
     C*
     C     SCSEX         IFEQ      *BLANK
     C                   SETON                                        9083
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     SCDOB         IFNE      *ZEROS
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDOB             7
     C     XXDOB         IFGT      XXACDT
     C                   SETON                                        9084
     C                   GOTO      ENDCHK
     C                   ENDIF
     C                   ENDIF
     C*
     C     ENDCHK        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*
     C*** SUBSCRIBER SEARCH
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##RRN1            9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN1        OREQ      *ZEROS
     C                   SETON                                        8590
     C                   ELSE
     C*
     C     ##RRN1        CHAIN     PSM02                              99
     C                   MOVEL     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   SETON                                        8690
     C                   ENDIF
     C*
     C*** FUND SEARCH ***
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          WHENEQ    'SCFND'
     C*****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM      '0'           ##KC              1
     C******INKL         PARM      '0'           ##KL              1
     C*****              PARM                    ##RRN2            9 0
     C*****
     C******INKC         IFEQ      *ON
     C*****              MOVE      '1'           OUTFLG
     C*****              ENDIF
     C*****
     C******INKL         IFEQ      *ON
     C*****##RRN2        OREQ      *ZEROS
     C*****              SETON                                        8690
     C*****              ELSE
     C*****
     C*****##RRN2        CHAIN     PFN02                              99
     C*****              MOVEL     FNFND         SCFND
     C*****              MOVEL(P)  FNNAME        SCFNAM
     C*****              SETON                                        8790
     C*****              ENDIF
     C******** End Del ***** 12/18/24 ***************************
     C*
     C*
     C*** EMPLOYER SEARCH ***
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C*
     C                   CALL      'F0140R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *ZEROS        ##RRN4            9 0
     C                   PARM      *ZEROS        ##VIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN3        OREQ      *ZEROS
     C                   SETON                                        8790
     C                   ELSE
     C*
     C     ##RRN3        CHAIN     PEM02                              99
     C                   MOVEL     EMEMP#        SCEMP#
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   SETON                                        8890
     C                   ENDIF
     C*
     C*
     C*** STATE SEARCH ***
     C*
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        8990
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C                   MOVEL(P)  STKEY         SCST
     C                   SETON                                        9060
     C                   ENDIF
     C*
     C*
     C*** GENDER SEARCH ***
     C*
     C     HFLD          WHENEQ    'SCSEX'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'GNDR'        ##TBID            4
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        6190
     C                   ELSE
     C*
     C                   MOVEL(P)  CMCO#         KKCO#
     C                   MOVEL(P)  'GNDR'        KKTBID            4
     C                   MOVEL(P)  SCSEX         KKTKEY           10
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCSEX
     C*
     C                   SETON                                        9085
     C*
     C*
     C*          ##RRN     CHAINPST01                99
     C*                    MOVELSTKEY     SCST      P
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      *ZEROS        KKCO#
     C                   MOVE      *ZEROS        KKSUB#
     C                   MOVE      *ZEROS        KKFND
     C                   MOVE      *ZEROS        KKEMP#
     C                   MOVE      *ZEROS        KKDIV
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVE      *BLANKS       SCSNAM
     C                   MOVE      *BLANKS       SCFNAM
     C                   MOVE      *BLANKS       SCENAM
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVEL     *ZEROS        SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C*
     C     @@MODE        IFNE      'A'                                          NOT ADD MODE
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     CMCO#         ##CO#
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         ##FYR
     C                   Z-ADD     CMCASE        ##CASE
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   MOVEL(P)  CMDIV         SCDIV
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL(P)  CMSS#         KKSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C                   MOVEL(P)  CMMID         SCMID
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL(P)  SMNAME        SCSNAM
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   MOVEL(P)  FNNAME        SCFNAM
     C*
     C     CMEMP#        IFNE      *ZEROS
     C*
     C                   MOVEL     CMSUB#        KKSUB#
     C                   MOVEL     CMCO#         KKCO#
     C                   MOVEL     CMFND         KKFND
     C                   MOVEL     CMEMP#        KKEMP#
     C                   MOVEL     CMDIV         KKDIV
     C     EMKY          CHAIN     PEM01                              99
     C                   MOVEL(P)  EMNAME        SCENAM
     C*
     C                   ENDIF
     C*
     C     CDKY          CHAIN     PCD01                              99
     C                   MOVE      CDADR1        SCADR1
     C                   MOVE      CDADR2        SCADR2
     C                   MOVE      CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZIP1
     C                   MOVE      CDZIP         SCZIP2
     C*** BEG ADD *** 10/20/03 **********************
     C     CDCTRY        COMP      'USA'                              0505
     C*** END ADD *** 10/20/03 **********************
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOB
     C                   MOVE      CDSEX         SCSEX
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
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
     C                   SETOFF                                       6061
     C                   SETOFF                                       6263
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       1617
     C     @@TD          IFEQ      'D'
     C                   SETON                                        17
     C                   ELSE
     C                   SETON                                        16
     C                   ENDIF
     C*
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C     *INKL         IFEQ      *OFF
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':scSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   CALL      'F7093R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   MOVEL     *ON           *IN90
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMCMP
     C                   ENDSR
