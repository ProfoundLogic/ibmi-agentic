     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0263R - WORK INFORMATION
     H*******************************************************************
     H*  F0263R - Work Information        C:62 03/24/98               *
     H*                                                               *
     H*  CALLED FROM: F0269R                                          *
     H*  CALLS TO   : FA701R                                          *
     H*               FA702R                                          *
     H*               FA703R                                          *
     H*               F0176R                                          *
     H*               F2173R                                          *
     H*               F2380R                                          *
     H*               SA001R                                          *
     H*               W0070R5                                         *
     H*               W4315R                                          *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/22/11 C:079 PROJECT 114MYS
     H*                 FOR NON-WORK COMP GROUPS DONT REQUIRE WORK
     H*                 HISTORY RECORD
     H*                                                               *
     H*  09/11/06 C:163 PROJECT 114WMF
     H*                 BACK TO BASE - MEDICAL CASE INFORMATION(114MAC
     H*                                AND SUPPORT MC114 31204)
     H*
     H*  02/11/02 C:62  Project: MC114 31204
     H*                 ALLOW F12 IN DISPLAY MODE EVEN IF THERE ISN'T
     H*                 ANY WORK HISTORY RECORDS.
     H*
     H*  01/25/01 C:62  Project: 114MAC
     H*                 KEEP USER FROM LEAVING THIS SCREEN IF THERE IS
     H*                 NO WORK HISTORY RECORD.
     H*
     H*  03/22/01 C:62  Project: MCMNT
     H*                 ADD ADDITIONAL ENTRY PARM FOR CALL AND USE IT
     H*                 WITH CALL OF F0176R.
     H*                                                               *
     H*  05/05/00 C:62  Project: MCMNT
     H*                 MODIFY THE WAY THE FIELDS WILL BE
     H*                                                               *
     H*  10/13/99 C:31  Project: MC214
     H*                 Work Hist usage w/WC
     H*                 1. Add call to SA009R
     H*                 2. Add test of WHPG parm to function 7
     H*                                                               *
     H* 06/17/99 C:53  PROJECT: MC145                                 *
     H*                QUALIFY WCMS CALLS W/INSTALL & NON-PDS FLAGS   *
     H*                                                               *
     H* 10/21/98 C:94  PROJECT: 87MFX                                 *
     H*                ADDED PARMS TO WA024R                          *
     H*                                                               *
     H* 10/07/98 C:94  PROJECT: 87MFX                                 *
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL              *
     H*                                                               *
     H*                                                               *
     H* 10/06/98 C:94  PROJECT: 87MFX                                 *
     H*                CHANGED @@WCUP TO LOOK FOR A 'Y' INSTEAD OF    *
     H*                BLANK                                          *
     H*                                                               *
     H* 09/29/98 C:94  PROJECT: 87MFX                                 *
     H*                ADDED PARMS TO WA024R                          *
     H*                                                               *
     H* 09/21/98 C:94  PROJECT: 87MFX                                 *
     H*                UPDATE WORKER'S COMP FIELDS                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  LINKED/STANDALONE       : 16/N16                             *
     H*  EDI INSTALLED/NOT INST. : 17/N17                             *
     H*  DISPLAY MODE/CHANGE MODE: 09/N09                             *
     H*  PLACE CURSOR INDICATORS : 31-33                              *
     H*  ERROR INDICATORS        : 71-76,89                           *
     H*                                                               *
     H*****************************************************************
     FF0263F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FFAWHL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFTDLL     IF   E           K DISK
     FFTWPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      /COPY SCOPYMT01D
     D*
     D FN1             S              1    DIM(78)                              Func L1 S1
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***PY SCOPYMT01I
     D*
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Work Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D F3              C                   CONST('F3=Exit')
     D F5              C                   CONST('F5=Refresh')
     D F7              C                   CONST('F7=Work History')
     D F10             C                   CONST('F10=Search')
     D F12             C                   CONST('F12=Cancel  ')
     D F14             C                   CONST('F14=DARTS')
     D F18             C                   CONST('F18=EDI')
     D START           C                   CONST('Start losing time')
     D RTURNS          C                   CONST('Returned to work')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FMCMP
     C*** BEGIN ADD *** 09/11/06 **************************************
     C                   PARM                    @@CONT            1
     C*** END   ADD *** 09/11/06 **************************************
     C*** BEGIN ADD *** 03/22/01 **************************************
     C                   PARM                    @@CALL            1
     C*** END   ADD *** 03/22/01 **************************************
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* Claim master key.
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* Work History file key
     C*
     C     WHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     WHKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKWKDT
     C*
     C* Generic table file key
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C* Level of duty file key
     C*
     C     DLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C* WCMC Work table file key.
     C*
     C     WPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*** Key list for fund master file.
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    WHWKDT        KKWKDT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    WPKEY         ##WKEY
     C     *LIKE         DEFINE    WPDESC        ##DESC
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    CMHIDT        XXHIDT
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CRTFN1 - SUBROUTINE FOR CREATING FUNCTION LINES FOR SCR01.
     C*================================================================
     C*
     C     CRTFN1        BEGSR
     C*
     C                   SETOFF                                       18
     C                   MOVE      *BLANKS       FN1
     C*
     C                   MOVEA     F3            FN1(1)
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C                   MOVEA     F5            FN1(11)
     C                   MOVEA     F7            FN1(24)
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   MOVEA     F10           FN1(42)
     C                   MOVEA     F12           FN1(55)
     C                   MOVEA     F14           FN1(70)
     C*
     C                   ELSE
     C*
     C                   MOVEA     F12           FN1(42)
     C                   MOVEA     F14           FN1(57)
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F18           FN1(69)
     C*
     C                   ENDIF                                                  *IN17
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ELSE
     C*
     C                   MOVEA     F7            FN1(11)
     C                   MOVEA     F12           FN1(29)
     C                   MOVEA     F14           FN1(44)
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F18           FN1(56)
     C*
     C                   ENDIF                                                  *IN17
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ENDIF                                                  *IN09
     C*
     C                   MOVEA     FN1           SCFNC1
      /If Defined (WEB)
     C                   eval      fl1(1) = scFnc1
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
     C                   Clear                   SCFNC1
      /Endif
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCHIDT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*
     C*EXECUTE WHEN CLIENT IS #114
     C     XXCLT#        IFEQ      '114'
     C*
     C     Q1PRMS        IFGT      4
     C     @@CONT        ANDEQ     'Y'
     C*
     C                   CALL      'F0275R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '1'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP
     C                   PARM      @@RRN         ##RRN2            9 0          FMCMP
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   SETOFF                                         KL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C* HIRE DATE MUST BE VALID DATE AND >= CMACDT IF NOT ZERO.
     C*
     C  N90SCHIDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCHIDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXHIDT
     C*
     C                   ENDIF
     C*
     C* EMPLOYMENT STATUS MUST BE VALID IF NOT BLANK.
     C*
     C  N90SCESTS        IFNE      *BLANKS
     C*
     C                   MOVEL     'ESTS'        KKTBID
     C                   MOVEL(P)  SCESTS        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* DAYS/WEEK CAN'T BE GREATER THAN SEVEN.
     C*
     C  N90SCDYWK        IFGT      7
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* HOURS/DAY CAN'T BE GREATER THAN 24.
     C*
     C  N90SCHRDY        IFGT      24
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* SHIFT MUST BE VALID IF NOT BLANK.
     C*
     C  N90SCWKPT        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCWKPT        KKKEY
     C*
     C     WPKY          CHAIN     PWP01                              99
     C*
     C   99              SETON                                        7690
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CMDT          IFGT      SAVEDT
     C     CMDT          OREQ      SAVEDT
     C     CMTIME        ANDGT     SAVETM
     C*
     C                   SETON                                        9089
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*
     C*EXECUTE WHEN CLIENT IS #114
     C*
     C     XXCLT#        IFEQ      '114'
     C*
     C     *IN90         IFEQ      *OFF
     C     STANDA        ANDEQ     '*YES'
     C*** BEGIN ADD *** 06/22/11 **************************************
     C     FNNWCG        ANDNE     'Y'
     C*** END   ADD *** 06/22/11 **************************************
     C     *IN90         OREQ      *OFF
     C     @@WHPG        ANDEQ     'M'
     C*** BEGIN ADD *** 06/22/11 **************************************
     C     FNNWCG        ANDNE     'Y'
     C*** END   ADD *** 06/22/11 **************************************
     C*THERE MUST BE A WORK HISTORY RECORD TO GO TO NEXT SCREEN
     C     WHKY          CHAIN     PWH01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETOFF                                       19
     C                   ELSE
     C                   SETON                                        779019
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
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
     C* KGDO - F7=Work History
     C*================================================================
     C     KGDO          BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C*** BEGIN DEL *** 03/22/01 **************************************
     C********** @@MODE    IFEQ 'D'
     C*
     C**********           MOVEL'1'       ##CALL
     C*
     C**********           ELSE
     C*
     C**********           MOVEL' '       ##CALL
     C*
     C**********           ENDIF
     C*** END   DEL *** 03/22/01 **************************************
     C*** BEGIN ADD *** 03/22/01 **************************************
     C     Q1PRMS        IFGT      4
     C                   MOVEL     @@CALL        ##CALL
     C                   ELSE
     C                   MOVEL     '1'           ##CALL
     C                   ENDIF
     C*** END   ADD *** 03/22/01 **************************************
     C*
     C*** BEGIN CHG *** 05/05/00 **************************************
     C********** *IN16     IFEQ *OFF
     C     STANDA        IFEQ      '*YES'
     C*** END   CHG *** 05/05/00 **************************************
     C*** BEGIN ADD *** 10/13/99 **************************************
     C     @@WHPG        OREQ      'M'
     C*** END ADD *** 10/13/99 ****************************************
     C*
     C                   CALL      'F0176R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C*** BEGIN CHG *** 03/22/01 **************************************
     C**********           PARM *BLANKS   ##CALL  1
     C                   PARM                    ##CALL            1
     C*** END   CHG *** 03/22/01 **************************************
     C                   PARM      *ZEROS        ##RRN             9 0          RRN FAWHP
     C                   PARM      @@RRN         ##RRN2            9 0          RRN FMCMP
     C*
     C                   ELSE
     C*
     C     ##WCIF        IFEQ      'Y'                                          ADD 06/17/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/17/99
     C*
     C                   CALL      'W4315R'
     C                   PARM      OPTNM         ##PRDS           40
     C*** BEGIN CHG *** 03/22/01 **************************************
     C**********           PARM *BLANKS   ##CALL
     C                   PARM                    ##CALL
     C*** END   CHG *** 03/22/01 **************************************
     C                   PARM      FNWCF#        ##FND
     C                   PARM      KKFYR         ##FYR
     C                   PARM      KKCASE        ##CASE
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   ENDIF                                                  ADD 06/17/99
     C                   ENDIF
     C*
     C                   SETON                                            90
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C     HFLD          IFEQ      'SCESTS'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '8'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'ESTS'        ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SCESTS
     C                   MOVEL     GTDESC        SCEDSC
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCWKPT'
     C*
     C                   CALL      'F2173R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##WKEY
     C                   PARM      *BLANKS       ##DESC
     C*
     C     *INKL         IFEQ      *ON
     C     ##WKEY        OREQ      *BLANKS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C                   MOVEL     ##WKEY        SCWKPT
     C                   MOVEL     ##DESC        SCSDSC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                            90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS.
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   SETON                                            90
     C*
     C                   ENDSR
     C*================================================================
     C* KSDO - F18=EDI.
     C*================================================================
     C     KSDO          BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C     ##WCIF        IFEQ      'Y'                                          ADD 06/17/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/17/99
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      FNWCF#        #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'W0070R5'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFNE      'A'
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF                                                  ADD 06/17/99
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C     *IN40         IFEQ      *OFF
     C*** BEGIN ADD *** 05/05/00 **************************************
     C     STANDA        IFEQ      '*YES'
     C*** END   ADD *** 05/05/00 **************************************
     C*
     C                   MOVEL     CMOCDS        SCOCDS
     C*
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCHIDT
     C*
     C                   MOVEL     CMESTS        SCESTS
     C                   MOVEL(P)  CMESTS        KKKEY
     C                   MOVEL     'ESTS'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL     GTDESC        SCEDSC
     C   99              MOVEL     *BLANKS       SCEDSC
     C*
     C                   MOVE      CMDYWK        SCDYWK
     C                   MOVE      CMHRDY        SCHRDY
     C                   MOVEL     CMWKPT        SCWKPT
     C                   MOVEL(P)  CMWKPT        KKKEY
     C*
     C     WPKY          CHAIN     PWP01                              99
     C*
     C  N99              MOVEL     WPDESC        SCSDSC
     C   99              MOVEL     *BLANKS       SCSDSC
     C*
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTLW
     C*** BEGIN ADD *** 05/05/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMFND         ##FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C                   MOVE      #1WCF#        ##FND
     C*
     C                   CALL      'WA043R'
     C                   PARM      CMCO#         ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM      CMFYR         ##FYR             3 0
     C                   PARM      CMCASE        ##CASE
     C                   PARM      *BLANKS       ##OCDS           40            Occupation
     C                   PARM      *ZEROS        ##HIDT            7 0          Hire Date
     C                   PARM      *BLANKS       ##ESTS            2            Emplyr Sts
     C                   PARM      *BLANKS       ##EDSC           40            Emplyr Sts Dsc
     C                   PARM      *ZEROS        ##DAYS            1 0          Days/Week
     C                   PARM      *ZEROS        ##HRSD            3 0          Hours/Day
     C                   PARM      *BLANKS       ##SHFT            1            Shift
     C                   PARM      *BLANKS       ##SDSC           40            Shift Desc
     C                   PARM      *ZEROS        ##DTLW            7 0          Dt Last Worked
     C                   PARM      *BLANKS       ##WKST            1            Work Status
     C                   PARM      *ZEROS        ##DATE            7 0          Status Date
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   MOVEL     ##OCDS        SCOCDS
     C*
     C                   Z-ADD     ##HIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCHIDT
     C*
     C                   MOVEL     ##ESTS        SCESTS
     C                   MOVEL(P)  ##EDSC        SCEDSC
     C                   Z-ADD     ##DAYS        SCDYWK
     C                   Z-ADD     ##HRSD        SCHRDY
     C                   MOVEL     ##SHFT        SCWKPT
     C                   MOVEL(P)  ##SDSC        SCSDSC
     C*
     C                   ENDIF
     C*
     C     STANDA        IFEQ      '*YES'
     C     @@WHPG        OREQ      'M'
     C*
     C     WHKY2         SETLL     PWH01
     C     WHKY          READE     PWH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  WHCODE        KKKEY
     C                   MOVEL     'WKCD'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     GTFG02        IFEQ      'L'
     C*
     C                   Z-ADD     WHWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTLW
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     WHKY          READE     PWH01                                  99
     C*
     C                   ENDDO
     C*
     C*** END   ADD *** 05/05/00 **************************************
     C*** BEGIN ADD *** 01/21/00 **************************************
     C     WHKY2         SETLL     PWH01
     C     WHKY          READE     PWH01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 01/21/00 **************************************
     C*
     C                   MOVEL     WHCODE        SCCODE
     C                   MOVEL(P)  WHCODE        KKKEY
     C                   MOVEL     'WKCD'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL     GTDESC        SCCDSC
     C   99              MOVEL     *BLANKS       SCCDSC
     C*
     C                   Z-ADD     WHWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWKDT
     C*
     C                   MOVEL     WHLVL         SCLVL
     C                   MOVEL(P)  WHLVL         KKKEY
     C*
     C     DLKY          CHAIN     PDL01                              99
     C*
     C  N99              MOVEL     DLDESC        SCLDSC
     C   99              MOVEL     *BLANKS       SCLDSC
     C*** BEGIN ADD *** 01/25/01 **************************************
     C                   SETOFF                                       19
     C*** END   ADD *** 01/25/01 **************************************
     C*** BEGIN ADD *** 01/21/00 **************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     *BLANKS       SCCDSC
     C                   Z-ADD     *ZEROS        SCWKDT
     C                   MOVEL     *BLANKS       SCLVL
     C                   MOVEL     *BLANKS       SCLDSC
     C*** BEGIN ADD *** 01/25/01 **************************************
     C*** BEGIN ADD *** 02/11/02 **************************************
     C     @@MODE        IFNE      'D'
     C*** END   ADD *** 02/11/02 **************************************
     C                   SETON                                        19
     C*** BEGIN ADD *** 02/11/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 02/11/02 **************************************
     C*** END   ADD *** 01/25/01 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 01/21/00 **************************************
     C*** BEGIN ADD *** 05/05/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##WKST        SCCODE
     C*
     C                   SELECT
     C*
     C     ##WKST        WHENEQ    'L'
     C                   MOVEL(P)  START         SCCDSC
     C*
     C     ##WKST        WHENEQ    'R'
     C                   MOVEL(P)  RTURNS        SCCDSC
     C*
     C                   ENDSL
     C*
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWKDT
     C*
     C                   Z-ADD     ##DTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTLW
     C*
     C                   ENDIF
     C*** END   ADD *** 05/05/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCOCDS
     C                   Z-ADD     *ZEROS        SCHIDT
     C                   MOVEL     *BLANKS       SCESTS
     C                   MOVEL     *BLANKS       SCEDSC
     C                   MOVE      *ZEROS        SCDYWK
     C                   MOVE      *ZEROS        SCHRDY
     C                   MOVEL     *BLANKS       SCWKPT
     C                   MOVEL     *BLANKS       SCSDSC
     C                   MOVEL     *BLANKS       SCOCDS
     C                   Z-ADD     *ZEROS        SCDTLW
     C                   MOVEL     *BLANKS       SCCODE
     C                   MOVEL     *BLANKS       SCCDSC
     C                   Z-ADD     *ZEROS        SCWKDT
     C                   MOVEL     *BLANKS       SCLVL
     C                   MOVEL     *BLANKS       SCLDSC
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCOCDS        CMOCDS
     C*
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMHIDT
     C*
     C                   MOVEL     SCESTS        CMESTS
     C                   MOVE      SCDYWK        CMDYWK
     C                   MOVE      SCHRDY        CMHRDY
     C                   MOVEL(P)  SCWKPT        CMWKPT
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
     C  NKE              EXSR      DATCVT
     C*
     C     *INKE         CASEQ     *ON           KEDO
     C     *INKG         CASEQ     *ON           KGDO
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C     *INKS         CASEQ     *ON           KSDO
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
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
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
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1            WC INST
     C                   PARM      ' '           ##MCIF            1            MC INST
     C                   PARM      ' '           ##WCIN            1            WC INTERFACE
     C                   PARM      ' '           ##AR              1            MC AR INST
     C                   PARM      ' '           ##NTWK            1            NETWORK INST
     C                   PARM      ' '           ##UR              1            PRECERT INST
     C                   PARM      ' '           ##MED             1            MEDMGMT INST
     C                   PARM      ' '           ##EDI             1            EDI INST
     C                   PARM      ' '           ##WCDK            1            WC DARTS KEY
     C*** BEG ADD *** 06/17/99 **************************************
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 06/17/99 **************************************
     C*** BEGIN ADD *** 10/13/99 **************************************
     C                   CALL      'SA009R'
 1   C                   PARM                    @@EOBD            3 0          EOMB DELETE SECURITY
 2   C                   PARM                    @@EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM                    @@ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM                    @@ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM                    @@PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM                    @@SDTS            1            ALLOW FUTURE SERV DT
 7   C                   PARM                    @@MCIV            1            MED CASES INIT VIEW
 8   C                   PARM                    @@PBIV            1            PROV BILLS ININT VIE
 9   C                   PARM                    @@AINV            1            AUTO INVOICE FLAG
10   C                   PARM                    @@WHPG            1            Work History Usage
     C*** END   ADD *** 10/13/99 **************************************
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*RETRIEVE CLIENT ID#
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     XXCLT#        IFEQ      '114'
     C                   SETON                                        30
     C                   ELSE
     C                   SETOFF                                       30
     C                   ENDIF                                                  XXCLT#=114
     C*
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C     ##EDI         IFEQ      'Y'
     C*
     C                   SETON                                        17
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       17
     C*
     C                   ENDIF
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        #1L1             78
     C     SCL2HD        PARM      *BLANK        #1L2             78
     C*
     C* CALL API TO DETERMINE IF STANDALONE OR LINKED WITH WCMS
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANK        ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'Y'
     C*
     C                   SETOFF                                       16
     C*** BEGIN ADD *** 05/05/00 **************************************
     C                   MOVEL(P)  '*YES'        STANDA            4
     C*** END   ADD *** 05/05/00 **************************************
     C*
     C                   ELSE
     C*
     C                   SETON                                        16
     C*** BEGIN ADD *** 05/05/00 **************************************
     C                   MOVEL(P)  '*NO '        STANDA            4
     C*** END   ADD *** 05/05/00 **************************************
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   Z-ADD     9999999       KKWKDT
     C*** BEG ADD *** 09/21/98 **************************************
     C*
     C*** BEG CHG *** 10/06/98 ***************************************
     C                   MOVE      *BLANK        @@WCUP
     C**********           MOVE 'F'       @@WCUP
     C*** END CHG *** 10/06/98 ***************************************
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C                   CALL      'FA715R'
     C                   PARM                    @@RTCD            1
     C                   PARM      KKCO#         @@CO#             3 0
     C                   PARM      KKSUB#        @@SUB#            5 0
     C                   PARM      KKFND         @@FND             3 0
     C                   PARM                    @@PVMC            1 0          Init Med
     C                   PARM                    @@PVPR            1 0          Init Prov
     C                   PARM                    @@WCUP            1            Allw Upd WCMS Clms
     C                   PARM                    @@CLDL            3 0          Clm Dlt Lvl
     C                   PARM                    @@CLDT            1            Clm Dlt Typ
     C                   PARM                    @@CLTL            3 0          Clm Xfer Lvl
     C                   PARM                    @@CLTT            1            Clm Xfer Typ
     C                   PARM                    @@SDCL            3 0          Settle Dt Lvl
     C                   PARM                    @@SDCT            1            Settle Dt Typ
     C                   PARM                    @@BSCL            3 0          BWC Sts Lvl
     C                   PARM                    @@BSCT            1            BWC Sts Typ
     C                   PARM                    @@DCDL            3 0          Del Diag Cd Lvl
     C                   PARM                    @@DCDT            1            Del Diag Cd Typ
     C                   PARM                    @@DCCL            3 0          Chg Diag St Lvl
     C                   PARM                    @@DCCT            1            Chg Diag St Typ
     C                   PARM                    @@C#CL            3 0          Clt Case Lvl
     C                   PARM                    @@C#CT            1            Clt Case Typ
     C                   PARM                    @@FIMR            5            RMCD: From IMR
     C                   PARM                    @@TIMR            5            RMCD: To IMR
     C                   PARM                    @@FRN             5            RMCD: From RN
     C                   PARM                    @@TRN             5            RMCD: To RN
     C*
     C*** BEG CHG *** 10/06/98 ***************************************
     C     @@WCUP        IFEQ      'Y'
     C********** @@WCUP    IFEQ ' '
     C*** END CHG *** 10/06/98 ***************************************
     C                   SETOFF                                       16
     C                   ENDIF                                                  @@WCUP = Y
     C*
     C                   ENDIF                                                  ##WCIF ##WCIN
     C*
     C                   ENDIF                                                  *IN16 = ON
     C*** END ADD *** 09/21/98 **************************************
     C*
     C* GET WORK HISTORY RECORDS
     C*
     C*** BEGIN DEL *** 01/21/00 **************************************
     C********** WHKY2     SETLLPWH01
     C********** WHKY      READEPWH01                    99
     C*** END   DEL *** 01/21/00 **************************************
     C*
     C* CREATE FUNCTION LINE
     C*
     C                   EXSR      CRTFN1
     C*
     C* GET WORKERS COMP FUND.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
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
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*** BEG ADD *** 09/21/98 ***************************************
     C*
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1HIDT
     C*
     C*** BEG CHG *** 10/06/98 ***************************************
     C     @@WCUP        IFEQ      'Y'
     C     ##WCIF        ANDEQ     'Y'                                          ADD 06/17/99
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/17/99
     C********** @@WCUP    IFEQ ' '
     C*** END CHG *** 10/06/98 ***************************************
     C                   CALL      'WA024R'
     C                   PARM      *BLANK        @1RTCD            1            WCMS fields
     C                   PARM                    @1CO#             3 0          CO#
     C                   PARM                    @1SUB#            5 0          -----------
     C                   PARM                    @1FND             3 0          FND
     C                   PARM                    @1FYR             3 0          FYR
     C                   PARM                    @1CASE            7 0          CASE
     C                   PARM                    @1EMP#            9 0          EMP#
     C                   PARM                    @1DIV             5 0          DIV
     C                   PARM                    @1ACDT            7 0          ACDT
     C                   PARM                    @1ACDS           60            ACDS
     C*** BEG ADD *** 10/21/98 ****************************************
     C                   PARM                    @1ACD2           60            ACDS
     C*** END ADD *** 10/21/98 ****************************************
     C                   PARM                    @1ACTM            6 0          ACTM
     C                   PARM                    @1LTFL            1            LTFL
     C                   PARM                    @1CLAD            3            CLEX
     C                   PARM                    @1CLSU            3            SUPR
     C                   PARM                    @1CLFA            3            FEAD
     C                   PARM                    @1MCON            1            MCON
     C                   PARM                    @1RCVD            7 0          RCVD
     C                   PARM                    @1DTRP            7 0          DTRP
     C                   PARM                    @1DTRE            7 0          DTRE
     C                   PARM                    @1DBDT            7 0          DBDT
     C                   PARM                    @1ENTD            7 0          ENTD
     C                   PARM                    @1DEDT            7 0          DOD
     C                   PARM                    @1MCO             1            MCO
     C                   PARM                    @1PREX            1            PREX
     C                   PARM                    @1ODIS            1            ODIS
     C*** BEG ADD *** 09/29/98 ****************************************
     C*
     C                   PARM                    @@CCL#           25            CMMC#
     C                   PARM                    @@NATR            2            NATR
     C                   PARM                    @@PART            2            PART
     C                   PARM                    @@CAUS            2            CAUS
     C*
     C*** END ADD *** 09/29/98 ****************************************
     C                   PARM      SCHIDT        @1HIDT            7 0          HIDT
     C                   PARM      SCOCDS        @1OCDS           40            OCDS
     C                   PARM      SCESTS        @1ESTS            2            ESTS
     C                   PARM      SCDYWK        @1DYWK            5 0          DAYS
     C                   PARM      SCHRDY        @1HRDY            3 0          HRSD
     C                   PARM      SCWKPT        @1WKPT            1            SHFT
     C*
     C                   ENDIF                                                  @@WCUP = 'Y'
     C*** END ADD *** 09/21/98 ***************************************
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*
     C*EXECUTE WHEN CLIENT IS #114
     C     XXCLT#        IFEQ      '114'
     C*
     C     Q1PRMS        IFGT      4
     C     @@CONT        ANDEQ     'Y'
     C*
     C                   CALL      'F0275R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP
     C                   PARM      @@RRN         ##RRN2            9 0          FMCMP
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   SETOFF                                         KL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 09/11/06 **************************************
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
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   MOVEL     SRDTO         CMIDT
     C*
     C                   MOVEL     DSTIME        CMTIME
     C                   MOVEL     DSTIME        CMITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMCMP
     C*** BEGIN ADD *** 09/11/06 **************************************
     C*
     C*EXECUTE WHEN CLIENT IS #114
     C     XXCLT#        IFEQ      '114'
     C*
     C     Q1PRMS        IFGT      4
     C     @@CONT        ANDEQ     'Y'
     C*
     C                   CALL      'F0275R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP
     C                   PARM      @@RRN         ##RRN2            9 0          FMCMP
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   SETOFF                                         KL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 09/11/06 **************************************
     C*
     C                   ENDSR
