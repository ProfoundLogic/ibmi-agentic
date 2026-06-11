     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  F0060R01 - WCMC GROUP INFORMATION  C:62 12/08/97             *
     H*                                                               *
     H*  CALLED FROM: F0060R                                          *
     H*  CALLS TO:    F0060R02                                        *
     H*               F0060R03 if Network installed else              *
     H*               F0061R   if EDI installed else                  *
     H*               F0120R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/20/16 C:062 Project: 114WIC                               *
     H*                 Map new screen field to FMFNP/FNOEOB          *
     H*                                                               *
     H*  11/10/16 C:309 Project: 90MIB                                *
     H*                 Map new screen field to FMFNP/FNCLELIG        *
     H*                                                               *
     H*  06/22/11 C:58  PROJECT: 114MYS                               *
     H*                 Add Non-WCMS flag and payment limit CSI Auto  *
     H*                                                               *
     H*  03/10/05 C:62  PROJECT: MC260                                *
     H*                 ADD ALTERNATIVE FEIN PROCESSING               *
     H*                                                               *
     H*  01/05/01 C:62  PROJECT: MCMNT        BASE ONLY               *
     H*                 COMPILE ONLY                                  *
     H*                                                               *
     H*  08/25/00 C:62  PROJECT: MC095 24409                          *
     H*                 REMOVE CONDITION OF CALL TO F0060R02.         *
     H*
     H*  06/27/00 C:58  WC993  Add 'Test' Group flag, editing.
     H*                                                               *
     H*  06/19/00 C:62 SUPPORT: MC065 19768                           *
     H*                SWAP OUT USAGE OF STABL TO STSTL3, SO THAT UC  *
     H*                BECOMES INVALID                                *
     H*                                                               *
     H*  03/03/00 C:62 PROJECT: MCMNT                                 *
     H*                ENHANCED THE WORKFLOW LOGIC.                   *
     H*                                                               *
     H*  01/13/00 C:62 PROJECT: MC035                                 *
     H*                MAKE SURE THE TEST FOR DUPLICATES WORKS.       *
     H*                                                               *
     H*  06/30/99 C:62 PROJECT: MC206                                 *
     H*                -PROTECT WCMS GROUP IF IN CHG MODE.            *
     H*                -LOAD SECOND SCREEN FUND NAME.                 *
     H*                                                               *
     H*  06/15/99 C:53 PROJECT: MC145                                 *
     H*                QUALIFY WCMS INTEGRATION.                      *
     H*                                                               *
     H*  01/20/99 C:53 PROJECT: MC178                                 *
     H*                ADD EXPIRATION DATE TO SCREEN/PGM              *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED F14=DARTS TO PROGRAM                     *
     H*                                                               *
     H*  10/07/98 C:94 PROJECT: 87MFX                                 *
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL              *
     H*                                                               *
     H*  03/04/98 C:53 REQ/PROJ: MC145                                *
     H*                MUST HAVE A SUBSCRIBER # TO ADD                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  POSITION CURSORS IN KJDO:  31 - 39.                          *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71 - 78, 89.                     *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK2: 71 - 89.                         *
     H*                                                               *
     H*  STANDALONE WCMC SYSTEN    :   17
     H*  LINKED TO WORKERS COMP    :  N17
     H*                                                               *
     H*  Client 114 - display Non-WCMS flag and Payment Limit IN49
     H*****************************************************************
     H*  MODES USED:                                                  *
     H*                                                               *
     H*  'A' = Add mode - workflow only                               *
     H*  'C' = Change mode - workflow only                            *
     H*  'D' = Display mode - workflow only                           *
     H*  'T' = Change mode - when called from F0060R from opt6        *
     H*  'S' = Display mode - when called from F0060R from opt6       *
     H*  'O' = To indicate that this program is called as part of the *
     H*        workflow.                                              *
     H*                                                               *
     H*  XXMODE is used to hold the mode that the program was called  *
     H*  with so that this program will know whether this program was *
     H*  call as part of the workflow or as an option from F0060R.    *
     H*                                                               *
     H*  This program is called without qualifications.               *
     H*                                                               *
     H*****************************************************************
     FF0060F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMFNP     UF A E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     F                                     INFDS(DBFDS3)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*** BEG DEL *** 06/15/99 **********************
     F* WMFNL * IF  E ********* K        DISK                           UC
     F*********** PFN01 ******                      KRENAMEPFN02
     F*** END DEL *** 06/15/99 **********************
     FFMFNL1    IF   E           K DISK
     F                                     RENAME(PFN01:PFN03)
     FFMF1P     UF A E             DISK
     F                                     INFDS(DBFDS4)
     FFMF1L     IF   E           K DISK
     F                                     RENAME(PF101:PF102)
     F                                     INFDS(DBFDS2)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMSML     IF   E           K DISK
     FSTSTP     IF   E             DISK
     F*** BEGIN CHG *** 06/19/00 **************************************
     F**********STABL   IF  E           K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END   CHG *** 06/19/00 **************************************
     F                                     RENAME(PST01:PST02)
     FSTSTL     IF   E           K DISK
     F                                     RENAME(PST01:PST03)
     F*** BEGIN DEL *** 09/25/00 **************************************
     F*** BEGIN ADD *** 03/03/00 **************************************
     F***********FMCXL   IF  E           K        DISK
     F***********                                   KINFDS DBFDS5
     F*** END   ADD *** 03/03/00 **************************************
     F*** END   DEL *** 09/25/00 **************************************
      /COPY SCOPYMT01D
     D*
     D*** BEG DEL *** 06/15/99 **********************
     D*PFN02 ******
     D************* FNCO#                           FXCO#
     D************* FNFND                           FXFND
     D************* FNNAME                          FXNAME
     D************* FNSHNM                          FXSHNM
     D************* FNMAD1                          FXMAD1
     D************* FNMAD2                          FXMAD2
     D************* FNMCTY                          FXMCTY
     D************* FNMST                           FXMST
     D************* FNMZIP                          FXMZIP
     D************* FNPAD1                          FXPAD1
     D************* FNPAD2                          FXPAD2
     D************* FNPCTY                          FXPCTY
     D************* FNPST                           FXPST
     D************* FNPZIP                          FXPZIP
     D************* FNPH1A                          FXPH1A
     D************* FNPH1                           FXPH1
     D************* FNAREA                          FXAREA
     D************* FNPH2                           FXPH2
     D************* FNFAXA                          FXFAXA
     D************* FNFAX                           FXFAX
     D************* FNEFDT                          FXEFDT
     D************* FNEXDT                          FXEXDT
     D************* FNSFIN                          FXSFIN
     D************* FNCGFL                          FXCGFL
     D************* FNMXEX                          FXMXEX
     D************* FNNACS                          FXNACS
     D************* FNNAEN                          FXNAEN
     D************* FNNAQN                          FXNAQN
     D************* FNAUTE                          FXAUTE
     D************* FNAUTD                          FXAUTD
     D************* FNDIV0                          FXDIV0
     D************* FNRS01                          FXRS01
     D************* FNRS02                          FXRS02
     D************* FNRS03                          FXRS03
     D************* FNRS04                          FXRS04
     D************* FNRS05                          FXRS05
     D************* FNRS06                          FXRS06
     D************* FNRS07                          FXRS07
     D************* FNRS08                          FXRS08
     D************* FNRS09                          FXRS09
     D************* FNRS10                          FXRS10
     D************* FNDC01                          FXDC01
     D************* FNDC02                          FXDC02
     D************* FNDC03                          FXDC03
     D************* FNDC04                          FXDC04
     D************* FNDC05                          FXDC05
     D************* FNDC06                          FXDC06
     D************* FNDC07                          FXDC07
     D************* FNDC08                          FXDC08
     D************* FNDC09                          FXDC09
     D************* FNDC10                          FXDC10
     D************* FNMETR                          FXMETR
     D************* FNMEP1                          FXMEP1
     D************* FNCOTR                          FXCOTR
     D************* FNEXTR                          FXEXTR
     D************* FNEXP1                          FXEXP1
     D************* FNLETR                          FXLETR
     D************* FNLEP1                          FXLEP1
     D************* FNDFRC                          FXDFRC
     D************* FNDFRA                          FXDFRA
     D************* FNCHK1                          FXCHK1
     D************* FNCHK2                          FXCHK2
     D************* FNLHNF                          FXLHNF
     D************* FNLHF                           FXLHF
     D************* FNANAM                          FXANAM
     D************* FNAAD1                          FXAAD1
     D************* FNAAD2                          FXAAD2
     D************* FNACTY                          FXACTY
     D************* FNAST                           FXAST
     D************* FNAZIP                          FXAZIP
     D************* FNINVK                          FXINVK
     D************* FNSRPR                          FXSRPR
     D************* FNLNRB                          FXLNRB
     D************* FNLNAB                          FXLNAB
     D************* FNLBX1                          FXLBX1
     D************* FNLBX2                          FXLBX2
     D************* FNLBX3                          FXLBX3
     D************* FNLBX4                          FXLBX4
     D************* FNCLMS                          FXCLMS
     D************* FNNOT1                          FXNOT1
     D************* FNNOT2                          FXNOT2
     D************* FNSTAT                          FXSTAT
     D************* FNUSER                          FXUSER
     D************* FNPGM                           FXPGM
     D************* FNTIME                          FXTIME
     D************* FNDT                            FXDT
     D************* FNIUSR                          FXIUSR
     D************* FNIPGM                          FXIPGM
     D************* FNITIM                          FXITIM
     D************* FNIDT                           FXIDT
     D************* FNIADR                          FXIADR
     D************* FNMIDY                          FXMIDY
     D*** END DEL *** 06/15/99 **********************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
     D*** BEGIN ADD *** 03/03/00 **************************************
     D DBFDS5          DS
     D  DBRRN5               397    400B 0
     D*** END   ADD *** 03/03/00 **************************************
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Group Inform-
     D                                     ation')
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C*
     C                   MOVEL     'S'           XXMODE            1
     C                   MOVEL     'D'           @@MODE
     C*
     C     @@MODE        WHENEQ    'T'
     C*
     C                   MOVEL     'T'           XXMODE
     C                   MOVEL     'C'           @@MODE
     C*
     C                   OTHER
     C*
     C                   MOVEL     'O'           XXMODE
     C*
     C                   ENDSL
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*** BEGIN ADD *** 03/03/00 **************************************
     C*
     C     ACDPL2        PLIST
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C*
     C     ACDPL3        PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      FNSUB#        ##SUB#            5 0
     C                   PARM      FNFND         ##FND             3 0
     C*** END   ADD *** 03/03/00 **************************************
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
     C*  State code search parm list
     C*
     C     STCPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*  Workers Compensation Group Parm List.
     C*
     C     WCGPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FND             3
     C                   PARM                    #@FYR             3
     C                   PARM      1             ##VIEW            1 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEGIN DEL *** 09/25/00 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C********** CXKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           SCSUB#
     C**********           KFLD           SCFND
     C*** END   ADD *** 03/03/00 **************************************
     C*** END   DEL *** 09/25/00 **************************************
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C* KEY LIST FOR WCMC GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN CHG *** 01/13/00 **************************************
     C**********           KFLD           FNSUB#
     C                   KFLD                    SCSUB#
     C*** END   CHG *** 01/13/00 **************************************
     C                   KFLD                    KKFND
     C*
     C* KEY LIST FOR WCMS GROUP MASTER AND WCMC BY CO#/WCF#
     C*
     C     FNKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    FNFND
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         ##CO#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    STKEY         KKKEY
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
     C     FNSTAT        IFEQ      'I'
     C                   MOVE      'A'           FNSTAT
     C                   ELSE
     C                   MOVE      'I'           FNSTAT
     C                   ENDIF                                                  FNSTAT
     C*
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C*
     C                   MOVEL     DSTIME        FNTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
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
     C                   EXSR      NEXTSC
     C                   ADD       1             SCR#
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*** BEG ADD *** 03/04/98 **********************
     C*
     C* SUBSCRIBER NUMBER MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90
     CANN30SCSUB#        IFEQ      *ZEROS
     C                   SETON                                        79  90
     C                   ELSE
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C     SMKY          CHAIN     PSM01                              99
     C  N99              MOVEL     SMNAME        SCSNAM
     C                   ENDIF
     C*** END ADD *** 03/04/98 **********************
     C*
     C* GROUP NUMBER MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCFND         IFEQ      *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE GROUP NUMBER HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVE      SCFND         KKFND
     C*
     C   40
     CANN90FNKY          CHAIN(N)  PFN01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* GROUP NAME CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* GROUP SHORT NAME CANNOT BE BLANK
     C*
     C  N90SCSHNM        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCDESC
     C*
     C* EFFECTIVE DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C*
     C* EXPIRATION DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF                                         ADD 01/20/99
     C*
     C     SCEXDT        IFNE      *ZEROS                                       ADD 01/20/99
     C                   Z-ADD     SCEXDT        SRDTI                          ADD 01/20/99
     C                   CALL      'SRCVTDAT'    DATCHK                         ADD 01/20/99
     C                   MOVEL     SRDTO         SCEXDT                         ADD 01/20/99
     C*                                                    ADD 01/20/99
     C     SRERR         IFEQ      '1'                                          ADD 01/20/99
     C                   SETON                                        7990      ADD 01/20/99
     C                   ENDIF                                                  ADD 01/20/99
     C                   ENDIF                                                  ADD 01/20/99
     C                   ENDIF                                                  ADD 01/20/99
     C*
     C* ADJUDICATION STATE CODE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCAST         KKKEY
     C     STKY          CHAIN     PST03                              99
     C   99              SETON                                        76  90
     C                   ENDIF
     C*
     C* MAILING ADDRESS 1 MUST BE ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMAD1        ANDEQ     *BLANKS
     C                   SETON                                        77  90
     C                   ENDIF
     C*
     C* MAILING CITY MUST BE ENTERTED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMCTY        ANDEQ     *BLANKS
     C                   SETON                                        78  90
     C                   ENDIF
     C*
     C* MAILING STATE CODE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCMST         KKKEY
     C     STKY          CHAIN     PST02                              99
     C   99              SETON                                        80  90
     C                   ENDIF
     C*
     C* MAILING ZIP MUST BE ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMZPA        ANDEQ     *ZEROS
     C                   SETON                                        81  90
     C                   ENDIF
     C*
     C* PHYSICAL STATE CODE IF NOT BLANK MUST BE VALID
     C*
     C     SCPST         IFNE      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C                   MOVEL     SCPST         KKKEY
     C     STKY          CHAIN     PST02                              99
     C   99              SETON                                        82  90
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90FNDT          IFGT      SAVEDT
     C     FNDT          OREQ      SAVEDT
     C     FNTIME        ANDGT     SAVETM
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C* SET *IN17 ON IF SCOUTA='Y' ELSE SET OFF *IN17.
     C*
     C  N90              SELECT
     C     SCOUTA        WHENEQ    'Y'
     C                   SETON                                        17
     C*
     C     SCOUTA        WHENEQ    'N'
     C                   SETOFF                                       17
     C*
     C     SCOUTA        WHENEQ    ' '
     C                   SETON                                        17
     C                   ENDSL
    C*** Beg Add ***** 11/10/16 *************************************
     C     *IN17         IFEQ      *ON
     C                   Eval      isGrpSA = *On
     C                   Else
     C                   Eval      isGrpSA = *Off
     C                   EndIf
    C*** End Add ***** 11/10/16 *************************************
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     SCNAME        SCGNAM
     C                   Z-ADD     1             SCR#              1 0
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
    C*** Beg Add ***** 11/10/16 *************************************
     C                   Eval      erCLELIG = *Off
     C                   Eval      msCLELIG = *Blanks
    C*** End Add ***** 11/10/16 *************************************
      *
     C*
     C*  THE ALTERNATE ADDRESS FLAG MUST BE 'Y' OR 'N'.
     C*
     C  N90SCFLG1        IFNE      'Y'
     C     SCFLG1        ANDNE     'N'
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C*  IF ALTERNATE ADDRESS FLAG IS YES THEN REQUIRE ADDRESS FIELDS.
     C*
     C  N90SCFLG1        IFEQ      'Y'
     C*
     C*  REQUIRE ALTERNATE NAME
     C*
     C     SCANME        IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE ADDRESS 1
     C*
     C  N90SCAAD1        IFEQ      *BLANKS
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE CITY
     C*
     C  N90SCACTY        IFEQ      *BLANKS
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE STATE
     C*
     C  N90SCALST        IFEQ      *BLANKS
     C                   SETON                                        7590
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE ZIP CODE
     C*
     C  N90SCAZPA        IFEQ      *ZEROS
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE AREA CODE
     C*
     C  N90SCAARA        IFEQ      *ZEROS
     C                   SETON                                        7890
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE PHONE NUMBER PREFIX
     C*
     C  N90SCAPHA        IFEQ      *ZEROS
     C                   SETON                                        7990
     C                   ENDIF
     C*
     C*  REQUIRE ALTERNATE PHONE NUMBER SUFFIX
     C*
     C  N90SCAPHB        IFEQ      *ZEROS
     C                   SETON                                        8090
     C                   ENDIF
     C                   ENDIF                                                  SCFLG1='Y'
     C*
     C*  STATE CODE MUST BE VALID IF NOT BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCALST        ANDNE     *BLANKS
     C*
     C                   MOVEL     SCALST        KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        7690
     C                   ENDIF
     C*
     C*  WCMS LINKED FLAG MUST BE 'Y' OR 'N'
     C*
     C  N90SCOUTA        IFNE      'Y'
     C     SCOUTA        ANDNE     'N'
     C                   SETON                                        8190
     C                   ENDIF
     C*
     C  N90SCOUTA        IFEQ      'N'
     C*
     C*  SINCE SCOUTA = 'N' THEN WCMS GROUP NUMBER CAN'T BE ZERO.
     C*
     C     SCWCF#        IFEQ      *ZEROS
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/15/99
     C                   SETON                                        8290
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     SCWCF#        IFNE      *ZEROS
     C                   SETON                                        8390
     C                   ENDIF
     C                   ENDIF                                                  SCOUTA='N'
     C*
     C*  WCMS GROUP NUMBER MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SCWCF#        ANDNE     *ZEROS
     C                   MOVE      SCWCF#        KKFND
     C*
     C*** BEG ADD *** 04/29/98 *************
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/15/99
     C*** BEG DEL *** 06/15/99 *************
     C********** XXWOPN    IFEQ ' '
     C**********           OPEN WMFNL
     C**********           MOVE 'Y'       XXWOPN  1
     C**********           ENDIF
     C*** END ADD *** 04/29/98 *************
     C********** FNKY1     CHAINPFN02               N99
     C*** END DEL *** 06/15/99 *************
     C*** BEG ADD *** 06/15/99 *************
     C                   CALL      'WA038R'
     C                   PARM                    ##CO#
     C                   PARM                    KKFND
     C     *IN99         PARM      '0'           XXRTCD            1
     C*** END ADD *** 06/15/99 *************
     C*
     C   99              SETON                                        8490
     C                   ENDIF                                                  ADD 04/29/98
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  WCMS GROUP NUMBER MUST BE UNIQUE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCWCF#        ANDNE     *ZEROS
     C                   MOVE      SCWCF#        KKFND
     C     FNKY1         CHAIN(N)  PFN03                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     FNFND         ANDNE     SCFND
     C                   SETON                                        8590
     C                   ENDIF
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C***** BEGIN ADD **** 06/27/00 ***********************************
     C*
     C*  The Test Group flag must be Y, BLANK, or N.
     C*
B02  C  N90SCTEST        IFNE      'Y'
     C     SCTEST        ANDNE     'N'
     C     SCTEST        ANDNE     ' '
     C                   SETON                                        9025
E02  C                   ENDIF                                                  TEST<>Y/N
     C***** END   ADD **** 06/27/00 ***********************************
     C*** Beg Add *** 06/22/11 ****************************************
B02  C  N90SCNWCG        IFNE      'Y'
     C     SCNWCG        ANDNE     'N'
     C     SCNWCG        ANDNE     ' '
     C                   SETON                                        9026
E02  C                   ENDIF                                                  TEST<>Y/N
     C*** End Add *** 06/22/11 ****************************************
     C*
     C*  WCMC CHECK ISSUANCE MUST BE Y/N IF LINKED WITH WCMS BUT IS
     C*  OPTIONAL IF STANDALONE.
     C*
     C  N90SCOUTA        IFEQ      'N'
     C     SCFL04        ORNE      *BLANKS
     C*
     C     SCFL04        IFNE      'Y'
     C     SCFL04        ANDNE     'N'
     C                   SETON                                        8990
     C                   ENDIF                                                  SCFL04 Y/N
     C                   ENDIF                                                  SCOUTA N
    C*** Beg Add ***** 11/10/16 ******************************************************
     C  N90scCLELIG      IFNE      'Y'
     C     scCLELIG      ANDNE     ' '
     C                   SETON                                        90
     C                   Eval      erCLELIG = *On
     C                   Eval      msCLELIG = 'This field must be "Y" or Blank.'
     C                   ENDIF
    C*** End Add ***** 11/10/16 ******************************************************
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     2             SCR#              1 0
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
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* INITIALIZE PLACE CURSOR INDICATORS.
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C     HFLD          IFEQ      'SCAST'
     C*
     C                   CALL      'F0601R'      STCPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   MOVEL     STKEY         SCAST
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCMST'
     C     HFLD          OREQ      'SCPST'
     C     HFLD          OREQ      'SCALST'
     C*
     C                   CALL      'SY601R'      STCPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCMST'
     C                   SETON                                        33
     C     HFLD          WHENEQ    'SCPST'
     C                   SETON                                        35
     C     HFLD          WHENEQ    'SCALST'
     C                   SETON                                        37
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCMST'
     C                   MOVEL     STKEY         SCMST
     C                   SETON                                        34
     C     HFLD          WHENEQ    'SCPST'
     C                   MOVEL     STKEY         SCPST
     C                   SETON                                        36
     C     HFLD          WHENEQ    'SCALST'
     C                   MOVEL     STKEY         SCALST
     C                   SETON                                        38
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCWCF#'
     C     ##WCIF        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/15/99
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      *ZEROS        #@FND
     C                   CALL      'W0010R'      WCGPL
     C*
     C     *INKL         IFEQ      *ON
     C     #@FND         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C*
     C                   MOVE      #@FND         SCWCF#
     C                   SETON                                        32
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/04/98 ****************************
     C  N30HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C                   SETON                                        37
     C*
     C                   MOVE      SMSUB#        KKSUB#
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSNAM
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD *** 03/04/98 ****************************
     C*
     C                   SETON                                            90
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ***************************************
     C*================================================================
     C* F14 = DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C*
     C                   CALL      'FA718R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ***************************************
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
     C*
     C* SCR01 VARIABLES
     C*
     C                   MOVE      FNSUB#        SCSUB#
     C                   MOVE      FNSUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNSHNM        SCSHNM
     C*
     C                   Z-ADD     FNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     FNEXDT        SRDTI                          ADD 01/20/99
     C                   CALL      'SRCVTDAT'    DTMDYY                         ADD 01/20/99
     C                   MOVEL     SRDTO         SCEXDT                         ADD 01/20/99
     C*
     C                   MOVEL     FNAST         SCAST
     C*
     C                   MOVEL     SCAST         KKKEY
     C*
     C     STKY          CHAIN     PST03                              99
     C*
     C   99              MOVEL     *BLANKS       SCSTNM
     C  N99              MOVEL     STNAME        SCSTNM
     C*
     C                   MOVEL     FNMAD1        SCMAD1
     C                   MOVEL     FNMAD2        SCMAD2
     C                   MOVEL     FNMCTY        SCMCTY
     C                   MOVEL     FNMST         SCMST
     C                   MOVEL     FNMZIP        SCMZPA
     C                   MOVE      FNMZIP        SCMZPB
     C                   MOVEL     FNPAD1        SCPAD1
     C                   MOVEL     FNPAD2        SCPAD2
     C                   MOVEL     FNPCTY        SCPCTY
     C                   MOVEL     FNPST         SCPST
     C                   MOVEL     FNPZIP        SCPZPA
     C                   MOVE      FNPZIP        SCPZPB
     C                   MOVE      FNARA1        SCARA1
     C                   MOVEL     FNPH1         SCPH1A
     C                   MOVE      FNPH1         SCPH1B
     C                   MOVE      FNAREA        SCAREA
     C                   MOVEL     FNPH2         SCPH2A
     C                   MOVE      FNPH2         SCPH2B
     C                   MOVE      FNFARA        SCFARA
     C                   MOVEL     FNFAX         SCFAXA
     C                   MOVE      FNFAX         SCFAXB
     C                   MOVEL     FNSTAT        SCSTAT
     C*
     C* SCR02 VARIABLES
     C*
     C     F1KY          CHAIN     PF102                              99
     C*
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   MOVEL(P)  FNNAME        SCGNAM
     C*** END   ADD *** 06/30/99 **************************************
     C                   MOVEL     F1FLG1        SCFLG1
     C                   MOVEL     F1ANME        SCANME
     C                   MOVEL     F1AAD1        SCAAD1
     C                   MOVEL     F1AAD2        SCAAD2
     C                   MOVEL     F1ACTY        SCACTY
     C                   MOVEL     F1AST         SCALST
     C                   MOVEL     F1AZIP        SCAZPA
     C                   MOVE      F1AZIP        SCAZPB
     C                   MOVE      F1AARA        SCAARA
     C                   MOVEL     F1APHN        SCAPHA
     C                   MOVE      F1APHN        SCAPHB
     C*** BEGIN ADD *** 03/10/05 **************************************
     C                   Z-ADD     F1AFEI        SCAFEI
     C*** END   ADD *** 03/10/05 **************************************
     C                   MOVEL     F1OUTA        SCOUTA
     C                   MOVE      FNWCF#        SCWCF#
     C                   MOVEL     F1FL04        SCFL04
     C*
     C*** BEGIN ADD ***  06/27/00  ************************************
     C                   MOVEL     FNTEST        SCTEST
     C*** END ADD *****  06/27/00  ************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   MOVEL     FNNWCG        SCNWCG
     C                   MOVEL     FNMPLN        SCMPLM
     C*** End Add *** 06/22/11 ****************************************
    C*** Beg Add ***** 11/10/16 **************************************
     C                   MOVE      FNclelig      scCLELIG
    C*** End Add ***** 11/10/16 **************************************
     C*** Begin Add *** 12/20/17 **************************************
     C                   MOVEL     FNoEob        scOutEob
     C*** End   Add *** 12/20/17 **************************************
     C*
     C                   ELSE
     C*
     C* SCR01 VARIABLES
     C*
     C                   Z-ADD     0             DBRRN3
     C                   Z-ADD     0             DBRRN4
     C*
     C                   MOVE      ##SUB#        SCSUB#
     C                   MOVE      ##SUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCSHNM
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *ZEROS        SCEXDT                         ADD 01/20/99
     C                   MOVE      *BLANKS       SCAST
     C                   MOVEL     *BLANKS       SCSTNM
     C                   MOVE      *BLANKS       SCMAD1
     C                   MOVE      *BLANKS       SCMAD2
     C                   MOVE      *BLANKS       SCMCTY
     C                   MOVE      *BLANKS       SCMST
     C                   MOVE      *ZEROS        SCMZPA
     C                   MOVE      *ZEROS        SCMZPB
     C                   MOVE      *BLANKS       SCPAD1
     C                   MOVE      *BLANKS       SCPAD2
     C                   MOVE      *BLANKS       SCPCTY
     C                   MOVE      *BLANKS       SCPST
     C                   MOVE      *ZEROS        SCPZPA
     C                   MOVE      *ZEROS        SCPZPB
     C                   MOVE      *ZEROS        SCARA1
     C                   MOVE      *ZEROS        SCPH1A
     C                   MOVE      *ZEROS        SCPH1B
     C                   MOVE      *ZEROS        SCAREA
     C                   MOVE      *ZEROS        SCPH2A
     C                   MOVE      *ZEROS        SCPH2B
     C                   MOVE      *ZEROS        SCFARA
     C                   MOVE      *ZEROS        SCFAXA
     C                   MOVE      *ZEROS        SCFAXB
     C                   MOVEL     'A'           SCSTAT
     C*
     C* SCR02 VARIABLES
     C*
     C                   MOVE      *BLANKS       SCFLG1
     C                   MOVE      *BLANKS       SCANME
     C                   MOVE      *BLANKS       SCAAD1
     C                   MOVE      *BLANKS       SCAAD2
     C                   MOVE      *BLANKS       SCACTY
     C                   MOVE      *BLANKS       SCAST
     C                   MOVE      *ZEROS        SCAZPA
     C                   MOVE      *ZEROS        SCAZPB
     C                   MOVE      *ZEROS        SCAARA
     C                   MOVE      *ZEROS        SCAPHA
     C                   MOVE      *ZEROS        SCAPHB
     C*** BEGIN ADD *** 03/10/05 **************************************
     C                   Z-ADD     *ZEROS        SCAFEI
     C*** END   ADD *** 03/10/05 **************************************
     C                   MOVE      *BLANKS       SCOUTA
     C                   MOVE      *ZEROS        SCWCF#
     C                   MOVE      *BLANKS       SCFL04
     C*
     C*** BEGIN ADD ***  06/27/00  ************************************
     C                   MOVE      *BLANKS       SCTEST
     C*** END ADD *****  06/27/00  ************************************
    C*** Beg Add ***** 11/10/16 ******************************************************
     C                   MOVE      *Blanks       scCLELIG
    C*** End Add ***** 11/10/16 ******************************************************
     C*
     C                   ENDIF
     C*
     C* SET *IN17 ON IF SCOUTA='Y' ELSE SET OFF *IN17.
     C*
     C  N90              SELECT
     C*
     C     SCOUTA        WHENEQ    'Y'
     C*
     C                   SETON                                        17
     C*
     C     SCOUTA        WHENEQ    'N'
     C*
     C                   SETOFF                                       17
     C*
     C     SCOUTA        WHENEQ    ' '
     C*
     C                   SETON                                        17
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* SCR01 VARIABLES
     C*
     C                   Z-ADD     ##CO#         FNCO#
     C                   MOVE      SCSUB#        FNSUB#
     C                   MOVE      SCFND         FNFND
     C                   MOVEL     SCNAME        FNNAME
     C                   MOVEL     SCSHNM        FNSHNM
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI                          ADD 01/20/99
     C                   CALL      'SRCVTDAT'    DTCYMD                         ADD 01/20/99
     C                   MOVEL     SRDTO         FNEXDT                         ADD 01/20/99
     C*
     C                   MOVEL     SCAST         FNAST
     C                   MOVEL     SCMAD1        FNMAD1
     C                   MOVEL     SCMAD2        FNMAD2
     C                   MOVEL     SCMCTY        FNMCTY
     C                   MOVEL     SCMST         FNMST
     C                   MOVEL     SCMZPA        FNMZIP
     C                   MOVE      SCMZPB        FNMZIP
     C                   MOVEL     SCPAD1        FNPAD1
     C                   MOVEL     SCPAD2        FNPAD2
     C                   MOVEL     SCPCTY        FNPCTY
     C                   MOVEL     SCPST         FNPST
     C                   MOVEL     SCPZPA        FNPZIP
     C                   MOVE      SCPZPB        FNPZIP
     C                   MOVE      SCARA1        FNARA1
     C                   MOVEL     SCPH1A        FNPH1
     C                   MOVE      SCPH1B        FNPH1
     C                   MOVE      SCAREA        FNAREA
     C                   MOVEL     SCPH2A        FNPH2
     C                   MOVE      SCPH2B        FNPH2
     C                   MOVE      SCFARA        FNFARA
     C                   MOVEL     SCFAXA        FNFAX
     C                   MOVE      SCFAXB        FNFAX
     C                   MOVEL     SCSTAT        FNSTAT
     C*
     C* SCR02 VARIABLES
     C*
     C                   MOVE      ##CO#         F1CO#
     C                   MOVE      SCSUB#        F1SUB#
     C                   MOVE      SCFND         F1FND
     C                   MOVEL     SCFLG1        F1FLG1
     C                   MOVEL     SCANME        F1ANME
     C                   MOVEL     SCAAD1        F1AAD1
     C                   MOVEL     SCAAD2        F1AAD2
     C                   MOVEL     SCACTY        F1ACTY
     C                   MOVEL     SCALST        F1AST
     C                   MOVEL     SCAZPA        F1AZIP
     C                   MOVE      SCAZPB        F1AZIP
     C                   MOVE      SCAARA        F1AARA
     C                   MOVEL     SCAPHA        F1APHN
     C                   MOVE      SCAPHB        F1APHN
     C*** BEGIN ADD *** 03/10/05 **************************************
     C                   Z-ADD     SCAFEI        F1AFEI
     C*** END   ADD *** 03/10/05 **************************************
     C                   MOVEL     SCOUTA        F1OUTA
     C                   MOVE      SCWCF#        FNWCF#
     C                   MOVE      SCFL04        F1FL04
     C*
     C*** BEGIN ADD ***  06/27/00  ************************************
     C                   MOVE      SCTEST        FNTEST
     C*** END ADD *****  06/27/00  ************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   MOVEL     SCNWCG        FNNWCG
     C                   MOVEL     SCMPLM        FNMPLN
     C*** End Add *** 06/22/11 ****************************************
    C*** Beg Add ***** 11/10/16 ******************************************************
     C                   MOVE      scCLELIG      FNclelig
    C*** End Add ***** 11/10/16 ******************************************************
     C*** Begin Add *** 12/20/17 **************************************
     C                   MOVEL     scOutEob      FNoEob
     C*** End   Add *** 12/20/17 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C     NEXTSC        BEGSR
     C*
     C     SCR#          IFEQ      2
     C*
     C     XXMODE        IFEQ      'O'
     C*
     C                   MOVE      SCFND         KKFND
     C*
     C     FNKY          CHAIN(N)  PFN01                              99
     C*
     C                   MOVE      DBRRN         ##RRN
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   MOVE      DBRRN         ##RRN2
     C*** END   ADD *** 03/03/00 **************************************
     C*
     C     @@MODE        IFEQ      'D'
     C*
     C                   MOVE      'D'           ##MODE
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   MOVE      '1'           ##CALL
     C*** END   ADD *** 03/03/00 **************************************
     C*
     C                   ELSE
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   MOVE      ' '           ##CALL
     C*** END   ADD *** 03/03/00 **************************************
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C                   MOVE      'A'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVE      'C'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 09/25/00 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C*
     C**********           SELEC
     C*
     C********** ##AR      WHEQ 'Y'
     C*** END   ADD *** 03/03/00 **************************************
     C*** END   DEL *** 09/25/00 **************************************
     C                   CALL      'F0060R02'    ACDPL1
     C*** BEGIN DEL *** 09/25/00 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C********** ##NTWK    WHEQ 'Y'
     C**********           CALL 'F0060R03'ACDPL1
     C*
     C********** ##EDI     WHEQ 'Y'
     C**********           CALL 'F0061R'  ACDPL2
     C*
     C**********           OTHER
     C********** CXKY      CHAINPCX01                99
     C*
     C********** *IN99     IFEQ *ON
     C*
     C**********           MOVEL'A'       ##MODE
     C**********           Z-ADD*ZEROS    ##RRN
     C*
     C**********           ELSE
     C*
     C**********           MOVEL'C'       ##MODE
     C**********           Z-ADDDBRRN5    ##RRN
     C*
     C**********           ENDIF
     C*
     C**********           CALL 'F0120R01'ACDPL3
     C*
     C**********           ENDSL
     C*** END   ADD *** 03/03/00 **************************************
     C*** END   DEL *** 09/25/00 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C*
     C* SET *IN17 ON IF SCOUTA='Y' ELSE SET OFF *IN17.
     C*
     C  N90              SELECT
     C*
     C     SCOUTA        WHENEQ    'Y'
     C*
     C                   SETON                                        17
     C*
     C     SCOUTA        WHENEQ    'N'
     C*
     C                   SETOFF                                       17
     C*
     C     SCOUTA        WHENEQ    ' '
     C*
     C                   SETON                                        17
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     DBRRN3        DELETE    DBRCDF                             99
     C     DBRRN4        DELETE    PF101                              99
     C                   MOVEL     *ON           OUTFLG
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
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
     C     *INKJ         CASEQ     *ON           KJDO
     C*** BEG ADD *** 12/01/98 ***************************************
     C     *INKN         CASEQ     *ON           KNDO
     C*** END ADD *** 12/01/98 ***************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     *INKC         IFEQ      *ON
     C*
     C     DBRRN3        DELETE    DBRCDF                             99
     C     DBRRN4        DELETE    PF101                              99
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** BEG ADD *** 04/29/98 *************
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C*** END ADD *** 04/29/98 *************
     C*** BEG ADD *** 06/15/99 *************
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 06/15/99 *************
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     ##WCIF        IFEQ      'Y'
     C                   SETON                                        18
     C                   ELSE
     C                   SETOFF                                       18
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/04/98 **************************
     C     ##SUB#        COMP      *ZEROS                             3030
     C*** END ADD *** 03/04/98 **************************
     C                   Z-ADD     2             #OFSCR            1 0
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
     C                   Z-ADD     1             SCR#
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C                   CLEAR                   DBRRN3
     C                   CLEAR                   DBRRN4
     C*
     C                   ENDIF
     C*
     C     @@CALL        IFNE      *BLANKS
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C* PROTECT WCMS MODE IF IN CHANGE MODE
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   SETON                                        19
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       19
     C*
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     XXCLT#        IFEQ      '114'
     C                   SETON                                        49
     C                   ENDIF
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'I'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATD - Update detail record if no record writes detail record
     C*================================================================
     C     UPDATD        BEGSR
     C*
     C     F1KY          CHAIN     PF102                              99
     C*
     C  N99DBRRN2        CHAIN     PF101                              99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F1USER
     C                   MOVEL     Q1PGM         F1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1DT
     C*
     C                   MOVEL     DSTIME        F1TIME
     C*
     C                   UPDATE    PF101
     C*
     C                   ELSE
     C*
     C                   EXSR      WRITED
     C*
E01  C                   ENDIF
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
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C*
     C                   MOVEL     DSTIME        FNTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      UPDATD
     C*
     C                   EXSR      NEXTSC
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITED - Write a detail record when there is already a master
     C*================================================================
     C     WRITED        BEGSR
     C*
     C                   CLEAR                   PF101
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F1USER
     C                   MOVEL     Q1PGM         F1PGM
     C                   MOVEL     Q1PGM         F1IPGM
     C                   MOVEL     Q1USER        F1IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1DT
     C*
     C                   MOVEL     SRDTO         F1IDT
     C*
     C                   MOVEL     DSTIME        F1TIME
     C                   MOVEL     DSTIME        F1ITIM
     C*
     C                   WRITE     PF101
     C*
     C                   FEOD      FMF1P
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
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C                   MOVEL     Q1PGM         FNIPGM
     C                   MOVEL     Q1USER        FNIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C*
     C                   MOVEL     SRDTO         FNIDT
     C*
     C                   MOVEL     DSTIME        FNTIME
     C                   MOVEL     DSTIME        FNITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMFNP
     C*
     C                   EXSR      WRITED
     C*
     C                   FEOD      FMF1P
     C*
     C                   EXSR      NEXTSC
     C*
     C*
     C                   ENDSR
