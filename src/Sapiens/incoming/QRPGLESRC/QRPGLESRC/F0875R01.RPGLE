     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0875R01 - WORK WITH EDI HISTORY LOG UPDATE/DISPLAY
     H*****************************************************************
     H* PROGRAM: F0875R01
     H* DESCRIPTION: WORK WITH EDI LOG HISTORY MAINTENANCE
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/20/02 C:53  SUPPORT: WC048/32360
     H*                 IF SENT DATE = 0 LOAD 0 TO SENT TIME.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*  05/12/00 C:53  PROJECT: MCMNT
     H*                 CHANGED EMPLOYER DIVISION DEFN FROM 3 TO 5.
     H**********************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**     N40      837/835 EOMB                                   **
     H**      40      278 Treatment plan                             **
     H*****************************************************************
     H*
     FF0875F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFELGP     UF A E             DISK
     F                                     RENAME(PLG01:DBRCDF)
     FFMF1L     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFTGTL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** BEGIN ADD *** 10/09/97 ***********************************************
     FFATRL     IF   E           K DISK
     FFATIL     IF   E           K DISK
     FFATDL1    IF   E           K DISK
     F*** END ADD *** 10/09/97 *************************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('837 Log History')
     D*** BEGIN ADD *** 10/09/97 ***********************************************
     D OPTNM2          C                   CONST('278 Log History')
     D*** END ADD *** 10/09/97 *************************************************
     D COMAPI          C                   CONST('FA013R')
     D #OFSCR          C                   CONST(1)
     D*
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D** DARTS KEY (SC010R) LINKED WITH WCMS
     D*
     D                 DS
     D  DSKEY                  1     16
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSFYR                  7      9  0
     D  DSCASE                10     16  0
     D*
     D** DARTS KEY (SC010R) FOR OUTSIDE ACCOUNT
     D*
     D                 DS
     D  DRKEY                  1     21
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0

     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@SBNM           40
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0          CHG 05/12/00
     C                   PARM                    @@EMNM           40
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C                   PARM                    @@LSNM           40
     C                   PARM                    @@SCL#           25
     C     SCL1HD        PARM                    @@L1HD           78
     C     SCL2HD        PARM                    @@L2HD           78
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID           10
     C*
     C*** KEY LIST FOR GROUP AUX FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKY
     C*
     C*** KEY LIST FOR STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C*
     C*   KEY LIST FOR BWC TREATMENT FILES -- FATIL, FATRL
     C*
     C     TIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    XXTX#
     C     *LIKE         DEFINE    TITX#         XXTX#
     C*** END ADD *** 10/09/97 *************************************************
     C*
     C     *LIKE         DEFINE    LGSUB#        XXSUB#
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKY
     C     *LIKE         DEFINE    GTKEY         HLCODE
     C     *LIKE         DEFINE    GTDESC        HLDESC
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*
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
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN97         IFEQ      '1'
     C                   EXSR      CMDKEY
     C     *IN90         CABEQ     '1'           ERRSTP
     C     OUTFLG        CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C*  VALIDATE SUBMISSION DUE DATE
     C*
     C     SCDUE         IFNE      *ZEROS
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUE
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCEDSC
     C     SCSTS         IFEQ      *BLANKS
     C                   SETON                                        9080
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ELSE
     C                   MOVEL(P)  'EDIS'        XXTBID
     C                   MOVEL(P)  SCSTS         XXKY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCEDSC
     C                   ELSE
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C                   ENDIF
     C*
     C** CHECK FOR HOLD/RELEASE STATUS
     C*
     C     SCSTAT        IFNE      'H'
     C     SCSTAT        ANDNE     ' '
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C*  VALIDATE EXTRACT DATE
     C*
     C     SCEXDT        IFNE      *ZEROS
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9077
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C                   END
     C*
     C*  VALIDATE DATE SENT
     C*
     C     SCSENT        IFNE      *ZEROS
     C                   Z-ADD     SCSENT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSENT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9078
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C                   END
     C*
     C*  VALIDATE DATE ACKNOWLEDGED
     C*
     C     SCARCV        IFNE      *ZEROS
     C                   Z-ADD     SCARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCARCV
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9075
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C                   END
     C*
     C                   ENDIF                                                  @@MODE IFEQ
     C*
     C     ERRSTP        ENDSR
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *ZEROS        SCEOB#
     C                   MOVE      *BLANKS       SCJST
     C                   MOVE      *BLANKS       SCJNME
     C                   Z-ADD     0             SCDUE
     C                   MOVE      *BLANKS       SCTSET
     C                   MOVE      *BLANKS       SCTDSC
     C                   MOVE      *BLANKS       SCMTC
     C                   MOVE      *BLANKS       SCMDSC
     C                   MOVE      *BLANKS       SCSTS
     C                   MOVE      *BLANKS       SCEDSC
     C                   MOVE      *BLANKS       SCSTAT
     C                   Z-ADD     0             SCEXDT
     C                   Z-ADD     0             SCSENT
     C                   Z-ADD     0             SCARCV
     C******               MOVE *BLANKS   SCEFT#
     C******               Z-ADD0         SCAENT
     C                   MOVE      LGEOB#        SCEOB#
     C*
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'D'
     C*
     C                   MOVEL(P)  LGJST         SCJST
     C                   MOVEL(P)  LGJST         XXKEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  STNAME        SCJNME
     C                   ENDIF
     C*
     C                   MOVEL     LGTSET        SCTSET
     C                   MOVEL(P)  LGTSET        XXKY
     C                   MOVEL(P)  'TSET'        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCTDSC
     C                   ENDIF
     C*
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUE
     C*
     C                   MOVEL(P)  LGMTC         SCMTC
     C                   MOVEL(P)  LGMTC         XXKY
     C                   MOVEL     'MTC '        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCMDSC
     C                   ENDIF
     C*
     C                   MOVEL(P)  LGSTS         SCSTS
     C                   MOVEL(P)  LGSTS         XXKY
     C                   MOVEL(P)  'EDIS'        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCEDSC
     C                   ENDIF
     C*
     C                   MOVEL(P)  LGSTAT        SCSTAT
     C*
     C                   Z-ADD     LGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   Z-ADD     LGOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSENT
     C                   Z-ADD     LGARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCARCV
     C*
     C********             MOVELLGEFT#    SCEFT#    P
     C*** DISPLAY DATE SENT TO ANTHEM ONLY IF ANTHEM FLATFILE FLAG = Y
     C****       LGANTH    IFEQ 'Y'
     C****                 SETON                     77
     C****                 Z-ADDLGAENT    SRDTI
     C****                 CALL 'SRCVTDAT'DTMDYY
     C****                 MOVELSRDTO     SCAENT
     C****                 ENDIF
     C*
     C                   ENDIF                                                  END @@MODE
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C                   SETOFF                                       40
     C     LGTSET        IFNE      '837'
     C                   SETON                                        40
     C*
     C                   DO
     C                   MOVE      LGEOB#        XXTX#
     C     TIKY          CHAIN     PTR01                              60
     C   60              LEAVE                                                  VVV LEAVE
     C                   Z-ADD     TRBGDT        SRDTI             8 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C     TIKY          CHAIN     PTI01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     TIPRIM        IFEQ      'Y'
     C                   MOVEL(P)  TIICD9        SCICD9
     C                   SETON                                        60
     C     TIKY          CHAIN     PTD01                              61
     C*
     C  N61TDTYPE        IFEQ      'D'
     C     TDSEQ#        ANDEQ     1
     C                   MOVEL(P)  TDDESC        SCDSC1
     C*
     C                   ELSE                                                   TYPE = 'D'
     C                   MOVEL     *BLANKS       SCDSC1
     C                   ENDIF                                                  TYPE = 'D'
     C                   ENDIF                                                  PRIM = 'Y'
     C*
     C  N60TIKY          READE     PTI01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C                   ENDDO                                                  End DO loop
     C                   ENDIF                                                  TSET <> 837
     C*** END ADD *** 10/09/97 *************************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C                   MOVEL(P)  SCSTS         LGSTS
     C                   MOVEL(P)  SCSTAT        LGSTAT
     C                   Z-ADD     SCARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGARCV
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGEXDT
     C                   Z-ADD     SCSENT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGOTDT
     C*** BEG ADD *** 06/20/02 ***************************
     C     LGOTDT        IFEQ      *ZEROS
     C                   Z-ADD     *ZEROS        LGOTTM
     C                   ENDIF
     C*** END ADD *** 06/20/02 ***************************
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C                   MOVEL(P)  OPTNM2        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRD2
     C*** END ADD *** 10/09/97 *************************************************
     C*
     C                   MOVE      @@SUB#        SCSUB#            5 0
     C                   MOVEL(P)  @@SBNM        SCSBNM           40
     C                   MOVE      @@FND         SCFND             3 0
     C                   MOVE      @@FYR         SCFYR             3 0
     C                   MOVE      @@CASE        SCCASE            7 0
     C                   MOVE      @@EMP#        SCEMP#            9 0
     C                   MOVE      @@DIV         SCDIV             5 0          CHG 05/12/00
     C                   MOVEL(P)  @@EMNM        SCEMNM           40
     C                   MOVEL     @@SS#         SCSS#             9
     C                   MOVE      @@ACDT        SCACDT            8 0
     C                   MOVEL(P)  @@LSNM        SCLSNM           40
     C                   MOVEL(P)  @@SCL#        SCSCL#           25
     C*
     C                   TIME                    DFDTTM
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C     @@MODE        IFEQ      'S'
     C                   EXSR      UPDATR
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C     @@MODE        IFNE      'S'
     C                   EXSR      MVSCDB
     C                   ELSE
     C     LGSTAT        IFEQ      'H'
     C                   MOVE(P)   ' '           LGSTAT                         CHG 09/29/97
     C                   ELSE
     C     LGSTAT        IFEQ      ' '                                          CHG 09/29/97
     C                   MOVE(P)   'H'           LGSTAT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   TIME                    DFDTTM
     C                   MOVEL(P)  Q1USER        LGUSER
     C                   MOVEL(P)  Q1PGM         LGPGM
     C                   MOVEL(P)  DFTIME        LGTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   TIME                    DFDTTM
     C                   MOVEL(P)  Q1USER        LGIUSR
     C                   MOVEL(P)  Q1PGM         LGIPGM
     C                   MOVEL(P)  DFTIME        LGITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGIDT
     C*
     C                   MOVEL(P)  Q1USER        LGUSER
     C                   MOVEL(P)  Q1PGM         LGPGM
     C                   MOVEL(P)  DFTIME        LGTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C*
     C                   Z-ADD     SCSUB#        XXSUB#
     C                   Z-ADD     SCFND         XXFND
     C                   MOVE(P)   SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C*
     C                   WRITE     DBRCDF
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* CMDKEY - COMMAND KEY PROCESSING
     C*================================================================
     C     CMDKEY        BEGSR
     C*
     C**************
     C* F10=SEARCH *
     C**************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*** MAINTENANCE CODE SEARCH
     C*
     C****NEW BEG ***********************************************
     C     HFLD          IFEQ      'SCMTC'
     C                   MOVEL     'MTC '        ##TBID
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        34         PC
     C*
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCMTC
     C                   MOVEL(P)  GTDESC        SCMDSC
     C                   SETON                                        35         PC
     C                   ENDIF
     C                   ENDIF
     C*
     C****NEW END ***********************************************
     C*          HFLD      IFEQ 'SCMTC'
     C*
     C*                    MOVELSCMTC     HLCODE    P
     C*                    MOVELSCMDSC    HLDESC    P
     C*
     C*                    MOVELOPTNM     ##PRDS    P
     C*                    MOVE *BLANKS   ##CALL    P
     C*                    MOVEL'MTC '    ##ID      P
     C*                    MOVE *BLANKS   ##KY
     C*                    MOVE *BLANKS   ##RTCD
     C*
     C*                    CALL 'F2493R'
     C*                    PARM           ##PRDS 40
     C*                    PARM           ##CALL  1
     C*                    PARM           ##ID    4
     C*                    PARM           ##KY   10
     C*                    PARM           ##RTCD  1
     C*                    MOVEL##KY      SCMTC     P
     C*
     C*          ##RTCD    IFEQ 'A'                        F3
     C*                    MOVELHLCODE    SCMTC     P
     C*                    MOVELHLDESC    SCMDSC    P
     C*                    MOVE '1'       OUTFLG  1
     C*                    ELSE
     C*                    MOVE '0'       OUTFLG
     C*                    ENDIF                           ##RTCD IFEQ A
     C*
     C*          ##RTCD    IFEQ 'B'                        F12
     C*                    MOVELHLCODE    SCMTC     P
     C*                    MOVELHLDESC    SCMDSC    P
     C*                    SETON                     90
     C*                    ENDIF
     C*
     C*          ##RTCD    IFEQ '*'                        SELECTION
     C*                    MOVELSCMTC     XXKY      P
     C*                    MOVEL'MTC '    XXTBID    P
     C*          GTKY      CHAINPGT01                99
     C*          *IN99     IFEQ '0'
     C*                    MOVELGTDESC    SCMDSC    P
     C*                    ENDIF                           *IN99 IFEQ
     C*                    SETON                     9076
     C*                    ENDIF                           ##RTCD IFEQ *
     C*                    ENDIF                           HFLD IFEQ
     C*
     C*
     C*** EDI STATUS CODE SEARCH
     C*
     C****NEW BEG ***********************************************
     C     HFLD          IFEQ      'SCSTS'
     C                   MOVEL     'EDIS'        ##TBID
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      *BLANKS       SCMDSC
     C                   SETON                                        7990       PC
     C*
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCSTS
     C                   MOVEL(P)  GTDESC        SCEDSC
     C                   SETON                                        7690       PC
     C                   ENDIF
     C                   ENDIF
     C*
     C****NEW END ***********************************************
     C*          HFLD      IFEQ 'SCSTS'
     C*
     C*                    MOVELSCSTS     HLCODE    P
     C*                    MOVELSCEDSC    HLDESC    P
     C*
     C*                    MOVELOPTNM     ##PRDS    P
     C*                    MOVE *BLANKS   ##CALL    P
     C*                    MOVEL'EDIS'    ##ID      P
     C*                    MOVE *BLANKS   ##KY      P
     C*                    MOVE *BLANKS   ##RTCD    P
     C*
     C*                    CALL 'F2493R'
     C*                    PARM           ##PRDS 40
     C*                    PARM           ##CALL  1
     C*                    PARM           ##ID    4
     C*                    PARM           ##KY   10
     C*                    PARM           ##RTCD  1
     C*                    MOVEL##KY      SCSTS     P
     C*
     C*          ##RTCD    IFEQ 'A'
     C*                    MOVELHLCODE    SCSTS     P
     C*                    MOVELHLDESC    SCEDSC    P
     C*                    MOVE '1'       OUTFLG  1
     C*                    ELSE
     C*                    MOVE '0'       OUTFLG
     C*                    ENDIF                           ##RTCD IFEQ A
     C*
     C*          ##RTCD    IFEQ 'B'
     C*                    MOVELHLCODE    SCSTS     P
     C*                    MOVELHLDESC    SCEDSC    P
     C*                    SETON                     90
     C*                    ENDIF
     C*
     C*          ##RTCD    IFEQ '*'
     C*                    MOVELSCSTS     XXKY      P
     C*                    MOVEL'EDIS'    XXTBID    P
     C*          GTKY      CHAINPGT01                99
     C*          *IN99     IFEQ '0'
     C*                    MOVELGTDESC    SCEDSC    P
     C*                    ENDIF                           *IN99 IFEQ
     C*                    SETON                     9076
     C*                    ENDIF                           ##RTCD IFEQ *
     C*                    ENDIF                           HFLD IFEQ
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C**************
     C* F14=DARTS  *
     C**************
     C*
     C     *INKN         IFEQ      '1'
     C*
     C*
     C     F1KEY         CHAIN     PF101                              99
     C     *IN99         IFEQ      '0'
     C     F1OUTA        IFEQ      'N'                                          LINKED WITH
     C*** BEGIN CHG *** 09/23/97 **************************************
     C*   NEED TO BLANK OUT KEY, NOT ZERO OUT KEY
     C                   MOVE      *BLANKS       DSKEY                          WCMS
     C*** END CHG ***** 09/23/97 **************************************
     C                   MOVE      ##CO#         DSCO#
     C                   MOVE      SCFND         DSFND
     C                   MOVE      SCFYR         DSFYR
     C                   MOVE      SCCASE        DSCASE
     C                   MOVEL(P)  DSKEY         ##KEY            40
     C*
     C                   ELSE
     C*** BEGIN CHG *** 09/23/97 **************************************
     C*   NEED TO BLANK OUT KEY, NOT ZERO OUT KEY
     C*********            MOVE *ZEROS    DRKEY            OUTSIDE
     C                   MOVE      *BLANKS       DRKEY                          OUTSIDE
     C*** END CHG ***** 09/23/97 **************************************
     C                   MOVE      ##CO#         DRCO#                          ACCOUNT
     C                   MOVE      SCSUB#        DRSUB#
     C                   MOVE      SCFND         DRFND
     C                   MOVE      SCFYR         DRFYR
     C                   MOVE      SCCASE        DRCASE
     C                   MOVEL(P)  DRKEY         ##KEY
     C                   ENDIF                                                  F1OUTA IFEQ
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C*** BEGIN CHG *** 09/23/97 **************************************
     C*** USING F0270R INSTEAD OF F0075R
     C*********            MOVEL'F0075R'  ##PGM  40 P
     C                   MOVEL(P)  'F0270R'      ##PGM            40
     C*** END CHG ***** 09/23/97 **************************************
     C*
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGM
     C                   ENDIF                                                  *INKN IFEQ
     C*
     C                   ENDSR
