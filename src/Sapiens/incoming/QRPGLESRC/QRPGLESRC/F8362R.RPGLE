     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F8362R - WORK WITH TWCC ADL ITEMS                        *
     H*****************************************************************
     H* CREATED ON 09/18/2003 BY C:62 FOR PROJECT TX021               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/20/05 C:62 PROJECT MC025 49503                             *
     H*               CORRECT PAGING-                                 *
     H*                                                               *
     H*****************************************************************
     FF8362F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW02S:RRN)
     F*
     FFZBCL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFZCCL2    IF   E           K DISK

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D NOLNK           C                   CONST('No entries available')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXLIC#                 1     12
     D  TYPE                   1      2
     D  LIC#                   3     12
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*================================================================
     C* PARAMETER LISTS
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@LIC#           12
     C                   PARM                    @@RTCD            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8               MDY(YY)
     C                   PARM      *BLANKS       SRTFMT            8               MDYY
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8            YYMMDD
     C                   PARM      *BLANKS       SRTFMT            8            MMDDYY
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*================================================================
     C* KEY LISTS
     C*================================================================
     C     BCKY          KLIST
     C                   KFLD                    CCID#
     C*
     C     CCKY          KLIST
     C                   KFLD                    KKLIC#
     C                   KFLD                    KKLCTP
     C                   KFLD                    KKJURS
     C*================================================================
     C* LIKE DEFINES
     C*================================================================
     C     *LIKE         DEFINE    CCLIC#        KKLIC#
     C     *LIKE         DEFINE    CCLCTP        KKLCTP
     C     *LIKE         DEFINE    CCJURS        KKJURS
     C*================================================================
     C* MAIN LINE PROCESSING
     C*================================================================
     C*
     C                   EXSR      MAINSR
     C*
     C                   SETON                                        LR
     C*
     C*====================================================================*
     C* CHKLIC = CHECK TO SEE IF THE LICENSE NUMBER CAN BE FOUND
     C*====================================================================*
     C     CHKLIC        BEGSR
     C*
     C                   MOVEL     *OFF          EXISTS            1
     C                   MOVEL     *OFF          ACTIVE            1
     C                   MOVEL(P)  'F8362R'      SCFILE
     C*
     C     *LOVAL        SETLL     PCC01
     C                   READ      PCC01                                  99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     *ON           ACTIVE
     C                   MOVEL(P)  @@LIC#        XXLIC#
     C                   MOVEL(P)  TYPE          KKLCTP
     C                   MOVEL(P)  LIC#          KKLIC#
     C                   MOVEL(P)  'TX'          KKJURS
     C     CCKY          CHAIN     PCC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     *ON           EXISTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================*
     C* INITB - INIT SUBROUTINE FOR WINDOW 2
     C*====================================================================*
     C     INITB         BEGSR
     C*
     C                   Z-ADD     0             RRN               5 0
     C                   MOVEL     'WDW02'       SCRECF                          FOR DISPLAY
     C                   MOVEL(P)  @@LIC#        SCLIC#
     C                   MOVEL(P)  CCID#         SCID#
     C*
     C                   ENDSR
     C*====================================================================*
     C* MAINSR - MAIN PROCESSING
     C*====================================================================*
     C     MAINSR        BEGSR
     C*
     C                   EXSR      CHKLIC
     C     ACTIVE        IFEQ      *ON
     C     EXISTS        IFEQ      *OFF
     C                   MOVEL     'WDW01'       SCRECF                          FOR DISPLAY
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   MOVEL     '*'           @@RTCD
     C                   ENDIF
     C                   ELSE
     C                   EXSR      XWDW02
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================*
     C* LOAD  - LOAD WDW02 SUBFILE
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   SETOFF                                       0532
     C                   MOVE      *BLANK        SCSEL
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UPTO 6 LINES
     C*
     C*** BEGIN CHG *** 10/20/05 **************************************
     C**********           DO   6         X       50
     C                   DO        9999          X                 5 0
     C*** END   CHG *** 10/20/05 **************************************
     C*
     C     BCKY          READE     PBC01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C     X             ANDEQ     1
     C                   SETON                                        32
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             RRN
     C*
     C* Load screen fields
     C*
     C                   Z-ADD     DBRRN         SCDBRR
     C                   MOVEL     BCUDAT        SCUDAT
     C                   MOVE      BCCEFD        SCCEFD
     C                   MOVE      BCCDDT        SCCDDT
     C                   MOVEL     BCCTYP        SCCTYP
     C                   MOVEL     BCCSTS        SCCSTS
     C                   MOVEL     BCCID#        SCCID#
     C*
     C                   SETOFF                                       06
     C                   WRITE     WDW02S
     C                   ELSE
     C                   SETON                                        05
     C*** BEGIN ADD *** 10/20/05 **************************************
     C                   LEAVE
     C*** END   ADD *** 10/20/05 **************************************
     C                   ENDIF
     C                   ENDDO
     C*
     C* INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C                   Z-ADD     *ZEROS        SCDBRR
     C                   Z-ADD     *ZEROS        SCUDAT
     C                   Z-ADD     *ZEROS        SCCEFD
     C                   Z-ADD     *ZEROS        SCCDDT
     C                   MOVEL     *BLANKS       SCCTYP
     C                   MOVEL     *BLANKS       SCCSTS
     C                   MOVEL     *BLANKS       SCCID#
     C*
     C     RRN           IFEQ      0
     C     *IN99         ANDEQ     *ON
     C*
     C* Initialize screen fields here
     C*
     C                   Z-ADD     1             RRN
     C                   SETON                                        06
     C                   WRITE     WDW02S
     C                   ENDIF
     C*
     C* TO POSITION SCREEN AND CURSOR
     C*
     C*** BEGIN CHG *** 10/20/05 **************************************
     C**********           Z-ADDRRN       #@$2
     C                   Z-ADD     1             #@$2
     C*** END   CHG *** 10/20/05 **************************************
     C*
     C* Initialize screen fields here for next load
     C*
     C                   Z-ADD     *ZEROS        SCDBRR
     C                   Z-ADD     *ZEROS        SCUDAT
     C                   Z-ADD     *ZEROS        SCCEFD
     C                   Z-ADD     *ZEROS        SCCDDT
     C                   MOVEL     *BLANKS       SCCTYP
     C                   MOVEL     *BLANKS       SCCSTS
     C                   MOVEL     *BLANKS       SCCID#
     C*
     C                   ENDSR
     C*====================================================================*
     C* OPT05 - DISPLAY ADL INFORMATION
     C*====================================================================*
     C     OPT05         BEGSR
     C*
     C                   CALL      'F8362R01'
     C                   PARM      @@LIC#        ##LIC#           12
     C                   PARM      SCDBRR        ##DBRR            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*====================================================================*
     C* XWDW02 = EXECUTE WDW 02 PROCESSING
     C*====================================================================*
     C     XWDW02        BEGSR
     C*
     C                   EXSR      INITB
     C*
     C     BCKY          SETLL     PBC01                              99
     C*
     C* CLEAR SUBFILE
     C*
     C                   SETON                                        04
     C                   WRITE     WDW02C
     C                   SETOFF                                       04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRTB         TAG
     C*
     C                   SETON                                        01
     C                   WRITE     WDW02C
     C                   WRITE     WDW02
     C                   EXFMT     WDW02C
     C                   SETOFF                                       20
     C                   SETOFF                                       01
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      *ON
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C     *IN32         CABEQ     *ON           STRTB                          NO ROLL IF EOF
     C                   MOVE      #@$2          RRN
     C                   EXSR      LOAD
     C                   GOTO      STRTB
     C                   ENDIF                                                  END 1 *IN18 ROLLUP
     C*
     C*****************************************************************
     C*   *INKL -  Cancel
     C*****************************************************************
     C     *INKL         IFEQ      *ON                                          Return
     C                   MOVEL     '*'           @@RTCD
     C                   GOTO      RETRNB
     C                   ENDIF                                                  END  *INKL       P
     C*
     C                   GOTO      STRTB
     C                   ENDIF                                                  *IN11
     C**************************************************************************
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSELB        TAG
     C                   READC     WDW02S                                 99
     C     *IN99         IFEQ      *OFF
     C*
     C* IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO   INDICATE
     C* A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C                   SELECT
     C     SCSEL         WHENEQ    ' '
     C                   UPDATE    WDW02S
     C                   GOTO      CKSELB
     C     SCSEL         WHENEQ    '5'
     C                   EXSR      OPT05
     C                   MOVEL     *BLANKS       SCSEL
     C                   UPDATE    WDW02S
     C                   GOTO      CKSELB
     C                   OTHER
     C                   SETON                                        20
     C                   UPDATE    WDW02S
     C                   GOTO      STRTB
     C                   ENDSL
     C                   ELSE
     C     *IN90         IFEQ      *ON
     C                   SETOFF                                       90
     C                   GOTO      STRTB
     C                   ENDIF
     C                   ENDIF
     C*
     C     RETRNB        TAG
     C*
     C                   ENDSR
