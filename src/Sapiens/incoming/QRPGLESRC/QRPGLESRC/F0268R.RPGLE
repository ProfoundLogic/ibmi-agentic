     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0268R - ADD A MEDICAL CASE
     H*****************************************************************
     H*  F0268R - ADD A MEDICAL CASE C:62 01/16/97                    *
     H*                                                               *
     H*  CALLED FROM: F0269R                                          *
     H*  CALLS TO:                                                    *
     H*               F0267R                                          *
     H*  VIA F0269C   F0270R                                          *
     H*               F0006R                                          *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*                                                                 *
     H*  06/09/20 C:212 Project : SY420
     H*                 Mask SSN
     H*
     H*  06/22/11 C:198 Project: 114MYS
     H*                 Added logic not to allow F6=Add for Non Workers Comp
     H*                 Grp.
     H*
     H*  08/08/00 C:62  Project: MC182
     H*                 IF USER PARMS CHANGE PASS SCREEN SUB#/FND TO
     H*                 F0260R.
     H*
     H*  01/11/99 C:31  Project: MC178
     H*                 V4: Bill Entry re-write
     H*                 1. Add error MC40158 - Group expired
     H*                                                               *
     H*  01/26/98 C:62  PROJECT: MC145                                *
     H*                 1) SA001R ADDED TO DEAL WITH ACCESS FLAGS     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1 : 71 AND 72.                      *
     H*                               73 (GROUP LINKED TO WCMS)       *
     H*                                                               *
     H*  GLOBAL INDICATOR IN STDALN : 16 (GROUP NOT LINKED TO WCMS).  *
     H*                                                               *
     H*****************************************************************
     FF0268F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FFMCMLB    IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 01/11/99 **************************************
     FFMFNL     IF   E           K DISK
     F*** END ADD *** 01/11/99 ****************************************
      /COPY SCOPYMT01D

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Add a Medical Case')
     D COMAPI          C                   CONST('FA007R')
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCSS#         ##SS#             9
     C                   PARM      SCACDT        ##ACDT            8 0
     C                   PARM      ' '           ##SDOI            1
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'A'           ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCSS#         ##SS#             9
     C                   PARM      SCACDT        ##ACDT            8 0
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C     ACSSPL        PLIST
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C*
     C     COMPL         PLIST
     C                   PARM      *ZEROS        ##CO#             3 0
     C                   PARM      *ZEROS        ##FND             3 0
     C                   PARM      *BLANKS       ##CONM           40
     C                   PARM      *BLANKS       ##FNAM           40
     C                   PARM      *ZEROS        ##SUB#            5 0
     C                   PARM      *ZEROS        ##YR              3 0
     C*
     C*  PARM LIST FOR FA701R
     C*
     C     INSPL         PLIST
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      ' '           ##OUTS            1
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C*** Beg Add *** 06/22/11 ************************************
     C     *LIKE         DEFINE    FNNWCG        XXNWCG
     C*** End Add *** 06/22/11 ************************************
     C*================================================================
     C*  KEY LISTS
     C*================================================================
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C*** BEGIN ADD *** 01/11/99 **************************************
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C     *LIKE         DEFINE    FNEXDT        XXDATE
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C*================================================================
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
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *ON
     C*
     C*  SOCIAL SECURITY NUMBER CAN'T BE LEFT BLANK.
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C     SCSS#         IFEQ      *BLANKS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C*  ACCIDENT DATE MUST BE VALID.
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C  N90              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 01/11/99 **************************************
     C********             ELSE
     C********
     C***WCMC*GROUP IS LINKED TO WCMS.
     C********
     C**N90***             SETON                     7390
     C********
     C****END*DEL *** 01/11/99 ****************************************
     C                   ENDIF
     C*
     C                   ENDSR
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
     C* KMDO - F13=CHG USER PARMS
     C*================================================================
     C     KMDO          BEGSR
     C*
     C                   CALL      'F0006R'
     C*
     C                   CALL      'FA013R'      COMPL
     C*
     C                   MOVE      ##SUB#        SCSUB#
     C*
     C     SMKY          CHAIN(N)  PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSBNM
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCGPNM
     C*
     C                   EXSR      STDALN
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      ##SUB#        SCSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSBNM
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCGPNM
     C*
     C     @@SS#         IFNE      *BLANKS
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     @@SS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'@@SS#':@@SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      @@ACDT        SCACDT
     C*
     C                   ELSE
     C*
     C   16              MOVE      *BLANKS       SCSS#
     C   16              MOVE      *ZEROS        SCACDT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         CMCO#
     C                   MOVE      ##SUB#        CMSUB#
     C                   MOVE      ##FND         CMFND
      ***** Beg Chg *** 06/09/20 *******************************************
     C****               MOVEL     SCSS#         CMSS#
      /Free
       CMSS# = UnMaskData('SSN':'SCSS#':scSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMACDT
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
     C*
     C     *INKM         CASEQ     *ON           KMDO
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
     C* API THAT RETURNS ACCESS FLAGS
     C*
     C                   CALL      'SA001R'      ACSSPL
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   EXSR      STDALN
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* STDALN - STAND ALONE PROCESSING
     C*================================================================
     C     STDALN        BEGSR
     C*
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      ##SUB#        #@SUB#
     C                   MOVE      ##FND         #@FND
     C*
     C                   CALL      'FA701R'      INSPL
     C*
     C     ##OUTS        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   EXSR      MVDBSC
     C*
     C                   WRITE     SCR01
     C*
     C                   SETON                                        7390
     C*
     C*
     C                   ENDIF
     C*** Beg Add *** 06/22/11 ************************************
     C                   MOVEL     ' '           XXNWCG
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNWCG        XXNWCG
     C                   ENDIF                                                  *in99 = *off
     C     XXNWCG        IFEQ      'Y'
     C                   SETOFF                                       16
     C                   EXSR      MVDBSC
     C                   WRITE     SCR01
     C                   SETON                                        7590
     C                   ENDIF
     C*** End Add *** 06/22/11 ************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C*
     C*          If group expired display error message
     C*          ------------------------------------------------------
     C     *IN16         IFEQ      *ON
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *ON
     C     FNEXDT        ORLE      XXDATE
     C     FNEXDT        ANDNE     0
     C                   SETOFF                                       16
     C                   EXSR      MVDBSC
     C                   WRITE     SCR01
     C                   SETON                                        7490
     C                   ENDIF                                                  *IN99 = On
     C                   ENDIF                                                  *IN16 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *ON
     C*
     C*  SET SS# FLAG.
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     SCSS#         KKSS#
      /Free
       kkSS# = UnMaskData('SSN':'SCSS#':scSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C     CMKY          CHAIN(N)  PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     *ON           SSF               1
     C*
     C                   ELSE
     C*
     C                   MOVEL     *OFF          SSF
     C*
     C                   ENDIF
     C*
     C*  IF SOCIAL SECURITY FLAG IS ON CALL WW SELECTED SS#.
     C*
     C     SSF           IFEQ      *ON
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       // unmask ssn to be passed
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   CALL      'F0267R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   MOVEL     *ON           OUTFLG
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       // mask ssn before being shown on screen
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ENDIF
     C*
     C                   ELSE
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       // unmask ssn to be passed
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   CALL      'F0260R'      ACDPL2
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   MOVEL     'A'           @@MODE
     C                   MOVEL     *ON           OUTFLG
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       // mask ssn before being shown on screen
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
