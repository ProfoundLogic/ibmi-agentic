     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC325R01 - PROGRAM/PATH ID CODES UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the Program/Path ID   **
     H**     codes file.                                             **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC325R - Work with Program/Path ID Codes                **
     H**   CALLS TO:                                                 **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   12/12/97      PROJECT: DR051                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 12/18/24 C:342 Project : MC447 Phase 2
     H*                1.F10 Search disabled on the Group Field
     H*                DSPF Changes:
     H*                1.Changed Existing Group field into Typeahead
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**      60   Used for chaining
     H**      74   SCR01 SC40002 Error - Duplicate record (PGMN)
     H**      77   SCR01 SCM0503 Error - Invalid field (PATH)
     H**      79   SCR01 SCM0503 Error - Required field (PGMN)
     H**      80   SCR01 SCM0503 Error - Required field (PATH)
     H**      81   SCR01 SCM0503 Error - Required field (DS40)
     H**      84   SCR01 SCM0631 Error - Must be A, I, or blank (STAT)
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     FSC325F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMPPP     UF A E             DISK
     F                                     RENAME(PPP01:DBRCDF)
     FSMPPL     IF   E           K DISK
     FSMPIL     IF   E           K DISK
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     FFMFNP     IF   E             DISK    USROPN
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Program/Path ID code-
     D                                     s')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

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
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     WCMSPL        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM      *ZERO         ##FND             3
     C*
     C     WCMCPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     PIKY          KLIST
     C                   KFLD                    SCPATH
     C                   KFLD                    SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     PPKY          KLIST
     C                   KFLD                    XXPGMN
     C                   KFLD                    XXPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*================================================================
     C*14*LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PPPGMN        XXPGMN
     C     *LIKE         DEFINE    PPPATH        XXPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    SCGRP         XXGRP
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C     *LIKE         DEFINE    ##WCIF        WCMS
     C     *LIKE         DEFINE    ##MCIF        WCMC
     C***  END   ADD  ***  02/01/01  *********************************
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
     C                   DO
     C*
     C* CHECK PPOGRAM
     C*
     C  N90SCPGMN        IFEQ      *BLANKS
     C                   SETON                                        9079
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C****       SCPGMN    IFNE CPPGMN
     C****       SCPGMN    CHAINPPP01                60
     C**N60                SETON                     7490
     C**N60                LEAVE                                       s
     C****                 ENDIF
     C                   ENDIF
     C****  TO PREVENT DUPLICATE RECORD WHILE IN ADD MODE  ****
     C     *IN40         IFEQ      *ON
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCPGMN    CHAINPPP01                60
     C     PPKY          CHAIN     PPP01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C  N60              SETON                                        7490
     C  N60              LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK PATH ID CODE
     C*
     C  N90SCPATH        IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C     SCPATH        CHAIN     PPI01                              60
     C   60
     CAN 40              SETON                                        7790
     C   60
     CAN 40              LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK DESCRIPTION
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9081
     C                   LEAVE                                                              s
     C                   ENDIF
     C                   ENDDO
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C     *IN40         IFEQ      *ON
     C                   MOVE      *BLANKS       SCPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      *BLANKS       SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      *BLANKS       SCPTDS
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPGMN
     C                   MOVE      'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      PPPATH        SCPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   PPGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      PPDESC        SCDESC
     C                   MOVE      PPPGMN        SCPGMN
     C                   MOVE      PPPGMN        CPPGMN            8
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCPATH    CHAINPPI01                60
     C     PIKY          CHAIN     PPI01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C                   MOVEL(P)  PIDESC        SCPTDS
     C                   MOVE      PPSTAT        SCSTAT
     C                   ENDIF                                                  *IN40
     C*
     C                   TIME                    DFDTTM
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      SCPATH        PPPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   SCGRP         PPGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      SCDESC        PPDESC
     C                   MOVE      SCPGMN        PPPGMN
     C                   MOVE      SCSTAT        PPSTAT
     C                   EXSR      MVUPDB
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPDT
     C                   MOVEL     DSTIME        PPTIME
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
     C*******************
     C* F10=SEARCH      *
     C*******************
     C     *INKJ         IFEQ      '1'
     C*** BEG ADD *** 06/23/98 *************************************
     C                   SETOFF                                       1011
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCGRP'
     C     USEGRP        ANDEQ     'Y'
     C*
     C                   CLEAR                   XXGRP
     C                   CLEAR                   FNFND
     C*
     C                   SELECT
     C     WCMS          WHENEQ    'Y'
     C                   MOVE      ##CO#         #@CO#
     C                   CALL      'W0010R'      WCMSPL                 99
     C                   MOVE      ##FND         XXGRP
     C     WCMC          WHENEQ    'Y'
     C                   OPEN      FMFNP
     C******** Beg Del ***** 12/18/24 ***************************
     C*****              CALL      'F0060R'      WCMCPL                 99
     C*****##RRN         CHAIN     FMFNP                              60
     C*****              MOVE      FNFND         XXGRP
     C******** End Del ***** 12/18/24 ***************************
     C                   CLOSE     FMFNP
     C                   ENDSL
     C*
     C     XXGRP         IFNE      *ZEROS
     C                   MOVE      XXGRP         SCGRP
     C                   ELSE
     C                   SETON                                        73
     C                   ENDIF                                                  XXGRP<>*ZEROS
     C*_______________________________________________________________
     C*
     C                   OTHER
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     HFLD          IFEQ      'SCPGMN'
     C                   SETON                                        7690
     C                   ENDIF
     C     HFLD          IFEQ      'SCDESC'
     C                   SETON                                        751190
     C                   ENDIF
     C*** END ADD *** 06/23/98 *************************************
     C*
     C     HFLD          IFEQ      'SCPATH'
     C*
     C                   MOVE      *BLANKS       ##PATH
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'SC320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##PATH            8
     C                   PARM                    ##RTCD            1
     C*
     C                   MOVEL     ##PATH        SCPATH
     C     SCPATH        CHAIN     PPI01                              60
     C                   MOVEL(P)  PIDESC        SCPTDS
     C*
     C                   ENDIF                                                  HFLD
     C*** BEG ADD *** 06/23/98 **************************************
     C     SCPATH        IFEQ      *BLANKS
     C                   SETON                                        1090
     C                   ELSE
     C                   SETON                                        1190
     C                   ENDIF
     C*** END ADD *** 06/23/98 **************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *INKJ
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
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C*
     C                   CALL      'SA001R'                             99
     C     WCMS          PARM                    ##WCIF            1            WC INST
     C     WCMC          PARM                    ##MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     PPSTAT        IFEQ      'I'
     C                   MOVE      'A'           PPSTAT
     C                   ELSE
     C                   MOVE      'I'           PPSTAT
     C                   ENDIF
     C                   EXSR      MVUPDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        PPIUSR
     C                   MOVEL     Q1PGM         PPIPGM
     C                   MOVEL     DFTIME        PPITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPIDT
     C                   WRITE     DBRCDF
     C                   FEOD      SMPPP
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
