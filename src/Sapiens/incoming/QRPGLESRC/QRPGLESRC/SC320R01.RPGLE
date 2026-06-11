     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC320R01 - PATH ID CODES UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the Path ID Codes file**
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC320R - Work with Path ID Codes                        **
     H**   CALLS TO:                                                 **
     H**     SC310R - Work with Custom ID Codes                      **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   12/16/97      PROJECT: DR050                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 12/18/24 C:342 Project : MC447 Phase 2
     H*                1.F10 Search disabled on the Group Field
     H*                DSPF Changes:
     H*                1.Changed Existing Group field into Typeahead
     H*
     H* 03/01/01 C:128 PROJECT: DR108
     H*                Change to allow correct F keys to display.
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H** 06/22/98 C:94  PROJECT: DR050
     H**                1) SEARCH MODE WILL TAKE YOU TO CORRECT FIELD
     H**                   WHEN RETURN TO PROGRAM
     H**                2) F3 FUNCTION TAKEN OUT OF SEARCH
     H**
     H** 04/05/99 C:02  PROJECT: SUPP
     H**                1. ADD CODE TO LOAD AND UPDATE PIRF01-10
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**   10-20   Cursor positioning
     H**      40   Add mode
     H**   50-57   SCR01 SC40002 Error - Blank Custom ID in Path(CSID)
     H**      60   Used for chaining
     H**      68   Used to signify a blank Custom ID Code
     H**   69-78   SCR01 SC40002 Error - Custom Code not valid (CSID)
     H**      79   SCR01 SC40002 Error - Duplicate record (PATH)
     H**      80   SCR01 SCM0503 Error - Required field (PATH)
     H**      81   SCR01 SCM0503 Error - Required field (DS40)
     H**      82   SCR01 SCM0503 Error - Required field (CSID)
     H**      84   SCR01 SCM0502 Error - Required field (FLAG)
     H**      85   SCR01 SCM0528 Error - Must have 10 custom IDs (PATH)
     H**      86   SCR01 SCM0526 Error - Path ID Code not valid (PATH)
     H**      87   SCR01 SCM0570 Error - Search not allowed for field
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     FSC320F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMPIP     UF A E             DISK
     F                                     RENAME(PPI01:DBRCDF)
     FSMCGL     IF   E           K DISK
     FSMPIL     IF   E           K DISK
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     FFMFNP     IF   E             DISK    USROPN
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYMT01D
     D*** BEGIN ADD *** 04/05/99 **************************************
     D*** END ADD *** 04/05/99 ****************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Path ID Codes')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D*** CUSTOM ID
     D*
     D                 DS
     D  CI                     1     20
     D                                     DIM(10)
     D  SCCS01                 1      2
     D  SCCS02                 3      4
     D  SCCS03                 5      6
     D  SCCS04                 7      8
     D  SCCS05                 9     10
     D  SCCS06                11     12
     D  SCCS07                13     14
     D  SCCS08                15     16
     D  SCCS09                17     18
     D  SCCS10                19     20
     D*
     D*** CUSTOM ID DESCRIPATION
     D*
     D                 DS
     D  CD                     1    400
     D                                     DIM(10)
     D  SCPDS1                 1     40
     D  SCPDS2                41     80
     D  SCPDS3                81    120
     D  SCPDS4               121    160
     D  SCPDS5               161    200
     D  SCPDS6               201    240
     D  SCPDS7               241    280
     D  SCPDS8               281    320
     D  SCPDS9               321    360
     D  SCPD10               361    400
     D*
     D*** CUSTOM ID IN DATABASE FILE
     D*
     D                 DS
     D  PI                     1     20
     D                                     DIM(10)
     D  PICI01                 1      2
     D  PICI02                 3      4
     D  PICI03                 5      6
     D  PICI04                 7      8
     D  PICI05                 9     10
     D  PICI06                11     12
     D  PICI07                13     14
     D  PICI08                15     16
     D  PICI09                17     18
     D  PICI10                19     20
     D*** BEGIN ADD *** 04/05/99 **************************************
     D                 DS
     D  SR                     1     10
     D                                     DIM(10)
     D  SCRF01                 1      1
     D  SCRF02                 2      2
     D  SCRF03                 3      3
     D  SCRF04                 4      4
     D  SCRF05                 5      5
     D  SCRF06                 6      6
     D  SCRF07                 7      7
     D  SCRF08                 8      8
     D  SCRF09                 9      9
     D  SCRF10                10     10

     I*** END ADD *** 04/05/99 ****************************************

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
     C     XXKY          KLIST
     C                   KFLD                    XXCS
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CGCSID        XXCS
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
     C* CHECK PATH ID CODE
     C*
     C  N90SCPATH        IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCPATH        IFNE      CPPATH
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCPATH    CHAINPPI01                60
     C     PIKY          CHAIN     PPI01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C  N60              SETON                                        7990
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C                   ENDIF                                                  SCPATH = BLANKS
     C****  TO PREVENT DUPLICATE RECORD WHILE IN ADD MODE  ****
     C     *IN40         IFEQ      *ON
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCPATH    CHAINPPI01                60
     C     PIKY          CHAIN     PPI01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C  N60              SETON                                        7990
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C*
     C* CHECK DESCRIPTION
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9081
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK CUSTOM ID 1 - REQUIRED
     C*
     C  N90SCCS01        IFEQ      *BLANKS
     C                   SETON                                        9082
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK ALLOW OUTSIDE RANGE - MUST BE Y OR N
     C*
     C  N90SCALOR        IFNE      'Y'
     C     SCALOR        ANDNE     'N'
     C                   SETON                                        9084
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C* CHECK CUSTOM ID 1 - 10 FOR VALIDITY
     C*
     C                   Z-ADD     1             A                 2 0
     C                   DO        10
     C     A             ADD       68            ER                2 0
     C                   MOVE      '0'           *IN(ER)
     C*************        MOVE '0'       *IN90
     C****  TO REMOVE ANY BLANK CUSTOM CODES IN THE PATH   *****
     C     CI(A)         IFEQ      *BLANKS
     C     A             ADD       1             D                 2 0
     C     11            SUB       D             C                 1 0
     C     D             SUB       1             B                 2 0
     C                   DO        C
     C                   MOVE      CI(D)         CI(B)
     C     B             ADD       1             B
     C     D             ADD       1             D
     C                   ENDDO                                                  C TIMES
     C****  TO REMOVE TWO OR MORE BLANK CUSOM CODES TOGETHER  *****
     C     CI(A)         IFEQ      *BLANKS
     C     A             SUB       1             A
     C                   ENDIF                                                  OLD CI,A+1  S
     C                   MOVE      *BLANK        CI(10)
     C                   ELSE
     C****  REGULAR PROCESSING -  NO BLANK CUSTOM CODES IN PATH  ****
     C                   MOVE      CI(A)         XXCS
     C     XXCS          CHAIN     PCG01                              60
     C  N60              MOVE      CGDESC        CD(A)
     C  N60              MOVE      CGRFMT        SR(A)                           04/05/99
     C   60              MOVE      '1'           *IN(ER)
     C   60              SETON                                        90
     C   60              LEAVE                                                              s
     C                   ENDIF
     C     A             ADD       1             A
     C                   ENDDO                                                  10 TIMES
     C*
     C* CHECK CUSTOM ID TO MAKE SURE THERE ARE 10 CUSTOM CODES
     C*
     C  N90SCNXPI        IFNE      *BLANKS
     C     CI(10)        ANDEQ     *BLANKS
     C                   SETON                                        9085
     C                   LEAVE                                                              s
     C                   ELSE
     C*
     C* CHECK NEXT PATH ID CODE TO BE SURE IT IS A VALID PATH
     C*
     C  N90SCNXPI        IFNE      *BLANKS
     C     CI(10)        ANDNE     *BLANKS
     C     SCNXPI        CHAIN     PPI01                              60
     C   60              SETON                                        8690
     C   60              LEAVE                                                              s
     C                   ENDIF
     C                   ENDIF
     C*
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
     C*
     C*          Check function keys
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      OTHFKS
     C                   ENDIF                                                   *INKJ = On
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   TIME                    DFDTTM
     C                   MOVE      *BLANKS       SCPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      *BLANKS       SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCNXPI
     C                   MOVE      *BLANKS       SCALOR
     C*** BEGIN ADD *** 04/05/99 **************************************
     C                   MOVE      *BLANKS       SCRF01
     C                   MOVE      *BLANKS       SCRF02
     C                   MOVE      *BLANKS       SCRF03
     C                   MOVE      *BLANKS       SCRF04
     C                   MOVE      *BLANKS       SCRF05
     C                   MOVE      *BLANKS       SCRF06
     C                   MOVE      *BLANKS       SCRF07
     C                   MOVE      *BLANKS       SCRF08
     C                   MOVE      *BLANKS       SCRF09
     C                   MOVE      *BLANKS       SCRF10
     C*** END   ADD *** 04/05/99 **************************************
     C*
     C                   DO        10            A                 2 0
     C                   MOVE      *BLANKS       CI(A)
     C                   MOVE      *BLANKS       CD(A)
     C                   ENDDO
     C*
     C     *IN40         IFEQ      *OFF
     C                   MOVE(P)   PIPATH        SCPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   PIGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   PIPATH        CPPATH            8
     C                   MOVE(P)   PIDESC        SCDESC
     C                   MOVE(P)   PISTAT        SCSTAT
     C                   MOVE(P)   PINXPI        SCNXPI
     C                   MOVE(P)   PIALOR        SCALOR
     C*** BEGIN ADD *** 04/05/99 **************************************
     C                   MOVE      PIRF01        SCRF01
     C                   MOVE      PIRF02        SCRF02
     C                   MOVE      PIRF03        SCRF03
     C                   MOVE      PIRF04        SCRF04
     C                   MOVE      PIRF05        SCRF05
     C                   MOVE      PIRF06        SCRF06
     C                   MOVE      PIRF07        SCRF07
     C                   MOVE      PIRF08        SCRF08
     C                   MOVE      PIRF09        SCRF09
     C                   MOVE      PIRF10        SCRF10
     C*** END ADD *** 04/05/99 ****************************************
     C*
     C* MOVE CUSTOM ID 1 - 10
     C*
     C                   DO        10            A
     C     PI(A)         IFNE      *BLANKS
     C                   MOVE      PI(A)         CI(A)
     C                   MOVE      PI(A)         XXCS
     C     XXCS          CHAIN     PCG01                              60
     C  N60              MOVE      CGDESC        CD(A)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      SCPATH        PIPATH
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   SCGRP         PIGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      SCDESC        PIDESC
     C                   MOVE      SCSTAT        PISTAT
     C*
     C                   DO        10            A
     C                   MOVE      CI(A)         PI(A)
     C                   ENDDO
     C*
     C                   MOVE      SCNXPI        PINXPI
     C                   MOVE      SCALOR        PIALOR
     C*** BEGIN ADD *** 04/05/99 **************************************
     C                   MOVE      SCRF01        PIRF01
     C                   MOVE      SCRF02        PIRF02
     C                   MOVE      SCRF03        PIRF03
     C                   MOVE      SCRF04        PIRF04
     C                   MOVE      SCRF05        PIRF05
     C                   MOVE      SCRF06        PIRF06
     C                   MOVE      SCRF07        PIRF07
     C                   MOVE      SCRF08        PIRF08
     C                   MOVE      SCRF09        PIRF09
     C                   MOVE      SCRF10        PIRF10
     C*** END ADD *** 04/05/99 ****************************************
     C                   EXSR      MVUPDB
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        PIUSER
     C                   MOVEL     Q1PGM         PIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PIDT
     C                   MOVEL     DSTIME        PITIME
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
     C*          Process Function 10: Search Custom ID
     C*----------------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C*** BEG ADD *** 06/22/98 ********************************
     C                   SETOFF                                       101112
     C                   SETOFF                                       131415
     C                   SETOFF                                       161718
     C                   SETOFF                                       1920
     C*** END ADD *** 06/22/98 ********************************
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
     C*_______________________________________________________________
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C***  Begin Change *  03/01/01  *********************************
      *
     C     HFLD          IFEQ      'SCNXPI'                                     F10 on Path
      *
     C                   MOVE      *ON           *IN87                          No search
     C                   MOVE      *ON           *IN20                          PC
      *
     C                   ELSE
      *
     C***  End   Change *  03/01/01  *********************************
      *
     C                   DO
     C*** SET THE INDEX ***
     C                   DO        10            S                 2 0
     C                   MOVE      S             R                 2
     C     'SCCS'        CAT       R             CMPFLD            6
     C     HFLD          IFEQ      CMPFLD
     C                   LEAVE
     C                   ENDIF                                                  CI,A = HFLD
     C                   ENDDO                                                  DO 10 TIMES
     C*
     C                   MOVE      *BLANKS       QQCS              2
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'SC310R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    QQCS
     C                   PARM                    QQRTC
     C*
     C*** BEG DEL *** 06/22/98 ********************************
     C***        QQRTC     IFEQ 'A'
     C***                  MOVE '1'       OUTFLG
     C***                  LEAVE
     C***                  ENDIF                           RTC = 'A'
     C*** END DEL *** 06/22/98 ********************************
     C*
     C*** BEG ADD *** 06/22/98 ********************************
     C     QQRTC         IFEQ      *BLANK
     C                   SELECT
     C     S             WHENEQ    1
     C                   SETON                                        1090
     C     S             WHENEQ    2
     C                   SETON                                        1190
     C     S             WHENEQ    3
     C                   SETON                                        1290
     C     S             WHENEQ    4
     C                   SETON                                        1390
     C     S             WHENEQ    5
     C                   SETON                                        1490
     C     S             WHENEQ    6
     C                   SETON                                        1590
     C     S             WHENEQ    7
     C                   SETON                                        1690
     C     S             WHENEQ    8
     C                   SETON                                        1790
     C     S             WHENEQ    9
     C                   SETON                                        1890
     C     S             WHENEQ    10
     C                   SETON                                        1990
     C                   ENDSL
     C*** END ADD *** 06/22/98 ********************************
     C                   LEAVE
     C                   ENDIF                                                  RTC = BLANK
     C*
     C                   SETON                                        90
     C     QQRTC         IFEQ      '*'
     C*** BEG ADD *** 06/22/98 ********************************
     C                   SELECT
     C     S             WHENEQ    1
     C                   SETON                                        11
     C     S             WHENEQ    2
     C                   SETON                                        12
     C     S             WHENEQ    3
     C                   SETON                                        13
     C     S             WHENEQ    4
     C                   SETON                                        14
     C     S             WHENEQ    5
     C                   SETON                                        15
     C     S             WHENEQ    6
     C                   SETON                                        16
     C     S             WHENEQ    7
     C                   SETON                                        17
     C     S             WHENEQ    8
     C                   SETON                                        18
     C     S             WHENEQ    9
     C                   SETON                                        19
     C     S             WHENEQ    10
     C                   SETON                                        20
     C                   ENDSL
     C*** END ADD *** 06/22/98 ********************************
     C                   MOVEL     QQCS          CI(S)
     C                   MOVEL     QQCS          XXCS
     C     XXCS          CHAIN     PCG01                              60
     C                   MOVEL(P)  CGDESC        CD(S)
     C                   SETON                                        89
     C                   LEAVE
     C                   ENDIF                                                  RTC = '*'
     C*
     C                   ENDDO
     C*
     C***  Begin Change *  03/01/01  *********************************
     C                   ENDIF                                                  F10 on Path
     C***  End   Change *  03/01/01  *********************************
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   ENDSL
     C*
     C                   SETON                                            90
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDIF                                                  INKJ = On
     C*
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
     C     PISTAT        IFEQ      'I'
     C                   MOVE      'A'           PISTAT
     C                   ELSE
     C                   MOVE      'I'           PISTAT
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
     C                   MOVEL     Q1USER        PIIUSR
     C                   MOVEL     Q1PGM         PIIPGM
     C                   MOVEL     DFTIME        PIITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PIIDT
     C                   WRITE     DBRCDF
     C                   FEOD      SMPIP
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
