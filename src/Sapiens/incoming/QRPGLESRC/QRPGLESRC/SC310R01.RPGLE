     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC310R01 - CUSTOM GROUP ID CODES -  UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays Custom Group ID fields**
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC310R - Work with Custom Group ID Codes                **
     H**   CALLS TO:                                                 **
     H**     S2380R - Generic Table Search                           **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/22/98      PROJECT: DR048                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 12/18/24 C:342 Project : MC447 Phase 2
     H*                1.F10 Search disabled on the Group Field
     H*                DSPF Changes:
     H*                1.Changed Existing Group field into Typeahead
     H*
     H* 06/08/01 C:117 PROJECT: DR155
     H*                ADDING WARNING WINDOW BEFORE UPDATE
     H*                IF FORMAT IS CHANGED
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H**
     H** 10/01/98  C:94  PROJECT: DR048
     H**                 DISPLAY ERROR MESSAGE ON SECURITY LEVEL
     H**                 IF F10=SEARCH IS PRESSED
     H**
     H** 06/25/98  C:94  PROJECT: DR048
     H**                 DISPLAY ERROR MESSAGE IF SEARCH WAS USED ON
     H**                 A FIELD WITH NO TABLE BEHIND IT.
     H**
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**   50-57   SCR01 SC40002 Error - Blank Custom ID in Path(CSID)
     H**      60   Used for chaining
     H**      68   Used to signify a blank Custom ID Code
     H**   69-78   SCR01 SC40002 Error - Custom Code not valid (CSID)
     H**      79   SCR01 SC40002 Error - Duplicate record (CSID)
     H**      80   SCR01 SCM0503 Error - Required field (CSID)
     H**      81   SCR01 SCM0503 Error - Required field (DS40)
     H**      82   SCR01 SCM0549 Error - Required field (FLAG)
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     FSC310F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMCGP     UF A E             DISK
     F                                     RENAME(PCG01:DBRCDF)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********SMCGL   IF  E           K        DISK
     FSMCGL3    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     FSTGTP     IF   E             DISK
     FSTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     FFMFNP     IF   E             DISK    USROPN
     F***  END   ADD  ***  02/01/01  *********************************
     F***  END   ADD  ***  02/01/01  *********************************
     FSMPIP     UF   E             DISK
     F***  END   ADD  ***  06/08/01  *********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Custom Group ID Info-
     D                                     rmation')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Work With Generic Table Item parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
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
     C     CGKY          KLIST
     C                   KFLD                    SCGRP
     C                   KFLD                    SCCSID
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKGTKY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGTKY
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    SCGRP         XXGRP
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C     *LIKE         DEFINE    ##WCIF        WCMS
     C     *LIKE         DEFINE    ##MCIF        WCMC
     C***  END   ADD  ***  02/01/01  *********************************
     C*** BEG ADD *** 06/07/01 ****************************************
     C     *LIKE         DEFINE    CGRFMT        HLRFMT
     C*** END ADD *** 06/07/01 ****************************************
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
     C* CHECK CUSTOM GROUP ID CODE
     C*
     C  N90SCCSID        IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCCSID        IFNE      CPCSID
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCCSID    CHAINPCG01                60
     C     CGKY          CHAIN     PCG01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C  N60              SETON                                        7990
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C                   ENDIF                                                  SCPATH = BLANKS
     C****  TO PREVENT DUPLICATE RECORD WHILE IN ADD MODE  ****
     C     *IN40         IFEQ      *ON
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCCSID    CHAINPCG01                60
     C     CGKY          CHAIN     PCG01                              60
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
     C* CHECK FORMAT
     C*
     C  N90SCFRMT        IFNE      'A'
     C     SCFRMT        ANDNE     'B'
     C                   SETON                                        9082
     C                   LEAVE                                                              s
     C                   ENDIF
     C*** BEG ADD *** 06/08/01 ****************************************
     C* POP UP WARNING WINDOW IF FORMAT CHANGES
     C*
     C     SCFRMT        IFNE      HLRFMT
     C                   EXFMT     SCR03
     C                   ENDIF
     C     *INKL         IFEQ      *ON
     C***                  EXFMTSCR02
     C                   SETON                                        90
     C                   ELSE
     C                   EXSR      UPDPID
     C                   ENDIF
     C*
     C*** END ADD *** 06/08/01 ****************************************
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
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      *BLANKS       SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      *BLANKS       SCCSID
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCFRMT
     C                   MOVE      *BLANKS       SCSCLV
     C                   MOVE      *BLANKS       SCFMDS
     C*
     C     *IN40         IFEQ      *OFF
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   CGGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   CGCSID        SCCSID
     C                   MOVE(P)   CGCSID        CPCSID            2
     C                   MOVE(P)   CGDESC        SCDESC
     C                   MOVE(P)   CGSTAT        SCSTAT
     C                   MOVE(P)   CGRFMT        SCFRMT
     C*** BEG ADD *** 06/08/01 ****************************************
     C                   MOVE(P)   CGRFMT        HLRFMT
     C*** END ADD *** 06/08/01 ****************************************
     C                   MOVE(P)   CGRCDS        SCSCLV
     C*
     C     CGRFMT        IFNE      *BLANKS
     C                   MOVEL(P)  'CGID'        KKTBID
     C                   MOVEL(P)  CGRFMT        KKGTKY
     C     GTKY          CHAIN     PGT02                              60
     C                   MOVEL(P)  GTDESC        SCFMDS
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE(P)   SCGRP         CGGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVE      SCCSID        CGCSID
     C                   MOVE      SCDESC        CGDESC
     C                   MOVE      SCSTAT        CGSTAT
     C                   MOVE      SCFRMT        CGRFMT
     C                   MOVE      SCSCLV        CGRCDS
     C                   EXSR      MVUPDB
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        CGUSER
     C                   MOVEL     Q1PGM         CGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CGDT
     C                   MOVEL     DSTIME        CGTIME
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
     C*          Process Function 10: Search FORMAT
     C*----------------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C*** BEG ADD *** 06/25/98 ***********************************
     C                   SETOFF                                       101112
     C                   SETOFF                                       13
     C*** END ADD *** 06/25/98 ***********************************
     C*** BEG ADD *** 10/01/98 ***********************************
     C                   SETOFF                                       14
     C*** END ADD *** 10/01/98 ***********************************
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
     C                   SETON                                        71
     C                   ENDIF                                                  XXGRP<>*ZEROS
     C*_______________________________________________________________
     C*
     C                   OTHER
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   MOVE      *ZEROS        ##RRN
     C     HFLD          IFEQ      'SCFRMT'
     C*
     C                   MOVEL(P)  'CGID'        ##TBID
     C                   CALL      'S2380R'      ACDPL
     C     ##RRN         CHAIN     PGT01                              60
     C                   MOVEL     GTKEY         SCFRMT
     C                   MOVEL(P)  GTDESC        SCFMDS
     C*** BEG ADD *** 06/25/98 ************************************
     C                   SETON                                        11
     C                   ELSE
     C     HFLD          IFEQ      'SCCSID'
     C                   SETON                                        1090
     C                   ENDIF
     C     HFLD          IFEQ      'SCDESC'
     C                   SETON                                        121390
     C                   ENDIF
     C*** END ADD *** 06/25/98 ************************************
     C*** BEG ADD *** 10/01/98 ************************************
     C     HFLD          IFEQ      'SCSCLV'
     C                   SETON                                        1490
     C                   ENDIF
     C*** END ADD *** 10/01/98 ************************************
     C                   ENDIF
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  INKJ = On
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
     C     CGSTAT        IFEQ      'I'
     C                   MOVE      'A'           CGSTAT
     C                   ELSE
     C                   MOVE      'I'           CGSTAT
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
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*** BEG ADD *** 06/08/01 ****************************************
     C*================================================================
     C* UPDPID - Update path ID's
     C*================================================================
     C     UPDPID        BEGSR
     C*
     C* Read through SMPIP file and look at all the PICIxx fields and
     C* change the corresponding PIRFxx field to the new format.
     C*
     C                   READ      PPI01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C                   SETOFF                                       25
     C     PICI01        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF01
     C                   SETON                                        25
     C                   ENDIF                                                  PICI01=SCCSID
     C     PICI02        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF02
     C                   SETON                                        25
     C                   ENDIF                                                  PICI02=SCCSID
     C     PICI03        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF03
     C                   SETON                                        25
     C                   ENDIF                                                  PICI03=SCCSID
     C     PICI04        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF04
     C                   SETON                                        25
     C                   ENDIF                                                  PICI04=SCCSID
     C     PICI05        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF05
     C                   SETON                                        25
     C                   ENDIF                                                  PICI05=SCCSID
     C     PICI06        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF06
     C                   SETON                                        25
     C                   ENDIF                                                  PICI06=SCCSID
     C     PICI07        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF07
     C                   SETON                                        25
     C                   ENDIF                                                  PICI07=SCCSID
     C     PICI08        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF08
     C                   SETON                                        25
     C                   ENDIF                                                  PICI08=SCCSID
     C     PICI09        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF09
     C                   SETON                                        25
     C                   ENDIF                                                  PICI09=SCCSID
     C     PICI10        IFEQ      SCCSID
     C                   MOVEL     SCFRMT        PIRF10
     C                   SETON                                        25
     C                   ENDIF                                                  PICI10=SCCSID
     C*
     C                   MOVEL     Q1USER        PIUSER
     C                   MOVEL     Q1PGM         PIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PIDT
     C                   MOVEL     DSTIME        PITIME
     C*
     C   25              UPDATE    PPI01
     C                   READ      PPI01                                  61
     C                   ENDDO                                                  *IN61=*ON
     C*
     C                   ENDSR
     C*** END ADD *** 06/08/01 ****************************************
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        CGIUSR
     C                   MOVEL     Q1PGM         CGIPGM
     C                   MOVEL     DFTIME        CGITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CGIDT
     C                   WRITE     DBRCDF
     C                   FEOD      SMCGP
     C                   ENDSR
