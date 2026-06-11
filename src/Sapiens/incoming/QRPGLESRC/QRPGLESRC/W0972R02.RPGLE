     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     H*
     H*****************************************************************
     H/TITLE W0972R02 - Add-Change-Delete Auto-DARTS Control File
     H*****************************************************************
     H*  DESCRIPTION:  This program updates a WC Auto-DARTS Control
     H*                record.
     H*
     H*****************************************************************
     H*  CREATED: 02/10/05       C:58        PROJECT: WC1128
     H*
     H*---------------------------------------------------------------
     H*  REVISIONS:
     H*
     H* 08/13/10  C:235  Project:SY190(SEQ#24)                        *
     H*                  Client Specific Flag is missing for the      *
     H*                  changes done under proj 25WQO
     H*
     F* 02/01/10  C:247  Project: 25WQO:                              *
     F*                  New Auto diary features - New fields added   *
     F*                  (NCCI codes, # of weeks paid, % of group ret *
     F*                  and Total paid                               *
     H*---------------------------------------------------------------
     H*  CALLED FROM:
     H*    W0010R   - WORK WITH GROUPS
     H*****************************************************************
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*
     F*****************************************************************
     F*       F    I    L    E    S
     F*****************************************************************
     FW0972F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Auto DARTS Control     PF(for maintaining record)
     FWMADP     UF A E             DISK
     F                                     RENAME(PAD01:DBRCDF)
     F*
     FWMADL     IF   E           K DISK
     F                                     RENAME(PAD01:PAD02)
     F*
     F*  Generic Table File
     FWTGTL     IF   E           K DISK
     F*
     F*  WC Users
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     F*
     F*  DARTS Remark Table File
     FSTCTL1    IF   E           K DISK
     F*                                                                _
     F*  Payment value 1
     FWTABL     IF   E           K DISK
     F*
     F*  Group Master File.
     FWMFNP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Auto DARTS Control')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Add/change/display program parameter list
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      1             ##VIEW            1 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C     GTKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  REMARK CODE TABLE FILE
     C     RMKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C*  AUTODARTS CONTROL
     C     ADKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    SCKEY
     C                   KFLD                    SCRMCD
     C*
     C*  TABLE FILE KEY
     C     TBKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    XXKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C*
     C***** BEG ADD ********** 02/01/10 *******************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '025'
     C                   SETON                                        35
     C                   ENDIF
     C***** END ADD ********** 02/01/10 *******************************
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ADSTAT        IFEQ      'I'
     C                   MOVE      'A'           ADSTAT
     C*
     C                   ELSE
     C                   MOVE      'I'           ADSTAT
     C                   ENDIF                                                  ADSTAT=I
     C*
     C                   MOVEL(P)  Q1USER        ADUSER
     C                   MOVEL(P)  Q1PGM         ADPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADDT
     C*
     C                   MOVEL     DSTIME        ADTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
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
     C     *IN40         IFEQ      *ON
     C     SCKEY         IFEQ      *BLANK
     C                   SETON                                        71  90
     C                   ELSE
     C                   MOVEL(P)  'AXKY'        XXTBID
     C                   MOVEL(P)  SCKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e KEY=BLANK
     C*
     C  N90SCRMCD        IFEQ      *BLANK
     C                   SETON                                        73  90
     C*
     C                   ELSE
     C                   MOVEL(P)  SCRMCD        XXRMCD
     C     RMKY          CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ELSE                                                   X 60 ON
     C                   MOVEL(P)  CTDESC        SCDESC
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e RMCD=BLANK
     C*
     C  N90ADKY          CHAIN     PAD02                              60
     C     *IN60         IFEQ      *OFF                                          ALREADY THERE
     C                   SETON                                        75  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e 40 ON
     C*
     C*      ----------------------------------------------------
     C*
     C  N90SCDESC        IFEQ      *BLANK
     C                   SETON                                        76  90
     C                   ENDIF                                                  e DESC = BLANK
     C*
     C  N90SCSLVL        IFEQ      *ZERO
     C*******              DIV  SCSLVL    CTDSLV           - dump -
     C                   Z-ADD     CTDSLV        SCSLVL
     C                   ENDIF                                                  e SLVL = ZERNO
     C*
     C*
     C  N90SCOCUR        IFEQ      *BLANK
     C                   SETON                                        82  90
     C*
     C                   ELSE                                                   x OCUR = BLANK
     C                   MOVEL(P)  'AXOC'        XXTBID
     C                   MOVEL(P)  SCOCUR        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        83  90
     C                   ELSE                                                   x 60 ON
     C                   MOVEL(P)  GTDESC        SCDOCD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e OCUR = BLANK
     C*
     C*    Get remark table record
     C     *IN90         IFEQ      *OFF                                                     RE
     C                   MOVEL(P)  SCRMCD        XXRMCD
     C     RMKY          CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        80  90
     C                   Z-ADD     0             CTDSLV
     C                   MOVEL(P)  *BLANKS       CTDESC
     C                   MOVEL(P)  *BLANKS       CTFL01
     C                   ELSE                                                   e IN60 ON
     C*
     C*    Diary fields apply only if diary remark type.
     C     CTFL01        IFEQ      'Y'
     C*
     C*    Can't have user code and specific user
     C  N90SCDIUS        IFNE      *BLANK
     C     SCUSR         ANDNE     *BLANK
     C                   SETON                                        77  90
     C                   ENDIF                                                  e DIUS<>BLANK
     C*
     C*
     C*    Must have user code or specific user
     C  N90SCDIUS        IFEQ      *BLANK
     C     SCUSR         ANDEQ     *BLANK
     C                   SETON                                        81  90
     C                   ENDIF                                                  e DIUS<>BLANK
     C*
     C*
     C  N90SCDIUS        IFNE      *BLANK
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCDIUS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        78  90
     C                   ELSE                                                   x 60 ON
     C                   MOVEL(P)  GTDESC        SCDUSD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e DIUS=BLANK
     C*
     C  N90SCUSR         IFNE      *BLANK
     C     SCUSR         CHAIN     PUP02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        86  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e USR=BLANK
     C*
     C  N90SCDIAU        IFNE      *BLANK
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCDIAU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        79  90
     C                   ELSE                                                   X 60 ON
     C                   MOVEL(P)  GTDESC        SCDUS2
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e DIAU<>BLANK
     C*
     C  N90SCDIDT        IFEQ      *BLANK
     C                   SETON                                        84  90
     C*
     C                   ELSE
     C                   MOVEL(P)  'DIDT'        XXTBID
     C                   MOVEL(P)  SCDIDT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        85  90
     C*
     C                   ELSE                                                   x 60 ON
     C                   MOVEL(P)  GTDESC        SCDIDD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e DIDT=BLANK
     C*  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  _
     C*   Or diary fields must be null.
     C                   ELSE                                                   X FL01<>'Y'
     C     SCDIUS        IFNE      *BLANK                                                   RE
     C     SCUSR         ORNE      *BLANK                                                   RE
     C     SCDIAU        ORNE      *BLANK                                                   RE
     C     SCDIDY        ORNE      *ZERO                                                    RE
     C     SCDIDT        ORNE      *BLANK                                                   RE
     C                   SETON                                        87  90
     C                   ENDIF                                                  e DIUS<>' '
     C                   ENDIF                                                  E FL01<>'Y'
     C                   ENDIF                                                  e IN60 OFF
     C                   ENDIF                                                  e IN90 OFF
     C*
     C*  WE HAVE PASSED ALL EDITS
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C  N90SCLTFL        IFNE      *BLANK                                                   RE
     C     SCLTFL        ANDNE     'I'                                                      RE
     C     SCLTFL        ANDNE     'M'                                                      RE
     C     SCLTFL        ANDNE     'R'                                                      RE
     C     SCLTFL        ANDNE     'N'                                                      RE
     C                   SETON                                        71  90
     C                   ENDIF                                                  e LTFL<>' '
     C*
     C  N90SCCSTS        IFNE      *BLANK                                                   RE
     C     SCCSTS        ANDNE     'O'                                                      RE
     C     SCCSTS        ANDNE     'C'                                                      RE
     C                   SETON                                        72  90
     C                   ENDIF                                                  e CSTS<>' '
     C*
     C  N90SCCIND        IFNE      *BLANK
     C                   MOVEL(P)  'AXCI'        XXTBID
     C                   MOVEL(P)  SCCIND        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ELSE                                                   X 60 ON
     C                   MOVEL(P)  GTDESC        SDCIND
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e CIND<>' '
     C*
     C*  TEST THE PAYMENT VALUE 1 CODE FOR VALIDITY AND ACTIVITY
     C*
     C  N90SCBR2         IFNE      *BLANK
     C                   MOVEL(P)  SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ELSE                                                   X 60 ON
     C                   MOVEL(P)  B2DESC        SDBR2
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e  BR2<>' '
     C*
     C****** BEG ADD ********* 02/01/10 *******************************
      **** PAYMENT VALUE 1  TIBS CODES
     C     CLTID         IFEQ      '025'
     C  N90SCBR2A        IFNE      *BLANK
     C                   MOVEL(P)  SCBR2A        XXKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        94  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e  BR2<>' '
     C*
     C  N90SCBR2B        IFNE      *BLANK
     C                   MOVEL(P)  SCBR2B        XXKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        95  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e  BR2<>' '
     C*
     C  N90SCBR2C        IFNE      *BLANK
     C                   MOVEL(P)  SCBR2C        XXKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        96  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e  BR2<>' '
     C*
     C  N90SCBR2D        IFNE      *BLANK
     C                   MOVEL(P)  SCBR2D        XXKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        97  90
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e  BR2<>' '
     C*
     C                   ENDIF                                                  CLTID = 025
     C****** END ADD ********* 02/01/10 *******************************
     C*  TEST THE RESERVE CODE FOR VALIDITY AND ACTIVITY
     C  N90SCRECD        IFNE      *BLANK
     C                   MOVEL(P)  SCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        75  90
     C*
     C                   ELSE                                                   X 60 ON
     C                   MOVEL(P)  RTDESC        SDRECD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e RECD<>' '
     C*
     C******* BEG ADD ********** 02/01/10 ****************************
     C* IF AN NCCI CODE IS ENTERED IT MUST BE VALID
     C  N90SCNCC1        IFNE      *BLANK
     C                   MOVEL(P)  SCNCC1        XXKEY
     C                   MOVE(P)   '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C     *IN90         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCNCC2        IFNE      *BLANK
     C                   MOVEL(P)  SCNCC2        XXKEY
     C                   MOVE(P)   '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C     *IN90         IFEQ      '1'
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCNCC3        IFNE      *BLANK
     C                   MOVEL(P)  SCNCC3        XXKEY
     C                   MOVE(P)   '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C     *IN90         IFEQ      '1'
     C                   MOVE      '1'           *IN93
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C* IF # OF WEEKS PAID ENTERED, PV1 VALUE MUST BE REQUIRED
     C******* BEG ADD ********** 08/13/10 ****************************
     C     *IN35         IFEQ      *ON
     C******* END ADD ********** 08/13/10 ****************************
     C  N90SCWKPD        IFNE      *ZEROS
     C     SCBR2A        ANDEQ     *BLANKS
     C     SCBR2B        ANDEQ     *BLANKS
     C     SCBR2C        ANDEQ     *BLANKS
     C     SCBR2D        ANDEQ     *BLANKS
     C                   SETON                                        88  90
     C                   ENDIF
     C******* END ADD ********** 02/01/10 ****************************
     C******* BEG ADD ********** 08/13/10 ****************************
     C                   ENDIF
     C******* END ADD ********** 08/13/10 ****************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for screen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*===============================================================
     C* KJDO -  F10 - SEARCH PROCESSING
     C*===============================================================
     C     KJDO          BEGSR
     C                   MOVEA     '00000'       *IN(41)                        41-45
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
     C*
     C                   SELECT
     C*   Key field.
     C     HFLD          WHENEQ    'SCKEY '
     C                   MOVEL(P)  'AXKY'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN41                          POS CUSR
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCDESC
     C                   END
     C                   END
     C*
     C*   Remark code
     C     HFLD          WHENEQ    'SCRMCD'
     C                   CALL      'SC305R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM      *BLANK        QQRMCD            5
     C                   PARM      *BLANK        QQDESC           40
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  RTC = 'A'
     C*
     C     QQRTC         IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF                                                  RTC = 'B'
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVEL(P)  QQRMCD        XXRMCD
     C     RMKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVE      *OFF          *IN42                          POS CUSR
     C                   MOVEL(P)  CTRMCD        SCRMCD
     C                   END
     C                   ENDIF                                                  RTC = '*'
     C*
     C*   Occurence code
     C     HFLD          WHENEQ    'SCOCUR'
     C                   MOVEL(P)  'AXOC'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN42                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCOCUR
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCDOCD
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   MOVE      *OFF          *IN42
     C                   END
     C                   END
     C*
     C*
     C*   Diary user
     C     HFLD          WHENEQ    'SCDIUS'
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN43                          POS CUSR
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCDIUS
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCDUSD
     C                   MOVE      *ON           *IN44                          POS CUSR
     C                   MOVE      *OFF          *IN43
     C                   END
     C                   END
     C*
     C*   STAFF CODE SEARCH - SPECIFIC USER
     C*
     C     HFLD          WHENEQ    'SCUSR '
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPUSR         SCUSR
     C                   MOVE      *OFF          *IN44
     C                   MOVE      *ON           *IN45
     C                   ELSE
     C                   MOVE      *ON           *IN44
     C                   MOVE      *OFF          *IN45
     C                   END
     C*
     C*   Alternate diary user
     C     HFLD          WHENEQ    'SCDIAU'
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN45                          POS CUSR
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCDIAU
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCDUS2
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   MOVE      *OFF          *IN45
     C                   END
     C                   END
     C*
     C*   Basis date code
     C     HFLD          WHENEQ    'SCDIDT'
     C                   MOVEL(P)  'DIDT'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN46                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCDIDT
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCDIDD
     C                   MOVE      *OFF          *IN46
     C                   END
     C                   END
     C*
     C*   - = - = - = - = - = - = - = - = - = - = - = - = - = - = - =
     C*
     C*   Claim indicator
     C     HFLD          WHENEQ    'SCCIND'
     C                   MOVEL(P)  'AXCI'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      *ON           *IN41                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCCIND
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SDCIND
     C                   MOVE      *ON           *IN42                          POS CUSR
     C                   MOVE      *OFF          *IN41
     C                   END
     C                   END
     C*
     C*             PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          WHENEQ    'SCBR2'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR2
     C****** BEG ADD ******* 02/01/10 *********************************
     C     CLTID         IFEQ      '025'
     C                   SETON                                        50
     C                   ELSE
     C****** END ADD ******* 02/01/10 *********************************
     C                   SETON                                        44
     C****** BEG ADD ******* 02/01/10 *********************************
     C                   ENDIF
     C****** END ADD ******* 02/01/10 *********************************
     C*
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        43
     C                   ENDIF                                                  *INKL = Off
     C*
     C****** BEG ADD ******* 02/01/10 *********************************
      *
     C     HFLD          WHENEQ    'SCBR2A'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR2A
     C                   SETON                                        51
     C                   SETOFF                                       50
     C*
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        50
     C                   SETOFF                                       50
     C                   ENDIF                                                  *INKL = Off
      *
     C     HFLD          WHENEQ    'SCBR2B'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR2B
     C                   SETON                                        52
     C                   SETOFF                                       51
     C*
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        51
     C                   SETOFF                                       52
     C                   ENDIF                                                  *INKL = Off
      *
     C     HFLD          WHENEQ    'SCBR2C'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR2C
     C                   SETON                                        53
     C                   SETOFF                                       52
     C*
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        52
     C                   SETOFF                                       53
     C                   ENDIF                                                  *INKL = Off
      *
     C     HFLD          WHENEQ    'SCBR2D'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR2D
     C                   SETON                                        54
     C                   SETOFF                                       53
     C*
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        53
     C                   SETOFF                                       54
     C                   ENDIF                                                  *INKL = Off
      *
     C****** END ADD ******* 02/01/10 *********************************
     C*             RESERVE CODE
     C*             ---------------------------------------------------
     C     HFLD          WHENEQ    'SCRECD'
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXKEY
     C*
     C                   MOVEL     XXKEY         SCRECD
     C                   MOVE      *ON           *IN45
     C*
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN44
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN45
     C                   ENDIF
     C*
     C********** BEG ADD *********** 02/01/10 *************************
     C* SEARCH NCCI CODES
     C     HFLD          WHENEQ    'SCNCC1'
     C                   MOVEL(P)  '  NC'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCNCC1
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN55
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN56
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCNCC2'
     C                   MOVEL(P)  '  NC'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCNCC2
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN56
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN57
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCNCC3'
     C                   MOVEL(P)  '  NC'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCNCC3
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN57
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN43
     C                   ENDIF
      *
     C********** END ADD *********** 02/01/10 *************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  ADKEY         SCKEY
     C                   MOVEL(P)  ADRMCD        SCRMCD
     C                   MOVEL(P)  ADDESC        SCDESC
     C                   Z-ADD     ADSLVL        SCSLVL
     C                   MOVEL(P)  ADDIUS        SCDIUS
     C                   MOVEL(P)  ADDIAU        SCDIAU
     C                   MOVEL(P)  ADUSR         SCUSR
     C                   Z-ADD     ADDIDY        SCDIDY
     C                   MOVEL(P)  ADDIDT        SCDIDT
     C                   MOVEL(P)  ADOCUR        SCOCUR
     C                   MOVEL(P)  ADLTFL        SCLTFL
     C                   MOVEL(P)  ADCSTS        SCCSTS
     C                   Z-ADD     ADAMT         SCAMT
     C                   MOVEL(P)  ADRECD        SCRECD
     C                   MOVEL(P)  ADBR2         SCBR2
     C                   Z-ADD     ADCNT         SCCNT
     C                   MOVEL(P)  ADCIND        SCCIND
     C                   MOVEL(P)  ADJRST        SCJRST
     C*
     C*   Now get Table File descriptions.
     C                   MOVEL(P)  *BLANK        SCDOCD
     C                   MOVEL(P)  *BLANK        SCDUSD
     C                   MOVEL(P)  *BLANK        SCDUS2
     C                   MOVEL(P)  *BLANK        SCDIDD
     C                   MOVEL(P)  *BLANK        SDCIND
     C                   MOVEL(P)  *BLANK        SDBR2
     C                   MOVEL(P)  *BLANK        SDRECD
     C*
     C     SCOCUR        IFNE      *BLANK
     C                   MOVEL(P)  'AXOC'        XXTBID
     C                   MOVEL(P)  SCOCUR        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCDOCD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e OCUR = BLANK
     C*
     C     SCDIUS        IFNE      *BLANK
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCDIUS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCDUSD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e DIUS=BLANK
     C*
     C  N90SCDIAU        IFNE      *BLANK
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCDIAU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCDUS2
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e DIAU<>BLANK
     C*
     C     SCDIDT        IFNE      *BLANK
     C                   MOVEL(P)  'DIDT'        XXTBID
     C                   MOVEL(P)  SCDIDT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCDIDD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e DIDT=BLANK
     C*
     C     SCCIND        IFNE      *BLANK
     C                   MOVEL(P)  'AXCI'        XXTBID
     C                   MOVEL(P)  SCCIND        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SDCIND
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e CIND<>' '
     C     SCBR2         IFNE      *BLANK
     C                   MOVEL(P)  SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  B2DESC        SDBR2
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e  BR2<>' '
     C*
     C*  TEST THE RESERVE CODE FOR VALIDITY AND ACTIVITY
     C*
     C     SCRECD        IFNE      *BLANK
     C                   MOVEL(P)  SCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RTDESC        SDRECD
     C                   ENDIF                                                  e 60 ON
     C                   ENDIF                                                  e RECD<>' '
     C*
     C***** BEG ADD ******* 02/01/10 ********************************
     C                   Z-ADD     ADWKPD        SCWKPD
     C                   Z-ADD     ADGRET        SCGRET
     C                   Z-ADD     ADTOTP        SCTOTP
     C                   MOVEL     ADNCC1        SCNCC1
     C                   MOVEL     ADNCC2        SCNCC2
     C                   MOVEL     ADNCC3        SCNCC3
     C******* BEG ADD ****** 02/01/10 *********************************
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  ADBR2A        SCBR2A
     C                   MOVEL(P)  ADBR2B        SCBR2B
     C                   MOVEL(P)  ADBR2C        SCBR2C
     C                   MOVEL(P)  ADBR2D        SCBR2D
     C                   ENDIF
     C******* BEG ADD ****** 02/01/10 *********************************
     C***** END ADD ******* 02/01/10 ********************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     FNCO#         ADCO#
     C                   Z-ADD     SCFND         ADFND
     C                   MOVEL(P)  SCKEY         ADKEY
     C                   MOVEL(P)  SCRMCD        ADRMCD
     C                   MOVEL(P)  SCDESC        ADDESC
     C                   Z-ADD     SCSLVL        ADSLVL
     C                   MOVEL(P)  SCDIUS        ADDIUS
     C                   MOVEL(P)  SCDIAU        ADDIAU
     C                   MOVEL(P)  SCUSR         ADUSR
     C                   Z-ADD     SCDIDY        ADDIDY
     C                   MOVEL(P)  SCDIDT        ADDIDT
     C                   MOVEL(P)  SCOCUR        ADOCUR
     C                   MOVEL(P)  SCLTFL        ADLTFL
     C                   MOVEL(P)  SCCSTS        ADCSTS
     C                   Z-ADD     SCAMT         ADAMT
     C                   MOVEL(P)  SCRECD        ADRECD
     C                   MOVEL(P)  SCBR2         ADBR2
     C                   Z-ADD     SCCNT         ADCNT
     C                   MOVEL(P)  SCCIND        ADCIND
     C                   MOVEL(P)  SCJRST        ADJRST
     C*
     C****** BEG ADD ******* 02/01/10 ********************************
     C                   Z-ADD     SCWKPD        ADWKPD
     C                   Z-ADD     SCGRET        ADGRET
     C                   Z-ADD     SCTOTP        ADTOTP
     C                   MOVEL     SCNCC1        ADNCC1
     C                   MOVEL     SCNCC2        ADNCC2
     C                   MOVEL     SCNCC3        ADNCC3
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  SCBR2A        ADBR2A
     C                   MOVEL(P)  SCBR2B        ADBR2B
     C                   MOVEL(P)  SCBR2C        ADBR2C
     C                   MOVEL(P)  SCBR2D        ADBR2D
     C                   ENDIF
     C****** END ADD ******* 02/01/10 ********************************
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
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
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@RRN2        CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCNAME
     C                   ENDIF
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     0             @@RRN
     C                   ELSE                                                   @@MODE=A
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ADKEY         SCKEY
     C                   MOVEL(P)  ADRMCD        SCRMCD
     C                   ENDIF                                                  e IN60  OFF
     C                   MOVEL(P)  SCRMCD        XXRMCD
     C     RMKY          CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             CTDSLV
     C                   MOVEL(P)  *BLANKS       CTDESC
     C                   ENDIF                                                  e IN60 ON
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C     @@MODE        IFEQ      'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        ADUSER
     C                   MOVEL     Q1PGM         ADPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADDT
     C                   MOVEL     DSTIME        ADTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        ADUSER
     C                   MOVEL     Q1PGM         ADPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADDT
     C                   MOVEL     DSTIME        ADTIME
     C                   MOVEL     Q1USER        ADIUSR
     C                   MOVEL     Q1PGM         ADIPGM
     C                   MOVEL     SRDTO         ADIDT
     C                   MOVEL     DSTIME        ADITIM
     C                   MOVE      'A'           ADSTAT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WMADP
     C                   MOVEL     *BLANKS       SCDOCD
     C                   MOVEL     *BLANKS       SCDUSD
     C                   MOVEL     *BLANKS       SCDUS2
     C                   MOVEL     *BLANKS       SCDIDD
     C                   MOVEL     *BLANKS       SDCIND
     C                   MOVEL     *BLANKS       SDBR2
     C                   MOVEL     *BLANKS       SDRECD
     C                   ENDSR
