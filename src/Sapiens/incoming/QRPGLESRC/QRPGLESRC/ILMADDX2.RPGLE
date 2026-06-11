     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ILMADDX2 ADD INDEX IMAGE RECORDS
     H*********************************************************************
     H*                                                                   *
     H****  ADD
     F*
      * GET DART RECORD FROM DO VALUES.
      *  SETUP PARM LIST FOR CALL TO COPY/TRANSFER PROGRAM
      *  PERFORM COPY CALL
      *  WERE DONE!
      *      THIS SHOULD CPY DART RCD, AS WELL AS CALL THE PGM TO CPY IMAGE
      *      AND OPTICAL
     H*                                                                   *
     H*    CALLS TO:                                                      *
     H*                                                                   *
     H*                                                                   *
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H**                                                                  *
     H*                                                                   *
     H*********************************************************************
     H* INDICATOR USAGE                                                   *
     H*                                                                   *
     H* 60  GENERAL CHAIN INDICATOR                                       *
     H* 61  GENERAL CHAIN INDICATOR                                       *
     H*********************************************************************
     H** FILES:                                                          **
     H*********************************************************************
     F*
     FSDCMLE    IF   E           K DISK
      * DOCUMENT INDEX RECORD
     FIMDOLB    IF   E           K DISK
     F*
     FSMKTL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XCURDT                 1      8  0
     D  XCURYR                 1      4  0
     D  XCURMD                 5      8  0
     D*
     D                 DS
     D  D2DTTM                 1     14  0
     D  D2TIME                 1      6  0
     D  D2DATE                 7     14  0
     D*
     D                 DS
     D  XXA75                  1     75
     D  XXTXT                  4     20
     D  XXBSTS                24     25
     D  XXBM                  24     25
     D  XXSLH1                26     26
     D  XXBD                  27     28
     D  XXHYPH                28     28
     D  XXSLH2                29     29
     D  XXBYR                 30     33
     D  XXBTMH                39     40
     D  XXCOL1                41     41
     D  XXBTMM                42     43
     D  XXCOL2                44     44
     D  XXBTMS                45     46
     D  XXLDSC                31     70
     D  XXBUSR                24     33
     D  XXBERR                24     75
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D QCTXT           C                   CONST('Verified, New Sts')
     D DUMQ            C                   CONST('QEZJOBLOG ')

     C*
     C     PL915C        PLIST
     C                   PARM                    $$TYPE            1
     C                   PARM                    $$FRKY           40
     C                   PARM                    $$FRS#            5
     C                   PARM                    $$FRRC            5
     C                   PARM                    $$FRDI            1
     C                   PARM                    $$FRDU           10
     C                   PARM                    $$FRDD            8
     C                   PARM                    $$TOKY           40
     C                   PARM                    $$TORC            5
     C                   PARM                    $$TODI            1
     C                   PARM                    $$TODU           10
     C                   PARM                    $$TODD            8
     C                   PARM                    $$KTYP            2
     C                   PARM      DUMQ          $$OUTQ           10
     C                   PARM      'D'           $$MODE            1
     C*
     C*** DARTS FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    DODKEY           40
     C                   KFLD                    DODSQ#            5 0
     C*
     C*
     C* DARTS KEY TYPE FILE
     C*
     C     KTKY          KLIST
     C                   KFLD                    CMPNAM
     C*
     C*
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12              BTCH ID
     C                   PARM                    ##KEY            40
     C                   PARM                    QQRTCD            1              RTN CODE
     C***
      **** USE DCID TO GET DO RCD.  FROM DO RCD USE DKEY AND DSQ# TO GET
      ****   THE CM RECORD (FROM).  FILL IN THE PARMS TO CALL THE SC915C
      ****   PROGRAM USING THE FROM FIELDS OF CM FILE, AND USE ##KEY FOR
      ****   THE "TO KEY" ON THE TRANSFER.
     ******************************************************************
     C*
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *================================================================
      *
      *** Process
      *** document.
      *
     C     DOKY1         CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      SRDL                                              ADD-INDEX
     C*
      ********>
     C*
     C     CMKY1         CHAIN     PCM01                              63
     C     *IN63         IFEQ      *OFF
     C*
     C*** BEFORE CALL TO SC915C, MUST MAKE SURE THAT DATES ARE IN MDYY FMT
     C*
     C                   MOVEL     'C'           $$TYPE            1
     C                   MOVEL     CMKEY         $$FRKY           40
     C                   MOVE      CMSEQ#        $$FRS#            5
     C                   MOVEL     CMRMCD        $$FRRC            5
     C                   MOVEL     CMDIFL        $$FRDI            1
     C                   MOVEL     CMUSER        $$FRDU           10
     C*** TRANSLATE FROM DIARY DATE  (CMCDAT)
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDAT8            8
      *
     C                   MOVEL     XXDAT8        $$FRDD            8
     C                   MOVEL     ##KEY         $$TOKY           40
     C                   MOVEL     $$FRRC        $$TORC            5
     C                   MOVEL     $$FRDI        $$TODI            1
     C                   MOVEL     $$FRDU        $$TODU           10
     C                   MOVEL     $$FRDD        $$TODD            8
     C*** GET KEY TYPE (2A) FROM PNAM
     C*
     C     KTKY          CHAIN     PKT01                              65
     C*
     C  N65              MOVEL     KTKTYP        $$KTYP            2
     C   65              MOVEL     'DL'          $$KTYP
     C*
     C                   MOVEL     'QPRINT'      $$OUTQ           10
     C                   MOVEL     'D'           $$MODE            1
     C*
     C* * * PERFORM THE TRANSFER ***
     C                   CALL      'SC915C'      PL915C
     C*
     C*
     C                   ELSE
     C*** ERROR NO CM RCD
     C                   MOVE      '1'           QQRTCD
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      '1'           QQRTCD
     C*** ERROR NO DO RECORD
     C                   ENDIF
     C*
     C*
      *
      *================================================================
     C*
     C*
     C     TERM          TAG
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
      *===================================================================
     C*
     C*
      *===================================================================
      ***   SRDL     -     UPDATE RECORD TO DELETED STATUS           *****
      *===================================================================
     C*
     C     SRDL          BEGSR
     C*
      *
     C*
     C     ENDDL         ENDSR
     C*
     C*
     C*
