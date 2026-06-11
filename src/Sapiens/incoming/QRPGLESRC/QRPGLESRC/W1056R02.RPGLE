     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     *===============================================================*
     *===============================================================*
     H/TITLE W1056R02 - MAINTAIN MIDTERM ENDORSEMENT FORM TABLE RECORDS
     *===============================================================*
     * Program Description: This program will allow the user to      *
     *   change or add an Endorsement.                               *
     *                                                               *
     * Created   : 08/17/99                                          *
     * Programmer: C:113                                             *
     * Project   : WC904                                             *
     *===============================================================*
     *  REVISIONS:                                                   *
     *                                                               *
     * XX/XX/XX C:XXX PROJECT: XXXXX                                 *
     *                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     * ------------------------------------------------------------- *
     *===============================================================*
     FW1056F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
     ** Physical file definition (for maintaining record)
     FWTMYP     UF A E             DISK
     F                                     RENAME(PMY01:DBRCDF)
      *
     ** Logical file definition (for maintaining record)
     FWTMYL     IF   E           K DISK
      *
     ** Group Master File
     FWMFNL     IF   E           K DISK
     *===============================================================*
     *  Data structures and constants
     *===============================================================*
      *
    D***PY SCOPYMT01I
      *
     *===============================================================*
     *  /COPY required constants
     *===============================================================*
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Midterm Endorsements')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     *===============================================================*
     *  PLIST definitions
     *===============================================================*

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FND             3 0
      *
     ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM      *BLANKS       ##FNAM           40
     *===============================================================*
     *  KLIST definitions
     *===============================================================*
      *
     C     MYKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##SORT
      *
     ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     *===============================================================*
     *  *LIKE DEFN statements
     *===============================================================*
     C     *LIKE         DEFINE    MYSORT        ##SORT
     C     *LIKE         DEFINE    MYSORT        @@SORT
     C     *LIKE         DEFINE    MYFND         ##FND
     *===============================================================*
     * Main
     *===============================================================*
      *
    C/COPY SCOPYMT01C
      *
     *===============================================================*
     * CHGSTS - SUBROUTINE FOR CHANGING STATUS
     *===============================================================*
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     MYSTAT        IFEQ      'I'
     C                   MOVE      'A'           MYSTAT
     C                   ELSE
     C                   MOVE      'I'           MYSTAT
     C                   ENDIF                                                  MYSTAT
      *
     C                   MOVEL     Q1USER        MYUSER
     C                   MOVEL     Q1PGM         MYPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MYDT
      *
     C                   MOVEL     DSTIME        MYTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
     *===============================================================*
     * DLTPRE - Pre-processing before delete occurs
     *===============================================================*
     C     DLTPRE        BEGSR
     C                   ENDSR
     *===============================================================*
     * DSPLYR - Processing after displaying information
     *===============================================================*
     C     DSPLYR        BEGSR
     C                   ENDSR
     *===============================================================*
     * ERRCK1 - Error check for screen 1
     *===============================================================*
     C     ERRCK1        BEGSR
      *
     ** Group field cannot be less than or equal to zero
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     SCFND         ANDLE     *ZERO
     C                   SETON                                        52  90
     C                   ENDIF                                                  SCFND
     C                   ENDIF                                                  SCFND
      *
     ** Group field must be valid
     C                   MOVEL     SCFND         ##FND
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     FNKY          CHAIN     PFN01                              99
     C   99              SETON                                        53  90
     C  N99              MOVEL     FNNAME        SCFNNM
     C                   ENDIF                                                  SCFND
     C                   ENDIF                                                  SCFND
      *
     ** Sort field must be greater than 0 (If in ADD/UPD/COPY MODE)
     C     *IN09         IFEQ      *OFF                                         NOT INQUIRE
     C     *IN90         ANDEQ     *OFF                                         NO ERRORS SO FAR
     C     SCSORT        IFLE      *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCSORT
     C                   ENDIF                                                  *IN09 = *OFF
      *
     ** Check to be sure that sort code has not been used previously
     C                   MOVEL     SCSORT        ##SORT
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     *IN41         OREQ      *ON                                          COPY MODE
     C     *IN90         IFEQ      *OFF                                         NO ERRORS SO FAR
     C     MYKY          CHAIN     PMY01                              99
     C  N99              SETON                                        72  90
     C                   ENDIF
     C                   ENDIF
      *
     ** Description cannot be blank
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
      *
     ** State report div only flag must be 'Y' or 'N'
     C     *IN90         IFEQ      *OFF
     C     SCSRFL        IFNE      'N'
     C     SCSRFL        ANDNE     'Y'
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCSRFL <> N
     C                   ENDIF                                                  IN90
      *
     ** Processing program must not be blank
     C     *IN90         IFEQ      *OFF
     C     SCPGM1        ANDEQ     *BLANK
     C                   SETON                                        75  90
     C                   ENDIF                                                  IN90/SCPGM1
      *
     ** Form description must not be blank
     C     *IN90         IFEQ      *OFF
     C     SCDSC1        ANDEQ     *BLANK
     C                   SETON                                        80  90
     C                   ENDIF                                                  IN90/SCDSC1
      *
     ** Check to be sure screen has not been updated since entry
     C  N90MYDT          IFGT      SAVEDT
     C     MYDT          OREQ      SAVEDT
     C     MYTIME        ANDGT     SAVETM
     C                   SETON                                        9080
     C                   ENDIF
      *
     ** We have passed all edits, goto the next screen
     C                   ENDSR
     *===============================================================*
     * ERRCK2 - Error checking for screen 2 (SCR02)
     *===============================================================*
     C     ERRCK2        BEGSR
     C                   ENDSR
     *===============================================================*
     * ERRCK3 - Error checking for screen 3 (SCR03)
     *===============================================================*
     C     ERRCK3        BEGSR
     C                   ENDSR
     *===============================================================*
     * MVDBSC - Move data base fields to screen fields
     *===============================================================*
     C     MVDBSC        BEGSR
      *
     C     *IN40         IFEQ      *OFF
      *
     C                   MOVEL     MYDESC        SCDESC
     C                   MOVE      MYSORT        SCSORT
     C                   MOVEL     MYSRFL        SCSRFL
     C                   MOVEL     MYPGM1        SCPGM1
     C                   MOVEL     MYDSC1        SCDSC1
     C                   MOVEL     MYSTAT        SCSTAT
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *ZEROS        SCSORT
     C                   MOVE      *BLANKS       SCSRFL
     C                   MOVE      *BLANKS       SCPGM1
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVEL     'A'           SCSTAT
      *
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * MVSCDB - Move screen fields to data base fields
     *===============================================================*
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         MYCO#
     C                   Z-ADD     SCFND         MYFND
     C                   MOVEL     SCDESC        MYDESC
     C                   MOVE      SCSORT        MYSORT
     C                   MOVEL     SCSRFL        MYSRFL
     C                   MOVEL     SCPGM1        MYPGM1
     C                   MOVEL     SCDSC1        MYDSC1
     C                   MOVEL     SCSTAT        MYSTAT
      *
     C                   ENDSR
     *===============================================================*
     * OTHERR - Processing for other modes
     *===============================================================*
     C     OTHERR        BEGSR
     C                   ENDSR
     *===============================================================*
     * OTHFKS - Processing for other functions keys
     *===============================================================*
     C     OTHFKS        BEGSR
     C                   ENDSR
     *===============================================================*
     * PGMEND - End of program processing
     *===============================================================*
     C     PGMEND        BEGSR
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
     *===============================================================*
     * PGMSTR - Start of program processing
     *===============================================================*
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'P'                                    41    COPY
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
     C                   MOVE      @@FND         ##FND
      *
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNNM
     C                   Z-ADD     FNFND         SCFND
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * PREOTH - Pre-processing for other modes
     *===============================================================*
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
      *
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR
     *===============================================================*
     * UPDATR - Update data base record and after processing
     *===============================================================*
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        MYUSER
     C                   MOVEL     Q1PGM         MYPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MYDT
      *
     C                   MOVEL     DSTIME        MYTIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   ENDSR
     *===============================================================*
     * WRITER - Write data base record and after processing
     *===============================================================*
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        MYUSER
     C                   MOVEL     Q1PGM         MYPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MYDT
      *
     C                   MOVEL     DSTIME        MYTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WTMYP
      *
     C                   ENDSR
