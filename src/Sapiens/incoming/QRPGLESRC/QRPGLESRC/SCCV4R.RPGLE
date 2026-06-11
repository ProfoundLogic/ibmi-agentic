     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE SCCV4R - DELETE PROCESS2 - DELETE DLOS FROM SYSTEM
      *
      **** REMEBER, IF CLIENT HAS VDARTS, MORE WORK MUST BE DONE
      ****  IN THESE PGMS, AS OTHER OBJS. ALREADY EXIST IN FOLDERS.
      ****   THE SELECT PROCESS TO BUILD THE WRK FILE MUST KNOW THIS
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      *
      ***************************************************************
      *  REVISIONS:
     H***************************************************************
     FSXCVP     UF   E             DISK
     F*
     F*
     F*

     D*
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * * *  END PROGRAM CONTROL DATA AREA (NON BLANK MEANS END PROGRAM)
      * * *    X#FLG = THIS PROGRAM CONTROL  X#FLG2 = DELETE PROCESS CTL
     D SCCV           UDS
     D  X#FLG                  1      1
     D  X#FLG2                 2      2
     D  X#FLG3                 3      3
     D  X#BLNK                 4    256
     D*
     D                 DS
     D  ERDS                   1     40
     D  ERPGM                  1      8
     D  ERBLK1                 9      9
     D  ERRTCD                10     10
     D  ERBLK2                11     11
     D  ERDESC                12     40
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** DARTS RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    CVKEY
     C                   KFLD                    CVSEQ#
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLKY          KLIST
     C                   KFLD                    CVKEY
     C                   KFLD                    CVSEQ#
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQRTN             1
      *
      *
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
     C                   Z-ADD     0             COUNT             4 0
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C                   READ      PCV01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     *** CHECK FOR END OF JOB REQUEST (USE DATA AREA FLAG?)
     ***       CLEAR FLAG, END PGM
     C                   IN        SCCV
     C     X#FLG3        IFNE      *BLANK
     C     *LOCK         IN        SCCV
     C                   MOVE      *BLANK        X#FLG3
     C                   OUT       SCCV
     C                   GOTO      TERM
     C                   ENDIF
     ***
     C* BYPASS THE RECORD IF ALREADY PROCESSED... EITHER IT IS "C" CONVERTED
     C*** OR THE ERROR FLAG IS SET IN ORDER TO USE THE RECORD.
     C*
     C     CVPRCS        IFNE      'U'
     C     CVEFLG        OREQ      'E'
     C                   GOTO      NXTONE
     C                   ENDIF
     C*
     C*** 1) GET CM RCD (KEY/SEQ# NO SELECT/OMIT)
     C*
     C**8 DELETE THE DLO HERE
     C                   CALL      'SCCV6C'
     C                   PARM                    CVDCID
     C                   PARM                    CVFOLD
     C                   PARM      *BLANKS       XXRT              2
     C*
     C     XXRT          IFNE      '  '
     C                   MOVEL     'D'           CVEFLG
     C                   ELSE
     C*
     C*** 4) MARK CVPRCS AS A "D" FOR DELETED DLO
      *
     C*
     C                   MOVEL     'D'           CVPRCS
     C                   ENDIF
     C*
     C*
     C                   UPDATE    PCV01
      *
     C     NXTONE        TAG
     C                   READ      PCV01                                  60
     C                   ENDDO
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C********
