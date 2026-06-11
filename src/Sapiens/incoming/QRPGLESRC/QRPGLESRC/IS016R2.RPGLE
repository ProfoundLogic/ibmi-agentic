     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      /TITLE IS016R1 -
      *****************************************************************
      * REVISIONS
      *
      *
      *****************************************************************
      * INDICATOR USAGE
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     F*
     FIMDOLD    IF   E           K DISK
     FSMARL2    UF   E           K DISK
     F*
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************

     D*
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXIDT                  1      6  0
     D  XXYRS                  1      2  0
     D  XXMOS                  3      4  0
     D  XXDAS                  5      6  0
     D                 DS
     D  XXTRTM                 1      6  0
     D  XXHRS                  1      2  0
     D  XXMNS                  3      4  0
     D  XXSCS                  5      6  0
     D*
     D                 DS
     D  DOSDT                  1      8  0
     D  DOSDYY                 1      4
     D  DOSDMD                 5      8
     D*
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C*
     C*
     C*
     C*   KEY LIST FOR THE BATCH NOTES FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
     C                   KFLD                    HLCM75
     C*
     C     ARKY2         KLIST
     C                   KFLD                    QQDCID
     C*
     C*   KEY LIST FOR THE DOCUMNT NOTES FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C     *LIKE         DEFINE    DODCID        QQDCID
     C     *LIKE         DEFINE    ARCM75        QQCM75
     C     *LIKE         DEFINE    ARCM75        HLCM75
     C     *LIKE         DEFINE    ARASQ#        QQASQ#
     C     *LIKE         DEFINE    ARCDAT        QQCDAT
     C     *LIKE         DEFINE    ARTRDT        QQTRDT
     C     *LIKE         DEFINE    ARPNAM        QQPNAM
     C     *LIKE         DEFINE    ARTRTM        QQTRTM
     C     *LIKE         DEFINE    ARUSER        QQLUSR
     C     *LIKE         DEFINE    ARPGM         QQLPGM
     C     *LIKE         DEFINE    ARTIME        QQLTIM
     C     *LIKE         DEFINE    ARDT          QQLDT
     C     *LIKE         DEFINE    ARDT          QQIDT
     C     *LIKE         DEFINE    ARUSER        QQIUSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID
     C                   PARM                    QQCM75
     C                   PARM                    QQASQ#
     C                   PARM                    QQCDAT
     C                   PARM                    QQTRDT
     C                   PARM                    QQPNAM
     C                   PARM                    QQTRTM
     C                   PARM                    QQPRFL            1              PROTECT FLG
     C                   PARM                    QQLUSR
     C                   PARM                    QQLPGM
     C                   PARM                    QQLTIM
     C                   PARM                    QQLDT
     C                   PARM                    QQIDT
     C                   PARM                    QQIUSR
     C                   PARM                    HLCM75
     C                   PARM                    XXXRTC            1
     C*
      *
      ** Date check/conversion parameters
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C*** ARFILE PLIST
     C     ARPL          PLIST
     C                   PARM                    DODKEY
     C                   PARM                    QQDSQ#            5
     C                   PARM                    QQCM75
     C                   PARM                    QQPNAM
     C                   PARM      *BLANKS       QQSECL            3
     C                   PARM      QQIUSR        QQUSER           10
     C                   PARM      QQLPGM        QQPGM            10
     C                   PARM      *BLANKS       QQRMCD            5
     C                   PARM                    Q1ASQ#            5
     C***************      PARM *BLANKS   QQCSID  2        10
     C*******************  PARM           Q1CDAT  7        11
     C*******************  PARM *BLANK    QQCODE  2        12
      *
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   MOVEL     XXXRTC        IDXTYP            1
     C*
     C                   MOVEL     *BLANK        XXXRTC
     C                   EXSR      INIT
     C*
     C*
     C     IDXTYP        CASEQ     'U'           UPDIDX
     C     IDXTYP        CASEQ     'W'           WRTIDX
     C                   ENDCS
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   MOVE      *HIVAL        X#SEQ#            5 0
     C                   MOVE      *ON           ADDFLG            1
     C*
     C                   Z-ADD     0             XXASQ#            5 0
     C*
     C*
     C                   ENDSR
     C*
     C*
      *====================================================================*
      * * *   UPDIDX SUBROUTINE
      *====================================================================*
     C*
     C     UPDIDX        BEGSR
     C     DOKY          CHAIN     PDO01                              78
     C     *IN78         DOWEQ     *OFF
     C*
     C*** UPDATE THE SMARP FILE WITH THE INFO FROM THE DN FILE...
     C***  DKEY/DSQ# FROM DO FILE
     C*
     C     ARKY          CHAIN     PAR01                              76
     C     *IN76         IFEQ      *OFF
     C                   MOVEL     QQCM75        ARCM75
     C                   UPDATE    PAR01
     C                   ENDIF
     C*
     C*
     C     DOKY          READE     PDO01                                  78
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*
      *====================================================================*
      * * *   WRTIDX SUBROUTINE
      *====================================================================*
     C*
     C     WRTIDX        BEGSR
     C     DOKY          CHAIN     PDO01                              78
     C     *IN78         DOWEQ     *OFF
     C*
     C*** WRITE  THE SMARP FILE WITH THE INFO FROM THE DN FILE...
     C***  DKEY/DSQ# FROM DO FILE
     C*
     C                   MOVE      DODSQ#        QQDSQ#
     C****                 MOVE QQASQ#    Q1ASQ#
     C****                 MOVE QQCDAT    Q1CDAT
     C*
     C                   CALL      'DA701R'      ARPL
     C*
     C     DOKY          READE     PDO01                                  78
     C                   ENDDO
     C*
     C*
     C                   ENDSR
     C*
