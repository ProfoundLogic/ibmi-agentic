     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE SRKCVT - KEY DEFINITION CONVERT SUB-PROGRAM
      *****************************************************************
      *****************************************************************
      * REVISIONS:
      *      7-24-91   LJQ  PROJ#: SC030  CLIENT#: 0003
      *                ADDED CONVERSION PGM NAME TO SMKDP FILE-KDCVTP
      *                IF KDCVTP <> *BLANKS, THEN EXECUTE THAT PROGRAM
      *                TO PASS BACK DIFFERENT KEY INFORMATION.
      *                (IE; CLAIMANT NAME, EMPLOYER NAME, ETC.)
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FSMKDL     IF   E           K DISK
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D KY              S              1    DIM(40)                              KEY DATA
     D H1              S              1    DIM(8)                               HEAD TEMP
     D*
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  MP                     1     79
     D                                     DIM(79)                              KEY DEF TO DSP ARRY
     D  KEYOUT                 1     79
     D*
     D*
     D                 DS
     D  HD                     1     64
     D                                     DIM(8)                               HEAD DESCS
     D  KDHD01                 1      8
     D  KDHD02                 9     16
     D  KDHD03                17     24
     D  KDHD04                25     32
     D  KDHD05                33     40
     D  KDHD06                41     48
     D  KDHD07                49     56
     D  KDHD08                57     64
     D*
     D                 DS
     D  HL                     1     40  0
     D                                     DIM(8)                               HEAD LENGTHS
     D  KDHL01                 1      5  0
     D  KDHL02                 6     10  0
     D  KDHL03                11     15  0
     D  KDHL04                16     20  0
     D  KDHL05                21     25  0
     D  KDHL06                26     30  0
     D  KDHL07                31     35  0
     D  KDHL08                36     40  0
     D*
     D                 DS
     D  DS                     1     40  0
     D                                     DIM(8)                               DATA FLD START POS
     D  KDDS01                 1      5  0
     D  KDDS02                 6     10  0
     D  KDDS03                11     15  0
     D  KDDS04                16     20  0
     D  KDDS05                21     25  0
     D  KDDS06                26     30  0
     D  KDDS07                31     35  0
     D  KDDS08                36     40  0
     D*
     D                 DS
     D  DL                     1     40  0
     D                                     DIM(8)                               DATA FLD LENGTH
     D  KDDL01                 1      5  0
     D  KDDL02                 6     10  0
     D  KDDL03                11     15  0
     D  KDDL04                16     20  0
     D  KDDL05                21     25  0
     D  KDDL06                26     30  0
     D  KDDL07                31     35  0
     D  KDDL08                36     40  0
     D*
     D*
     D                 DS
     D  DD                     1     40
     D                                     DIM(8)                               DATA FLD DECIMALS
     D  KDDD01                 1      5
     D  KDDD02                 6     10
     D  KDDD03                11     15
     D  KDDD04                16     20
     D  KDDD05                21     25
     D  KDDD06                26     30
     D  KDDD07                31     35
     D  KDDD08                36     40
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C*   KEY LIST FOR THE KEY DEFINITION FILE
     C*
     C     KDKEY         KLIST
     C                   KFLD                    QQPGM
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
      **** QQKEY IS THE KEY DATA VALUE PASED IN FROM THE CALLING PROGRAM,
      **** 79A - 40 BYTES AR INPUT TO THIS PROGRAM.  THE QQPGM FIELD CONTAINS
      **** THE NAME OF THE PROGRAM USED TO GET THE KEY DEF RCD.  THIS FIELD IS
      **** THE KEY TO THE KEY DEFINITION FILE.
      ****    THE QQKEY VALUE WILL BE PASSED BACK TO THE CALLING PROGRAM
      ****    AS A 79 BYTE FIELD, FORMATTED WITH THE HEADING FOR THE KEY DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPGM            10
     C                   PARM                    QQKEY            79
     C                   PARM                    QQRTCD            1
     C*
     C*** INITIALIZE THE DATA STRUCTURES
     C*
     C                   Z-ADD     *ZEROS        HL
     C                   Z-ADD     *ZEROS        DS
     C                   Z-ADD     *ZEROS        DL
     C                   MOVEL     *BLANKS       DD
     C*
     C*
     C                   MOVEA     QQKEY         KY(1)
     C***
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        QQPGM
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      'N'           QQRTCD
     C                   GOTO      ENDPGM
     C                   END
     C*
     C                   END
     C*
     C*+++ BGN ADD  LJQ 7-24-91 ++++++++++++++++++++++++++++++++++++++*
     C*
     C* IF CONVERSION PROGRAM NAME <> BLANKS; THEN CALL PROGRAM
     C* TO PASS BACK KEY IN REQUESTED FORM
     C*
     C     KDCVTP        IFNE      *BLANKS
     C                   MOVE      *BLANKS       Q1PGM            10
     C                   MOVE      *BLANKS       Q1KEY            79
     C                   MOVE      *BLANKS       Q1NAME           79
     C                   MOVEL     QQPGM         Q1PGM
     C                   MOVEL     QQKEY         Q1KEY
     C                   CALL      KDCVTP
     C                   PARM                    Q1PGM
     C                   PARM                    Q1KEY
     C                   PARM                    Q1NAME
     C                   MOVE      *BLANKS       KEYOUT
     C                   MOVEL     Q1NAME        KEYOUT
     C                   GOTO      DONE
     C                   END
     C*+++ END ADD  LJQ 7-24-91 ++++++++++++++++++++++++++++++++++++++*
     C*
     C                   Z-ADD     1             Z                 5 0
     C                   Z-ADD     1             A                 5 0
     C                   Z-ADD     1             X                 5 0
     C                   Z-ADD     1             Y                 5 0
     C                   MOVE      *BLANKS       MP
     C*
     C     X             DOWLE     79
     C*
     C     HD(A)         IFNE      *BLANKS
     C                   Z-ADD     HL(A)         Y
     C*
     C                   MOVEA     HD(A)         H1
     C*
     C                   Z-ADD     1             G                 5 0
     C*
     C                   DO        Y
     C                   MOVEL     H1(G)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             G
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   Z-ADD     DS(A)         C                 5 0
     C                   Z-ADD     DL(A)         B                 5 0
     C                   DO        B
     C                   MOVEL     KY(C)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             C
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   ADD       1             A                 5 0
     C     A             CABGT     8             DONE
     C*
     C                   END
     C*
     C     DONE          TAG
     C*
     C*
     C     KEYOUT        IFNE      *BLANK
     C                   MOVEL     KEYOUT        QQKEY
     C                   ELSE
     C                   MOVEL     *BLANK        QQKEY
     C                   MOVEL     'N'           QQRTCD
     C                   END
     C*
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
