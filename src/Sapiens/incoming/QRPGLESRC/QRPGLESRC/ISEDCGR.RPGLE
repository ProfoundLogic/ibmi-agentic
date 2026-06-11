     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE ISEDCGR - EDIT CUSTOM FIELDS FOR VISDARTS
     H*
      *****************************************************************
      ** REVISIONS:
      *
      * 10/01/03 C:128 Project: VI114 Support: 38751
      *                Added two arrays to check numeric fields in
      *                *Entry parm QQFDTA.
      *
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
      *** RMV 05/07/01 *** 02 *****
     F****SMCGL   IF  E           K        DISK
     F****STTBL   IF  E           K        DISK
     F****STTBL1  IF  E           K        DISK
      *** ADD 05/07/01 ****
     FSMCGL3    IF   E           K DISK
     FSTTBL4    IF   E           K DISK
     FSTTBL5    IF   E           K DISK
      *** END ADD 05/07/01 ****
     F                                     RENAME(PTB01:PTB02)
     FSMSLL     IF   E           K DISK
     F*
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D*
      *** Begin Add 10/01/03 ******************************************
     D CUD             S              8    DIM(5)                               USER ASSIGN DATE
     D CUN             S              9    DIM(5)                               USER ASSIGN NUMB
      *** End   Add 10/01/03 ******************************************
     D*
     D*
     D*
     D*
     D*
     D*
     D*
     D RF              S              1    DIM(10)                              SCREEN PATH RFMTS
     D*
     D OUT             S              6    DIM(5)                               RTN CVTD DATES
     D*
     D PTH             S              8    DIM(10)                              PATH ID'S FOR CONTIN
     D PHI             S             10    DIM(10)                              SCREEN PATH DSPATR(H
     D*
     D FL              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES UA
     D FD              S              4    DIM(5) CTDATA PERRCD(5)              FIELD NAMES UD
     D FN              S              4    DIM(5) CTDATA PERRCD(5)              FIELD NAMES UN
     D FB              S              4    DIM(6) CTDATA PERRCD(6)              FIELD NAMES UB
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  UA                     1    100
     D                                     DIM(10)                              USER ASSIGN FLD
     D  SCUA01                 1     10
     D  SCUA02                11     20
     D  SCUA03                21     30
     D  SCUA04                31     40
     D  SCUA05                41     50
     D  SCUA06                51     60
     D  SCUA07                61     70
     D  SCUA08                71     80
     D  SCUA09                81     90
     D  SCUA10                91    100
     D*
     D                 DS
     D  UD                     1     40  0
     D                                     DIM(5)                               USER ASSIGN DATE
     D  SCUD01                 1      8  0
     D  SCUD02                 9     16  0
     D  SCUD03                17     24  0
     D  SCUD04                25     32  0
     D  SCUD05                33     40  0
     D  XXUD                   1     40
     D*
     D                 DS
     D  UN                     1     45  2
     D                                     DIM(5)                               USER ASSIGN NUMB
     D  SCUN01                 1      9  2
     D  SCUN02                10     18  2
     D  SCUN03                19     27  2
     D  SCUN04                28     36  2
     D  SCUN05                37     45  2
     D  XXUN                   1     45
     D*
     D                 DS
     D  NG                     1      5
     D                                     DIM(5)                               NEGATIVE NUMBERS?
     D  SCNG01                 1      1
     D  SCNG02                 2      2
     D  SCNG03                 3      3
     D  SCNG04                 4      4
     D  SCNG05                 5      5
     D  XXNG                   1      5
     D*
     D                 DS
     D  UB                     1    210
     D                                     DIM(6)                               USER ASSIGN FLD 35A
     D  SCUB01                 1     35
     D  SCUB02                36     70
     D  SCUB03                71    105
     D  SCUB04               106    140
     D  SCUB05               141    175
     D  SCUB06               176    210
     D*
     D                 DS
     D  DA                     1    100
     D                                     DIM(10)                              USR ASGN FLD DSC
     D  SCDA01                 1     10
     D  SCDA02                11     20
     D  SCDA03                21     30
     D  SCDA04                31     40
     D  SCDA05                41     50
     D  SCDA06                51     60
     D  SCDA07                61     70
     D  SCDA08                71     80
     D  SCDA09                81     90
     D  SCDA10                91    100
     D*
     D                 DS
     D  DD                     1     50
     D                                     DIM(5)                               USR ASGN DAT DSC
     D  SCDD01                 1     10
     D  SCDD02                11     20
     D  SCDD03                21     30
     D  SCDD04                31     40
     D  SCDD05                41     50
     D*
     D                 DS
     D  DN                     1     50
     D                                     DIM(5)                               USR ASGN NUM DSC
     D  SCDN01                 1     10
     D  SCDN02                11     20
     D  SCDN03                21     30
     D  SCDN04                31     40
     D  SCDN05                41     50
     D*
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               USR ASGN 35A FLD DSC
     D  SCDB01                 1     10
     D  SCDB02                11     20
     D  SCDB03                21     30
     D  SCDB04                31     40
     D  SCDB05                41     50
     D  SCDB06                51     60
     D*
     D                 DS
     D  TA                     1     10
     D                                     DIM(10)                              TABLE BAKUP FLDS
     D  CGTA01                 1      1
     D  CGTA02                 2      2
     D  CGTA03                 3      3
     D  CGTA04                 4      4
     D  CGTA05                 5      5
     D  CGTA06                 6      6
     D  CGTA07                 7      7
     D  CGTA08                 8      8
     D  CGTA09                 9      9
     D  CGTA10                10     10
     D*
     D*
     D                 DS
     D  RC                     1     27
     D                                     DIM(27)                              RTN CODESE
     D  QQRTNC                 1     27
     D*
     D                 DS
     D  TD                     1      5
     D                                     DIM(5)                               TABLE BAKUP DATE
     D  CGTD01                 1      1
     D  CGTD02                 2      2
     D  CGTD03                 3      3
     D  CGTD04                 4      4
     D  CGTD05                 5      5
     D*
     D                 DS
     D  TN                     1      5
     D                                     DIM(5)                               TABLE BAKUP NUMB
     D  CGTN01                 1      1
     D  CGTN02                 2      2
     D  CGTN03                 3      3
     D  CGTN04                 4      4
     D  CGTN05                 5      5
     D*
     D                 DS
     D  TB                     1      6
     D                                     DIM(6)                               TABLE BAKUP 35A FLDS
     D  CGTB01                 1      1
     D  CGTB02                 2      2
     D  CGTB03                 3      3
     D  CGTB04                 4      4
     D  CGTB05                 5      5
     D  CGTB06                 6      6
     D*
     D                 DS
     D  MA                     1     10
     D                                     DIM(10)                              MANDATORY ENTRY
     D  CGMA01                 1      1
     D  CGMA02                 2      2
     D  CGMA03                 3      3
     D  CGMA04                 4      4
     D  CGMA05                 5      5
     D  CGMA06                 6      6
     D  CGMA07                 7      7
     D  CGMA08                 8      8
     D  CGMA09                 9      9
     D  CGMA10                10     10
     D*
     D                 DS
     D  MD                     1      5
     D                                     DIM(5)                               "
     D  CGMD01                 1      1
     D  CGMD02                 2      2
     D  CGMD03                 3      3
     D  CGMD04                 4      4
     D  CGMD05                 5      5
     D*
     D                 DS
     D  MN                     1      5
     D                                     DIM(5)                               "
     D  CGMN01                 1      1
     D  CGMN02                 2      2
     D  CGMN03                 3      3
     D  CGMN04                 4      4
     D  CGMN05                 5      5
     D*
     D                 DS
     D  MB                     1      6
     D                                     DIM(6)                               " FOR 35A FLDS
     D  CGMB01                 1      1
     D  CGMB02                 2      2
     D  CGMB03                 3      3
     D  CGMB04                 4      4
     D  CGMB05                 5      5
     D  CGMB06                 6      6
     D*
     D                 DS
     D  GA                     1    100
     D                                     DIM(10)                              SPECIAL EDIT PGM
     D  CGPA01                 1     10
     D  CGPA02                11     20
     D  CGPA03                21     30
     D  CGPA04                31     40
     D  CGPA05                41     50
     D  CGPA06                51     60
     D  CGPA07                61     70
     D  CGPA08                71     80
     D  CGPA09                81     90
     D  CGPA10                91    100
     D*
     D                 DS
     D  GD                     1     50
     D                                     DIM(5)                               "
     D  CGPD01                 1     10
     D  CGPD02                11     20
     D  CGPD03                21     30
     D  CGPD04                31     40
     D  CGPD05                41     50
     D*
     D                 DS
     D  GN                     1     50
     D                                     DIM(5)                               "
     D  CGPN01                 1     10
     D  CGPN02                11     20
     D  CGPN03                21     30
     D  CGPN04                31     40
     D  CGPN05                41     50
     D*
     D                 DS
     D  GB                     1     60
     D                                     DIM(6)                               " FOR 35A FLDS
     D  CGPB01                 1     10
     D  CGPB02                11     20
     D  CGPB03                21     30
     D  CGPB04                31     40
     D  CGPB05                41     50
     D  CGPB06                51     60
     D*
     D                 DS
     D  ED                     1      5
     D                                     DIM(5)                               EDIT DATE
     D  CGED01                 1      1
     D  CGED02                 2      2
     D  CGED03                 3      3
     D  CGED04                 4      4
     D  CGED05                 5      5
     D*
     D                 DS
     D  EN                     1      5
     D                                     DIM(5)                               EDIT NUMERIC
     D  CGEN01                 1      1
     D  CGEN02                 2      2
     D  CGEN03                 3      3
     D  CGEN04                 4      4
     D  CGEN05                 5      5
     D*
     D                 DS
     D  SA                     1     30  0
     D                                     DIM(10)                              SECURITY LEVEL
     D  CGSA01                 1      3  0
     D  CGSA02                 4      6  0
     D  CGSA03                 7      9  0
     D  CGSA04                10     12  0
     D  CGSA05                13     15  0
     D  CGSA06                16     18  0
     D  CGSA07                19     21  0
     D  CGSA08                22     24  0
     D  CGSA09                25     27  0
     D  CGSA10                28     30  0
     D*
     D                 DS
     D  SD                     1     15  0
     D                                     DIM(5)                               "
     D  CGSD01                 1      3  0
     D  CGSD02                 4      6  0
     D  CGSD03                 7      9  0
     D  CGSD04                10     12  0
     D  CGSD05                13     15  0
     D*
     D                 DS
     D  SN                     1     15  0
     D                                     DIM(5)                               "
     D  CGSN01                 1      3  0
     D  CGSN02                 4      6  0
     D  CGSN03                 7      9  0
     D  CGSN04                10     12  0
     D  CGSN05                13     15  0
     D*
     D                 DS
     D  SB                     1     18  0
     D                                     DIM(6)                               " FOR 35A FLDS
     D  CGSB01                 1      3  0
     D  CGSB02                 4      6  0
     D  CGSB03                 7      9  0
     D  CGSB04                10     12  0
     D  CGSB05                13     15  0
     D  CGSB06                16     18  0
     D*
     D                 DS
     D  PA                     1     20
     D                                     DIM(10)                              SCREEN PATH CSID'S
     D  SCPA01                 1      2
     D  SCPA02                 3      4
     D  SCPA03                 5      6
     D  SCPA04                 7      8
     D  SCPA05                 9     10
     D  SCPA06                11     12
     D  SCPA07                13     14
     D  SCPA08                15     16
     D  SCPA09                17     18
     D  SCPA10                19     20
     D*
     D                 DS
     D  CI                     1     20
     D                                     DIM(10)                              PATH FILE CSID'S
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
     D*
     D                 DS
     D  FR                     1     10
     D                                     DIM(10)                              PATH FILE RFMTS
     D  PIRF01                 1      1
     D  PIRF02                 2      2
     D  PIRF03                 3      3
     D  PIRF04                 4      4
     D  PIRF05                 5      5
     D  PIRF06                 6      6
     D  PIRF07                 7      7
     D  PIRF08                 8      8
     D  PIRF09                 9      9
     D  PIRF10                10     10
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C*   KEY LIST FOR THE TABLE RECORD - RECORD FORMAT A
     C     TBKY          KLIST
     C***  BEGIN ADD  ***  05/07/01  *********************************
     C                   KFLD                    XXGRP             3 0
     C***  END   ADD  ***  05/07/01  *********************************
     C                   KFLD                    QQCSID
     C                   KFLD                    FL(K)
     C                   KFLD                    XXVUA
     C                   KFLD                    XXVUD
     C                   KFLD                    XXVUN
     C*
     C*   KEY LIST FOR THE TABLE RECORD - RECORD FORMAT B
     C     TBKY2         KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    XXGRP             3 0
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    QQCSID
     C                   KFLD                    FB(K)
     C                   KFLD                    XXVUB
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     CGKY          KLIST
      *** ADD *** 05/07/01 ****
     C                   KFLD                    XXGRP             3 0
      *** END ADD 05/07/01 ****
     C                   KFLD                    QQCSID
     C                   KFLD                    QQRFMT
     C*
     C*
     C     SLKY          KLIST
     C                   KFLD                    $$USER
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
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C*
     C     *LIKE         DEFINE    TBVUA         XXVUA
     C     *LIKE         DEFINE    TBVUD         XXVUD
     C     *LIKE         DEFINE    TBVUN         XXVUN
     C     *LIKE         DEFINE    TBVUB         XXVUB
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCSID            2
     C                   PARM                    QQRFMT            1
     C                   PARM                    $$USER           10
     C                   PARM                    XXRTNC           27            RTN CODES
     C                   PARM                    QQFDTA          210
     C*
     C*** ADD 05/07/01 ****
     C                   MOVEL     XXRTNC        $$GRP             3
     C                   MOVEL     '   '         XXRTNC
     C                   MOVE      $$GRP         XXGRP             3 0
     C*** END ADD 05/07/01 ****
     C                   MOVEL     XXRTNC        QQRTNC
     C******************** MOVELQ1USER    $$USER 10
     C     SLKY          CHAIN     PSL01                              60
     C*
     C                   Z-ADD     1             V                 5 0
     C* Next line intentionally left open because of compilation of array
     C                   MOVE      FD(V)         XXXXX             8 0
     C* Next line intentionally left open because of compilation of array
     C                   MOVE      FN(V)         XXXX1             9 2
     C*
     **** EACH FIELD 1-26 HAS A 1 CHR FIELD AS A RETURN CODE.  THEN THE LAST
     ***  BYTE (POS 27) IS A GENERAL RETURN CODE.
     C*
     C                   CLEAR                   OUT
     C                   CLEAR                   RC
     C                   CLEAR                   UB
     C                   CLEAR                   UA
     C                   CLEAR                   UN
     C                   CLEAR                   UD
     C*
     C     QQRFMT        IFEQ      'B'
     C                   MOVEA     QQFDTA        UB
     C                   ELSE
     C                   MOVEL     QQFDTA        XX100           100
     C                   MOVEA     XX100         UA
      *** Begin Add 10/01/03 ******************************************
     C     45            SUBST     QQFDTA:101    XCUN             45
     C                   MOVEA     XCUN          CUN
     C                   DO        5             E                 1 0
     C     CUN(E)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        CUN(E)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     CUN           XX45             45
      *
     C     40            SUBST     QQFDTA:146    XCUD             40
     C                   MOVEA     XCUD          CUD
     C                   DO        5             F                 1 0
     C     CUD(F)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        CUD(F)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     CUD           XX40             40
      *
     C******     45        SUBSTQQFDTA:101XX45   45
     C******     40        SUBSTQQFDTA:146XX40   40
      *** End   Add 10/01/03 ******************************************
     C     5             SUBST     QQFDTA:186    XX05              5
     C                   MOVE      XX45          XXUN
     C                   MOVE      XX40          XXUD
     C                   MOVE      XX05          XXNG
     C*
     C                   ENDIF
     C*
     C*
     C*
     C     QQRFMT        IFEQ      'B'
     C******************** EXSR H1GET
     C                   EXSR      CGGETB
     C                   ELSE                                                   ELSE EQ 'B'
     C******************** EXSR HDGET
     C                   EXSR      CGGETA
     C                   END                                                    END EQ 'B'
     C*
     C**** DO EDITS
     C                   EXSR      EDIT1
     C*
     C     ENDPGM        TAG
     C**** KEEP PGM ACTIVE BY USING RETURN... HELP SPEED OF PROCESS.
     C******************** RETRN *****************************************
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
     C*
      *------------------------*
     C     EDIT1         BEGSR
      *------------------------*
     C*
     C     CGKY          CHAIN     PCG01                              60
     C*** IF NOT HIT, THIS IS A GENERAL ERROR!!!!! POSITION 27 OF THE RTNC
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      'A'           XXRTNC                          (POS 27)
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C* ===================================================================
     C*
     C**** EDIT FOR RECORD FORMAT 'B'
     C     QQRFMT        IFEQ      'B'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 35A ALPHA FIELDS
     C*
     C                   Z-ADD     20            Z                 5 0
     C                   Z-ADD     70            Y                 5 0
     C                   Z-ADD     0             K                 5 0
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C*
     C                   DO        6             X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*??????????????
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF 35A ALPHA FIELDS ***
     C     MB(X)         IFEQ      'Y'
     C     UB(X)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     !*** ERROR HERE ***
     C                   MOVE      '1'           RC(Z)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT35A
     C*
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR 35A ALPHA FIELDS ***
     C     GB(X)         IFNE      *BLANKS
     C     UB(X)         ANDNE     *BLANKS
     C                   MOVE      ' '           GBRTCD            1
     C                   MOVE      *OFF          *IN60
     C                   CALL      GB(X)                                60
     C                   PARM      *BLANKS       GBUA             10
     C                   PARM      *ZEROS        GBUD              6 0
     C                   PARM      *ZEROS        GBUN              9 2
     C                   PARM      UB(X)         GBUB             35
     C                   PARM                    GBRTCD            1
     C*
     C     *IN60         IFEQ      *OFF
     C     GBRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C*** ERROR HERE
     C                   MOVE      '3'           RC(Z)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT35A
     C                   END                                                    END GBRTCD EQ '1'
     C                   END                                                    END 0 *IN60
     C                   END                                                    END GB,X NE *BLANKS
     C*
     C*
     C*** TABLE BACKUP VALUES FOR ALPHA FIELDS ***
     C     TB(X)         IFEQ      'Y'
     C     UB(X)         ANDNE     *BLANK
     C*
     C                   MOVE      *BLANKS       XXVUA
     C                   MOVE      *ZEROS        XXVUD
     C                   MOVE      *ZEROS        XXVUN
     C                   MOVE      UB(X)         XXVUB
     C*
     C     TBKY2         CHAIN     PTB02                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN(Z)
     C*** ERROR HERE
     C                   MOVE      '2'           RC(Z)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT35A
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END                                                    END 0 *IN,Y
     C*
     C     NXT35A        TAG
     C                   END                                                    END D0 6 X
     C*
     C*
     C*
     C* ===================================================================
     C**** "DEFAULT" EDIT
     C                   ELSE                                                   ELSE EQ 'B'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 10A ALPHA FIELDS
     C*
     C                   Z-ADD     20            Z                 5 0
     C                   Z-ADD     70            Y                 5 0
     C                   Z-ADD     0             K                 5 0
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C                   DO        10            X                 5 0
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*
     C                   MOVEL     *OFF          *IN(Z)
     C*?????????????????
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF ALPHA FIELDS ***
     C     MA(X)         IFEQ      'Y'
     C     UA(X)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     C*** ERROR HERE
     C                   MOVE      '1'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTRDA
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR ALPHA FIELDS ***
     C     GA(X)         IFNE      *BLANKS
     C     UA(X)         ANDNE     *BLANKS
     C                   MOVE      ' '           GARTCD
     C                   MOVE      *OFF          *IN60
     C                   CALL      GA(X)                                60
     C                   PARM      UA(X)         GAUA             10
     C                   PARM      *ZEROS        GAUD              6 0
     C                   PARM      *ZEROS        GAUN              9 2
     C                   PARM      *BLANKS       GAUB             35
     C                   PARM                    GARTCD            1
     C*
     C     *IN60         IFEQ      *OFF
     C     GARTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN(Z)
     C*** ERROR HERE
     C                   MOVE      '3'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTRDA
     C*
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*
     C*** TABLE BACKUP VALUES FOR ALPHA FIELDS ***
     C     TA(X)         IFEQ      'Y'
     C     UA(X)         ANDNE     *BLANK
     C*
     C                   MOVE      UA(X)         XXVUA
     C                   MOVE      *ZEROS        XXVUD
     C                   MOVE      *ZEROS        XXVUN
     C                   MOVE      *BLANKS       XXVUB
     C*
     C     TBKY          CHAIN     PTB01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN(Z)
     C*** ERROR HERE
     C*
     C                   MOVE      '2'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTRDA
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     NXTRDA        TAG
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 6.0 NUMERIC FIELDS
     C*
     C                   Z-ADD     40            Z                 5 0
     C                   Z-ADD     80            Y                 5 0
     C                   Z-ADD     10            K                 5 0
     C*
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C*
     C                   DO        05            X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C*?????????????????
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF NUMERIC (DATE) FIELDS ***
     C     MD(X)         IFEQ      'Y'
     C     UD(X)         ANDEQ     *ZERO
     C                   MOVE      '1'           *IN09
     C                   MOVE      '1'           *IN(Z)
     C                   MOVE      '1'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTDTA
     C*
     C                   END
     C*
     C*** DATE EDIT OF NUMERIC (DATE) FIELDS ******
     C     ED(X)         IFEQ      'Y'
     C     UD(X)         ANDNE     *ZEROS
     C*
     C*
     C                   Z-ADD     UD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         UD(X)
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN10
     C                   MOVE      *ON           *IN(Z)
     C*** ERROR HERE
     C                   MOVE      '5'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTDTA
     C*
     C*
     C                   END
     C*
     C                   Z-ADD     UD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         OUT(X)
     C                   ELSE
     C*
     C     UD(X)         IFGT      9999999
     C*** ERROR HERE
     C                   MOVE      '8'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTDTA
     C                   ENDIF
     C*
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR NUMERIC (DATE) FIELDS
     C     GD(X)         IFNE      *BLANKS
     C     UD(X)         ANDNE     *ZEROS
     C                   MOVE      ' '           GARTCD
     C                   MOVE      *OFF          *IN60
     C                   CALL      GD(X)                                60
     C                   PARM      *BLANKS       GAUA             10
     C                   PARM      UD(X)         GAUD              6 0
     C                   PARM      *ZEROS        GAUN              9 2
     C                   PARM      *BLANKS       GAUB             35
     C                   PARM                    GARTCD            1
     C*
     C     *IN60         IFEQ      *OFF
     C     GARTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN09
     C                   MOVE      *ON           *IN10
     C                   MOVE      *ON           *IN(Z)
     C*** ERROR HERE
     C                   MOVE      '3'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTDTA
     C*
     C*
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*
     C     TD(X)         IFEQ      'Y'
     C     UD(X)         ANDNE     *ZEROS
     C*
     C                   MOVE      *BLANKS       XXVUA
     C                   MOVE      UD(X)         XXVUD
     C                   MOVE      *ZEROS        XXVUN
     C                   MOVE      *BLANKS       XXVUB
     C*
     C     TBKY          CHAIN     PTB01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN(Z)
     C*
     C*** ERROR HERE
     C*
     C                   MOVE      '2'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXTDTA
     C*
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     NXTDTA        TAG
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** EDIT ON 9.2 NUMERIC FIELDS
     C*
     C                   Z-ADD     48            Z                 5 0
     C                   Z-ADD     85            Y                 5 0
     C                   Z-ADD     15            K                 5 0
     C*
     C*
     C                   MOVE      '0'           *IN09                              SETOF ERROR INDS
     C                   MOVE      '0'           *IN10
     C*
     C                   DO        05            X
     C*
     C                   ADD       1             Y
     C                   ADD       1             K
     C                   ADD       1             Z
     C* ??????????????????????????
     C     *IN(Y)        IFEQ      '0'
     C*
     C*** MANDATORY ENTRY OF AMOUNT FIELDS ***
     C     MN(X)         IFEQ      'Y'
     C     UN(X)         ANDEQ     *ZERO
     C                   MOVE      *ON           *IN09
     C                   MOVE      *ON           *IN(Z)
     C*** ERROR HERE
     C*
     C                   MOVE      '1'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT92A
     C*
     C*
     C                   END
     C*
     C*** POSITIVE EDIT OF AMOUNT FIELDS ******
     C     EN(X)         IFEQ      '1'
      * *** CHANGE THE NUMERIC VALUES TO NEG IF REQUIERD (FOR EDIT)
     C     NG(X)         IFEQ      'Y'
     C     UN(X)         MULT      -1            XXTST1            9 2
     C                   ELSE
     C                   MOVE      UN(X)         XXTST1            9 2
     C                   ENDIF
     C*
     C********** UN,X      ANDLT0
     C     XXTST1        IFLT      0
     C                   MOVE      *ON           *IN10
     C                   MOVE      *ON           *IN(Z)
     C*** ERROR HERE
     C*
     C                   MOVE      '6'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT92A
     C*
     C*
     C                   ENDIF
     C                   END
     C*
     C*** NEGATIVE EDIT OF AMOUNT FIELDS ******
     C     EN(X)         IFEQ      '2'
      * *** CHANGE THE NUMERIC VALUES TO NEG IF REQUIERD (FOR EDIT)
     C     NG(X)         IFEQ      'Y'
     C     UN(X)         MULT      -1            XXTST1            9 2
     C                   ELSE
     C                   MOVE      UN(X)         XXTST1            9 2
     C                   ENDIF
     C*
     C********** UN,X      ANDGT0
     C     XXTST1        IFGT      0
     C                   MOVE      *ON           *IN10
     C                   MOVE      *ON           *IN(Z)
     C*** ERROR HERE
     C*
     C                   MOVE      '7'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT92A
     C*
     C*
     C                   ENDIF
     C                   END
     C*
     C*** SPECIAL EDIT PROGRAM FOR AMOUNT FIELDS
     C     GN(X)         IFNE      *BLANKS
     C     UN(X)         ANDNE     *ZEROS
     C                   MOVE      ' '           GARTCD
     C                   MOVE      *OFF          *IN60
     C                   CALL      GN(X)                                60
     C                   PARM      *BLANKS       GAUA             10
     C                   PARM      *ZEROS        GAUD              6 0
     C                   PARM      UN(X)         GAUN              9 2
     C                   PARM      *BLANKS       GAUB             35
     C                   PARM                    GARTCD            1
     C*
     C     *IN60         IFEQ      *OFF
     C     GARTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN09
     C                   MOVE      *ON           *IN10
     C                   MOVE      *ON           *IN(Z)
     C*** ERROR HERE
     C*
     C                   MOVE      '3'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT92A
     C*
     C*
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*
     C     TN(X)         IFEQ      'Y'
     C     UN(X)         ANDNE     *ZEROS
     C*
     C                   MOVE      *BLANKS       XXVUA
     C                   MOVE      *ZEROS        XXVUD
     C                   MOVE      UN(X)         XXVUN
     C                   MOVE      *BLANKS       XXVUB
     C*
     C     TBKY          CHAIN     PTB01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN(Z)
     C*
     C*** ERROR HERE
     C*
     C                   MOVE      '2'           RC(K)
     C                   MOVE      *ON           *IN69
     C                   GOTO      NXT92A
     C*
     C*
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     NXT92A        TAG
     C                   END
     C*
     C                   END                                                    END EQ 'B'
     C*
     C*** ALL EDITS PASSED, WRITE DATA BASE RECORDS.
     C* *** DONT WRITE HERE WILL WRITE IN THE CLIENT (VDARTS) PROGRAM
     C*
     C     QQRFMT        IFEQ      'B'
     C*******************  EXSR WRITEB
     C                   ELSE                                                   ELSE EQ 'B'
     C*******************  EXSR WRITEA
     C                   END                                                    END EQ 'B'
     C*
     C*
     C**** ERROR ???
     C*** CHECK TO SEE IF ANY ERRORS OCCURED... IF SO SET THE RC,27 ELSE
     C*** DONT
     C     *IN69         IFEQ      *ON
     C     RC(27)        ANDEQ     *BLANK
     C                   MOVE      '1'           RC(27)                           *** GENERAL
     C                   ELSE
     C                   CLEAR                   QQFDTA
     C                   MOVEA     OUT           QQFDTA
     C                   ENDIF                                                        ERROR
     C                   MOVEL     QQRTNC        XXRTNC
     C*
     C     ENDEDT        ENDSR
     C*
      *====================================================================*
      * * *   CGGETA SUBROUTINE (CUSTOM GROUP ID - FORMAT A)    C G G E T A ***
      *====================================================================*
     C*
     C     CGGETA        BEGSR
     C*
     C     CGKY          CHAIN     PCG01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      CGDA01        SCDA01
     C                   MOVE      CGDA02        SCDA02
     C                   MOVE      CGDA03        SCDA03
     C                   MOVE      CGDA04        SCDA04
     C                   MOVE      CGDA05        SCDA05
     C                   MOVE      CGDA06        SCDA06
     C                   MOVE      CGDA07        SCDA07
     C                   MOVE      CGDA08        SCDA08
     C                   MOVE      CGDA09        SCDA09
     C                   MOVE      CGDA10        SCDA10
     C*
     C                   MOVE      CGDD01        SCDD01
     C                   MOVE      CGDD02        SCDD02
     C                   MOVE      CGDD03        SCDD03
     C                   MOVE      CGDD04        SCDD04
     C                   MOVE      CGDD05        SCDD05
     C*
     C                   MOVE      CGDN01        SCDN01
     C                   MOVE      CGDN02        SCDN02
     C                   MOVE      CGDN03        SCDN03
     C                   MOVE      CGDN04        SCDN04
     C                   MOVE      CGDN05        SCDN05
     C*
     C******************   MOVELCGDESC    SCCGD2
     C*** *** *** ***
     C***        CGRCDS    IFGT SLSECL
     C***                  MOVE '1'       ERRSEC  1
     C***                  ELSE
     C***                  MOVE '0'       ERRSEC
     C***                  END
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        SCDA01
     C                   MOVE      *BLANK        SCDA02
     C                   MOVE      *BLANK        SCDA03
     C                   MOVE      *BLANK        SCDA04
     C                   MOVE      *BLANK        SCDA05
     C                   MOVE      *BLANK        SCDA06
     C                   MOVE      *BLANK        SCDA07
     C                   MOVE      *BLANK        SCDA08
     C                   MOVE      *BLANK        SCDA09
     C                   MOVE      *BLANK        SCDA10
     C*
     C                   MOVE      *BLANK        SCDD01
     C                   MOVE      *BLANK        SCDD02
     C                   MOVE      *BLANK        SCDD03
     C                   MOVE      *BLANK        SCDD04
     C                   MOVE      *BLANK        SCDD05
     C*
     C                   MOVE      *BLANK        SCDN01
     C                   MOVE      *BLANK        SCDN02
     C                   MOVE      *BLANK        SCDN03
     C                   MOVE      *BLANK        SCDN04
     C                   MOVE      *BLANK        SCDN05
     C*
     C                   END
     C*** NOT HERE *** THIS DONE IN CLIENT APP (VDARTS)
     C*
     C* * * * WHICH ALPHA FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     70            Y
     C*
     C                   DO        10            X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DA(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SA(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DA(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C*
     C* * * * WHICH NUMERIC FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     80            Y
     C*
     C                   DO        5             X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DD(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SD(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DD(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C*
     C* * * * WHICH AMOUNT FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     85            Y
     C*
     C                   DO        5             X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DN(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SN(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DN(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '0'           *IN55
     C                   ENDSR
     C*
      *====================================================================*
      * * *   CGGETB SUBROUTINE (CUSTOM GROUP ID - FORMAT B)    C G G E T B ***
      *====================================================================*
     C     CGGETB        BEGSR
     C*
     C     CGKY          CHAIN     PCG01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      CGDB01        SCDB01
     C                   MOVE      CGDB02        SCDB02
     C                   MOVE      CGDB03        SCDB03
     C                   MOVE      CGDB04        SCDB04
     C                   MOVE      CGDB05        SCDB05
     C                   MOVE      CGDB06        SCDB06
     C*
     C*****************    MOVELCGDESC    SCCGD2
     C*
     C***        CGRCDS    IFGT SLSECL
     C***                  MOVE '1'       ERRSEC  1
     C***                  ELSE
     C***                  MOVE '0'       ERRSEC
     C***                  END
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        SCDB01
     C                   MOVE      *BLANK        SCDB02
     C                   MOVE      *BLANK        SCDB03
     C                   MOVE      *BLANK        SCDB04
     C                   MOVE      *BLANK        SCDB05
     C                   MOVE      *BLANK        SCDB06
     C*
     C                   END
     C*** NOT HERE THIS DONE IN CLIENT APP (VDARTS)
     C* * * * WHICH (35A) ALPHA FIELDS TO DISPLAY AND ALLOW INPUT FOR.
     C                   Z-ADD     70            Y
     C*
     C                   DO        6             X
     C                   ADD       1             Y
     C*
     C*** IF THE FIELD IS NOT DEFINED OR THE USER DOES NOT HAVE THE SECURITY
     C*** LEVEL AUTHORIZATION - DO NOT DISPLAY
     C     DB(X)         IFEQ      *BLANK
     C     SLSECL        ORLT      SB(X)
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      *BLANKS       DB(X)
     C                   ELSE
     C                   MOVE      '0'           *IN(Y)
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '1'           *IN55
     C                   ENDSR
** FIELD NAMES
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** UD FIELDS
UD01UD02UD03UD04UD05
** UN FIELDS
UN01UN02UN03UN04UN05
** UB FIELD NAMES (FB)
UB01UB02UB03UB04UB05UB06
