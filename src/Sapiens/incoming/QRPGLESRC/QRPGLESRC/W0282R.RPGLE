     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0282R - GENERATE REPORT DEFINITIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM WILL GENERATE REPORT DEFINITION (WMRDP)    **
     H**     RECORDS FOR ALL FUNDS/FYRS AND EMPLOYERS.  FUND 999 AND **
     H**     EMPLOYER DIVISIONS OTHER THAN 0 ARE IGNORED.  REPORT    **
     H**     DEFINITION RECORDS ARE GENERATED FOR 5 REPORTS:         **
     H**                                                             **
     H**       1) W7536C - SUMMARY OF LOSS - CLIENT LEVEL            **
     H**       2) W7556C - SUMMARY OF LOSS - ACCOUNT LEVEL           **
     H**       3) W7566C - SUMMARY OF LOSS - DEPARTMENT LEVEL        **
     H**       4) W7576C - OPEN CLAIM DETAIL REPORT - DEPT LEVEL     **
     H**       5) W7586C - CLAIM DETAIL REPORT - ALL CLAIMS          **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     COMMAND LINE                                            **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: C:09                                          **
     H**   CREATED:   06/02/94      PROJECT: 59WAW                   **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*   C:58 6/10/00  PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H*  07/01/94 C:09 PROJECT 59WAW                                  *
     H*                MODIFY DATA LOAD FOR THE FOLLOWING REPORT:     *
     H*                  OPEN CLAIM DETAIL REPORT BY DEPT (W7575R)    *
     H*                SINCE THIS REPORT PROMPTS FOR A FUND YEAR, WE  *
     H*                SHOULD BE LOADING '000' INTO FYR TO RUN THIS   *
     H*                REPORT FOR ALL FUND YEARS.                     *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**                                                             **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      55, 65, 75        MAIN READ INDICATORS                 **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**                                                             **
     H** FILES:                                                      **
     H**    WDFNL (PFN01): CO/FND                                    **
     H*** BEGIN DEL ***** 07/01/94 ***************************************
     H******WDF1L*(PF101):*CO/FND/FYR                                **
     H*** END DEL ******* 07/01/94 ***************************************
     H**    WMEML (PEM01): CO/FND/EMP/DIV                            **
     H**    WMRDL (PRD01): KEY                                       **
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     F*** BEGIN DEL ***** 07/01/94 ***************************************
     F*************WDF1L   IF  E           K        DISK
     F*** END DEL ******* 07/01/94 ***************************************
     FWMEML     IF   E           K DISK
     FWMRDL     UF A E           K DISK

     D NAM             S             10    DIM(5) CTDATA PERRCD(1)
     D NUM             S              3    DIM(5) CTDATA PERRCD(1)
     D DES             S             40    DIM(5) CTDATA PERRCD(1)
     D PAR             S             50    DIM(5) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXPAR1                 1     50
     D  XXFL1A                 1      8
     D  XXFND1                 9     11
     D  XXFL1B                12     50
     D                 DS
     D  XXPAR2                 1     50
     D  XXFL2A                 1      8
     D  XXFND2                 9     11
     D  XXFL2B                12     14
     D  XXEMP2                15     19
     D  XXFL2C                20     50
     D                 DS
     D  XXPAR3                 1     50
     D  XXFL3A                 1      8
     D  XXFND3                 9     11
     D  XXFL3B                12     14
     D  XXFYR3                15     16
     D  XXFL3C                17     19
     D  XXEMP3                20     24
     D  XXFL3D                25     50
     D                 DS
     D  XXPAR4                 1     50
     D  XXFL4A                 1      8
     D  XXFND4                 9     11
     D  XXFL4B                12     24
     D  XXEMP4                25     29
     D  XXFL4C                30     50
     D                 DS
     D  XXDES1                 1     40
     D  XXFL5A                 1     31
     D  XXFND5                32     34
     D  XXHYP1                35     35
     D  XXEMP5                36     40
     D                 DS
     D  XXDES2                 1     40
     D  XXFL6A                 1     37
     D  XXFND6                38     40
     D                 DS
     D  XXDES3                 1     40
     D  XXFL7A                 1     27
     D  XXFND7                28     31
     D  XXHYP2                32     32
     D  XXEMP7                33     37
     D  XXHYP3                38     38
     D  XXFYR7                39     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I************************************************************

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
     C     *LIKE         DEFINE    RDKEY         XXRPKY
     C     *LIKE         DEFINE    EMEMP#        XXSVEM
     C*** BEGIN DEL ***** 07/01/94 ***************************************
     C*********************
     C***FUND*DETAIL*FILE**
     C*********************
     C***********F1KY******KLIST
     C*********************KFLD           FNCO#
     C*********************KFLD           FNFND
     C*** END DEL ******* 07/01/94 ***************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C*
     C*  REPORT DEFINITION FILE
     C*
     C     RDKY          KLIST
     C                   KFLD                    XXRPKY
     C*
     C                   DO        5             X                 3 0
     C*
     C     X             IFEQ      1
     C                   EXSR      SRSUB1
     C                   ELSE
     C     X             IFEQ      4
     C                   EXSR      SRSUB3
     C                   ELSE
     C                   EXSR      SRSUB2
     C                   END                                                    X  =  4
     C                   END                                                    X  =  1
     C*
     C                   END                                                    DO  5  X
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C***************************************************************
     C*                                                             *
     C*  SRSUB1 SUBROUTINE -  SUBR USED TO GENERATE RPT #1          *
     C*                                                             *
     C***************************************************************
     C     SRSUB1        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXCNT1            5 0
     C     *LOVAL        SETLL     PFN01
     C                   READ      PFN01                                  55
     C     *IN55         DOWEQ     '0'
     C**** BEGIN CHG *** 06/10/00 ************************************
     C********** FNFND     IFNE 999
     C     FNTEST        IFNE      'Y'
     C**** END CHANGE ** 06/10/00 ************************************
     C*
     C                   MOVEL     NUM(X)        XXRPKY
     C                   ADD       1             XXCNT1
     C                   MOVE      XXCNT1        XXRPKY
     C*
     C     RDKY          CHAIN     PRD01                              40
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     59            RDCO#
     C                   Z-ADD     *ZEROS        RDFND
     C                   MOVE      XXRPKY        RDKEY
     C                   MOVE      NAM(X)        RDPGNM
     C                   MOVE      *BLANKS       XXDES2
     C                   MOVEL     DES(X)        XXDES2
     C                   MOVE      FNFND         XXFND6
     C                   MOVE      '-'           XXHYP1
     C                   MOVEL     XXDES2        RDDESC
     C*                    MOVE *BLANKS   RDRPGN
     C                   MOVE      *BLANKS       RDOF01
     C                   MOVEL     'QSYSPRT'     RDOF01
     C                   MOVE      *BLANKS       RDOF02
     C                   MOVE      *BLANKS       RDOF03
     C                   MOVE      *BLANKS       RDOF04
     C                   MOVE      *BLANKS       RDOF05
     C                   MOVE      *BLANKS       RDOF06
     C                   MOVE      *BLANKS       RDOF07
     C                   MOVE      *BLANKS       RDOF08
     C                   MOVE      *BLANKS       RDOF09
     C                   MOVE      *BLANKS       RDOF10
     C                   MOVE      PAR(X)        XXPAR1
     C                   MOVE      FNFND         XXFND1
     C                   MOVE      *BLANKS       RDADPM
     C                   MOVEL     XXPAR1        RDADPM
     C                   MOVE      Q1USER        RDUSER
     C                   MOVE      Q1PGM         RDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   MOVE      'A'           RDSTAT
     C                   MOVE      *BLANKS       RDOFDC
     C                   MOVE      *BLANKS       RDOFFR
     C                   WRITE     PRD01
     C                   END                                                    *IN40 = 1
     C*
     C                   END                                                    FNFND <> 999
     C*
     C                   READ      PFN01                                  55
     C*
     C                   END                                                    DOW *IN55 '0'
     C*
     C                   ENDSR
     C*
     C***************************************************************
     C*                                                             *
     C*  SRSUB2 SUBROUTINE -  SUBR USED TO GENERATE RPTS #2,3 & 5   *
     C*                                                             *
     C***************************************************************
     C     SRSUB2        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXCNT2            5 0
     C     *LOVAL        SETLL     PFN01
     C                   READ      PFN01                                  55
     C     *IN55         DOWEQ     '0'
     C**** BEGIN CHG *** 06/10/00 ************************************
     C********** FNFND     IFNE 999
     C     FNTEST        IFNE      'Y'
     C**** END CHANGE ** 06/10/00 ************************************
     C*
     C                   Z-ADD     *ZEROS        XXSVEM
     C     EMKY          CHAIN     PEM01                              75
     C     *IN75         DOWEQ     '0'
     C*
     C     EMEMP#        IFNE      XXSVEM
     C                   MOVEL     NUM(X)        XXRPKY
     C                   ADD       1             XXCNT2
     C                   MOVE      XXCNT2        XXRPKY
     C*
     C     RDKY          CHAIN     PRD01                              40
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     59            RDCO#
     C                   Z-ADD     *ZEROS        RDFND
     C                   MOVE      XXRPKY        RDKEY
     C                   MOVE      NAM(X)        RDPGNM
     C                   MOVE      *BLANKS       XXDES1
     C                   MOVEL     DES(X)        XXDES1
     C                   MOVE      EMFND         XXFND5
     C                   MOVE      EMEMP#        XXEMP5
     C                   MOVE      '-'           XXHYP1
     C                   MOVEL     XXDES1        RDDESC
     C*                    MOVE *BLANKS   RDRPGN
     C                   MOVE      *BLANKS       RDOF01
     C                   MOVEL     'QSYSPRT'     RDOF01
     C                   MOVE      *BLANKS       RDOF02
     C                   MOVE      *BLANKS       RDOF03
     C                   MOVE      *BLANKS       RDOF04
     C                   MOVE      *BLANKS       RDOF05
     C                   MOVE      *BLANKS       RDOF06
     C                   MOVE      *BLANKS       RDOF07
     C                   MOVE      *BLANKS       RDOF08
     C                   MOVE      *BLANKS       RDOF09
     C                   MOVE      *BLANKS       RDOF10
     C     X             IFNE      5
     C                   MOVE      PAR(X)        XXPAR2
     C                   MOVE      EMFND         XXFND2
     C                   MOVE      EMEMP#        XXEMP2
     C                   MOVE      *BLANKS       RDADPM
     C                   MOVEL     XXPAR2        RDADPM
     C                   ELSE
     C                   MOVE      PAR(X)        XXPAR4
     C                   MOVE      EMFND         XXFND4
     C                   MOVE      EMEMP#        XXEMP4
     C                   MOVE      *BLANKS       RDADPM
     C                   MOVEL     XXPAR4        RDADPM
     C                   END                                                    X <> 5
     C                   MOVE      Q1USER        RDUSER
     C                   MOVE      Q1PGM         RDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   MOVE      'A'           RDSTAT
     C                   MOVE      *BLANKS       RDOFDC
     C                   MOVE      *BLANKS       RDOFFR
     C                   WRITE     PRD01
     C                   END                                                    *IN40 = 1
     C                   Z-ADD     EMEMP#        XXSVEM
     C                   END                                                    EMEMP#<>XXSVEM
     C*
     C     EMKY          READE     PEM01                                  75
     C                   END                                                    DOW *IN75 '0'
     C*
     C                   END                                                    FNFND <> 999
     C*
     C                   READ      PFN01                                  55
     C*
     C                   END                                                    DOW *IN55 '0'
     C*
     C                   ENDSR
     C***************************************************************
     C*                                                             *
     C*  SRSUB3 SUBROUTINE -  SUBR USED TO GENERATE RPT #4          *
     C*                                                             *
     C***************************************************************
     C     SRSUB3        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXCNT3            5 0
     C     *LOVAL        SETLL     PFN01
     C                   READ      PFN01                                  55
     C     *IN55         DOWEQ     '0'
     C**** BEGIN CHG *** 06/10/00 ************************************
     C********** FNFND     IFNE 999
     C     FNTEST        IFNE      'Y'
     C**** END CHANGE ** 06/10/00 ************************************
     C*
     C*** BEGIN DEL ***** 07/01/94 ***************************************
     C***********F1KY******CHAINPF101                65
     C************IN65*****DOWEQ'0'
     C*** END DEL ******* 07/01/94 ***************************************
     C*
     C                   Z-ADD     *ZEROS        XXSVEM
     C     EMKY          CHAIN     PEM01                              75
     C     *IN75         DOWEQ     '0'
     C*
     C     EMEMP#        IFNE      XXSVEM
     C                   MOVEL     NUM(X)        XXRPKY
     C                   ADD       1             XXCNT3
     C                   MOVE      XXCNT3        XXRPKY
     C*
     C     RDKY          CHAIN     PRD01                              40
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     59            RDCO#
     C                   Z-ADD     *ZEROS        RDFND
     C                   MOVE      XXRPKY        RDKEY
     C                   MOVE      NAM(X)        RDPGNM
     C                   MOVE      *BLANKS       XXDES3
     C                   MOVEL     DES(X)        XXDES3
     C                   MOVE      EMFND         XXFND7
     C                   MOVE      EMEMP#        XXEMP7
     C*** BEGIN DEL ***** 07/01/94 ***************************************
     C*********************MOVE F1FYR     XXFYR7
     C*** END DEL ******* 07/01/94 ***************************************
     C                   MOVE      '-'           XXHYP2
     C*** BEGIN DEL ***** 07/01/94 ***************************************
     C*********************MOVE '-'       XXHYP3
     C*** END DEL ******* 07/01/94 ***************************************
     C                   MOVEL     XXDES3        RDDESC
     C*                    MOVE *BLANKS   RDRPGN
     C                   MOVE      *BLANKS       RDOF01
     C                   MOVEL     'QSYSPRT'     RDOF01
     C                   MOVE      *BLANKS       RDOF02
     C                   MOVE      *BLANKS       RDOF03
     C                   MOVE      *BLANKS       RDOF04
     C                   MOVE      *BLANKS       RDOF05
     C                   MOVE      *BLANKS       RDOF06
     C                   MOVE      *BLANKS       RDOF07
     C                   MOVE      *BLANKS       RDOF08
     C                   MOVE      *BLANKS       RDOF09
     C                   MOVE      *BLANKS       RDOF10
     C                   MOVE      PAR(X)        XXPAR3
     C                   MOVE      EMFND         XXFND3
     C                   MOVE      EMEMP#        XXEMP3
     C*** BEGIN CHG ***** 07/01/94 ***************************************
     C*********************MOVE F1FYR     XXFYR3
     C                   MOVE      *ZEROS        XXFYR3
     C*** END CHG ******* 07/01/94 ***************************************
     C                   MOVE      *BLANKS       RDADPM
     C                   MOVEL     XXPAR3        RDADPM
     C                   MOVE      Q1USER        RDUSER
     C                   MOVE      Q1PGM         RDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   MOVE      'A'           RDSTAT
     C                   MOVE      *BLANKS       RDOFDC
     C                   MOVE      *BLANKS       RDOFFR
     C                   WRITE     PRD01
     C                   END                                                    *IN40 = 1
     C                   Z-ADD     EMEMP#        XXSVEM
     C                   END                                                    EMEMP#<>XXSVEM
     C*
     C     EMKY          READE     PEM01                                  75
     C                   END                                                    DOW *IN75 '0'
     C*
     C*** BEGIN DEL ***** 07/01/94 ***************************************
     C***********F1KY******READEPF101                    65
     C*********************END                             DOW *IN65 '0'
     C*** END DEL ******* 07/01/94 ***************************************
     C*
     C                   END                                                    FNFND <> 999
     C*
     C                   READ      PFN01                                  55
     C*
     C                   END                                                    DOW *IN55 '0'
     C*
     C                   ENDSR
**  TABLE NAM
W7536C
W7556C
W7566C
W7576C
W7586C
**  TABLE NUM
001
002
003
005
006
**  TABLE DES
SUMMARY OF LOSS - CLIENT LEVEL
SUMMARY OF LOSS - ACCT LEVEL
SUMMARY OF LOSS - DEPT LEVEL
OPEN CLM DTL RPT-DEPT LVL
CLAIM DTL REPORT- ALL CLAIMS
**  TABLE PAR
('059' '   ' 'RNDATE' ' ')
('059' '   ' '     ' 'RNDATE' 'Y' 'N' ' ')
('059' '   ' '     ' ' ' 'RNDATE' ' ')
('059' '   ' '  ' '     ' ' ' 'RNDATE' ' ')
('059' '   ' '00' '99' '     ' ' ' 'RNDATE' ' ')
