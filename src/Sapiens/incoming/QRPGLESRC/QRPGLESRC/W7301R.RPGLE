     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7301R - Agent Loss Ratio Report Work File Output
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 01/08/10  C:238  Project: 139WNI
     H*                  Agent Loss Ratio Report for all Groups
     F*                                                               *
     F* 10/20/09 C:189 PROJECT WC1726                                 *
     F*                CSB-AGT LOSS RATIO/PPR RPT SELECTION CHG       *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*                MADE SOME CHANGES AS PER THE REQUIREMENT.      *
     H*                                                               *
     H* 05/12/04 C:62 PROJECT: WC1215                                 *
     H*               PROCESS EFFECTIVE DATE RANGE INSTEAD OF FYR     *
     H*                                                               *
     H* 12/14/99 C:90 PROJECT: WC577                                  *
     H*               Y2K FIX - CORRECTLY LOAD ALDT                   *
     H*                                                               *
     H* 12/16/92 C:09 PROJECT WC183                                   *
     H*               ADDED EFFECTIVE DATE TO THE AGENT HISTORY KEY   *
     H*               LIST TO GET THE LAST EFFECTIVE AGENT.           *
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE                             *
     H*                                                              ****
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H*****************************************************************
     FWMAHL     IF   E           K DISK
     F* * * Agent History File
     F*
     FWMAAL1    IF   E           K DISK
     F* * * Agent Assignment File
     F*
     FWMAGL     IF   E           K DISK
     F* * * Agent Master File
     F*
     FWXALL     O    E             DISK
     F*** BEGIN ADD *** 05/12/04 **************************************
     FWDELL     IF   E           K DISK
     F* * * Employer Detail File
     F*
     F*** END   ADD *** 05/12/04 **************************************
      *** BEG ADD **** 07/16/07 ************************************
     FWMEAL     IF   E           K DISK
     FWTABL     IF   E           K DISK
      *** END ADD **** 07/16/07 ************************************
     F*** BEG   ADD *** 01/08/10 **************************************
     FWXGSL     IF   E           K DISK

     D*** END   ADD *** 01/08/10 **************************************
     F* * * Work File Output
     F*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     AHKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR             3 0
     C     AHKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*** BEGIN ADD *** 05/12/04 **************************************
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHEMP#
     C                   KFLD                    AHDIV
     C                   KFLD                    AHFYR
     C*** END   ADD *** 05/12/04 **************************************
     C*
      *** BEG ADD **** 07/16/07 ************************************
     C*  EMPLOYER DETAIL FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHEMP#
     C                   KFLD                    AHDIV
     C                   KFLD                    XXFYR
      *
     C     TTKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    KKKEY            10
      *
      *** END ADD **** 07/16/07 ************************************
     C     *LIKE         DEFINE    AHEMP#        XXEMP#
     C     *LIKE         DEFINE    AHDIV         XXDIV
     C     *LIKE         DEFINE    AHDT          XXEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           PARM           QQFYR   3
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C*** END   CHG *** 05/12/04 **************************************
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      *** BEG ADD **** 10/20/09 ************************************
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQPROD            5
      *** END ADD **** 10/20/09 ************************************
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           MOVE QQFYR     XXFYR   30
     C                   MOVE      QQFRDT        XXFRDT            7 0
     C                   MOVE      QQTODT        XXTODT            7 0
     C*** END   CHG *** 05/12/04 **************************************
     C                   MOVE      QQDTE         XXDATE            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
      *** BEG ADD **** 10/20/09 ************************************
     C                   Z-ADD     0             X@AGT#            5 0
     C                   Z-ADD     0             X@PROD            5 0
     C*
     C     Q1PRMS        IFGT      8
     C*
     C                   MOVE      QQAGT#        X@AGT#
     C                   MOVE      QQPROD        X@PROD
     C*
     C                   ENDIF
     C*
      *** END ADD **** 10/20/09 ************************************
     C                   EXSR      SR1ST
     C*** BEG   ADD *** 01/08/10 **************************************
     C     COKEY         READE     WXGSL                                  50
     C     *IN50         DOWEQ     *OFF
     C                   MOVE      GSCO#         XXCO#
     C                   MOVE      GSFND         XXFND
     C                   EXSR      PROC
     C     COKEY         READE     WXGSL                                  50
     C                   ENDDO
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C*
     C     PROC          BEGSR
     C*** END   ADD *** 01/08/10 **************************************
     C*  READ AGENT HISTORY FILE
     C*
     C*          AHKEY -------------- CO#, FND
     C     AHKEY         CHAIN     PAH01                              50
     C     *IN50         DOWEQ     '0'
     C                   Z-ADD     AHEMP#        XXEMP#
     C                   Z-ADD     AHDIV         XXDIV
     C*** BEGIN ADD *** 05/12/04 **************************************
     C                   Z-ADD     AHFYR         XXFYR
     C*** END   ADD *** 05/12/04 **************************************
     C*          AHKEY2 ------------- CO#, FND, EMP#, DIV, FYR
     C     AHKEY2        SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END
     C*
     C                   MOVE      '0'           *IN51
     C     *IN50         IFEQ      '1'
     C     AHCO#         ORNE      XXCO#
     C     AHFND         ORNE      XXFND
     C     AHEMP#        ORNE      XXEMP#
     C     AHDIV         ORNE      XXDIV
     C     AHFYR         ORNE      XXFYR
     C                   MOVE      '1'           *IN51
     C                   END
      *** BEG ADD **** 10/20/09 ************************************
     C*
     C     X@AGT#        IFNE      0
     C     AHAGT#        ANDNE     X@AGT#
     C                   MOVE      '1'           *IN51
     C                   END
     C*
     C     X@PROD        IFNE      0
     C     AHPROD        ANDNE     X@PROD
     C                   MOVE      '1'           *IN51
     C                   END
     C*
      *** END ADD **** 10/20/09 ************************************
     C*** BEGIN ADD *** 05/12/04 **************************************
     C     *IN51         IFEQ      *OFF
     C     ELKY          CHAIN     PEL01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        51
     C                   ELSE
     C     ELPFDT        IFLT      XXFRDT
     C     ELPFDT        ORGT      XXTODT
     C                   SETON                                        51
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/12/04 **************************************
     C*
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     AHAGT#        XXAGT#
     C**
     C*          AAKEY -------------- CO#, FND, AGT#
     C     AAKEY         CHAIN     PAA01                              80
     C                   Z-ADD     AAFEIN        XXFEIN            9 0
     C                   Z-ADD     AAMOD         XXMOD             3 0
     C**
     C*          AGKEY -------------- CO#, FEIN, MOD
     C     AGKEY         CHAIN     PAG01                              80
     C                   MOVEL     AGNAME        ANAME            23
     C**
     C*
     C*  PREPARE FIELDS FOR OUTPUT
     C                   Z-ADD     AHCO#         ALCO#
     C                   Z-ADD     AHFND         ALFND
     C                   Z-ADD     XXFYR         ALFYR
     C                   Z-ADD     AHAGT#        ALAGT#
     C                   Z-ADD     AHPROD        ALPROD
     C                   Z-ADD     AHEMP#        ALEMP#
     C                   Z-ADD     AHDIV         ALDIV
     C                   MOVE      Q1PGM         ALPGM
     C                   Z-ADD     DSTIME        ALTIME
      **** BEGIN CHG **** 12/14/99 ************************************
     C******************** Z-ADDDSDATE    ALDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
      **** END   CHG **** 12/14/99 ************************************
     C                   MOVE      Q1USER        ALUSER
     C                   Z-ADD     AACHAP        ALCHAP
     C                   Z-ADD     AGAREA        ALAREA
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVE AGCNTY    ALCNTY
     C     AGPST         IFNE      *BLANKS
     C*
     C                   MOVEL     AGPST         ALCNTY
     C*
     C                   ELSE
     C*
     C                   MOVEL     AGMST         ALCNTY
     C*
     C                   ENDIF                                                  AGPST
     C*
     C                   MOVE      AGCNTY        ALCNTY
     C*
     C*** END   CHG *** 06/26/97 **************************************
      *** BEG ADD **** 07/16/07 ************************************
     C                   Z-ADD     *ZERO         ALPMTS
      *
     C     EAKY          CHAIN     PEA01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'P'
      *
     C     EAARTY        IFEQ      'P'
     C                   ADD       EAAMT         ALPMTS
     C                   ENDIF                                                  EAARTY
      *
     C     EAARTY        IFEQ      'R'
     C     EAARTY        OREQ      'N'
     C                   SUB       EAAMT         ALPMTS
     C                   ENDIF                                                  EAARTY
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *** END ADD **** 07/16/07 ************************************
     C**
     C                   WRITE     PAL01
     C                   END
     C*** BEGIN CHG *** 05/12/04 **************************************
     C********** AHKEY3    SETGTPAH01
     C     AHKEY2        SETGT     PAH01
     C*** END   CHG *** 05/12/04 **************************************
     C     AHKEY         READE     PAH01                                  50
     C                   END
     C*
     C*** BEG   ADD *** 01/08/10 **************************************
     C                   ENDSR
     C*** END   ADD *** 01/08/10 **************************************
     C*
     C*** BEG   DEL *** 01/08/10 **************************************
     C*                    MOVE '1'       *INLR
     C*** END   DEL *** 01/08/10 **************************************
     C**
     C**
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
     C**
