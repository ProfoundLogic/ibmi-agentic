     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7431R - BUILD WORK FILE - LOSS RATIO VARIOUS SEQUENCES
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/15/99 C:15 Project: WC577                                 *
     H*                Y2K - Chgd the length of PLLOSS to 7.2.        *
     H*                                                               *
     H*  06/14/99 C:90 SEQ#: WA033/18419                              *
     H*                ADDED PROCESSING TO INCLUDE FLAT CX ACCTS.     *
     H*                                                               *
     H*  02/02/99 C:61 PROJECT 96WBI                                  *
     H*                Load new work file fields:                     *
     H*                  PLRTST - Rating State                        *
     H*                  PLRGN# - Region Code                         *
     H*                  PLRDES - Region Description                  *
     H*                  PLGNAM - Group Name                          *
     H*                  PLCNAM - Company Name                        *
     H*                                                               *
     H* 01/18/99 C:113 PROJECT: SUPPORT #15991                        *
     H*                Check audited flag(ELFAF) - if 'A' use ELAARP  *
     H*                for selection, else use ELANPR.                *
     H*                                                               *
     H* 08/23/95 C:24 PROJECT: CM447                                  *
     H*               COMMERCIAL ENHANCEMENTS - ADD SELECTION BY      *
     H*               ACCIDENT DATE RANGE FOR NONCOMMON EXPIRATION    *
     H*               DATE FUNDS.  ADD SELECTION OF AN EMPLOYER RANGE *
     H*               FOR REPORTS SORTED BY EMPLOYER NUMBER.          *
     H*****************************************************************
      *
      *
     FWMCML     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWXPLP     O    E             DISK
      ***** BEG ADD ******* 02/02/99 **********************************
      *
      * Rating State
     FWMRSL     IF   E           K DISK
      *
      * Region
     FWMREL     IF   E           K DISK
      *
      * Fund
     FWMFNL     IF   E           K DISK
      *
      * Company
     FSMCOL     IF   E           K DISK
      *
      ***** END ADD ******* 02/02/99 **********************************
      *
      *
      *
      *

     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXYMD
      ***** BEG ADD ******* 02/02/99 **********************************
      *
      * Rating State
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      * Region
      *
     C     REKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMRGN#
      *
      * Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      * Company
      *
     C     COKY          KLIST
     C                   KFLD                    EMCO#
      *
      ***** END ADD ******* 02/02/99 **********************************
     C*
     C     *LIKE         DEFINE    EMDT          XXFDAT
     C     *LIKE         DEFINE    EMDT          XXTDAT
     C     *LIKE         DEFINE    EMDT          XXYMD
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMEMP#        XXTEMP
     C     *LIKE         DEFINE    EMDIV         XXTDIV
     C     *LIKE         DEFINE    PLOPEN        XXTOTC
     C*** BEGIN ADD *** 01/18/99 **************************************
     C     *LIKE         DEFINE    ELANPR        XXANPR
     C*** END   ADD *** 01/18/99 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQFRPR           11
     C                   PARM                    QQTOPR           11
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQTDAT            8
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQFDIV            5
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQTDIV            5
     C                   PARM                    QQCMFL            1
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
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   Z-ADD     XXFYR         XXYR              3 0
     C                   MOVE      QQDTE         XXDATE            8 0
     C                   MOVE      QQFRPR        XXFRPR           11 2
     C                   MOVE      QQTOPR        XXTOPR           11 2
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C                   MOVE      QQTDAT        X1TDAT            8 0
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   Z-ADD     X1TDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT
     C                   MOVE      QQFEMP        XXEMP#
     C                   MOVE      QQFDIV        XXDIV
     C                   MOVE      QQTEMP        XXTEMP
     C                   MOVE      QQTDIV        XXTDIV
     C*
     C     XXYR          COMP      0                                      35
     C*
     C*  READ EMPLOYER FILE
     C*
     C     EMKY1         SETLL     PEM01
     C     EMKEY         READE     PEM01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  VERIFY EMPLOYER IS IN THE REQUESTED RANGE
     C*
     C     EMEMP#        IFGT      XXTEMP
     C     EMEMP#        OREQ      XXTEMP
     C     EMDIV         ANDGT     XXTDIV
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDIF
     C     *IN60         DOWEQ     '0'
     C*
     C  N35ELKEY         CHAIN     PEL01                              60
     C   35ELKEY1        CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 01/18/99 **************************************
     C*
     C*  If the employer has been audited, use the audited premium.
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        XXANPR
     C*
     C                   ELSE
     C                   Z-ADD     ELANPR        XXANPR
     C*
     C                   ENDIF                                                  ELFAF=A
     C*** END   ADD *** 01/18/99 **************************************
     C*
     C*  FOR NONCOMMON EXPIRATION, DO NOT PROCESS FUND YEARS
     C*  OUTSIDE OF THE ACCIDENT DATE RANGE, OR IF ANNUAL
     C*  ESTMATED PREMIUM FALLS OUTSIDE OF THE PREMIUM RANGE.
     C*
     C     QQCMFL        IFEQ      *BLANK
     C     ELPFDT        ORLT      XXTDAT
     C     ELPTDT        ANDGT     XXFDAT
     C*** BEGIN CHG *** 01/18/99 **************************************
     C********** ELANPR    ANDGEXXFRPR
     C********** ELANPR    ANDLEXXTOPR
     C     XXANPR        ANDGE     XXFRPR
     C     XXANPR        ANDLE     XXTOPR
     C*** END   CHG *** 01/18/99 **************************************
      **** BEGIN ADD **** 06/14/99 ************************************
     C     ELPFDT        ORGE      XXFDAT
     C     ELPFDT        ANDLE     XXTDAT
     C     ELPFDT        ANDEQ     ELPTDT
      **** END   ADD **** 06/14/99 ************************************
     C*
     C                   Z-ADD     ELFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     0             PLCLOS
     C                   Z-ADD     0             PLOPEN
     C                   Z-ADD     0             PLINC
     C                   Z-ADD     0             PLPAID
     C                   EXSR      WRIT
     C*
     C                   END
     C*
     C                   ENDIF                                                  QQCMFL = *BLANK
     C*
     C  N35              MOVE      '1'           *IN60
     C   35ELKEY1        READE     PEL01                                  60
     C                   END                                                                8
     C*
     C     EMKEY         READE     PEM01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  VERIFY EMPLOYER IS IN THE REQUESTED RANGE
     C*
     C     EMEMP#        IFGT      XXTEMP
     C     EMEMP#        OREQ      XXTEMP
     C     EMDIV         ANDGT     XXTDIV
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDIF
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*==================================================================*
     C*   Level 1 Calculations             ** Subroutine **   W R I T    *
     C*==================================================================*
     C*
     C     WRIT          BEGSR
     C*
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM THIS PERIOD
     C**
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     ELEMP#        SREMP#            9
     C                   MOVEL     ELDIV         SRDIV             5
     C                   MOVEL     XXDATE        QQDTE             8
     C                   MOVE      QQFDAT        SRFRDT            8
     C                   MOVE      QQTDAT        SRTODT            8
     C*
     C     QQCMFL        IFEQ      *ON
     C                   MOVE      'Y'           SRRNGE            1
     C                   ELSE
     C                   MOVE      'N'           SRRNGE
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SRERR             1
     C                   MOVE      *BLANK        SRANN            11
     C                   MOVE      *BLANK        SRANNM           11
     C                   MOVE      *BLANK        SRNRM            11
     C                   MOVE      *BLANK        SRNRMM           11
     C                   MOVE      *BLANK        SRSTD            11
     C                   MOVE      *BLANK        SRSTDM           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SREPT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRFRDT
     C                   PARM                    SRTODT
     C                   PARM                    SRRNGE
     C                   PARM                    SRANN
     C                   PARM                    SRANNM
     C                   PARM                    SRNRM
     C                   PARM                    SRNRMM
     C                   PARM                    SRSTD
     C                   PARM                    SRSTDM
     C                   PARM                    SRERR
     C                   PARM                    QQTERM
     C*
     C                   MOVE      SRANN         PRMYTD           11 2
     C                   MOVE      SRANNM        PRMCUR           11 2
     C*
     C*  FOR COMMON EXPIRATION DATE FUNDS, VERIFY THAT EARNED PREMIUM
     C*  FALLS WITHIN THE PREMIUM RANGE.
     C*
     C     PRMYTD        IFGE      XXFRPR
     C     PRMYTD        ANDLE     XXTOPR
     C     QQCMFL        ORNE      *BLANK
     C*
     C                   Z-ADD     PRMYTD        PLPREM                                     8
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C     QQCMFL        IFEQ      *BLANK
     C     CMACDT        ORGE      XXFDAT
     C     CMACDT        ANDLE     XXTDAT
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '1'
     C     CHCO#         ORNE      CMCO#
     C     CHFND         ORNE      CMFND
     C     CHFYR         ORNE      CMFYR
     C     CHCASE        ORNE      CMCASE
     C     CHEFDT        ORGT      XXYMD
     C                   MOVE      CMSTAT        CHOCST
     C                   END
     C*
     C     CHOCST        IFEQ      'C'
     C                   ADD       1             PLCLOS
     C                   ELSE
     C                   ADD       1             PLOPEN
     C                   END
     C*
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
     C*
     C                   XFOOT     CMA           TOTPMT            9 2
     C                   XFOOT     CMR           TOTRES            9 2
     C*
     C                   ADD       TOTPMT        PLINC            11 2
     C                   ADD       TOTRES        PLINC
     C                   ADD       TOTPMT        PLPAID           11 2
     C*
     C                   ENDIF
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C*
     C*    CALCULATE LOSS RATIO
     C*
     C     PLOPEN        ADD       PLCLOS        XXTOTC
     C     PLPREM        IFNE      *ZERO
     C     PLINC         MULT      100           XXITOT           13 2
     C*** BEGIN CHG *** 12/15/99 **************************************
     C********** XXITOT    DIV  PLPREM    PLLOSS  62H
     C     XXITOT        DIV(H)    PLPREM        PLLOSS
     C*** END CHG ***** 12/15/99 **************************************
     C                   ELSE
     C                   Z-ADD     *ZERO         PLLOSS
     C                   END
     C*
     C*    CALCULATE FREQUENCY RATIO
     C*
     C     PLOPEN        ADD       PLCLOS        XXTOTC
     C     PLPREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     XXTOTC        DIV(H)    THPREM        PLFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         PLFREQ
     C                   END
     C*
     C                   Z-ADD     XXCO#         PLCO#
     C                   Z-ADD     XXFND         PLFND
     C                   Z-ADD     ELFYR         PLFYR
     C                   Z-ADD     ELEMP#        PLEMP#
     C                   Z-ADD     ELDIV         PLDIV
      ***** BEG ADD ******* 02/02/99 **********************************
     C                   MOVEL     EMNAME        PLNAME
      ***** END ADD ******* 02/02/99 **********************************
     C                   MOVE      Q1USER        PLUSER
     C                   MOVE      Q1PGM         PLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PLDT
      ***** BEG ADD ******* 02/02/99 **********************************
      *
      *  Rating State
      *
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVEL(P)  RSJRST        PLRTST
     C   61              MOVEL(P)  *BLANKS       PLRTST
      *
      *  Region Code / Description
      *
     C                   Z-ADD     EMRGN#        PLRGN#
      *
     C     REKY          CHAIN     PRE01                              61
     C  N61              MOVEL(P)  REDESC        PLRDES
     C   61              MOVEL(P)  *BLANKS       PLRDES
      *
      *  Group Name
      *
     C     FNKY          CHAIN     PFN01                              61
     C  N61              MOVEL(P)  FNNAME        PLGNAM
     C   61              MOVEL(P)  *BLANKS       PLGNAM
      *
      *  Company Name
      *
     C     COKY          CHAIN     SMCOL                              61
     C  N61              MOVEL(P)  CONAME        PLCNAM
     C   61              MOVEL(P)  *BLANKS       PLCNAM
      *
      ***** END ADD ******* 02/02/99 **********************************
     C                   WRITE     PPL01
     C*
     C                   END
     C*
     C                   ENDSR
     C**
