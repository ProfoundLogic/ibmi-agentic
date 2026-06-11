     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4122R5 - WORKFILE BUILD - EXPERIENCE BY PAYROLL CLASS RPT
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: W4121C5 - OVERRIDE CL                            *
     H*                                                               *
     H* CALLS TO:    NONE                                             *
     H*                                                               *
     H* CREATED:  03/16/00   C:61  PROJECT WC741                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/20/09 C:190 Project: WC1473                                *
     H*                Load the new class code extension field.       *
     H*                                                               *
     H* 05/23/05 C:33  Project: WC1252                                *
     H*                Load 'Anniversary Rating' fields.              *
     H*                                                               *
     H* 07/29/04 C:61  PROJECT WC741                                  *
     H*                Process for entered "Group range".             *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
      * Employer Payroll
     F********BEG CHG***04/20/09**************************************
     FWMEPL7E   IF   E           K DISK
     F*******WMEPL7  IF  E           K        DISK
     F********END CHG***04/20/09**************************************
      *
      * Employer Self-Reporting Payroll
     F********BEG CHG***04/20/09**************************************
     FWASRL4E   IF   E           K DISK
     F*******WASRL4  IF  E           K        DISK
     F********END CHG***04/20/09**************************************
      *
      * Table - (billing schedule code)
     FWTABL     IF   E           K DISK
      *
      * Claims
     FWMCML5    IF   E           K DISK
      *
      * Internal Workfile - Experience by Payroll Class Report
     FWXPYP     O    E             DISK
      *
      *
      *
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
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
      *
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
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      ************************************************************
      *===============================================================
      *  KLIST
      *===============================================================
      *
      *********
      ***** BEG DEL ******* 07/29/04 **********************************
      *********
      ***Employer Detail
      *********

     C*********  ELKY      KLIST
     C*********            KFLD           KKCO#
     C*********            KFLD           KKFND
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG ADD ******* 07/29/04 **********************************
      *
      *  Employer Detail
      *
     C     ELKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFFND
      *
     C     ELKY2         KLIST
     C                   KFLD                    KKCO#
      ***** END ADD ******* 07/29/04 **********************************
      *
      *  Employer Payroll
      *
     C     EPKY          KLIST
      ***** BEG CHG ******* 07/29/04 **********************************
     C*************        KFLD           KKCO#
     C*************        KFLD           KKFND
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      ***** END CHG ******* 07/29/04 **********************************
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     EPKY1         KLIST
      ***** BEG CHG ******* 07/29/04 **********************************
     C*************        KFLD           KKCO#
     C*************        KFLD           KKFND
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      ***** END CHG ******* 07/29/04 **********************************
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKCLAS
      *
     C     EPKY2         KLIST
     C                   KFLD                    K1CO#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1EMP#
     C                   KFLD                    K1DIV
     C                   KFLD                    K1CLAS
     C                   KFLD                    K1SPEC
     C***  Begin Add  ***  04/20/09  **************************************
     C                   KFLD                    K1EXT             5
     C***  End   Add  ***  04/20/09  **************************************
      *
      *  Billing schedule code
      *
     C     BSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKKEY
      *
      *  Claims master
      *
     C     CMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *  Employer Self - Reporting Payroll
      *
     C     SRKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPFYR
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPSPEC
     F********BEG ADD***04/20/09**************************************
     C                   KFLD                    EPEXT
     F********END ADD***04/20/09**************************************
      *
      *
      ************************************************************
      *===============================================================
      *   *LIKE defines
      *===============================================================
      *
     C     *LIKE         DEFINE    ELPFDT        XXFRDT
     C     *LIKE         DEFINE    ELPTDT        XXTODT
     C     *LIKE         DEFINE    EPCO#         KKCO#
      ***** BEG CHG ******* 07/29/04 **********************************
     C*********  *LIKE     DEFN EPFND     KKFND
     C     *LIKE         DEFINE    EPFND         KKFFND
     C     *LIKE         DEFINE    EPFND         KKTFND
      ***** END CHG ******* 07/29/04 **********************************
     C     *LIKE         DEFINE    EPCLAS        KKCLAS
     C     *LIKE         DEFINE    BSKEY         KKKEY
     C     *LIKE         DEFINE    PYPAMT        XXPAMT
     C     *LIKE         DEFINE    PYEPRM        XXMPRM
      *
     C     *LIKE         DEFINE    EPCO#         K1CO#
     C     *LIKE         DEFINE    EPFND         K1FND
     C     *LIKE         DEFINE    EPFYR         K1FYR
     C     *LIKE         DEFINE    EPEMP#        K1EMP#
     C     *LIKE         DEFINE    EPDIV         K1DIV
     C     *LIKE         DEFINE    EPCLAS        K1CLAS
     C     *LIKE         DEFINE    EPSPEC        K1SPEC
      *
      ************************************************************
      *
      *  PLIST definitions
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
      ***** BEG CHG ******* 07/29/04 **********************************
     C*********            PARM           @@FND   3
     C                   PARM                    @@FFND            3
     C                   PARM                    @@TFND            3
      ***** END CHG ******* 07/29/04 **********************************
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@CLAS            5
     C                   PARM                    @@RNDT            8
     C                   PARM                    @@SRPR            1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C***  Begin Add  ***  05/23/05  **************************************
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C***  End   Add  ***  05/23/05  **************************************
      *
      *
      *
      ************************************************************
      *  Begin
      ************************************************************
      *
      *  INITIALIZE
      *
     C                   MOVEL     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   MOVEL     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C                   MOVE      @@CO#         KKCO#
      ***** BEG CHG ******* 07/29/04 **********************************
     C*************        MOVE @@FND     KKFND
     C                   MOVE      @@FFND        KKFFND
     C                   MOVE      @@TFND        KKTFND
      ***** END CHG ******* 07/29/04 **********************************
     C                   MOVE      @@CLAS        KKCLAS
      *
     C     KKCLAS        IFNE      *ZERO
     C                   SETON                                        71
     C                   ENDIF                                                  KKCLAS
      *
      *  READ EMPLOYER DETAIL
      *
      ***** BEG DEL ******* 07/29/04 **********************************
     C*********  ELKY      CHAINPEL01                60
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG ADD ******* 07/29/04 **********************************
     C     ELKY1         SETLL     PEL01
     C     ELKY2         READE     PEL01                                  60
      ***** END ADD ******* 07/29/04 **********************************
      *
     C     *IN60         DOWEQ     '0'
      ***** BEG ADD ******* 07/29/04 **********************************
      *
     C     ELFND         IFLE      KKTFND
      ***** END ADD ******* 07/29/04 **********************************
      *
     C     ELPFDT        IFGE      XXFRDT
     C     ELPFDT        ANDLT     XXTODT
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C***  Begin Add  ***  05/23/05  **************************************
     C     *LIKE         DEFINE    ELANDT        X7ANDT
     C     *LIKE         DEFINE    ELANDT        X7AND1
     C     *LIKE         DEFINE    PYARAT        XXARAT
     C     *LIKE         DEFINE    PYANRT        XXANRT
     C*
     C                   CLEAR                   X7ANDT
     C                   CLEAR                   X7AND1
     C                   CLEAR                   XXANRT
     C                   CLEAR                   XXARAT
     C***  End   Add  ***  05/23/05  **************************************
      *
     C  N71EPKY          CHAIN     PEP01                              60        ALL CLASSES
     C   71EPKY1         CHAIN     PEP01                              60        ONE CLASS
      *
     C     *IN60         DOWEQ     *OFF
      *
      *   Accumulate payroll and premium amount for estimating employer
      *
     C                   Z-ADD     EPCO#         K1CO#
     C                   Z-ADD     EPFND         K1FND
     C                   Z-ADD     EPFYR         K1FYR
     C                   Z-ADD     EPEMP#        K1EMP#
     C                   Z-ADD     EPDIV         K1DIV
     C                   Z-ADD     EPCLAS        K1CLAS
     C                   MOVE      EPSPEC        K1SPEC
     C***  Begin Add  ***  04/20/09  **************************************
     C                   MOVE      EPEXT         K1EXT
     C***  End   Add  ***  04/20/09  **************************************
      *
     C                   Z-ADD     *ZERO         XXPAMT
     C                   Z-ADD     *ZERO         XXMPRM
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EPSTAT        IFEQ      'A'
     C*
     C***  Begin Add  ***  05/23/05  **************************************
     C     ELANDT        IFNE      *ZERO
     C                   Z-ADD     ELANDT        X7ANDT
     C     X7ANDT        ADD       10000         X7AND1                         Ann Rt Dt + 1yr
     C                   ENDIF
     C*
     C     ELANDT        IFNE      *ZERO
     C*
     C                   SELECT
     C     EPEFDT        WHENLT    X7AND1
     C                   Z-ADD     EPARAT        XXANRT
     C     EPEFDT        WHENGE    X7AND1
     C                   Z-ADD     EPARAT        XXARAT
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     EPEFDT        WHENLE    ELPFDT
     C     XXARAT        OREQ      *ZERO
     C                   Z-ADD     EPARAT        XXARAT
     C                   ENDSL
     C*
     C                   ENDIF
     C***  End   Add  ***  05/23/05  **************************************
      *
     C     ELFAF         IFEQ      'A'
     C                   ADD       EPAAPR        XXPAMT
     C                   ADD       EPAMPR        XXMPRM
      *
     C                   ELSE
      *
     C                   ADD       EPEAPR        XXPAMT
     C                   ADD       EPEMPR        XXMPRM
     C                   ENDIF                                                  ELFAF
      *
     C                   ENDIF                                                  EPSTAT
      *
     C     EPKY2         READE     PEP01                                  60
     C                   ENDDO
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     EPKY2         SETGT     PEP01
     C  N71EPKY          READE     PEP01                                  60     ALL CLASSES
     C   71EPKY1         READE     PEP01                                  60     ONE CLASS
     C                   ENDDO
      *
     C                   ENDIF
      *
      ***** BEG DEL ******* 07/29/04 **********************************
     C*********  ELKY      READEPEL01                    60
      ***** END DEL ******* 07/29/04 **********************************
      ***** BEG ADD ******* 07/29/04 **********************************
     C                   ENDIF                                                  < TFND
      *
     C     ELKY2         READE     PEL01                                  60
      ***** END ADD ******* 07/29/04 **********************************
     C                   ENDDO
      *
      *
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * SROUT - Write record to work file
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PPY01
      *
     C                   Z-ADD     ELCO#         PYCO#
     C                   Z-ADD     ELFND         PYFND
     C                   Z-ADD     ELFYR         PYFYR
     C                   Z-ADD     ELEMP#        PYEMP#
     C                   Z-ADD     ELDIV         PYDIV
     C                   Z-ADD     EPCLAS        PYCLAS
     C***  Begin Chg  ***  05/23/05  **************************************
     C**********           Z-ADDEPARAT    PYARAT
     C                   Z-ADD     XXARAT        PYARAT
     C*
     C                   Z-ADD     XXANRT        PYANRT
     C                   Z-ADD     X7ANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PYANDT
     C***  End   Chg  ***  05/23/05  **************************************
     C*
     C                   MOVE      EPSPEC        PYSPEC
     C***  Beg   Add  ***  04/20/09  **************************************
     C                   MOVE      EPEXT         PYEXT
     C***  End   Add  ***  04/20/09  **************************************
      *
      *  Total normal premium
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELANMP        PYTNPR
      *
     C                   ELSE
      *
     C                   Z-ADD     ELENMP        PYTNPR
     C                   ENDIF                                                  ELFAF
      *
      *  Manual Premium
      *
     C                   Z-ADD     XXMPRM        PYMPRM                         MANUAL PRM
      *
      *  Determine if self reporting or estimating employer
      *
     C                   MOVEL(P)  ELBSCD        KKKEY
     C     BSKY          CHAIN     PBS01                              60
      *
     C     BSSEFL        IFEQ      'S'
     C     *IN60         ANDEQ     *OFF
     C     @@SRPR        ANDEQ     'Y'
      *
      *  Get payroll
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     XXPAMT        PYPAMT
      *
     C                   ELSE
      *
     C                   EXSR      GETSRP
     C                   ENDIF
      *
      *  Use self reporting earned premium
      *
     C                   MOVE      ELCO#         ##CO#             3
     C                   MOVE      ELFND         ##FND             3
     C                   MOVE      ELEMP#        ##EMP#            9
     C                   MOVE      ELDIV         ##DIV             5
     C                   MOVE      ELFYR         ##FYR             3
     C                   MOVE      EPCLAS        ##CLAS            5
     C                   MOVE      EPSPEC        ##SPEC            1
     C                   MOVE      @@RNDT        ##RNDT            8
      *
     C                   CALL      'SRPRT3'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CLAS            5
     C                   PARM                    ##SPEC            1
     C******BEG ADD *****04/20/09*************************************
     C                   PARM                    ##EXT             5
     C******END ADD *****04/20/09*************************************
     C                   PARM                    ##RNDT            8
     C                   PARM                    ##AMT1           11            S/R YTD PYRL
     C                   PARM                    ##AMT2           11            S/R YTD PRM
     C                   PARM                    ##AMT3           11            S/R MTH PYRL
     C                   PARM                    ##AMT4           11            S/R MTH PRM
      *
     C                   MOVE      ##AMT2        PYEPRM
      *
     C                   ELSE
      *
      *  Use estimating employer payroll and premium
      *
     C                   Z-ADD     XXPAMT        PYPAMT
      *
     C                   EXSR      SBCPA
     C                   ENDIF                                                  BSSEFL
      *
      *  Get number of claims and total incurred for class
      *
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         DOWEQ     '0'
      *
     C     CMCLAS        IFEQ      EPCLAS
     C     CMSPEC        ANDEQ     EPSPEC
     C******BEG ADD *****04/20/09*************************************
     C     CMEXT         ANDEQ     EPEXT
     C******END ADD *****04/20/09*************************************
     C                   ADD       1             PYCLMS                         COUNT CLAIMS
     C                   Z-ADD     *ZERO         TOTPMT           11 2
     C                   Z-ADD     *ZERO         TOTRES           11 2
     C                   XFOOT     CMA           TOTPMT
     C                   XFOOT     CMR           TOTRES
     C                   ADD       TOTPMT        PYTOTI
     C                   ADD       TOTRES        PYTOTI
     C                   ENDIF                                                  CMCLAS
      *
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO                                                  *IN60 CMKY
      *
     C                   MOVEL     Q1PGM         PYPGM
     C                   MOVEL     Q1USER        PYUSER
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PYDT
      *
     C                   MOVEL     DSTIME        PYTIME
      *
      *  Write to output file
      *
     C                   WRITE     PPY01
     C                   ENDSR                                                  SROUT
      *
      *
      *
      *==================================================================*
      *   Calculate Earned Premium Amount  ** Subroutine **   S B C P A  *
      *==================================================================*
     CSR   SBCPA         BEGSR
      *
      *
     C     ELFAF         IFEQ      'A'
      *
     C     ELAMPR        IFNE      *ZEROS
     C     XXMPRM        DIV       ELAMPR        XXRPC             6 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRPC
     C                   ENDIF                                                  ELAMPR=0
      *
     C                   ELSE
      *
     C     ELMAPR        IFNE      *ZEROS
     C     XXMPRM        DIV       ELMAPR        XXRPC
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRPC
     C                   ENDIF                                                  ELMAPR=0
      *
     C                   ENDIF                                                  ELFAF=A
      *
      *  CALCULATE EARNED PREMIUM TO DATE
      *
     C                   MOVE      ELCO#         ##CO#             3
     C                   MOVE      ELFND         ##FND             3
     C                   MOVE      ELFYR         ##FYR             3
     C                   MOVE      ELEMP#        ##EMP#            9
     C                   MOVE      ELDIV         ##DIV             5
     C                   MOVE      @@RNDT        ##RNDT            8
      *
     C                   CALL      'SRPRT1'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##RNDT            8
     C                   PARM      *BLANKS       ##AMT            11
     C                   PARM      *BLANKS       ##AMT2           11
     C                   PARM      '1'           ##TERM            1
      *
     C                   MOVE      ##AMT         XXPRTD           11 2
      *
     C     XXPRTD        MULT      XXRPC         PYEPRM
      *
     C                   ENDSR                                                  END SBCPA
      *
      *
      *
      *================================================================
      * GETSRP - Get "Annualized" Self Reported Payroll
      *================================================================
     C     GETSRP        BEGSR
      *
     C                   Z-ADD     *ZERO         XXCNT            11 0
     C                   Z-ADD     *ZERO         XXAMT            15 2
      *
     C     SRKY          CHAIN     PSR01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   ADD       1             XXCNT
     C                   ADD       SRAMTP        XXAMT
     C     SRKY          READE     PSR01                                  60
     C                   ENDDO
      *
     C     XXCNT         IFNE      *ZERO
     C     XXAMT         DIV       XXCNT         XXTEMP           15 2
     C     XXTEMP        MULT      12            PYPAMT
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         PYPAMT
     C                   ENDIF                                                  XXCNT
      *
     C                   ENDSR                                                  GETSRP
      *
      *
      *
