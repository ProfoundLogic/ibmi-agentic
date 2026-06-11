     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('STBNDDIR')
     H/define CONVERT
     H/TITLE W7009R - RELEASE WCMC PENDED PAYMENTS BY DATE
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*      THIS PROGRAM WILL READ THE REQUEST FOR PAYMENT FILE      *
     H*      (WARPP) AND SELECT FEE SHEDULE PAYMENTS (EOB# = 0) THAT  *
     H*      ARE CURRENTLY PENDED AND RELEASE THE PAYMENTS BY DATE.   *
     H*      THE PAYMENTS ARE SUBJECT TO THE SAME RESERVE VALIDATION  *
     H*      THAT ALL RELEASED PAYMENTS INCUR. IF INADEQUATE RESERVES *
     H*      DO EXIST, THE PAYMENT WILL REMAIN ON A PEND STATUS AND A *
     H*      PRINT OUT OF THE ERROR WILL BE GENERATED.                *
     H*                                                               *
     H* CALLED FROM: W7009C                                           *
     H* CALLS TO:    NONE                                             *
     H* CREATED:     09/13/93      PROGRAMMER: 11      PROJECT: WC264 *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   SMORL   POR01: CO/PGM                              Override *
     H*   WARPL8  PRP01: CO/TRDT/STAT                 REQUEST PAYMENT *
     H*   WARPL   PRP02: CO/FND/FYR/CASE/SEQ#         REQUEST PAYMENT *
     H*   WMCML1  PCM01: CO/FND/FYR/CASE                 CLAIM MASTER *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   50   LOOK UP                                                *
     H*   60   GENERAL CHAINS & READS                                 *
     H*   65   READ REQUEST PAYMENT FILE FOR UPDATE                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/06/20 C:79  Support: 133/92674                             *
     H*                Rename RPEFTC                                  *
     H*                                                               *
     H* 06/20/19 C:62  Project: 152MAV                                *
     H*                Add Negative Fee Check Processing.             *
     H*                                                               *
     H* 07/19/18 C:180 Project: 146WHY                                *
     H*                Generate checks when they are from W8092R.     *
     H*                                                               *
     H* 04/12/16 C:309 Project: 114MXO                                *
     H*                Added check for Client# 114 for "Negative      *
     H*                fee adjust/ReConsiderations"                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 05/15/14  C:296  Support: 85570                               *
     H*                  Generate checks when it is from W8090R.      *
     H*                                                               *
     H*  02/09/10 C:062 Project WC139 69943                           *
     H*                 Do not allow release if it makes the group    *
     H*                 check negative.                               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/09/09 C:220 Project: 136MEP                                *
     H*                Negative fee adjust/ReConsiderations           *
     H*                                                               *
     H*  04/01/08 C:191 Project 139MIV                                *
     H*                 only allow negative payments to be released   *
     H*                 if the total claim paid is adequate to prevent*
     H*                 a negative total paid amount.                 *
     H*                                                               *
     H*  09/27/07 C:58 Project 132MGL                                 *
     H*                Added code to release pended payments created  *
     H*                by the UR/MC upload.                           *
     H*                                                               *
     H*  09/30/05 C:15 Support: WC126/49111  126MCI                   *
     H*                Added code to process pended payments created  *
     H*                by the Provima Upload.                         *
     H*                                                               *
     H*  12/02/04 C:15 Support: WC121/44840                           *
     H*                Add code to allow claims with all 10 reserve   *
     H*                buckets setup, to pass the reserve edits, if   *
     H*                reserves are adequate.                         *
     H*                                                               *
     H*  11/18/02 C:53 project: WC909                                 *
     H*                Add new fields to the rename section           *
     H*                                                               *
     H*  05/02/02 C:15 Support: WC065/32354                           *
     H*                Chgd reserve edit to only include pmts with    *
     H*                the same status code when accumulating all pmts*
     H*                for a particular EOMB.                         *
     H*                                                               *
     H*  08/28/00 C:15 Project: 65MAL                                 *
     H*                If EOMB pmts do not have the same reserve      *
     H*                code, one could be released while the other    *
     H*                stays on a pend status.  I chgd the edit to    *
     H*                check this situation.                          *
     H*                                                               *
     H*  04/04/00 C:15 Project: WC577                                 *
     H*                Added code to rename all WARPP fields to       *
     H*                prevent them from being changed inadvertantly. *
     H*                                                               *
     H*  02/25/00 C:15 Project: WC577                                 *
     H*                When a pmt is released, load the release maint *
     H*                fields in WARPP.                               *
     H*                                                               *
     H*  10/19/99 C:15 Support: WC044/19944                           *
     H*                Changed pgm to process all transactions with   *
     H*                a date less than or equal to the entered date. *
     H*                This is how it worked in V2.                   *
     H*                                                               *
     H*  06/21/99 C:15 Project: 48MBV                                 *
     H*                Fixed the reserve edits.                       *
     H*                                                               *
     H*  12/04/98 C:15 Project: 48MBV                                 *
     H*                To allow this pgm to be called from the Daily  *
     H*                batch process, the pgm had to be changed to    *
     H*                use RPSTAT when checking for pended payments.  *
     H*                                                               *
     H*  11/23/98 C:94 PROJ: 48MBV                                    *
     H*                1.  ADDED PARM                                 *
     H*                2.  IF QQGRP IS ZEROS THEN CHECK EACH GROUP    *
     H*                    TO SEE IF CHECK PROCESSING FLAG = 'Y'      *
     H*                    AND CHECK TO MAKE SURE GROUP HAS NOT       *
     H*                    EXPIRED PRIOR TO DATE ON PROMPT SCREEN     *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWARPL     UF   E           K DISK
     FWARPLE    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*

      //*** Begin Add *** 06/20/19 ****************************************
      /copy sPrModule

     D allowNegFees    s               n
     D*** End   Add *** 06/20/19 ****************************************
     D*** BEG CHG ****** 04/01/08 ************************************
     D****************    ER      1   3 35                 ERROR DESC
     D ER              S             35    DIM(4) CTDATA PERRCD(1)                ERROR DESC
     D*** END CHG ****** 04/01/08 ************************************
     D*** BEG ADD ****** 04/01/08 ************************************
     D*** END ADD ****** 04/01/08 ************************************
     D*** BEGIN ADD *** 08/28/00 **************************************
     D RC              S              2    DIM(10)
     D REM             S              9  2 DIM(10)
     D SVR             S              9  2 DIM(10)
     D*** END ADD ***** 08/28/00 **************************************
     D*** BEGIN ADD *** 02/09/10 **************************************
     D RMG             S              9  2 DIM(10)
     D*** END ADD *** 11/18/02 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                                RESERVE CODE
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
     D  RR                     1     90  2
     D                                     DIM(10)                                REM RESERVES
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
     D*** BEG ADD ****** 04/01/08 ************************************
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                                PAY AGAINST RESERV
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
     D*** END ADD ****** 04/01/08 ************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END   ADD *** 02/09/10 **************************************
     I*
     IPRP02
     I              RPCO#                       R2CO#
     I              RPFND                       R2FND
     I              RPFYR                       R2FYR
     I              RPCASE                      R2CASE
     I              RPSEQ#                      R2SEQ#
     I              RPTRDT                      R2TRDT
     I              RPFEIN                      R2FEIN
     I              RPMOD                       R2MOD
     I              RPAMT                       R2AMT
     I              RPBR2                       R2BR2
     I              RPBR7                       R2BR7
     I              RPNCCI                      R2NCCI
     I              RPFRDT                      R2FRDT
     I              RPTODT                      R2TODT
     I              RPWKS                       R2WKS
     I              RPDAYS                      R2DAYS
     I              RPTRCD                      R2TRCD
     I              RPTYPE                      R2TYPE
     I              RPRETP                      R2RETP
     I              RPRECD                      R2RECD
     I              RPRMDS                      R2RMDS
     I              RPRMK1                      R2RMK1
     I              RPRMK2                      R2RMK2
     I              RPRMK3                      R2RMK3
     I              RPDESC                      R2DESC
     I              RPCKCN                      R2CKCN
     I              RPPNAM                      R2PNAM
     I              RPADR1                      R2ADR1
     I              RPADR2                      R2ADR2
     I              RPCTY                       R2CTY
     I              RPST                        R2ST
     I              RPZIP                       R2ZIP
     I              RPSTAT                      R2STAT
     I              RPUSER                      R2USER
     I              RPPGM                       R2PGM
     I              RPTIME                      R2TIME
     I              RPDT                        R2DT
     I              RPBUL1                      R2BUL1
     I              RPCHK#                      R2CHK#
     I              RPEOB#                      R2EOB#
     I              RPPGTP                      R2PGTP
     I              RPRCVD                      R2RCVD
     I              RPDUE                       R2DUE
     I              RPSTS                       R2STS
     I              RPSORT                      R2SORT
     I              RPSENT                      R2SENT
     I              RPDSEQ                      R2DSEQ
     I*** BEGIN ADD *** 04/04/00 **************************************
     I              RPOFF#                      R2OFF#
     I              RPCKTP                      R2CKTP
     I              RPACCT                      R2ACCT
     I              RPRQST                      R2RQST
     I              RPLTCD                      R2LTCD
     I              RPNTWK                      R2NTWK
     I              RPEOBS                      R2EOBS
     I              RPFNAL                      R2FNAL
     I              RPIDT                       R2IDT
     I              RPITM                       R2ITM
     I              RPIPGM                      R2IPGM
     I              RPIUSR                      R2IUSR
     I              RPRDT                       R2RDT
     I              RPRTM                       R2RTM
     I              RPRPGM                      R2RPGM
     I              RPRUSR                      R2RUSR
     I*** END ADD ***** 04/04/00 **************************************
     I*** BEG ADD *** 11/18/02 **************************************
     I              RPCMSQ                      R2CMSQ
     I              RPCTRY                      R2CTRY
     I              RPADR3                      R2ADR3
     I*** Beg Add *** 11/06/20 **************************************
     I              RPEFTC                      R2EFTC
     I*** End Add *** 11/06/20 **************************************

     C*
     C********************************************************************
     C*  PROGRAM KEY LISTINGS
     C********************************************************************
     C*
     C*   KEY LIST FOR THE COMAMNY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  REQUEST PAYMENT FILE - PARTIAL
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     RPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C********************************************************************
     C*  *LIKE DEFINES
     C********************************************************************
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    RPTRDT        XXDT                           YYMMDD
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C*** BEGIN DLT *** 08/28/00 **************************************
     C********** *LIKE     DEFN RPAMT     XXREM
     C*** END DLT ***** 08/28/00 **************************************
     C     *LIKE         DEFINE    RPAMT         XXCUSH
     C*** BEGIN DLT *** 08/28/00 **************************************
     C*** BEGIN ADD *** 06/21/99 **************************************
     C********** *LIKE     DEFN RPAMT     SVREM
     C*** END ADD ***** 06/21/99 **************************************
     C*** END DLT ***** 08/28/00 **************************************
     C*** BEGIN ADD *** 08/28/00 **************************************
     C     *LIKE         DEFINE    RPRECD        XXRECD
     C*** END ADD ***** 08/28/00 **************************************
     C*
     C                   Z-ADD     *ZERO         X                 3 0
     C                   MOVE      *BLANKS       XXTITL
     C                   MOVE      *BLANK        XXERDS           35
     C                   MOVE      *BLANKS       XXPNAM           20            PAYEE NAME
     C********************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C********************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            7
     C                   PARM                    QQPCDE            2
     C                   PARM                    QQRCDE            2
     C                   PARM                    QQGRP             3
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQGRP         XXFND
     C                   MOVE      QQDATE        XXDT
     C*
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C     XXFND         COMP      *ZEROS                                 91
     C                   TIME                    DSDTTM
     C*
     C*  GET COMPANY INFORMATION
     C*
     C     COKY          CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** BEGIN ADD *** 09/27/07 **************************************
     C*   Get Client ID
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*** END ADD ***** 09/27/07 **************************************
     C*** Begin Chg *** 06/20/19 ****************************************
      /free
           // Check to see if negative fees are allowed
           allowNegFees = *Off;
           If IsModEnabled('Negative Fee Checks');
              allowNegFees = *On;
           EndIf;
      /end-free
     C*** End   Chg *** 06/20/19 ****************************************
     C*
     C*  GET THE DISTRIBUTION INFORMATION & PRINT IT.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INOA
     C                   END
     C                   EXCEPT    HEAD
     C*
     C*   START READING THE REQUEST PAYMENT FILE
     C*
     C   91RPKY          CHAIN     PRP01                              65
     C  N91RPKY1         CHAIN     PRP01                              65
     C*
     C*   IF NO RECORD EXIST THEN PRINT ERROR MSG
     C*
     C     *IN65         IFEQ      '1'
     C                   EXCEPT    NORCDS
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN65         DOWEQ     '0'
     C*
     C*   VERIFY THAT THE RECORD IS A PENDED FSMS RECORD FOR THE
     C*    DATE KEYED ON THE PROMPT SCREEN.
     C*
     C     RPEOB#        IFEQ      0
     C*** Begin add *** 09/30/05 **************************************
     C     RPIPGM        ANDNE     'F8042R'
     C*** End add ***** 09/30/05 **************************************
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     RPIPGM        ANDNE     'W8321R'
     C*** END ADD ***** 09/27/07 **************************************
     C*** BEGIN ADD *** 05/15/14 **************************************
     C     RPIPGM        ANDNE     'W8090R'
     C*** END ADD ***** 05/15/14 **************************************
     C*** BEGIN ADD *** 07/19/18 **************************************
     C     RPIPGM        ANDNE     'W8092R'
     C*** END ADD ***** 07/19/18 **************************************
     C     RPSTAT        ORNE      'P'
     C                   GOTO      NEXT
     C                   END
     C*
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     ##CLT#        IFEQ      '132'
     C     RPAMT         ANDGE     250
     C                   GOTO      NEXT
     C                   END
     C*** END ADD ***** 09/27/07 **************************************
     C*
     C     XXDT          IFNE      0
     C*** BEGIN CHG *** 10/19/99 **************************************
     C********** RPTRDT    ANDNEXXDT
     C     RPTRDT        ANDGT     XXDT
     C*** END CHG ***** 10/19/99 **************************************
     C                   GOTO      NEXT
     C                   END
     C*
     C     QQPCDE        IFNE      *BLANKS
     C     RPSTS         ANDNE     QQPCDE
     C                   GOTO      NEXT
     C                   END
     C*
     C*  Determine if payments are allowed on the group.
     C*
     C                   CALL      'WA011R'
     C                   PARM                    RPCO#
     C                   PARM                    RPFND
     C                   PARM                    RPFYR
     C                   PARM                    RPCASE
     C                   PARM                    @@RC              2
     C                   PARM      'N'           @@03              1
     C                   PARM      'N'           @@05              1
     C                   PARM      'Y'           @@09              1
     C*
     C     @@RC          IFEQ      '01'
     C     @@RC          OREQ      '02'
     C     @@RC          OREQ      '03'
     C     @@RC          OREQ      '04'
     C     @@RC          OREQ      '05'
     C                   MOVEL(P)  ER(1)         XXERDS
     C                   EXSR      SRPRT
     C                   MOVE      RPFND         XXFND
     C     RPKY1         SETGT     PRP01
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*   CHAIN OUT TO THE CLAIM MASTER FILE TO GET CLAIM STATUS AND
     C*   REMAINING RESERVE AMOUNTS.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL(P)  ER(2)         XXERDS
     C                   EXSR      SRPRT
     C                   GOTO      NEXT
     C                   END
     C*
     C*  Process the reserve edits.
     C*
     C                   EXSR      SREDT
     C*
     C*  Release the payment
     C*
     C                   EXSR      SRUPD
     C*
     C     NEXT          TAG                                                    RPEOB# NE 0
     C*
     C   91RPKY          READE     PRP01                                  65
     C  N91RPKY1         READE     PRP01                                  65
     C                   ENDDO
     C*
     C*  End of program
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*================================================================
     C*  SREDT - This subroutines determines if the payment can be
     C*          released.
     C*================================================================
     C     SREDT         BEGSR
     C*
     C*  Accumulate all payments for this EOMB.
     C*
     C*** BEGIN CHG *** 08/28/00 **************************************
     C******************** Z-ADD0         XXREM
     C                   CLEAR                   RC
     C                   CLEAR                   REM
     C*** END CHG ***** 08/28/00 **************************************
     C*** BEG ADD ***** 02/09/10 **************************************
     C                   CLEAR                   RMG
     C*** END ADD ***** 02/09/10 **************************************
     C*
     C*** Begin add *** 09/30/05 **************************************
     C     RPEOB#        IFEQ      *ZEROS
     C*
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    RC(X)                                  60
     C     *IN60         IFEQ      *ON
     C                   ADD       RPAMT         REM(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  60
     C                   MOVEL     RPRECD        RC(X)
     C                   ADD       RPAMT         REM(X)
     C                   ENDIF
     C*
     C                   ELSE
     C*** End add ***** 09/30/05 **************************************
     C*
     C     RPKY2         CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
      *
     C     R2STAT        IFEQ      'P'
     C*** Begin add *** 05/02/02 **************************************
     C     R2STS         ANDEQ     QQPCDE
     C*** End add ***** 05/02/02 **************************************
     C*** BEGIN DLT *** 08/28/00 **************************************
     C********** R2RECD    ANDEQRPRECD
     C**********           ADD  R2AMT     XXREM
     C*** END DLT ***** 08/28/00 **************************************
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   Z-ADD     1             X
     C     R2RECD        LOOKUP    RC(X)                                  60
     C     *IN60         IFEQ      *ON
     C                   ADD       R2AMT         REM(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  60
     C                   MOVEL     R2RECD        RC(X)
     C                   ADD       R2AMT         REM(X)
     C                   ENDIF
     C*
     C*** END ADD ***** 08/28/00 **************************************
     C                   ENDIF
      *
     C     RPKY2         READE     PRP02                                  60
     C                   ENDDO
     C*
     C*** Begin add *** 09/30/05 **************************************
     C                   ENDIF
     C*** End add ***** 09/30/05 **************************************
     C*
     C*** BEGIN ADD *** 06/21/99 **************************************
     C*  Save the amount for the pended EOMB requests.
     C*
     C*** BEGIN CHG *** 08/28/00 **************************************
     C*******************  Z-ADDXXREM     SVREM
     C                   MOVEA     REM           SVR
     C*** END CHG ***** 08/28/00 **************************************
     C*
     C*  Accumulate all released payments for this claim.
     C*
     C     CMKY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     R2STAT        IFEQ      ' '
     C*** BEGIN DLT *** 08/28/00 **************************************
     C********** R2RECD    ANDEQRPRECD
     C**********           ADD  R2AMT     XXREM
     C*** END DLT ***** 08/28/00 **************************************
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   Z-ADD     1             X
     C     R2RECD        LOOKUP    RC(X)                                  60
     C     *IN60         IFEQ      *ON
     C                   ADD       R2AMT         REM(X)
     C*** BEG ADD ***** 02/09/10 **************************************
     C     RPAMT         IFLT      0
     C     RPPGTP        ANDEQ     'G'
     C     R2FEIN        ANDEQ     RPFEIN
     C     R2MOD         ANDEQ     RPMOD
     C     R2PGTP        ANDEQ     RPPGTP
     C                   ADD       R2AMT         RMG(X)
     C                   ENDIF
     C*** END ADD ***** 02/09/10 **************************************
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  60
     C                   MOVEL     R2RECD        RC(X)
     C                   ADD       R2AMT         REM(X)
     C*** BEG ADD ***** 02/09/10 **************************************
     C     RPAMT         IFLT      0
     C     RPPGTP        ANDEQ     'G'
     C     R2FEIN        ANDEQ     RPFEIN
     C     R2MOD         ANDEQ     RPMOD
     C     R2PGTP        ANDEQ     RPPGTP
     C                   ADD       R2AMT         RMG(X)
     C                   ENDIF
     C*** END ADD ***** 02/09/10 **************************************
     C                   ENDIF
     C*** END ADD ***** 08/28/00 **************************************
     C                   ENDIF
     C*
     C     CMKY          READE     PRP02                                  60
     C                   ENDDO
     C*** END ADD ***** 06/21/99 **************************************
     C*** BEG ADD ***** 02/09/10 **************************************
     C*
     C*  Accumulate all pended payments for this claim.
     C*
     C     RPAMT         IFLT      0
     C     RPPGTP        ANDEQ     'G'
     C     CMKY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     R2STAT        IFEQ      'P'
     C     R2FEIN        ANDEQ     RPFEIN
     C     R2MOD         ANDEQ     RPMOD
     C     R2PGTP        ANDEQ     RPPGTP
     C                   ADD       R2AMT         RMG(X)
     C                   ENDIF
     C*
     C     CMKY          READE     PRP02                                  60
     C                   ENDDO
     C                   ENDIF
     C*** END ADD ***** 02/09/10 **************************************
     C*
     C*  If the Claim is Open, the amount can not be greater than
     C*     than Remaining Reserve
     C*
     C     CMSTAT        IFNE      'C'
     C*
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   DO        10            Y                 3 0
     C*** END ADD ***** 08/28/00 **************************************
     C*** Begin add *** 12/02/04 **************************************
     C     RC(Y)         IFEQ      *BLANKS
     C     REM(Y)        ANDEQ     0
     C                   LEAVE
     C                   ENDIF
     C*** End add ***** 12/02/04 **************************************
     C*
     C                   Z-ADD     1             X                 3 0
     C*** BEGIN DLT *** 08/28/00 **************************************
     C********** RPRECD    LOKUPRT,X                     60
     C********** *IN60     IFEQ *OFF
     C********** XXREM     ORGT RR,X
     C*** END DLT ***** 08/28/00 **************************************
     C*** BEGIN ADD *** 08/28/00 **************************************
     C     RC(Y)         LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      *OFF
     C     REM(Y)        ORGT      RR(X)
     C*** END ADD ***** 08/28/00 **************************************
     C                   MOVEL(P)  ER(3)         XXERDS
     C                   EXSR      SRPRT
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   ENDDO
     C*** END ADD ***** 08/28/00 **************************************
     C*
     C                   ENDIF
     C*
     C*  If the claim is closed, process the reserve cushion edit.
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   DO        10            Y
     C*** END ADD ***** 08/28/00 **************************************
     C*** Begin add *** 12/02/04 **************************************
     C     RC(Y)         IFEQ      *BLANKS
     C     SVR(Y)        ANDEQ     0
     C                   LEAVE
     C                   ENDIF
     C*** End add ***** 12/02/04 **************************************
     C*
     C                   CALL      'SRCUSH'
     C                   PARM                    RPCO#
     C                   PARM                    RPFND
     C                   PARM                    RPFYR
     C                   PARM                    RPCASE
     C*** BEGIN CHG *** 08/28/00 **************************************
     C*******************  PARM           RPRECD
     C                   PARM      RC(Y)         XXRECD
     C*** END CHG ***** 08/28/00 **************************************
     C                   PARM                    CMLTFL
     C                   PARM                    XXCUSH
     C*
     C*** BEGIN CHG *** 06/21/99 **************************************
     C********** XXREM     IFGT XXCUSH
     C*** BEGIN DLT *** 08/28/00 **************************************
     C********** SVREM     IFGT XXCUSH
     C*** END DLT ***** 08/28/00 **************************************
     C*** END CHG ***** 06/21/99 **************************************
     C*** BEGIN ADD *** 08/28/00 **************************************
     C     SVR(Y)        IFGT      XXCUSH
     C*** END ADD ***** 08/28/00 **************************************
     C                   MOVEL(P)  ER(3)         XXERDS
     C                   EXSR      SRPRT
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/28/00 **************************************
     C                   ENDDO
     C*** END ADD ***** 08/28/00 **************************************
     C*
     C                   ENDIF                                                  CMSTAT = 'C'
     C*
     C*** BEG ADD ****** 04/01/08 ************************************
     C* Check if the value in REM for the reserve
     C* will produce a negative total amount paid.
     C* If so, print the exception on the report and do not release the payment.
     C     ##CLT#        IFEQ      '139'
     C*** BEG CHG ***** 02/09/10 **************************************
     C     RPAMT         ANDLT     0
     C     RPPGTP        ANDEQ     'G'
     C*** END CHG ***** 02/09/10 **************************************
     C*** BEG ADD ****** 11/09/09 ************************************
     C*** Begin Chg *** 06/20/19 ****************************************
     C*****##CLT#        OREQ      '136'
     C     allowNegFees  ANDEQ     *On
     C     RPAMT         ANDLT     0
     C     RPPGTP        ANDEQ     'G'
     C*** End   Chg *** 06/20/19 ****************************************
     C*** END ADD ****** 11/09/09 ************************************
     C*** Beg Add ****** 04/12/16 ************************************
     C     ##CLT#        OREQ      '114'
     C*** End Add ****** 04/12/16 ************************************
     C                   DO        10            Y                 3 0
     C*
     C     RC(Y)         IFEQ      *BLANKS
     C*** BEG CHG ***** 02/09/10 **************************************
     C********   REM,Y     ANDEQ0
     C     RMG(Y)        ANDEQ     0
     C*** END CHG ***** 02/09/10 **************************************
     C                   LEAVE
     C                   ENDIF                                                  RC,Y = ' '
     C*
     C*** BEG CHG ***** 02/09/10 **************************************
     C********** REM,Y     IFLT 0
     C     RMG(Y)        IFLT      0
     C*** END CHG ***** 02/09/10 **************************************
     C*
     C*** BEG DEL ***** 02/09/10 **************************************
     C**********           Z-ADD1         X
     C********** RC,Y      LOKUPRT,X                     60
     C********** *IN60     IFEQ *ON
     C********** AR,X      ADD  RPAMT     XXTEMP  92
     C********** XXTEMP    IFLE 0
     C*** END DEL ***** 02/09/10 **************************************
     C                   MOVEL(P)  ER(4)         XXERDS
     C                   EXSR      SRPRT
     C                   GOTO      NEXT
     C*** BEG DEL ***** 02/09/10 **************************************
     C**********           ENDIF                           XXTEMP<0
     C*
     C**********           ENDIF                           *IN60 = *ON
     C*** END DEL ***** 02/09/10 **************************************
     C                   ENDIF                                                  REM,Y < 0
     C                   ENDDO                                                  Y = 1 to 10
     C                   ENDIF                                                  ##CLT#='139'
     C*** END ADD ****** 04/01/08 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C*  LOAD REPORT VALUES AND PRINT THE ERROR IF ONE OCCURRED.
     C*================================================================
     C     SRPRT         BEGSR
     C*
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   MOVEL     RPPNAM        XXPNAM
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ENDSR
     C*================================================================
     C*  Release the payment and complete any DARTS diaries.
     C*================================================================
     C     SRUPD         BEGSR
     C*
     C*  Release the pmt
     C*
     C     QQRCDE        IFNE      *BLANKS
     C                   MOVE      QQRCDE        RPSTS
     C                   ELSE
     C                   MOVE      'PY'          RPSTS
     C                   END
     C                   MOVE      ' '           RPSTAT
     C*** BEGIN ADD *** 02/25/00 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     Q1USER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   Z-ADD     RPRDT         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   MOVEL     Q1USER        RPUSER
     C*** END ADD ***** 02/25/00 **************************************
     C                   MOVE      Q1PGM         RPPGM
     C                   UPDATE    PRP01                                        UPDATE REQUEST PMT
     C*
     C*  If the claim is closed, increase reserves in claim master and
     C*    reserve history.
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'R'           QQTYPE                         "Allocate" Res
     C                   CALL      'SRPRCC'
     C                   PARM                    RPCO#
     C                   PARM                    RPFND
     C                   PARM                    RPFYR
     C                   PARM                    RPCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C                   ENDIF                                                  CMSTAT = 'C'
     C*
     C** COMPLETE DARTS DIARY REMARK
     C*
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     RPFND         QCFND
     C                   Z-ADD     RPFYR         QCFYR
     C                   Z-ADD     RPCASE        QCCASE
     C*
     C                   CALL      'WA018R'
     C                   PARM                    QCKEY
     C                   PARM                    RPDSEQ
     C                   PARM                    ##RTCD            1
     C*
     C                   ENDSR
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           20 'CO#:'
     O                       XXCO#               25
     O          E            ROUTE          2
     O                                           20 'DATE:'
     O                       XXDATE              30 '  /  /    '
     O**** BEG ADD ***** 03/10/95 *************************************
     O          E            ROUTE          2
     O                                           20 'PEND PMT STS CDE:'
     O                       QQPCDE              24
     O          E            ROUTE          2
     O                                           20 'RLS PMT STS CODE:'
     O                       QQRCDE              24
     O**** END ADD ***** 03/10/95 *************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1998'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       DSDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              94
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           1
     O                                           32 'RESV'
     O                                           85 'CLAIM'
     O          E            HEAD           2
     O*** BEG CHG *** 11/23/98 ***************************************
     O                                           03 'GRP'
     O                                           07 'FYR'
     O                                           15 'CASE'
     O**********                         14 'CLAIM NUMBER'
     O*** END CHG *** 11/23/98 ***************************************
     O                                           26 'TRANS DT'
     O                                           32 'CODE'
     O                                           44 'PAYEE NAME'
     O                                           64 'EOMB #'
     O                                           77 'AMOUNT'
     O                                           86 'STATUS'
     O                                          107 'ERROR DESCRIPTION'
     O          E            DET            1
     O                       RPFND         Z     03
     O                       RPFYR         Z     07
     O                       RPCASE        Z     15
     O                       XXTRDT              26 '  /  /    '
     O                       RPRECD              31
     O                       XXPNAM              54
     O                       RPEOB#        Z     65
     O*** BEGIN CHG *** 02/09/10 **************************************
     O**********               RPAMT J   79
     O                       RPAMT         J     79
     O*** END   CHG *** 02/09/10 **************************************
     O                       CMSTAT              83
     O                       XXERDS             125
     O*
     O          E            NORCDS      1  2
     O                                           16 '*** NO RECORDS '
     O                                           31 'WERE PROCESSED '
     O                                           34 '***'
     O********************************************************************
** ERROR DESCRIPTIONS
GROUP NOT ELIGIBLE FOR PAYMENTS
CLAIM NOT FOUND
INADEQUATE RESERVES ON CLAIM
PAYMENT WILL PRODUCE -VE TOTAL AMT
