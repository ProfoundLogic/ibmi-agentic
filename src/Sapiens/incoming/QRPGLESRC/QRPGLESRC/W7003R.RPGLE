     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE W7003R - DAILY CHECK PRINTING DRIVER
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 04/13/21 C:062 Project: WC2749
     H*                Prevent duplicate checks.
     H*
     H* 10/21/18 C:136 Project: WC2337
     H*                Do not process checks if the check schedule
     H*                record is flagged for EFT only
     H*
     H* 02/25/14 C:136 Support: RED3966
     H*                Allow pgm to update Check Print Flag (CPsta0)
     H*                for EFT payments so that W7000R can write
     H*                payments
     H*
     H*  01/22/13 C:287 Project: SY266_QRY                            *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 01/14/13 C:53  SUPPORT: WC114 81470
     H*                Correct character field trying to be converted
     H*                to an interger
     H*                                                               *
     H* 04/17/12 C:282 PROJECT WC2044                                 *
     H*                Replacing WARPLR to WARPL12 with EFT flag      *
     H*                                                               *
     H* 05/13/10 C:61  PROJECT C136WFD                                *
     H*                Load Maintenance fields with update.           *
     H*                                                               *
     H* 11/19/01 C:15 Project: WC1020                                 *
     H*               Chgd pgm back to use WACPL instead of WACPL4.   *
     H*                                                               *
     H* 05/01/01 C:15 Project: WC1020                                 *
     H*               Added code to read the check schedule records   *
     H*               by CO#/PRDT/ACCT/CKTP/FND.  Then, process the   *
     H*               print pgm for each CKTP.                        *
     H*                                                               *
     H* 09/08/94 C:15 PROJECT: WC361                                  *
     H*               ADDED TYPE PARM TO DIFFERENTIATE BETWEEN GROUP  *
     H*               CHECKS AND NORMAL CHECKS.                       *
     H*****************************************************************
     F*** Begin chg *** 05/01/01 **************************************
     F******WACPL   UF  E           K        DISK
     F*** Begin dlt *** 11/19/01 **************************************
     F******WACPL4  UF  E           K        DISK
     F*** End dlt ***** 11/19/01 **************************************
     F*** End chg ***** 05/01/01 **************************************
     F*** Begin add *** 11/19/01 **************************************
     FWACPL     UF   E           K DISK
     F*** End add ***** 11/19/01 **************************************
     FSMCOL     IF   E           K DISK
      *___________________________________________________________________
      *  Request for Payment file
     F*** Begin chg *** 05/01/01 **************************************
     F*******WARPL6  IF  E           K        DISK
     F*** Beg chg *** 04/17/12 **************************************
     F*****WARPLR    IF   E           K DISK
     FWARPL12   IF   E           K DISK
     F*** End chg *** 04/17/12 **************************************
      *___________________________________________________________________
      *  Bank Information file
     F*** Begin chg *** 05/01/01 **************************************
     F*******WMBIL1  IF  E           K        DISK
     FWMBIL2    IF   E           K DISK
     F*** End chg ***** 05/01/01 **************************************
      *___________________________________________________________________
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d ##co#           s              3
     d ##fnd           s              3
     d ##appl          s              2
     d ##srdt          s              8
     d ##call          s              1
     d ##hdcy          s              1
     d ##chkq          s             10
     d ##ftyp          s             10
     d ##efdt          s              7
     d ##cpyq          s             10
     D ##cktp          s             10
     d ##eftc          s              1
     d @@coid          s             20
     d ##rtcd          s              1
     d ##acct          s             20
     d xxeftc          s              1
     d*** Beg Del *** 01/14/13 ********************************
     d*************** ##acct#         s             20  0
     d*** End Del *** 01/14/13 ********************************
      *******End Add********04/17/12**********************************
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT              7 0
     C*
     C*   Request for Payment file
     C*
     C     RPKY          KLIST
     C                   KFLD                    CPCO#
     C*** Begin add *** 05/01/01 **************************************
     C*** Beg Del *** 04/17/12 **************************************
     C*****              KFLD                    CPCKTP
     C*** End Del *** 04/17/12 **************************************
     C*** End add ***** 05/01/01 **************************************
     C                   KFLD                    XXFND
     C*** Beg Add *** 04/17/12 **************************************
     C                   KFLD                    CPCKTP
     C                   KFLD                    XXEFTC
     C*** End Add *** 04/17/12 **************************************
     C*
     C*   Bank Information file
     C*
     C     BIKY          KLIST
     C                   KFLD                    CPCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C*** Begin chg *** 05/01/01 **************************************
     C*******************  KFLD           CPACCT
     C                   KFLD                    CPCKTP
     C*** End chg ***** 05/01/01 **************************************
     C                   KFLD                    XXEFDT
     C*
     C     BIKY1         KLIST
     C                   KFLD                    CPCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C*** Begin chg *** 05/01/01 **************************************
     C*******************  KFLD           CPACCT
     C                   KFLD                    CPCKTP
     C*** End chg ***** 05/01/01 **************************************
     C*
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C*___________________________________________________________________
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTYPE            1
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   START READING THE CHECK PROCESSING FILE
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   MAKE SURE PROCESSING FLAGS AREN'T FLIPPED
     C*
     C*** Beg Add *** 04/17/12 **************************************
     C*****CPSTA0        IFEQ      ' '
     C*****CPSTA1        ANDEQ     ' '
      /free
       //*** beg chg ***** 02/25/14 **********************************
       //*** If CPSTA0 = ' ' and CPSTA1 = ' ' and CPEFTC <> 'E';
       If CPSTA0 = ' ' and CPSTA1 = ' ';
       //*** end chg ***** 02/25/14 **********************************
      /end-free
     C*** End Add *** 04/17/12 **************************************
     C*
     C*  Determine if a payment exists for the group.
     C*
     C*** Add Chg *** 04/17/12 **************************************
      /free
       XXeftc = *blanks;
      /end-free
     C*** End Chg *** 04/17/12 **************************************
     C                   Z-ADD     CPFND         XXFND                          BASE LOGIC
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  For each group with a payment, access Bank Information record
     C*  and call check processing CLP.
     C*
     C                   MOVE      'WC'          XXAPPL
     C                   Z-ADD     XXDT          XXEFDT
     C*
     C*** Beg Chg *** 04/17/12 **************************************
     C*****BIKY          SETLL     PBI01
     C*****BIKY1         READE     PBI01                                  60
     C*
     C******IN60         DOWEQ     *OFF
     C*****BISTAT        ANDEQ     'I'
     C*****BIKY1         READE     PBI01                                  60
     C*****              ENDDO
     C*
     C******IN60         IFEQ      *OFF
     C*****BIHDCY        ANDEQ     'Y'
     C*****              MOVEL     BIFTYP        QQFTYP
     C*****              MOVEL     BIOUTQ        QQOUTQ
     C*****              MOVEL     BICPYQ        QQCPYQ
      /Free
        ##co#  = %editc(CPco#:'X');
        ##fnd  = %editc(xxfnd:'X');
        ##appl = XXappl;
        ##srdt = %editc(XXdate:'X');
        ##call = *Blanks;
        ##hdcy = *blanks;
        ##chkq = *Blanks;
        ##ftyp = *Blanks;
        ##efdt = *Zeros ;
        ##cpyq = *Zeros ;
        ##cktp = CPckTp;
        ##eftc = *Blanks;
        @@coid = *Blanks;
        ##rtcd = *Blanks;
        ##acct = *Blanks;

        ##acct = GetBankAcc(##co#: ##fnd: ##appl: ##srdt: ##call:
                 ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                 ##cktp: ##eftc: @@coid: ##rtcd);

         //*** Beg Del *** 01/14/13 ********************************
            //***** ##acct# = %int(%Trim(##acct));*******************
         //*** End Del *** 01/14/13 *********************************

          //*** Beg Chg *** 01/14/13 *********************************
          // ****** If ##acct# <> 0 and ##hdcy = 'Y';
        If (##acct <> *blank And ##acct <> *Zeros) and ##hdcy = 'Y';
          //*** End Chg *** 01/14/13 *********************************
           QQftyp = ##FTYP;
           QQoutq = ##chkq;
           QQcpyq = ##cpyq;
        EndIf;

      /End-Free
     C*** End Chg *** 04/17/12 **************************************
     C*
     C*   CALL THE CL TO APPLY THE OVERRIDE
     C*
      *** beg add **** 10/21/15 *************************************
     c*** Begin Chg *** 04/13/21 *******************************************
     c**********         If        CPeftc <> 'E'
     c                   If        CPeftc <> 'E' And CPeftc <> 'V'
     c*** End   Chg *** 04/13/21 *******************************************
      *** end add **** 10/21/15 *************************************
     C                   MOVE      RPFND         QQFND
     C                   MOVE      CPACCT        QQACCT
     C*
     C                   CALL      'W7002C'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE
     C                   PARM                    QQFND             3
     C                   PARM                    QQTYPE
     C                   PARM                    QQACCT           20
     C                   PARM                    QQFTYP           10
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQCPYQ           10
     C*** Begin add *** 05/01/01 **************************************
     C                   PARM      CPCKTP        QQCKTP           10
     C*** End add ***** 05/01/01 **************************************
     C*
      *** beg add **** 10/21/15 *************************************
     c                   Endif
      *** beg add **** 10/21/15 *************************************
     C*** Beg Del ***** 05/01/01 **************************************
     C*****              ENDIF                                                  *IN60, BIHDCY
     C*** End Del ***** 05/01/01 **************************************
     C*
     C                   ENDIF
     C*
     C                   MOVE      'Y'           CPSTA0
      ***** BEG ADD ******* 05/13/10 **********************************
     C                   MOVE      Q1USER        CPUSER
     C                   MOVE      Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C*
     C                   MOVE      DSTIME        CPTIME
      ***** END ADD ******* 05/13/10 **********************************
     C                   UPDATE    PCP01
      ***** Beg Del ******* 05/13/10 **********************************
     C*****              END
      /free
       endif;
      /end-free
      ***** End Del ******* 05/13/10 **********************************
     C*
     C     CPKEY         READE     PCP01                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
