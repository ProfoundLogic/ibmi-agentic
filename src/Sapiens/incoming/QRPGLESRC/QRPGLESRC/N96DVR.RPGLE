     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE N96DVR - Network Discount Type DV Processing Program
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO PROCESS PHYSICIAN BILLS - CAP *
     H* PAYMENTS FOR PHYSICAL THERAPY AND RETAIN FEE SCHEDULE REDUCT- *
     H* IONS ALONG WITH NETWORK REDUCTIONS.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  CALLs To:                                                    *
     H*    FA783R and FA784R                                          *
     H*****************************************************************
     H* Created on 05/30/2013 by C:277 for Project 147MAS             *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/23 C: 342 Project: NY060                               *
     H*                  NY Compliance: CARC/RARC Update              *
     H*                                                               *
     H*  12/04/15 C: 079 Support: 114/38325                           *
     H*                  Pay FS minus percent not percent of FS       *
     H*                  Pay up to total per day correctly            *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALH    UF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFAEOL     IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** Beg Add *** 12/04/15 *****************************************
     FNDCOL6    IF   E           K DISK
     F*** End Add *** 12/04/15 *****************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D*
     D ##rtcd          s              1    inz(' ')
     D @@DSC1          s             70
     D @@DSC2          s             70
     D @@DSC3          s             70
     D @@DSC4          s             70
     D @@TEXT          s             70
     D @@st            s              2
     D XXDSEQ          s              5  0
     D*** Beg Add *** 12/04/15 *****************************************
     D fsPaid          s              9  2
     D decAllow        s              9  2
     D isSpecific      s              1n
     D kkCode          s              3A
     D kkEfDt          s              7P 0
     D kkFrCd          s             10A
     D kkToCd          s             10A
     D*** End Add *** 12/04/15 *****************************************
     C*
     C*  CASE DETAIL RECORDS
     C*
     C     CAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXFDAT
     C*
     C     CAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    EOFRDT
     C*
     C     BDKY2         KLIST
     C                   KFLD                    QQCODE
     C*
     C*          PEO01 - EOMB Transaction File
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     EOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTRDT
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C*          PDH01 - EOMB Phys/HCFA Detail Transaction File
     C*          PDU01 - EOMB Hosp/UB Detail Transaction File
     C*
     C     DHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     DUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    XXDSEQ
     C*
     C*   KEY LIST FOR PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KY1         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXNWCD            5
     C                   KFLD                    CAFDAT
     C*
     C     S4KY2         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXNWCD
     C*** Beg Add *** 12/04/15 *********************************************
     C*
     C     COKY1         KLIST
     C                   KFLD                    kkCode
     C                   KFLD                    kkEfDt
     C                   KFLD                    kkFrCd
     C*
     C     COKY2         KLIST
     C                   KFLD                    kkCode
     C                   KFLD                    kkEfDt
     C*** End Add *** 12/04/15 *********************************************
     C*
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C     *LIKE         DEFINE    BDDT          XXFDT
     C     *LIKE         DEFINE    BDDT          SAVDAT
     C     *LIKE         DEFINE    CAMRAA        ##ALOW
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    CARCN         XXRCN
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DEEOBCD       XXEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C     *LIKE         DEFINE    CAPAID        X1PAID
     C     *LIKE         DEFINE    CAFDAT        @@EFDT
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         @@EOB
     C     *LIKE         DEFINE    DEEOBCD       @@EOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    EOEOB#        @@EOB#
     C     *LIKE         DEFINE    DHMOD1        @@MOD
     C     *LIKE         DEFINE    DHPRCD        @@CODE
     C     *LIKE         DEFINE    EOTRDT        XXTRDT
     C     *LIKE         DEFINE    CAPAID        XXTOTA
     C     *LIKE         DEFINE    CAPAID        XXTOTP
     C     *LIKE         DEFINE    CACPT4        XXCPT4
     C     *LIKE         DEFINE    EOEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFDAT        X2FDAT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQCODE            3
     C                   PARM                    QQCEDT            8
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQEOB#            9 0
     C                   PARM                    QQRCN             3 0
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
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVEL     QQDS1         XXNWCD            5
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C                   MOVE      *ZEROS        SAVDAT
     C                   EVAL      XXDSEQ = %Dec(QQDSEQ:5:0)
     C                   MOVE      *ON           FIRST             1
     C*
     C***  DO NOT PROCESS IF ADDITIONAL PARM NOT RECEIVED
     C*
     C     Q1PRMS        IFLT      24
     C                   EXSR      SREXIT
     C                   ENDIF
     C*
     C* CHECK EOBTYP, IF NOT 'N' or 'H' THEN QUIT PROGRAM
     C*
     C                   MOVE      QQEOB#        XXEOB#            9 0
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFNE      '0'
     C     EOBTYP        ORNE      'N'
     C     EOBTYP        ANDNE     'H'
     C                   EXSR      SREXIT
     C                   ELSE
     C                   Z-ADD     EORCN         XXRCN
     C                   ENDIF
     C*
     C*** CHECK THE ALGORITHM USED, IF NOT DN THEN QUIT PROGRAM
     C*
     C     BDKY1         SETGT     PBD01
     C     BDKY2         READPE    PBD01                                  63
     C     *IN63         IFEQ      '0'
     C     *IN63         DOWEQ     '0'
     C*
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ENDIF
     C*
     C     BDKY2         READPE    PBD01                                  63
     C                   ENDDO
     C*
     C     BDTYPE        IFNE      'DV'
     C                   EXSR      SREXIT
     C                   ENDIF
     C                   ELSE
     C                   EXSR      SREXIT
     C                   ENDIF
     C*
     C* GET THE CASE DETAILS
     C*
     C     CAKY2         CHAIN     PCA01                              61
     C     *IN61         DOWEQ     '0'                                          LOOP TO RETRIEVE REC
     C                   MOVE      CADSEQ        XXDSEQ
     C*
     C     FIRST         IFEQ      *ON
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   MOVE      *OFF          FIRST
     C                   ENDIF
     C*
     C* EXECUTE ERROR CHECK
     C*
     C                   EXSR      SRERCK
     C     ERRIND        IFEQ      *OFF
     C*
     C*** PROCESSING FOR THE SAME DATE
     C     XXFDAT        IFEQ      CAFDAT
     C                   MOVE      '0'           LSVLDR            1
     C                   CLEAR                   DAYS
     C*
     C*  RETRIEVE TOTAL NO. OF DAYS FROM PHYSICIAN FILE
     C                   SELECT
     C                   WHEN      EOBTYP = 'N'
     C                   EXSR      SRDHN
     C                   EXSR      SRAMT
     C*
     C                   WHEN      EOBTYP = 'H'
     C                   EXSR      SRDUN
     C                   EXSR      SRAMT
     C*
     C                   ENDSL
     C*
     C                   MOVE      '1'           LSVLDR
     C                   MOVE      CAFDAT        XXFDT
     C                   ENDIF
     C*
     C* RETRIEVE PT CAP AMOUNT FROM DISCOUNT FILE
     C*
     C     BDAM01        IFNE      *ZEROS
     C                   MOVE      BDAM01        NETAMT            9 2
     C                   ELSE
     C                   MOVE      9999999.99    NETAMT
     C                   ENDIF
     C*
     C* IF DIFFERENT DATE IS ENCOUNTERED
     C*
     C     XXFDAT        IFNE      CAFDAT
     C*
     C* PERFORM NETWORK OPERATIONS ON THESE LINES OF SAME DATE
     C*
     C                   Z-ADD     CAFDAT        SAVDAT
     C     LSVLDR        IFEQ      *ON
     C                   EXSR      SRCALN
     C                   ENDIF
     C                   Z-ADD     SAVDAT        XXFDAT
     C                   CLEAR                   DAYS
     C                   CLEAR                   XXPAID
     C*
     C     CAKY1         CHAIN     PCA01                              61
     C                   ITER
     C                   ENDIF                                                              K
     C*
     C                   ELSE                                                   ERROR INDICATOR
     C     CAKY2         READE     PCA01                                  61
     C                   ITER
     C                   ENDIF
     C*
     C     LSVLDR        IFEQ      '1'
     C                   MOVE      XXFDT         XXFDAT
     C                   MOVE      *OFF          LSVLDR
     C                   EXSR      SRCALN
     C                   ENDIF
     C                   MOVE      CAFDAT        X2FDAT
     C     CAKY2         READE     PCA01                                  61
     C*
     C                   ENDDO
     C*
     C* IF THERE ARE SOME RECORDS LEFT TO BE PROCESSED
     C     LSVLDR        IFEQ      '1'
     C                   MOVE      XXFDT         XXFDAT
     C                   EXSR      SRCALN
     C                   ENDIF
     C*
     C                   SETON                                            LR
     C*
     C**************MAIN PROCESSING ENDS****************************
     C***********************************************************
     C* SUBROUTINE TO CHECK ERROR
     C***********************************************************
     C     SRERCK        BEGSR
     C                   MOVE      '0'           ERRIND            1            ERROR INDICATOR
     C*
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C     S4KY1         SETGT     PS401
     C     NXTPHY        TAG
     C     S4KY2         READPE    PS401                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     CAFDAT        IFGT      S4ENDT
     C     S4ENDT        ANDNE     *ZEROS
     C     S4STAT        OREQ      'I'
     C                   GOTO      NXTPHY
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      '1'           ERRIND
     C                   GOTO      END
     C                   ENDIF                                                  *IN61
     C*
     C* CHECK FOR THE RECON NUMBER, READ NEXT RECORD IF NOT SAME
     C*
     C     XXRCN         IFNE      CARCN
     C                   MOVE      '1'           ERRIND
     C                   GOTO      END
     C                   ENDIF
     C*
     C* CHECK FOR CORRECT CODING FLAG
     C*
     C*
     C*  CHECK FOR PT CODE
     C*
     C     CACPT4        IFGE      '10000'
     C     CACPT4        ANDLE     '99607'
     C                   MOVE      '1'           PTFLAG            1
     C                   ELSE
     C*
     C     CACPT4        IFGE      'A0021'
     C     CACPT4        ANDLE     'V5362'
     C                   MOVE      '1'           PTFLAG            1
     C                   ELSE
     C*
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
     C                   MOVE      '1'           PTFLAG            1
     C                   ELSE
     C*
     C                   MOVE      '1'           ERRIND
     C                   GOTO      END
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     END           TAG
     C                   ENDSR
     C***************************************************************
     C**************************************************************
     C*** SUBROUTINE TO COUNT THE NUMBER OF DAYS FROM PHYSICIAN FILE
     C**************************************************************
     C     SRDHN         BEGSR
     C*
     C     DHKY          CHAIN     PDH01                              64
     C     *IN64         IFEQ      '0'
     C                   ADD       DHUNTS        DAYS              5 2
     C                   ENDIF
     C*
     C                   ENDSR
     C*************************************************************
     C**************************************************************
     C*** SUBROUTINE TO SUMUP THE AMOUNT
     C**************************************************************
     C     SRAMT         BEGSR
     C*
     C                   EVAL      @@efdt = CAFDAT
     C                   SELECT
     C                   WHEN      EOBTYP = 'N'
     C                   EVAL      @@code = DHPRCD
     C                   EVAL      @@MOD  = DHMOD1
     C*
     C                   WHEN      EOBTYP = 'H'
     C                   EVAL      @@code = DURATE
     C                   EVAL      @@MOD  = DUPMOD
     C*
     C                   ENDSL
     C                   IF        BDOT1F = 'Y'
     C*** Beg Add *** 12/04/15 **************************************
     C                             OR BDOT1F = 'y'
     C*** End Add *** 12/04/15 **************************************
     C
     C                   Call      'FA784R'
     C                   PARM      EOEOB#        @@eob#
     C                   PARM                    @@code
     C                   PARM                    @@mod
     C                   PARM                    @@efdt
     C                   PARM      'FL'          @@st
     C                   PARM      *zeros        ##alow
     C                   PARM      *blanks       ##rtcd
     C*
     C                   EVAL      XXPAID = XXPAID + ##alow
     C                   ELSE
     C*
     C                   EVAL      XXPAID = XXPAID + CAPAID
     C                   ENDIF
     C*
     C                   ENDSR
     C*************************************************************
     C**************************************************************
     C*** SUBROUTINE TO COUNT THE NUMBER OF DAYS FROM HOSPITAL FILE
     C**************************************************************
     C     SRDUN         BEGSR
     C*
     C     DUKY          CHAIN     PDU01                              64
     C     *IN64         IFEQ      '0'
     C                   ADD       DUUNTS        DAYS              5 2
     C                   ENDIF
     C*
     C                   ENDSR
     C*************************************************************
     C*************************************************************
     C*** SUBROUTINE TO CALCULATE NETWORK
     C*************************************************************
     C     SRCALN        BEGSR
     C*
     C*** Beg Del *** 12/04/15 **************************************
     C***************    IF        BDOT1F = 'Y'
     C*****NETAMT****    DIV       DAYS          NETVAL            9 2
     C***************
     C*****NETVAL****    MULT      DAYS          RES               9 2
     C*****XXPAID****    SUB       RES           CANDIS
     C***************    MOVE      RES           CAPAID
     C***************    Else
     C*** End Del *** 12/04/15 **************************************
     C*** Beg Add *** 12/04/15 *********************************************
     C                   eval      fsPaid = CApaid
     C                   eval      isSpecific =  '0'
     C*
     C* Pay specific codes
     C*
     C                   eval      kkCode = BDcode
     C                   eval      kkEfDt = BDefDt
     C                   eval      kkFrCd = DHprCd
     C*
     C     COky1         Setgt     PCO01
     C     COky2         Readpe    PCO01                                  99
     C*
     C                   if        *in99 = *off and
     C                             CAfDat >= COefDt and DHprCd >= CO10a1
     C                             and DHprCd <= CO10a2
     C*
     C                   eval      decAllow = COrate * CAbrUn
     C*
     C                   if        decAllow  < CApaid or EOast = 'FL'
     C*
     C                   if        decAllow >= CAchrg
     C                   eval      CApaid = CAchrg
     C*
     C                   else
     C                   eval      CApaid = decAllow
     C*
     C                   endif
     C*
     C     fsPAID        SUB       CApaid        CAnDis
     C*
     C                   endif
     C*
     C                   eval      isSpecific =  '1'
     C*
     C                   Endif
     C*
     C*** End Add *** 12/04/15 *********************************************
     C*
     C* Payment is greater than the Highest Limit for the day then set
     C* Highest limit
     C*** Beg Add *** 12/04/15 *********************************************
     C                   if         isSpecific = '0'
     C                   eval       decAllow = ##alow * CAbrUn
     C*** End Add *** 12/04/15 *********************************************
     C                   If        CAPAID <> 0
     C                   If        X2FDAT = CAFDAT
     C*** Beg Chg *** 12/04/15 *********************************************
     C***************    eval      CAPAID = CAPAID * BDPC01
     C                   If        decAllow * (1 - BDpc01) <= CAchrg
     C                   eval      CApaid = decAllow * (1 -BDpc01)
     C                   Endif
     C*** End Chg *** 12/04/15 *********************************************
     C                   eval      XXTOTA = XXTOTA + CAPAID
     C*** Beg Del *** 12/04/15 *********************************************
     C****************   If        XXTOTA > BDAM01
     C****************   eval      CAPAID = BDAM01 - XXTOTP
     C****************   If        XXTOTP > BDAM01
     C****************   eval      CAPAID = 0
     C****************   Endif
     C****************   Endif
     C*** End Del *** 12/04/15 *********************************************
     C*
     C                   Else
     C                   EVAL      XXTOTA = 0
     C                   EVAL      XXTOTP = 0
     C*** End Chg *** 12/04/15 *********************************************
     C****************   eval      CAPAID = CAPAID * BDPC01
     C                   If        decAllow * (1 - BDpc01) <= CAchrg
     C                   eval      CApaid = decAllow * (1 -BDpc01)
     C                   Endif
     C*** End Chg *** 12/04/15 *********************************************
     C                   eval      XXTOTA = XXTOTA + CAPAID
     C                   Endif
     C                   Endif
     C                   Eval      XXTOTP = XXTOTA
     C                   EVAL      NETAMT = CAPAID
     C*** Beg Del *** 12/04/15 *********************************************
     C**********
     C*****NETAMT        DIV       DAYS          NETVAL            9 2
     C**********
     C*****NETVAL        MULT      DAYS          RES               9 2
     C*****XXPAID        SUB       RES           CANDIS
     C*****fsPAID        SUB       RES           CANDIS
     C**********         MOVE      RES           CAPAID
     C*** End Del *** 12/04/15 *********************************************
     C*** Beg Add *** 12/04/15 *********************************************
     C                   if        xxTotA > BDam01
     C                   eval      CApaid = CApaid - (xxTotA - BDam01)
     C                   eval      xxTotA = BDam01
     C                   endif
     C     fsPAID        SUB       CApaid        CAnDis
     C*** End Add *** 12/04/15 *********************************************
     C*
     C                   Endif
     C*
     C                   EXSR      SRCREO
     C*
     C*** Beg Add *** 12/04/15 **************************************
     C                   eval      CApgm = Q1pgm
     C                   eval      CAuser = Q1user
     C                   time                    dsDtTm
     C                   z-Add     DSdate        srDti
     C                   Call      'SRCVTDAT'    dtCyMd
     C                   movel     srDto         CAdt
     C                   eval      CAtime = dsTime
     C*** End Add *** 12/04/15 **************************************
     C                   UPDATE    PCA01
     C*
     C                   ENDSR
     C*****************************************************************
     C*****************************************************************
     C*** SUBROUTINE TO CREATE EOB NOTE
     C*****************************************************************
     C     SRCREO        BEGSR
     C*
     C* GET NEXT DETAIL SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PDE01
     C*
     C     CAAST         IFEQ      'FL'
     C                   MOVEL     ' 93'         XXEOB
     C                   ELSE
     C                   MOVEL     '398'         XXEOB
     C                   ENDIF
     C*
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      XXEOB         DEEOB
     C                   MOVEL     XXEOB         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
      *
     C     CAAST         IFEQ      'FL'
     C*
     C                   MOVE      CACO#         @@CO#             3 0
     C                   CALL      'FA783R'
     C                   PARM                    @@CO#
     C                   PARM      XXEOB         @@EOB
     C                   PARM      BDCOM1        @@TEXT
     C                   PARM      *BLANKS       @@DSC1
     C                   PARM      *BLANKS       @@DSC2
     C                   PARM      *BLANKS       @@DSC3
     C                   PARM      *BLANKS       @@DSC4
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@DSC1        DENOT1
     C                   MOVEL     @@DSC2        DENOT2
     C                   MOVEL     @@DSC3        DENOT3
     C                   MOVEL     @@DSC4        DENOT4
     C*
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C                   Z-ADD     CARCN         DERCN
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   WRITE     PDE01
     C*
     C                   ENDSR
     C****************************************************************
     C*************************************************************
     C* SUBROUTINE TO EXIT PROGRAM
     C*************************************************************
     C     SREXIT        BEGSR
     C                   SETON                                            LR
     C                   RETURN
     C                   ENDSR
     C*************************************************************
