     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE APB042R- UPDATE G/L WITH CHECK PRINTING                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 04/11/02  C:35   PROJECT: SUPPORT AP093 #32046                *
     H*                  INITIALIZE SVCHK# FIELD.                     *
     H*                                                               *
     H*    12/08/97 C:30 Project: WC500                               *
     H*             Track the "highest" Check # for the Voucher       *
     H*             Selection file along with Bank #, Company #, and  *
     H*             Check Date.  Upon completion of Voucher Selection *
     H*             processing, access AP Bank Information to determine
     H*             the current Bank Account.  Then access the WCMS   *
     H*             Bank Account Master and update Starting Check #   *
     H*             using the "highest" Check # plus 1.               *
     H*                                                               *
     H*** 4/30/93 *** C:22 ********************************************
     H*  MADE CHANGE TO USE CHECK DATE FOR ACCOUNTING MONTH AND YEAR  *
     H* 03/06/96 C:22 FIX TO VENDOR ACCESS
     H*
     H** 4/24/97 *** C:107 * AP010 ************************************
     H*  CHANGED PGM TO OUTPUT ZEROS INSTEAD OF BLANKS TO THE ROLLED  *
     H*   TO GL DATE. (NEW FIELD)                                     *
     H*
     H** 5/20/97 *** C:107 * AP017 ************************************
     H*  CHANGED PGM TO OUTPUT THE INVOICE ENTRY DATE WHEN WRITING    *
     H*   TO A/P HISTORY FILE. (NEW FIELD)                            *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
      *****************************************************************
     FAPCSL2    UF   E           K DISK
     F*** BEGIN CHANGE 03/06/96 ***************************************
     FAPVML5    UF   E           K DISK
     F*PVML1  UF  E           K        DISK
     F*** END CHANGE 03/06/96 *****************************************
     FAPADL1    UF   E           K DISK
     FAPCHL1    O  A E           K DISK
     FAPBKL1    IF   E           K DISK
     FBKCFL1    IF   E           K DISK
     FBKDTL1    IF   E           K DISK
     FAPGDL1    O    E           K DISK
     FAPGFL1    O    E           K DISK
      *** BEGIN ADD *** 12/08/97 **************************************
      *
      *  AP Bank Info file
     FAPBIL1    IF   E           K DISK
      *
      *  WCMS Bank Account Master
     FWSCKL1    UF   E           K DISK
      *** END   ADD *** 12/08/97 **************************************
     F*

     D DS              S             30    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFAC#
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     ADKEY         KLIST
     C                   KFLD                    CSCO#
     C                   KFLD                    CSFAC#
     C                   KFLD                    CSVDR
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VENDOR MASTER
     C*
     C     VMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXVDR
      *
      *** BEGIN ADD *** 12/08/97 **************************************
      *
      *  Bank Information file
      *
     C     BIKY1         KLIST
     C                   KFLD                    SVFND
     C                   KFLD                    SVBNK#
     C                   KFLD                    SVCHKD
      *
     C     BIKY2         KLIST
     C                   KFLD                    SVFND
     C                   KFLD                    SVBNK#
      *
      *** END   ADD *** 12/08/97 **************************************
     C*
     C*   DEFINE THE PARM LIST.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    X@BNK#            3 0
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
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CSCO#         XXCO#
     C     *LIKE         DEFINE    CSFAC#        XXFAC#
     C     *LIKE         DEFINE    CSVDR         XXVDR
     C     *LIKE         DEFINE    CSINV#        XXINV#
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
     C     *LIKE         DEFINE    CSBNK#        SVBNK#                         Bank #
     C     *LIKE         DEFINE    CSCO#         SVFND                          Company #
     C     *LIKE         DEFINE    CSCHKD        SVCHKD                         Check Date
     C     *LIKE         DEFINE    CSCHK#        SVCHK#                         Check #
      *
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      **** BEGIN ADD **** 04/11/02 *********************************************
     C                   Z-ADD     0             SVCHK#
      **** END   ADD **** 04/11/02 *********************************************
     C*
     C*** GET DATE RECORD
     C*
     C     'AP'          CHAIN     PDT01                              60
     C                   Z-ADD     DTAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XDATE             8 0
     C                   MOVEL     DTAPDT        XMOYR             5 0
     C                   MOVEL     XMOYR         XCYR              3 0
     C                   MOVE      XCYR          XYR               2 0
     C                   MOVEL     XCYR          XCN               1 0
     C                   MOVE      XMOYR         XMO               2 0
     C*
     C*** GET BANK FILE
     C*
     C     X@BNK#        CHAIN     PBK01                              60
     C*
     C*** READ DETAIL FILE
     C*
     C     X@BNK#        SETLL     PCS01
     C     X@BNK#        READE     PCS01                                  50
     C*
     C                   Z-ADD     CSCO#         XXCO#
     C                   Z-ADD     CSFAC#        XXFAC#
     C                   Z-ADD     CSVDR         XXVDR
     C                   MOVE      CSINV#        XXINV#
     C*
     C     CFKEY         CHAIN     PCF01                              99
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*** DON'T FINALIZE IF CHECK NOT PRINTED
     C*
     C     CSCHK#        IFEQ      *ZERO
     C                   GOTO      NOFIN
     C                   END
     C*
     C*** IF COMPANY OR FACILITY CHANGES PRINT FACILITY TOTAL.
     C*
     C     CSCO#         IFNE      XXCO#
     C     CSFAC#        ORNE      XXFAC#
     C                   EXSR      TOTAL
     C                   Z-ADD     CSFAC#        XXFAC#
     C                   Z-ADD     CSCO#         XXCO#
     C*
     C     CFKEY         CHAIN     PCF01                              99
     C                   END
     C*
0049 C     CSDISC        COMP      *ZERO                              2121
0050 C     CSAMT         SUB       CSDISC        CSNET             9 2
0050 C                   Z-SUB     CSAMT         TOT              11 2
0050 C                   ADD       CSDISC        DISCT            11 2
0050 C                   ADD       CSNET         CASHT            11 2
0050 C                   ADD       TOT           AP               11 2
     C*
     C*** UPDATE VENDOR
     C*
     C                   Z-ADD     CSVDR         XXVDR
     C*** BEGIN CHANGE 03/06/96 ********************************
     C     VMKY          CHAIN     PVM01                              98
     C********** XXVDR     CHAINPVM01                98
     C*** END CHANGE 03/06/96 **********************************
     C                   ADD       1             VMTINV
     C********             ADD  CSAMT     VMPAMT
     C                   ADD       CSNET         VMPAMT
     C                   MOVE      CSCHKD        VMLPDT
     C  N98              UPDATE    PVM01
     C*
     C*** UPDATE EACH RECORD
     C*
     C                   Z-ADD     CSCO#         GFCO#
     C                   Z-ADD     CSFAC#        GFFAC#
     C                   Z-ADD     CSVDR         GFVDR
     C                   Z-ADD     CSBNK#        GFBNK#
     C                   Z-ADD     CSCHK#        GFCHK#
     C                   MOVE      CSCHKD        GFCHKD
     C*****                Z-ADDCSCO#     GFCO1
     C*****                Z-ADDCSFAC#    GFFAC1
     C******               Z-ADDCFDPT1    GFGLD1
     C*****                Z-ADDCFACC1    GFACC1
     C******               Z-ADDCFSUB1    GFSUB1
     C     CSAMT         SUB       CSDISC        GFAMT
     C                   Z-ADD     CSDISC        GFDISC
     C                   MOVE      CSINV#        GFINV#
     C                   MOVEL     CSPODS        GFPODS
     C                   MOVEL     XMO           GFACMO
     C                   MOVE      XYR           GFACYR
     C     XCN           IFEQ      1
     C                   MOVEL     20            GFACYR
     C                   ELSE
     C                   MOVEL     19            GFACYR
     C                   END
     C                   MOVE      Q1PGM         GFIPGM
     C                   MOVE      Q1USER        GFIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GFIDT
     C                   Z-ADD     DSTIME        GFITIM
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GFRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGF01
     C*
     C*** CHECK FOR DISCOUNT
     C*
     C**         CSDISC    IFNE *ZERO
     C**                   Z-ADDCFCO2     GFCO1
     C**                   Z-ADDCFFAC2    GFFAC1
     C**                   Z-ADDCFDPT2    GFGLD1
     C**                   Z-ADDCFACC2    GFACC1
     C**                   Z-ADDCFSUB2    GFSUB1
     C**                   Z-SUBCSDISC    GFAMT
     C**                   WRITEPGF01
     C**                   END
     C*
     C*  UPDATE INVOICE HISTORY
     C*
     C                   MOVE      CSSEQ1        SCSEQ1            5 0
     C     ADKEY         CHAIN     PAD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ADSEQ1        CHSEQ1
     C                   MOVE      ADCODE        CHCODE
     C                   MOVE      ADVDR         CHVDR
     C                   MOVE      ADCO#         CHCO#
     C                   MOVE      ADFAC#        CHFAC#
     C                   MOVE      ADBTCH        CHBTCH
     C********             MOVE ADACMO    CHACMO
     C********             MOVE ADACYR    CHACYR
     C*** BEGAN CHANGE *** 4/30/93 ****************************
     C                   MOVEL     CSCHKD        XMOYR
     C                   MOVEL     XMOYR         XCYR
     C                   MOVE      XMOYR         XMO
     C*** END   CHANGE *** 4/30/93 ***************************
     C                   MOVE      XMO           CHACMO
     C                   MOVEL     XCYR          XCN               1 0
     C                   MOVE      XCYR          CHACYR
     C     XCN           IFEQ      1
     C                   MOVEL     20            CHACYR
     C                   ELSE
     C                   MOVEL     19            CHACYR
     C                   END
     C                   MOVE      ADSEQ2        CHSEQ2
     C                   MOVE      ADDUED        CHDUED
     C                   MOVE      ADINVD        CHINVD
     C                   MOVE      ADINV#        CHINV#
     C                   MOVE      ADPONO        CHPONO
     C                   MOVE      ADPODS        CHPODS
     C                   Z-ADD     CSDISC        CHDISC
     C                   Z-ADD     CSAMT         CHAMT
     C                   Z-ADD     CSCHK#        CHCHK#
     C                   Z-ADD     CSBNK#        CHBNK#
     C*
     C                   MOVE      Q1USER        CHCUSR
     C                   MOVE      Q1PGM         CHIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHCDT
     C                   Z-ADD     DSTIME        CHCTIM
     C                   MOVE      Q1USER        CHIUSR
     C                   TIME                    DSDTTM
     C*******    DSDATE    MULT 10000.01  CHIDT
     C                   MOVE      CSCHKD        CHIDT
     C                   Z-ADD     DSTIME        CHITIM
      * BEG ADD * 05/20/97 * AP017
     C                   Z-ADD     ADRDAT        GLENDT
      * END ADD * 05/20/97 * AP017
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             CHRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PCH01
     C*
     C     CSAMT         IFEQ      ADAMT
     C                   DELETE    PAD01
     C                   ELSE
     C                   SUB       CSAMT         ADAMT
     C                   UPDATE    PAD01
     C                   END
     C                   END                                                    END *IN60
      *
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
      *  Track Voucher Selection fields for later update of Starting Check# in
      *  WCMS Bank Account Master.
      *
     C                   Z-ADD     CSBNK#        SVBNK#                         Bank #
     C                   Z-ADD     CSCO#         SVFND                          Company #
     C                   Z-ADD     CSCHKD        SVCHKD                         Check Date
      *
     C     CSCHK#        IFGT      SVCHK#
     C                   Z-ADD     CSCHK#        SVCHK#                         Check #
     C                   ENDIF
      *
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C*  DELETE CHECK SELECTION
     C*
     C                   DELETE    PCS01
     C*
     C*** READ NEXT RECORD.
     C*
     C     NOFIN         TAG
     C     X@BNK#        READE     PCS01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C                   EXSR      TOTAL
      *
      *** BEGIN ADD *** 12/08/97 ***********************************************
      *
      *  Update WCMS Bank Account Master file with 'New' Starting Check#
      *
     C                   EXSR      SRACCT
      *
      *** END   ADD *** 12/08/97 ***********************************************
     C*
     C*** END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*
     C     TOTAL         BEGSR
     C*
     C*** WRITE THE CASH A/P TRANSACTION.
     C*
     C                   Z-ADD     BKCO3         GDCO1
     C                   Z-ADD     BKFAC3        GDFAC1
     C                   Z-ADD     BKDPT3        GDGLD1
     C                   Z-ADD     BKACC3        GDACC1
     C                   Z-ADD     BKSUB3        GDSUB1
     C                   Z-SUB     CASHT         GDAMT
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     DS(1)         GDDESC
     C                   MOVE      CSBNK#        GDDESC
     C                   MOVEL     'AP'          GDSRC
     C*
     C                   MOVEL     XMO           GDACMO
     C                   MOVE      XYR           GDACYR
     C     XCN           IFEQ      1
     C                   MOVEL     20            GDACYR
     C                   ELSE
     C                   MOVEL     19            GDACYR
     C                   END
     C                   MOVE      *BLANKS       GDREFR
     C                   MOVE      CSCHKD        GDIDAT
     C                   MOVEL     'AUTOPOST'    GDBTCH
     C                   Z-ADD     *ZEROS        GDSEQ1
     C                   Z-ADD     1             GDSEQ2
     C                   MOVE      '2'           GDTYPE
     C                   MOVE      *ZEROS        GDVDR
     C                   MOVE      XXCO#         GDCO#
     C                   MOVE      XXFAC#        GDFAC#
     C                   MOVE      *BLANKS       GDINV#
     C*
     C                   MOVE      Q1USER        GDCUSR
     C                   MOVE      Q1PGM         GDIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDCDT
     C                   Z-ADD     DSTIME        GDCTIM
     C*
     C                   MOVE      Q1USER        GDIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDT
     C                   Z-ADD     DSTIME        GDITIM
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GDRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGD01
     C*
     C     DISCT         IFNE      *ZERO
     C*
     C*** WRITE THE CASH DISCOUNT A/P TRANSACTION.
     C*
     C                   Z-ADD     CFCO2         GDCO1
     C                   Z-ADD     CFFAC2        GDFAC1
     C                   Z-ADD     CFDPT2        GDGLD1
     C                   Z-ADD     CFACC2        GDACC1
     C                   Z-ADD     CFSUB2        GDSUB1
     C                   Z-SUB     DISCT         GDAMT
     C                   Z-ADD     2             GDSEQ2
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     DS(1)         GDDESC
     C                   MOVE      '3'           GDTYPE
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GDRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGD01
     C                   END
     C*
     C*** OFFSET THE A/P ACCOUNT
     C*
     C                   Z-ADD     CFCO1         GDCO1
     C                   Z-ADD     CFFAC1        GDFAC1
     C                   Z-ADD     CFDPT1        GDGLD1
     C                   Z-ADD     CFACC1        GDACC1
     C                   Z-ADD     CFSUB1        GDSUB1
     C                   Z-SUB     AP            GDAMT
     C                   Z-ADD     2             GDSEQ2
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     DS(1)         GDDESC
     C                   MOVE      '4'           GDTYPE
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GDRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGD01
     C*
     C*** CLEAR TOTALS
     C*
     C                   Z-ADD     *ZERO         AP
0050 C                   Z-ADD     *ZERO         DISCT
0050 C                   Z-ADD     *ZERO         CASHT
     C                   ENDSR
      *
      *** BEGIN ADD *** 12/08/97 ***********************************************
      **************************************************************************
      *  SRACCT - Sub to update WCMS Bank Account # with 'New' Starting Check #
      **************************************************************************
     C     SRACCT        BEGSR
      *
      *  Access AP Bank Information file
      *
     C     BIKY1         SETLL     PBI01
     C     BIKY2         READE     PBI01                                  76
      *
     C     *IN76         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY2         READE     PBI01                                  76
     C                   ENDDO
      *
     C     *IN76         IFEQ      '1'
     C                   GOTO      EXACCT
     C                   END
      *
      *  Access WCMS Bank Account Master file
      *
     C     BIACCT        CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SVCHK#        ADD       1             CKLCHK                         Starting Check#
     C                   MOVE      Q1USER        CKUSER                         USER
     C                   MOVE      Q1PGM         CKPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CKTIME                         TIME MAINTAINED
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CKDT                           DATE MAINTAINED
      *
     C                   UPDATE    PCK01
      *
     C                   ENDIF
      *
     C     EXACCT        TAG
      *
     C                   ENDSR
      *
      *** END   ADD *** 12/08/97 ***********************************************
**
ACCOUNTS PAYABLE CHECK WRITING
