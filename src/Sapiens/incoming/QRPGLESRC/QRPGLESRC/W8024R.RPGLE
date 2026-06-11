     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8024R - RESET NET DEDUCTIBLE RECOVERY
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   This program reverses NET deductible recoveries.  This pgm  *
     H*   will be called from the Claim Transfer pgm when a claim is  *
     H*   transferred from one employer to another.                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7265R - Claim Transfer                                     *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:15                                              *
     H* CREATED:    02/25/98    PROJECT:                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*                                                               *
     H* 01/03/08 C:15  Support: WC135/60772                           *
     H*                Added code to apply the A/R deduct credit      *
     H*                to the main PEO division for client 135.       *
     H*                                                               *
     H* 03/23/04 C:15  Project:  WC1170                               *
     H*                Added code to load the reserve history buckets *
     H*                according to the order in the claim file.      *
     H*                                                               *
     H* 03/09/04 C:15  Project: WC1080   seq# 36459                   *
     H*                Added code to create an A/R deduct tax record  *
     H*                to reduce the tax billed based on the amount   *
     H*                the deduct billed was reduced.                 *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP.  *
     H*                                                               *
     H* 03/14/03 C:15  Project: WC1080   seq# 36459                   *
     H*                Added code to retrieve the correct A/R deduct  *
     H*                billing code.  The pgm should not select the   *
     H*                deduct tax code.                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   20  -  CHAIN failed on file WADQP                           *
     H*   60  -  General work indicator                               *
     H*   61  -  General work indicator                               *
     H*****************************************************************
     FWADQL     UF A E           K DISK
     FWDELL     UF   E           K DISK
     FWDPAL3    IF A E           K DISK
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMEAL     O    E           K DISK
     FWMCML1    UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMRHP     O    E           K DISK
     F*** Begin add *** 03/14/03 **************************************
     FWDF1L     IF   E           K DISK
     F*** End add ***** 03/14/03 **************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F*** Begin add *** 03/09/04 ************************************
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     F*** End add ***** 03/09/04 ************************************
     F*

     D RC              S              2    DIM(10)
     D AM              S              9  2 DIM(10)
     D*** Begin add *** 03/23/04 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
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
     D  AR                     1     90  2
     D                                     DIM(10)
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
     D*
     D*** Begin add *** 03/23/04 **************************************
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)                              RH rsv types
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)                              RH rsv amt
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** End add ***** 03/23/04 **************************************
     D*
     D XXDESC          C                   CONST('DEDUCTIBLE RECOVERY -
     D                                     REVERSAL')
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** End add ***** 03/23/04 **************************************
     I*
     IPPA02
     I              PACHK#                      P2CHK#
     I              PABASQ                      P2BASQ
     I              PATRDT                      P2TRDT
     I              PAPODT                      P2PODT
     I              PAFEIN                      P2FEIN
     I              PAMOD                       P2MOD
     I              PAAMT                       P2AMT
     I              PABR2                       P2BR2
     I              PABR7                       P2BR7
     I              PAFRDT                      P2FRDT
     I              PATODT                      P2TODT
     I              PAWKS                       P2WKS
     I              PADAYS                      P2DAYS
     I              PATRCD                      P2TRCD
     I              PATYPE                      P2TYPE
     I              PARECD                      P2RECD
     I              PARETP                      P2RETP
     I              PADESC                      P2DESC
     I              PAADST                      P2ADST
     I              PASTAT                      P2STAT
     I              PAUSER                      P2USER
     I              PAPGM                       P2PGM
     I              PATIME                      P2TIME
     I              PADT                        P2DT
     I              PABUL1                      P2BUL1
     I              PAPGTP                      P2PGTP
     I              PAEOB#                      P2EOB#

     C**************************************************************************
     C*  Process initialization subroutine.
     C*
     C                   EXSR      SRINIT
     C*
     C*  Accumulate the amount of deduct recoveries for each reserve
     C*    code.
     C*
     C                   CLEAR                   RC
     C                   CLEAR                   AM
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PABR2         IFEQ      FNMEP1
     C     PABR2         OREQ      FNCOP1
     C     PABR2         OREQ      FNEXP1
     C     PABR2         OREQ      FNLEP1
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RC(X)                                  61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  61
     C   61              MOVE      PARECD        RC(X)
     C                   ENDIF
     C   61              ADD       PAAMT         AM(X)
     C*
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C*  Next, loop thru the recoveries by reserve code and apply the
     C*    reversals to the appropriate files.
     C*
     C                   Z-ADD     1             X
     C     RC(X)         DOWNE     *BLANKS
     C*
     C     AM(X)         IFNE      *ZERO
     C*
     C                   EXSR      UPDCLM                                       WMCMP
     C                   EXSR      UPDPMT                                       WDPAP
     C                   EXSR      RSVHST                                       WMRHP
     C                   EXSR      PNDDED                                       WADQP
     C*
     C                   ENDIF                                                  AM,X IFNE 0
     C*
     C                   ADD       1             X
     C                   ENDDO                                                  CD,X DOWNE *BLANKS
     C*
     C*  Update the TO Employer's 'Total Claims Paid' amount.
     C*
     C                   EXSR      UPDEMP
     C*
     C*  Adjust the FROM Employer's deduct billing in A/R.
     C*
     C                   EXSR      UPDAR
     C*
     C*  End of Program.
     C*
     C                   MOVE      *ON           *INLR
      *
     C***  Begin Add  ***  11/19/09  **************************************
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EANINV        #IINV#
     C*
     C     CLTID         IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      EAPODT        #IDT
     C     EANINV        PARM                    #IINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C***  End   Add  ***  11/19/09  **************************************
      *
     C**************************************************************************
     C* SRINIT - This subroutine initializes all fields and key lists.
     C**************************************************************************
     C     SRINIT        BEGSR
     C*
     C*  Key list for the Fund Master file.
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*** Begin add *** 03/14/03 **************************************
     C*  Key list for the Fund Detail file.
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*** End add ***** 03/14/03 **************************************
     C*
     C*  Key list for the Pending Deductible Recovery file
     C*
     C     DQKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFEMP
     C                   KFLD                    XXFDIV
     C*** Begin chg *** 03/09/04 ************************************
     C*******************  KFLD           XXFYR
     C                   KFLD                    XXFFYR
     C*** End chg ***** 03/09/04 ************************************
     C                   KFLD                    XXCHK#
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  Key list for the Employer Detail file
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXTEMP
     C                   KFLD                    XXTDIV
     C                   KFLD                    XXFYR
     C*
     C*** Begin add *** 03/09/04 ************************************
     C     ELFKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFEMP
     C                   KFLD                    XXFDIV
     C                   KFLD                    XXFFYR
     C*** End add ***** 03/09/04 ************************************
     C*
     C*  Key list for the Payment Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  key list for the Claim Table files
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     TBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*** Begin add *** 03/09/04 ************************************
     C*  Key list for the Rate Set Code File
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C*  Key list for Stat Rating/Reporting File
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*** End add ***** 03/09/04 ************************************
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQFDIV            5
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQTDIV            5
     C                   PARM                    QQAMTB            9
     C                   PARM                    QQRTCD            1
     C*** Begin add *** 03/09/04 ************************************
     C                   PARM                    QQFFYR            3
     C*** End add ***** 03/09/04 ************************************
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
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXFEMP
     C     *LIKE         DEFINE    CMDIV         XXFDIV
     C     *LIKE         DEFINE    CMEMP#        XXTEMP
     C     *LIKE         DEFINE    CMDIV         XXTDIV
     C     *LIKE         DEFINE    DQCHK#        XXCHK#
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C     *LIKE         DEFINE    EAARCD        XXDEDC
     C     *LIKE         DEFINE    EAAMT         XXAMTB
     C*** Begin add *** 03/09/04 ************************************
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    EAFYR         XXFFYR
     C*** End add ***** 03/09/04 ************************************
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Y                 3 0
     C*
     C*** Begin add *** 01/03/08 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End add ***** 01/03/08 **************************************
     C*
     C*  Load the entry parms into the work fields.
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQFEMP        XXFEMP
     C                   MOVE      QQFDIV        XXFDIV
     C                   MOVE      QQTEMP        XXTEMP
     C                   MOVE      QQTDIV        XXTDIV
     C                   MOVE      QQAMTB        XXAMTB
     C*** Begin add *** 03/09/04 ************************************
     C     Q1PRMS        IFGT      11
     C                   MOVE      QQFFYR        XXFFYR
     C                   ELSE
     C                   MOVE      QQFYR         XXFFYR
     C                   ENDIF
     C*** End add ***** 03/09/04 ************************************
     C*
     C*  Retrieve the Fund Master record.
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       FNMETR
     C                   MOVE      *BLANKS       FNMEP1
     C                   MOVE      *BLANKS       FNCOTR
     C                   MOVE      *BLANKS       FNCOP1
     C                   MOVE      *BLANKS       FNEXTR
     C                   MOVE      *BLANKS       FNEXP1
     C                   MOVE      *BLANKS       FNLETR
     C                   MOVE      *BLANKS       FNLEP1
     C                   ENDIF
     C*
     C*** Begin add *** 03/14/03 **************************************
     C*  Retrieve the deduct billing A/R code.
     C*
     C     F1KY          CHAIN     PF101                              99
     C   99              MOVE      'D1'          F1DTXB
     C*** End add ***** 03/14/03 **************************************
     C*
     C*  Retrieve the deduct billing A/R code.
     C*
     C                   MOVE      *BLANKS       XXDEDC
     C     TBKY1         CHAIN     PTT01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTTYPE        IFEQ      'F'
     C     TTCTYP        ANDEQ     'R'
     C*** Begin add *** 03/14/03 **************************************
     C     TTKEY         ANDNE     F1DTXB
     C*** End add ***** 03/14/03 **************************************
     C                   MOVEL(P)  TTKEY         XXDEDC
     C                   LEAVE
     C                   ENDIF
     C*
     C     TBKY1         READE     PTT01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C* UPDCLM - This subroutine updates the payment bucket in the
     C*   Claim Master file.
     C**************************************************************************
     C     UPDCLM        BEGSR
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     1             Y
     C     RC(X)         LOOKUP    RT(Y)                                  61
     C   61              ADD       AM(X)         AR(Y)
     C*
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C* UPDPMT - This subroutine writes the recovery reversals to the
     C*   Payment Detail file.
     C**************************************************************************
     C     UPDPMT        BEGSR
     C*
     C*  Load the payment fields.
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   Z-ADD     XXFND         PAFND
     C                   Z-ADD     XXFYR         PAFYR
     C                   Z-ADD     XXCASE        PACASE
     C                   Z-ADD     9999999       PACHK#
     C                   Z-ADD     *ZERO         PABASQ
     C                   MOVE      *ZERO         PAFEIN
     C                   MOVE      *ZERO         PAMOD
     C                   Z-SUB     AM(X)         PAAMT
     C*
     C                   MOVEL(P)  RC(X)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     RTTYPE        IFEQ      'M'
     C                   MOVE      FNMEP1        PABR2
     C                   MOVE      FNMETR        PATRCD
     C                   ELSE
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      FNCOP1        PABR2
     C                   MOVE      FNCOTR        PATRCD
     C                   ELSE
     C     RTTYPE        IFEQ      'E'
     C                   MOVE      FNEXP1        PABR2
     C                   MOVE      FNEXTR        PATRCD
     C                   ELSE
     C     RTTYPE        IFEQ      'L'
     C                   MOVE      FNLEP1        PABR2
     C                   MOVE      FNLETR        PATRCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      'R'           PATYPE
     C                   MOVEL     RC(X)         PARECD
     C                   MOVE      RTTYPE        PARETP
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C                   MOVEL     B2DESC        PADESC
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL     'LT'          PABR7
     C                   ELSE
     C                   MOVEL     'NL'          PABR7
     C                   END
     C*
     C                   Z-ADD     *ZERO         PAFRDT
     C                   Z-ADD     *ZERO         PATODT
     C                   Z-ADD     *ZERO         PAWKS
     C                   Z-ADD     *ZERO         PADAYS
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      *BLANK        PAADST
     C                   Z-ADD     0             PABUL1
     C                   MOVE      *BLANK        PAPGTP
     C                   Z-ADD     0             PAEOB#
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C                   MOVE      PATRDT        PAPODT
     C                   MOVE      PATRDT        PADT
     C                   Z-ADD     DSTIME        PATIME
     C*
     C*  Get the next available payment seq#.
     C*
     C     PAKY          SETGT     PPA02
     C     PAKY          READPE    PPA02                                  60
     C   60              Z-ADD     0             PASEQ#
     C                   ADD       1             PASEQ#
     C*
     C                   WRITE     PPA01
     C*
     C                   ENDSR
     C**************************************************************************
     C* RSVHST - This subroutine creates an entry in the Reserve
     C*   History file since the remaining reserves are not updated.
     C**************************************************************************
     C     RSVHST        BEGSR
     C*
     C                   CLEAR                   PRH01
     C*
     C                   Z-ADD     XXCO#         RHCO#
     C                   Z-ADD     XXFND         RHFND
     C                   Z-ADD     XXFYR         RHFYR
     C                   Z-ADD     XXCASE        RHCASE
     C*** Begin dlt *** 03/23/04 **************************************
     C**********           MOVE RC,X      RHRT01
     C**********           ADD  AM,X      RHSU01
     C*** End dlt ***** 03/23/04 **************************************
     C*** Begin add *** 03/23/04 **************************************
     C                   MOVE      RC(X)         RRT(Y)
     C                   ADD       AM(X)         RSU(Y)
     C*** End add ***** 03/23/04 **************************************
     C                   MOVEL     XXDESC        RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      CMSTAT        RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVE      RHDT          RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
     C*** BEG ADD ***** 03/03/10 **************************************
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
     C*** END ADD ***** 03/03/10 **************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR
     C**************************************************************************
     C* PNDDED - This subroutine increases the amount in the Pending
     C*   Deductible Recovery file.
     C**************************************************************************
     C     PNDDED        BEGSR
     C*
     C*  Add the reversal amount into check# 9999999 in the Pending Ded
     C*    Recovery file.
     C*
     C                   Z-ADD     9999999       XXCHK#
     C     DQKY          CHAIN     PDQ01                              40
     C*
     C                   Z-ADD     XXCO#         DQCO#
     C                   Z-ADD     XXFND         DQFND
     C*** Begin chg *** 03/09/04 ************************************
     C*******************  Z-ADDXXFYR     DQFYR
     C                   Z-ADD     XXFFYR        DQFYR
     C*** End chg ***** 03/09/04 ************************************
     C                   Z-ADD     XXFEMP        DQEMP#
     C                   Z-ADD     XXFDIV        DQDIV
     C                   Z-ADD     XXCHK#        DQCHK#
     C                   ADD       AM(X)         DQAMT
     C                   ADD       AM(X)         DQAMTR
     C                   MOVE      Q1USER        DQUSER
     C                   MOVE      Q1PGM         DQPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DQDT
     C                   Z-ADD     DSTIME        DQTIME
     C                   MOVE      *BLANK        DQSTAT
     C*
     C  N40              UPDATE    PDQ01
     C   40              WRITE     PDQ01
     C*
     C                   ENDSR
     C**************************************************************************
     C* UPDEMP - This subroutine increases the 'Total claim pmts' bucket
     C*   in the Employer Detail file.
     C**************************************************************************
     C     UPDEMP        BEGSR
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Calculate the total amount reversed.
     C*
     C                   XFOOT     AM            XXAMT
     C     XXAMT         IFNE      0
     C*
     C*  Update the Emp Detail file.
     C*
     C                   ADD       XXAMT         ELTCLP
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C**************************************************************************
     C* UPDAR - This subroutine creates a deduct billing credit, for the
     C*   employer from which the claim was transferred, in the A/R file.
     C**************************************************************************
     C     UPDAR         BEGSR
     C*
     C     XXAMTB        IFNE      0
     C*
     C*  Create the deduct billing credit record in the
     C*
     C                   CLEAR                   PEA01
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C*** Begin chg *** 03/09/04 ************************************
     C*******************  Z-ADDXXFYR     EAFYR
     C                   Z-ADD     XXFFYR        EAFYR
     C*** End chg ***** 03/09/04 ************************************
     C                   Z-ADD     XXFEMP        EAEMP#
     C                   Z-ADD     XXFDIV        EADIV
     C*** Begin add *** 01/03/08 **************************************
     C     CLTID         IFEQ      '135'
     C     FNPEOF        ANDNE     *BLANKS
     C                   Z-ADD     0             EADIV
     C                   ENDIF
     C*** End add ***** 01/03/08 **************************************
     C                   MOVE      XXDEDC        EAARCD
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C  N60              MOVE      TTTYPE        EAARTY
     C  N60              MOVE      TTDESC        EADESC
      *** BEG ADD ***** 11/04/03 ************************************
     C  N60              MOVE      TTCTYP        EACTYP
      *** END ADD ***** 11/04/03 ************************************
     C*
     C                   Z-SUB     XXAMTB        EAAMT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     EATRDT        EAPODT
     C                   MOVE      *BLANKS       EAEDIT
     C                   MOVE      'A'           EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     EATRDT        EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     XXCHK#        EACHK#
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      GETINV
     C***  End   Add  ***  11/19/09  **************************************
     C                   WRITE     PEA01
     C*
     C*** Begin add *** 03/09/04 ************************************
     C*
     C*  If 'TAX DEDUCTIBLE BILL' flag in Stat Rating File is 'Y',
     C*  then create a deduct tax adjustment record.
     C*
     C     ELFKY         CHAIN     PEL01                              99
     C*
     C     RSKY          CHAIN     PRS01                              99
     C  N99              MOVE      RSJRST        XXST
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  99
     C*
     C  N99RRRDTX        IFEQ      'Y'
     C*
     C                   CLEAR                   PEA01
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFFYR        EAFYR
     C                   Z-ADD     XXFEMP        EAEMP#
     C                   Z-ADD     XXFDIV        EADIV
     C*** Begin add *** 01/03/08 **************************************
     C     CLTID         IFEQ      '135'
     C     FNPEOF        ANDNE     *BLANKS
     C                   Z-ADD     0             EADIV
     C                   ENDIF
     C*** End add ***** 01/03/08 **************************************
     C                   MOVE      F1DTXB        EAARCD
     C*
     C                   MOVEL(P)  F1DTXB        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C  N60              MOVE      TTTYPE        EAARTY
     C  N60              MOVE      TTDESC        EADESC
     C  N60              MOVE      TTCTYP        EACTYP
     C*
     C     XXAMTB        MULT      RRTXPC        EAAMT
     C     EAAMT         MULT(H)   -.01          EAAMT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     EATRDT        EAPODT
     C                   MOVE      *BLANKS       EAEDIT
     C                   MOVE      'A'           EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     EATRDT        EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     XXCHK#        EACHK#
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      GETINV
     C***  End   Add  ***  11/19/09  **************************************
     C                   WRITE     PEA01
     C*
     C                   ENDIF
     C*** End add ***** 03/09/04 ************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
