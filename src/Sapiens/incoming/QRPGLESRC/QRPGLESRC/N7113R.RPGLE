     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE N7113R - HBA ACTIVITY REPORT - PBILLS
     H*
     H* CREATED 03/08/01 BY C:62 FOR PROJECT:MC242
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAEAL11   IF   E           K DISK
     FNMARL2    IF   E           K DISK
     FNMARL8    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D INVA            S              9  0 DIM(9999)
     D INVB            S              5  0 DIM(9999)
     D INVC            S              5  0 DIM(9999)
     D                 DS
     D  ZZSS#                  1      9
     D  ZZSS1                  1      3
     D  ZZSS2                  4      5
     D  ZZSS3                  6      9
     D                 DS
     D  XXSS#                  1     11
     D  XXSS1                  1      3
     D  XXHYP1                 4      4    INZ('-')
     D  XXSS2                  5      6
     D  XXHYP2                 7      7    INZ('-')
     D  XXSS3                  8     11
     D                 DS
     D  XSZIP                  1      9  0
     D  S1ZPP1                 1      5  0
     D  S1ZPP2                 6      9  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB             5
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZATION SECTION
     C*
     C     *LIKE         DEFINE    EAEOB#        HLEOB#
     C     *LIKE         DEFINE    EARCN         HLRCN
     C     *LIKE         DEFINE    UPDT          XXFDT
     C     *LIKE         DEFINE    UPDT          XXTDT
     C     *LIKE         DEFINE    EACHRG        XCCHRG
     C     *LIKE         DEFINE    EAOTHR        XCOTHR
     C     *LIKE         DEFINE    EAOTH2        XCOTH2
     C     *LIKE         DEFINE    EANDIS        XCNDIS
     C     *LIKE         DEFINE    EAMRA         XCMRA
     C     *LIKE         DEFINE    EAPAID        XCPAID
     C     *LIKE         DEFINE    EACHRG        XGCHRG
     C     *LIKE         DEFINE    EAOTHR        XGOTHR
     C     *LIKE         DEFINE    EAOTH2        XGOTH2
     C     *LIKE         DEFINE    EANDIS        XGNDIS
     C     *LIKE         DEFINE    EAMRA         XGMRA
     C     *LIKE         DEFINE    EAPAID        XGPAID
     C     *LIKE         DEFINE    EACHRG        XTCHRG
     C     *LIKE         DEFINE    EAOTHR        XTOTHR
     C     *LIKE         DEFINE    EAOTH2        XTOTH2
     C     *LIKE         DEFINE    EANDIS        XTNDIS
     C     *LIKE         DEFINE    EAMRA         XTMRA
     C     *LIKE         DEFINE    EAPAID        XTPAID
     C     *LIKE         DEFINE    EAMRA         XTAMTS
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C*
     C*  A/R FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C*
     C     ARKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXINV#            9 0
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ARCO#
     C                   KFLD                    ARSUB#
     C                   KFLD                    ARFND
     C                   KFLD                    ARFYR
     C                   KFLD                    ARCASE
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  BILL HEADER FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    ARCO#
     C                   KFLD                    ARSUB#
     C                   KFLD                    ARFND
     C                   KFLD                    ARINV#
     C                   KFLD                    AREOB#
     C                   KFLD                    ARRCN
     C*
     C*  EOB WORK FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    HLDFND
     C*
     C*  ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVE      @@SUB         XXSUB#            5 0
     C                   MOVE      @@FRDT        XXFRDT            8 0
     C                   MOVE      @@TODT        XXTODT            8 0
     C*
     C                   Z-ADD     0             I                 4 0          CHG 11/07/95
     C                   Z-ADD     0             J                 4 0
     C                   Z-ADD     0             K                 4 0
     C                   Z-ADD     0             INVTL             7 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C*  GET SUBSCRIBER RECORD (CARRIER INFO)
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR REPORT
     C*
     C                   MOVE      '0'           XXPRT                           QSYSPRT
     C     ORKY          CHAIN     POR01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION FOR REPORT
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       XXTITL
     C                   ENDIF                                                  END IFEQ '0'
     C*
     C* * * GET COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   ENDIF
     C*
     C*   PRINT THE HEADING INFORMATION ON THE REPORT
     C*
     C                   EXCEPT    HEAD1
     C*
     C*  GET A/R RECORDS
     C*
     C     ARKY          CHAIN     PAR01                              99
     C     *IN99         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C                   ADD       1             I
     C                   MOVEA     ARINV#        INVA(I)
     C*
     C                   Z-ADD     ARRCN         HLRCN
     C                   Z-ADD     AREOB#        HLEOB#
     C                   Z-ADD     ARCASE        HLDCSE            7 0
     C                   Z-ADD     ARFND         HLDFND            3 0
     C*
     C     EAKY          CHAIN     PEA01                              98
     C     *IN98         IFEQ      '0'
     C*
     C                   ADD       ARAMT         XCAMTI            9 2
     C                   ADD       EACHRG        XCCHRG
     C                   ADD       EAOTHR        XCOTHR
     C                   ADD       EAOTH2        XCOTH2
     C                   ADD       EANDIS        XCNDIS
     C                   ADD       EAMRA         XCMRA
     C                   ADD       EAPAID        XCPAID
     C                   ADD       1             XCCNT             6 0
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     CMSS#         ZZSS#
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     ZZSS1         XXSS1
     C***                MOVEL     ZZSS2         XXSS2
     C***                MOVEL     ZZSS3         XXSS3
      /Free
       xxSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C                   MOVEL     CMCCL#        XXCLM#           13
     C                   END
     C*
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM            1
     C*
     C                   MOVEL     SRLNAM        XXNAME           25
     C                   END
     C*
     C     FNKY          CHAIN     PFN01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNME           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNME
     C                   END
     C*
     C                   EXCEPT    HEAD2
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C*  TEST FOR CASE LEVEL BREAK
     C*
     C     ARCASE        IFNE      HLDCSE
     C     ARFND         ANDEQ     HLDFND
     C                   EXSR      SRTCSE
     C                   END
     C*
     C*  TEST FOR GROUP LEVEL BREAK
     C*
     C     ARFND         IFNE      HLDFND
     C                   EXSR      SRTCSE
     C                   EXSR      SRTFND
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C*  TEST FOR CHANGE IN EOB#
     C*  IF CHANGED, READ EOB FILE FOR AMT CHARGED AND REC. ALLOWANCE
     C*
     C     AREOB#        IFNE      HLEOB#
     C     ARRCN         ORNE      HLRCN
     C     EAKY          CHAIN     PEA01                              51
     C     *IN51         IFEQ      '0'
     C                   ADD       ARAMT         XCAMTI            9 2
     C                   ADD       EACHRG        XCCHRG
     C                   ADD       EAOTHR        XCOTHR
     C                   ADD       EAOTH2        XCOTH2
     C                   ADD       EANDIS        XCNDIS
     C                   ADD       EAMRA         XCMRA
     C                   ADD       EAPAID        XCPAID
     C                   ADD       1             XCCNT             6 0
     C                   ENDIF
     C                   Z-ADD     AREOB#        HLEOB#
     C                   Z-ADD     ARRCN         HLRCN
     C                   END
     C*
     C*  READ THE NEXT A/R RECORD FOR THIS SUBSCRIBER
     C*
     C     ARKY          READE     PAR01                                  99
     C                   END                                                    ENDDOW *IN50
     C*
     C*** END PROGRAM
     C*
     C                   EXSR      SRTCSE
     C                   EXSR      SRTFND
     C                   EXSR      SRTOTL
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*  TOTAL LINE FOR CASE (DETAIL SUMMARY)            S R T C S E  *
     C*****************************************************************
     C     SRTCSE        BEGSR
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   Z-ADD     HLDCSE        XXCASE            7 0
     C*
     C     XCCNT         IFNE      0                                            ## LJD 9/21
     C                   EXCEPT    TOTCSE
     C                   END                                                    ##
     C*
     C                   ADD       XCCNT         XGCNT             6 0          ADD 08/20/93
     C                   ADD       XCCHRG        XGCHRG
     C                   ADD       XCOTHR        XGOTHR
     C                   ADD       XCOTH2        XGOTH2
     C                   ADD       XCNDIS        XGNDIS
     C                   ADD       XCMRA         XGMRA
     C                   ADD       XCPAID        XGPAID
     C                   ADD       XCAMTI        XGAMTI            9 2
     C*
     C                   Z-ADD     *ZERO         XXACDT
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXCLM#
     C                   Z-ADD     *ZERO         XCCNT
     C                   Z-ADD     *ZERO         XCCHRG
     C                   Z-ADD     *ZERO         XCOTHR
     C                   Z-ADD     *ZERO         XCOTH2
     C                   Z-ADD     *ZERO         XCNDIS
     C                   Z-ADD     *ZERO         XCMRA
     C                   Z-ADD     *ZERO         XCPAID
     C                   Z-ADD     *ZERO         XCAMTI
     C*
     C                   Z-ADD     ARCASE        HLDCSE
     C*
     C     CMKY          CHAIN     PCM01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVEL     CMSS#         ZZSS#
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     ZZSS1         XXSS1
     C***                MOVEL     ZZSS2         XXSS2
     C***                MOVEL     ZZSS3         XXSS3
      /Free
       xxSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   MOVEL     CMCCL#        XXCLM#           13
     C                   END
     C*
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      '0'
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM            1
     C*
     C                   MOVEL     SRLNAM        XXNAME           25
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  TOTAL LINE FOR FUND                             S R T F N D  *
     C*****************************************************************
     C     SRTFND        BEGSR
     C                   EXCEPT    TOTFND
     C*
     C                   ADD       XGCNT         XTCNT             6 0          ADD 08/20/93
     C                   ADD       XGAMTI        XTAMTI            9 2
     C                   ADD       XGCHRG        XTCHRG
     C                   ADD       XGOTHR        XTOTHR
     C                   ADD       XGOTH2        XTOTH2
     C                   ADD       XGNDIS        XTNDIS
     C                   ADD       XGMRA         XTMRA
     C                   ADD       XGPAID        XTPAID
     C*
     C                   Z-ADD     *ZERO         XGCNT
     C                   Z-ADD     *ZERO         XGCHRG
     C                   Z-ADD     *ZERO         XGOTHR
     C                   Z-ADD     *ZERO         XGOTH2
     C                   Z-ADD     *ZERO         XGNDIS
     C                   Z-ADD     *ZERO         XGMRA
     C                   Z-ADD     *ZERO         XGPAID
     C                   Z-ADD     *ZERO         XGAMTI
     C*
     C                   Z-ADD     ARFND         HLDFND
     C*
     C     FNKY          CHAIN     PFN01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNME
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNME
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  FINAL TOTAL - REPORT SUMMARY                    S R T O T L  *
     C*****************************************************************
     C     SRTOTL        BEGSR
     C*
     C*  CALCULATE/PRINT SUMMARY PAGE TOTALS
     C*
     C     XTOTHR        ADD       XTOTH2        XTAMTS
     C                   ADD       XTNDIS        XTAMTS
     C                   ADD       XTMRA         XTAMTS
     C*
     C     XTCHRG        IFNE      *ZERO
     C     XTOTHR        DIV(H)    XTCHRG        XXPERS            5 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPERS            5 3
     C                   END
     C     XXPERS        MULT      100           XXPERS
     C                   Z-ADD     XXPERS        XXPOTH            3 1
     C*
     C     XTCHRG        IFNE      *ZERO
     C     XTOTH2        DIV(H)    XTCHRG        XXPERS            5 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPERS            5 3
     C                   END
     C     XXPERS        MULT      100           XXPERS
     C                   Z-ADD     XXPERS        XXPOT2            3 1
     C*
     C     XTCHRG        IFNE      *ZERO
     C     XTNDIS        DIV(H)    XTCHRG        XXPERS            5 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPERS            5 3
     C                   END
     C     XXPERS        MULT      100           XXPERS
     C                   Z-ADD     XXPERS        XXPNDS            3 1
     C*
     C     XTCHRG        IFNE      *ZERO
     C     XTMRA         DIV(H)    XTCHRG        XXPERS            5 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPERS            5 3
     C                   END
     C     XXPERS        MULT      100           XXPERS
     C                   Z-ADD     XXPERS        XXPMRA            3 1
     C*
     C     XTCHRG        IFNE      *ZERO
     C     XTAMTS        DIV(H)    XTCHRG        XXPERS            5 3
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPERS            5 3
     C                   END
     C     XXPERS        MULT      100           XXPERS
     C                   Z-ADD     XXPERS        XXPSAV            3 1
     C*
     C     XTAMTI        IFNE      *ZERO
     C     XTAMTS        DIV(H)    XTAMTI        XXROIA            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         XXROIA            4 2
     C                   END
     C*
     C     XTAMTS        SUB       XTAMTI        XXNETS           11 2
     C     XTCNT         IFNE      *ZERO
     C     XTAMTI        DIV(H)    XTCNT         XXAVBR            9 2
     C     XXNETS        DIV(H)    XTCNT         XXAVSV            9 2
     C                   ELSE
     C                   Z-ADD     *ZERO         XXAVBR            9 2
     C                   Z-ADD     *ZERO         XXAVSV            9 2
     C                   END
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C*
     C                   Z-ADD     1             XXINV#
     C*
     C     ARKY2         SETLL     PAR02
     C                   READ      PAR02                                  70
     C*
     C     *IN70         DOWEQ     '0'
     C*
     C*** CONVERT FROM AND TO DATES AND COMPARE
     C*
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C     ARTRDT        CABLT     XXFDT         READNW
     C     ARTRDT        CABGT     XXTDT         READNW
     C*
     C     XXONCE        IFEQ      ' '
     C                   MOVE      'X'           XXONCE            1
     C                   Z-ADD     ARINV#        XAINV#            9 0
     C                   ADD       1             INVTL
     C                   ENDIF
     C*
     C     ARINV#        IFNE      XAINV#
     C     ARSUB#        ANDEQ     XXSUB#
     C                   Z-ADD     ARINV#        XAINV#
     C                   ADD       1             INVTL
     C                   ENDIF
     C*
     C     READNW        TAG
     C*
     C                   READ      PAR02                                  70
     C*
     C                   ENDDO
     C                   EXCEPT    LRTOTL
     C                   ENDSR
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********             Q1USER              27
     O**********                                 29 '-'
     O**********             UPUSNM              80
     O**********E            ROUTE          1
     O**********E            ROUTE          1
     O**********E            ROUTE          2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       Q1USER              27
     O                                           29 '-'
     O                       UPUSNM              80
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              96
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           31 'SUBSCRIBER......:'
     O                       XXSUB#        4     38
     O                                           40 '-'
     O                       XXSBNM            +002
     O          E            ROUTE          2
     O                                           31 'DATE RANGE:.....:'
     O                       XXFRDT              43 '0  /  /    '
     O                                           46 'TO'
     O                       XXTODT              57 '0  /  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               62 'INC. 2001          '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD1          1 06
     O                       DSDATE              10 '  /  /    '
     O                       XXTITL             129
     O                       DSTIME             198 '  :  :  '
     O          E            HEAD1          2
     O                                           11 'F7113R     '
     O                       XXCONM             119
     O                                          193 'PAGE:'
     O                       PAGE               198
     O          E            HEAD1          2
     O                       XXFRDT              97 '0  /  /    '
     O                                           99 '-'
     O                       XXTODT             110 '0  /  /    '
     O          E            ERRMSG         2
     O                                           17 '*** NO BILLS MEET'
     O                                           36 'SELECTION CRITERIA'
     O          E            HEAD2          1
     O                                           12 'SUBSCRIBER:'
     O                       XXSUB#        4     17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD2          2
     O                                           12 'GROUP . . :'
     O                       HLDFND        4     17
     O                                           19 '-'
     O                       XXFNME              60
     O          E            HEAD2          1
     O                                           23 ' ACCIDENT '
     O                                         +002 ' # OF'
     O          E            HEAD2          2
     O                                           11 '  SOC SEC #'
     O                                         +002 '   DATE   '
     O                                         +002 'BILLS'
     O                                         +002 '   CASE'
     O                                         +002 'CLIENT NAME'
     O                                         +015 ' CLAIM NUMBER'
     O                                         +002 '        CHARGED'
     O                                         +002 '          OTHER'
     O                                         +002 '        OTHER 2'
     O                                         +002 '        NETWORK'
     O                                         +002 '            MRA'
     O                                         +002 '           PAID'
     O                                         +002 ' REVIEWER FEE'
     O          E            HEAD3          1
     O                                           12 'SUBSCRIBER:'
     O                       XXSUB#        4     17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD3       1  2
     O                                           21 'ALL GROUP TOTALS'
     O          E            TOTCSE         1
     O                       XXSS#               11
     O                       XXACDT            +002 '  /  /    '
     O                       XCCNT         Z   +001
     O                       XXCASE        Z   +002
     O                       XXNAME            +002
     O                       XXCLM#            +002
     O                       XCCHRG        J   +002
     O                       XCOTHR        J   +002
     O                       XCOTH2        J   +002
     O                       XCNDIS        J   +002
     O                       XCMRA         J   +002
     O                       XCPAID        J   +002
     O                       XCAMTI        J   +002
     O          E            TOTFND      2
     O                                            6 'GROUP'
     O                       HLDFND        4     10
     O                                           18 'TOTALS:'
     O                       XGCNT         Z     30
     O                       XGCHRG        J   +053
     O                       XGOTHR        J   +002
     O                       XGOTH2        J   +002
     O                       XGNDIS        J   +002
     O                       XGMRA         J   +002
     O                       XGPAID        J   +002
     O                       XGAMTI        J   +002
     O          E            LRTOTL         2
     O                                           64 'TOTAL NUMBER OF BILLS:'
     O                       XTCNT         1     80
     O          E            LRTOTL         2
     O                                           55 'TOTAL NUMBER OF '
     O                                           64 'INVOICES:'
     O                       INVTL         1     80
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT CHARGED:'
     O                       XTCHRG        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT OTHER:'
     O                       XTOTHR        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT OTHER 2:'
     O                       XTOTH2        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT NETWORK:'
     O                       XTNDIS        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT MRA:'
     O                       XTMRA         J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL AMOUNT PAID:'
     O                       XTPAID        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'TOTAL REVIEWER FEE:'
     O                       XTAMTI        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'NET SAVINGS:'
     O                       XXNETS        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'PERCENT SAVINGS:'
     O                       XXPSAV        J     77
     O                                           81 '%'
     O          E            LRTOTL         2
     O                                           64 'AVERAGE BILLING RATE:'
     O                       XXAVBR        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'AVERAGE SAVINGS:'
     O                       XXAVSV        J     81 '$'
     O          E            LRTOTL         2
     O                                           64 'RETURN ON INVESTMENT:'
     O                       XXROIA        J     81
     O                                           73 '1:'
