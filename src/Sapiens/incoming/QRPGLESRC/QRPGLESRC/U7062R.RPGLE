     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7062R - CPEATE PROCEDURE/DIAGNOSIS ANALYSIS REPORT WORKFILE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*  PGM CREATES THE PROCEDURE/DIAGNOSIS ANALYSIS WORKFILE WHICH  *CMD
     H*  WILL BE USED IN THE PRINT PGM                                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    U7060C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C20                PROJECT: UR002                *
     H*  CREATED: 07/15/93                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  60           GENERAL CHAIN                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Print ICD10 is ICD10 is active else Print ICD9*
     H*                                                               *
     H* 01/23/13 C:287 Project: SY266_QRY                             *
     H*                Add DFTACTGRP and ACTGRP compile options       *
     H* 03/14/00 C:62 PROJECT:URMNT                                   *
     H*               PRE-CERT PROCEDURE CODES                        *
     H*                                                               *
     H*********************************************************************
     FUMPCL1    IF   E           K DISK
     FUXPDP     O    E           K DISK
     FFMCML1    IF   E           K DISK
     FFAEOL1    IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FUMCHL1    IF   E           K DISK
     F*** BEGIN ADD *** 03/14/00 **************************************
     FUDPOL     IF   E           K DISK
     F*** END   ADD *** 03/14/00 **************************************

     D*
     D CODE            S              5    DIM(999)                             CPT4/ICD9 CODE
     D*** Beg Chg *** 08/05/13 ****************************************
     D CODE10          S              7    DIM(999)                             CPT4/ICD10 CODE
     D*** End Chg *** 08/05/13 ****************************************
     D TACT            S              5  0 DIM(999)                             TOTAL ACTIVITY
     D ACRT            S              5  0 DIM(999)                             APPR # CERTS
     D REQ             S              5  0 DIM(999)                             TOTAL REQ DAYS
     D REC             S              5  0 DIM(999)                             TOTAL REC DAYS
     D ASAV            S              5  0 DIM(999)                             TOTAL APPR SAV DAYS
     D DCRT            S              5  0 DIM(999)                             DENY # CERTS
     D DSAV            S              5  0 DIM(999)                             TOTAL DENY SAV DAYS
     D TSAV            S              5  0 DIM(999)                             TOTAL ALL SAV DAYS
     D EMP             S              9  0 DIM(999)                             EMPLOYER NUMBER
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ATYP                   1      5
     D                                     DIM(5)                               ADMISSION TYPE
     D  XXATP1                 1      1
     D  XXATP2                 2      2
     D  XXATP3                 3      3
     D  XXATP4                 4      4
     D  XXATP5                 5      5
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C*   KEY LIST FOR THE EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    EOEOB#
     C*
     C*   PRE-CERT HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    PCCERT
     C*** BEGIN ADD *** 03/14/00 **************************************
     C*
     C*   PRE-CERT PROCEDURE CODE FILE
     C*
     C     POKY          KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    PCCERT
     C*** END   ADD *** 03/14/00 **************************************
     C*
     C     *LIKE         DEFINE    CHDT          XXFRDT
     C     *LIKE         DEFINE    CHDT          XXTODT
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQATP1            1
     C                   PARM                    QQATP2            1
     C                   PARM                    QQATP3            1
     C                   PARM                    QQATP4            1
     C                   PARM                    QQATP5            1
     C                   PARM                    QQSEL             1
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PCMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    CATDAT        XXTDAT                         6.0N
     C     *LIKE         DEFINE    CAFDAT        XXFDAT                         6.0N
     C                   Z-ADD     *ZEROS        XXEMP#                         9.0N
     C                   Z-ADD     *ZEROS        XXTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXDIFF            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXTACD            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXFRDT                         7.0N
     C                   Z-ADD     *ZEROS        XXTODT                         7.0N
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQATP1        XXATP1
     C                   MOVE      QQATP2        XXATP2
     C                   MOVE      QQATP3        XXATP3
     C                   MOVE      QQATP4        XXATP4
     C                   MOVE      QQATP5        XXATP5
     C*
     C     XXATP1        IFEQ      *BLANKS
     C     XXATP2        ANDEQ     *BLANKS
     C     XXATP3        ANDEQ     *BLANKS
     C     XXATP4        ANDEQ     *BLANKS
     C     XXATP5        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C                   MOVE      '0'           *IN90
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C* READ PRE-CERT FILE RECORDS
     C*
     C                   READ      PPC01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     PCLIAB        IFNE      'P'
     C*
     C     *IN90         IFEQ      '0'
     C     PCTYAD        IFNE      *BLANKS
     C                   Z-ADD     1             Z
     C     PCTYAD        LOOKUP    ATYP(Z)                                80
     C     *IN80         IFEQ      '0'
     C                   GOTO      NXTPC
     C                   END
     C                   ELSE
     C                   GOTO      NXTPC
     C                   END
     C                   END
     C*
     C* CHAIN TO PRE-CERT CASE HISTORY FILE TO LOOK AT CLOSING DATE
     C*
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         IFEQ      '0'
     C     CHEFDT        IFGE      XXFRDT
     C     CHEFDT        ANDLE     XXTODT
     C*
     C* CHAIN TO CLAIM FILE TO CHECK EMPLOYER
     C*
     C     XXEMP#        IFNE      *ZEROS
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C     CMEMP#        IFEQ      XXEMP#
     C                   EXSR      WRTWK
     C                   END                                                    END CMEMP#
     C                   END                                                    END *IN60
     C                   ELSE
     C     CMKEY         CHAIN     PCM01                              60
     C                   EXSR      WRTWK
     C                   END                                                    END XXEMP#
     C*
     C                   END                                                    END CHEFDT
     C                   END                                                    END *IN60
     C                   END                                                    END PCLIAB
     C*
     C* READ NEXT PRE-CERT FILE RECORD
     C*
     C     NXTPC         TAG
     C                   READ      PPC01                                  50
     C                   END
     C*
     C* MOVE ARRAY TOTALS TO WORKFILE FIELDS FOR OUTPUT
     C*
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANKS       LOOKUP    CODE(Z)                                75
     C     Z             IFNE      1
     C     Z             SUB       1             Z
     C                   DO        Z             Q                 5 0
     C     QQSEL         IFEQ      'P'
     C                   MOVE      CODE(Q)       PDCPT4
     C                   MOVE      *BLANKS       PDICD9
     C                   ELSE
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        PCicdInd = 'Y'
     C                   Movel     Code10(Q)     PDicd10
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      CODE(Q)       PDICD9
     C*** Beg Add *** 08/05/13 ****************************************
     C                   Endif
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       PDCPT4
     C                   END
     C                   Z-ADD     TACT(Q)       PDTACT
     C                   Z-ADD     ACRT(Q)       PDAPPR
     C                   Z-ADD     DCRT(Q)       PDDENY
     C                   Z-ADD     REQ(Q)        PDREQD
     C                   Z-ADD     REC(Q)        PDRECD
     C                   Z-ADD     ASAV(Q)       PDASAV
     C                   Z-ADD     DSAV(Q)       PDDSAV
     C                   Z-ADD     TSAV(Q)       PDTSAV
     C                   Z-ADD     XXCO#         PDCO#
     C                   Z-ADD     XXSUB#        PDSUB#
     C                   Z-ADD     XXFND         PDFND
     C                   Z-ADD     XXFEIN        PDFEIN
     C                   Z-ADD     XXMOD         PDMOD
     C                   Z-ADD     EMP(Q)        PDEMP#
     C                   WRITE     PPD01
     C                   END
     C                   END
     C*
     C*********************************************
     C*  END THE PROGRAM                          *
     C*********************************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* WRTWK - SUBROUTINE TO WRITE RECORDS TO WORKFILE FOR PRTDOC
     C*****************************************************************
     C     WRTWK         BEGSR
     C*
     C* CHECK FOR SELECTION TYPE (P = PROCEDURE  OR  D = DIAGNOSIS)
     C*
     C     QQSEL         IFEQ      'P'
     C*
     C*** BEGIN DEL *** 03/14/00 **************************************
     C********** PCCPT1    IFEQ *BLANKS
     C**********           GOTO NXTRCD
     C**********           END
     C*** END   DEL *** 03/14/00 **************************************
     C*** BEGIN ADD *** 03/14/00 **************************************
     C     POKY          CHAIN     PPO01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** END   ADD *** 03/14/00 **************************************
     C*
     C                   Z-ADD     1             X                 5 0
     C     NXTLOK        TAG
     C*** BEGIN CHG *** 03/14/00 **************************************
     C********** PCCPT1    LOKUPCODE,X                   70
     C     POPROC        LOOKUP    CODE(X)                                70
     C*** END   CHG *** 03/14/00 **************************************
     C     *IN70         IFEQ      '1'
     C     CMEMP#        IFNE      EMP(X)
     C                   ADD       1             X
     C                   GOTO      NXTLOK
     C                   END
     C*
     C                   ADD       1             TACT(X)
     C                   Z-ADD     CMEMP#        EMP(X)
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             ACRT(X)
     C                   ADD       PCREQD        REQ(X)
     C                   ADD       PCRECD        REC(X)
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        XXTACD
     C     EOKEY         CHAIN     PEO01                              40
     C     *IN40         DOWEQ     '0'
     C     EOCERT        IFEQ      PCCERT
     C*
     C                   Z-ADD     0             XXTDAT
     C                   Z-ADD     0             XXFDAT
     C     CAKEY         CHAIN     PCA01                              30
     C     *IN30         DOWEQ     '0'
     C     CAFDAT        IFNE      XXFDAT
     C     CATDAT        ANDNE     XXTDAT
     C     CATDAT        SUB       CAFDAT        XXDIFF
     C     XXDIFF        IFEQ      0
     C                   ADD       1             XXTACD
     C                   ELSE
     C                   ADD       XXDIFF        XXTACD
     C                   END
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     CATDAT        XXTDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   END                                                    END CAFDAT
     C     CAKEY         READE     PCA01                                  30
     C                   END                                                    END *IN30
     C                   END                                                    END EOCERT
     C     EOKEY         READE     PEO01                                  40
     C                   END                                                    END *IN40
     C     PCRECD        SUB       XXTACD        XXTSAV
     C     XXTSAV        IFLT      0
     C                   Z-ADD     0             XXTSAV
     C                   END                                                    END XXTSAV
     C                   ADD       XXTSAV        ASAV(X)
     C                   ADD       XXTSAV        TSAV(X)
     C*
     C                   ELSE                                                   ELSE EQ 'A'
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             DCRT(X)
     C                   ADD       PCRECD        DSAV(X)
     C                   ADD       PCRECD        TSAV(X)
     C                   END                                                    END EQ 'D'
     C                   END                                                    END EQ 'A'
     C*
     C                   ELSE                                                   ELSE *IN70
     C*
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    CODE(X)                                75
     C     *IN75         IFEQ      '1'
     C*** BEGIN CHG *** 03/14/00 **************************************
     C**********           MOVELPCCPT1    CODE,X
     C                   MOVEL     POPROC        CODE(X)
     C*** END   CHG *** 03/14/00 **************************************
     C                   ADD       1             TACT(X)
     C                   Z-ADD     CMEMP#        EMP(X)
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             ACRT(X)
     C                   ADD       PCREQD        REQ(X)
     C                   ADD       PCRECD        REC(X)
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        XXTACD
     C     EOKEY         CHAIN     PEO01                              40
     C     *IN40         DOWEQ     '0'
     C     EOCERT        IFEQ      PCCERT
     C*
     C                   Z-ADD     0             XXTDAT
     C                   Z-ADD     0             XXFDAT
     C     CAKEY         CHAIN     PCA01                              30
     C     *IN30         DOWEQ     '0'
     C     CAFDAT        IFNE      XXFDAT
     C     CATDAT        ANDNE     XXTDAT
     C     CATDAT        SUB       CAFDAT        XXDIFF
     C     XXDIFF        IFEQ      0
     C                   ADD       1             XXTACD
     C                   ELSE
     C                   ADD       XXDIFF        XXTACD
     C                   END
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     CATDAT        XXTDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   END                                                    END CAFDAT
     C     CAKEY         READE     PCA01                                  30
     C                   END                                                    END *IN30
     C                   END                                                    END EOCERT
     C     EOKEY         READE     PEO01                                  40
     C                   END                                                    END *IN40
     C     PCRECD        SUB       XXTACD        XXTSAV
     C     XXTSAV        IFLT      0
     C                   Z-ADD     0             XXTSAV
     C                   END                                                    END XXTSAV
     C                   ADD       XXTSAV        ASAV(X)
     C                   ADD       XXTSAV        TSAV(X)
     C*
     C                   ELSE                                                   ELSE EQ 'A'
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             DCRT(X)
     C                   ADD       PCRECD        DSAV(X)
     C                   ADD       PCRECD        TSAV(X)
     C                   END                                                    END EQ 'D'
     C                   END                                                    END EQ 'A'
     C                   END                                                    END *IN75
     C*
     C                   END                                                    END *IN70
     C*** BEGIN ADD *** 03/14/00 **************************************
     C     POKY          READE     PPO01                                  99
     C*
     C                   ENDDO
     C*** END   ADD *** 03/14/00 **************************************
     C*
     C                   ELSE                                                   ELSE EQ 'P'
     C*
     C     PCICD1        IFEQ      *BLANKS
     C                   GOTO      NXTRCD
     C                   END
     C*
     C                   Z-ADD     1             X                 5 0
     C     NXTLO1        TAG
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        PCicdInd = 'Y'
     C     PCicd1001     Lookup    Code10(x)                              70
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C     PCICD1        LOOKUP    CODE(X)                                70
     C*** Beg Add *** 08/05/13 ****************************************
     C                   Endif
     C*** End Add *** 08/05/13 ****************************************
     C     *IN70         IFEQ      '1'
     C     CMEMP#        IFNE      EMP(X)
     C                   ADD       1             X
     C                   GOTO      NXTLO1
     C                   END
     C*
     C                   ADD       1             TACT(X)
     C                   Z-ADD     CMEMP#        EMP(X)
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             ACRT(X)
     C                   ADD       PCREQD        REQ(X)
     C                   ADD       PCRECD        REC(X)
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        XXTACD
     C     EOKEY         CHAIN     PEO01                              40
     C     *IN40         DOWEQ     '0'
     C     EOCERT        IFEQ      PCCERT
     C*
     C                   Z-ADD     0             XXTDAT
     C                   Z-ADD     0             XXFDAT
     C     CAKEY         CHAIN     PCA01                              30
     C     *IN30         DOWEQ     '0'
     C     CAFDAT        IFNE      XXFDAT
     C     CATDAT        ANDNE     XXTDAT
     C     CATDAT        SUB       CAFDAT        XXDIFF
     C     XXDIFF        IFEQ      0
     C                   ADD       1             XXTACD
     C                   ELSE
     C                   ADD       XXDIFF        XXTACD
     C                   END
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     CATDAT        XXTDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   END                                                    END CAFDAT
     C     CAKEY         READE     PCA01                                  30
     C                   END                                                    END *IN30
     C                   END                                                    END EOCERT
     C     EOKEY         READE     PEO01                                  40
     C                   END                                                    END *IN40
     C     PCRECD        SUB       XXTACD        XXTSAV
     C     XXTSAV        IFLT      0
     C                   Z-ADD     0             XXTSAV
     C                   END                                                    END XXTSAV
     C                   ADD       XXTSAV        ASAV(X)
     C                   ADD       XXTSAV        TSAV(X)
     C*
     C                   ELSE                                                   ELSE EQ 'A'
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             DCRT(X)
     C                   ADD       PCRECD        DSAV(X)
     C                   ADD       PCRECD        TSAV(X)
     C                   END                                                    END EQ 'D'
     C                   END                                                    END EQ 'A'
     C*
     C                   ELSE                                                   ELSE *IN70
     C*
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    CODE(X)                                75
     C     *IN75         IFEQ      '1'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        PCicdInd = 'Y'
     C                   Movel     PCicd1001     Code10(X)
     C                   Else
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     PCICD1        CODE(X)
     C*** End Add *** 08/05/13 ****************************************
     C                   Endif
     C*** End Add *** 08/05/13 ****************************************
     C                   ADD       1             TACT(X)
     C                   Z-ADD     CMEMP#        EMP(X)
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             ACRT(X)
     C                   ADD       PCREQD        REQ(X)
     C                   ADD       PCRECD        REC(X)
     C*
     C* GET THE TOTAL NUMBER OF ACTUAL DAYS FROM THE DETAIL CHARGE FILE
     C*
     C                   Z-ADD     *ZEROS        XXTACD
     C     EOKEY         CHAIN     PEO01                              40
     C     *IN40         DOWEQ     '0'
     C     EOCERT        IFEQ      PCCERT
     C*
     C                   Z-ADD     0             XXTDAT
     C                   Z-ADD     0             XXFDAT
     C     CAKEY         CHAIN     PCA01                              30
     C     *IN30         DOWEQ     '0'
     C     CAFDAT        IFNE      XXFDAT
     C     CATDAT        ANDNE     XXTDAT
     C     CATDAT        SUB       CAFDAT        XXDIFF
     C     XXDIFF        IFEQ      0
     C                   ADD       1             XXTACD
     C                   ELSE
     C                   ADD       XXDIFF        XXTACD
     C                   END
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     CATDAT        XXTDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   END                                                    END CAFDAT
     C     CAKEY         READE     PCA01                                  30
     C                   END                                                    END *IN30
     C                   END                                                    END EOCERT
     C     EOKEY         READE     PEO01                                  40
     C                   END                                                    END *IN40
     C     PCRECD        SUB       XXTACD        XXTSAV
     C     XXTSAV        IFLT      0
     C                   Z-ADD     0             XXTSAV
     C                   END                                                    END XXTSAV
     C                   ADD       XXTSAV        ASAV(X)
     C                   ADD       XXTSAV        TSAV(X)
     C*
     C                   ELSE                                                   ELSE EQ 'A'
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             DCRT(X)
     C                   ADD       PCRECD        DSAV(X)
     C                   ADD       PCRECD        TSAV(X)
     C                   END                                                    END EQ 'D'
     C                   END                                                    END EQ 'A'
     C                   END                                                    END *IN75
     C*
     C                   END                                                    END *IN70
     C*
     C     NXTRCD        TAG
     C                   END                                                    END EQ 'P'
     C*
     C                   ENDSR
