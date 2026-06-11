     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  F4100R - FSMRA PROCESSING                     C:79 07/21/02  *
     H*                                                               *
     H*  CALLED FROM: FA735R                                          *
     H*  CALLS TO: NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 05/30/03 C:79 Project: FSMNT
     H*               Release record lock if code = FSMRA
     H*
     H* 05/22/03 C:79 Project: FS260
     H*               1.  Use logicals that have EOB# in the key
     H*               2.  Allow Dental Codes for Oklahoma
     H*
     H* 04/28/03 C:79 Project: FSMNT
     H*               Exclude HCPCS codes from OK
     H*
     H* 03/11/03 C:79 Support: MC095 36391
     H*               Change FDDHP,FDDUP to be input/add but not update
     H*
     H* 02/04/03 C:79 Support: MC071 35839
     H*               Dont create FSMRA if AL provider is excluded
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFDCAL13   UF A E           K DISK
     FFDCALF    UF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     FFDCAL19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     F*** BEG CHG *** 05/22/03 ****************************************
     F**** FDDDL1  UF  E           K        DISK                      A
     FFDDDL2    UF A E           K DISK
     F*** END CHG *** 05/22/03 ****************************************
     F*** BEG CHG *** 03/11/03 ****************************************
     F*****FDDUL1  UF  E           K        DISK                      A
     F*****FDDHL3  UF  E           K        DISK                      A
     F*** BEG CHG *** 05/22/03 ****************************************
     F**** FDDUL1  IF  E           K        DISK                      A
     FFDDUL3    IF A E           K DISK
     F*** END CHG *** 05/22/03 ****************************************
     FFDDHL3    IF A E           K DISK
     F*** END CHG *** 03/11/03 ****************************************
     FFMPRL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK
     F*** BEG ADD *** 02/04/03 ****************************************
     FFMP2LAL   IF   E           K DISK
     FFDDUL     IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     F*** END ADD *** 02/04/03 ****************************************
     F*

     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D MRAADJ          C                   CONST('MRA adjustment')

     C*
     C*
     C*  PLIST definitions
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0          FAEOP
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* KEY LISTS
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C     DDKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEG ADD *** 05/22/03 ****************************************
     C                   KFLD                    KKEOB#
     C*** END ADD *** 05/22/03 ****************************************
     C                   KFLD                    KKPRCD
     C*
     C     DUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEG ADD *** 05/22/03 ****************************************
     C                   KFLD                    KKEOB#
     C*** END ADD *** 05/22/03 ****************************************
     C                   KFLD                    K1RVCD
     C                   KFLD                    KKRATE
     C*
     C     DHKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKPRCD
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRCD
     C*
     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCMSV
     C*** BEG ADD *** 02/04/03 ****************************************
     C*
     C* Key list for Provider Auxiliary file
     C*
     C     P2KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCMSV
     C                   KFLD                    KKFDAT
     C*
     C     P2KY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCMSV
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 02/04/03 ****************************************
     C*
     C* LIKE DEFINES
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DURATE        KKRATE
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    DDPRCD        KKPRCD
     C*
     C     *LIKE         DEFINE    DURVCD        K1RVCD
     C*
     C     *LIKE         DEFINE    CACHRG        X3CHRG
     C     *LIKE         DEFINE    CAMRAA        X3MRAA
     C     *LIKE         DEFINE    CADIFF        X3DIFF
     C*
     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C*
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C*** BEG ADD *** 02/04/03 ****************************************
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C*** END ADD *** 02/04/03 ****************************************
     C*
     C* INITIALIZATION
     C*
     C                   Z-ADD     *ZEROS        HLDIFF           11 2
     C                   Z-ADD     *ZEROS        HLCHRG           11 2
     C                   Z-ADD     *ZEROS        HLPAID           11 2
     C                   Z-ADD     *ZEROS        HLTDIF           11 2
     C                   Z-ADD     *ZEROS        HLNEWP           11 2
     C*
     C                   Z-ADD     *ZEROS        X3CHRG
     C                   Z-ADD     *ZEROS        X3MRAA
     C                   Z-ADD     *ZEROS        X3DIFF
     C*
     C* LOAD KEY FIELDS FOR EOB#
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C*
     C* EXECUTE SUBROUTINE BASED ON BILL TYPE
     C*
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      CRTMED
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      CRTDEN
     C*
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      CRTHOS
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
     C*
     C*================================================================
     C* CRTMED - Calculate FSMRA adjustment for Medical Bills
     C*================================================================
     C     CRTMED        BEGSR
     C*
     C*** The following logic is for Medical/Physician bills
     C*
     C     CAKY1         CHAIN     PCA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C     CACPT4        IFNE      'FSMRA'
     C*** BEG ADD *** 04/28/03 ****************************************
     C*
     C* EXCLUDE OKLAHOMA HCPCS CODES. HCPCS ARE PAID LESSER OF CHARGED
     C* OR HCFA ALLOWANCE.  PG 327 1.
     C*
     C     EOAST         IFEQ      'OK'
     C     CACPT4        ANDGE     'A0000'
     C*** BEG CHG *** 05/22/03 ****************************************
     C********** CACPT4    ANDLE'Z9999'
     C     CACPT4        ANDLE     'C9999'
     C     EOAST         OREQ      'OK'
     C     CACPT4        ANDGE     'E0001'
     C     CACPT4        ANDLE     'Z9999'
     C*** END CHG *** 05/22/03 ****************************************
     C*
     C                   UPDATE    PCA01
     C     CAKY1         READE     PCA01                                  99
     C*
     C                   ELSE
     C*** END ADD *** 04/28/03 ****************************************
     C                   Z-ADD     CAMRAA        X3MRAA
     C                   Z-ADD     CACHRG        X3CHRG
     C     X3MRAA        SUB       X3CHRG        X3DIFF
     C*
     C     X3DIFF        IFGT      0
     C                   Z-ADD     X3DIFF        CADIFF
     C                   ADD       CADIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             CADIFF
     C*
     C                   ENDIF                                                  CADIFF=0
     C*
     C                   ADD       CACHRG        HLCHRG
     C                   ADD       CAPAID        HLPAID
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C     CAKY1         READE     PCA01                                  99
     C*** BEG ADD *** 04/28/03 ****************************************
     C*
     C                   ENDIF
     C*** END ADD *** 04/28/03 ****************************************
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*
     C*** BEG ADD *** 05/30/03 ****************************************
     C                   UPDATE    PCA01
     C*** END ADD *** 05/30/03 ****************************************
     C*
     C     CAKY1         READE     PCA01                                  99
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     HLCHRG        SUB       HLPAID        HLTDIF
     C*
     C     HLDIFF        IFLE      HLTDIF
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     HLTDIF        HLNEWP
     C                   ENDIF                                                  DIFF<TDIF
     C*
     C     HLNEWP        IFGT      *ZERO
     C     HLNEWP        OREQ      *ZERO
     C     *IN55         ANDEQ     *ON
     C*
     C                   EXSR      WRTMED
     C*
     C                   ENDIF                                                  HLNEWP>0
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* CRTDEN - Calculate FSMRA adjustment for Dental Bills
     C*================================================================
     C     CRTDEN        BEGSR
     C*
     C*** The following logic is for Dental bills
     C*
     C     CAKY1         CHAIN     PCA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C     CACPT4        IFNE      'FSMRA'
     C*** BEG ADD *** 04/28/03 ****************************************
     C*
     C* EXCLUDE OKLAHOMA HCPCS CODES. HCPCS ARE PAID LESSER OF CHARGED
     C* OR HCFA ALLOWANCE.  PG 327 1.
     C*
     C     EOAST         IFEQ      'OK'
     C     CACPT4        ANDGE     'A0000'
     C*** BEG CHG *** 05/22/03 ****************************************
     C********** CACPT4    ANDLE'Z9999'
     C     CACPT4        ANDLE     'C9999'
     C     EOAST         OREQ      'OK'
     C     CACPT4        ANDGE     'E0001'
     C     CACPT4        ANDLE     'Z9999'
     C*** END CHG *** 05/22/03 ****************************************
     C*
     C                   UPDATE    PCA01
     C     CAKY1         READE     PCA01                                  99
     C*
     C                   ELSE
     C*** END ADD *** 04/28/03 ****************************************
     C                   Z-ADD     CAMRAA        X3MRAA
     C                   Z-ADD     CACHRG        X3CHRG
     C     X3MRAA        SUB       X3CHRG        X3DIFF
     C*
     C     X3DIFF        IFGT      0
     C                   Z-ADD     X3DIFF        CADIFF
     C                   ADD       CADIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             CADIFF
     C*
     C                   ENDIF                                                  CADIFF=0
     C*
     C                   ADD       CACHRG        HLCHRG
     C                   ADD       CAPAID        HLPAID
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C     CAKY1         READE     PCA01                                  99
     C*** BEG ADD *** 05/15/02 ****************************************
     C*
     C                   ENDIF
     C*** END ADD *** 05/15/02 ****************************************
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*** BEG ADD *** 05/30/03 ****************************************
     C                   UPDATE    PCA01
     C*** END ADD *** 05/30/03 ****************************************
     C*
     C     CAKY1         READE     PCA01                                  99
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     HLCHRG        SUB       HLPAID        HLTDIF
     C*
     C     HLDIFF        IFLE      HLTDIF
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     HLTDIF        HLNEWP
     C                   ENDIF                                                  DIFF<TDIF
     C*
     C     HLNEWP        IFGT      *ZERO
     C     HLNEWP        OREQ      *ZERO
     C     *IN55         ANDEQ     *ON
     C*
     C                   EXSR      WRTDEN
     C*
     C                   ENDIF                                                  HLNEWP>0
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* CRTHOS - Calculate FSMRA adjustment for Hosptial Bills
     C*================================================================
     C     CRTHOS        BEGSR
     C*
     C*** The following logic is for Hospital bills
     C*
     C     CAKY1         CHAIN     PCA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEG ADD *** 02/04/03 ****************************************
     C*
     C* Alabama Hospitals flagged with an '*' are excluded from FSMRA
     C*
     C     EOAST         IFEQ      'AL'
     C*
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   MOVE      'N'           XXSKIP            1
     C*
     C     DUKY          CHAIN     PDU02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      DURVCD        KKRVCD
     C                   MOVE      DURATE        KKHCPC
     C                   MOVE      *ZEROS        KKUNTS
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   Z-ADD     CAFEIN        KKFEIN
     C                   Z-ADD     CAMOD         KKMOD
     C*
     C                   ENDIF
     C*
     C     P2KY          SETGT     PP201
     C     P2KY1         READPE    PP201                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     P2FL02        ANDEQ     '*'
     C*
     C                   MOVE      'Y'           XXSKIP
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  EOAST = AL
     C*
     C*** END ADD *** 02/04/03 ****************************************
     C     CACPT4        IFNE      'FSMRA'
     C*** BEG ADD *** 05/30/02 ****************************************
     C     XXSKIP        IFNE      'Y'
     C*** END ADD *** 05/30/02 ****************************************
     C                   Z-ADD     CAMRAA        X3MRAA
     C                   Z-ADD     CACHRG        X3CHRG
     C*
     C     X3MRAA        SUB       X3CHRG        X3DIFF
     C*
     C     EOAST         IFEQ      'AL'
     C                   ADD       CADIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C     X3DIFF        IFGT      0
     C                   Z-ADD     X3DIFF        CADIFF
     C                   ADD       CADIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             CADIFF
     C                   ENDIF                                                  CADIFF=0
     C                   ENDIF                                                  EOAST = AL
     C*
     C                   ADD       CACHRG        HLCHRG
     C                   ADD       CAPAID        HLPAID
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*** BEG ADD *** 05/30/02 ****************************************
     C                   ENDIF                                                  XXSKIP <> Y
     C*** END ADD *** 05/30/02 ****************************************
     C*
     C                   UPDATE    PCA01
     C     CAKY1         READE     PCA01                                  99
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*** BEG ADD *** 05/30/03 ****************************************
     C                   UPDATE    PCA01
     C*** END ADD *** 05/30/03 ****************************************
     C*
     C     CAKY1         READE     PCA01                                  99
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     HLCHRG        SUB       HLPAID        HLTDIF
     C*
     C     EOAST         IFEQ      'AL'
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C     HLDIFF        IFLE      HLTDIF
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     HLTDIF        HLNEWP
     C                   ENDIF                                                  DIFF<TDIF
     C*
     C                   ENDIF                                                  EOAST = AL
     C*
     C     HLNEWP        IFGT      *ZERO
     C     HLNEWP        OREQ      *ZERO
     C     *IN55         ANDEQ     *ON
     C*
     C                   EXSR      WRTHOS
     C*
     C                   ENDIF                                                  HLNEWP>0
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDEN - Writes dental FSMRA record to FDCAP, FDDAP, & FDDDP
     C*================================================================
     C     WRTDEN        BEGSR
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     CAKY2         SETGT     PCA0F
     C     CAKY3         READPE(N) PCA0F                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY4         SETGT     PCA19
     C     CAKY1         READPE(N) PCA19                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C*  If you are on a recon, it checks to see if there is all ready
     C*  an FSMRA record in FDCAP. If not, then it will write an FSMRA
     C*  record in FDCAP, otherwise, it will update the existing FSMRA
     C*  record.
     C*
     C     EORCN         IFGT      *ZERO
     C*
     C     DDKY1         CHAIN     PDD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DDEOB#        KKEOB#
     C                   Z-ADD     DDDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CACHRG
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   MOVE      'X'           CADELF
     C*
     C                   UPDATE    PCA0F
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*  Update FDDDP
     C*
     C     DDKY1         CHAIN     PDD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DDCO#
     C                   Z-ADD     EOSUB#        DDSUB#
     C                   Z-ADD     EOFND         DDFND
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   MOVEL     CASS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   Z-ADD     XXDSEQ        DDDSEQ
     C                   Z-ADD     EOEOB#        DDEOB#
     C                   MOVEL     'FSMRA'       DDPRCD
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DDPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DDPRDS
     C*
     C                   ENDIF
     C                   MOVEL     '11'          DDPOS
     C                   Z-ADD     *ZEROS        DDUNTS
     C                   MOVEL     *BLANKS       DDPART
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     Q1PGM         DDIPGM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSTIME        DDITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDIDT
     C                   MOVEL     'C'           DDRCFL
     C                   MOVEL     *BLANKS       DDBPRC
     C                   MOVEL     *BLANKS       DDBPRD
     C*
     C                   WRITE     PDD01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN>0
     C*
     C*  If you are NOT on a recon, then an FSMRA record gets written
     C*  to FDCAP and FDDDP.
     C*
     C     EORCN         IFEQ      0
     C*
     C     CAKY1         CHAIN(N)  PCA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Update FDDDP
     C*
     C     DDKY1         CHAIN     PDD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DDCO#
     C                   Z-ADD     EOSUB#        DDSUB#
     C                   Z-ADD     EOFND         DDFND
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   MOVEL     CASS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   Z-ADD     CADSEQ        DDDSEQ
     C                   Z-ADD     EOEOB#        DDEOB#
     C                   MOVEL     'FSMRA'       DDPRCD
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DDPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DDPRDS
     C*
     C                   ENDIF
     C                   MOVEL     '11'          DDPOS
     C                   Z-ADD     *ZEROS        DDUNTS
     C                   MOVEL     *BLANKS       DDPART
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     Q1PGM         DDIPGM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSTIME        DDITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDIDT
     C                   MOVEL     'C'           DDRCFL
     C                   MOVEL     *BLANKS       DDBPRC
     C                   MOVEL     *BLANKS       DDBPRD
     C*
     C                   WRITE     PDD01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHOS - Writes hospital FSMRA record to FDCAP, FDDAP, & FDDUP
     C*================================================================
     C     WRTHOS        BEGSR
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     CAKY2         SETGT     PCA0F
     C     CAKY3         READPE(N) PCA0F                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY4         SETGT     PCA19
     C     CAKY1         READPE(N) PCA19                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   MOVEL     'FSMRA'       KKRATE
     C                   MOVE      *ZEROS        K1RVCD
     C*
     C*  If you are on a recon, it checks to see if there is all ready
     C*  an FSMRA record in FDCAP. If not, then it will write an FSMRA
     C*  record in FDCAP, otherwise, it will update the existing FSMRA
     C*  record.
     C*
     C     EORCN         IFGT      *ZERO
     C*
     C     DUKY1         CHAIN     PDU01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DUEOB#        KKEOB#
     C                   Z-ADD     DUDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'S'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CACHRG
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   UPDATE    PCA0F
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'S'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*  Update FDDUP
     C*
     C     DUKY1         CHAIN     PDU01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     CADSEQ        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C                   MOVE      '11'          DUPOS
     C                   Z-ADD     *ZEROS        DURVCD
     C                   MOVEL     'FSMRA'       DURATE
     C*
     C* Get description from serv code file or default to MRAADJ const.
     C*
     C                   CLEAR                   KKCMSV
     C                   MOVE      '000'         KKRVCD
     C                   MOVE      '000'         KKUNTS
     C                   MOVEL     'FSMRA'       KKHCPC
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY          READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        DUDESC
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DUDESC
     C*
     C                   ENDIF
     C                   Z-ADD     1             DUUNTS
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C                   MOVEL     *BLANKS       DUBRAT
     C                   MOVEL     *BLANKS       DUBPRC
     C                   MOVEL     *BLANKS       DUBPRD
     C*
     C                   WRITE     PDU01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN>0
     C*
     C*  If you are NOT on a recon, then an FSMRA record gets written
     C*  to FDCAP and FDDUP.
     C*
     C     EORCN         IFEQ      0
     C*
     C     CAKY1         CHAIN(N)  PCA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'S'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Update FDDUP
     C*
     C     DUKY1         CHAIN     PDU01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     CADSEQ        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C                   MOVE      '11'          DUPOS
     C                   Z-ADD     *ZEROS        DURVCD
     C                   MOVEL     'FSMRA'       DURATE
     C*
     C* Get description from serv code file or default to MRAADJ const.
     C*
     C                   CLEAR                   KKCMSV
     C                   MOVE      '000'         KKRVCD
     C                   MOVE      '000'         KKUNTS
     C                   MOVEL     'FSMRA'       KKHCPC
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY          READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        DUDESC
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DUDESC
     C*
     C                   ENDIF
     C                   Z-ADD     1             DUUNTS
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C                   MOVEL     *BLANKS       DUBRAT
     C                   MOVEL     *BLANKS       DUBPRC
     C                   MOVEL     *BLANKS       DUBPRD
     C*
     C                   WRITE     PDU01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDSR
     C*================================================================
     C* WRTMED - Writes medical FSMRA record to FDCAP, FDDAP, & FDDHP
     C*================================================================
     C     WRTMED        BEGSR
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     CAKY2         SETGT     PCA0F
     C     CAKY3         READPE(N) PCA0F                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY4         SETGT     PCA19
     C     CAKY1         READPE(N) PCA19                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C*  If you are on a recon, it checks to see if there is all ready
     C*  an FSMRA record in FDCAP. If not, then it will write an FSMRA
     C*  record in FDCAP, otherwise, it will update the existing FSMRA
     C*  record.
     C*
     C     EORCN         IFGT      *ZERO
     C*
     C     DHKY1         CHAIN     PDH01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DHEOB#        KKEOB#
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CACHRG
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   UPDATE    PCA0F
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C*
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*
     C*  Write FDDHP
     C     DHKY1         CHAIN     PDH01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     EOCO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   MOVEL     CASS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   Z-ADD     XXDSEQ        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVE      '11'          DHPOS
     C                   MOVEL     *BLANKS       DHTOS
     C                   MOVEL     'FSMRA'       DHPRCD
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DHPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DHPRDS
     C*
     C                   ENDIF
     C                   MOVEL     *BLANKS       DHMOD1
     C                   MOVEL     *BLANKS       DHMOD2
     C                   MOVEL     *BLANKS       DHMOD3
     C                   MOVEL     *BLANKS       DHMOD4
     C*
     C                   MOVEL     'X'           DHDXR1
     C                   MOVEL     *BLANKS       DHDXR2
     C                   MOVEL     *BLANKS       DHDXR3
     C                   MOVEL     *BLANKS       DHDXR4
     C                   Z-ADD     1             DHUNTS
     C                   MOVEL     *BLANKS       DHAREA
     C                   MOVEL     *BLANKS       DHPPLN
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHIDT
     C                   MOVEL     'C'           DHRCFL
     C                   MOVEL     *BLANKS       DHBPRC
     C                   MOVEL     *BLANKS       DHBPRD
     C*
     C                   WRITE     PDH01
     C*
     C                   ENDIF                                                  IN60=ON
     C*
     C*
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C*
     C*  If you are NOT on a recon, then an FSMRA record gets written
     C*  to FDCAP and FDDHP.
     C*
     C     EORCN         IFEQ      0
     C*
     C     CAKY1         CHAIN(N)  PCA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     EOFRDT        CAFDAT
     C                   Z-ADD     EOTODT        CATDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     'FSMRA'       CACPT4
     C                   MOVEL     *BLANKS       CAFLAG
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     *BLANKS       CADXR
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-ADD     *ZERO         CAOTHR
     C                   Z-ADD     *ZERO         CAOTH2
     C                   Z-ADD     *ZERO         CANDIS
     C                   Z-ADD     HLNEWP        CAPAID
     C                   Z-ADD     *ZEROS        CACHRG
     C     CACHRG        SUB       CAPAID        CAMRA
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL     *BLANKS       CARULE
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     *ZEROS        CAMRAA
     C                   Z-ADD     *ZEROS        CAMNAA
     C                   MOVEL     *BLANKS       CAADJ
     C                   MOVEL     *BLANKS       CABPRC
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Update FDDHP
     C*
     C     DHKY1         CHAIN     PDH01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   MOVEL     CASS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   Z-ADD     CADSEQ        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C*
     C                   MOVE      '11'          DHPOS
     C                   MOVEL     *BLANKS       DHTOS
     C                   MOVEL     'FSMRA'       DHPRCD
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DHPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DHPRDS
     C*
     C                   ENDIF
     C                   MOVEL     *BLANKS       DHMOD1
     C                   MOVEL     *BLANKS       DHMOD2
     C                   MOVEL     *BLANKS       DHMOD3
     C                   MOVEL     *BLANKS       DHMOD4
     C*
     C                   MOVEL     'X'           DHDXR1
     C                   MOVEL     *BLANKS       DHDXR2
     C                   MOVEL     *BLANKS       DHDXR3
     C                   MOVEL     *BLANKS       DHDXR4
     C                   Z-ADD     1             DHUNTS
     C                   MOVEL     *BLANKS       DHAREA
     C                   MOVEL     *BLANKS       DHPPLN
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHIDT
     C                   MOVEL     'C'           DHRCFL
     C                   MOVEL     *BLANKS       DHBPRC
     C                   MOVEL     *BLANKS       DHBPRD
     C*
     C                   WRITE     PDH01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDSR
