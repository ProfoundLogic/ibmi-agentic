     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* *****************************************************************
     H/TITLE F4200R - Apportionment Calculation Program
     H*                                                                 *
     H*******************************************************************
     H* Description: This program will tru to calculate the           * *
     H*              apportionment amount.  If it can't then it       * *
     H*              returns to the calling program.  If it can       * *
     H*              then it writes a new line with the code "APORT". * *
     H*                                                               * *
     H*  Called From:  FA713R - API - Bill Review and Repricing       * *
     H*                FA735R - API - CR Bill Review and Repricing    * *
     H*                                                               * *
     H*  Calls To   :  None                                           * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 03/25/2003 by c:62 for Project 129MBH              * *
     H*******************************************************************
     H* Revisions:                                                    * *
     H*                                                               * *
     H* 10/17/14 C:079 Support : MC129 87019                          *
     H*                Correct apportionment logic                    *
     H*                                                               * *
     H* 11/25/13 C:282 Project : 129WSC                               *
     H*                Code moved form Client 129 to Base.            *
     H*                                                               *
     H* 03/24/09 C:62 SUPPORT: MC129 # 67768                          *
     H*               Correct hospital process.                       *
     H*                                                               *
     H* 09/23/03 C:53 SUPPORT: WC129 # 38762                          *
     H*               Rename record format                            *
     H*                                                               * *
     H* 05/22/03 C:62 Project 129MBO                                  * *
     H*               -Add parm to indicate where Apportionment line  * *
     H*                numbering begins                               * *
     H*               -Add process to add/update FDDAP/FAEOP/FAEAP    * *
     H*                                                               * *
     H*******************************************************************
      *** Beg CHG ***  11/25/13  ****************************************
     F*****WMCMPX    IF   E           K DISK
     FWMCML1    IF   E           K DISK
      *** End CHG ***  11/25/13  ****************************************
     FFAEOP     IF   E             DISK
     FFDCAL13   IF   E           K DISK
     FFDCALF    UF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     FFDCAL19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     FFDDDL2    IF A E           K DISK
     FFDDUL3    IF A E           K DISK
     FFDDHL3    IF A E           K DISK
     FFDDPL3    IF A E           K DISK
     FFMPRL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK
     FFMRXL     IF   E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** BEGIN ADD *** 05/22/03 **************************************
     FFDDAL2    UF A E           K DISK
     FFDDAL4    IF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     FFAEAL1    UF   E           K DISK
     FFAEOL4    UF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** END   ADD *** 05/22/03 **************************************
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
     D APPADJ          C                   CONST('Apportionment-
     D                                      adjustment')
     D NOTE1           C                   CONST('Apportionment appli-
     D                                     es.  Amount allowed-
     D                                      reflects this clai-
     D                                     m''s')
     D NOTE2           C                   CONST('responsibility for')
     D CHGNUM          C                   CONST('SA020R')

     C*
     C*
     C*  PLIST definitions
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0          FAEOP
     C*** BEGIN ADD *** 05/22/03 **************************************
     C                   PARM                    @@LIN#            9 0
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      'EDITCODE'    ##MASK           10
     C                   PARM      2             ##DEC             3 0
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
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     DAKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    EORCN
     C*
     C     DAKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     EAKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     EAKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C     DDKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKPRCD
     C*
     C     DEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
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
     C     DPKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKNDC
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRCD
     C*
     C     RXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNDC
     C*
     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCMSV
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
     C*
     C* LIKE DEFINES
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFND         KKWCF#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DURATE        KKRATE
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    DDPRCD        KKPRCD
     C     *LIKE         DEFINE    DURVCD        K1RVCD
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    DPNDC         KKNDC
     C     *LIKE         DEFINE    CMAPCT        HDAPCT
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *LIKE         DEFINE    EACHRG        HDCHRG
     C     *LIKE         DEFINE    EAMRA         HDMRA
     C     *LIKE         DEFINE    EAOTHR        HDOTHR
     C     *LIKE         DEFINE    EAOTH2        HDOTH2
     C     *LIKE         DEFINE    EANDIS        HDNDIS
     C     *LIKE         DEFINE    EAPAID        HDPAID
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C* INITIALIZATION
     C*
     C*** BEGIN ADD *** 05/22/03 **************************************
     C                   SETOFF                                       16
     C     Q1PRMS        IFGT      1
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C                   Z-ADD     *ZEROS        HLPAID           11 2
     C                   Z-ADD     *ZEROS        APPAID           11 2
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
     C                   MOVE      EOCO#         $$CO#
     C                   MOVE      EOSUB#        $$SUB#
     C                   MOVE      EOFND         $$FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       $$RTCD            1
     C                   PARM                    $$CO#             3
     C                   PARM      *BLANKS       $$WCF#            3
     C                   PARM                    $$SUB#            5
     C                   PARM                    $$FND             3
     C*
     C     $$RTCD        IFEQ      *BLANKS
     C                   MOVE      $$WCF#        KKWCF#
     C*
     C* LETS GET THE APPORTIONMENT INFO
     C*
     C*** BEG CHG *** 09/23/03 ****************************
     C********** CMKY      CHAINPCM01                99
      *** Beg CHG ***  11/25/13  ******************************************
     C*****CMKY          CHAIN     PCM01X                             99
     C     CMKY          CHAIN     PCM01                              99
      *** END CHG ***  11/25/13  ******************************************
     C*** END CHG *** 09/23/03 ****************************
     C     *IN99         IFEQ      *OFF
     C     CMAEFF        ANDNE     *ZEROS
     C                   EXSR      CRTALL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*
     C                   RETURN
     C*================================================================
     C* CLCAPP - Calculate Apportionment Amount
     C*================================================================
     C     CLCAPP        BEGSR
     C*
     C     CACODE        IFNE      'APORT'
     C*** BEGIN ADD *** 03/24/09 **************************************
     C     CACODE        ANDNE     '000APORT'
     C*** END   ADD *** 03/24/09 **************************************
     C     CAFDAT        IFGE      CMAEFF
     C     100           SUB       CMAPCT        HDAPCT
     C     HDAPCT        DIV       100           XXAPCT            5 4
     C     CAPAID        MULT(H)   XXAPCT        XXPAID
     C                   ADD       XXPAID        HLPAID
     C                   ENDIF
     C                   ELSE
     C                   ADD       CAPAID        APPAID
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CRTALL - Calculate APORT adjustment for all bills
     C*================================================================
     C     CRTALL        BEGSR
     C*
     C                   SETOFF                                       55
     C*
     C* The following logic is for Medical/Dental/Hospital/Rx Bills
     C*
     C     CAKY1         CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*** Beg Del *** 10/17/14 ***************************************
     C****
     C**** CACODE        IFNE      'APORT'
     C**** CACODE        OREQ      'APORT'
     C**** CARCN         ANDNE     EORCN
     C****BEGIN ADD *** 03/24/09 **************************************
     C**** CACODE        OREQ      '000APORT'
     C**** CARCN         ANDNE     EORCN
     C****END   ADD *** 03/24/09 **************************************
     C*** End Del *** 10/17/14 ***************************************
     C*** Beg Add *** 10/17/14 ***************************************
     C                   IF        (CACODE <> 'APORT' and CACODE <> '000APORT')
     C                             or (CACODE='APORT' and CARCN <> EORCN)
     C                             or (CACODE='000APORT' and CARCN <> EORCN)
     C*** End Add *** 10/17/14 ***************************************
     C                   EXSR      CLCAPP
     C                   ELSE
     C                   SETON                                        55
     C                   ENDIF
     C     CAKY1         READE     PCA01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   ADD       APPAID        HLPAID
     C*
     C     HLPAID        IFNE      *ZEROS
     C     *IN55         OREQ      *ON
     C     EOBTYP        CASEQ     'N'           WRTMED
     C     EOBTYP        CASEQ     'D'           WRTDEN
     C     EOBTYP        CASEQ     'H'           WRTHOS
     C     EOBTYP        CASEQ     'X'           WRTPHR
     C                   ENDCS
     C                   ENDIF                                                  HLPAID<>0
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      UPDEOB
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/22/03 **************************************
     C*================================================================
     C* UPDEOB - Updates amounts in FAEOP/FAEAP
     C*================================================================
     C     UPDEOB        BEGSR
     C*
     C* Summing up the amounts for the current recon
     C*
     C                   CLEAR                   HDCHRG
     C                   CLEAR                   HDMRA
     C                   CLEAR                   HDOTHR
     C                   CLEAR                   HDOTH2
     C                   CLEAR                   HDNDIS
     C                   CLEAR                   HDPAID
     C*
     C     DAKY2         CHAIN     PDA02                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       DACHRG        HDCHRG
     C                   ADD       DAMRA         HDMRA
     C                   ADD       DAOTHR        HDOTHR
     C                   ADD       DAOTH2        HDOTH2
     C                   ADD       DANDIS        HDNDIS
     C                   ADD       DAPAID        HDPAID
     C*
     C     DAKY2         READE     PDA02                                  99
     C                   ENDDO
     C*
     C* Now update FAEAP with the above amounts
     C*
     C     EAKY1         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     HDCHRG        EACHRG
     C                   Z-ADD     HDMRA         EAMRA
     C                   Z-ADD     HDOTHR        EAOTHR
     C                   Z-ADD     HDOTH2        EAOTH2
     C                   Z-ADD     HDNDIS        EANDIS
     C                   Z-ADD     HDPAID        EAPAID
     C*
     C                   MOVEL(P)  Q1PGM         EAPGM
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C* Summing up the amounts for the total bill
     C*
     C                   CLEAR                   HDCHRG
     C                   CLEAR                   HDMRA
     C                   CLEAR                   HDOTHR
     C                   CLEAR                   HDOTH2
     C                   CLEAR                   HDNDIS
     C                   CLEAR                   HDPAID
     C*
     C     EAKY2         CHAIN(N)  PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       EACHRG        HDCHRG
     C                   ADD       EAMRA         HDMRA
     C                   ADD       EAOTHR        HDOTHR
     C                   ADD       EAOTH2        HDOTH2
     C                   ADD       EANDIS        HDNDIS
     C                   ADD       EAPAID        HDPAID
     C*
     C     EAKY2         READE     PEA01                                  99
     C                   ENDDO
     C*
     C* Now update FAEAP with the above amounts
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     HDCHRG        EOCHRG
     C                   Z-ADD     HDMRA         EOMRA
     C                   Z-ADD     HDOTHR        EOOTHR
     C                   Z-ADD     HDOTH2        EOOTH2
     C                   Z-ADD     HDNDIS        EONDIS
     C                   Z-ADD     HDPAID        EOPAID
     C*
     C                   MOVEL(P)  Q1PGM         EOPGM
     C*
     C                   UPDATE    PEO02
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 05/22/03 **************************************
     C*================================================================
     C* WRTDEN - Writes dental APORT record to FDCAP & FDDDP
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
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     X1LIN#        IFLT      @@LIN#
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     @@LIN#        IFNE      *ZEROS
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ENDIF
     C*
     C                   MOVEL(P)  'APORT'       KKPRCD
     C*
     C*  It checks to see if there is all ready an APORT record
     C*  in FDCAP. If not, then it will write an APORT record in
     C*  FDCAP, otherwise, it will update the existing APORT record.
     C*
     C                   MOVEL     *ON           WRITE             1
     C     DDKY1         CHAIN     PDD01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DDEOB#        KKEOB#
     C                   Z-ADD     DDDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              99
     C     *IN99         IFEQ      *OFF
     C     CARCN         IFEQ      EORCN
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   MOVEL(P)  'APORT'       CACPT4
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   MOVEL(P)  'APORT'       CACODE
     C                   MOVEL     *OFF          WRITE
     C                   UPDATE    PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   LEAVE
     C                   ELSE
     C                   UPDATE    PCA0F
     C                   ENDIF                                                  CARCN
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C     DDKY1         READE     PDD01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     WRITE         IFEQ      *ON
     C                   CLEAR                   PCA0F
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
     C                   MOVEL(P)  'APORT'       CACPT4
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
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL(P)  'APORT'       CACODE
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C*  Write FDDDP
     C*
     C                   CLEAR                   PDD01
     C                   Z-ADD     EOCO#         DDCO#
     C                   Z-ADD     EOSUB#        DDSUB#
     C                   Z-ADD     EOFND         DDFND
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   MOVEL     CASS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   Z-ADD     XXDSEQ        DDDSEQ
     C                   Z-ADD     EOEOB#        DDEOB#
     C                   MOVEL     'APORT'       DDPRCD
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'APORT'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DDPRDS
     C                   ELSE
     C                   MOVEL(P)  APPADJ        DDPRDS
     C                   ENDIF
     C*
     C                   MOVEL     '11'          DDPOS
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
     C*
     C                   WRITE     PDD01
     C                   ENDIF                                                  WRITE=*ON
     C*
     C                   EXSR      WRTDEP
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDEP - Write FDDEP record
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   MOVEL     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *ON
     C     DESTAT        OREQ      'I'
     C*
     C                   CLEAR                   PDE01
     C*
     C                   Z-ADD     EOCO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   MOVEL     EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     EORCN         DERCN
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     1             DESEQ#
     C*
     C     CMAPCT        IFNE      *ZEROS
     C                   MOVEL(P)  CMAPCT        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         APPPCT            7
     C                   ELSE
     C                   MOVEL(P)  '0.00'        APPPCT
     C                   ENDIF
     C*
     C                   MOVEL(P)  NOTE1         DENOT1
     C     NOTE2         CAT(P)    APPPCT:1      DENOT2
     C                   CAT       '%.':0        DENOT2
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CADT          DEDT
     C                   Z-ADD     CAIDT         DEIDT
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CAITIM        DEITIM
     C*
     C                   MOVEL(P)  'A'           DEESRC
     C                   MOVEL(P)  'Y'           DEPRNT
     C*
     C                   WRITE     PDE01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHOS - Writes hospital APORT record to FDCAP & FDDUP
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
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     X1LIN#        IFLT      @@LIN#
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     @@LIN#        IFNE      *ZEROS
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ENDIF
     C*
     C                   MOVEL     'APORT'       KKRATE
     C                   MOVE      *ZEROS        K1RVCD
     C*
     C*  It checks to see if there is all ready an APORT record
     C*  in FDCAP. If not, then it will write an APORT record in
     C*  FDCAP, otherwise, it will update the existing APORT record.
     C*
     C                   MOVEL     *ON           WRITE
     C     DUKY1         CHAIN     PDU01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     DUEOB#        KKEOB#
     C                   Z-ADD     DUDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              99
     C     *IN99         IFEQ      *OFF
     C     CARCN         IFEQ      EORCN
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   MOVEL(P)  'APORT'       CACPT4
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   MOVEL(P)  '000APORT'    CACODE
     C                   MOVEL     *OFF          WRITE
     C                   UPDATE    PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   LEAVE
     C                   ELSE
     C                   UPDATE    PCA0F
     C                   ENDIF                                                  CARCN
     C                   ENDIF                                                  *IN99=*OFF
     C     DUKY1         READE     PDU01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     WRITE         IFEQ      *ON
     C                   CLEAR                   PCA0F
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
     C                   MOVEL(P)  'APORT'       CACPT4
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
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL(P)  '000APORT'    CACODE
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C*  Update FDDUP
     C*
     C                   CLEAR                   PDU01
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
     C                   MOVEL(P)  'APORT'       DURATE
     C*
     C* Get description from serv code file or default to MRAADJ const.
     C*
     C                   CLEAR                   KKCMSV
     C                   MOVE      '000'         KKRVCD
     C                   MOVE      '000'         KKUNTS
     C                   MOVEL     'APORT'       KKHCPC
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY          READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        DUDESC
     C                   ELSE
     C                   MOVEL(P)  APPADJ        DUDESC
     C                   ENDIF
     C*
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
     C*
     C                   WRITE     PDU01
     C                   ENDIF                                                  WRITE=*ON
     C*
     C                   EXSR      WRTDEP
     C*
     C                   ENDSR
     C*================================================================
     C* WRTMED - Writes medical APORT record to FDCAP & FDDHP
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
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     X1LIN#        IFLT      @@LIN#
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     @@LIN#        IFNE      *ZEROS
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ENDIF
     C*
     C                   MOVEL(P)  'APORT'       KKPRCD
     C*
     C*  It checks to see if there is all ready an APORT record
     C*  in FDCAP. If not, then it will write an APORT record in
     C*  FDCAP, otherwise, it will update the existing APORT record.
     C*
     C                   MOVEL     *ON           WRITE
     C     DHKY1         CHAIN     PDH01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHEOB#        KKEOB#
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              99
     C     *IN99         IFEQ      *OFF
     C     CARCN         IFEQ      EORCN
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   MOVEL(P)  'APORT'       CACPT4
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   MOVEL(P)  'APORT'       CACODE
     C                   MOVEL     *OFF          WRITE
     C                   UPDATE    PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   LEAVE
     C                   ELSE
     C                   UPDATE    PCA0F
     C                   ENDIF                                                  CARCN
     C                   ENDIF                                                  *IN99=*OFF
     C     DHKY1         READE     PDH01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     WRITE         IFEQ      *ON
     C                   CLEAR                   PCA0F
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
     C                   MOVEL(P)  'APORT'       CACPT4
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
     C                   MOVEL     'C'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   Z-ADD     *ZERO         CADIFF
     C                   MOVEL(P)  'APORT'       CACODE
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C*  Write FDDHP
     C*
     C                   CLEAR                   PDH01
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
     C                   MOVEL(P)  'APORT'       DHPRCD
     C*
     C* Get description from proc code file or default to APPADJ const.
     C*
     C                   MOVEL(P)  'APORT'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DHPRDS
     C                   ELSE
     C                   MOVEL(P)  APPADJ        DHPRDS
     C                   ENDIF
     C*
     C                   MOVEL     'X'           DHDXR1
     C                   Z-ADD     1             DHUNTS
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
     C*
     C                   WRITE     PDH01
     C                   ENDIF                                                  WRITE*ON
     C*
     C                   EXSR      WRTDEP
     C*
     C                   ENDSR
     C*================================================================
     C* WRTPHR - Writes pharmacy APORT record to FDCAP & FDDPP
     C*================================================================
     C     WRTPHR        BEGSR
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
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     X1LIN#        IFLT      @@LIN#
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C     @@LIN#        IFNE      *ZEROS
     C                   Z-ADD     @@LIN#        X1LIN#
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   ENDIF
     C*
     C                   MOVEL(P)  'APORT'       KKNDC
     C*
     C*  It checks to see if there is all ready an APORT record
     C*  in FDCAP. If not, then it will write an APORT record in
     C*  FDCAP, otherwise, it will update the existing APORT record.
     C*
     C                   MOVEL     *ON           WRITE
     C     DPKY1         CHAIN     PDP01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     DPEOB#        KKEOB#
     C                   Z-ADD     DPDSEQ        KKDSEQ
     C*
     C     CAKY2         CHAIN     PCA0F                              99
     C     *IN99         IFEQ      *OFF
     C     CARCN         IFEQ      EORCN
     C                   MOVEL     '2'           CACSCD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CARDAT
     C                   MOVEL(P)  *BLANKS       CACPT4
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   MOVEL(P)  'APORT'       CACODE
     C                   MOVEL     *OFF          WRITE
     C                   UPDATE    PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C                   LEAVE
     C                   ELSE
     C                   UPDATE    PCA0F
     C                   ENDIF                                                  CARCN
     C                   ENDIF                                                  *IN99=*OFF
     C     DPKY1         READE     PDP01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     WRITE         IFEQ      *ON
     C                   CLEAR                   PCA0F
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
     C                   MOVEL(P)  'APORT'       CACPT4
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
     C                   MOVEL     'X'           CARCFL
     C                   MOVEL     EOSUB#        CASUB#
     C                   Z-SUB     HLPAID        CAPAID
     C                   Z-ADD     HLPAID        CAOTHR
     C                   MOVEL     EORCN         CARCN
     C                   MOVEL(P)  'APP'         CAOT1C
     C                   Z-ADD     X1LIN#        CALIN#
     C                   Z-ADD     XXDSEQ        CADSEQ
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL(P)  'APORT'       CACODE
     C                   MOVE      'X'           CADELF
     C*
     C                   WRITE     PCA0F
     C*** BEGIN ADD *** 05/22/03 **************************************
     C     *IN16         IFEQ      *ON
     C                   EXSR      WRUDAP
     C                   ENDIF
     C*** END   ADD *** 05/22/03 **************************************
     C*
     C*  Write FDDPP
     C*
     C                   CLEAR                   PDP01
     C                   Z-ADD     EOCO#         DPCO#
     C                   Z-ADD     EOSUB#        DPSUB#
     C                   Z-ADD     EOFND         DPFND
     C                   Z-ADD     EOFYR         DPFYR
     C                   Z-ADD     EOCASE        DPCASE
     C                   MOVEL     CASS#         DPSS#
     C                   Z-ADD     EOACDT        DPACDT
     C                   Z-ADD     EOEOB#        DPEOB#
     C                   Z-ADD     XXDSEQ        DPDSEQ
     C                   MOVEL(P)  'APORT'       DPNDC
     C*
     C* Get description from proc code file or default to APPADJ const.
     C*
     C                   MOVEL(P)  'APORT'       KKNDC
     C*
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNAME        DPDESC
     C                   ELSE
     C                   MOVEL(P)  APPADJ        DPDESC
     C                   ENDIF
     C*
     C                   Z-ADD     1             DPRXMQ
     C                   Z-ADD     1             DPUNTS
     C                   Z-ADD     CAFDAT        DPRXFD
     C*
     C                   MOVEL     'A'           DPSTAT
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1USER        DPIUSR
     C                   MOVEL     Q1PGM         DPPGM
     C                   MOVEL     Q1PGM         DPIPGM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSTIME        DPITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPIDT
     C*
     C                   WRITE     PDP01
     C                   ENDIF                                                  WRITE*ON
     C*
     C                   EXSR      WRTDEP
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 05/22/03 **************************************
     C*================================================================
     C* WRUDAP - Writes/Updates FDDAP
     C*================================================================
     C     WRUDAP        BEGSR
     C*
     C     DAKY1         CHAIN     PDA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDA01
     C                   Z-ADD     CACO#         DACO#
     C                   Z-ADD     CASUB#        DASUB#
     C                   Z-ADD     CAFND         DAFND
     C                   Z-ADD     CAFYR         DAFYR
     C                   Z-ADD     CACASE        DACASE
     C                   MOVEL     CASS#         DASS#
     C                   Z-ADD     CAACDT        DAACDT
     C                   Z-ADD     CAEOB#        DAEOB#
     C                   Z-ADD     EORCN         DARCN#
     C                   Z-ADD     CADSEQ        DADSEQ
     C                   Z-ADD     CARDAT        DATRDT
     C*
     C                   Z-ADD     CACHRG        DACHRG
     C                   Z-ADD     CAMRA         DAMRA
     C                   Z-ADD     CAOTHR        DAOTHR
     C                   Z-ADD     CANDIS        DANDIS
     C                   Z-ADD     CAPAID        DAPAID
     C*
     C                   MOVEL     'A'           DASTAT
     C                   MOVEL(P)  CAUSER        DAUSER
     C                   MOVEL(P)  CAPGM         DAPGM
     C                   Z-ADD     CATIME        DATIME
     C                   Z-ADD     CADT          DADT
     C                   MOVEL(P)  CAUSER        DAIUSR
     C                   MOVEL(P)  CAPGM         DAIPGM
     C                   Z-ADD     CAITIM        DAITIM
     C                   Z-ADD     CAIDT         DAIDT
     C*
     C                   WRITE     PDA01
     C                   ELSE
     C*
     C                   Z-ADD     CACHRG        DACHRG
     C                   Z-ADD     CAMRA         DAMRA
     C                   Z-ADD     CAOTHR        DAOTHR
     C                   Z-ADD     CANDIS        DANDIS
     C                   Z-ADD     CAPAID        DAPAID
     C*
     C                   MOVEL(P)  CAUSER        DAUSER
     C                   MOVEL(P)  CAPGM         DAPGM
     C                   Z-ADD     CATIME        DATIME
     C                   Z-ADD     CADT          DADT
     C*
     C                   UPDATE    PDA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 05/22/03 **************************************
