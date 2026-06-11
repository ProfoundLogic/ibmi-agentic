     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F3040R02 - T/M TOTALS FOR EOMB#/SVC#           C:62 04/20/98 *
     H*                                                               *
     H*  CALLED FROM: F3040R - WW T/M ACTIVITY (F8=Claim Summary)     *
     H*                                                               *
     H*  CALLS TO   : FA702R - TWO LINE HEADER API.                   *
     H*               SA001R - API THAT ASSIGNS VARIOUS SYSTEM FLAGS. *
     H*             SRCVTDAT - DATE CONVERSION PROGRAM.               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/14/01 C:62  PROJECT: MC111 30098                          *
     H*                 -REDEFINE INVOICED EOBS TO ALSO BE IF         *
     H*                  EAEPFL = 'P'(BEEN THRU BATCH) IF AUTO CHECK  *
     H*                                                               *
     H*  05/19/00  C:62  PROJECT: ARMNT                               *
     H*                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.  *
     H*                                                               *
     H*  10/10/99  C:62  PROJECT: MC212                               *
     H*                  ADD A SUBSET FEATURE SO THAT WHATEVER THE    *
     H*                  SUBSET HAPPENED TO BE IN F3040R WOULD BE USED*
     H*                  TO SELECT T/M LINE ITEMS FOR CALCULATING     *
     H*                  THE BILLING AMOUNT/HOURS/MILES/MISC CHARGES  *
     H*                  ETC.                                         *
     H*                                                               *
     H*  05/28/99  C:62  PROJECT: MC145                               *
     H*                  CHANGE FEE FROM CALCULATING IT TO CAMT.      *
     H*                                                               *
     H*  01/17/99  C:99  PROJECT: MC152                               *
     H*                  ADD RECON TO KEY                             *
     H*                                                               *
     H*  12/17/98  C:94  PROJECT: MC152                               *
     H*                  CHANGE PGM TO USE FAEAP INSTEAD OF           *
     H*                  NATRP, NAMBP, NAPBP, AND FAHBP               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF3040F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FNDTMP     UF   E             DISK
     F                                     RENAME(PTM01:DBRCDF)
     F*** BEGIN CHG *** 01/17/99 **************************************
     F**********NDTML   IF  E           K        DISK
     FNDTML1    IF   E           K DISK
     F*** END   CHG *** 01/17/99 **************************************
     FFAEOP     IF   E             DISK
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEG ADD *** 12/17/98 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** BEG ADD *** 12/17/98 ***************************************
     FFAHBL     IF   E           K DISK
     FNATRL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     FNAMBL1    IF   E           K DISK
     FFTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('T/M totals for EOMB/-
     D                                     Svc#')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FAEOP RRN
     C*** BEGIN ADD *** 10/20/99 **************************************
     C                   PARM                    @@SYS             1
     C                   PARM                    @@INVD            1
     C                   PARM                    @@EOB#            9 0
     C*** END   ADD *** 10/20/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM                    @@RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN DEL *** 10/20/99 **************************************
     C**********           KFLD           KKEOB#
     C*** END   DEL *** 10/20/99 **************************************
     C     EOKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     HBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    EOEOB#
     C*
     C     MBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    EOEOB#
     C*
     C     PBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    EOEOB#
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     TRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    EOEOB#
     C*** BEG ADD *** 12/17/98 ***************************************
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN DEL *** 10/20/99 **************************************
     C**********           KFLD           KKEOB#
     C*** BEGIN ADD *** 01/17/99 **************************************
     C**********           KFLD           KKRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*** END   DEL *** 10/20/99 **************************************
     C     EAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     F*** END ADD *** 12/17/98 ***************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*** BEGIN ADD *** 01/17/99 **************************************
     C     *LIKE         DEFINE    EORCN         KKRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*** BEGIN DEL *** 10/20/99 **************************************
     C**********
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C********** MVDBSC    BEGSR
     C*
     C**********           Z-ADD0         HDAMNT  92
     C*
     C********** EOKY      CHAINPEO02                99
     C*
     C*** BEGIN CHG *** 10/20/99 **************************************
     C********** *IN99     IFEQ '0'
     C********** *IN99     DOWEQ'0'
     C*** END   CHG *** 10/20/99 **************************************
     C*
     C*** BEG ADD *** 12/17/98 ***************************************
     C********** ##PRVB    IFEQ 'Y'
     C*
     C**********           SELEC
     C*
     C********** EOSYS     WHEQ 'P'
     C*
     C**********           Z-ADDEOBAMT    HDAMNT
     C*
     C********** EOSYS     WHEQ 'N'
     C*
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ '0'
     C**********           Z-ADDEANAMT    HDAMNT
     C*** BEGIN ADD *** 05/28/99 **************************************
     C**********           Z-ADDEABCAM    SCFEE
     C*** END   ADD *** 05/28/99 **************************************
     C**********           ENDIF
     C*
     C********** EOSYS     WHEQ 'U'
     C********** EOSYS     OREQ 'H'
     C********** EOSYS     OREQ 'M'
     C*
     C********** EAKY      CHAINPEA01                99
     C********** *IN99     IFEQ '0'
     C**********           Z-ADDEABAMT    HDAMNT
     C*** BEGIN ADD *** 05/28/99 **************************************
     C**********           Z-ADDEABCAM    SCFEE
     C*** END   ADD *** 05/28/99 **************************************
     C**********           ENDIF
     C*
     C**********           ENDSL
     C*
     C**********           ELSE                            ##PRVB = ' '
     C*
     C*** END ADD *** 12/17/98 ***************************************
     C*
     C**********           SELEC
     C*
     C********** EOSYS     WHEQ 'P'
     C*
     C**********           Z-ADDEOBAMT    HDAMNT
     C*** BEGIN ADD *** 05/28/99 **************************************
     C**********           Z-ADD*ZEROS    SCFEE
     C*** END   ADD *** 05/28/99 **************************************
     C*
     C********** EOSYS     WHEQ 'N'
     C*
     C********** TRKY      CHAINPTR01                99
     C*
     C********** *IN99     IFEQ '0'
     C*
     C**********           Z-ADDTRBAMT    HDAMNT
     C*
     C**********           ENDIF
     C*
     C********** EOSYS     WHEQ 'U'
     C*
     C********** PBKY      CHAINPPB01                99
     C*
     C********** *IN99     IFEQ '0'
     C*
     C**********           Z-ADDPBBAMT    HDAMNT
     C*** BEGIN ADD *** 05/28/99 **************************************
     C**********           Z-ADDPBCAMT    SCFEE
     C*** END   ADD *** 05/28/99 **************************************
     C*
     C**********           ENDIF
     C*
     C********** EOSYS     WHEQ 'H'
     C*
     C********** HBKY      CHAINPHB01                99
     C*
     C********** *IN99     IFEQ '0'
     C*
     C**********           Z-ADDHB1AAM    HDAMNT
     C*** BEGIN ADD *** 05/28/99 **************************************
     C**********           Z-ADDHB1CAM    SCFEE
     C*** END   ADD *** 05/28/99 **************************************
     C*
     C**********           ENDIF
     C*
     C********** EOSYS     WHEQ 'M'
     C*
     C********** MBKY      CHAINPMB01                99
     C*
     C********** *IN99     IFEQ '0'
     C*
     C**********           Z-ADDMBBAMT    HDAMNT
     C*** BEGIN ADD *** 05/28/99 **************************************
     C**********           Z-ADDMBCAMT    SCFEE
     C*** END   ADD *** 05/28/99 **************************************
     C*
     C**********           ENDIF
     C*
     C**********           ENDSL
     C*
     C*** BEG ADD *** 12/17/98 **************************************
     C**********           ENDIF                           ##PRVB = Y
     C*** END ADD *** 12/17/98 **************************************
     C*
     C*** BEGIN CHG *** 10/20/99 **************************************
     C**********           ENDIF
     C**********           ENDDO
     C*** END   CHG *** 10/20/99 **************************************
     C*
     C**********           Z-ADDHDAMNT    SCAMNT
     C*
     C* Initialize holding fields.
     C*
     C**********           Z-ADD0         HDITMS  40
     C**********           Z-ADD0         HDMILE  92
     C**********           Z-ADD0         HDMIRT  92
     C**********           Z-ADD0         HDHRS   92
     C**********           Z-ADD0         HDHRRT  92
     C**********           Z-ADD0         HDMSCG  92
     C*
     C* Calculations to determine screen fields.
     C*
     C********** TMKY      CHAINPTM01                99
     C*
     C********** *IN99     DOWEQ*OFF
     C*** BEGIN ADD *** 08/19/98 **************************************
     C********** TMSTAT    IFNE 'I'
     C*** END   ADD *** 08/19/98 **************************************
     C*
     C**********           ADD  1         HDITMS
     C*
     C**********           ADD  TMMILE    HDMILE
     C*
     C**********           Z-ADD0         HDHOLD  92
     C********** TMMILE    MULT TMMIRT    HDHOLD
     C**********           ADD  HDHOLD    HDMIRT
     C*
     C**********           ADD  TMHRS     HDHRS
     C*
     C**********           Z-ADD0         HDHOLD
     C********** TMHRS     MULT TMHRRT    HDHOLD
     C**********           ADD  HDHOLD    HDHRRT
     C*
     C**********           ADD  TMMSCG    HDMSCG
     C*** BEGIN ADD *** 08/19/98 **************************************
     C**********           ENDIF
     C*** END   ADD *** 08/19/98 **************************************
     C*
     C********** TMKY      READEPTM01                    99
     C*
     C**********           ENDDO
     C*
     C*
     C*
     C*** BEGIN DEL *** 05/28/99 **************************************
     C**********           Z-ADD0         HDHOLD
     C********** HDAMNT    SUB  HDHRRT    HDHOLD
     C**********           SUB  HDMIRT    HDHOLD
     C**********           SUB  HDMSCG    HDHOLD
     C**********           Z-ADDHDHOLD    SCFEE
     C*** END   DEL *** 05/28/99 **************************************
     C*
     C*
     C*
     C**********           Z-ADDHDITMS    SCITMS
     C**********           Z-ADDHDHRS     SCHRS
     C**********           Z-ADDHDHRRT    SCHRRT
     C**********           Z-ADDHDMILE    SCMILE
     C**********           Z-ADDHDMIRT    SCMIRT
     C**********           Z-ADDHDMSCG    SCMSCG
     C*
     C**********           ENDSR
     C*** END   DEL *** 10/20/99 **************************************
     C*** BEGIN ADD *** 10/20/99 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* Initialize holding fields.
     C*
     C                   Z-ADD     0             HDITMS            4 0
     C                   Z-ADD     0             HDMILE            9 2
     C                   Z-ADD     0             HDMIRT            9 2
     C                   Z-ADD     0             HDHRS             9 2
     C                   Z-ADD     0             HDHRRT            9 2
     C                   Z-ADD     0             HDMSCG            9 2
     C                   Z-ADD     0             HDAMNT            9 2
     C                   MOVEL     *OFF          MEETS             1
     C*
     C*****************************************************************
     C* PROVIDER BILLS PROCESS ****************************************
     C*****************************************************************
B001 C     ##PRVB        IFEQ      'Y'
     C*
     C  N16EAKY          CHAIN     PEA01                              99
     C   16EAKY2         CHAIN     PEA01                              99
     C*
D001 C     *IN99         DOWEQ     *OFF
     C*
B002 C     @@SYS         IFNE      *BLANKS
     C     @@SYS         ANDNE     EASYS
     C*
     C                   MOVEL     *ON           MEETS
     C*
E002 C                   ENDIF
     C*
B003 C     @@INVD        IFEQ      'Y'
     C     EABINV        ANDEQ     *ZEROS
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     @@INVD        OREQ      'Y'
     C     EABINV        ANDEQ     *ZEROS
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   MOVEL     *ON           MEETS
     C*
E003 C                   ENDIF
     C*
B004 C     @@INVD        IFEQ      'N'
     C     EABINV        ANDNE     *ZEROS
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     @@INVD        OREQ      'N'
     C     EABINV        ANDEQ     *ZEROS
     C     EAEPFL        ANDEQ     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   MOVEL     *ON           MEETS
     C*
E004 C                   ENDIF
     C*
B004 C     @@EOB#        IFNE      *ZEROS
     C     EAEOB#        ANDNE     @@EOB#
     C*
     C                   MOVEL     *ON           MEETS
     C*
E004 C                   ENDIF
     C*** BEGIN ADD *** 05/19/00 **************************************
     C*
B004 C     @@EOB#        IFNE      *ZEROS
     C     EARCN         ANDNE     @@RCN
     C*
     C                   MOVEL     *ON           MEETS
     C*
E004 C                   ENDIF
     C*** END   ADD *** 05/19/00 **************************************
     C*
B005 C     MEETS         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     EASYS         WHENEQ    'P'
     C*
     C                   ADD       EABAMT        HDAMNT
     C                   ADD       EABCAM        SCFEE
     C*
     C     EASYS         WHENEQ    'N'
     C*
     C                   ADD       EANAMT        HDAMNT
     C                   ADD       EABCAM        SCFEE
     C*
     C     EASYS         WHENEQ    'U'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'M'
     C*
     C                   ADD       EABAMT        HDAMNT
     C                   ADD       EABCAM        SCFEE
     C*
     C                   ENDSL
     C*
     C                   EXSR      TMCLC
     C*
E005 C                   ENDIF
     C*
     C                   MOVEL     *OFF          MEETS
     C*
     C  N16EAKY          READE     PEA01                                  99
     C   16EAKY2         READE     PEA01                                  99
     C*
D001 C                   ENDDO
     C*****************************************************************
     C* BILL ENTRY PROCESS  *******************************************
     C*****************************************************************
     C*
S001 C                   ELSE                                                   ##PRVB = ' '
     C*
     C  N16EOKY          CHAIN     PEO02                              99
     C   16EOKY2         CHAIN     PEO02                              99
     C*
D002 C     *IN99         DOWEQ     *OFF
     C*
BB01 C     @@SYS         IFNE      *BLANKS
     C     @@SYS         ANDNE     EOSYS
     C*
     C                   MOVEL     *ON           MEETS
     C*
EE02 C                   ENDIF
     C*
BB03 C     @@INVD        IFEQ      'Y'
     C     EOINV#        ANDEQ     *ZEROS
     C*
     C                   MOVEL     *ON           MEETS
     C*
EE03 C                   ENDIF
     C*
BB04 C     @@INVD        IFEQ      'N'
     C     EOINV#        ANDNE     *ZEROS
     C*
     C                   MOVEL     *ON           MEETS
     C*
EE04 C                   ENDIF
     C*
BB05 C     MEETS         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     EOSYS         WHENEQ    'P'
     C*
     C                   ADD       EOBAMT        HDAMNT
     C                   ADD       *ZEROS        SCFEE
     C*
     C     EOSYS         WHENEQ    'N'
     C*
     C     TRKY          CHAIN     PTR01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   ADD       TRBAMT        HDAMNT
     C*
     C                   ENDIF
     C*
     C     EOSYS         WHENEQ    'U'
     C*
     C     PBKY          CHAIN     PPB01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   ADD       PBBAMT        HDAMNT
     C                   ADD       PBCAMT        SCFEE
     C*
     C                   ENDIF
     C*
     C     EOSYS         WHENEQ    'H'
     C*
     C     HBKY          CHAIN     PHB01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   ADD       HB1AAM        HDAMNT
     C                   ADD       HB1CAM        SCFEE
     C*
     C                   ENDIF
     C*
     C     EOSYS         WHENEQ    'M'
     C*
     C     MBKY          CHAIN     PMB01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   ADD       MBBAMT        HDAMNT
     C                   ADD       MBCAMT        SCFEE
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   EXSR      TMCLC
     C*
EE   C                   ENDIF
     C*
     C                   MOVEL     *OFF          MEETS
     C*
     C  N16EOKY          READE     PEO02                                  99
     C   16EOKY2         READE     PEO02                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*
     C                   Z-ADD     HDAMNT        SCAMNT
     C                   Z-ADD     HDITMS        SCITMS
     C                   Z-ADD     HDHRS         SCHRS
     C                   Z-ADD     HDHRRT        SCHRRT
     C                   Z-ADD     HDMILE        SCMILE
     C                   Z-ADD     HDMIRT        SCMIRT
     C                   Z-ADD     HDMSCG        SCMSCG
     C*
     C                   ENDSR
     C*** END   ADD *** 10/20/99 **************************************
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVEL     *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** BEG ADD *** 12/17/98 ***************************************
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*** END ADD *** 12/17/98 ***************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C*
     C* GET FAEOP RECORD.
     C*
     C     @@RRN         CHAIN(N)  PEO01                              99
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*** BEGIN CHG *** 05/19/00 **************************************
     C*** BEGIN ADD *** 01/17/99 **************************************
     C**********           Z-ADDEORCN     KKRCN
     C                   Z-ADD     @@RCN         KKRCN
     C*** END   ADD *** 01/17/99 **************************************
     C*** END   CHG *** 05/19/00 **************************************
     C*** BEGIN ADD *** 10/20/99 **************************************
     C     EOSYS         IFEQ      'U'
     C     @@EOB#        ORNE      *ZEROS
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 10/20/99 **************************************
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C* LOAD ADDITIONAL HEADER VALUES
     C*
     C                   MOVEL     EOSYS         SCSYS
     C                   MOVEL(P)  EOSYS         KKKEY
     C                   MOVEL(P)  'SYS'         KKTBID
     C                   MOVE      EOEOB#        SCEOB#
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTDESC        SCSYNM
     C*
     C                   MOVEL     EOEOB#        SCEOB#
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* TMCLC - CALCULATE TIME AND MATERIALS
     C*================================================================
     C     TMCLC         BEGSR
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     *ZEROS        KKRCN
     C*
     C                   ENDIF
     C*
     C* Calculations to determine screen fields.
     C*
     C     TMKY          CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     TMSTAT        IFNE      'I'
     C*
     C                   ADD       1             HDITMS
     C*
     C                   ADD       TMMILE        HDMILE
     C*
     C                   Z-ADD     0             HDHOLD            9 2
     C     TMMILE        MULT      TMMIRT        HDHOLD
     C                   ADD       HDHOLD        HDMIRT
     C*
     C                   ADD       TMHRS         HDHRS
     C*
     C                   Z-ADD     0             HDHOLD
     C     TMHRS         MULT      TMHRRT        HDHOLD
     C                   ADD       HDHOLD        HDHRRT
     C*
     C                   ADD       TMMSCG        HDMSCG
     C*
     C                   ENDIF
     C*
     C     TMKY          READE     PTM01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
