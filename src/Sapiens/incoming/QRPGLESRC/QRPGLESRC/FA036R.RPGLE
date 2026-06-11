     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*
     H* Write T/M Activity
     H*
     H*****************************************************************
     H* Created on 08/13/2006 by c:62 for Project 139MEM
     H*****************************************************************
     H* Revisions:
     H*
     H* 06/12/07 C:62 PROJECT 139MHK
     H*               ADD PARM TO PASS AMOUNT
     H*
     H*****************************************************************
     FFAMSP     IF   E             DISK
     FFMCMP     IF   E             DISK
     FFMSBL1    IF   E           K DISK
     FNDTMP     O    E             DISK
     FNDTML1    IF   E           K DISK
     F                                     RENAME(PTM01:PTM02)
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     FFAEAL1    IF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRC01)
     FFMFRL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D DBFDSB          DS
     D  DBRRNB               397    400B 0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          FAMSP
     C                   PARM                    @@RRN2            9 0          FMCMP
     C                   PARM                    @@MSCD            4
     C                   PARM                    @@BGDT            7 0
     C                   PARM                    @@RTCD            1
     C*** BEGIN ADD *** 06/12/07 **************************************
     C                   PARM                    @@MSCG            9 2
     C*** END   ADD *** 06/12/07 **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     ATKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    KKKEY
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     FRKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C*
     C     FRKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    @@BGDT
     C*
     C     SBKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    @@MSCD
     C*
     C     SBKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    @@MSCD
     C                   KFLD                    @@BGDT
     C*
     C     TMKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    KKRCN
     C*
     C     TMKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKSEQ#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SBEFDT        KKEFDT
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    ATKEY         KKKEY
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    TMSEQ#        KKSEQ#
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
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C                   PARM      *ZEROS        ##DT01            7 0
     C                   PARM      *ZEROS        ##DT02            7 0
     C                   PARM      *ZEROS        ##DT03            7 0
     C                   PARM      *ZEROS        ##DT04            7 0
     C                   PARM      *ZEROS        ##DT05            7 0
     C*
     C                   MOVEL     *BLANKS       @@RTCD
     C                   Z-ADD     0             KKRCN
     C                   Z-ADD     MSBGDT        KKEFDT
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C     SBKY2         SETGT     PSB01
     C     SBKY1         READPE    PSB01                                  99
     C     *IN99         IFEQ      *OFF
     C     @@CALL        IFEQ      'W'
     C                   EXSR      WRTTMP
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  'F'           @@RTCD
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*================================================================
     C* WRTTMP - WRITES NDTMP RECORD
     C*================================================================
     C     WRTTMP        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXEOB#
     C     @@RRN         CHAIN     PMS01                              99
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         DOWEQ     *OFF
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     EABINV        IFEQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C                   ENDIF
     C     EOKY          READE     PEO01                                  99
     C                   ENDDO
     C*
     C     XXEOB#        IFEQ      *ZEROS
     C                   MOVE      'W'           XXFAEO
     C                   MOVE      'W'           XXFAEA
     C     MSCO#         CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   Z-ADD     C1EOB#        ##EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C                   ELSE
     C                   MOVE      ' '           XXFAEO
     C                   MOVE      'U'           XXFAEA
     C                   Z-ADD     XXEOB#        ##EOB#
     C                   ENDIF
     C*** BEGIN ADD *** 06/12/07 **************************************
     C     Q1PRMS        IFGT      6
     C                   Z-ADD     @@MSCG        ##TMAA
     C                   ELSE
     C                   Z-ADD     SBFEE         ##TMAA
     C                   ENDIF
     C*** END   ADD *** 06/12/07 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      @@RRN2        ##RRN             9 0          RRN OF CASE
     C                   PARM                    ##EOB#            9 0          EOB #
     C                   PARM      *ZEROS        ##RCN             3 0          RECON #
     C                   PARM      Q1PGM         ##PGM            10            NAME OF PGM CALLING
     C                   PARM                    ##BAMT            9 2          BRS AMOUNT
     C                   PARM                    ##NAMT            9 2          NWS AMOUNT
     C                   PARM      'H'           ##BHRF            1            BRS INV H/R FLAG
     C                   PARM                    ##BBLF            1            BRS INV PROC FLAG
     C                   PARM                    ##BINV            9 0          BRS INVOICE #
     C                   PARM                    ##BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    ##NHRF            1            NWS INV H/R FLAG
     C                   PARM                    ##NBLF            1            NWS INV PROC FLAG
     C                   PARM                    ##NINV            9 0          NWS INVOICE #
     C                   PARM      'M'           ##SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        ##WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C*** BEGIN CHG *** 06/12/07 **************************************
     C**********           PARM SBFEE     ##TMAA  92       TIME/MAT AFTER
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END   CHG *** 06/12/07 **************************************
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C     TMKY2         SETGT     NDTML1
     C     TMKY1         READPE    NDTML1                                 99
     C     *IN99         IFEQ      *OFF
     C     TMSEQ#        ADD       1             XXSEQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PTM01
     C*
     C                   Z-ADD     MSCO#         TMCO#
     C                   Z-ADD     MSSUB#        TMSUB#
     C                   Z-ADD     MSFND         TMFND
     C                   Z-ADD     MSFYR         TMFYR
     C                   Z-ADD     MSCASE        TMCASE
     C                   Z-ADD     ##EOB#        TMEOB#
     C                   Z-ADD     0             TMRCN
     C                   MOVEL     'M'           TMSYS
     C                   Z-ADD     XXSEQ#        TMSEQ#
     C                   MOVEL(P)  MSMSCD        TM10A1
     C*
     C                   Z-ADD     MSBGDT        TMSVDT
     C*
     C                   MOVEL     MSMSCD        TMMSCD
     C                   Z-ADD     MSSEQ#        TMMSEQ
     C                   MOVEL     MSSTAF        TMSTAF
     C                   MOVEL     SBACT         TMACT
     C*
     C                   MOVEL(P)  TMACT         KKKEY
     C     ATKY          CHAIN     PAT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     ATDESC        TMDESC
     C                   MOVEL     ATTYP2        TMPNFL
     C                   MOVEL     ATTYP1        TMMVFL
     C                   ELSE
     C                   MOVEL     *BLANKS       TMDESC
     C                   MOVEL     *BLANKS       TMPNFL
     C                   MOVEL     *BLANKS       TMMVFL
     C                   ENDIF
     C*
     C     FRKY1         SETGT     PFR01
     C     FRKY          READPE    PFR01                                  99
     C     *IN99         IFEQ      *OFF
     C     TMPNFL        IFEQ      'P'
     C                   Z-ADD     FRPRT1        TMHRRT
     C                   ELSE
     C                   Z-ADD     FRNRT1        TMHRRT
     C                   ENDIF
     C                   Z-ADD     FRMRT1        TMMIRT
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       TMHOLD
     C                   Z-ADD     *ZEROS        TMMILE
     C                   Z-ADD     *ZEROS        TMHRS
     C*** BEGIN ADD *** 06/12/07 **************************************
     C     Q1PRMS        IFGT      6
     C                   Z-ADD     @@MSCG        TMMSCG
     C                   ELSE
     C*** END   ADD *** 06/12/07 **************************************
     C                   Z-ADD     SBFEE         TMMSCG
     C*** BEGIN ADD *** 06/12/07 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/12/07 **************************************
     C                   MOVEL     ' '           TMFLG3
     C                   MOVEL     ' '           TMFLG2
     C                   MOVEL     'Y'           TMBFLG
     C*
     C                   MOVEL     Q1USER        TMUSER
     C                   MOVEL     Q1USER        TMIUSR
     C                   MOVEL     Q1PGM         TMPGM
     C                   MOVEL     Q1PGM         TMIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TMDT
     C*
     C                   MOVEL     SRDTO         TMIDT
     C*
     C                   MOVEL     DSTIME        TMTIME
     C                   MOVEL     DSTIME        TMITIM
     C*
     C                   WRITE     PTM01
     C*
     C                   ENDSR
