      *  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
      *
      ****************************************************************
      *  F8683R - Complete Coventry Validated Bills
      *
      *  DESCRIPTION: This pgm will update the EOB's that were not accepted
      *               by Coventry.  It will put them in completed status and
      *               generate a check request if necessary
      *
      *  CREATED    : 11/03/2008 PROGRAMMER: 79    PROJECT: 129MPN
      *
      *  CALLED BY  : F8681C
      *  CALLS TO   : FA700R  - Translate WC-GROUP into MC-SUB#/GROUP
      *               WA023R1 - API for determining Bank Code
      *
      ****************************************************************
      *  REVISIONS:
      *
      ****************************************************************
    Hdebug(*yes) option(*srcstmt:*nodebugio)
     F* faeop by co/sub/eob
     FFAEOL3    UF   E           K DISK
     F*
     F* fdcap by CO#/SUB#/FND/FYR/CASE/EOB#/RCN
     FfdcaL13   UF   E           K DISK
     F*
     F* faeap by CO#/SUB#/FND/FYR/CASE/EOB#/RCN
     FFAEAL1    UF   E           K DISK
     FFAEAp     IF   E           K DISK    rename(pea01:pea02)
     F*
     F* EOB History
     FFAESP     o    E             DISK
     F* ---
     F* Check Request file
     FWARPLN    IF A E           K DISK
     FWARPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMCIL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWMNRL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
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
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWTPSL     IF   E           K DISK
     FftxmL     IF   E           K DISK
     D*****************************************************************
     D/COPY SCOPYWW01I
     D*
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D TRANS           C                   CONST('Transaction ID:')
     D USERID          C                   CONST('PDSPGMR   ')
     D complete        C                   CONST('Completed by Coventry upload')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  DATA                   1     50
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*
     D updatebill      s              1
     D*
     D $$APPL          s              2
     D $$CASE          s              7
     D $$CKTP          s             10
     D $$CO#           s              3
     D $$DATA          s             50
     D $$DATE          s              7
     D $$FND           s              3
     D $$FYR           s              3
     D $$GRP           s              3
     D $$MODE          s              1
     D $$PCDE          s              2
     D $$RCDE          s              2
     D $$RTCD          s              1
     D*
     D* ------------------------
     IPCM03
     I              CMOSCL                      CWOSCL
     C*================================================================
     C*  PLIST definitions
     C*================================================================
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD2       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C     EAKY          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    kkSUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkRCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    kkSUB#
     C                   KFLD                    kkEOB#
     C*
     C     CAKY          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    kkSUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkRCN
     *
     C     RPKY1         KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     *
     C     RPKY2         KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C*
     C     NRKY          KLIST
     C                   KFLD                    eoCO#
     C                   KFLD                    eofein
     C                   KFLD                    eomod
     *
     C     CMKY4         KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     *
     C     CIKY          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     *
     C     TBKY          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOco#         KKco#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOSUB#        KKsub#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOEOB#        svEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOfein        KKfein
     C     *LIKE         DEFINE    EOdt          xxDT
     C     *LIKE         DEFINE    EOtime        xxTime
     C     *LIKE         DEFINE    EOmod         KKmod
     C     *LIKE         DEFINE    EOuser        user
     C     *LIKE         DEFINE    easinv        xxsinv
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C     *LIKE         DEFINE    PCKEY         KKKEY
     C     *LIKE         DEFINE    ciptyp        KKptyp
     C     *LIKE         DEFINE    cisys         KKsys
     C     *LIKE         DEFINE    cmfnd         KKwcf#
     C     *LIKE         DEFINE    rpseq#        xxseq#
     C*================================================================
     C*                MAIN SUBROUTINE
     C*================================================================
     C*
     C                   EXSR      PGMSTR
     C*
     C                   read      pea02                                  98
     C                   dow       *in98 = *off
     C*
     C* Determine the record format and load the data structure.
     C*
     C                   eval      kkco# = eaco#
     C                   eval      kksub# = easub#
     C                   eval      kkfnd = eafnd
     C                   eval      kkfyr = eafyr
     C                   eval      kkcase= eacase
     C                   eval      kkeob#= eaeob#
     C                   eval      kkrcn = earcn
     C*
     C                   if        eansnt = 'S' and earcn=0 and eaepfl='E'
     C                   if        eaeob#<>  000631233 and
     c                             eaeob#<>  000631312 and
     c                             eaeob#<>  000631382 and
     c                             eaeob#<>  000631411 and
     c                             eaeob#<>  000631771 and
     c                             eaeob#<>  000631795 and
     c                             eaeob#<>  000631804 and
     c                             eaeob#<>  000631990 and
     c                             eaeob#<>  000632134 and
     c                             eaeob#<>  000632315 and
     c                             eaeob#<>  000632364 and
     c                             eaeob#<>  000632196 and
     c                             eaeob#<>  000632738 and
     c                             eaeob#<>  000632777 and
     c                             eaeob#<>  000633012 and
     c                             eaeob#<>  000633307 and
     c                             eaeob#<>  000633713 and
     c                             eaeob#<>  000633759 and
     c                             eaeob#<>  000633770 and
     c                             eaeob#<>  000633789 and
     c                             eaeob#<>  000633815 and
     c                             eaeob#<>  000633829
     C                   exsr      process
     C                   endif
     C                   endif
     C*
     C                   read      pea02                                  98
     C                   enddo
     C*
     C* Update Transmission file
     C*
     C                   EXSR      PGMEND
     C*================================================================
     C* PROCESS - Process the EOB
     C*================================================================
     C     PROCESS       BEGSR
     C*
     C     eaky          chain     pea01
     C                   if        %found
     C                   exsr      upfaeap
     C                   exsr      upfdcap
     C                   exsr      wrfaesp
     C                   exsr      wrtrpp
     C                   endif
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        xxTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxDT
     C*
 001 C                   MOVEL(P)  USERID        USER
     C                   z-add     777           ##CO#
     C                   CALL      COMAPI        COMPL                  99
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UpFAEAP - Update FAEAP & FAEOP
     C*================================================================
     C     UpFAEAP       BEGSR
     C*
     C                   eval      eaepfl=' '
     C                   MOVEL(P)  xxsinv        easinv
     C                   MOVEL(P)  'P'           eansnt
    C*
     C                   movel(P)  q1pgm         eapgm
     C                   z-add     xxdt          eadt
     C                   z-add     xxtime        eatime
     C                   update    pea01
     C* ---
     C* update FAEOP
     C*
     C     eoky          chain     peo01                              99
     C                   if        *in99 = *off
     C                   MOVEL(P)  xxsinv        eosinv
     C                   MOVEL(P)  'C'           eoblst
     C                   z-add     eoeob#        eochk#
    C*
     C                   movel(P)  q1pgm         eopgm
     C                   z-add     xxdt          eodt
     C                   z-add     xxtime        eotime
     C                   update    peo01
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* WrFAESP - Write FAESP for EOB History
     C*================================================================
     C     WrFAESP       BEGSR
     C*
     c                   clear                   pes01
     C                   move      ##co#         esco#
     C                   move      kksub#        essub#
     C                   move      kkfnd         esfnd
     C                   move      kkfyr         esfyr
     C                   move      kkcase        escase
     C                   move      kkeob#        eseob#
     C                   move      kkrcn         esrcn
     C                   move      eoss#         esss#
     C                   z-add     eoacdt        esacdt
     C                   movel(P)  'C'           esblst
     C                   movel(P)  complete      esreas
    C*
    C* load maintenance fields
     C                   movel     'A'           esstat
     C                   movel     q1user        esuser
     C                   movel     q1pgm         espgm
     C                   movel     q1user        esiusr
     C                   movel     q1pgm         esipgm
     C                   z-add     xxdt          esdt
     C                   z-add     xxdt          esidt
     C                   eval      estime = xxtime
     C                   eval      esitim = xxtime
    C*
     C                   write     pes01
     C*
     C                   ENDSR
     C*================================================================
     C* UpFDCAP - Update FDCAP
     C*================================================================
     C     UpFDCAP       BEGSR
     C*
     C     caky          chain     pca01                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   z-add     caeob#        cachk#
     C*
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   Z-ADD     xxTIME        CATIME
     C                   Z-ADD     xxDT          CADT
     C*
     C                   update    PCA01
     C*
     C     caky          reade     pca01                                  99
     C                   enddo
     C*
     C                   ENDSR
      *================================================================
      * WRTRPP - Write WARPP record
      *================================================================
     C     WRTRPP        BEGSR
      *
 003  * Let's get the WCMS group number
      *
 003 C                   MOVE      ##CO#         #1CO#
 003 C                   MOVE      eaSUB#        #1SUB#
 003 C                   MOVE      eaFND         #1FND
 003  *
 003 C                   CALL      'FA700R'
 003 C                   PARM      *BLANKS       ##RTCD            1
 003 C                   PARM                    #1CO#             3
 003 C                   PARM      *BLANKS       #1WCF#            3
 003 C                   PARM                    #1SUB#            5
 003 C                   PARM                    #1FND             3
 003  *
B004 C                   if        ##RTCD = *BLANKS
 004 C                   MOVE      #1WCF#        KKWCF#
X004 C                   ELSE
 004 C                   eval      KKWCF# = *ZEROS
E004 C                   ENDIF
      *
      *
     C                   eval      KKEOB# = EOEOB#
     C                   eval      KKEOBS = EORCN
     C     RPKY1         CHAIN     PRP01                              99
     C                   if        *IN99 = *ON
      *
     C     RPKY2         SETGT     PRP02
     C     RPKY2         READPE    PRP02                                  99
     C                   if        *IN99 = *OFF
     C                   eval      XXSEQ# = RPSEQ# + 1
     C                   ELSE
     C                   eval      XXSEQ# = 1
     C                   ENDIF
      *
     C                   CLEAR                   PRP01
     C                   eval      RPCO# = EOCO#
     C                   eval      RPFND = KKWCF#
     C                   eval      RPFYR = EOFYR
     C                   eval      RPCASE = EOCASE
     C                   eval      RPSEQ# = XXSEQ#
     C                   eval      RPTRDT = EOTRDT
     C                   eval      RPFEIN = EOFEIN
     C                   eval      RPMOD = EOMOD
     C                   eval      RPAMT = EAPAID
      *
     C     NRKY          CHAIN     PNR01                              99
     C*
     C                   if        *in99 = *OFF
     C                   MOVEL(P)  'G'           RPPGTP
     C                   end
     C*
     C     CMKY4         CHAIN     PCM03                              99
     C                   MOVEL     'P'           KKPTYP
     C                   MOVEL     EOSYS         KKSYS
     C     CIKY          CHAIN     PCI01                              99
     C                   if        *IN99 = *OFF
      *
     C                   if        CMCVFL = 'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C                   if        EOBTYP = 'X'
     C                   MOVE(P)   CIXBR2        RPBR2
     C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
      *
     C                   if        CMLTFL = 'I'
     C                   MOVE(P)   CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
     C                   ENDIF
      *
     C                   MOVEL(P)  CISTS         RPSTS
      *
     C                   ELSE
      *
     C                   if        EOSYS = 'P'
     C                   MOVEL(P)  'MD'          RPBR2
     C                   ELSE
     C                   MOVEL(P)  'HP'          RPBR2
     C                   ENDIF
      *
     C                   if        CMLTFL = 'I'
     C                   MOVEL(P)  'LT'          RPBR7
     C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
     C                   ENDIF
      *
     C                   MOVEL(P)  'PD'          RPSTS
      *
     C                   ENDIF
      *
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   eval      RPFRDT = EOFRDT
     C                   eval      RPTODT = EOTODT
     C                   MOVEL(P)  'MP'          RPTRCD
     C                   MOVEL(P)  'P'           RPTYPE
     C                   MOVEL(P)  'M'           RPRETP
     C                   MOVEL(P)  'ME'          RPRECD
      *
     C                   MOVEL(P)  RPTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C                   if        *IN99 = *OFF
     C                   MOVEL(P)  PCDESC        RPDESC
     C                   ENDIF
      *
     C                   MOVEL(P)  EOPNAM        RPPNAM
     C                   MOVEL(P)  EOADR1        RPADR1
     C                   MOVEL(P)  EOADR2        RPADR2
     C                   MOVEL(P)  EOCTY         RPCTY
     C                   MOVEL(P)  EOST          RPST
     C                   eval      RPZIP = EOZIP
     C                   MOVEL     'P'           RPSTAT
     C                   MOVEL(P)  EOUSER        RPUSER
     C                   MOVEL(P)  EOPGM         RPPGM
     C                   eval      RPTIME = EOTIME
     C                   eval      RPDT = EODT
     C                   eval      RPEOB# = EOEOB#
     C                   eval      RPEOBS = EORCN
     C                   eval      RPRCVD = EODT
      *
     C     TRANS         CAT(P)    xxsinv:1      RPRMDS
      *
     C                   MOVEL(P)  RPSTS         KKKEY
     C     TBKY          CHAIN     PPS01                              99
     C                   if        *IN99 = *OFF
     C                   MOVE(P)   PSSRT         RPSORT
     C                   ENDIF
      *
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  USER          RPRQST
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         $$CO#
     C                   MOVE      RPFND         $$FND
     C                   MOVE      RPFYR         $$FYR
     C                   MOVE      RPCASE        $$CASE
     C                   eval      QXTRDT = RPTRDT
     C                   eval      QXFRDT = RPFRDT
     C                   eval      QXTODT = RPTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           $$MODE                         Mode
     C                   PARM                    $$CO#                          Company
     C                   PARM                    $$FND                          Group #
     C                   PARM                    $$FYR                          Year
     C                   PARM                    $$CASE                         Case
     C                   PARM      'WC'          $$APPL                         Applic Code
     C                   PARM      DATA          $$DATA                         Selection criteria
     C                   PARM      *BLANKS       $$CKTP                         Bank Acct Code
     C                   PARM      *BLANKS       $$RTCD                         Return Code
      *
     C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
      *
     C                   MOVEL(P)  EAIUSR        RPIUSR
     C                   MOVEL(P)  EAPGM         RPIPGM
     C                   eval      RPITM = EAITIM
     C                   eval      RPIDT = EAIDT
      *
     C                   if        RPAMT > *ZEROS
     C                   WRITE     PRP01
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
