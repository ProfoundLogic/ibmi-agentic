     Ffaeal1    uf   e           k disk
     Ffaeol3    if   e           k disk
     Fwmcil     if   e           k disk
     Fwmcml1    uf   e           k disk
     Fwmpdl     if   e           k disk
     Fwarpl     if A e           k disk    infds(dbfds1)
     Fwtpsl     if   e           k disk
     Fwtabl     if   e           k disk
     F                                     ignore(PAT01)
     F                                     ignore(PSP01)
     F                                     ignore(PNC01)
     F                                     ignore(PNA01)
     F                                     ignore(PPA01)
     F                                     ignore(PCA01)
     F                                     ignore(PRC01)
     F                                     ignore(PB201)
     F                                     ignore(PB701)
     F                                     ignore(PBS01)
     F                                     ignore(PTT01)
     F                                     ignore(PCC01)
     F                                     ignore(PBC01)
     F                                     ignore(PCS01)
     F                                     ignore(PCN01)
     F                                     ignore(PRS01)
     F                                     ignore(PLR01)
     F                                     ignore(PVS01)
     F                                     ignore(PTR01)
     F                                     ignore(PHY01)
     F                                     ignore(PSY01)
     F                                     ignore(PBA01)
     F                                     ignore(PID01)
     F                                     ignore(PIS01)
     Fwdpal     if a e           k disk
     Fwdrpl     o    e           k disk    rename(prp01:prp02)
     Fwdell     uf   e           k disk
     Fwmrhp     o    e             disk
     f*
     d*=====================================================================
     d* Data Structures
     d*=====================================================================
     d fids01        e ds                  extname(SZQ2P)
     d psds          esds                  extname(SZQ1P)

     d dbfds1          ds
     d  dbrrn                397    400B 0

     d                 ds
     d  dsdttm                 1     12  0
     d  dstime                 1      6  0
     d  dsdate                 7     12  0

     d                 ds
     d  data                   1     50
     d  qxtrdt                 1      7  0
     d  qxfrdt                 8     14  0
     d  qxtodt                15     21  0
     d  qxtrcd                22     23
     d  qxrecd                24     25
     d  qxpv1                 26     27
     d  qxpv2                 28     29

     d                 DS
     d  rr                     1     90  2
     d                                     dim(10)
     d  cmrr01                 1      9  2
     d  cmrr02                10     18  2
     d  cmrr03                19     27  2
     d  cmrr04                28     36  2
     d  cmrr05                37     45  2
     d  cmrr06                46     54  2
     d  cmrr07                55     63  2
     d  cmrr08                64     72  2
     d  cmrr09                73     81  2
     d  cmrr10                82     90  2

     d                 DS
     d  rt                     1     20
     d                                     dim(10)
     d  cmrt01                 1      2
     d  cmrt02                 3      4
     d  cmrt03                 5      6
     d  cmrt04                 7      8
     d  cmrt05                 9     10
     d  cmrt06                11     12
     d  cmrt07                13     14
     d  cmrt08                15     16
     d  cmrt09                17     18
     d  cmrt10                19     20

     d                 DS
     d  AR                     1     90  2
     d                                     dim(10)
     d  cmar01                 1      9  2
     d  cmar02                10     18  2
     d  cmar03                19     27  2
     d  cmar04                28     36  2
     d  cmar05                37     45  2
     d  cmar06                46     54  2
     d  cmar07                55     63  2
     d  cmar08                64     72  2
     d  cmar09                73     81  2
     d  cmar10                82     90  2

     d                 DS                  INZ
     d  rrt                    1     20
     d                                     dim(10)
     d  rhrt01                 1      2
     d  rhrt02                 3      4
     d  rhrt03                 5      6
     d  rhrt04                 7      8
     d  rhrt05                 9     10
     d  rhrt06                11     12
     d  rhrt07                13     14
     d  rhrt08                15     16
     d  rhrt09                17     18
     d  rhrt10                19     20

     d                 ds                  inz
     d  rsu                    1     90  2
     d                                     dim(10)
     d  rhsu01                 1      9  2
     d  rhsu02                10     18  2
     d  rhsu03                19     27  2
     d  rhsu04                28     36  2
     d  rhsu05                37     45  2
     d  rhsu06                46     54  2
     d  rhsu07                55     63  2
     d  rhsu08                64     72  2
     d  rhsu09                73     81  2
     d  rhsu10                82     90  2

     d*=====================================================================
     d* Like defines
     d*=====================================================================
     d kkeobs          s                   like(paeobs)
     d xxseq#          s                   like(paseq#)
     d hdbamt          s                   like(eabamt)
     d xxptyp          s                   like(ciptyp)
     d xxsys           s                   like(cisys)
     d xxfein          s                   like(eafein)
     d xxmod           s                   like(eamod)
     d xxkey           s                   like(pckey)
     d x               s              3  0

     d*=====================================================================
     d* Constants
     d*=====================================================================
     d desc1           c                   const('Adjustment due to in-
     d                                     creased BRS fee')
     d*
     c*=====================================================================
     c* Parameter Lists
     c*====================================
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
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANK        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     c*=====================================================================
     c* Key Lists
     c*=====================================================================
     c     cmky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eafyr
     c                   kfld                    eacase
     c*
     c     eoky          klist
     c                   kfld                    eaco#
     c                   kfld                    easub#
     c                   kfld                    eaeob#
     c*
     C     paky          KLIST
     C                   KFLD                    paco#
     C                   KFLD                    pafnd
     C                   KFLD                    pafyr
     C                   KFLD                    pacase
     d*
     c     rpky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    eafyr
     c                   kfld                    eacase
     c*
     c     ciky          klist
     c                   kfld                    eaco#
     c                   kfld                    eafnd
     c                   kfld                    xxptyp
     c                   kfld                    xxsys
     c*
     c     pdky          klist
     c                   kfld                    xxfein
     c                   kfld                    xxmod
     c*
     c     tbky          KLIST
     c                   KFLD                    eaco#
     c                   KFLD                    xxkey
     c*
     c     elky          KLIST
     c                   KFLD                    eaco#
     c                   KFLD                    eafnd
     c                   KFLD                    cmemp#
     c                   KFLD                    cmdiv
     c                   KFLD                    cmfyr
     c*=====================================================================
     c* Main Process
     c*=====================================================================
     c                   time                    dsdttm
     c*
     c                   read      pea01
     c                   dow       not %eof
     c                   if        earcn = 0 and eabamt < 0
     c*
     C     eoky          chain     peo01
     C     cmky          chain(n)  pcm01
     c*
     c                   eval      hdbamt = eabamt * -1
     c                   eval      eabamt = (eamra + eaothr + eaoth2) * .17
     c                   eval      eabcam = eabamt
     c*
     c                   if        eabamt < 0
     c                   eval      eabamt = 0
     c                   eval      eabcam = 0
     c                   endif
     c*
     c                   exsr      wrtbrs
     c                   exsr      adjamt
     c*
     c                   endif
     c                   update    pea01
     c                   read      pea01
     c                   enddo
     c*
     c                   seton                                        lr
     C*===============================================================*
     C*  ADJAMT - Adjust the payment
     C*
     C*===============================================================*
     C     ADJAMT        BEGSR
     C*
     C     PAKY          SETGT     PPA01
     C     PAKY          READPE(N) PPA01                                  60
     C  N60              Z-ADD     PASEQ#        XXSEQ#
     C   60              Z-ADD     0             XXSEQ#
     C                   CLEAR                   PPA01
     C*
     C*
     C     XXSEQ#        ADD       1             PASEQ#
     C*
     C                   Z-ADD     HDBAMT        PAAMT
     C                   Z-ADD     EACO#         PACO#
     C                   Z-ADD     EAFND         PAFND
     C                   Z-ADD     EAFYR         PAFYR
     C                   Z-ADD     EACASE        PACASE
     C                   Z-ADD     0             PACHK#
     C                   Z-ADD     0             PABASQ
     C                   Z-ADD     RPFEIN        PAFEIN
     C                   Z-ADD     RPMOD         PAMOD
     C*
     C                   Z-ADD     RPDT          PATRDT
     C                   Z-ADD     RPDT          PAPODT
     C                   Z-ADD     RPFRDT        PAFRDT
     C                   Z-ADD     RPTODT        PATODT
     C                   Z-ADD     0             PAWKS
     C                   Z-ADD     0             PADAYS
     C*
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C*
     C     *LOVAL        SETLL     PPC01
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C     PCRECD        IFEQ      RPRECD
     C     PCTRTP        ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF
     C                   READ      PPC01                                  60
     C                   ENDDO
     C*
     C  N60              MOVEL     PCKEY         PATRCD
     C   60              MOVE      RPTRCD        PATRCD
     C                   MOVEL     DESC1         PADESC
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C                   Z-ADD     RPEOB#        PAEOB#
     C                   Z-ADD     RPEOBS        PAEOBS
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     RPDT          PADT
     C                   Z-ADD     RPTIME        PATIME
     C                   MOVEL     Q1USER        PAIUSR
     C                   MOVEL     Q1PGM         PAIPGM
     C                   Z-ADD     RPDT          PAIDT
     C                   Z-ADD     RPTIME        PAITIM
     C*
     C                   WRITE     PPA01
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVE      *BLANKS       RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   WRITE     PRP02
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C     cmky          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
     C*
     C     CMSTAT        IFEQ      'C'
     C     PAAMT         ORGT      RR(X)
     C                   EXSR      RSVHST
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C                   ADD       PAAMT         AR(X)
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'O'
     C                   SUB       PAAMT         ELOUTS
     C                   ENDIF                                                  CMSTAT=O
     C                   ADD       PAAMT         ELTCLP
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================*
     C*  RSVHST - Write a reserve history record
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   ADJUSTMENT TO KEEP CLAIM MSTR (WMCMP) AND RSRV HIST (WMRHP)
     C*   IN BALANCE.
     C*===============================================================*
     C     RSVHST        BEGSR
     C*
     C                   CLEAR                   PRH01
     C*
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C                   Z-ADD     1             Y                 3 0
     C     PARECD        LOOKUP    RT(Y)                                  99
     C                   MOVEL     PARECD        RRT(Y)
     C                   Z-ADD     PAAMT         RSU(Y)
     C                   MOVEL     DESC1         RHEXPL
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVEL     SRDTO         RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR                                                  RSVHST
     C******************************************************************
     C*  WRTBRS - This subroutine creates the bill review service pmt.
     C******************************************************************
     C     WRTBRS        BEGSR
     C*
     C*  Determine Next sequence number for payment requests
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE(N) PRP01                                  60
     C   60              Z-ADD     0             RPSEQ#
     C*
     C*  Retrieve the BRS WCMC check issuance record.
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      EASYS         XXSYS
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     EACO#         RPCO#
     C                   Z-ADD     EAFND         RPFND
     C                   Z-ADD     EAFYR         RPFYR
     C                   Z-ADD     EACASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EATRDT        RPTRDT
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C                   Z-ADD     EABAMT        RPAMT
     C*
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C*
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   MOVEL     CIFEIN        XXFEIN
     C                   MOVEL     CIMOD         XXMOD
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C*
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
     C                   MOVE      EAUSER        RPUSER
     C                   MOVEL     *BLANKS       RPCKCN
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   ELSE
     C                   Z-ADD     EOTRDT        RPRCVD
     C                   ENDIF
     C*
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    ##CO#             3            Company
     C                   PARM                    ##FND             3            Group #
     C                   PARM                    ##FYR             3            Year
     C                   PARM                    ##CASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C                   PARM      DATA          ##DATA           50            Selection criteria
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   MOVE      *BLANK        RPACCT
     C                   MOVEL     CISTS         RPSTS
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVEL     EAUSER        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C                   MOVE      'B'           RPNTWK
     C                   Z-ADD     EORCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     RPAMT         IFGT      0
     C     EAPBRC        ANDNE     'N'
     C                   WRITE     PRP01
     C                   ENDIF                                                  RPAMT GT 0
     C*
     C                   ENDSR
