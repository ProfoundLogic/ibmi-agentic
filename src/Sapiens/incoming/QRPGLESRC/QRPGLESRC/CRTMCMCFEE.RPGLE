    Hcopyright('(C) Copyright Fiserv, Inc.,  2007')
    H**********************************************************************
    H/title CRTMCMCFEE - Generates MCMC Fee Checks
    H**********************************************************************
    H* Created on xx/xx/xxxx by c:xxx for Project 139MHS
    H**********************************************************************
     H* Revisions:
     H*
    H**********************************************************************
     Ffxmfp     if   e             disk
     Ffmf1l     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwarpl     if a e           K dISk
     Ffaeal37   if   e           K disk
     Fwmcil     if   e           K disk
     Ffaeol4    if   e           K disk
     Fwmpdl     if   e           K disk
     Fsmcol     if   e             disk
     Fsmupp     if   e             disk
     Fwtpsl     if   e           K disk
     Fwtabl     if   e           K disk
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
     F*
     Fqsysprt1  o    f  198        printer infds(pr1ds)
     F                                     oflind(*inob)
     D*=====================================================================
     D* Data Structures
     D*=====================================================================
     D pr1ds           ds
     D  fill3                  1    200
     D  fill4                201    366
     D  x1line               367    368B 0
     D  x1page               369    372B 0
     D*
     Ddsclm#           ds            20
     Dkkfnd                    1      3  0
     Dkkfyr                    4      6  0
     Dkkcase                   7     13  0

     D                 ds
     D  data                   1     50
     D  qxtrdt                 1      7  0
     D  qxfrdt                 8     14  0
     D  qxtodt                15     21  0
     D  qxtrcd                22     23
     D  qxrecd                24     25
     D  qxpv1                 26     27
     D  qxpv2                 28     29

     D                 ds
     D  f1n$                   1     90  2
     D                                     dim(10)
     D  f1n$01                 1      9  2
     D  f1n$02                10     18  2
     D  f1n$03                19     27  2
     D  f1n$04                28     36  2
     D  f1n$05                37     45  2
     D  f1n$06                46     54  2
     D  f1n$07                55     63  2
     D  f1n$08                64     72  2
     D  f1n$09                73     81  2
     D  f1n$10                82     90  2

     D                 ds
     D  f1nf                   1     10
     D                                     dim(10)
     D  f1nf01                 1      1
     D  f1nf02                 2      2
     D  f1nf03                 3      3
     D  f1nf04                 4      4
     D  f1nf05                 5      5
     D  f1nf06                 6      6
     D  f1nf07                 7      7
     D  f1nf08                 8      8
     D  f1nf09                 9      9
     D  f1nf10                10     10

     D                 ds
     D  f1nw                   1     50
     D                                     dim(10)
     D  f1nw01                 1      5
     D  f1nw02                 6     10
     D  f1nw03                11     15
     D  f1nw04                16     20
     D  f1nw05                21     25
     D  f1nw06                26     30
     D  f1nw07                31     35
     D  f1nw08                36     40
     D  f1nw09                41     45
     D  f1nw10                46     50

     D                 ds
     D  f1ni                   1     90  0
     D                                     dim(10)
     D  f1ni01                 1      9  0
     D  f1ni02                10     18  0
     D  f1ni03                19     27  0
     D  f1ni04                28     36  0
     D  f1ni05                37     45  0
     D  f1ni06                46     54  0
     D  f1ni07                55     63  0
     D  f1ni08                64     72  0
     D  f1ni09                73     81  0
     D  f1ni10                82     90  0

     D                 ds
     D  f1nt                   1     10
     D                                     dim(10)
     D  f1nt01                 1      1
     D  f1nt02                 2      2
     D  f1nt03                 3      3
     D  f1nt04                 4      4
     D  f1nt05                 5      5
     D  f1nt06                 6      6
     D  f1nt07                 7      7
     D  f1nt08                 8      8
     D  f1nt09                 9      9
     D  f1nt10                10     10

     D                 ds
     D  f1nm                   1     30  0
     D                                     dim(10)
     D  f1nm01                 1      3  0
     D  f1nm02                 4      6  0
     D  f1nm03                 7      9  0
     D  f1nm04                10     12  0
     D  f1nm05                13     15  0
     D  f1nm06                16     18  0
     D  f1nm07                19     21  0
     D  f1nm08                22     24  0
     D  f1nm09                25     27  0
     D  f1nm10                28     30  0

     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
     D*=====================================================================
     D* Variable Definitions
     D*=====================================================================
     D e               s              3  0
     D x               s              3  0
     D ##leng          s              3  0
     D errcnt          s              9  0 inz(0)
     D*
     D ##co#           s                   like(upco#)
     D ##sub#          s                   like(upsub#)
     D ##fnd           s                   like(upfnd)
     D ##fyr           s                   like(upfyr)
     D ##conm          s                   like(coname)
     D ##fnam          s                   like(coname)

     D kkco#           s                   like(rpco#)
     D kkptyp          s                   like(ciptyp)
     D kksys           s                   like(cisys)
     D kkkey           s                   like(pckey)
     D kkmfid          S                   like(mftrid)
     D kksinv          s                   like(easinv)
     D hdseq#          S                   like(rpseq#)

     D errmsg          S             20    dim(6) ctdata perrcd(1)              ERROR MESSAGE

     C*=====================================================================
     C* Parameter Lists
     C*=====================================================================
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*=====================================================================
     C* Key Lists
     C*=====================================================================
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    c*
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksinv
    c*
     C     eoky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
    c*
     C     f1ky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C*
     C     ciky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkptyp
     C                   kfld                    kksys
     C*
     C     pdky          klist
     C                   kfld                    rpfein
     C                   kfld                    rpmod
     C*
     C     rpky          klist
     C                   kfld                    eaCO#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C*
     C     tbky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C*=====================================================================
     C* Main Processing
     C*=====================================================================
     C                   time                    dsdttm
     C*
     C                   call      'FA007R'
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C                   move      ##co#         kkco#
     C                   except    rphd1
     C                   except    rphd2
     C*
     C* Read the Fee File
     C*
     c                   read      pmf01
     c                   dow       not %eof
     C*
     C* Is this a valid claim?
     C*
     C                   eval      dsclm# = mfclm#
     c     cmky          chain     pcm01
     c                   if        %found
     C*
     C* Does the bill exist?
     C*
     C                   eval      kksinv = mftrid
     c     eaky          chain     pea01
     C                   if        %found
     C*
     C* Write Check Request
     C*
     C                   exsr      srntwk
     c                   else
     C*
     C* Invalid Bill, write to error report
     C*
     c                   z-add     2             e
     c                   exsr      wrterr
     C                   endif                                                  *in51 = *off
     c                   else
     C*
     C* Invalid Claim, write to error report
     C*
     c                   z-add     1             e
     c                   exsr      wrterr
     C                   endif                                                  *in50 = *off
     C*
     C* Read next fee record
     c*
     c                   read      pmf01
     C                   enddo
     C*
     C                   if        errcnt = 0
     C                   except    errerr
     C                   endif
     C*
     C                   except    totals
     C*
     C                   seton                                        lr
     C******************************************************************
     C*  SRNTWK - This subroutine creates the network payment.
     C******************************************************************
     C     srntwk        begsr
     C*
     C*  Determine Next sequence number for payment requests
     C*
     C     rpky          setgt     prp01
     C     rpky          readpe(n) prp01                                  60
     C                   if        *in60
     C                   eval      hdseq# = 1
     C                   else
     C                   eval      hdseq# = rpseq# + 1
     C                   endif
     C*
     C                   clear                   prp01
     C*
     C* Create the payment request record.
     C*
     C     eoky          chain     peo01
     C*
     C                   z-add     1             x                 3 0
     C                   z-add     eaco#         rpco#
     C                   z-add     eafnd         rpfnd
     C                   z-add     eafyr         rpfyr
     C                   z-add     eacase        rpcase
     C                   z-add     hdseq#        rpseq#
     C                   z-add     eotrdt        rptrdt
     C                   z-add     mfamt         rpamt
     C*
     C     f1ky          chain     pf101
     C*
     C     eontwk        lookup    f1nw(x)                                61
     c  n61              z-add     4             e
     c  n61              exsr      wrterr
     C  n61              goto      nxtc
     C*
     C* If inhouse network:
     C*
B01  C     f1nf(x)       ifeq      'I'
     C*
     C* Retrieve WCMC Check Issuance Information:
     C*
     C                   move      'B'           kkptyp
     C                   move      'N'           kksys
     C     ciky          chain     pci01                              60
     C   60              goto      nxtc
     C*
     C                   z-add     cifein        rpfein
     C                   z-add     cimod         rpmod
     C*
     C* Outside Network
     C*
S01  C                   else
     C*
     C* Retrieve WCMC Check Issuance Information:
     C*
     C                   movel     'N'           kkptyp
     C                   movel     eosys         kksys
     C     ciky          chain     pci01                              60
     c   60              z-add     5             e
     c   60              exsr      wrterr
     C   60              goto      nxtc
     C*
     C* Establish Payee Fein/Mod
     C*
     C                   z-add     f1ni(x)       rpfein
     C                   z-add     f1nm(x)       rpmod
E01  C                   endif
     C*
     C     cmcvfl        ifeq      'Y'
     C                   move      cip1cv        rpbr2
     C                   else
     C                   move      cibr2         rpbr2
     C                   endif
     C*
     C     cmltfl        ifeq      'I'
     C                   move      cibr7l        rpbr7
     C                   else
     C                   move      cibr7n        rpbr7
     C                   endif
     C*
     C                   move      cmncci        rpncci
     C                   z-add     eofrdt        rpfrdt
     C                   z-add     eotodt        rptodt
     C*
     C     eatrcd        ifne      *blanks
     C                   move      eatrcd        rptrcd
     C                   else
     C     cmcvfl        ifne      'Y'
     C                   movel(p)  citrcd        rptrcd
     C                   else
     C                   movel(p)  citrcv        rptrcd
     C                   endif
     C                   endif

     C                   movel(p)  rptrcd        kkkey
     C     tbky          chain     ppc01                              60
     C     *in60         ifeq      '0'
     C                   move      pcrecd        rprecd
     C                   move      pctrtp        rptype
     C                   movel     pcdesc        rpdesc
     C                   endif
     C*
     C                   movel(p)  rprecd        kkkey
     C     tbky          chain     prt01                              60
     C     *in60         ifeq      '0'
     C                   move      rttype        rpretp
     C                   endif
     C*
     C                   movel(p)  eacntl        rprmds
     C*
     C     pdky          chain     ppd01                              60
     c   60              z-add     6             e
     c   60              exsr      wrterr
     C   60              goto      nxtc

     C     pddba1        ifne      'Y'
     C                   move      pdname        rppnam
     C                   move      pdadr1        rpadr1
     C                   move      pdadr2        rpadr2
     C                   else
     C                   move      pdadr1        rppnam
     C                   move      pdadr2        rpadr1
     C                   move      *blanks       rpadr2
     C                   endif
     C                   move      pdcty         rpcty
     C                   move      pdst          rpst
     C                   move      pdzip         rpzip
     C     pdadr3        ifne      *blanks
     C                   movel(p)  pdctry        rpctry
     C                   movel(p)  pdadr3        rpadr3
     C                   endif
     C                   z-add     eaeob#        rpeob#
     C                   move      'P'           rpstat
     C                   move      'G'           rppgtp
     C*
     C                   move      eaiusr        rpuser
     C                   move      q1pgm         rppgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rpdt
     C                   time                    dsdttm
     C                   z-add     dstime        rptime
     C*
     C                   z-add     eocrdt        rprcvd
      *
      *  Call API to determine the Bank "Code".
      *
     C                   move      rpco#         ##co#
     C                   move      rpfnd         ##fnd
     C                   move      rpfyr         ##fyr
     C                   move      rpcase        ##case
     C                   z-add     rptrdt        qxtrdt
     C                   z-add     rpfrdt        qxfrdt
     C                   z-add     rptodt        qxtodt
     C                   movel     rptrcd        qxtrcd
     C                   movel     rprecd        qxrecd
     C                   movel     rpbr2         qxpv1
     C                   movel     rpbr7         qxpv2
      *
     C                   call      'WA023R1'
     C                   parm      '2'           ##mode            1            Mode
     C                   parm                    ##co#                          Company
     C                   parm                    ##fnd                          Group #
     C                   parm                    ##fyr                          Year
     C                   parm                    ##case            7            Case
     C                   parm      'WC'          ##appl            2            Applic Code
     C                   parm      data          ##data           50            Selection criteria
     C                   parm                    ##cktp           10            Bank Acct Code
     C                   parm                    ##rtcd            1            Return Code
      *
     C                   move      ##cktp        rpcktp                         Bank Acct# Code
     C     eackst        ifeq      *blanks
     C                   movel     cists         rpsts
     C                   else
     C     eackst        ifeq      'PY'
     C                   movel     cists         rpsts
     C                   else
     C                   move      eackst        rpsts
     C                   endif
     C                   endif
     C*
     C                   movel(p)  rpsts         kkkey
     C     tbky          chain     pps01                              60
     C     *in60         ifeq      '0'
     C                   move      pssrt         rpsort
     C                   endif
     C*
     C     f1nf(x)       ifeq      'I'
     C                   move      'I'           rpntwk
     C                   endif
     C*
     C                   move      'N'           rpsent
     C                   move      eaiusr        rprqst
     C                   z-add     earcn         rpeobs
     C                   z-add     rpdt          rpidt
     C                   z-add     rptime        rpitm
     C                   movel     rpuser        rpiusr
     C                   movel     q1pgm         rpipgm
     C*
     C     rpstat        ifeq      ' '
     C                   z-add     rpdt          rprdt
     C                   z-add     rptime        rptime
     C                   movel     rpuser        rprusr
     C                   movel     rppgm         rprpgm
     C                   endif
     C*
     C     rpamt         ifgt      0
     C                   write     prp01
     C                   else
     c                   z-add     3             e
     c                   exsr      wrterr
     C                   endif
     C*
     C     nxtc          tag
     C*
     C                   endsr
     C******************************************************************
     C*  WRTERR - Write error
     C******************************************************************
     C     wrterr        begsr
     C*
     c                   add       1             errcnt
     c     *inob         ifeq      *on
     C                   except    rphd1
     C                   except    rphd2
     C                   eval      *inob = *off
     C                   endif                                                  *in51 = *off
     C                   except    errmesg
     C*
     C                   endsr
     O*=====================================================================
     O* Report Layout
     O*=====================================================================
     Oqsysprt1  E            rphd1          1 06
     O                       dsdate        Y     10
     O                       ##conm              86
     O                       dstime             129 '  :  :  '
     O          E            rphd1          2
     O                                           10 'CRTMCMCFEE'
     O                                           74 'MCMC Fee Checks'
     O                       x1page        Z    129
     O                                          122 'Page:'
     O          E            rphd2       1  1
     O                                           15 'MCMC Bill ID   '
     O                                           23 'Claim#'
     O                                           49 'Fee Amount'
     O                                           63 'Error Reason'
     O          E            errmesg        1
     O                       mftrid              15
     O                       mfclm#            +002
     O                       mfamt         4   +002
     O                       errmsg(e)         +002
     O          E            errerr         1
     O                                            9 'No errors'
     O          E            totals      1  1
     O                                            6 'Count:'
     O                       errcnt            +001 '       0 '
     O*
** (ERRMSG)
Invalid Claim
Unable to find bill
Amount was zero
Network not setup in group
WCMC Check Issuance not setup
Invalid Provider
