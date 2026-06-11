     H* (C) Copyright Fiserv, Inc., 2004 - 2008
    H*
    H/TITLE F8010R - WCMC MONTH-END PROGRAM
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H*****************************************************************
    H*  CREATED ON 08/09/2004 BY C:62 FOR PROJECT 114IIE             *
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*  02/04/08 c:190 Project 10UKC                                 *
    H*                Added new fields.                              *
    H*                                                               *
    H*  06/23/06 c:62 Support PQ010 53241                            *
    H*                Populate Pharmacy License Number               *
    H*                                                               *
    H*  02/24/06 c:62 Support MC277                                  *
    H*                Additional changes.                            *
    H*                                                               *
    H*****************************************************************
     Ffaeal1    if   e           k disk
     Ffaeol4    if   e           k disk
     Ffaedl     if   e           k disk
     Ffaehl     if   e           k disk
     Ffaepl     if   e           k disk
     Ffaeul     if   e           k disk
    F*
     Ffdcalf    if   e           k disk
     Ffddal4    if   e           k disk
     Ffdddl     if   e           k disk
     Ffddhl     if   e           k disk
     Ffddpl     if   e           k disk
     Ffddul     if   e           k disk
     F*** begin add *** 02/24/06 *******************************************
     Ffddel     if   e           k disk
     F*** end   add *** 02/24/06 *******************************************
     F*** begin add *** 02/04/08 *******************************************
     Ffmdil     if   e           k disk
     F*** end   add *** 02/04/08 *******************************************
    F*
     Fqfemp     o    e             disk
    D*
     D Write_QFEMP     pr
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
    D*
     D @@frdt          s              8
     D @@todt          s              8
    C*****************************************************************
    C*   PARAMETER LISTS INCLUDING ENTRY PARMS
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@frdt
     C                   parm                    @@todt
    C*****************************************************************
    C*   Initialize
    C*****************************************************************
    C*
     C                   read      pea01
     C                   dow       not %eof(faeal1)
    C*
     C                   callp     write_qfemp
    C*
     C                   read      pea01
     C                   enddo
    C*
     C                   eval      *inlr = *on
    C*------------------------------------------------------------------------
    C* Write_QFEMP
    C*------------------------------------------------------------------------
     P Write_QFEMP     B
     D Write_QFEMP     PI

     *** Locals ***
     D date_mdyy       s               d   datfmt(*usa)
     D date_yymd       s               d   datfmt(*iso)
     D frdt_In         s              8  0
     D todt_In         s              8  0
     D count           s              1  0
    C*
     C     eobky         klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
    C*
     C     daky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
     C                   kfld                    eafyr
     C                   kfld                    eacase
     C                   kfld                    eaeob#
     C                   kfld                    earcn
    C*
     C     detky         klist
     C                   kfld                    daco#
     C                   kfld                    dasub#
     C                   kfld                    dafnd
     C                   kfld                    dafyr
     C                   kfld                    dacase
     C                   kfld                    daeob#
     C                   kfld                    dadseq
    C*
     C     linky         klist
     C                   kfld                    daco#
     C                   kfld                    dasub#
     C                   kfld                    dafnd
     C                   kfld                    dafyr
     C                   kfld                    dacase
     C                   kfld                    dadseq
     C*** begin add *** 02/24/06 *******************************************
    C*
     C     deky          klist
     C                   kfld                    daco#
     C                   kfld                    dasub#
     C                   kfld                    dafnd
     C                   kfld                    dafyr
     C                   kfld                    dacase
     C                   kfld                    daeob#
     C                   kfld                    dadseq
     C*** end   add *** 02/24/06 *******************************************
     C*** begin add *** 02/04/08 *******************************************
    C*
     C     diky          klist
     C                   kfld                    DIICD9
     C*** end   add *** 02/04/08 *******************************************

     C                   clear                   pem01

     C*
     C* Header Information
     C*
     C                   eval      emco# = eaco#
     C                   eval      emsub# = easub#
     C                   eval      emfnd = eafnd
     C                   eval      emfyr = eafyr
     C                   eval      emcase = eacase
     C                   eval      emeob# = eaeob#
     C                   eval      emrcn = earcn
     C                   eval      emfein = eafein
     C                   eval      emmod = eamod
     C                   eval      empnam = eapnam
     C*
     C                   if        eatrdt <> 0
     C     *cymd         move      eatrdt        emtrdt
     C                   endif
     C*
     C                   eval      emwcpc = eawcpc
     C*
     C                   if        eawcpd <> 0
     C                   move      eawcpd        emwcpd
     C                   endif
     C*
     C                   eval      emwcbc = eawcbc
     C*
     C                   if        eawcbd <> 0
     C                   move      eawcbd        emwcbd
     C                   endif
     C*
     C                   eval      emwcnc = eawcnc
     C*
     C                   if        eawcnd <> 0
     C                   move      eawcnd        emwcnd
     C                   endif
     C*
     C                   eval      embamt = eabamt
     C                   eval      emnamt = eanamt
     C                   eval      embinv = eabinv
     C                   eval      emninv = eaninv
     C*
     C                   if        eabidt <> 0
     C     *cymd         move      eabidt        embidt
     C                   endif
     C                   if        eanidt <> 0
     C     *cymd         move      eanidt        emnidt
     C                   endif
     C*
     C     eobky         chain     peo01
     C                   if        %found
     C                   eval      emsys = eosys
     C                   eval      embtyp = eobtyp
     C                   eval      emast = eoast
     C                   eval      emss# = eoss#
     C*** begin add *** 02/04/08 *******************************************
     C                   eval      empost= eopost
     C                   eval      emuser= eouser
     C                   eval      embnpi= eobnpi
     C*** end   add *** 02/04/08 *******************************************
     C                   if        eoacdt <> 0
     C     *cymd         move      eoacdt        emacdt
     C                   endif
     C                   eval      emntwk = eontwk
     C                   eval      emspcd = eospcd
     C                   eval      emlsn = eolsn
     C                   eval      emfsn = eofsn
     C                   if        eofrdt <> 0
     C     *cymd         move      eofrdt        emfrdt
     C                   endif
     C                   if        eotodt <> 0
     C     *cymd         move      eotodt        emtodt
     C                   endif
     C*** begin add *** 02/24/06 *******************************************
     C                   if        eotrdt <> 0
     C     *cymd         move      eotrdt        emtrd2
     C                   endif
     C                   eval      emblst = eoblst
     C*** end   add *** 02/24/06 *******************************************
     C                   endif

     C                   select
     C*
     C* Dental Header
     C*
     C                   when      eobtyp = 'D'
     C     eobky         chain     ped01
     C                   if        %found
     C                   eval      emdia1 = eddia1
     C                   eval      emdia2 = eddia2
     C                   eval      emdia3 = eddia3
     C                   eval      emdia4 = eddia4
     C*** begin add *** 02/24/06 *******************************************
     C                   eval      emlic# = edlic#
     C                   eval      emalic = *blanks
     C                   eval      emflic = edflic
     C*** end   add *** 02/24/06 *******************************************
     C*** begin add *** 02/04/08 *******************************************
     C                   move      eddia1        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid1= didsc1
     C                   endif                                                  eddia1
     C*
     C                   move      eddia2        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid2= didsc1
     C                   endif                                                  eddia2
     C*
     C                   move      eddia3        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid3= didsc1
     C                   endif                                                  eddia3
     C*
     C                   move      eddia4        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid4= didsc1
     C                   endif                                                  eddia4
     C*
     C*** end   add *** 02/04/08 *******************************************
     C                   endif
     C*
     C* Physician Header
     C*
     C                   when      eobtyp = 'N'
     C     eobky         chain     peh01
     C                   if        %found
     C                   if        ehdfc <> 0
     C     *cymd         move      ehdfc         emdfc
     C                   endif
     C                   eval      emdia1 = ehdia1
     C                   eval      emdia2 = ehdia2
     C                   eval      emdia3 = ehdia3
     C                   eval      emdia4 = ehdia4
     C*** begin add *** 02/24/06 *******************************************
     C                   eval      emlic# = ehlic#
     C                   eval      emalic = ehalic
     C                   eval      emflic = ehflic
     C*** end   add *** 02/24/06 *******************************************
     C*** begin add *** 02/04/08 *******************************************
     C                   move      ehdia1        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid1= didsc1
     C                   endif                                                  ehdia1
     C*
     C                   move      ehdia2        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid2= didsc1
     C                   endif                                                  ehdia2
     C*
     C                   move      ehdia3        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid3= didsc1
     C                   endif                                                  ehdia3
     C*
     C                   move      ehdia4        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid4= didsc1
     C                   endif                                                  ehdia4
     C*
     C*** end   add *** 02/04/08 *******************************************
     C                   endif
     C*
     C* Pharmacy Header
     C*
     C                   when      eobtyp = 'X'
     C     eobky         chain     pep01
     C                   if        %found
     C                   eval      emphar = epphar
     C*** begin add *** 02/04/08 *******************************************
     C                   eval      emplic = epplic
     C*** end   add *** 02/04/08 *******************************************
     C                   endif
     C*
     C* Hospital Header
     C*
     C                   when      eobtyp = 'H'
     C     eobky         chain     peu01
     C                   if        %found
     C                   eval      emdia1 = eudia1
     C                   eval      emdia2 = eudia2
     C                   eval      emdia3 = eudia3
     C                   eval      emdia4 = eudia4
     C                   eval      emblty = eublty
     C*** begin add *** 02/04/08 *******************************************
     C                   eval      emdia5 = eudia5
     C                   eval      emdia6 = eudia6
     C                   eval      emdia7 = eudia7
     C                   eval      emdia8 = eudia8
     C                   eval      emprc3 = euprc3
     C                   eval      emprc4 = euprc4
     C                   eval      emprc5 = euprc5
     C                   eval      emcau1 = eucau1
     C                   eval      emcau2 = eucau2
     C                   eval      emcau3 = eucau3
     C                   eval      emolic = euolic
     C                   eval      emschd = euschd
     C*
     C                   move      eudia1        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid1= didsc1
     C                   endif                                                  eudia1
     C*
     C                   move      eudia2        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid2= didsc1
     C                   endif                                                  eudia2
     C*
     C                   move      eudia3        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid3= didsc1
     C                   endif                                                  eudia3
     C*
     C                   move      eudia4        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid4= didsc1
     C                   endif                                                  eudia4
     C*
     C                   move      eudia5        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid5= didsc1
     C                   endif                                                  eudia5
     C*
     C                   move      eudia6        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid6= didsc1
     C                   endif                                                  eudia6
     C*
     C                   move      eudia7        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid7= didsc1
     C                   endif                                                  eudia7
     C*
     C                   move      eudia8        diicd9
     C     diky          chain     PDI01
     C                   if        %found
     C                   eval      emdid8= didsc1
     C                   endif                                                  eudia8
     C*** end   add *** 02/04/08 *******************************************
     C*
     C                   if        euscpf <> 0
     C     *cymd         move      euscpf        emscpf
     C                   endif
     C                   if        euscpt <> 0
     C     *cymd         move      euscpt        emscpt
     C                   endif
     C                   if        euaddt <> 0
     C     *cymd         move      euaddt        emaddt
     C                   endif
     C                   if        euprd1 <> 0
     C     *cymd         move      euprd1        emprd1
     C                   endif
     C                   if        eudcdt <> 0
     C     *cymd         move      eudcdt        emdcdt
     C                   endif
     C*
     C                   eval      emadty = euadty
     C                   eval      emprc1 = euprc1
     C                   eval      emprc2 = euprc2
     C                   eval      emtfei = eutfei
     C                   eval      emtmod = eutmod
     C                   eval      emmcar = eumcar
     C*** begin add *** 02/24/06 *******************************************
     C                   eval      emlic# = eulic#
     C                   eval      emalic = eualic
     C                   eval      emflic = euflic
     C*** end   add *** 02/24/06 *******************************************
     C*
     C                   endif
     C                   endsl

     C*
     C* Detail Information
     C*
     C     daky          chain     pda01
     C                   if        %found
     C                   dou       %eof(fddal4)
     C*
     C                   eval      emdseq = dadseq
     C                   eval      emchrg = dachrg
     C                   eval      emmra  = damra
     C                   eval      emndis = dandis
     C                   eval      emothr = daothr
     C                   eval      emoth2 = daoth2
     C                   eval      empaid = dapaid

     C*** begin add *** 02/24/06 *******************************************
     C                   eval      count = 1
     C     deky          chain     pde01
     C                   if        %found
     C                   dou       %eof(fddel)
     C*
     C                   select
     C                   when      count = 1
     C                   eval      emeob1 = deeob
     C                   when      count = 2
     C                   eval      emeob2 = deeob
     C                   when      count = 3
     C                   eval      emeob3 = deeob
     C                   when      count = 4
     C                   eval      emeob4 = deeob
     C                   when      count = 5
     C                   eval      emeob5 = deeob
     C                   when      count = 6
     c                   leave
     C                   endsl
     C*
     C                   eval      count = count + 1
     C*
     C     deky          reade     pde01
     C                   enddo
     C                   endif
     C*** end   add *** 02/24/06 *******************************************
     C*

     C     linky         chain     pca01
     C                   if        %found
     C                   if        cafdat <> 0
     C     *cymd         move      cafdat        emfdat
     C                   endif
     C                   if        catdat <> 0
     C     *cymd         move      catdat        emtdat
     C                   endif
     C*
     C                   eval      emzipa = cazipa
     C                   eval      emot1c = caot1c
     C                   eval      emot2c = caot2c
     C                   eval      emmraa = camraa
     C                   eval      emmnaa = camnaa
     C*
     C                   endif

     C                   select
     C*
     C* Dental Detail
     C*
     C                   when      eobtyp = 'D'
     C     detky         chain     pdd01
     C                   if        %found
     C                   eval      empos = ' ' + %subst(ddpos:1:2)
     C                   eval      emprcd = ddprcd
     C                   eval      emprds = ddprds
     C                   eval      emdxr1 = dddxr1
     C                   eval      emdxr2 = dddxr2
     C                   eval      emdxr3 = dddxr3
     C                   eval      emdxr4 = dddxr4
     C                   eval      emunts = ddunts
     C                   endif
     C*
     C* Physician Detail
     C*
     C                   when      eobtyp = 'N'
     C     detky         chain     pdh01
     C                   if        %found
     C                   eval      empos = dhpos
     C                   eval      emtos = dhtos
     C                   eval      emprcd = dhprcd
     C                   eval      emprds = dhprds
     C                   eval      emmod1 = dhmod1
     C                   eval      emmod2 = dhmod2
     C                   eval      emmod3 = dhmod3
     C                   eval      emmod4 = dhmod4
     C                   eval      emdxr1 = dhdxr1
     C                   eval      emdxr2 = dhdxr2
     C                   eval      emdxr3 = dhdxr3
     C                   eval      emdxr4 = dhdxr4
     C                   eval      emunts = dhunts
     C                   eval      emarea = dharea
     C*** begin add *** 02/04/08 *******************************************
     C                   eval      emnpir = dhnpir
     C*** end   add *** 02/04/08 *******************************************
     C                   endif
     C*
     C* Pharmacy Detail
     C*
     C                   when      eobtyp = 'X'
     C     detky         chain     pdp01
     C                   if        %found
     C                   eval      emunts = dpunts
     C                   eval      emdesc = dpdesc
     C                   eval      emrx#  = dprx#
     C                   eval      emndc  = dpndc
     C                   eval      emrxmq = dprxmq
     C                   eval      emrxmm = dprxmm
     C                   eval      emrxgb = dprxgb
     C*** begin add *** 06/23/06 *******************************************
     C                   eval      emlic# = *blanks
     C                   eval      emalic = *blanks
     C                   eval      emflic = dplic#
     C*** end   add *** 06/23/06 *******************************************
     C*** begin add *** 02/04/08 *******************************************
     C                   eval      emrxpt = dprxpt
     C                   eval      emcert = dpcert
     C                   eval      emprin = dpprin
     C*** end   add *** 02/04/08 *******************************************
     C                   if        dprxfd <> 0
     C     *cymd         move      dprxfd        emrxfd
     C                   endif
     C                   endif
     C*
     C* Hospital Detail
     C*
     C                   when      eobtyp = 'H'
     C     detky         chain     pdu01
     C                   if        %found
     C                   eval      empos = dupos
     C                   eval      emunts = duunts
     C                   eval      emrvcd = durvcd
     C                   eval      emdesc = dudesc
     C                   eval      emrate = durate
     C                   eval      emnoch = dunoch
     C                   eval      empscd = dupscd
     C                   eval      emscds = duscds
     C                   eval      empmod = dupmod
     C                   endif
     C                   endsl
     C*
     C                   move      @@todt        todt_In
     C                   move      todt_In       date_mdyy
     C                   subdur    15:*days      date_mdyy
     C                   eval      date_yymd = date_mdyy
     C                   move      date_yymd     todt_In
     C                   movel     todt_In       emdate
     C*
     C                   write     pem01
     C*
     C     daky          reade     pda01
     C                   enddo
     C                   endif


     P Write_QFEMP     E
