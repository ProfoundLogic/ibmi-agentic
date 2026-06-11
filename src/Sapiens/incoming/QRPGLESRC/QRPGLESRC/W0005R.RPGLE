     h*  (C) Copyright Progressive Data Solutions, Inc., 2005
    h*
     h/TITLE W0005R - Company ACH Control Information
     h**************************************************************************
     h* PROGRAM DESCRIPTION:  Update/display  Company ACH Control Informationb *
     h*                                                                        *
     h* CALLED BY: S0001R                                                      *
     h*                                                                        *
     h* CALLS TO:  NONE                                                        *
     h*                                                                        *
     h* CREATED: 04/11/03  C:136 PROJECT WC1116                                *
     h**************************************************************************
     h* REVISIONS:                                                             *
     h*                                                                        *
     h* 06/26/09 C:227 Support: WA146 68801                                    *
     h*                ACH issue                                               *
     h*                                                                        *
     h* 10/06/08 C:136 Support: WA139 65401                                    *
     h*                Expand Immed Origin to 10                               *
     h*                                                                        *
     h* 02/22/05 C:62  Project: 129WJO                                         *
     h*                Added ACH security batch security.                      *
     h*                                                                        *
     h* 09/07/04 C:15  Project: 121WBJ                                         *
     h*                Added additional payment A/R codes.                     *
     h*                                                                        *
     h**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0005f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f*  Physical file definition (for maintaining record)
     fwmcbp     uf a e             disk
     f                                     rename(pcb01:dbrcdf)
     fwmcbl     if   e           k disk    infds(cblfds)
     fsmcop     if   e             disk
     fwtabl     if   e           k disk
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d optnm           c                   'Company ACH Control Info'
     d comapi          c                   'WA001R'
     d #ofscr          c                   1
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d @@mode          s              1
     d @@rrn           s              9  0
     d @@rrn1          s              9  0
     d cblfds          ds
     d  cbrrn                397    400B 0
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     c/COPY SCOPYMT01C
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn1                         wmfnp
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     tbky          klist
     c                   kfld                    coco#
     c                   kfld                    kkkey            10

     c     cbky          klist
     c                   kfld                    coco#
    c*================================================================
    c* DLTPRE - Pre-processing before delete occurs
    c*================================================================
     c     dltpre        begsr
     c                   endsr
    c*================================================================
    c* DSPLYR - Processing after displaying information
    c*================================================================
     c     dsplyr        begsr
     c                   endsr
    c*================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
    c*================================================================
     c     errck1        begsr

     c                   if        scidst = *zero
     c                   seton                                        71  90
     c                   endif
      ***********
      *** beg chg ***** 10/06/08 **************************************
     c**n90******        if        sciorg = *zero
     c***********        seton                                        72  90
     c***********        endif

     c  n90              select
     c                   when      cltID = '139'
     c** BEG ADD *** 06/26/09 **********************************************
     c                             or cltid = '146'
     c** END ADD *** 06/26/09 **********************************************
     c                   if        sciorg10 = *zero
     c                   seton                                        72  90
     c                   endif
     c                   other
     c                   if        sciorg = *zero
     c                   seton                                        72  90
     c                   endif
     c                   endsl
      *** end chg ***** 10/06/08 **************************************

     c  n90              if        scbnam = *blank
     c                   seton                                        73  90
     c                   endif

     c  n90              if        sccnam = *blank
     c                   seton                                        74  90
     c                   endif

     c  n90              if        scordi = *zero
     c                   seton                                        75  90
     c                   endif

    c*** begin dlt *** 09/07/04 ****************************************************
     c**n90              if        scarcd <> *blank
     c****               eval      kkkey = scarcd
     c**** tbky          chain     ptt01
     c****               if        %found and ttstat = 'A'
      ****
     c****               if        ttctyp <> 'P' or tttype <> 'P'
     c****               seton                                        76  90
     c****               endif                                                  ttctyp
      ****
     c****               else
     c****               seton                                        77  90
     c****               endif                                                  %found
      ****
     c****               else
     c****               seton                                        78  90
     c****               endif                                                  scarcd<>*blank
    c*** End dlt ***** 09/07/04 ****************************************************

     c  n90              if        scsycd <> *blank

     c                   eval      kkkey = scsycd
     c     tbky          chain     psy01
     c                   if        not %found or systat <> 'A' or
     c                             sytype <> 'Y'
     c                   seton                                        79  90
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        80  90
     c                   endif                                                  scarcd<>*blank

    c*** begin add *** 09/07/04 ****************************************************
     c* Edit the A/R payment codes.
     c*
     c  n90              if        scarcd <> *blank
     c                   eval      kkkey = scarcd
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'P' or tttype <> 'P'
     c                   seton                                        762090
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772090
     c                   endif                                                  %found

     c                   else
     c                   seton                                        782090
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        scecar <> *blank
     c                   eval      kkkey = scecar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'E' or tttype <> 'P'
     c                   seton                                        762190
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772190
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782190
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc1aar <> *blank
     c                   eval      kkkey = sc1aar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '1' or tttype <> 'Q'
     c                   seton                                        762290
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772290
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782290
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        scifar <> *blank
     c                   eval      kkkey = scifar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'F' or tttype <> 'Q'
     c                   seton                                        762390
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772390
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782390
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc2aar <> *blank
     c                   eval      kkkey = sc2aar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '2' or tttype <> 'Q'
     c                   seton                                        762490
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772490
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782490
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        scsfar <> *blank
     c                   eval      kkkey = scsfar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'S' or tttype <> 'Q'
     c                   seton                                        762590
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772590
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782590
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc3aar <> *blank
     c                   eval      kkkey = sc3aar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '3' or tttype <> 'Q'
     c                   seton                                        762690
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772690
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782690
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        scmfar <> *blank
     c                   eval      kkkey = scmfar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'M' or tttype <> 'Q'
     c                   seton                                        762790
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772790
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782790
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc4aar <> *blank
     c                   eval      kkkey = sc4aar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '4' or tttype <> 'Q'
     c                   seton                                        762890
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772890
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782890
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sctxar <> *blank
     c                   eval      kkkey = sctxar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'T' or tttype <> 'Q'
     c                   seton                                        762990
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772990
     c                   endif                                                  %found

     c                   else
     c******             seton                                        782990
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc5aar <> *blank
     c                   eval      kkkey = sc5aar
     c     tbky          chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '5' or tttype <> 'Q'
     c                   seton                                        763090
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        773090
     c                   endif                                                  %found

     c                   else
     c******             seton                                        783090
     c                   endif                                                  scarcd<>*blank
    c*** End add ***** 09/07/04 ****************************************************

     c                   endsr                                                  errck1
    c*================================================================
    c* ERRCK2 - Error checking for srceen 2 (SCR02)
    c*================================================================
     c     errck2        begsr
     c                   endsr
    c*================================================================
    c* ERRCK3 - Error checking for srceen 3 (SCR03)
    c*================================================================
     c     errck3        begsr
     c                   endsr
    c*================================================================
    c* KJDO - Processing for F10
    c*================================================================
     c     kjdo          begsr

    c*
    c*   Premium payment A/R code search
    c*
     c                   select
     c                   when      hfld = 'SCARCD'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scarcd = sckey
    c*** begin chg *** 09/07/04 ****************************************************
     c****************   seton                                        51
     c                   seton                                        5021
    c*** end chg ***** 09/07/04 ****************************************************
     c                   else
    c*** begin chg *** 09/07/04 ****************************************************
     c****************   seton                                        50
     c                   seton                                        5020
    c*** end chg ***** 09/07/04 ****************************************************
     c                   endif                                                  *inkl <> *on

    c*
    c*   A/R batch system code search
    c*
     c                   when      hfld = 'SCSYCD'

     c                   eval      sckey = *blanks
     c                   call      'W1202R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scsycd = sckey
    c*** begin add *** 09/07/04 ****************************************************
     c                   seton                                        5020
    c*** end add ***** 09/07/04 ****************************************************
     c                   else
     c                   seton                                        51
     c                   endif                                                  *inkl <> *on

    c*** begin add *** 09/07/04 ****************************************************
    c*
    c*   Expense constant A/R code search
    c*
     c                   when      hfld = 'SCECAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scecar = sckey
     c                   seton                                        5022
     c                   else
     c                   seton                                        5021
     c                   endif                                                  *inkl <> *on
    c*
    c*   1st assessment A/R code search
    c*
     c                   when      hfld = 'SC1AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc1aar = sckey
     c                   seton                                        5023
     c                   else
     c                   seton                                        5022
     c                   endif                                                  *inkl <> *on
    c*
    c*   Installment fees A/R code search
    c*
     c                   when      hfld = 'SCIFAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scifar = sckey
     c                   seton                                        5024
     c                   else
     c                   seton                                        5023
     c                   endif                                                  *inkl <> *on
    c*
    c*   2nd assessment A/R code search
    c*
     c                   when      hfld = 'SC2AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc2aar = sckey
     c                   seton                                        5025
     c                   else
     c                   seton                                        5024
     c                   endif                                                  *inkl <> *on
    c*
    c*   Safety fees A/R code search
    c*
     c                   when      hfld = 'SCSFAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scsfar = sckey
     c                   seton                                        5026
     c                   else
     c                   seton                                        5025
     c                   endif                                                  *inkl <> *on
    c*
    c*   3rd assessment A/R code search
    c*
     c                   when      hfld = 'SC3AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc3aar = sckey
     c                   seton                                        5027
     c                   else
     c                   seton                                        5026
     c                   endif                                                  *inkl <> *on
    c*
    c*   Membership fees A/R code search
    c*
     c                   when      hfld = 'SCMFAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scmfar = sckey
     c                   seton                                        5028
     c                   else
     c                   seton                                        5027
     c                   endif                                                  *inkl <> *on
    c*
    c*   4th assessment A/R code search
    c*
     c                   when      hfld = 'SC4AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc4aar = sckey
     c                   seton                                        5029
     c                   else
     c                   seton                                        5028
     c                   endif                                                  *inkl <> *on
    c*
    c*   State tax fees A/R code search
    c*
     c                   when      hfld = 'SCTXAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sctxar = sckey
     c                   seton                                        5030
     c                   else
     c                   seton                                        5029
     c                   endif                                                  *inkl <> *on
    c*
    c*   5th assessment A/R code search
    c*
     c                   when      hfld = 'SC5AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc5aar = sckey
     c                   else
     c                   seton                                        5030
     c                   endif                                                  *inkl <> *on
    c*** end add ***** 09/07/04 ****************************************************

     c                   endsl

     c                   endsr
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   eval      scidst = cbidst
      *** beg chg ***** 10/06/08 ************************************************
     c***********        eval      sciorg = cbiorg
     c                   select
     c                   when      cltID = '139'
     c** BEG ADD *** 06/26/09 **********************************************
     c                             or cltid = '146'
     c** END ADD *** 06/26/09 **********************************************
     c                   eval      sciorg10 = cbiorg
     c                   other
     c                   eval      sciorg = cbiorg
     c                   endsl
      *** end chg ***** 10/06/08 ************************************************
     c                   eval      scbnam = cbbnam
     c                   eval      sccnam = cbconm
     c                   eval      scrfcd = cbrfcd
     c                   eval      scordi = cbordi
     c                   eval      scarcd = cbarcd
     c                   eval      scsycd = cbsycd
    c*** begin add *** 09/07/04 ****************************************************
     c                   eval      scecar = cbecar
     c                   eval      sc1aar = cb1aar
     c                   eval      scifar = cbifar
     c                   eval      sc2aar = cb2aar
     c                   eval      scsfar = cbsfar
     c                   eval      sc3aar = cb3aar
     c                   eval      scmfar = cbmfar
     c                   eval      sc4aar = cb4aar
     c                   eval      sctxar = cbtxar
     c                   eval      sc5aar = cb5aar
    c*** end add ***** 09/07/04 ****************************************************
    c*** begin add *** 02/22/05 ****************************************************
     c                   eval      scachl = cbachl
     c                   eval      scacht = cbacht
    c*** end   add *** 02/22/05 ****************************************************

     c                   endsr
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr

     c                   eval      cbco#  = coco#
     c                   eval      cbidst = scidst
      *** beg chg ***** 10/06/08 ************************************************
     c***********        eval      cbiorg = sciorg
     c                   select
     c                   when      cltID = '139'
     c** BEG ADD *** 06/26/09 **********************************************
     c                             or cltid = '146'
     c** END ADD *** 06/26/09 **********************************************
     c                   eval      cbiorg = sciorg10
     c                   other
     c                   eval      cbiorg = sciorg
     c                   endsl
      *** end chg ***** 10/06/08 ************************************************
     c                   eval      cbbnam = scbnam
     c                   eval      cbconm = sccnam
     c                   eval      cbrfcd = scrfcd
     c                   eval      cbordi = scordi
     c                   eval      cbarcd = scarcd
     c                   eval      cbsycd = scsycd
    c*** begin add *** 09/07/04 ****************************************************
     c                   eval      cbecar = scecar
     c                   eval      cb1aar = sc1aar
     c                   eval      cbifar = scifar
     c                   eval      cb2aar = sc2aar
     c                   eval      cbsfar = scsfar
     c                   eval      cb3aar = sc3aar
     c                   eval      cbmfar = scmfar
     c                   eval      cb4aar = sc4aar
     c                   eval      cbtxar = sctxar
     c                   eval      cb5aar = sc5aar
    c*** end add ***** 09/07/04 ****************************************************
    c*** begin add *** 02/22/05 ****************************************************
     c                   eval      cbachl = scachl
     c                   eval      cbacht = scacht
    c*** end   add *** 02/22/05 ****************************************************

     c                   endsr
    c*================================================================
    c* OTHERR - Processing for other modes
    c*================================================================
     c     otherr        begsr
     c                   endsr
    c*================================================================
    c* OTHFKS - Processing for other functions keys
    c*================================================================
     c     othfks        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     c                   endsr
    c*================================================================
    c* PGMEND - End of program processing
    c*================================================================
     c     pgmend        begsr
     c                   seton                                        lr
     c                   endsr
    c*================================================================
    c* PGMSTR - Start of program processing
    c*================================================================
     c     pgmstr        begsr
     c
      *** beg add ***** 10/06/08 **************************************
     c                   call      'RTVCLTID'
     c                   parm                    cltID             3
     c                   select
     c                   when      cltID = '139'
     c** BEG ADD *** 06/26/09 **********************************************
     c                             or cltid = '146'
     c** END ADD *** 06/26/09 **********************************************
     c                   eval      *in39 = *on
     c                   other
     c                   endsl
      *** end add ***** 10/06/08 **************************************
     c
     c     @@rrn1        chain     pco01
     c                   if        %found
     c                   eval      scco# = coco#
     c                   eval      sccomn = coname
     C                   movel(p)  coname        ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
     C                   movel     ##conm        scconm
     c
     c     cbky          chain     pcb01
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn = cbrrn
     c                   else
     c                   eval      @@mode = 'A'
     c                   endif
     c
     c                   else
     c                   seton                                        lr
     c                   endif
     c
     c                   endsr
    c*================================================================
    c* PREOTH - Pre-processing for other modes
    c*================================================================
     c     preoth        begsr
     c                   endsr
    c*================================================================
    c* UPDATR - Update data base record and after processing
    c*================================================================
     c     updatr        begsr
     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     c                   eval      cbuser = q1user
     c                   eval      cbpgm = q1pgm
     c                   time                    dsdttm
     c                   eval      cbtime = dstime
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         cbdt
     c                   update    dbrcdf
E01  c                   endif
     c                   endsr
    c*================================================================
    c* WRITER - Write data base record and after processing
    c*================================================================
     c     writer        begsr
     c                   clear                   dbrcdf
     c                   exsr      mvscdb
     c                   eval      cbuser = q1user
     c                   eval      cbiusr = q1user
     c                   eval      cbpgm = q1pgm
     c                   eval      cbipgm = q1pgm
     c                   time                    dsdttm
     c                   eval      cbtime = dstime
     c                   eval      cbitim = dstime
     c                   eval      srdti  = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         cbdt
     c                   movel(p)  srdto         cbidt
     c                   write     dbrcdf
     c                   eval      @@mode = ' '
     c                   feod      wmcbp
     c                   endsr
