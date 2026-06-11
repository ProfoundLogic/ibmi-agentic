     Hcopyright('(c) Copyright Fiserv, Inc., 2006 - 2007')
     Hdftactgrp(*no)
     *****************************************************************
     *    PROGRAM: W2280R02                                          *
     *                                                               *
     *    DESCRIPTION: Business Catagories Maintenance               *
     *                                                               *
     *    CREATED BY:  C:132  08/15/06   PROJECT# WC1196             *
     *                 C:33                                          *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  06/04/07  C:33  Project: WC1196                              *
      *                  Changes from Q/A testing.                    *
     *****************************************************************
     Fw2285f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtb3p     uf a e             disk
     F                                     rename(pb301:dbrcdf)
     Fwtb3l     If   e           k disk
    F*
    F*  Business Category File
     F*
     Fwttgp     if   e             disk    rename(ptg01:ptg0p)
     Fwttgl     if   e           k disk
     F*
    F*  Class Code Description file
     Fwmprp     if   e             disk
     F                                     rename(ppr01:ppr0p)
     fwmprl     if   e           k disk    rename(ppr01:ppr00)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
    F*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Gov Class/Business Ctgy Assoc'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D ##rrn           s              9  0
     D addMode         s              1n   inz
     D chgMode         s              1n   inz
     D dspMode         s              1n   inz
     D length          s              3  0
     D kkco#           s                   like(b3co#)
     D kkType          s                   like(B3TYPE)
     D kkClas          s                   like(PRCLAS)
     D kkgovn          s                   like(b3govn)
     D kkbctg          s                   like(b3bctg)
     D kkefdt          s                   like(prefdt)
     D svType          s                   like(SCTYPE)
     D x1              s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*
     DLoadTypeDesc     pr                  like(SCTYPEDESC)
     D type                                like(SCTYPE)
     D*
     DSearchGenTable   pr                  like(GTKEY)
     D tableID                             like(GTTBID) const
     D*
     DValidGenTable    pr                  like(*in72)
     D inValue                             like(SCGOVN) const
     D tableID                             like(GTTBID) const
     D*
     D/COPY sPrString
    D*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     acdpl1        plist
     C                   parm      '2'           ##call            1
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm      *zeros        ##rrn             9 0
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     b3ky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkType
     C                   kfld                    kkgovn
     C                   kfld                    kkbctg
     C*
     C     tgky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkbctg
     C*
     C     keyWMPRL      klist
     C                   kfld                    ##co#
     C                   kfld                    kkClas
     C                   kfld                    kkefdt
     C*
     C     key1WMPRL     klist
     C                   kfld                    ##co#
     C                   kfld                    kkClas
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99=*off
     C*
     C                   if        b3stat='A'
     C                   eval      b3stat =  'I'
     C                   else
     C                   eval      b3stat = 'A'
     C                   endif                                                  fnflag='I'
     C*
     C                   update    dbrcdf
     C*
     C                   endif                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
     C*
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C                   select
     C*_____
     C                   when      SCTYPE <> 'G' and
     C                             SCTYPE <> 'S' and
     C                             SCTYPE <> 'N'
     C                   eval      *in81 = *on
     C                   eval      *in90 = *on
     C*
     C                   when      SCTYPE <> svType
     C                   eval      SCTYPEDESC = LoadTypeDesc(SCTYPE)
     C                   eval      *in45 = *on
     C                   eval      *in82 = *on
     C                   eval      svType = SCTYPE
     C                   eval      *in90 = *on
     C*_____
     C                   when      SCGOVN = *blanks
     C                   eval      *in71 = *on
     C                   eval      *in90 = *on
     C*_____
     C                   when      SCBCTG = *blanks
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
     C*_____
     C                   other
     C*_____
     C*
     C                   eval      kkType = SCTYPE
     C                   eval      kkgovn = scgovn
     C                   eval      kkbctg = scbctg
     C*
     C     b3ky          chain     pb301                              99
     C*
     C                   if        not *in90 and addMode
     C                             and %found
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   select
     C*__________
     C                   when      *in90
     C*__________
     C                   when      SCTYPE = 'G'
     C*
     C                   eval      length = %len(kkClas)
     C                   eval      SCGOVN = RightJstfy(SCGOVN:length)
     C                   do        length        x1
     C                   if        %subst(SCGOVN:x1:1) = ' '
     C                   eval      %subst(SCGOVN:x1:1) = '0'
     C                   endif
     C                   enddo
     C                   movel     SCGOVN        kkClas
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkefdt
     C     keyWMPRL      setgt     ppr00
     C     key1WMPRL     readpe    ppr00
     C*
     C                   select
     C                   when      %eof
     C                   eval      *in72 = *on
     C                   eval      *in90 = *on
     C                   when      PRSTAT <> 'A'
     C                   eval      *in74 = *on
     C                   eval      *in90 = *on
     C                   endsl
     C*__________
     C                   when      SCTYPE = 'S'
     C                   eval      *in72 = not ValidGenTable(SCGOVN:'SIC')
     C*__________
     C                   when      SCTYPE = 'N'
     C                   eval      *in72 = not ValidGenTable(SCGOVN:'NAIC')
     C*__________
     C                   endsl
     C*
     C                   if        *in72
     C                   eval      *in90 = *on
     C                   else
     C                   select
     C                   when      SCTYPE = 'G'
     C                   eval      SCCCDS = PRDESC
     C                   when      SCTYPE = 'S' or SCTYPE = 'N'
     C                   eval      SCCCDS = GTDESC
     C                   endsl
     C                   endif
     C*
     C* Now get Business catagory description.
     C                   eval      kkbctg = scbctg
     C     tgky          chain     ptg01                              99
     C*
     C                   select
     C*__________
     C                   when      *in90
     C*__________
     C                   when      not %found
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
     C                   when      TGSTAT <> 'A'
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in78 = *on
     C                   eval      *in77 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      *in90 = *on
     C                   other
     C                   movel(p)  tgbcds        scbcds
     C                   endsl
     C*_____
     C                   endsl
     C*
     C     ender1        endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
      *===============================================================
      * KJDO - Subroutine for F10
      *===============================================================
      *
     C     kjdo          begsr
      *
     C                   select
      *_____
     C     hfld          wheneq    'SCBCTG'
     C                   move      *blanks       scbcds
     C                   call      'W2280R'      acdpl1
     C     ##rrn         chain     ptg0p                              99
     C     *in99         ifeq      *on
     C*
     C                   else
     C                   eval      *in41 = *on
     C                   eval      scbctg = tgbctg
     C                   movel(p)  tgbcds        scbcds
     C                   endif
      *_____
      * Searches after this will not process if not in --ADD-- mode
     C                   when      not addMode
      *_____
     C                   when      hfld = 'SCGOVN' and
     C                              SCTYPE <> 'G' and
     C                              SCTYPE <> 'S' and
     C                              SCTYPE <> 'N'
     C                   eval      *in81 = *on
     C                   eval      *in90 = *on
      *_____
     C                   when      hfld = 'SCGOVN' and SCTYPE = 'G'
     C
      *  Search only allowed for Governing Class Code if in Add Mode
     C                   call      'W0630R'
     C                   parm      '2'           @@call            1
     C     *inkc         parm                    @@kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm                    ##RRN             9 0
     C                   parm                    ##rtcd            3
     C                   parm      2             ##view            1 0
     C                   parm                    ##date            8
     C
     C                   if        ##RRN <> *zeros
     C     ##RRN         CHAIN     PPR0P                              99
     C                   if        NOT *in99 and prstat = 'A'
     C                   eval      SCGOVN = %editc(PRCLAS:'Z')
     C                   eval      SCCCDS = PRDESC
     C                   eval      *in83 = *on
     C                   else
     C                   eval      *in82 = *on
     c                   endif                                                  Not *in99 and Active
     c                   endif                                                  ##RRN <> *ZEROS
      *_____
     C                   when      hfld = 'SCGOVN' and SCTYPE = 'S'
     C                   eval      SCGOVN = SearchGenTable('SIC')
      *_____
     C                   when      hfld = 'SCGOVN' and SCTYPE = 'N'
     C                   eval      SCGOVN = SearchGenTable('NAIC')
      *_____
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      SCTYPE = B3TYPE
     C                   eval      scbctg = b3bctg
     C                   eval      scgovn = b3govn
     C                   eval      scstat = b3stat
     C*
     C                   eval      SCTYPEDESC = LoadTypeDesc(SCTYPE)
     C                   eval      svType = SCTYPE
     C*
     C                   select
     C                   when      SCTYPE = *blanks
     C                   eval      *in45 = *off
     C                   other
     C                   eval      *in45 = *on
     C                   endsl
     C*
     C                   if        addMode
     C                   eval      scstat = 'A'
     C                   endif
     C*
     C                   eval      kkbctg = scbctg
     C     tgky          chain     ptg01                              99
     C                   if        not *in99
     C                   eval      scbcds = tgbcds
     C                   else
     C                   eval      scbcds = *blanks
     C                   endif
     C*
     C                   select
     C*_____
     C                   when      SCTYPE = 'G'
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkefdt
     C                   movel     SCGOVN        kkClas
     C*
     C     keyWMPRL      setgt     ppr00
     C     key1WMPRL     readpe    ppr00                                  99
     C                   IF        *IN99 <> *ON
     C                   movel(p)  prdesc        scccds
     C                   else
     C                   eval      scccds = *blanks
     C                   endif
     C*_____
     C                   when      SCTYPE = 'S'
     C                   select
     C                   when      ValidGenTable(SCGOVN:'SIC')
     C                   other
     C                   endsl
     C*_____
     C                   when      SCTYPE = 'N'
     C                   select
     C                   when      ValidGenTable(SCGOVN:'NAIC')
     C                   other
     C                   endsl
     C*_____
     C                   other
     C                   clear                   SCCCDS
     C*_____
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      b3co# = ##co#
     C                   eval      B3TYPE = SCTYPE
     C                   eval      b3govn = scgovn
     C                   eval      b3bctg = scbctg
     C                   eval      b3stat = scstat
     C*
     C                   eval      b3user = q1user
     C                   eval      b3pgm = q1pgm
     C                   time                    dsdttm
     C                   eval      b3time = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         b3dt
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   ENDCS
     C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C                   TIME                    DSDTTM
      *
     C     @@mode        comp      '1'                                    09
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C*
     C                   select
     C                   when      @@mode = 'A'
     C                   eval      addMode = *on
     C                   when      @@mode = 'C'
     C                   eval      chgMode = *on
     C                   when      @@mode = 'D'
     C                   eval      dspMode = *on
     C                   endsl
      *
     C                   eval      scccds = *blanks
     C                   eval      scbcds = *blanks
      *
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
     C*
     C                   exsr      mvscdb
     C*
     C                   eval      b3iusr = q1user
     C                   eval      b3ipgm = q1pgm
     C                   eval      b3itm = B3TIME
     C                   eval      B3IDT = B3DT
     C*
     C                   write     dbrcdf
     C*
     C                   feod      wtb3p
     C*
     C                   endsr

     P*====================================================================
     P* Load the type description field
     P*====================================================================
     PLoadTypeDesc     b

     DLoadTypeDesc     pi                  like(SCTYPEDESC)
     D type                                like(SCTYPE)

     D desc            s                   like(SCTYPEDESC)
     D outDesc         s                   like(SCTYPEDESC)
     D                                     inz('. . . . . . . . . . . . .')

     C                   select
     C                   when      type = 'G'
     C                   eval      desc = 'Governing class code'
     C                   when      type = 'S'
     C                   eval      desc = 'SIC code'
     C                   when      type = 'N'
     C                   eval      desc = 'NAICS code'
     C                   other
     C                   eval      desc = ' .'
     C                   endsl

     C                   eval      %subst(outDesc:1:%len(%trim(desc))+1) = desc

     C                   return    outDesc

     PLoadTypeDesc     e

     P*====================================================================
     P* Search the generic table values
     P*====================================================================
     PSearchGenTable   b

     DSearchGenTable   pi                  like(GTKEY)
     D tableID                             like(GTTBID) const

     D ##Data          s             40
     D ##Mode          s              1                 inz
     D ##TbID          s                   like(GTTBID)
     D ##Key           s                   like(GTKEY)  inz
     D rtnCode         s              1                 inz

     C                   call      'W2493R'
     C                   parm                    ##data
     C                   parm                    ##mode
     C                   parm      tableID       ##TbID
     C                   parm                    ##Key
     C                   parm                    rtnCode

     C                   select
     C                   when      ValidGenTable(##Key:tableID)
     C                   eval      *in83 = *on
     C                   other
     C                   eval      *in82 = *on
     C                   endsl

     C                   return    ##Key

     PSearchGenTable   e

     P*====================================================================
     P* Validate the generic table values
     P*====================================================================
     PValidGenTable    b

     DValidGenTable    pi                  like(*in72)
     D inValue                             like(SCGOVN) const
     D tableID                             like(GTTBID) const

     C     tableKey      klist
     C                   kfld                    ##Co#
     C                   kfld                    tableID
     C                   kfld                    inValue

     C                   clear                   GTDESC
     C     tableKey      chain     pgt00
     C                   eval      SCCCDS = GTDESC
     C                   return    %found

     PValidGenTable    e
