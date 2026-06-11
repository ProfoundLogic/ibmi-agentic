     H*  (C) Copyright Progressive Data Solutions, Inc., 2003
     H*****************************************************************
     H/TITLE EXPSR - FQ exposure extract
     H*****************************************************************
     H*  Description: This program fills files for exporting to the   *
     H*               FQ system.                                      *
     H*                                                               *
     H*  Called from:                                                 *
     H*                                                               *
     H*  Calls to   :                                                 *
     H*                                                               *
     H*  Created: 08/01/03  Programmer: 087  Proj: FQ077              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  10/27/11 C:136 Support: WA114 78364                          *
     H*                 Remove code that fetches only latest          *
     H*                 payroll class record                          *
     H*                                                               *
     H*  08/23/05 C:087 Project: FQ077                                *
     H*                 If updating EXP2P because a record already    *
     H*                 exists, then need to update P2RATE, P2MINP    *
     H*                 along with P2EFDT.                            *
     H*                                                               *
     H*  02/26/04 C:087 Project: FQ077                                *
     H*                 Add parm for effective date.  If entered then *
     H*                 only extract codes, descriptions and rates    *
     H*                 for those with an effective date >= to the    *
     H*                 date entered.                                 *
     H*                                                               *
     H*****************************************************************
     Fwmprl1    if   e           k disk
     Fwmrsl     if   e           k disk
     Fwmpxl     if   e           k disk
     F
     Fexp1p     uf a e             disk
     Fexp2p     uf a e           k disk
     Fexp3p     uf a e           k disk


     D*  Date field definitions
     D efdate          s               d   datfmt(*usa/)
     D exdate          s               d   datfmt(*usa/)
     D savdate         s               d   datfmt(*usa/)
     D today           s               d   datfmt(*usa/)

     D*  key fields
     D kkco#           s                   like(prco#)
     D kkrtcd          s                   like(rsrtcd)
     D kkclas          s                   like(prclas)

     D*  field definitions
     D state           s                   like(rsjrst)
     D temp            s              7
     D temp2           s              8
     D factor          s              3  2
     D temp_rate       s                   like(prrt)
     D clas4           s              4  0
     C***  begin add  ***  02/26/04  ***************************************
     D min_date        s              7  0
     C***  end   add  ***  02/26/04  ***************************************
     D
     D savclas         s                   like(p2clas)
     D savst           s                   like(p2st)
     D savhaz          s                   like(p2haz)
     D savstd          s                   like(p2std)
     D savefdt         s                   like(p2efdt)

     DPSDS           ESDS                  Extname(SZQ1P)

     C*****************************************************************
     C*  Key lists
     C*****************************************************************
     C*   payroll file
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C
     C*   rate set code file
     C     rsky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C
     C*   extension file
     C     pxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkclas
     C
     C*   rates file
     C     p2ky          klist
     C                   kfld                    savclas
     C                   kfld                    savst
     C                   kfld                    savhaz
     C                   kfld                    savstd
     C                   kfld                    savefdt
     C
     C*   descriptions file
     C     p3ky          klist
     C                   kfld                    savclas
     C                   kfld                    savst
     C                   kfld                    savhaz
     C                   kfld                    savstd
     C                   kfld                    savefdt

     C*****************************************************************
     C*  Entry parms
     C*****************************************************************
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@rtcd            3
     C                   parm                    @@factor          3
     C***  begin add  ***  02/26/04  ***************************************
     C                   parm                    @@min_date        7
     C***  end   add  ***  02/26/04  ***************************************

     C                   move      @@co#         kkco#
     C                   move      @@rtcd        kkrtcd
     C                   move      @@factor      factor
     C***  begin add  ***  02/26/04  ***************************************
     C                   if        q1prms >= 4
     C                   movel     @@min_date    min_date
     C                   else
     C                   move      *zero         min_date
     C                   endif
     C***  end   add  ***  02/26/04  ***************************************

     C*  read through the payroll class code file
     C     prky          chain     ppr01                              99
     C                   dow       *in99 = *off

     C***  begin add  ***  02/26/04  ***************************************
     C                   if        min_date <= prefdt
     C***  end   add  ***  02/26/04  ***************************************

     C*    process only active class codes
     C                   if        prstat <> 'I'

     C     *cymd         test(d)                 prexdt                 98
     C  n98*cymd         move      prexdt        exdate
     C   98today         SubDur    1:*Days       exdate
     C*    process only those that have expiration date blank or > today
     C                   if        prexdt = 0 OR
     C                              exdate > today
     C*      get state
     C                   eval      kkrtcd = prrtcd
     C                   eval       state = *blanks
     C     rsky          chain     prs01                              60
     C                   if        *in60 = *off
     C                   eval       state = rsjrst

     C*              class code
     C*      Move in as a 4digit class code except where > 9999
     C*        (this is done for display purposes in FastQuote)
     C                   if        prclas > 9999
     C                   movel(p)  prclas        p1clas
     C                   movel(p)  prclas        p2clas
     C                   movel(p)  prclas        p3clas
     C                   else
     C                   eval      clas4 = prclas
     C                   movel(p)  clas4         p1clas
     C                   movel(p)  clas4         p2clas
     C                   movel(p)  clas4         p3clas
     C                   endif

     C*              state
     C                   eval      p1st = state
     C                   eval      p2st = state
     C                   eval      p3st = state

     C*              hazard group
     C                   if        prhzgr <> *blank
     C                   eval      p1haz = prhzgr
     C                   eval      p2haz = prhzgr
     C                   eval      p3haz = prhzgr
     C                   else
     C                   eval      p1haz = '0'
     C                   eval      p2haz = '0'
     C                   eval      p3haz = '0'
     C                   endif

     C*              standard exception
     C                   eval      p1std = 'N'
     C                   eval      p2std = 'N'
     C                   eval      p3std = 'N'
     c*              payroll type
     c                   eval      p2pcf = prpcf

     C*              is excluded
     C                   eval      p2exfl = 'N'
     C                   eval      p3exfl = 'N'

     C*              rate class
     C                   eval      p2rtcl = 'X'

     C*              rate begin date
     C     *cymd         move      prefdt        efdate
     C                   move      efdate        p2efdt
     C                   move      efdate        p3efdt

     C*              rate end date
     C                   if        prexdt <> 0
     C                   move      exdate        p3exdt
     C                   else
     C                   move      *blanks       p3exdt
     C                   endif

     C*              rate calculation type
     C                   eval      p2type = 'Scalar'

     C*              minimum premium
     C                   move(p)   prmnpr        temp
     C                   eval      temp2 = %subst(temp:1:5) + '.' +
     C                                       %subst(temp:6:2)
     C                   move(p)   temp2         p2minp

     C*              territory
     C                   eval      p2terr = '0'

     C*              rate
     C                   eval      temp_rate = prrt * factor
     C                   move(p)   temp_rate     temp
     C                   eval      temp2 = %subst(temp:1:5) + '.' +
     C                                       %subst(temp:6:2)
     C                   move(p)   temp2         p2rate

     C*              code (this correlates to extensions in pds system)
     C                   eval      p3code = '0'

     C*              description
     C                   eval      p3desc = prdesc

     C*    write to export files
     C*      codes file
     C                   write     pp101                                70
     C                   if         *in70 = *on
     C*                  *** then record already exists
     C*                  *** could be used for population an output file
     C                   endif
     C
      *** beg chg ***** 10/27/11 ***********************************************
     C*******rates file
     C*********if a record for clas, st, std, and haz has already been
     C*********written and the effective date of the rate is <= today
     C*********then update the file
     C***********        if        p2clas = savclas and
     C***********                  p2st = savst and
     C***********                  p2std = savstd and
     C***********                  p2haz = savhaz and
     C***********                  efdate <= today
     C***********
     C*****p2ky**        chain     pp201                              62
     C***********        if        *in62 = *off
     C***********
     C***********  update effective date
     C***********        move      efdate        p2efdt
     C***********  update rate
     C***********        eval      temp_rate = prrt * factor
     C***********        move(p)   temp_rate     temp
     C***********        eval      temp2 = %subst(temp:1:5) + '.' +
     C***********                            %subst(temp:6:2)
     C***********        move(p)   temp2         p2rate
     C***********
     C***********  update minimum premium
     C***********        move(p)   prmnpr        temp
     C***********        eval      temp2 = %subst(temp:1:5) + '.' +
     C***********                            %subst(temp:6:2)
     C***********        move(p)   temp2         p2minp
     C***********
     C***********        update    pp201
     C***********        endif
     C***********
     C***********        else
     C***********
     C***********        write     pp201
     C***********
     C***********        endif
     C***********
     C*******descriptions file
     C*********if an older effective date exists and the current effective
     C*********date is less than or equal to today, then update the
     C*********description record; otherwise write a new record.
     C***********        if        p3clas = savclas and
     C***********                  p3st = savst and
     C***********                  p3std = savstd and
     C***********                  p3haz = savhaz and
     C***********                  efdate <= today
     C***********
     C*****p3ky**        chain     pp301                              62
     C***********        if        *in62 = *off
     C***********        move      efdate        p3efdt
     C*****************  move      exdate        p3exdt
     C***********        update    pp301
     C***********        endif
     C***********
     C***********        else
     C***********
     C***********        write     pp301
     C***********
     C***********        endif

     c                   write     pp201
     c                   write     pp301
      *** end chg ***** 10/27/11 ***********************************************
     C
     C***    if extensions exist for class code, write record for each
     C                   eval      kkclas = prclas
     C     pxky          chain     ppx01                              61
     C                   dow       *in61 = *off

     C*          process active extensions only
     C                   if        pxstat <> 'I'
     C*            only write for matching states
     C                   if        pxst = state
     C
     C                   eval      p3desc = pxdesc
     C                   eval      p3code = pxext
     C                   write     pp301
     C
     C                   endif
     C                   endif

     C     pxky          reade     ppx01                                  61
     C                   enddo
     C***    end of extension processing

     C
     C*      set the save fields
     C                   eval      savclas = p2clas
     C                   eval      savst = p2st
     C                   eval      savhaz = p2haz
     C                   eval      savstd = p2std
     C                   eval      savefdt = p2efdt

     C*              *in60=*off
     C                   endif

     C*              end of status and date 'if' statements
     C                   endif
     C                   endif

     C***  begin add  ***  02/26/04  ***************************************
     C                   endif
     C***  end   add  ***  02/26/04  ***************************************

     C*      read next record
     C     prky          reade     ppr01                                  99
     C                   enddo
     C                   eval      *inlr = *on


     C*=========================================================================
     C*  *inzsr - performs all preprocessing necessary to begin extractions
     C*=========================================================================
     C     *inzsr        begsr
     C
     C                   time                    today
     C
     C                   endsr
