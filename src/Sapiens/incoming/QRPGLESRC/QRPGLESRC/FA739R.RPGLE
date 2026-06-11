    H* (C) Copyright Progressive Data Solutions, Inc. 2003           *
    H*                                                               *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H* API - FA739R - Texas: Retrieve Region Allownance              *DATA
    H*******************************************************************
    H* Description:  Retrieves a given region's allowance            *D*TA
    H*                                                               * *
    H*  Called From: F4000N - Adjudication                           * *
    H*  Calls To:    None                                            * *
    H*                                                               * *
    H*******************************************************************
    H* Created on 08/14/2003 by c:62 for Project TX020               * *
    H*******************************************************************
    H* Revisions:                                                    *
    H*                                                               *
     H*  09/18/12 C:284 Project TX079
     H*                 Modifying program to handle processing zip codes using
     H*                 the Zip5 file and the new zip + 4 file.
     H*
    H*****************************************************************
     Ffmprl     if   e           k disk
     Ffmzrl     if   e           k disk    usropn
     Ffmp1l     if   e           k disk
     F*** Beg Add *** 09/18/12 *************************************************
     Fftzpl1    if   e           k disk
     F*** End Add *** 09/18/12 *************************************************
    D*
     D qca             s             56    dim(2) ctdata perrcd(1)
    D*
     D                 ds
     D ##cmd                         56
     D  ##st                          2    overlay(##cmd:38)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@ast           s              2
     D @@co#           s              3  0
     D @@cpt4          s                   like(prcpt4)
     D @@fdat          s                   like(zrefdt)
     D @@fein          s                   like(p1fein)
     D @@flic          s              9
     D @@mod           s                   like(p1mod)
     D @@mra           s              9  2
     D @@pos           s              3
     D length          s             15  5
     D xxloc#          s                   like(zrrgn#)
     D kkfein          s                   like(p1fein)
     D kkmod           s                   like(p1mod)
     D kkco#           s                   like(zrco#)
     D kkzip           s                   like(zrfzcd)
     D kkefdt          s                   like(zrefdt)
     D kkcpt4          s                   like(prcpt4)
     D*** Beg Add *** 09/18/12 *************************************************
     DKkst             s                   like(Zpst)
     DKkpls4           s                   like(Zppls4)
     D*** End Add *** 09/18/12 *************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C* Parameter lists
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@cpt4
     C                   parm                    @@fdat
     C                   parm                    @@ast
     C                   parm                    @@fein
     C                   parm                    @@mod
     C                   parm                    @@flic
     C                   parm                    @@pos
     C                   parm                    @@mra
    C*================================================================
    C* Key lists
    C*================================================================
     C     p1ky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C*
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt
     C*
     C     zrky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     C*** Beg Add *** 09/18/12 *************************************************
     C     keyFtzpl1     klist
     C                   kfld                    Kkst
     C                   kfld                    Kkzip
     C                   Kfld                    Kkpls4
     C                   Kfld                    Kkefdt
     C*** End Add *** 09/18/12 *************************************************
    C*================================================================
    C* Like Defines
    C*================================================================
    C*
    C* Openning the Region File
    C*
     C                   movel(p)  qca(1)        ##cmd
     C                   movel(p)  @@ast         ##st
    C*
     C                   call      'QCMDEXC'
     C                   parm                    ##cmd
     C                   parm      56            length
    C*
     C                   open      fmzrl
    C*
     C                   exsr      loadamt
    C*
    C* Closing the Region file
    C*
     C                   close     fmzrl
    C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(2)
     C                   parm      56            length
    C*
     C                   return
    C********************************************************************
    C** LOADAMT SUBROUTINE                             L O A D A M T    *
    C********************************************************************
     C     loadamt       begsr
     C*
     C                   eval      kkco# = @@co#
     C                   eval      kkefdt = @@fdat
     C                   eval      kkcpt4 = @@cpt4
     C*
     C                   if        @@flic <> *blanks
     C                   movel     @@flic        kkzip
     C                   else
     C                   eval      kkfein = @@fein
     C                   eval      kkmod = @@mod
     C     p1ky          chain     pp101                              99
     C                   movel     p1zip         kkzip
     C                   endif
    C*
    C* Get Procedure code info
    C*
     C     prky          setgt     ppr01
     C                   readp     ppr01                                  99
     C*
     C* Default to locality 99
     C*
     C                   eval      xxloc# = 99
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C*
     C                   if        *in99 = *off and kkzip >= zrfzcd
     C                             and kkzip <= zrtzcd
     C                   eval      xxloc# = zrrgn#
     C                   endif
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       If %subst(@@flic:6:4) <> *blanks;
          Kkst = @@ast;
          Kkzip = %dec(%subst(@@flic:1:5):5:0);
          Kkpls4 = %dec(%subst(@@flic:6:4):4:0);
          Kkefdt = @@fdat;
          Setgt keyFtzpl1 Pzp01;
          Readp Pzp01;
          If %found(Ftzpl1) and (Zpst = Kkst) and (Zpzip = Kkzip)
           and (Zppls4 = Kkpls4);
             Xxloc# = Zploc;
          Endif;
       Endif;
      /End-Free
     C*** End Add *** 09/18/12 *************************************************
     C*
     C* Load amount fields based on region
     C*
     C                   select
     C*
     C                   when      xxloc# = 9
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92n6
     C                   else
     C                   eval      @@mra = prmra
     C                   endif
     C*
     C                   when      xxloc# = 11
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92n9
     C                   else
     C                   eval      @@mra = pr92n8
     C                   endif
     C*
     C                   when      xxloc# = 15
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92nf
     C                   else
     C                   eval      @@mra = pr92ne
     C                   endif
     C*
     C                   when      xxloc# = 18
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92nl
     C                   else
     C                   eval      @@mra = pr92nk
     C                   endif
     C*
     C                   when      xxloc# = 20
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92nr
     C                   else
     C                   eval      @@mra = pr92nq
     C                   endif
     C*
     C                   when      xxloc# = 28
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92nx
     C                   else
     C                   eval      @@mra = pr92nw
     C                   endif
     C*
     C                   when      xxloc# = 31
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92o4
     C                   else
     C                   eval      @@mra = pr92o3
     C                   endif
     C*
     C                   when      xxloc# = 99
     C                   if        @@pos <> ' 11'
     C                   eval      @@mra = pr92oa
     C                   else
     C                   eval      @@mra = pr92o9
     C                   endif
     C*
     C                   endsl
     C*
     C     loadamte      endsr
**  (QCMDEX)
OVRDBF FILE(FMZRL) TOFILE(*LIBL/FMZRL  )
DLTOVR FILE(FMZRL)
