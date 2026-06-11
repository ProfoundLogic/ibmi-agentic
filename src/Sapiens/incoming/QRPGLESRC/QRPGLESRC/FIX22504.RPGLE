    H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H* TITLE FIX22504 - Update accounting month as CYYMM                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
     Fgltrp     uf   e             disk
     Fgldep     uf   e             disk

     Dchg_actm         PR                  like(TRACTM)
     D actm                                like(TRACTM)

     C*
     C                   read      ptr01                                  60
     C                   dow       not *in60
     C                   eval      TRACTM = chg_actm(TRACTM)
     C                   update    ptr01
     C                   read      ptr01                                  60
     C                   enddo
     C*
     C                   read      pde01                                  60
     C                   dow       not *in60
     C                   eval      DTACTM = chg_actm(DTACTM)
     C                   update    pde01
     C                   read      pde01                                  60
     C                   enddo
     C*
     C                   move      *on           *inlr





     P*====================================================================
     P* chg_actm - Change the accounting date from CMMYY to CYYMM
     P*====================================================================
     Pchg_actm         b

     Dchg_actm         PI                  like(TRACTM)
     D actm                                like(TRACTM)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D actm_5          s              5  0
     D actm_4          s              4  0
     D actm_mth        s              2  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C** **              eval      actm_4 = actm
     C                   z-add     actm          actm_4
     C                   movel     actm_4        actm_mth
     C*
     C                   if        (actm_mth < 1 or actm_mth > 12) and
     C                             (actm_mth < 50)
     C                   movel     12            actm_4
     C                   endif
     C*
     C                   eval      srdti = actm_4
     C                   call      'SRCVTMY'     dtcymd
     C*
     C                   if        srerr = *off
     C                   movel     srdto         actm_5
     C                   move      actm_mth      actm_5
     C                   else
     C                   eval      actm_5 = actm
     C                   endif
     C*
     C                   return    actm_5

     P chg_actm        e
