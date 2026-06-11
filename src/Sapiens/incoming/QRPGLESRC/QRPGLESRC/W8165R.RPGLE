     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    
    H/title NGIF Prompt Program
    H*********************************************************************
    H*                                                                   *
    H*  Called from: Menu                                                *
    H*  Calls to: W8165C - Submitter                                     *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 7/29/2009 by c:62 for Project 114WTX                  *
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
    H*   12/14/10 C:220 Project: 114WXR                                   *
    H*                  Allow 00 months to extract all the claims        *
    H*                  Popup confirmation message if user enters 00 mth *
     H*                                                                    *
    H*   06/10/10 C:220 Project: 114WWL                                   *
    H*                  Add number of months parm in W8165C call.         *
     H*                                                                    *
    H*********************************************************************
     F**** Beg Chg ***** 12/14/10 ******************************************
     F****w8165f    cf   e             workstn
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8165f    cf   e             workstn infds(infds)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F**** End Chg ***** 12/14/10 ******************************************
     Fwmeml21   if   e           k disk
     Fwmemp     if   e             disk    rename(pem01:pem02)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D psds          esds                  extname(SZQ1P)
     D**** Beg Add ***** 12/14/10 ******************************************
     D infds           ds
     D key                   369    369
     D enter           c                   X'F1'
     D**** End Add ***** 12/14/10 ******************************************
     D optName         c                   const('NGIF Report Process')
     D pCo#            s              3
     D pMedt           s              7
     D pNaic           s              5
     D pMend           s              5
     D**** Beg Add ***** 06/10/10 ******************************************
     D pMth            s              2
     D**** Beg Add ***** 06/10/10 ******************************************
     D xxmeDt          s              7  0
     D xxqeDt1         s              7  0
     D xxqeDt2         s              7  0
     D xxqeDt3         s              7  0
     D xxqeDt4         s              7  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D ##co#           s              3  0
     D ##fnd           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##data          s             80
     D ##leng          s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D kkrsk#          s                   like(emrsk#)
     D xxrsk#          s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
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
    C*
     C     emky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkrsk#
    C*
     C                   exsr      pgmstr
    C*
     C                   dou       not *in90 or *inkc  or *inkl
     C*
     c                   exfmt     scr01
     c                   if        not *inkc
     c                   exsr      errck
     c                   if        not *in90
     c                   exsr      submit
     c                   endif
     c                   endif
     C*
     C                   enddo
    C*
     C                   seton                                        lr
    C*====================================================================
    C* ERRCK - Error checking
    C*====================================================================
     C     ERRCK         begsr
     C*
     C                   setoff                                       90
     C*
     C                   if        *inkj
     C                   exsr      kjdo
     C                   else
     C*
     C                   do
     C*
     C* Month-End Date must be valid date.
     C*
     c                   z-add     scmedt        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr='1'
     c                   seton                                        7190
     c                   leave
     c                   else
     c                   movel     srdto         scmedt
     c                   endif
     C*
     C* Month-End Date must be valid month-end
     C*
     C                   movel     scmedt        xxmnth            2
     C                   select
     c                   when      xxmnth='01'
     c                   movel     '0131'        scmedt
     c                   when      xxmnth='02'
     c                   movel     '0229'        scmedt
     c                   z-add     scmedt        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr='1'
     c                   movel     '0228'        scmedt
     c                   endif
     c                   when      xxmnth='03'
     c                   movel     '0331'        scmedt
     c                   when      xxmnth='04'
     c                   movel     '0430'        scmedt
     c                   when      xxmnth='05'
     c                   movel     '0531'        scmedt
     c                   when      xxmnth='06'
     c                   movel     '0630'        scmedt
     c                   when      xxmnth='07'
     c                   movel     '0731'        scmedt
     c                   when      xxmnth='08'
     c                   movel     '0831'        scmedt
     c                   when      xxmnth='09'
     c                   movel     '0930'        scmedt
     c                   when      xxmnth='10'
     c                   movel     '1031'        scmedt
     c                   when      xxmnth='11'
     c                   movel     '1130'        scmedt
     c                   when      xxmnth='12'
     c                   movel     '1231'        scmedt
     c                   endsl
     c***** Beg Add ***** 06/10/10 ****************************************
     C*
     C* Number of month can not be zero.
     C*
     c***** Beg Del ***** 12/14/10 ****************************************
     c*********          if        scnmth = 0
     c*********          seton                                        7690
     c*********          endif
     c***** End Del ***** 12/14/10 ****************************************
     c***** End Add ***** 06/10/10 ****************************************
     C*
     C*
     C* NAIC # Must be valid
     C*
     C                   if        scnaic<>0
     C                   z-add     scnaic        xxrsk#
     C                   movel     xxrsk#        kkrsk#
     c     emky          chain     pem01
     c                   if        not %found
     c                   seton                                        7290
     c                   leave
     c                   endif
     c                   else
     c                   seton                                        7290
     c                   leave
     c                   endif
     c*
     c***** Beg Add ***** 12/14/10 ****************************************
     c                   if        scnmth = 0  and *in90 = *off
     c                   dow       *inkg = *off and *inkl = *off
     c*
     c                   exfmt     wdw01
     c*
     c                   if        key = enter
     c                   leave
     c                   endif
     c                   enddo
     c*
     c                   if        *inkl = *on or key = enter
     c                   eval      *inkl = *off
     c                   seton                                          90
     c                   leave
     c                   endif
     c*
     c                   endif
     c***** End Add ***** 12/14/10 ****************************************
     C*
     C                   enddo
     C                   endif
     C*
     C                   endsr
    C*====================================================================
    C* KJDO - F10=Search
    C*====================================================================
     C     KJDO          begsr
     C*
     C                   seton                                        90
     C*
     C                   select
     C                   when      hfld='SCNAIC'
     C*
     C                   call      'W0050R'
     C                   parm      '0'           qqkc              1
     C                   parm      '0'           qqkl              1
     C                   parm      '2'           qqcall            1
     C                   parm      0             qqrrn             9 0
     C                   parm      0             qqrrn2            9 0
     C                   parm      *blanks       qqqte             1
     C                   parm      1             qqview            1 0
     **
     C                   if        qqrrn=0
     C                   seton                                        31
     C                   else
     C     qqrrn         chain     wmemp
     C                   movel     emrsk#        xxrsk#
     C                   z-add     xxrsk#        scnaic
     C                   endif
     C                   endsl
     C*
     C                   endsr
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     PGMSTR        begsr
    
     C                   call      'WA001R'
     C                   parm      *zeros        ##co#
     C                   parm      *zeros        ##fnd
     C                   parm      *blanks       ##conm
     C                   parm      *blanks       ##fnam

     C                   movel(p)  ##conm        scconm

     C                   movel(p)  optName       ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        scprds
    
    C***** Beg Add ***** 06/10/10 ****************************************
     C                   move      1             scnmth
    C***** End Add ***** 06/10/10 ****************************************
     C                   endsr
    C*====================================================================
    C* SUBMIT - Submit NGIF Reporting Process
    C*====================================================================
     C     SUBMIT        begsr
    C*
     C                   move      ##co#         pCo#
     C                   z-add     scmedt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         pMedt
     C                   movel     srdto         xxmeDt
     C                   move      scnaic        pNaic
     C*
     C                   eval      pMend='N'
     C                   move      xxmeDt        xxqeDt1
     C                   move      0331          xxqeDt1
     C                   move      xxmeDt        xxqeDt2
     C                   move      0630          xxqeDt2
     C                   move      xxmeDt        xxqeDt3
     C                   move      0930          xxqeDt3
     C                   move      xxmeDt        xxqeDt4
     C                   move      1231          xxqeDt4
     C*
     C                   if        xxmeDt=xxqedt1 or xxmeDt=xxqedt2 or
     C                             xxmeDt=xxqedt3 or xxmeDt=xxqedt4
     C                   eval      pMend='Y'
     C                   endif
    C*
     c**** Beg Add ***** 06/10/10 ******************************************
     c                   move      scnmth        pmth
     c**** End Add ***** 06/10/10 ******************************************
    C*
     C                   call      'W8165C'
     C                   parm                    pCo#
     C                   parm                    pMedt
     C                   parm                    pNaic
     c**** Beg Chg ***** 06/10/10 ******************************************
     C****               parm                    pMend
     C                   parm                    pMth
     c**** End Chg ***** 06/10/10 ******************************************
    C*
     C                   eval      *inkc=*on
    C*
     C                   endsr
