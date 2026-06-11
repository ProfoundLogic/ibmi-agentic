     H*****************************************************************
     H copyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     h/define CONVERT
     H*****************************************************************
     H*                                                               *
     H*    Medical Data Call Submission - Prompt                      *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/02/2009 by c:227    Project MC331              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 07/13/09  C: 227 Proj: MC331                                  *
     H*                  Edit check changes                           *
     H*                                                               *
     H*****************************************************************
     Ff6301f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftgtp     if   e             disk
VW#1 Fftgtl     if   e           k disk    rename(pgt01:pgt02)
    F*  Submission control File
     Ffeu9l2    if   e           k disk
    F*
    D*
    D*================================================================
    D*  Constants
    D*================================================================
     D optnm           c                   const('NCCI Data Call Submission')
     D comapi          c                   const('FA007R')
     D pdsnam          c                   const('StoneRiver, Inc.')
    D*================================================================
    D*  Variables
    D*================================================================
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnd           s              3  0
     D ##fnam          s             40
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D ##tbid          s                   like(gttbid)
     D ##data          s             80
     D ##leng          s              3  0

     D $$co#           s              3
     D $$mth           s              2
     D $$qtr           s              1
     D $$year          s              4
     D $$ftcd          s              1
     D $$file          s             30

     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D kkyear          s                   like(u9year)
     D kkqtr           s                   like(u9qtr)
     D kkmth           s                   like(u9mth)
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     gtpl          plist
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##kc
     C                   parm      *blanks       ##kl
     C                   parm      *zeros        ##rrn
     C                   parm                    ##tbid
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C     u9ky          klist
     C                   kfld                    kkyear
     C                   kfld                    kkqtr
     C                   kfld                    kkmth
     C     u9ky1         klist
     C                   kfld                    kkyear
     C                   kfld                    kkqtr
    C*================================================================
    C*  Main Process Loop
    C*================================================================
     c*
     C                   exsr      getcof
     C                   exsr      getpgt
     c*
     c                   dou       not *in90
     c                   exfmt     scr01
     c                   setoff                                       90
     c                   setoff                                       313233
     c                   setoff                                       717273
     c                   setoff                                       747577
     c                   setoff                                       78
     c     *inkj         caseq     *on           kjdo
     c                   endcs
     c                   if        not *in90 and not *inkc
     c                   exsr      errck
     c                   endif
     c                   enddo
     c*
     c                   if        not *inkc and not *inkl
     c                   exsr      submit
     c                   endif
     c*
     c                   seton                                        lr
     c*=====================================================================
     c* ERRCK - Validate Screen Fields
     c*=====================================================================
     c     errck         begsr
     c*
     c                   do
     c*
     c* Either Month or Quarter should be entered
     c*
     c                   if        (scmth <> *blank and
     c                             scqtr <> *blank) or
     c                             (scmth = *blank and
     c                             scqtr = *blank)
     c                   seton                                        7590
     c                   leave
     c                   endif
     c*
     c* Month must be valid
     c*
     c                   if        scmth <> *blank
     c                   eval      kktbid = 'MTHS'
     c                   eval      kkkey = scmth
     c     gtky          chain     pgt02
     c                   if        not %found
     c                   seton                                        7190
     c                   leave
     c                   endif
     c                   endif
     c*
     c* Quarter must be valid
     c*
     c                   if        scqtr <> *blank
     c                   eval      kktbid = 'QTRS'
     c                   eval      kkkey = scqtr
     c     gtky          chain     pgt02
     c                   if        not %found
     c                   seton                                        7290
     c                   leave
     c                   endif
     c                   endif
     c*
     c* Year must be valid
     c*
     c                   if        scyear = 0
     c                   seton                                        7390
     c                   leave
     c                   endif
     c*
     c                   if        scyear <= 2009 or scyear > 3000
     c                   seton                                        7490
     c                   leave
     c                   endif
     c* Range should not be previously submitted
     c                   move      scqtr         kkqtr
     c*** BEG ADD *** 07/13/09 *********************************************
     c                   move      scyear        kkyear
     c*** END ADD *** 07/13/09 *********************************************
     c                   if        scmth <> *blanks
     c                   select
     c                   when      scmth = '01' or
     c                             scmth = '02' or
     c                             scmth = '03'
     c                   eval      kkqtr = 1
     c                   when      scmth = '04' or
     c                             scmth = '05' or
     c                             scmth = '06'
     c                   eval      kkqtr = 2
     c                   when      scmth = '07' or
     c                             scmth = '08' or
     c                             scmth = '09'
     c                   eval      kkqtr = 3
     c                   when      scmth = '10' or
     c                             scmth = '11' or
     c                             scmth = '12'
     c                   eval      kkqtr = 4
     c                   endsl
     c*** BEG DEL *** 07/13/09 *********************************************
     c*******            endif
     c******* The following line has been moved above **********************
     c*******            move      scyear        kkyear
     c*** END DEL *** 07/13/09 *********************************************
     c                   move      scmth         kkmth
     c     u9ky          setll     pu901
     c     u9ky          reade     pu901                                  55
     c                   dow       *in55 = *off
     c                   if        u9prfl <> 'F'
     c                   seton                                        7890
     c                   leave
     c                   endif
     c     u9ky          reade     pu901                                  55
     c                   enddo
     c*** BEG CHG *** 07/13/09 *********************************************
     c*                  else
     c*                  if        scmth = '  '
     c                   else
     c                   if        scmth = '  '
     c*** END CHG *** 07/13/09 *********************************************
     c     u9ky1         setll     pu901
     c     u9ky1         reade     pu901                                  55
     c                   dow       *in55 = *off
     c                   if        u9prfl <> 'F'
     c                   seton                                        7890
     c                   leave
     c                   endif
     c     u9ky1         reade     pu901                                  55
     c                   enddo
     c*** BEG CHG *** 07/13/09 *********************************************
     c*                  endif
     c*                  endif
     c                   endif
     c                   endif
     c*** END CHG *** 07/13/09 *********************************************
     c                   enddo
     c*
     c                   endsr
    c*================================================================
    c*  GETCOF - Gets company and fund names if possible; otherwise
    c*           get PDS name for company name
    c*================================================================
     c     getcof        begsr
     c*
     c                   call      comapi        compl                  99

B01  c                   if        *in99 = *on
     c                   movel(p)  pdsnam        ##data
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel     ##data        ##conm
E01  c                   endif

     c                   movel     ##conm        scconm
     c*
     c                   endsr
    c*================================================================
    c*  GETPGT - Get program title for display heading
    c*================================================================
     c     getpgt        begsr
     c*
     c                   movel(p)  optnm         ##data
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel     ##data        scprds
     c*
     c                   endsr
     c*=====================================================================
     c* KJDO - F10=Search
     c*=====================================================================
     c     kjdo          begsr
     c*
     c                   if        hfld = 'SCMTH'
     c*
     C                   eval      ##tbid = 'MTHS'
     c                   call      'F2380R'      gtpl
     c                   if        ##kc = *on or ##kl = *on
     c                             or ##rrn = *zeros
     c                   seton                                        31
     c                   else
     c     ##rrn         chain     pgt01
     c                   movel(p)  gtkey         scmth
     c                   movel(p)  gtdesc        scmthd
     c                   seton                                        31
     c                   endif
     c                   endif
     c*
     c                   if        hfld = 'SCQTR'
     c*
     C                   eval      ##tbid = 'QTRS'
     c                   call      'F2380R'      gtpl
     c                   if        ##kc = *on or ##kl = *on
     c                             or ##rrn = *zeros
     c                   seton                                        32
     c                   else
     c     ##rrn         chain     pgt01
     c                   movel(p)  gtkey         scqtr
     c                   movel(p)  gtdesc        scqtrd
     c                   seton                                        32
     c                   endif
     c                   endif
     c*
     c*
     c                   if        hfld = 'SCYEAR'
     c                   seton                                        77
     c                   endif
     c*
     c                   seton                                        90
     c*
     c                   endsr
     c*=====================================================================
     c* SUBMIT - Submit the EDI Process
     c*=====================================================================
     c     submit        begsr
     c*
     c                   move      ##co#         $$co#
     c                   move      scyear        $$year
     c
     c                   call      'F6301C'
     c                   parm                    $$co#
     c                   parm      scmth         $$mth
     c                   parm      scqtr         $$qtr
     c                   parm                    $$year
     c                   parm      'O'           $$ftcd
     c                   parm      *blank        $$file
     c
     c                   endsr
