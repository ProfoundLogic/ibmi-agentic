     H*****************************************************************
    H* (c) 2009 StoneRiver, Inc. All rights Reserved.                *
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H*                                                               *
     H*    Tennessee Data Submission - Prompt                         *
     H*                                                               *
     H*****************************************************************
     H*  Created on 10/24/2006 by c:62 for Project TN004              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 01/29/10  C:XXX  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff9710ftn  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftgtp     if   e             disk
VW#1 Fftgtl     if   e           k disk    rename(pgt01:pgt02)
    D*
    D*================================================================
    D*  Constants
    D*================================================================
     D optnm           c                   const('Tennesse Data Submission')
     D comapi          c                   const('FA007R')
     I*** BEGIN CHG *********** 01/29/10 ************************************
     D*** pdsnam          c                const('Progressive Data So-
     D***                                  lutions, Inc.')
     D    pdsnam       S             40    INZ
     I*** END CHG   ********* 01/29/10 **************************************
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
     D $$qtr           s              1
     D $$year          s              4

     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D***  Begin Add  ***  01/29/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/29/10  **************************************
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
     C                   parm      'QTR'         ##tbid
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*================================================================
    C*  Main Process Loop
    C*================================================================
     c*
     C                   exsr      getcof
     C                   exsr      getpgt
     c*
     c                   dou       not *in90
     c                   exfmt     scr01
     c                   setoff                                       903132
     c                   setoff                                       7172
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
     c* Quarter must be valid
     c*
     c                   eval      kktbid = 'QTR'
     c                   eval      kkkey = scqtr
     c     gtky          chain     pgt02
     c                   if        not %found
     c                   seton                                        7190
     c                   leave
     c                   endif
     c*
     c* Year must be valid
     c*
     c                   if        scyear < 2005 or scyear > 3000
     c                   seton                                        7290
     c                   leave
     c                   endif
     c*
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
     C***  Begin Chg  ***  01/29/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval        ##data  = brandname
     C***  End   Chg  ***  01/29/10  **************************************
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
     c                   if        hfld = 'SCQTR'
     c*
     c                   call      'F2380R'      gtpl
     c                   if        ##kc = *on or ##kl = *on
     c                             or ##rrn = *zeros
     c                   seton                                        31
     c                   else
     c     ##rrn         chain     pgt01
     c                   movel(p)  gtkey         scqtr
     c                   movel(p)  gtdesc        scqtrd
     c                   seton                                        32
     c                   endif
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
     c*
     c                   call      'F9710CTN'
     c                   parm                    $$co#
     c                   parm      scqtr         $$qtr
     c                   parm                    $$year
     c*
     c                   endsr
