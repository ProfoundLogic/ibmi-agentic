     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title Quarterly WMCMP Report
     H*====================================================================
     H*  Created: 11/08/10   C:53   Project: SY246
     H*====================================================================
    H*  Revisions:
     H*
     H* 04/05/12 C:279 Project: SY246  Priss #35
     H*                Modified to correct the individual state count
     H*                and total claim count.
     H*
     H*====================================================================

     F*** co#/jrst
     FWMCML13   if   e           k disk
     * System Company
     FSMCOL     if   e           k disk
     * System Company
     FWMFNL     if   e           k disk
    F*  Printer file
     Fqsysprt   o    f  132        printer oflind(*inof)

     D ltary           s              1    dim(50)
     D ltarycnt        s              9  0 dim(50)
    D*================================================================
    D*  Data Structures
    D*================================================================
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

      *---------------------------------------------------------------------
      * Constants
      *---------------------------------------------------------------------
     D comapi          c                   const('WA001R')
     D xxtitl1         c                   const('  Claim count by State  ')
     D xxtitl2         c                   const('Claim count by Lost Time')
      *---------------------------------------------------------------------
    D*================================================================
    D*  External Data Structure
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D svjrst          s                   like(cmjrst)
     D countI          s                   like(cmbul1)
     D countM          s                   like(cmbul1)
     D countR          s                   like(cmbul1)
     D countB          s                   like(cmbul1)
     D countst         s                   like(cmbul1)
     D totclm          s                   like(cmbul1)
    D*---------------------------------------------------------------------
     D companynm       s             40
     D ##data          s             80
     D ##leng          s              3  0
     D kkco#           s              3  0
     D xxco#           s              3
     D xx1st           s              1
     D i               s              2  0
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@co#             3
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     fnky          KLIST
     C                   KFLD                    cmco#
     C                   KFLD                    cmfnd
      *
    C*================================================================
    C*                  M A I N L I N E
    C*================================================================
     C*
     C                   exsr      initialize
     C*
     C* Read through WMCMP and populate the report data
     C*
     C     *loval        setll     pcm01
     C                   read      pcm01                                  90
     C                   exsr      getco
     C                   Dow       *in90 = *off
     C*
     C****** Beg Add ***** 04/05/12 ***************************************
     C                   if        cmstat = 'O'
     C****** End Add ***** 04/05/12 ***************************************
     C     fnky          chain     pfn01                              91
     C                   if        fntest <> 'Y'
     C*
     C                   if        *inof = *on
     C                   except    Header
     C                   except    Header1
     C                   eval      *inof = *off
     C                   endif
      *
     C                   If        svjrst <> cmjrst
     C                   If        xx1st = *off
     C                   except    prtst
     C                   else
     C                   eval      xx1st = *off
     C                   Endif                                                  svfnd <> d1fnd
     C                   eval      countst = 0
     C                   eval      svjrst = cmjrst
     C                   Endif                                                  svfnd <> d1fnd
     C*
     C                   eval      countst = countst + 1
     C                   eval      totclm = totclm + 1
     C*
     C                   select
     C     cmltfl        wheneq    'I'
     C                   eval      countI = countI + 1
     C     cmltfl        wheneq    'M'
     C                   eval      countM = countM + 1
     C     cmltfl        wheneq    'R'
     C                   eval      countR = countR + 1
     C     cmltfl        wheneq    ' '
     C                   eval      countB = countB + 1
     C                   eval      *in20 = *on
     C*
     C                   other
     C                   eval      i=1
     C     cmltfl        lookup    ltary(i)                               98
     C                   if        *in98=*on
     C                   eval      ltarycnt(i)=ltarycnt(i) + 1
     C                   else
     C*
     C                   eval      i=1
     C     *blanks       lookup    ltary(i)                               98
     C                   if        *in98=*on
     C                   eval      ltary(i)=cmltfl
     C                   eval      ltarycnt(i)=ltarycnt(i) + 1
     C                   endif
     C*
     C                   endif
     C                   endsl
     C                   endif                                                  svfnd <> d1fnd
     C*
     C****** Beg Add ***** 04/05/12 ***************************************
     C                   endif
     C****** End Add ***** 04/05/12 ***************************************
     C                   Read      Pcm01                                  90
     C*
     C                   if        cmco# <> coco# and *in90 <> *on
     C                   except    prtst
     C                   except    prttot
     C*
     C**** print Lost Time summary
     C*
     C                   eval      *in81 = *off
     C                   eval      *in82 = *on
     C                   except    Header
     C                   except    prtlt
     C                   exsr      prtary
     C                   except    prttot
     C                   exsr      getco
     C                   exsr      initialize
     C                   endif
     C*
     C                   Enddo
     C*
     C****** Beg Add ***** 04/05/12 ***************************************
     C                   if        countst > 0
     C                   except    prtst
     C                   eval      countst = 0
     C                   endif
     C****** End Add ***** 04/05/12 ***************************************
     C                   except    prttot
     C*
     C**** print Lost Time summary
     C*
     C                   eval      *in81 = *off
     C                   eval      *in82 = *on
     C                   except    Header
     C                   except    prtlt
     C                   exsr      prtary
     C                   except    prttot
     C                   eval      *inlr = *on
     C                   Return
    C*===============================================================*
     C     initialize    Begsr
     C*
     C                   eval      *inof = *on
     C                   eval      *in81 = *on
     C                   eval      *in82 = *off
     C                   eval      *in20 = *off
     C                   eval      countst = 0
     C                   eval      countI  = 0
     C                   eval      countM  = 0
     C                   eval      countR  = 0
     C                   eval      totclm  = 0
     C                   eval      xx1st = *on
     C* Time & date
     C                   Time                    dsdttm
     C                   Endsr
    C*===============================================================*
     C     getco         Begsr
     C     cmco#         chain     pco01                              60
    C*
     C                   if        *in60 = *off
     C*
     C                   move      coco#         xxco#
     C     coname        cat(p)    '#':4         ##data
     C                   cat       xxco#:1       ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        companyNm
    C*
     C                   endif
    C*
     C                   Endsr
    C*===============================================================*
     C     prtary        Begsr
     C*
     C                   Do        50            i
     C                   If        ltary(i) <> *blank
     C                   except    prtltary
     C                   else
     C                   leave
     C                   Endif
     C                   Enddo
     C*
     C                   Endsr
    C*===============================================================*
     OQsysprt   e            Header         1 05
     O                       dstime              09 '  :  :  '
     O                       CompanyNm           86
     O                       dsdate             132 '  /  /    '
     O*
     O          e            Header         3
     o                       q1pgm               11
     O               81      xxtitl1             76
     O               82      xxtitl2             76
     O                                          127 'Page:'
     O                       page          Z    132
    O*===============================================================*
     O          e            Header1        1
     O                                           35 'Total'
     O          e            Header1        2
     O                                           22 'State'
     O                                           36 'Claims'
    O*===============================================================*
     O          e            Prtst          1
     O                       svjrst              20
     O                       countst       j     36
    O*===============================================================*
     O          e            Prttot      1  2
     O                                           14 'Total Claims:'
     O                       totclm        J     36
    O*===============================================================*
     O          e            Prtlt          2
     O                                           23 'Total Indemnity   (I):'
     O                       countI        j     36
     O*
     O          e            Prtlt          2
     O                                           23 'Total Medical     (M):'
     O                       countM        j     36
     O*
     O          e            Prtlt          2
     O                                           23 'Total Record only (R):'
     O                       countR        j     36
     O*
     O          e    20      Prtlt          2
     O                                           23 'Total Blank       ( ):'
     O                       countB        j     36
     O*
     O          e            Prtltary       2
     O                                           23 'Total Other       ( ):'
     O                       ltary(i)            21
     O                       ltarycnt(i)   j     36
