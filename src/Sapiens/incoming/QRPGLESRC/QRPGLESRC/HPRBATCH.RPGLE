    H* (C) Copyright Progressive Data Solutions, Inc. 2002
    H*
    H/TITLE HPRBATCH - CodeReview Batch Program
    H**********************************************************************
    H*                                                                    *
    H* Called From: Menu                                                  *
    H* Calls to   : HPRBIIA - CodeReview Batch Program                    *
    H*                                                                    *
    H**********************************************************************
    H* Created on 07/23/2002 by c:62 for Project 114MDC                   *
    H**********************************************************************
    H* REVISIONS:                                                         *
    H*                                                                    *
    H**********************************************************************
     Ffaeop     if   e             disk
     Ffaehl     if   e           k disk
     Ffdcal13   if   e           k disk
     Ffddhl1    if   e           k disk
     Ffmcdl     if   e           k disk
     Ffmdil     if   e           k disk
     Ffmprl     if   e           k disk    usropn
     Fhprinpt   if a e             disk    rename(hprinptr:inptcr)
     D dbfds           ds
     D dbrrn                 397    400b 0
    D*
     D optnm           c                   const('Resubmit EOMBs throu-
     D                                     gh CodeReview')
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D qqcmd                         56
     D  qqst                          2    overlay(qqcmd:38)
    
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkco#           s                   like(caco#)
     D kksub#          s                   like(casub#)
     D kkfnd           s                   like(cafnd)
     D kkfyr           s                   like(cafyr)
     D kkcase          s                   like(cacase)
     D kkfein          s                   like(cafein)
     D kkmod           s                   like(camod)
     D kkeob#          s                   like(eoeob#)
     D kkrcn           s                   like(eorcn)
     D kkcpt4          s                   like(prcpt4)
     D kkefdt          s                   like(prefdt)
     D kkss#           s                   like(cdss#)
     D kkicd9          s                   like(diicd9)
     D kklost          s                   like(dilost) inz('D')
     D kkdseq          s                   like(cadseq)
     D from_date       s                   like(eofrdt)
     D to_date         s                   like(eotodt)
     D cut_off_date    s                   like(cafdat)
     D hdeob#          s                   like(eoeob#)
     D hdrcn           s                   like(eorcn)

     D ##ar            s              1
     D ##arls          s              1
     D ##ccod          s              1
     D ##crac          s              1
     D ##crvw          s              1
     D ##edi           s              1
     D ##freq          s              1
     D ##fsch          s              1
     D ##mcif          s              1
     D ##mcrv          s              1
     D ##med           s              1
     D ##mss           s              1
     D ##npwc          s              1
     D ##ntwk          s              1
     D ##os            s              1
     D ##prom          s              1
     D ##prvb          s              1
     D ##rhab          s              1
     D ##ur            s              1
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     D @@rrn           s              9  0
     D @@pend          s              1
     D @@pcod          s              3
     D @@chg           s              1
     D srdfld          s             80
     D srdlen          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0 inz(1)
     D y               s              2  0 inz(0)
     D z               s              2  0 inz(1)
     D lines           s              3  0 inz(0)
     D bills           s              3  0 inz(0)
     D length          s             15  5 inz(56)
     D chartest        s              1
     D date_test1      s               d   inz datfmt(*usa)
     D date_test2      s              7  0
     D date_test3      s              8  0
     D date_test4      s               d   inz datfmt(*iso)
     D char_co#        s              3
     D char_sub#       s              5
     D char_fnd        s              3
     D char_fyr        s              3
     D char_case       s              7
     D char_eob#       s              9
     D char_rcn#       s              3
     D char_fein       s              9
     D char_mod        s              3
     D char_zip        s              9
     D char_zipa       s              5
     D char_zipb       s              4
     D char_claim      s             22
     D eight_long      s              8
     D six_long        s              6
     D two_long        s              2

     D sequence        s                   like(cadseq) dim(100) inz(*zeros)
     D qca             s             56    dim(2) ctdata perrcd(1)

    C*****************************************************************
    C*  Parameter Lists
    C*****************************************************************
     C     *entry        plist
     C                   parm                    @@rrn
    C*****************************************************************
    C*  Key Lists
    C*****************************************************************
    C*
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
    C*
     C     caky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kkdseq
    C*
     C     cdky          klist
     C                   kfld                    kkss#
    C*
     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
     C     diky          klist
     C                   kfld                    kkicd9
     C                   kfld                    kklost
    C*
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt
    C*
     C     prky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
    C*****************************************************************
    C*  *LIKE Defines
    C*****************************************************************
     C                   setoff                                       90
    C*
    C* Call API for system flags
    C*
     C                   call      'SA001R'
     C                   parm      *blanks       ##wcif
     C                   parm      *blanks       ##mcif
     C                   parm      *blanks       ##wcin
     C                   parm      *blanks       ##ar
     C                   parm      *blanks       ##ntwk
     C                   parm      *blanks       ##ur
     C                   parm      *blanks       ##med
     C                   parm      *blanks       ##edi
     C                   parm      *blanks       ##wcdk
     C                   parm      *blanks       ##rhab
     C                   parm      *blanks       ##mss
     C                   parm      *blanks       ##fsch
     C                   parm      *blanks       ##prvb
     C                   parm      *blanks       ##npwc
     C                   parm      *blanks       ##os
     C                   parm      *blanks       ##arls
     C                   parm      *blanks       ##ccod
     C                   parm      *blanks       ##crvw
     C                   parm      *blanks       ##prom
     C                   parm      *blanks       ##mcrv
     C                   parm      *blanks       ##freq
     C                   parm      *blanks       ##crac
     C*
     C                   exsr      filtering
     C                   if        *in90 = *off
     C                   exsr      write_hprinpt
     C                   call      'HPRBIIA'
     C                   endif
     C*
     C* End the Program
     C*
     C                   eval      *inlr = *on
    C**********************************************************************
    C* Filtering - This subroutine selects line items to pass to CodeReview
    C**********************************************************************
     C     filtering     begsr
    C*
     C     @@rrn         chain     peo01                              99
     C                   seton                                        90
     C                   if        *in99 = *off
     C*
     C* Initialize work fields
     C*
     C                   eval      kkco#=eoco#
     C                   eval      kksub#=eosub#
     C                   eval      kkfnd=eofnd
     C                   eval      kkfyr=eofyr
     C                   eval      kkcase=eocase
     C                   eval      kkeob#=eoeob#
     C                   eval      kkrcn=eorcn
     C                   eval      kkfein=eofein
     C                   eval      kkmod=eomod
     C                   eval      from_date = *zeros
     C                   eval      to_date = *zeros
     C*
     C* Here we are reading thru the current bills line items
     C*
     C     caky1         chain     pca01                              99
     C                   dow       *in99 = *off
     C*
     C* First lets see if the line can be passed into CodeReview.
     C*
     C                   exsr      qualify_line
     C                   if        *in91 = *off
     C*
     C* Setting off 90 to communicate that we will be calling CodeReview.
     C*
     C                   setoff                                       90
     C*
     C* Here we are keeping track of the number of lines, because we can't
     C* send more than 100 lines into CodeReview.
     C*
     C                   eval      lines = lines + 1
     C*
     C* Here we are adding the accepted line to the three arrays.
     C* These arrays will be used when loading the input_area and
     C* hist_bills (holds the historical bill numbers).
     C*
     C                   eval      sequence(lines) = cadseq
     C*
     C* Stop process if we already have 100 lines
     C*
     C                   if        lines = 100
     C                   leave
     C                   endif
     C*
     C                   endif
     C*
     C     caky1         reade     pca01                                  99
     C                   enddo
     C                   endif
    C*
     C                   endsr
    C**********************************************************************
    C* Move_to_file - Move to fields for Batch process.
    C**********************************************************************
     C     Move_to_file  begsr
     C*
     C                   if        *in99 = *off and *in99 = *on
     C     1             chain     inptcr                             99
     C                   endif
    C*
     C                   eval      kkdseq = sequence(x)
     C*
     C     caky2         chain     pca01                              99
     C                   if        *in99 = *off
     C*
     C                   move      eoeob#        char_eob#
     C                   move      eorcn         char_rcn#
     C     char_eob#     cat(p)    '-':0         inpt01
     C                   cat       char_rcn#:0   inpt01
     C*
     C                   eval      kkss# = eoss#
     C     cdky          chain     pcd01                              99
     C                   if        *in99 = *off
     C*
     C                   if        cdsex = 'M' and cdsex = 'F'
     C                   eval      inpt03 = cdsex
     C                   else
     C                   eval      inpt03 = 'U'
     C                   endif
     C*
     C                   if        cddtob <> 0
     C     *cymd         move      cddtob        date_test4
     C                   move      date_test4    date_test3
     C                   move      date_test3    inpt40
     C                   eval      inpt02 = *zeros
     C                   else
     C                   eval      inpt40 = '00000000'
     C                   eval      inpt02 = 35
     C                   endif
     C*
     C                   endif
     C*
     C                   eval      inpt04 = '99'
     C                   eval      inpt05 = '99'
     C*
     C                   move      eofein        char_fein
     C                   move      eomod         char_mod
     C     char_fein     cat(p)    '-':0         inpt06
     C                   cat       char_mod:0    inpt06
     C*
     C                   move      eozip         inpt07
     C                   move      calin#        inpt08
     C*
     C                   if        cafdat <> 0
     C     *cymd         move      cafdat        date_test4
     C                   move      date_test4    date_test3
     C                   move      date_test3    inpt09
     C                   else
     C                   move      '00000000'    inpt09
     C                   endif
     C*
     C                   if        catdat <> 0
     C     *cymd         move      catdat        date_test4
     C                   move      date_test4    date_test3
     C                   move      date_test3    inpt10
     C                   else
     C                   move      '00000000'    inpt10
     C                   endif
     C*
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
     C                   move      dhpos         inpt11
     C                   eval      inpt12 = dhprcd
     C                   eval      inpt13 = dhmod1
     C                   eval      inpt14 = dhmod2
     C                   eval      inpt15 = '001'
     C                   endif
     C*
     C                   eval      inpt16 = *blanks
     C                   eval      inpt17 = *blanks
     C                   eval      inpt18 = *blanks
     C                   eval      inpt19 = *blanks
     C                   eval      inpt20 = *blanks
     C                   eval      inpt21 = *blanks
     C                   eval      inpt22 = *blanks
     C                   eval      inpt23 = *blanks
     C*
     C                   if        ##prom = 'Y'
     C     ehky          chain     peh01                              99
     C                   if        *in99 = *off
     C     1             do        4             x
     C*
     C                   select
     C                   when      x = 1
     C                   eval      kkicd9 = ehdia1
     C                   when      x = 2
     C                   eval      kkicd9 = ehdia2
     C                   when      x = 3
     C                   eval      kkicd9 = ehdia3
     C                   when      x = 4
     C                   eval      kkicd9 = ehdia4
     C                   endsl
     C*
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   select
     C                   when      x = 1
     C                   movel     dids1         inpt16
     C                   when      x = 2
     C                   movel     dids1         inpt17
     C                   when      x = 3
     C                   movel     dids1         inpt18
     C                   when      x = 4
     C                   movel     dids1         inpt19
     C                   endsl
     C                   endif
     C                   enddo
     C                   endif
     C*
     C                   select
     C                   when      dhdxr1 <> *blanks
     C                   eval      inpt20 = '1'
     C                   when      dhdxr2 <> *blanks
     C                   eval      inpt21 = '2'
     C                   when      dhdxr3 <> *blanks
     C                   eval      inpt22 = '3'
     C                   when      dhdxr4 <> *blanks
     C                   eval      inpt23 = '4'
     C                   endsl
     C                   endif
     C*
     C                   move      cachrg        inpt27
     C                   move      *zeros        inpt29
     C*
     C                   select
     C                   when      ##mcrv = 'Y' and ##crvw = 'Y'
     C                   eval      inpt30 = 'B'
     C                   when      ##mcrv = 'Y'
     C                   eval      inpt30 = 'M'
     C                   other
     C                   eval      inpt30 = 'C'
     C                   endsl
     C*
     C                   if        ##prom = 'Y'
     C                   eval      inpt31 = 'Y'
     C                   else
     C                   eval      inpt31 = 'N'
     C                   endif
    C*
     C                   move (p)  eoco#         inpt33
     C                   move (p)  eosub#        inpt34
     C                   move (p)  eofnd         inpt35
     C*
     C                   z-add     y             inpt36
     C                   eval      y = y + 1
     C*
     C                   move      eofyr         char_fyr
     C                   move      eocase        char_case
     C     char_fyr      cat(p)    '-':0         inpt39
     C                   cat       char_case:0   inpt39
     C*
     C                   eval      inpt46 = 'X'
     C*
     C                   endif
    C*
     C                   endsr
    C**********************************************************************
    C* Qualify_line - Sees if line should be passed into CodeReview
    C**********************************************************************
     C     Qualify_line  begsr
     C*
     C                   setoff                                       91
     C*
     C* Paid must be greater than 0
     C*
     C                   if        capaid = *zeros
     C                   seton                                        91
     C                   endif
     C*
     C* Service date must be greater than the current date - 3yrs.
     C*
     C                   time                    dsdttm
     C                   move      dsdate        date_test1
     C                   subdur    3:*y          date_test1
     C     *cymd         move      date_test1    cut_off_date
     C*
     C                   if        cafdat < cut_off_date
     C                   seton                                        91
     C                   endif
     C*
     C* This can't be a user defined code.
     C*
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  caast         qqst
     C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     c*
     C                   open      fmprl
     C                   movel(p)  cacode        kkcpt4
     C                   eval      kkefdt = cafdat
     C     prky1         setgt     ppr01
     C     prky2         readpe    ppr01                                  99
     C                   if        *in99 = *off and pr1ag = 'Y'
     C                   seton                                        91
     C                   endif
     C*
     C                   close     fmprl
     C*
     C                   call      'QCMDEXC'                              99
     C                   parm                    qca(2)
     C                   parm                    length
     C*
     C* Exclude dental codes
     C*
     C                   movel     cacode        chartest
     C                   if        chartest = 'D'
     C                   seton                                        91
     C                   endif
     C*
     C* Exclude pharmacy codes except 'J' codes.
     C*
     C*
     C* Exclude routine vision codes.
     C*
    C*
     C                   endsr
    C**********************************************************************
    C* Write_hprinpt - Writes Hprinpt records.
    C**********************************************************************
     C     Write_hprinpt begsr
     C*
     C                   z-add     0             y
     C*
     C     1             do        lines         x
    C*
     C                   clear                   inptcr
     C                   exsr      move_to_file
     C                   write     inptcr
     C*
     C                   enddo
    C*
     C                   endsr
**  (QCMDEX)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMPRL)
