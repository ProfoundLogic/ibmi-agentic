     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* This program will be used to calculate Lost Time Number of    *
     H* Days .                                                        *
     H*****************************************************************
     H*  Created by:                                                  *
     H*  07/07/11  C:251 Project: 114WZN                              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     F* WORK COMP CLAIM MASTER FILE
     Fwmcml1    uf   e           k disk
     Ffawhl6    if   e           k disk
     Fftgtl     if   e           k disk
     F*
     D psds          esds                  extname(szq1p)
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     D srdate1         s              7  0
     D srdate2         s              7  0
     D xxdt            s              7  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*
     D @day            s              5  0
     D @@ltday         s              5  0
     D @@rrn           s              9  0
     D @@mode          s              1
     D @@rtcd          s              1
     D ##rtcd          s              1
     D ##grp           s              3
     D ##co            s              3
     D ##sub           s              5
     D ##fnd           s              3
     D kksub#          s              5  0
     D @@sub#          s              5  0
     D*
     D kkco#           s                   like(cmco# )
     D kkfnd           s                   like(cmfnd )
     D kkfyr           s                   like(cmfyr )
     D kkcase          s                   like(cmcase)
     D @@co#           s                   like(cmco# )
     D @@fnd           s                   like(cmfnd )
     D @@fyr           s                   like(cmfyr )
     D @@case          s                   like(cmcase)
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D kkwkdt          s                   like(whwkdt)
     D ltstr           s                   like(whwkdt)
     D ltend           s                   like(whwkdt)
     D @@wgrp          s                   like(cmfnd )
     C*
     C     whky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkwkdt
     C*
     C     whky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     cmky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     *entry        plist
     C                   parm                    @@mode
     C                   parm                    @@co#
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@ltday
     C                   parm                    @@wgrp
     C                   parm                    @@rtcd
     C*
     C* Initialize
     C                   eval      @days = *zeros
     C                   eval      @@ltday  = *zeros
     C*
     C                   eval      kkco# = @@co#
     C                   eval      kksub# = @@sub#
     C                   eval      kkfnd = @@fnd
     C                   eval      kkfyr = @@fyr
     C                   eval      kkcase = @@case
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt
     C*
     C* Call FA700R to get X-Ref fund
     C                   exsr      SrXref
     C*
     C** Read through Managed care history file to get the loss time days
     C                   exsr      SrHist
     C*
     C** Update WMCMP/CMTADL with number of days returned from API
     C*
     C     cmky          chain     pcm01
     C                   if        %found
     C                   Z-ADD     @@ltday       cmtadl
     C                   update    pcm01
     C                   endif
     C*
     C                   eval      *inlr = *on
     C*================================================================
     C* SrXref - Get Cross reference fund
     C*================================================================
     C     SrXref        begsr
     C*
     C                   move      @@co#         ##co
     C                   move      @@sub#        ##sub
     C                   move      @@fnd         ##fnd
     C*
     C                   call      'FA700R'
     C                   parm      @@rtcd        ##rtcd
     C                   parm                    ##co
     C                   parm      *blank        ##grp
     C                   parm                    ##sub
     C                   parm                    ##fnd
     C*
     C*** VALID RECORD ' ' = YES
     C*
     C                   if        ##rtcd = *blanks
     C                   move      ##grp         @@wgrp
     C                   else
     C                   eval      @@wgrp = *zeros
     C                   endif                                                  ##rtcd = *blanks
     C*
     C                   endsr
     C*================================================================
     C* SrHist - Get the records from managed care history file
     C*================================================================
     C     SrHist        begsr
     C*
     C                   eval      kkwkdt = *loval
     C* Read FAWHP
     C
     C     whky          setll     pwh01
     C     whky1         reade     pwh01
     C*
     C                   dow       not %eof
     C*
     C                   eval      kktbid = 'WKCD'
     C                   eval      kkkey  = whcode
     C*
     C     gtky          chain     pgt01
     C                   if        %found
     C*
     C                   select
     C                   when      gtfg01 = 'L'
     C                   eval      ltstr = whwkdt
     C*
     C                   when      gtfg01 = 'R' and ltstr <> *zeros
     C                   eval      ltend = whwkdt
     C                   endsl
     C*
     C                   endif
     C*
     C* calculate the number of days in b/w start and end date
     C*
     C                   if        ltstr <> *zeros and ltend <> *zeros
     C                   exsr      SrCalcDay
     C                   endif
     C*
     C     whky1         reade     pwh01
     C                   enddo
     C*
     C* If end day not found , then use current date as end date.
     C*
     C                   if        ltstr <> *zeros
     C                   eval      ltend  = xxdt
     C                   exsr      SrCalcDay
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* SrCalcDay - Calculate total Loss time days
     C*================================================================
     C     SrCalcDay     begsr
     C*
     C*** logic to calculate days difference and add in subsequent cycles
     C                   call      'SRDDIF7'
     C                   parm      ltstr         srdate1
     C                   parm      ltend         srdate2
     C                   parm      *Zeros        @days             5 0
     C                   parm      *Blanks       srerr             1
     C*
     C                   eval      @@ltday = @@ltday + @days
     C*
     C                   eval      ltstr = *zeros
     C                   eval      ltend = *zeros
     C                   eval      @days = *zeros
     C*
     C                   endsr
