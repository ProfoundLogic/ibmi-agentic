     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
    H/title Agent Activity Report - Quote Types
    H*********************************************************************
    H*  CALLED FROM: W7870R                                              *
    H*                                                                   *
    H*  CALLS TO: None                                                   *
    H*                                                                   *
    H*  Created:   10/20/03      C:33      Project: WC1017               *
    H*********************************************************************
    H*  Revisions:                                                       *
     H*                                                                   *
     H* 10/30/14 C:136 Project: 114QOR                                    *
     H*                Sort by Agent Activity Report Selection Order      *
     H*                                                                   *
     H* 11/06/12  C:33  Project: WC2115                                   *
     H*                 -Implement renewal and new types.                 *
     H*                                                                   *
     H* 11/13/09  C:232 Seq#: WA071/70678                                 *
     H*                 -Implement 'Change all status'.                   *
     H*                 -Retrofitted the SEQ 63055.                       *
     H*                                                                   *
     H* 05/28/09  C:33  Project: 133WGV                                   *
      *                 Use a new file for quote status.                  *                        *
    H*                                                                   *
    H* 10/19/07 C:193 Project: WC1561                                    *
    H*                Add a new Quote Tracking type 'S' CLOSED.          *
     H*                                                                   *
    H*  11/18/04  C:33  Project:  132WCB                                 *
    H*                  Add processing for 'S' quote type                *
    H*********************************************************************
     Fw7870f1   cf   e             workstn infds(fids01)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*  Physical file definition (for refreshing subfile record)
     Fwxq2p     if a e             disk    rename(xq201:pfRcdF)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwxq2l3    uf   e           k disk    infds(dbFDS)
     F                                     rename(xq201:dbRcdF)
     Fwtqsl2    if   e           k disk    rename(pqs01:pqs02)
    
     D*entry
     D W7870R1         pr
     D  @@call                        1
     D  @@PQB                         1
     D  @@Mbr                        10
     D  @@kc                               like(*inkc)
     D  @@kl                               like(*inkl)
     D W7870R1         pi
     D  @@call                        1
     D  @@PQB                         1
     D  @@Mbr                        10
     D  @@kc                               like(*inkc)
     D  @@kl                               like(*inkl)

     D AddChgDltPgm    pr                  extPgm('W7870R02')
     D  ##kc                               like(*inkc)
     D  ##kl                               like(*inkl)
     D  ##Mode                        1    const
     D  ##RRN                         9  0
     D  ##PQB                         1    options(*nopass)

     D UpdateMbr       pr                  extpgm('UPDMBR')
     D  ##Mode                        1    const
     D  ##File                       10    const
     D  ##Mbr                        10

     D PopulateFile    pr

     D StatDesc        pr                  like(scStat)
     D  inStat                             like(Q2STAT)
    
     D #ofTypes        c                   15
     D qteTypes        s              1    dim(#ofTypes) ctdata perrcd(1)
     D  qteDesc        s             25    dim(#ofTypes) alt(QteTypes)
    
     D #ofHeadings     c                   1
     D hd              s             78    dim(#ofHeadings) ctdata perrcd(1)
    
     D #ofOptFunc      c                   15
     D of              s              1    dim(#ofOptFunc) ctdata perrcd(1)
     D  ofd            s             35    dim(#ofOptFunc) alt(of)
    
    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'Agt Act Rpt - Quote Types'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W7870R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   14
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##PQB           s              1
     D ##rrn           s              9  0
     D kkaars          s                   like(Q2aars)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C/COPY SCOPYWW01C
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scDBRR        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C*
     C     key1          klist
     C                   kfld                    kkaars
     C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
     C*
     C     chkssm        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*====================================================================
     C*
     C     errck1        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*====================================================================
     C*
     C     errck2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* INZSST - Initialize the subset fields
    C*====================================================================
     C*
     C     inzsst        begsr
     C                   endsr
     C*
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
     C*
     C     keyinz        begsr
     C*
     C                   eval      kkaars = *loval
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
     C*
     C     keyset        begsr
     C                   endsr
     C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
     C*
     C     keysfl        begsr
     C*
     C                   eval      kkaars = scaars
     C*
     C                   endsr
     C*
    C*================================================================
    C* KUDO - Process F20=Chg status on all
    C*================================================================
     C     kudo          begsr
     C*
     C                   eval      ##mode = '*'
     C                   call      acdpgm        acdpl                  99
     C                   exsr      kedo
     C*
     C                   endsr                                                  kgdo
     C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C*
     C     mvdbsc        begsr
     C*
     C                   eval      scaars = Q2aars
     C                   eval      scType = Q2QTYP
     C                   eval      scDesc = Q2DESC
     C                   eval      scStat = StatDesc(Q2STAT)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
     C*
     C     othfks        begsr
     C*
     C     *inku         caseq     *on           kudo                       f7=submit
     C                   endcs
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OTHOPT - Process other options
    C*====================================================================
     C*
     C     othopt        begsr
     C*
     C                   select
     C                   when      scSel = '20'
     C                   callp     AddChgDltPgm(*inkc:*inkl:'S':scDBRR)
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C*
     C     pgmend        begsr
      /Free
       If not *inkc and not *inkl;
          Callp AddChgDltPgm(*inkc:*inkl:'#':##rrn:scQteTypes);
          rstPgm = *inkl;
       Endif;
      /End-free
     C*
     C                   if        not *inkc and not *inkl
     C                   callp     AddChgDltPgm(*inkc:*inkl:'X':scDBRR:
     C                                          ##PQB)
     C                   eval      rstPgm = *inkl
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      @@PQB = ##PQB
     C                   endif
     C*
     C                   if        %parms >= 5
     C                   eval      @@kc = *inkc
     C                   eval      @@kl = *inkl
     C                   endif
     C*
     C                   eval      *inlr = *on
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
     C*
     C     pgmstr        begsr
     C*
     C                   callp     PopulateFile
      /Free
       scQteTypes = 'A';
      /End-free
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
     C*
     C     read          begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C                   read      dbRcdF                                 95
     C                   unlock    wxq2l3
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
     C*
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C                   readp     dbRcdF                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
     C*
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C     key1          setgt     dbRcdF                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETLL  - Set lower limits on file
    C*====================================================================
     C*
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21 = *on
     C     key1          setll     dbRcdF                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*====================================================================
     C*
     C     sf1upd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*====================================================================
     C*
     C     slctrd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
     C*
     C     subset        begsr
      /Free
       Select;
       When Q2stat = '#';
          meetSS = *off;
       Endsl;
      /End-free
     C                   endsr
     C*____________________________________________________________________
     C*





    C*====================================================================
    C* PopulateFile - Populate the file with quote types
    C*====================================================================
     PPopulateFile     b

     D PopulateFile    pi

     D x               s              3  0

     C                   dou       %eof
     C*
     C                   read      pqs02
     C*
     C                   if        not %eof
     C*
     C     QSaars        chain     dbRcdF
     C*
     C                   eval      Q2QTYP = QSKEY
     C                   eval      Q2DESC = QSDESC
     C                   eval      Q2STAT = 'I'
     C                   eval      Q2aars = QSaars
      /Free
       Select;
       When QSrenew = 'Y';
          Q2stat = '#';
       Endsl;
      /End-free
     C*
     C                   select
     C                   when      %found
     C                   update    dbRcdF
     C                   other
     C                   write     pfRcdF
     C                   endsl
     C*
     C                   endif
     C*
     C                   enddo

     PPopulateFile     e





    C*====================================================================
    C* StatDesc - Get status description
    C*====================================================================
     PStatDesc         b

     D StatDesc        pi                  like(scStat)
     D  inStat                             like(Q2STAT)

     C                   select
     C                   when      inStat = 'I'
     C                   return    'Include'
     C                   when      inStat = 'E'
     C                   return    'Exclude'
     C                   other
     C                   return    *blank
     C                   endsl

     PStatDesc         e





** Quote Types
PPending
QQuoted
WWritten
CCancelled
LLost
DDeclined
RReinstated
BBound
MBound w/Money
TTransfer In
XTransfer Out
FRenewal Pending
GRenewal Quote
USubmitted                                                     11/18/04
SClosed                                                        10/19/07
** Column headings
Opt      Description                   Status
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Include/Exclude       YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
220F20=Chg status on all    YNN
3



