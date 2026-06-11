     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE S1225R - Work with Contact Sub-Heading Information
     H*******************************************************************
     H* Created on 02/01/2010 by c:62 for Project 25WQK                 *
     H*******************************************************************
     H* Revisions:                                                      *
     H*                                                                 *
     H*******************************************************************
     Fs1225f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fsms2p     if   e             disk
     F                                     rename(ps201:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fsms2l1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ps201:dbrcdf)
     Fsms2l     if   e           k disk    PREFIX(S3:2)
     Fsmchp     if   e             disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(14) ctdata perrcd(1)
     D ofd             s             35    dim(14) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Contact SubHeading Information'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'S1225R02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##rrn3          s              9  0
     D ##call          s              1
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D k1seqs          s                   like(s2seqh)
     D kkco#           s                   like(s2co#)
     D kktype          s                   like(s2type)
     D kkseqh          s                   like(s2seqh)
     D kkseqs          s                   like(s2seqh)
     D kkhdg1          s                   like(s2hdg1)
     D kkhdg2          s                   like(s2hdg2)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn2        ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key           klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
     C*
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
     C                   kfld                    kkseqs
     C                   kfld                    kkhdg1
     C                   kfld                    kkhdg2
     C*
     C     s2ky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
     C                   kfld                    k1seqs
     C*
     C     s2ky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C* Can't move item up if it is the first or second item
     C*
     C                   if        (scsel='21' and scseqs=1) or
     C                             (scsel='21' and scseqs=2)
     C                   seton                                        7190
     C                   endif
     C*
     C* Can't move item down if it is the last item
     C*
     C                   eval      k1seqs=99999
     C     s2ky1         setgt     ps201
     C     s2ky2         readpe    ps201
     C                   if        not %eof
     C                   if        scsel='22' and scseqs=s3seqs
     C                   seton                                        7290
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C     @@rrn2        chain     pch01
     C*
     C                   eval      kkco#=chco#
     C                   eval      kktype=chtype
     C                   eval      kkseqh=chseqh
     C                   eval      kkseqs=*loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   move      scpst1        kkseqs
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkseqs=scseqs
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scseqs=s2seqs
     C                   eval      schdg1=s2hdg1
     C                   eval      schdg2=s2hdg2
     C                   eval      scstat=s2stat
     C*
     C                   if        schdg1=*blanks
     C                   eval      schdg1='* * No Sub-Heading * *'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* OPT20 - Change Status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode='S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr
    C*================================================================
    C* OPT21 - Move Up
    C*================================================================
     C     opt21         begsr
     C*
     C                   eval      ##mode='U'
     C                   call      acdpgm        acdpl                  99
     C                   move      *on           autorf
     C*
     C                   endsr
    C*================================================================
    C* OPT22 - Move Down
    C*================================================================
     C     opt22         begsr
     C*
     C                   eval      ##mode='W'
     C                   call      acdpgm        acdpl                  99
     C                   move      *on           autorf
     C*
     C                   endsr
    C*================================================================
    C* OPT25 - Contact Information
    C*================================================================
     C     opt25         begsr
     C*
     C                   call      'S2225R'
     C                   parm      @@call        ##call
     C     *inkc         parm      *blanks       ##kc
     C     *inkl         parm      *blanks       ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      @@rrn2        ##rrn2
     C                   parm      scdbrr        ##rrn3
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '21'          opt21
     C     scsel         caseq     '22'          opt22
     C     scsel         caseq     '25'          opt25
     C                   endcs
     c*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   movel(p)  chhdg1        scmhdg
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C     key           reade     dbrcdf                                 95
     C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C     key           readpe    dbrcdf                                 95
     C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C     key1          setgt     dbrcdf                             96
     C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C     key1          setll     dbrcdf                             96
     C*
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt  Heading                                                       Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
12121=Move Up               YNN
12222=Move Down             YNN
12525=Contacts              YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
