    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE W8276R
     H**************************************************************************
    H*  W8276R     : First Health Claims Ack - Work with Claim Errors
    H*
     H*  Description:
     H*
    H*  Called from: W8275R01 - First Health Claims Ack - Transfer History
    H*  Calls to   : W8276R01 - First Health Claims Ack - Claim Errors
    H*
     H**************************************************************************
     H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8276f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwzc2p     uf   e             disk
     F                                     rename(pc201:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwzc2l2    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pc201:dbrcdf)
    F*
    F*  Workers' Compensation Claims Master File
     Fwmcml1    uf   e           k disk
    D*
     D hd              s             78    dim(01) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('First Health Claim Errors')
     D slmsgi          c                   const('MC49998')
     D slmsgf          c                   const('FSMESG')
     D acdpgm          c                   const('W8276R01')
     D comapi          c                   const('FA007R')
     D uspapi          c                   const('F0006R')
     D infomx          c                   const(1)
     D viewmx          c                   const(1)
     D sf1pag          c                   const(11)
     D completed       c                   const('***COMPLETED***-
     D                                     COMPLETED***CO-
     D                                     MPLETED***')
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##call          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rtcd          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@clm#          s                   like(c2clm#)
     D @@dcid          s                   like(c2dcid)
     D kkclm#          s                   like(c2clm#)
     D kkco#           s                   like(cmco#)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkcase          s                   like(cmcase)
     D updlin          s              1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C/COPY SCOPYWW01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@dcid
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##call
     C                   parm      *blanks       ##rtcd
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     key1          klist
     C                   kfld                    kkclm#
    C*
     C     cmky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
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
    C*
     C                   clear                   kkclm#
    C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
    C*
     C                   eval      %subst(kkclm#:1:3) = scpst1
     C                   eval      %subst(kkclm#:4:6) = scpst2
     C                   eval      %subst(kkclm#:7)   = scpst3
    C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
    C*
     C                   eval      %subst(kkclm#:1:3) = scfnd
     C                   eval      %subst(kkclm#:4:6) = scfyr
     C                   eval      %subst(kkclm#:7)   = sccase
    C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   if        updlin = *off
    C*
     C                   eval      scfnd = %subst(c2clm#:1:3)
     C                   eval      scfyr = %subst(c2clm#:4:6)
     C                   eval      sccase= %subst(c2clm#:7)
    C*
     C                   eval      scname = %trim(c2fsn) + ' ' + c2mid
     C                   if        c2mid <> *blanks
     C                   eval      scname = %trim(scname) + '.' + ' ' + c2lsn
     C                   else
     C                   eval      scname = %trim(c2fsn) + ' ' + c2lsn
     C                   endif
     C                   eval      scname = %trim(scname)
    C*
     C                   endif
    C*
     C                   movel     *off          updlin
    C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   eval      *in86 = *off
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C     scsel         caseq     '6'           opt06
     C                   endcs
     C                   endsr
    C*================================================================
    C* OPT06 - Complete Acknowledgement
    C*================================================================
     C     opt06         begsr
    C*
     C                   movel     *off          updlin
     C                   move      'C'           ##mode
     C                   move      ' '           ##call
     C                   move      *off          *in86
    C*
     C                   exfmt     scr06
    C*
     C                   if        *inkl = *off and *inkc = *off
     C                   movel     ##co#         kkco#
     C                   movel     scfnd         kkfnd
     C                   movel     scfyr         kkfyr
     C                   movel     sccase        kkcase
    C*
     C     cmky1         chain     pcm01
     C                   if        %found(wmcml1)
    C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmdt
     C                   movel     dstime        cmtime
     C                   movel     q1pgm         cmpgm
     C                   movel     q1user        cmuser
     C                   update    pcm01
    C*
     c*** begin del *** 11/08/06 *******************************************
     C*****scdbrr        chain     pfrcdf
     C**********         if        %found(wzc2p)
    C*
     C**********         movel     'Y'           c2prfl
     C**********         movel     'Y'           scprfl
     C**********         movel(p)  completed     scname
     C**********         movel     *on           updlin
    C*
     C**********         update    pfrcdf
    C*
     C**********         endif
     C**********         else
     C**********         eval      *in86 = *on
     c*** end   del *** 11/08/06 *******************************************
     C                   endif
     c*** begin add *** 11/08/06 *******************************************
     C     scdbrr        chain     pfrcdf
     C                   if        %found(wzc2p)
    C*
     C                   movel     'Y'           c2prfl
     C                   movel     'Y'           scprfl
     C                   movel(p)  completed     scname
     C                   movel     *on           updlin
    C*
     C                   update    pfrcdf
    C*
     C                   endif
     c*** end   add *** 11/08/06 *******************************************
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   if        *inkc = *off and *inkl = *off
     C                   eval      *inkl = *on
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   movel     *off          updlin
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21  = *on
     C                   dou       c2dcid = @@dcid or *in95
     C                   read      dbrcdf                                 95
     C                   enddo
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21  = *on
     C                   dou       c2dcid = @@dcid or *in95
     C                   readp     dbrcdf                                 95
     C                   enddo
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
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
Opt Group Year  Case      Claimant name
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Mark Completed         YYY
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
