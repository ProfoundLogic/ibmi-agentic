    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE W827601R
     H**************************************************************************
    H*  W8276R01   : First Health Claims Ack - Claim Errors
    H*
     H*  Description:
     H*
    H*  Called from: W8276R - First Health Claims Ack - Work with Claim Errors
    H*  Calls to   : W0070R - Work with Claims
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
     Fw8276f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwzc2p     if   e             disk
     F                                     rename(pc201:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     F******wzc2l2    if   e           k disk
     F                                     infds(dbfds)
     F******                               rename(pc201:dbrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwzc4l1    if   e           k disk
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
     D acdpgm          c                   const('W0070R')
     D comapi          c                   const('FA007R')
     D uspapi          c                   const('F0006R')
     D infomx          c                   const(1)
     D viewmx          c                   const(1)
     D sf1pag          c                   const(11)
     D clmopt          c                   const('066=Claim info      -
     D                                            YYN')
     D clropt          c                   const('                    -
     D                                            NNN')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##ar            s              1
     D ##call          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##edi           s              1
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fsch          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##mcif          s              1
     D ##med           s              1
     D ##mode          s              1
     D ##mss           s              1
     D ##npwc          s              1
     D ##ntwk          s              1
     D ##prvb          s              1
     D ##rhab          s              1
     D ##rrn           s              9  0
     D ##sort          s              2
     D ##ur            s              1
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     D #1case          s              7
     D #1co#           s              3
     D #1fnd           s              3
     D #1fyr           s              3
     D #2rrn           s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkclm#          s                   like(c2clm#)
     D kkcode          s                   like(c4code)
     D e               s              2  0

     D dserr           ds
     D  errs                         40
     D   c2er1                             overlay(errs:1)
     D   c2er2                             overlay(errs:5)
     D   c2er3                             overlay(errs:9)
     D   c2er4                             overlay(errs:13)
     D   c2er5                             overlay(errs:17)
     D   c2er6                             overlay(errs:21)
     D   c2er7                             overlay(errs:25)
     D   c2er8                             overlay(errs:29)
     D   c2er9                             overlay(errs:33)
     D   c2er10                            overlay(errs:37)
     D   erra                         4    overlay(errs) dim(10)
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
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     key1          klist
     C                   kfld                    kkclm#
    C*
     C     key4          klist
     C                   kfld                    kkcode
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
    C*
     C                   select
    C*
     C                   when      scsel = '5'
     C                   eval      scsel = 'O5'
     C                   eval      e = sce
    C*
     C                   when      scsel = '6'
     C                   eval      e = sce
    C*
     C                   endsl
    C*
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
    C*
     C                   eval      ##data = 'First Health Claim Errors'
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
    C*
     C                   eval      ##data = 'First Health Claim Error Codes'
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprd5
    C*
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
     C****************** movel(p)  scpst1        kkclm#
    C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
    C*
     C                   movel(p)  c2clm#        kkclm#
    C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   movel(p)  erra(e)       sccode
     C                   movel(p)  erra(e)       kkcode
     C     key4          chain     pc401
    C*
     C                   eval      scdesc = *blanks
     C                   if        %found(wzc4l1)
     C                   movel(p)  c4desc        scdesc
     C                   endif
    C*
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
    C*
     C                   select
    C*
     C                   when      scsel = 'O5'
     C                   exsr      othopt5
     C                   eval      scsel = '  '
     C                   eval      scrrn1 = sfrrn1
     C                   eval      *in90 = *on
    C*
     C                   when      scsel = '6'
     C                   exsr      opt06
     C                   eval      scsel = '  '
     C                   eval      scrrn1 = sfrrn1
     C                   eval      *in90 = *on
    C*
     C                   endsl
    C*
     C                   eval      e     = sce
    C*
     C                   endsr
    C*================================================================
    C* OTHOP5 - Process other options 5
    C*================================================================
     C     othopt5       begsr
    C*
     C                   eval      sc4dsc = *blanks
     C                   movel(p)  sccode        kkcode
    C*
     C     key4          chain     pc401
    C*
     C                   eval      sc4dsc = c4desc
    C*
     C                   dou       *inkc or *inkl or not *in10
     C                   exfmt     scr05
     C                   enddo
    C*
     C                   endsr
    C*================================================================
    C* OPT06 - Claim info
    C*================================================================
     C     opt06         begsr
    C*
     C                   move      ##co#         #1co#
     C                   move      scfnd         #1fnd
     C                   move      scfyr         #1fyr
     C                   move      sccase        #1case
    C*
     C                   call      'WA004R'
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm      *zeros        #2rrn
    C*
     C                   if        #2rrn <> 0
     C                   call      'W0070R'
     C                   parm      ' '           ##call
     C                   parm      '0'           ##kc
     C                   parm      '0'           ##kl
     C                   parm      'C'           ##mode
     C                   parm                    #2rrn
     C                   parm      *blanks       ##sort
     C                   else
     C                   eval      *in86 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
     C                   eval      e = 0
    C*
    C* CALL API FOR SYSTEM FLAGS
    C*
     C                   call      'SA001R'
     C                   parm      ' '           ##wcif
     C                   parm      ' '           ##mcif
     C                   parm      ' '           ##wcin
     C                   parm      ' '           ##ar
     C                   parm      ' '           ##ntwk
     C                   parm      ' '           ##ur
     C                   parm      ' '           ##med
     C                   parm      ' '           ##edi
     C                   parm      ' '           ##wcdk
     C                   parm      ' '           ##rhab
     C                   parm                    ##mss
     C                   parm                    ##fsch
     C                   parm                    ##prvb
     C                   parm                    ##npwc
    C*
    C* Determine if OPT06=Claim Info should be displayed
    C*
     C                   if        ##wcif = 'Y'
     C                             and ##npwc <> 'Y'
     C                   movel     clmopt        ofd(05)
     C                   else
     C                   movel     clropt        ofd(05)
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   eval      e = e + 1
     C                   if        e > 10
     C*                  eval      e = 10
     C                   eval      *in95 = *on
     C                   endif
     C                   eval      sce = e
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   eval      e = e - 1
     C                   if        e < 1
     C*                  eval      e = 1
     C                   eval      *in95 = *on
     C                   endif
     C                   eval      sce = e
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     @@rrn         chain     pfrcdf                             95
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     @@rrn         chain     pfrcdf                             95
     C                   endif
    C*
     C                   if        *in95  = *off
    C*
     C                   eval      scfnd  = %subst(c2clm#:1:3)
     C                   eval      scfyr  = %subst(c2clm#:4:3)
     C                   eval      sccase = %subst(c2clm#:7)
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
    C*
     C                   if        erra(e) = *blanks
     C                   move      *off          meetss
     C                   endif
    C*
     C                   endsr
** Column headings
Opt Err.Code  Description
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Claim info             YYN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
