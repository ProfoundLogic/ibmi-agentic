     H*  (C) Copyright Progressive Data Solutions, Inc., 2002
    H*
     H**********************************************************************
     H**  Work with CodeReview Group/State Information                    **
     H**                                                                  **
     H**********************************************************************
     H**  Created on 05/28/02 by C:062 for Project 114MDC                 **
     H**********************************************************************
     H** REVISIONS:                                                       **
     H*                                                                   **
     H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) actgrp(*caller)
      /ENDIF
     Ff0123f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffdsxp     if   e             disk
     F                                     rename(psx01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffdsxl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(psx01:dbrcdf)
     Ffdgxp     if   e             disk
     Ffmfnl     if   e           k disk
     Ffmsml     if   e           k disk
     Fftgtl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CR Group/State Information'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'F0123R01'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   7
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##call          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##sub#          s              5  0
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@mode          s              1
     D kkco#           s                   like(gxco#)
     D kksub#          s                   like(gxsub#)
     D kkfnd           s                   like(gxfnd)
     D kkcrvw          s                   like(gxcrvw)
     D kkefdt          s                   like(gxefdt)
     D kkst            s                   like(gxst)
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
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
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
     C                   kfld                    kkefdt

     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
     C                   kfld                    kkefdt
     C                   kfld                    kkst

     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd

     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey

     C     smky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#

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

     C     @@rrn2        chain     pgx01                              99
     C                   eval      kkco#=gxco#
     C                   eval      kksub#=gxsub#
     C                   eval      kkfnd=gxfnd
     C                   eval      kkcrvw=gxcrvw
     C                   eval      kkefdt=gxefdt
     C                   eval      kkst=*blanks

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr

     C                   eval      kkst=scpst1

     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr

     C                   eval      kkst=scst

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr


     C                   eval      scst = sxst
     C                   eval      sckbid = sxkbid
     C                   movel     sxstat        scstat

     C                   endsr
    C*================================================================
    C* OPT09 = State Info
    C*================================================================
     C     opt09         begsr
     C                   endsr
    C*================================================================
    C* OPT20 = Change Status
    C*================================================================
     C     opt20         begsr

     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99

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

     C     scsel         caseq     '20'          opt20
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr

     C                   return

     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C                   eval      scsub#=gxsub#
     C                   eval      scfnd=gxfnd
     C                   eval      sccrvw=gxcrvw

     C     smky          chain     psm01                              99
     C                   if        *in99=*off
     C                   eval      scsnam=smname
     C                   else
     C                   eval      scsnam=*blanks
     C                   endif

     C     fnky          chain     pfn01                              99
     C                   if        *in99=*off
     C                   eval      scgnam=fnname
     C                   else
     C                   eval      scgnam=*blanks
     C                   endif

     C                   movel(p)  'CRVW'        kktbid
     C                   movel(p)  gxcrvw        kkkey
     C     gtky          chain     pgt01                              99
     C                   if        *in99=*off
     C                   eval      sccrvd=gtdesc
     C                   else
     C                   eval      sccrvd=*blanks
     C                   endif

     C                   z-add     gxefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt

     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr

     C     key1          reade     dbrcdf                                 95

     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr

     C     key1          readpe    dbrcdf                                 95

     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr

     C     key2          setgt     dbrcdf                             96

     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr

     C     key2          setll     dbrcdf                             96

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

     C                   eval      @@rrn = scdbrr

     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt State  KBID  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
