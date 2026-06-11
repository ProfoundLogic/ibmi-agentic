     H*  (C) Copyright Progressive Data Solutions, Inc., 2002
    H*
     H**********************************************************************
     H**  Work with CodeReview Group Information                          **
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
     Ff0122f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffdgxp     if   e             disk
     F                                     rename(pgx01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffdgxl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pgx01:dbrcdf)
     Ffmfnp     if   e             disk
     Ffmsml     if   e           k disk
     Fftgtl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CodeReview Group Information'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'F0122R01'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   9
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
     D @@rrn3          s              9  0
     D @@mode          s              1
     D kkco#           s                   like(gxco#)
     D kksub#          s                   like(gxsub#)
     D kkfnd           s                   like(gxfnd)
     D kkcrvw          s                   like(gxcrvw)
     D kkefdt          s                   like(gxefdt)
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
     C                   parm                    @@rrn3
     C                   parm                    @@mode
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

     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
     C                   kfld                    kkefdt

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

     C     @@rrn2        chain     pfn01                              99
     C                   eval      kkco#=fnco#
     C                   eval      kksub#=fnsub#
     C                   eval      kkfnd=fnfnd
     C                   eval      kkcrvw=*blanks
     C                   eval      kkefdt=*zeros

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr

     C                   eval      kkcrvw=scpst1
     C                   eval      kkefdt=*zeros

     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr

     C                   eval      kkcrvw=sccrvw
     C                   eval      kkefdt=scefdt

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   eval      sccrvw = gxcrvw

     C                   movel(p)  'CRVW'        kktbid
     C                   movel(p)  sccrvw        kkkey
     C     gtky          chain     pgt01                              99
     C                   if        *in99=*off
     C                   eval      scdesc=gtdesc
     C                   else
     C                   eval      scdesc=*blanks
     C                   endif

     C                   eval      sckbid = gxkbid
     C                   eval      scefdt = gxefdt

     C                   movel     gxstat        scstat

     C                   z-add     gxefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefd2

     C                   endsr
    C*================================================================
    C* OPT09 = State Info
    C*================================================================
     C     opt09         begsr

     C                   call      'F0123R'
     C                   parm      @@call        ##call
     C     *inkc         parm      '0'           ##kc
     C     *inkl         parm      '0'           ##kl
     C                   parm      *zeros        ##rrn
     C                   parm      scdbrr        ##rrn2

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

     C     scsel         caseq     '9 '          opt09
     C     scsel         caseq     '20'          opt20
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr

     C                   if        *inkc=*off and *inkl=*off
     C                             and @@call <> '2'

     C                   call      'F0120R99'                           60
     C                   parm      ' '           ##call
     C     *inkc         parm      '0'           ##kc
     C     *inkl         parm      '0'           ##kl
     C                   parm      @@mode        ##mode
     C                   parm      @@rrn3        ##rrn
     C                   parm      scsub#        ##sub#
     C                   parm      scfnd         ##fnd
     C*
     C                   if        *inkl = *on
     C                   seton                                        90
     C                   setoff                                       kl
     C                   movel     *on           rstpgm
     C                   else
     C                   return
     C                   endif

     C                   else
     C                   return
     C                   endif

     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C                   eval      scsub#=fnsub#
     C                   eval      scfnd=fnfnd

     C     smky          chain     psm01                              99
     C                   if        *in99=*off
     C                   eval      scsnam=smname
     C                   else
     C                   eval      scsnam=*blanks
     C                   endif

     C                   eval      scgnam=fnname

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
Opt Module  Description                                    date      KBID  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1099=State info             YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
