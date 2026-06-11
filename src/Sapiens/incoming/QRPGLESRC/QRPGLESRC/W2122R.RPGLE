      * (C) Copyright Progressive Data Solutions, Inc., 2004

      /title W2122R - Work with Contractor Credit Factor Table
      *****************************************************************
      * Revised:
      *
      *
      * Created 01/27/04  C:61  Project WC1173
      *****************************************************************

     *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2122f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
     *
     *  Physical file definition (for refreshing subfile record)
     Fwtcdp     if   e             disk
     F                                     rename(pcd01:pfrcdf)
     *
     *  Primary logical file definition (for displaying items)
     Fwtcdl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pcd01:dbrcdf)
     *
     *
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    
    
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'Contractor Credit Factor'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2122R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    
    
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(cdco#)
     D kkst            s                   like(cdst)
     D kkawul          s                   like(cdawul)
    
    
    
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     C/COPY SCOPYWW01C
    
    
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm                    @@call
     *
     *  Add/change/display program parameter list
     *
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    
    
     *================================================================
     *  KLIST definitions
     *================================================================
     C     key           klist
     C                   kfld                    kkco#
    
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkst
     C                   kfld                    kkawul
    
    
     *================================================================
     *  *LIKE DEFN statements
     *================================================================
    
    
     *================================================================
     * CHKSSM - Set indicator 11 for subset message
     *================================================================
     C     chkssm        begsr
     C                   endsr
    
    
     *================================================================
     * ERRCK1 - Additional errors on subfile selection
     *================================================================
     C     errck1        begsr
    
    
     C                   endsr
    
    
     *================================================================
     * ERRCK2 - Error checking for the subset screen
     *================================================================
     C     errck2        begsr
     C                   endsr
    
    
     *================================================================
     * INZSST - Initialize the subset fields
     *================================================================
     C     inzsst        begsr
     C                   endsr
    
    
     *================================================================
     * KEYINZ - Initialize key values for program start
     *================================================================
     C     keyinz        begsr
     c                   eval      kkco#  = ##co#
     c                   eval      kkst   = *loval
     c                   eval      kkawul = *loval
     C                   endsr
    
    
     *================================================================
     * KEYSET - Set key values when doing "position to"
     *================================================================
     C     keyset        begsr
     c   21              eval      kkst   = scpst1
     c                   eval      kkawul = *loval
     C                   endsr
    
    
     *================================================================
     * KEYSFL - Set key values from subfile records for paging
     *================================================================
     C     keysfl        begsr
     c                   eval      kkco#  = ##co#
     c                   eval      kkst   = scst
     c                   eval      kkawul = scawul
     C                   endsr
    
    
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr
    
     c                   eval      scco#  = cdco#
     c   40              eval      scco#  = ##co#
     c                   eval      scst   = cdst
     c                   eval      scawul = cdawul
     c                   eval      scfact = cdfact
    
     c                   select
    
     *   view 1
     *  zzz
     c                   when      *in21 = *on
     c                   eval      scdata = ' ' + scst
     c                              + '      ' + %editc(scawul:'3')
     c                              + '  ' + %editc(scfact:'3')
    
     c                   endsl
    
     C                   endsr
    
    
     *================================================================
     * OTHFKS - Process other function keys
     *================================================================
     C     othfks        begsr
     C                   endsr
    
    
     *================================================================
     * OTHOPT - Process other options
     *================================================================
     C     othopt        begsr
     C                   endsr
    
    
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr
    
    
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
     C                   endsr
    
    
     *================================================================
     * READ   - Read next record from file
     *================================================================
     C     read          begsr
     C   21key           reade     dbrcdf                                 95
     C                   endsr
    
    
     *================================================================
     * READP  - Read previous record from file
     *================================================================
     C     readp         begsr
     C   21key           readpe    dbrcdf                                 95
     C                   endsr
    
    
     *================================================================
     * SETGT  - Set greater than on file
     *================================================================
     C     setgt         begsr

     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif

     C                   endsr
    
    
     *================================================================
     * SETLL  - Set lower limits on file
     *================================================================
     C     setll         begsr

     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif

     C                   endsr
    
    
     *================================================================
     * SF1UPD - Update subfile record when item is deleted
     *================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '     *** Deleted ***'
     C                   endsr
    
    
     *================================================================
     * SLCTRD - Move fields for selected record to parameters
     *================================================================
     C     slctrd        begsr
     C                   endsr
    
    
     *================================================================
     * SUBSET - Determine if record should be put in subfile
     *================================================================
     C     subset        begsr
     C                   endsr
    
    
** Column headings
Opt  State  Ave Wage  Factor
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NYY
217F17=Subset               NNN
3
