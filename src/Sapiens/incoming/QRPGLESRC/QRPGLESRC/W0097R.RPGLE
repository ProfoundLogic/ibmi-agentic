     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title W0097R - Work with SSN/FEIN Default Table
     h option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h debug(*yes)
      *****************************************************************
      * REVISED:
      *
      * 04/24/24 C:300 Project: WC2941
      *                This program is no longer in use.
      *                Pass-through this program to new screen.
      *                Add extra entry parm to avoid Pass-through of old screen
      *
      * 08/02/11 C:061 Project: WC2003
      *                Created.
      *
      *****************************************************************

     Fw0097f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtsfp     if   e             disk
     F                                     rename(psf01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtsfl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(psf01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'SSN/FEIN Default Criteria'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0097R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
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
     D @@call          s              1
     D kkco#           s                   like(sfco#)
     D kkrpid          s                   like(sfrpid)
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
      **** Begin Add **** 04/24/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************
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
     C     key           klist
     C                   kfld                    kkco#

     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrpid


    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================


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
     c                   eval      kkco#  = ##co#
     c                   eval      kkrpid = *loval
     C                   endsr


    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     c   21              eval      kkrpid = scpst1
     C                   endsr


    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     c                   eval      kkco#  = ##co#
     c                   eval      kkrpid = scrpid
     C                   endsr


    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    
     c                   eval      scco#  = sfco#
     c   40              eval      scco#  = ##co#
     c                   eval      scrpid = sfrpid
     c                   eval      scdesc = sfdesc
     c                   movel(p)  sfdesc        xxdesc           30
     c                   eval      scflag = sfflag
     c                   eval      sclvl  = sflvl
     c                   eval      scty01 = sfty01
     c                   eval      scty02 = sfty02
     c                   eval      scty03 = sfty03
     c                   eval      scty04 = sfty04
     c                   eval      scty05 = sfty05
     c                   eval      scty06 = sfty06
    
     c                   select
    
     *   view 1
    
     c                   when      *in21 = *on
     c                   eval      scdata = scrpid
     c                              + ' ' + xxdesc
     c                              + '    ' + scflag
     c                              + '       ' + scty01
     c                              + ' ' + scty02
     c                              + ' ' + scty03
     c                              + ' ' + scty04
     c                              + ' ' + scty05
     c                              + ' ' + scty06
     c                              + '    ' + %editc(sclvl:'4')
    
     c                   endsl
    
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
     C                   endsr


    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr


    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if 2nd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 2
     c                   Call      'WTSSNDFT'
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************
     C                   endsr


    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C   21key           reade     dbrcdf                                 95
     C                   endsr


    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C   21key           readpe    dbrcdf                                 95
     C                   endsr


    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr

     c                   select
     c                   when      *in21 = *on
     c     key1          setgt     dbrcdf                             96
     c                   endsl

     C                   endsr


    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr

     c                   select
     C                   when      *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endsl

     C                   endsr


    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '     *** Deleted ***'
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
Opt  Report ID  Description                    SSN/FEIN      Types      Level
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NYY
217F17=Subset               NNN
3
