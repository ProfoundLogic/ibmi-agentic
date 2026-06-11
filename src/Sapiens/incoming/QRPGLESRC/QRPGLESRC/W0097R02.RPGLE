     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title W0097R02 - SSN/FEIN Default Table Information
     h option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h debug(*yes)
      *****************************************************************
      * REVISED:
      *
      * 08/02/11 C:061 Project: WC2003
      *                Created.
      *
      *****************************************************************

     Fw0097f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtsfp     uf a e             disk
     F                                     rename(psf01:dbrcdf)

     *  SSN/FEIN Default table
     Fwtsfl     if   e           k disk
     F                                     rename(psf01:psf02)


    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'SSN/FEIN Default Criteria'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D scco#           s                   like(sfco#)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    
     D/copy sPrDate
     D/copy sPrString
     C/COPY SCOPYMT41C
    
    
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
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


     c     sfky          klist
     c                   kfld                    scco#
     c                   kfld                    scrpid

    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================


    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr


    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr


    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr

      * Report ID must be unique

     c                   if        (*in90 = *off) and (*in40 = *on)
     c     sfky          chain     psf02
     c                   if        %found
     c                   seton                                        90  71
     c                   leavesr
     c                   endif
     c                   endif

      * Report ID must be entered

     c                   if        (*in90 = *off) and (*in40 = *on)
     c                             and (SCrpid = *blanks)
     c                   seton                                        90  72
     c                   leavesr
     c                   endif

      * Report Description must be entered

     c                   if        (*in90 = *off) and (*in40 = *on)
     c                             and (SCdesc = *blanks)
     c                   seton                                        90  73
     c                   leavesr
     c                   endif

      * SSN/FEIN default must be entered

     c                   if        (*in90 = *off)
     c                             and (SCflag = *blanks)
     c                   seton                                        90  74
     c                   leavesr
     c                   endif

      * Type or Level must be entered

     c                   if        (*in90 = *off)
     c                             and (SCty01 = *blanks)
     c                             and (SCty02 = *blanks)
     c                             and (SCty03 = *blanks)
     c                             and (SCty04 = *blanks)
     c                             and (SCty05 = *blanks)
     c                             and (SCty06 = *blanks)
     c                             and (SClvl = *zero)
     c                   seton                                        90  75
     c                   leavesr
     c                   endif


     C                   endsr


    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr


    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr


    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     c                   eval      scco#  = sfco#
     c   40              eval      scco#  = ##co#
     c                   eval      scrpid = sfrpid
     c                   eval      scdesc = sfdesc
     c                   eval      scflag = sfflag
     c                   eval      sclvl  = sflvl
     c                   eval      scty01 = sfty01
     c                   eval      scty02 = sfty02
     c                   eval      scty03 = sfty03
     c                   eval      scty04 = sfty04
     c                   eval      scty05 = sfty05
     c                   eval      scty06 = sfty06

     C                   endsr


    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     c                   eval      sfco#  = scco#
     c   40              eval      sfco#  = ##co#
     c                   eval      sfrpid = scrpid
     c                   eval      sfdesc = scdesc
     c                   eval      sfflag = scflag
     c                   eval      sflvl  = sclvl
     c                   eval      sfty01 = scty01
     c                   eval      sfty02 = scty02
     c                   eval      sfty03 = scty03
     c                   eval      sfty04 = scty04
     c                   eval      sfty05 = scty05
     c                   eval      sfty06 = scty06

     C                   endsr


    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr


    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
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
     c     @@mode        comp      'A'                                    40
     c     @@mode        comp      'D'                                    09
     C                   endsr


    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr


    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      sfuser = q1user
     C                   eval      sfpgm  = q1pgm
     c                   eval      sfdt   = DateTo7(SystemDate)
     c                   eval      sftime = TimeTo6(SystemTime)                 Maintaince time
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr


    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      sfuser = q1user
     C                   eval      sfpgm  = q1pgm
     c                   eval      sfdt   = DateTo7(SystemDate)
     c                   eval      sftime = TimeTo6(SystemTime)                 Maintaince time
     C                   eval      sfiusr = q1user
     C                   eval      sfipgm = q1pgm
     c                   eval      sfidt  = DateTo7(SystemDate)
     c                   eval      sfitim = TimeTo6(SystemTime)                 Maintaince time
     C                   write     dbrcdf
     C                   feod      wtsfp
     c                   eval      outflg = *on
     C                   endsr


