     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
      * W0008R - Default WCPOLS/WCSTAT Transmittal Information
      *****************************************************************
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
      *****************************************************************
      * Revised:
      *
      * 01/13/14  C:190 Project: 136WFZ
      *                 Add check box field for "Allow Personal Name Designation".
      *
      * 06/11/07  C:61  Project: WC1504
      *                 Created.
      *
      *****************************************************************


      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0008f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

    F*  Physical file definition (for maintaining record)
     Fwmtmp     uf a e             disk
     F                                     rename(ptm01:dbrcdf)
     fwmtml     if   e           k disk    infds(tmlfds)

      * system company
     fsmcop     if   e             disk


     d                 ds
     d scphn#                        10  0
     d  scphn1                        3  0 overlay(scphn#:1)
     d  scphn2                        3  0 overlay(scphn#:4)
     d  scphn3                        4  0 overlay(scphn#:7)

     d                 ds
     d scfax#                        10  0
     d  scfax1                        3  0 overlay(scfax#:1)
     d  scfax2                        3  0 overlay(scfax#:4)
     d  scfax3                        4  0 overlay(scfax#:7)

     d                 ds
     d sczip                          9  0
     d  sczip5                        5  0 overlay(sczip:1)
     d  sczip4                        4  0 overlay(sczip:6)

    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnmS          c                   'WCSTAT Transmittal Default'
     D optnmP          c                   'WCPOLS Transmittal Default'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     d tmlfds          ds
     d  tmrrn                397    400B 0
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
     D optnm           s             40
    
    
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    
     C/COPY SCOPYMT01C
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
     C                   parm                    @@call            1
     *
     *  @@RRN  (SMCOP passed in)
     *  @@Call ('P'=WCPOLS; 'S'=WCSTAT)
     *

    C*  Company file/fund (group) retrieve program parameter list

     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam


    C*================================================================
    C*  KLIST definitions
    C*================================================================

     c     tmky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkflag            1


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

     c                   eval      scmail  = tmmail
     c                   eval      scname  = tmname
     c                   eval      scphn#  = tmphn#
     c                   eval      scext#  = tmext#
     c                   eval      scfax#  = tmfax#
     c                   eval      scadr1  = tmadr1
     c                   eval      sccty   = tmcty
     c                   eval      scst    = tmst
     c                   eval      sczip   = tmzip
      //********* Beg Add ******** 01/13/14 ****************
      /Free
       scPerNam = TMapnd;
      /End-free
      //********* End Add ******** 01/13/14 ****************

     C                   endsr


    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     c                   eval      tmco#   = scco#
     c                   eval      tmmail  = scmail
     c                   eval      tmname  = scname
     c                   eval      tmphn#  = scphn#
     c                   eval      tmext#  = scext#
     c                   eval      tmfax#  = scfax#
     c                   eval      tmadr1  = scadr1
     c                   eval      tmcty   = sccty
     c                   eval      tmst    = scst
     c                   eval      tmzip   = sczip
     c                   eval      tmflag  = @@call
      //********* Beg Add ******** 01/13/14 ****************
      /Free
       TMapnd = scPerNam;
      /End-free
      //********* End Add ******** 01/13/14 ****************

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
     C                   endsr


    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

      * get program title

     c                   if        @@call = 'P'
     c                   movel(p)  optnmP        optnm
     c                   else
     c                   movel(p)  optnmS        optnm
     c                   endif

     c                   exsr      getpgt

      * get company name

     c     @@rrn         chain     pco01

     c                   if        %found
     c                   eval      scco# = coco#
     c                   eval      sccomn = %trim(coname)
     C                   movel(p)  coname        ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
     C                   movel     ##conm        scconm
     c                   eval      kkco#  = scco#
     c                   eval      kkflag = @@call

     c     tmky          chain     ptm01
     c
     c                   if        %found
     c                   eval      @@mode = 'C'
     c                   eval      @@rrn = tmrrn
     c                   else
     c                   eval      @@mode = 'A'
     c                   endif
     c
     c                   else
     c                   seton                                        lr
     c                   endif

     c                   if        *in09 = *on
     c                   eval      @@mode = 'D'
     c                   endif
      ********** Beg Add ************* 01/13/14 *********************
      /Free
       visOnFlgP = *off;
       If @@Call = 'P';
          visOnFlgP = *on;
       Endif;
      /End-free
      ********** End Add ************* 01/13/14 *********************

     c                   endsr


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

     C                   eval      tmusr = q1user
     C                   eval      tmpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      tmtim = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         tmdat

     C                   update    dbrcdf
E01  C                   endif

     c                   eval      outflg = *on
     C                   endsr


    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb

     C                   eval      tmusr  = q1user
     C                   eval      tmiusr = q1user
     C                   eval      tmpgm  = q1pgm
     C                   eval      tmipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      tmtim  = dstime
     C                   eval      tmitim = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         tmdat
     C                   movel(p)  srdto         tmidat

     C                   write     dbrcdf
     C                   feod      wmtmp
     c                   eval      outflg = *on
     C                   endsr


