     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE F2554R01
     H**************************************************************************************
     H*  Description:  ICD10 Codes Update/Display                                          *
     H*                                                                                    *
     H*  Created: 06/14/13   Programmer C:277   PROJECT: ME005                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
    H*
     Ff2554f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmicd10p  uf a e             disk
     F                                     rename(pic01:dbrcdf)
    F*  Physical file definition (for maintaining record)
     Ffmicd10l1 if   e           k disk    rename(pic01:pic02)
    F*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'ICD10 Codes'
     D comapi          c                   'FA007R'
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
     D kkcode          s                   like(ICcode)
     D kkind           s                   like(ICind)
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
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
    C*
     C     fmicd10l1key  klist
     C                   kfld                    kkcode
     C                   kfld                    kkind
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
     C                   if        *in99 = *off
    C*
     C                   if        ICstat = 'I'
     C                   eval      ICstat = 'A'
     C                   else
     C                   eval      ICstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        ICuser
     C                   movel     q1pgm         ICpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ICdt
     C                   movel     dstime        ICtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
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
     C*
     C* If add mode
     C*
     C                   If        *in40 = *on
     C*
     C* ICD10 code not be blank.
     C*
     C                   if        scCOde = *zeros
     C                   seton                                        417190
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Diag type (D/0) not be blank.
     C*
     C                   if        scINd = *blanks
     C                   seton                                        427290
     C                   leavesr
     C                   else
     C*
     C                   if        scINd = 'D' and scINd ='0'
     C                   seton                                        427790
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* ICD10 desc not be blank.
     C*
     C                   if        scDSc1 = *blanks
     C                   seton                                        437390
     C                   leavesr
     C                   endif
     C*
     C* Full Code not be blank.
     C*
     C                   if        scFCod = *blanks
     C                   seton                                        447490
     C                   leavesr
     C                   endif
     C*
     C* Specific not be blank.
     C*
     C                   if        scVLd = *blanks
     C                   seton                                        457590
     C                   leavesr
     C                   else
     C*
     C                   if        scVLd = '1' and scVLd ='0'
     C                   seton                                        457890
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Check for duplicate record in FMICD10P
     C*
     C                   if        *in40 = *on
     C                   eval      kkcode = scCOde
     C                   eval      kkind  = scINd
     C     fmicd10l1key  chain     pic02
     C                   if        %found(fmicd10l1)
     C                   seton                                        417690
     C                   leavesr
     C                   endif
     C                   endif
     C*
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
     C*
     C                   if        *in40 = *on
     C*
     C                   move      *blanks       scCOde
     C                   move      *blanks       scINd
     C                   move      *blanks       scDSc1
     C                   move      *blanks       scFCod
     C                   move      *blanks       scLOs
     C                   move      *blanks       scVLd
     C                   move      'A'           scSTat
     C*
     C                   else
     C*
     C                   eval      scCOde = ICcode
     C                   movel     ICind         scINd
     C                   movel     ICdsc1        scdsc1
     C                   eval      scFCod = ICfcode
     C                   eval      scLOs  = IClos
     C                   eval      scVLd  = ICvld
     C                   eval      scSTat = ICstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      ICcode = scCOde
     C                   eval      ICDsc1 = scDSc1
     C                   eval      IClos  = scLOs
     C                   eval      ICind  = scINd
     C                   eval      ICfcode = scFCod
     C                   eval      ICvld  = scVLd
     C                   eval      ICstat = scSTat
     C*
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
    C* ERRWDW1   - Error check on window 1
    C*================================================================
     C     errwdw1       begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   return
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     @@mode        comp      'D'                                    09    Inquiry
     C     @@mode        comp      'A'                                    40    Add
     C     @@mode        comp      'C'                                    10    change
     C                   setoff                                           31
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   eval      *inlr = *on
     C                   return
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        ICuser
     C                   movel     q1pgm         ICpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ICdt
     c                   movel     dstime        ICtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        ICuser
     C                   movel     q1pgm         ICpgm
     C                   movel     q1pgm         ICipgm
     C                   movel     q1user        ICiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ICdt
     C                   movel     srdto         ICidt
     c                   movel     dstime        ICtime
     c                   movel     dstime        ICitim
     C                   write     dbrcdf
     C                   feod      fmicd10p
     C                   endsr
