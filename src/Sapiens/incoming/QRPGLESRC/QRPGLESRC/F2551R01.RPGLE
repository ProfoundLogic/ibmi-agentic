     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
    H**********************************************************************
    H* PROGRAM: F2551R01
    H* DESCRIPTION: NY Comorbidity Factors update/display
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED:  6/18/2013  C:274  PROJECT: NY029
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
     H*  12/10/25 C:342 Support : 120197 SY458                             *
     H*                 DSPF Change:                                       *
     H*                 1. Changed error indicator from 70 to erfact       *
     H*                    for scfact field and handled same in program.   *
    H*                                                                    *
    H*                                                                    *
    H**********************************************************************
     H*
     Ff2551f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftctpny   uf a e             disk
     F                                     rename(mor01:dbrcdf)
     Fftctl1ny  if   e           k disk    rename(mor01:mor02)
      /COPY SCOPYMT01D
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'NY Comorbidity Factors'
     D comapi          c                   'FA007R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D*##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkicd9          s                   like(cticd9)
     D kkefdt          s                   like(ctefdt)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
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
     C     key1          klist
     C                   kfld                    kkicd9
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C/COPY SCOPYMT01C
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
     C* ICD-CM code must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             SCICD9 = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C
     C* Desc must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scdesc = *blanks
     C                   seton                                        729088
     C                   leavesr
     C                   endif
     C
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739087
     C                   leavesr
     C                   else
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C* Check for duplicate record in FTCTPNY (If in ADD mode)
     C                   eval      kkicd9 = SCICD9
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     key1          chain     mor02
     C                   if        %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C* Comorbidity code must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             sccode = *blanks
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C* Comorbidity category must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             sccat  = *blanks
     C                   seton                                        759085
     C                   leavesr
     C                   endif
     C* Comorbidity factor must not be zero  (If in ADD mode)
     C                   if        scfact = *zeros
     C*** Beg Chg *** 12/10/25 ****************************************
     C***                seton                                        709084
     C                   eval      *in90=*on
     C                   eval      *in84=*on
     C                   eval      msfact='MCR0502'
     C                   eval      erfact=*on
     C*** End Chg *** 12/10/25 ****************************************
     C                   leavesr
     C                   endif
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
     C*** Beg Add *** 12/10/25 ****************************************
     c                   eval        msfact= *blanks
     c                   eval        erfact= *off
     C*** End Add *** 12/10/25 ****************************************
     C
     C* Initialize screen fields
     C                   eval      SCICD9 = *blanks
     C                   eval      SCDESC = *blanks
     C                   eval      SCEFDT = *zeros
     C                   eval      SCCODE = *blanks
     C                   eval      SCCAT  = *blanks
     C                   eval      SCFACT = *zeros
     C
     C* Load screen fields
     C                   eval      SCICD9 = CTICD9
     C                   eval      SCDESC = CTDESC
     C                   eval      SCEFDT = CTEFDT
     C                   z-add     CTEFDT        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   eval      SCCODE = CTCODE
     C                   eval      SCCAT  = CTCAT
     C                   eval      SCFACT = CTFACT
     C
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C
     C                   Select
     C                   When      @@mode = 'C'                                 Change
     C                   eval      CTFACT = SCFACT
     C                   When      @@mode = 'A'                                 Add
     C                   eval      CTICD9 = SCICD9
     C                   eval      CTDESC = SCDESC
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         CTEFDT
     C                   eval      CTCODE = SCCODE
     C                   eval      CTCAT  = SCCAT
     C                   eval      CTFACT = SCFACT
     C                   eval      CTSTAT = 'A'
     C                   Endsl
     C
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
     C*** Beg Add *** 12/10/25 ****************************************
     c                   eval        msfact= *blanks
     c                   eval        erfact= *off
     C*** End Add *** 12/10/25 ****************************************
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
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C     @@mode        comp      'C'                                    10
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        CTUSER
     C                   movel     q1pgm         CTPGM
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         CTDT
     c                   movel     dstime        CTTIME
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        CTUSER
     C                   movel     q1pgm         CTPGM
     C                   movel     q1pgm         CTIPGM
     C                   movel     q1user        CTIUSR
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         CTDT
     C                   movel     srdto         CTIDT
     c                   movel     dstime        CTTIME
     c                   movel     dstime        CTITIM
     C                   write     dbrcdf
     C                   feod      ftctpny
     C                   endsr
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
     C
     C     @@rrn         chain     dbrcdf                             99
     C
     C                   if        *in99 = *off
     C
     C                   if        ctstat = 'I'
     C                   eval      ctstat = 'A'
     C                   else
     C                   eval      ctstat = 'I'
     C                   endif
     C
     C                   movel     q1user        ctuser
     C                   movel     q1pgm         ctpgm
     C
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ctdt
     C                   movel     dstime        cttime
     C
     C                   update    dbrcdf
     C
     C                   endif
     C
     C                   endsr
