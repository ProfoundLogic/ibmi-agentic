     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H** ======================================================================================== **
    H/TITLE W0009R02 - W/W TPA Maint
     H*  Created: 06/25/09 by C:220 for project: 114WTR                                            *
    H* ========================================================================================= **
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0009f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmtcp     uf a e             disk
     F                                     rename(ptc01:dbrcdf)
     Fwmtcl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'TPA Contact Information'
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
     C                   parm                    @#co#             3 0
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
    C     KTC01         KLIST
    C                   KFLD                    ##co#
    C                   KFLD                    XXtpac           10
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
     C     ktc01         chain     ptc01
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
      *  TPA code and Description should not be blank (All required fields)
      *
     C                   if        sctpac = *blanks
     C*
     C                   seton                                        3690
     C*
     C                   endif
     C*
     C                   if        scdesc = *blanks and *in90 = *off
     C*
     C                   seton                                        3790
     C*
     C                   endif
     C*
     C                   if        scip = *blanks   and *in90 = *off
     C*
     C                   seton                                        3890
     C*
     C                   endif
     C*
     C                   if        scfldr = *blanks and *in90 = *off
     C*
     C                   seton                                        3990
     C*
     C                   endif
      *
     C                   if        scusid = *blanks and *in90 = *off
     C*
     C                   seton                                        4190
     C*
     C                   endif
      *
     C                   if        scpswd = *blanks and *in90 = *off
     C*
     C                   seton                                        4290
     C*
     C                   endif
      *
      *  No Duplicate Record allowed
      *
    C                   move      @#co#         ##co#
    C                   move      sctpac        XXtpac
      *
     C     ktc01         chain     ptc01                              60
     C     *in60         ifeq      *off
     C     @@mode        andne     'C'
     C                   seton                                        8490
     C                   endif                                                  *IN60 = *OFF
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
     C                   movel     tctpac        sctpac
     C                   movel     tcdesc        scdesc
     C                   movel     tcip          scip
     C                   movel     tcfldr        scfldr
     C                   movel     tcusid        scusid
     C                   movel     tcspsw        scpswd
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     sctpac        tctpac
     C                   movel     scdesc        tcdesc
     C                   movel     scip          tcip
     C                   movel     scfldr        tcfldr
     C                   movel     scusid        tcusid
     C                   movel     scpswd        tcspsw
     C                   movel     ##co#         tcco#
     C                   movel     q1user        tcuser
     C                   movel     q1user        tciusr
     C                   movel     q1pgm         tcpgm
     C                   movel     q1pgm         tcipgm
     C                   movel     'A'           tcstat
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tcdt
     C                   movel     srdto         tcidt
     C*
     C                   movel     dstime        tctime
     C                   movel     dstime        tcitim
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
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   TIME                    DSDTTM
     C                   if        @@mode = 'D'
     C                   move      @#co#         @@co#             3 0
     C                   move      *on           *in09
     C                   endif
     C                   if        @@mode = 'A'
     C                   move      *on           *in40
     C                   endif
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   select
     C*
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C*
     C                   endsl
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmtcp
     C                   endsr
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     DBRCDF                               60
     C*
     C     *in60         IFEQ      *OFF
     C*
     C     tcstat        ifeq      'I'
     C                   move      'A'           tcstat
     C                   else
     C                   move      'I'           tcstat
     C                   endif                                                    DPSTAT=I
     C*
     C                   movel     q1user        tcuser
     C                   movel     q1pgm         tcpgm
     C*
     C                   TIME                    DSDTTM
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tcdt
     C*
     C                   movel     dstime        tctime
     C*
     C                   update    DBRCDF
     C*
     C                   endif                                                    *IN60=*OFF
     C*
     C                   endsr                                                    CHGSTS
