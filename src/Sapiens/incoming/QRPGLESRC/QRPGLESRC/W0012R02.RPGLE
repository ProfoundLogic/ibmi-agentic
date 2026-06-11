     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H** ======================================================================================== **
    H/TITLE S0012R02 - Company contact information prompt.
     H*  CREATED: 06/25/09       PROJECT: 114WTR               C:220
    H* ========================================================================================= **
     *******************************************************************
      * Revised
      *
      * 08/17/10 C:61 Project SY190 priss 66
      *               Update screen to standard.  Recompile.
      *
     *******************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0012f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmfcp     uf a e             disk
     F                                     rename(pfc01:dbrcdf)
     Fwmfnl     if   e           k disk
     Fwmfcl     if   e           k disk    infds(dbfds)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Group Contact Information'
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
     D dbfds           ds
     D  dbrrn                397    400b 0
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
     C                   parm                    @@call            1
     C                   parm                    @@rrn
     C                   parm                    @@co#             3 0
     C                   parm                    @@fnd             3 0
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
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@fnd
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
     C*
     C                   if        sccon1 = *blanks and *in90 = *off
     C                   seton                                        3690
     C                   endif
     C*
     C                   if        scttl1 = *blanks and *in90 = *off
     C                   seton                                        3790
     C                   endif
     C*
     C                   if        scara1 = *zeros and *in90 = *off
     C                   seton                                        3890
     C                   endif
     C*
     C                   if        scphn1 = *zeros and *in90 = *off
     C                   seton                                        3990
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
     C*                  movel     @@co#         scco#
     C                   movel     @@fnd         scfnd
     C     fnky          chain     pfc01                              60
     C                   movel     fnname        scname
     C                   move      fccon1        SCCON1
     C                   move      fcttl1        SCTTL1
     C                   z-add     fcara1        SCARA1
     C                   z-add     fcphn1        SCPHN1
     C                   move      fccon2        SCCON2
     C                   move      fcttl2        SCTTL2
     C                   z-add     fcara2        SCARA2
     C                   z-add     fcphn2        SCPHN2
     C                   move      fccon3        SCCON3
     C                   move      fcttl3        SCTTL3
     C                   z-add     fcara3        SCARA3
     C                   z-add     fcphn3        SCPHN3
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   move      @@CO#         FCCO#
     C                   move      @@FND         FCFND
     C                   move      SCCON1        FCCON1
     C                   move      SCTTL1        FCTTL1
     C                   move      SCARA1        FCARA1
     C                   move      SCPHN1        FCPHN1
     C                   move      SCCON2        FCCON2
     C                   move      SCTTL2        FCTTL2
     C                   move      SCARA2        FCARA2
     C                   move      SCPHN2        FCPHN2
     C                   move      SCCON3        FCCON3
     C                   move      SCTTL3        FCTTL3
     C                   move      SCARA3        FCARA3
     C                   move      SCPHN3        FCPHN3
     C                   move      'A'           FCSTAT
     C                   movel     Q1USER        FCUSR
     C                   movel     Q1USER        FCIUSR
     C                   z-add     DSDATE        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         FCDT
     C                   movel     SRDTO         FCIDT
     C                   movel     DSTIME        FCTIM
     C                   movel     DSTIME        FCITIM
     C                   movel     Q1PGM         FCPGM
     C                   movel     Q1PGM         FCIPGM
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
     C*
     C     fnky          chain     pfn01                              60
     C  N60              movel     fnname        scname
     C*
     C     fnky          chain     pfc01                              60
     C                   if        *in60 = *off
     C                   eval      @@rrn=dbrrn
     C                   move      'C'           @@mode
     C                   else
     C                   move      'A'           @@mode
     C                   endif
     C                   movel     @@fnd         scfnd
     C*                  move      *blanks       scname
     C*
     C     @@call        ifeq      '1'
     C                   move      '1'           *in09                          INQUIRY
     C                   move      'D'           @@mode                         Display
     C                   endif
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
     C                   feod      wmfcp
     C                   move      *on           outflg
     C                   endsr
