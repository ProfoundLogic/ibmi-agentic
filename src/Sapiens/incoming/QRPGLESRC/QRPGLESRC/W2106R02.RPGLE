     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Compliance Codes
     H*====================================================================
     H*  Created: 06/24/09   C:33   Project: WC1819
     H*====================================================================
    H*  Revisions:
     H*
     H*  02/16/10 C:198  Project WC1819 Phase 2 changes
     H*                   Corrected header on screen.
     H*
     H*====================================================================
     Fw2106f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtdmp     uf a e             disk    rename(pdm01:dbrcdf)
     Fwtdml     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D*** Beg Chg *** 02/16/10 ****************************************
     D*optnm********   c                   'CMS Error Codes'
     D optnm           c                   'CMS Compliance Codes'
     D*** End Chg *** 02/16/10 ****************************************
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
    C*  Key list for Error Code file
    C*
     C     dmky          klist
     C                   kfld                    ##co#
     C                   kfld                    sccode
    C*
     C*================================================================
     C* CHGSTS - Change status of item
     C*================================================================
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     DMSTAT        IFEQ      'I'
     C                   MOVEL     'A'           DMSTAT
     C                   ELSE
     C                   MOVEL     'I'           DMSTAT
     C                   ENDIF
     C                   EXSR      UPDFLD
E01  C                   ENDIF
     C                   RETURN
     C                   ENDSR
    C*
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     c*
     c* Code field cannot be blank.
     c*
     c  N90              if        sccode = *blanks
     c                   move      *on           *in20
     c                   move      *on           *in90
     c                   endif
     c*
     c* Code cannot already exist.
     c*
     c     dmky          chain     pdm01                              60
     c  N90              if        *in60 = *off and *in40 = *on
     c                   move      *on           *in21
     c                   move      *on           *in90
     c                   endif
     c*
     c* Description cannot be blank.
     c*
     c  N90              if        scdes1 = *blanks
     c                   move      *on           *in22
     c                   move      *on           *in90
     c                   endif
     c*
     C                   endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     c                   move      dmcode        sccode
     c                   movel     dmdesc        scdes1
     c                   move      dmdesc        scdes2
     c   40              move      'A'           dmstat
     c                   move      dmstat        scstat
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c                   move      sccode        dmcode
     c                   movel     scdes1        dmdesc
     c                   move      scdes2        dmdesc
     C                   endsr
    C*
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@MODE        COMP      'C'                                    10
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C                   endsr
    C*
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C     @@MODE        IFEQ      'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDIF
     C                   endsr
    C*
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   EXSR      UPDFLD
E01  C                   endif
    C*
     C                   endsr
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDFLD        BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DMDT
     C                   MOVEL     DSTIME        DMTIME
     C                   MOVEL     Q1PGM         DMPGM
     C                   MOVEL     Q1USER        DMUSER
     C                   UPDATE    DBRCDF
     C                   ENDSR
    C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     c                   z-add     ##co#         dmco#
     c                   move      'A'           dmstat
     C                   exsr      mvscdb
     C                   MOVEL     Q1PGM         DMPGM
     C                   MOVEL     Q1USER        DMIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DMIDT
     C                   MOVEL     SRDTO         DMDT
     C                   Z-ADD     DSTIME        DMTIME
     C                   MOVEL(P)  Q1USER        DMUSER
     C                   write     dbrcdf
     C                   feod      wtdmp
    C*
     C                   endsr
