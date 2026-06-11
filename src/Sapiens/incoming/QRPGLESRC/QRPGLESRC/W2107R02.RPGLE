     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Disposition Codes
     H*
     H*  DESCRIPTION: This program shows the W/W  Screen for WTDSP
     H*
     H*  Called from: W2107R
     H*  Calls to   : None
     H*
     H*====================================================================
     H*  Created: 01/28/2010   C:198  Project: WC1819 Phase 2
     H*====================================================================
    H*  Revisions:
     H*
     H*
     H*====================================================================
     Fw2107f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtdsp     uf a e             disk    rename(pds01:dbrcdf)
     Fwtdsl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMS Disposition Codes'
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
     C     dsky          klist
     C                   kfld                    ##co#
     C                   kfld                    sccode
    C*
     C*================================================================
     C* CHGSTS - Change status of item
     C*================================================================
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     DSSTAT        IFEQ      'I'
     C                   MOVEL     'A'           DSSTAT
     C                   ELSE
     C                   MOVEL     'I'           DSSTAT
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
     c     dsky          chain     pds01                              60
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
     c                   move      dscode        sccode
     c                   movel     dsdesc        scdes1
     c                   move      dsdesc        scdes2
     c   40              move      'A'           dsstat
     c                   move      dsstat        scstat
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c                   move      sccode        dscode
     c                   movel     scdes1        dsdesc
     c                   move      scdes2        dsdesc
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
     C                   MOVEL     SRDTO         DSDT
     C                   MOVEL     DSTIME        DSTIME
     C                   MOVEL     Q1PGM         DSPGM
     C                   MOVEL     Q1USER        DSUSER
     C                   UPDATE    DBRCDF
     C                   ENDSR
    C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     c                   z-add     ##co#         dsco#
     c                   move      'A'           dsstat
     C                   exsr      mvscdb
     C                   MOVEL     Q1PGM         DSPGM
     C                   MOVEL     Q1USER        DSIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSIDT
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   MOVEL(P)  Q1USER        DSUSER
     C                   write     dbrcdf
     C                   feod      wtdsp
    C*
     C                   endsr
