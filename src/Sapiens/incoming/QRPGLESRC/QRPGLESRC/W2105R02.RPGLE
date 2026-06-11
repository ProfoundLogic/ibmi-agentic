     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Error Codes
     H*====================================================================
     H*  Created: 06/24/09   C:33   Project: WC1819
     H*====================================================================
    H*  Revisions:
     H*
     H* 11/25/09  C:198  Project: 132WNC
     H*                  Added a new feild on the screen.
     H*
     H*
     H*====================================================================
     Fw2105f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtdep     uf a e             disk    rename(pde01:dbrcdf)
     Fwtdel     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMS Error Codes'
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
     C     deky          klist
     C                   kfld                    ##co#
     C                   kfld                    sccode
    C*
     C*================================================================
     C* CHGSTS - Change status of item
     C*================================================================
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     DESTAT        IFEQ      'I'
     C                   MOVEL     'A'           DESTAT
     C                   ELSE
     C                   MOVEL     'I'           DESTAT
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
     c     deky          chain     pde01                              60
     c  N90              if        *in60 = *off and *in40 = *on
     c                   move      *on           *in21
     c                   move      *on           *in90
     c                   endif
     c*
     c*** Beg Add *** 11/25/09 ****************************************
     c* Field Name cannot be blank.
     c*
     c  N90              if        scfld  = *blanks
     c                   move      *on           *in23
     c                   move      *on           *in90
     c                   endif
     c*
     c*** End Add *** 11/25/09 ****************************************
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
     c                   move      decode        sccode
     c*** Beg Add *** 11/25/09 ****************************************
     c                   movel     defld         scfld
     c*** End Add *** 11/25/09 ****************************************
     c                   movel     dedesc        scdes1
     c                   move      dedesc        scdes2
     c   40              move      'A'           destat
     c                   move      destat        scstat
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c                   move      sccode        decode
     c*** Beg Add *** 11/25/09 ****************************************
     c                   movel     scfld         defld
     c*** End Add *** 11/25/09 ****************************************
     c                   movel     scdes1        dedesc
     c                   move      scdes2        dedesc
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
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     DSTIME        DETIME
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1USER        DEUSER
     C                   UPDATE    DBRCDF
     C                   ENDSR
    C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     c                   z-add     ##co#         deco#
     c                   move      'A'           destat
     C                   exsr      mvscdb
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1USER        DEIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEIDT
     C                   MOVEL     SRDTO         DEDT
     C                   Z-ADD     DSTIME        DETIME
     C                   MOVEL(P)  Q1USER        DEUSER
     C                   write     dbrcdf
     C                   feod      wtdep
    C*
     C                   endsr
