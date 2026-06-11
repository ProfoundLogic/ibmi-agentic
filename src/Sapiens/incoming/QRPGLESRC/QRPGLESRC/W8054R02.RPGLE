     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
     H* DESCRIPTION: W/W Holidy Blackout maintenance program          *
     H*                                                               *
     H* CALLED FROM: W8054R                                           *
     H*                                                               *
     H* CREATED: 05/01/09  PROGRAMMER: C:220 PROJECT: WC1766          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8054f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtcbp     uf a e             disk
     F                                     rename(pcb01:dbrcdf)
     FWTCBL     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Work with Holiday Blackout Dates'
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
     C*
    C*================================================================
    C*  KLIST definitions
    C*
    C     KCB01         KLIST
    C                   KFLD                    ##CO#
    C                   KFLD                    XXDATE            7 0
    C*
    C*
    C*================================================================
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
     C     KCB01         CHAIN     PCB01
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATC            7 0
     C*
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C*  Edit for a valid calendar date greater than or equal to the current date
     C*
     C     SRERR         IFNE      '1'
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATS            7 0
     C*
     C     @@MODE        IFNE      'C'
     C     XXDATS        ANDLT     XXDATC
     C                   SETON                                        3990
     C                   ENDIF                                                   DATC  <> DATS
     C*
     C                   ELSE
     C                   SETON                                        4090
     C                   ENDIF                                                   SRERR <> '1'
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     ##CO#         @@CO#
     C  N90              MOVEL     SCBDAT        @@DTI
     C                   MOVEL     '1'           @@MOD
     C*
     C                   CALL      'WA072R'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DTI             8
     C                   PARM      ' '           @@SRD             5
     C                   PARM                    @@MOD             1
     C                   PARM                    @@DTO             8
     C                   PARM                    @@RTCD            1
     C*
     C     @@RTCD        IFEQ      '0'
     C*
     C*****  DATE IS WEEKEND DATE
     C*
     C                   SETON                                        4190
     C*
     C                   ENDIF                                                   WEEKEND DAY
     C*
     C                   ENDIF                                                    NOT ERROR
      *
      *  No Duplicate Record allowed
      *
     C     @@MODE        IFNE      'C'
     C     *IN90         ANDEQ     *OFF
      *
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
      *
     C     KCB01         CHAIN     PCB01                              60
     C  N60              SETON                                        8490
     C                   ENDIF                                                  *IN90 = *OFF
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
     C* Change the Blackout date to display only if it is less than the current date
     C*
     C                   MOVE      *zeros        SCBDAT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1            7 0
     C*
     C*    CBDATE        IFLT      XXDAT1
     C*
     C                   Z-ADD     CBDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDAT2            8 0
     C*
     C                   MOVEL     XXDAT2        SCBDAT
     C*
     C*                  ENDIF                                                     Holiday date > Dt
     C*
     C                   MOVEL     CBDESC        SCBDSC
     C*
     C                   IF        @@MODE = 'A'
     C                   MOVEL     'A'           SCBSTS
     C                   ELSE
     C                   MOVEL     CBSTAT        SCBSTS
     C                   ENDIF
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   MOVEL     ##co#         CBCO#
     C*
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CBDATE
     C*
     C                   MOVEL     SCBDSC        CBDESC
     C*
     C                   MOVEL     'A'           CBSTAT
     C*
     C                   MOVEL     Q1PGM         CBIPGM
     C                   MOVEL     Q1PGM         CBPGM
     C*
     C                   MOVEL     Q1USER        CBIUSR
     C                   MOVEL     Q1USER        CBUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CBIDAT
     C                   MOVEL     SRDTO         CBDAT
     C*
     C                   MOVEL     DSTIME        CBITM
     C                   MOVEL     DSTIME        CBTM
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
     C                   MOVEL     'A'           SCBSTS
     C                   IF        @@Mode = 'C'
     C                   MOVE      '1'           *IN51
     C                   ENDIF
     C                   MOVEL     'A'           SCBSTS
     C                   TIME                    DSDTTM
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   SELECT
     C*
     C                   WHEN      @@Mode = 'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
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
     C                   if        *in90 = *off
     C                   write     dbrcdf
     C                   endif
     C                   feod      wtcbp
     C                   endsr
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                               60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     CBSTAT        IFEQ      'I'
     C                   MOVE      'A'           CBSTAT
     C                   ELSE
     C                   MOVE      'I'           CBSTAT
     C                   ENDIF                                                    DPSTAT=I
     C*
     C                   MOVEL     Q1USER        CBUSR
     C                   MOVEL     Q1PGM         CBPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CBDAT
     C*
     C                   MOVEL     DSTIME        CBTM
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                    *IN60=*OFF
     C*
     C                   ENDSR                                                    CHGSTS
     C*
     C*===============================================================
