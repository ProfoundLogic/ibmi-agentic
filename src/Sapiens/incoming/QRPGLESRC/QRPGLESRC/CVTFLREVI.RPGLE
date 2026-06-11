     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE CVTFLREVI                                                                      *
     H**************************************************************************************
     H*  Description:  Populates MEIG Revision indicator                                   *
     H*                                                                                    *
     H*  Created: 03/11/10   Programmer C:227   Project:FL083                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFAEOP     UF A E             DISK

     D*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     DPSDS           ESDS                  Extname(SZQ1P)
    C*
     C/COPY SCOPYDTPL
    C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   time                    dsdttm
     C*
     C                   read      peo01                                  99
     C                   dow       *in99 = *off
     C*
     C                   if        eoast = 'FL' and (eoblst <> 'X'
     C                             and eoblst <> 'D')
     C                   select
     C                   when      eotape = 'Y'
     C                   eval      eorevi = 'C'
     C                   when      eotape = ' ' or eotape = 'N'
     C                   eval      eorevi = 'N'
     C                   other
     C                   eval      eorevi = 'D'
     C                   endsl
     C*
     C                   exsr      updatr
     C*
     C                   endif
     C*
     C                   read      peo01                                  99
     C                   enddo                                                  *in60=*off
     C*
     C                   seton                                        lr
     C**********************************************************************
     C* UPDATR - UPDATE FAEOP
     C**********************************************************************
     C     updatr        begsr
     C*
     C                   eval      eopgm=q1pgm
     C                   eval      eouser=q1user
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eodt
     C                   move      dstime        eotime
     C*
     C                   update    peo01
     C*
     C     upde          endsr
