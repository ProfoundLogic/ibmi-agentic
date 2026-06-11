     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE CVTMEIGEOB                                                                     *
     H**************************************************************************************
     H*  Description:  Update the EOB code table on clients machines                       *
     H*                                                                                    *
     H*  Created: 03/11/10   Programmer C:227   Project:FL083                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FMEIGEOBS  IF   E             DISK    RENAME(PEB01:PEB02) PREFIX(E1:2)
     FFTEBLFL   UF A E           K DISK

     D*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     D kkco#           s                   like(ebco#)
     D kkkey           s                   like(ebkey)
    C*
     C/COPY SCOPYDTPL
    C*
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   time                    dsdttm
     C*
     C                   read      peb02                                  99
     C                   dow       *in99 = *off
     C*
     C                   eval      kkco# = e1co#
     C                   movel(p)  e1key         kkkey
     C     ebky          chain     peb01                              98
     C                   if        *in98 = *off
     C*
     C                   exsr      updatr
     C*
     C                   else
     C*
     C                   exsr      writer
     C*
     C                   endif
     C*
     C                   read      peb02                                  99
     C                   enddo                                                  *in60=*off
     C*
     C                   seton                                        lr
     C**********************************************************************
     C* UPDATR - UPDATE FTEBPFL
     C**********************************************************************
     C     updatr        begsr
     C*
     C                   eval      ebdesc = e1desc
     C                   eval      ebdsc2 = e1dsc2
     C*****              eval      ebrevi = e1revi
     C                   eval      ebstat = e1stat
     C                   eval      ebpgm=q1pgm
     C                   eval      ebuser=q1user
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ebdt
     C                   move      dstime        ebtime
     C*
     C                   update    peb01
     C*
     C     upde          endsr
     C**********************************************************************
     C* WRITER - WRITE FTEBPFL
     C**********************************************************************
     C     writer        begsr
     C*
     C                   clear                   peb01
     C                   eval      ebco# = e1co#
     C                   eval      ebkey = e1key
     C                   eval      ebdesc = e1desc
     C                   eval      ebdsc2 = e1dsc2
     C*****              eval      ebrevi = e1revi
     C                   eval      ebstat = e1stat
     C                   eval      ebpgm=q1pgm
     C                   eval      ebuser=q1user
     C                   eval      ebiusr=q1user
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ebdt
     C                   movel     srdto         ebidt
     C                   move      dstime        ebtime
     C                   eval      ebxref = e1xref
     C                   eval      ebcrst = e1crst
     C                   eval      ebcret = e1cret
     C*
     C                   write     peb01
     C*
     C     wrte          endsr
