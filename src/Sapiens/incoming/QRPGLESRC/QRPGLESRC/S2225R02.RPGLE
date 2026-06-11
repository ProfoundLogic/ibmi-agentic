     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE S2225R02 - Contact Sub-Heading Information
     H*******************************************************************
     H* Created on 02/01/2010 by c:62 for Project 25WQK                 *
     H*******************************************************************
     H* Revisions:                                                      *
     H*                                                                 *
     H*******************************************************************
     Fs2225f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fsdcnp     uf a e             disk
     F                                     rename(pcn01:dbrcdf)
     Fsmchp     if   e             disk
     Fsms2p     if   e             disk
     Fsdcnl1    if   e           k disk
     Fwmfnl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Contact Information'
     D comapi          c                   'SA005R'
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
     D @@rrn2          s              9  0
     D @@rrn3          s              9  0
     D @@co#           s              3  0
     D kkfnd           s                   like(cnfnd)
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D #@co#           s              3
     D #@fnd           s              3
     D #@fyr           s              3
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
     C                   parm                    @@rrn2
     C                   parm                    @@rrn3
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     cnky          klist
     C                   kfld                    s2co#
     C                   kfld                    s2type
     C                   kfld                    s2seqh
     C                   kfld                    s2seqs
     C                   kfld                    kkfnd
     C*
     C     fnky          klist
     C                   kfld                    s2co#
     C                   kfld                    kkfnd
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
     C                   do
     C*
     C                   if        *in40
     C*
     C* Group must be valid if entered.
     C*
     C                   if        scfnd<>0
     C                   eval      kkfnd=scfnd
     C     fnky          chain     pfn01
     C                   if        not %found
     C                   seton                                        7190
     C                   leave
     C                   endif
     C                   endif
     C*
     C* Duplicates not allowed
     C*
     C                   eval      kkfnd=scfnd
     C     cnky          chain     pcn01
     C                   if        %found
     C                   seton                                        7290
     C                   leave
     C                   endif
     C*
     C                   endif
     C*
     C* Name is required
     C*
     C                   if        scname=*blanks
     C                   seton                                        7390
     C                   leave
     C                   endif
     C*
     C* Title is required
     C*
     C                   if        sctitl=*blanks
     C****               seton                                        7490
     C****               leave
     C                   endif
     C*
     C                   enddo
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
    C* KJDO - F10=Search
    C*================================================================
     C     kjdo          begsr
     C*
     C                   select
     C                   when      hfld='SCFND'
     C                   call      'W0010R'
     C     *inkc         parm      *off          ##KC
     C     *inkl         parm      *off          ##KL
     C                   parm      '2'           ##CALL
     C                   parm      *blanks       #@CO#
     C                   parm      *blanks       #@FND
     C                   parm      *blanks       #@FYR
     C*
     C                   if        #@fnd=*blanks
     C                   seton                                        3190
     C                   else
     C                   seton                                        3290
     C                   move      #@fnd         scfnd
     C                   move      #@fnd         kkfnd
     C     fnky          chain     pfn01
     C                   if        %found
     C                   movel(p)  fnname        scfnnm
     C                   else
     C                   eval      scfnnm=' '
     C                   endif
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   if        @@mode='A'
     C                   eval      scfnd=0
     C                   eval      scfnnm=' '
     C                   eval      scname=' '
     C                   eval      sctitl=' '
     C                   eval      scext#=0
     C                   eval      scstat='A'
     C                   else
     C                   eval      scfnd=cnfnd
     C*
     C                   eval      kkfnd=cnfnd
     C     fnky          chain     pfn01
     C                   if        %found
     C                   movel(p)  fnname        scfnnm
     C                   else
     C                   eval      scfnnm=' '
     C                   endif
     C*
     C                   eval      scname=cnname
     C                   eval      sctitl=cntitl
     C                   eval      scext#=cnext#
     C                   eval      scstat=cnstat
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   if        *in40
     C                   eval      cnco#=s2co#
     C                   eval      cntype=s2type
     C                   eval      cnseqh=s2seqh
     C                   eval      cnseqs=s2seqs
     C                   eval      cnseqc=1
     C                   eval      cnfnd=scfnd
     C                   endif
     C*
     C                   eval      cnname=scname
     C                   eval      cntitl=sctitl
     C                   eval      cnext#=scext#
     C                   eval      cnstat=scstat
     C*
     C* Assign Values to Maintenance Fields.
     C*
     C                   time                    dsdttm
     C                   eval      cnuser=q1user
     C                   eval      cnpgm =q1pgm
     C                   eval      cntime=dstime
     C                   eval      srdti=dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cndt
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
     C*
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C*
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
     C                   setoff                                       0940
     C*
     C                   select
     C                   when      @@mode='A'
     C                   seton                                        40
     C                   when      @@mode='D'
     C                   seton                                        09
     C                   endsl
     C*
     C     @@rrn2        chain     pch01
     C     @@rrn3        chain     ps201
     C*
     C                   movel(p)  chhdg1        scmhdg
     C                   movel(p)  s2hdg1        scshdg
     C                   if        scshdg=*blanks
     C                   eval      scshdg='* * Blank line * *'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      cniusr=cnuser
     C                   eval      cnipgm=cnpgm
     C                   eval      cnitim=cntime
     C                   eval      cnidt=cndt
     C                   write     dbrcdf
     C                   feod      sdcnp
     C*
     C                   endsr
