     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE S1225R - Contact Sub-Heading Information
     H*******************************************************************
     H* Created on 02/01/2010 by c:62 for Project 25WQK                 *
     H*******************************************************************
     H* Revisions:                                                      *
     H*                                                                 *
     H*******************************************************************
     Fs1225f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fsms2p     uf a e             disk
     F                                     rename(ps201:dbrcdf)
     Fsms2l     uf   e           k disk    prefix(S3:2)
     Fsdcnl     uf   e           k disk
     Fsmchp     if   e             disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Contact SubHeading Information'
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
     D @@co#           s              3  0
     D kkseqh          s                   like(s2seqh)
     D kkseqs          s                   like(s2seqs)
     D hdseqs          s                   like(s2seqs)
     D sequence        s                   like(s2seqs)
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
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     s2ky          klist
     C                   kfld                    s2co#
     C                   kfld                    s2type
     C                   kfld                    s2seqh
     C                   kfld                    s2seqs
     C*
     C     s2ky1         klist
     C                   kfld                    chco#
     C                   kfld                    chtype
     C                   kfld                    chseqh
     C                   kfld                    kkseqs
     C*
     C     s2ky2         klist
     C                   kfld                    chco#
     C                   kfld                    chtype
     C                   kfld                    chseqh
     C*
     C     s2ky3         klist
     C                   kfld                    s2co#
     C                   kfld                    s2type
     C                   kfld                    s2seqh
     C*
     C     s2ky4         klist
     C                   kfld                    s3co#
     C                   kfld                    s3type
     C                   kfld                    s3seqh
     C                   kfld                    s3seqs
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C*
     C* Need to delete the contacts.
     C*
     C     s2ky          chain     pcn01                              99
     C                   dow       not *in99
     C                   delete    pcn01
     C     s2ky          reade     pcn01                                  99
     C                   enddo
     C*
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
     C                   if        @@mode='A'
     C                   eval      scstat='A'
     C                   else
     C                   eval      scstat=s2stat
     C                   endif
     C*
     C                   eval      schdg1=s2hdg1
     C                   eval      schdg2=s2hdg2
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   select
     C*
     C* Add Mode
     C*
     C                   when      @@mode='A'
     C                   eval      s2co#=chco#
     C                   eval      s2type=chtype
     C                   eval      s2seqh=chseqh
     C                   eval      kkseqs=*hival
     C     s2ky1         setgt     ps201
     C     s2ky2         readpe    ps201                                  99
     C                   if        not *in99
     C                   eval      s2seqs=s3seqs+1
     C                   else
     C                   eval      s2seqs=1
     C                   endif
     C*
     C* Change Status
     C*
     C                   when      @@mode='S'
     C                   if        s2stat='A'
     C                   eval      s2stat='I'
     C                   else
     C                   eval      s2stat='A'
     C                   endif
     C*
     C* Move this element up
     C*
     C                   when      @@mode='U'
     C                   exsr      updat2a
     C                   eval      kkseqs=s2seqs
     C                   eval      s2seqs=s2seqs-1
     C                   eval      hdseqs=s2seqs
     C*
     C* Move this element down
     C*
     C                   when      @@mode='W'
     C                   exsr      updat2a
     C                   eval      kkseqs=s2seqs
     C                   eval      s2seqs=s2seqs+1
     C                   eval      hdseqs=s2seqs
     C                   endsl
     C*
     C* Update/Write Screen fields to database
     C*
     C                   if        @@mode='A' or @@mode='C'
     C                   eval      s2hdg1=schdg1
     C                   eval      s2hdg2=schdg2
     C                   endif
     C*
     C* Assign Values to Maintenance Fields.
     C*
     C                   time                    dsdttm
     C                   eval      s2user=q1user
     C                   eval      s2pgm =q1pgm
     C                   eval      s2time=dstime
     C                   eval      srdti=dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         s2dt
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
     C*
     C* Re-sequence the sequence numbers
     C*
     C                   if        @@mode='E'
     C*
     C                   eval      sequence=1
     C*
     C     s2ky3         setll     ps201
     C     s2ky3         reade     ps201                                  99
     C                   dow       not *in99
     C*
     C     s2ky4         chain     pcn01                              99
     C                   dow       not *in99
     C                   eval      cnseqs=sequence
     C                   update    pcn01
     C     s2ky4         reade     pcn01                                  99
     C                   enddo
     C*
     C                   eval      s3seqs=sequence
     C                   update    ps201
     C                   eval      sequence=sequence+1
     C*
     C     s2ky3         reade     ps201                                  99
     C                   enddo
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     @@rrn2        chain     pch01
     C                   movel(p)  chhdg1        scmhdg
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   if        @@mode='S' or @@mode='U' or @@mode='W'
     C                   exsr      updatr
     C                   endif
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
     C                   exsr      updCNP
     C                   update    dbrcdf
     C                   if        @@mode='U' or @@mode='W'
     C                   exsr      updat2b
     C                   endif
E01  C                   endif
     C* Return
     C                   if        @@mode='S' or @@mode='U' or @@mode='W'
     C                   return
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* UPDCNP - Update SDCNP
    C*================================================================
     C     updCNP        begsr
     C*
     C     s2ky1         chain     pcn01                              99
B01  C                   dow       *in99 = *off
     C                   eval      cnseqs=hdseqs
     C                   time                    dsdttm
     C                   eval      cnuser=q1user
     C                   eval      cnpgm =q1pgm
     C                   eval      cntime=dstime
     C                   eval      srdti=dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cndt
     C                   update    pcn01
     C     s2ky1         reade     pcn01                                  99
     C                   enddo
     C*
     C                   endsr
    C*================================================================
    C* UPDAT2A - Update the other record to seq# 0
    C*================================================================
     C     updat2a       begsr
     C*
     C                   if        @@mode='U'
     C                   eval      kkseqs=s2seqs-1
     C                   else
     C                   eval      kkseqs=s2seqs+1
     C                   endif
     C*
     C     s2ky1         chain     ps201                              99
B01  C                   if        *in99 = *off
     C                   eval      s3seqs=0
     C                   eval      hdseqs=0
     C                   time                    dsdttm
     C                   eval      s3user=q1user
     C                   eval      s3pgm =q1pgm
     C                   eval      s3time=dstime
     C                   eval      srdti=dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         s3dt
     C                   exsr      updCNP
     C                   update    ps201
E01  C                   endif
     C*
     C                   endsr
    C*================================================================
    C* UPDAT2B - Update the other record to Final Sequence #
    C*================================================================
     C     updat2b       begsr
     C*
     C                   eval      kkseqs=0
     C*
     C     s2ky1         chain     ps201                              99
B01  C                   if        *in99 = *off
     C*
     C                   if        @@mode='U'
     C                   eval      s3seqs=s2seqs+1
     C                   else
     C                   eval      s3seqs=s2seqs-1
     C                   endif
     C*
     C                   eval      hdseqs=s3seqs
     C*
     C                   time                    dsdttm
     C                   eval      s3user=q1user
     C                   eval      s3pgm =q1pgm
     C                   eval      s3time=dstime
     C                   eval      srdti=dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         s3dt
     C                   exsr      updCNP
     C                   update    ps201
E01  C                   endif
     C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      s2stat='A'
     C                   eval      s2iusr=s2user
     C                   eval      s2ipgm=s2pgm
     C                   eval      s2itim=s2time
     C                   eval      s2idt=s2dt
     C                   write     dbrcdf
     C                   feod      sms2p
     C*
     C                   endsr
