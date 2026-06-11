     H* (C) Copyright Fiserv Insurance Solutions, Inc., 2009                                       *
     H*============================================================================================*
     H/title FIXWADQP - Net Deductible cleanup                                                     *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H*============================================================================================*
     H* Created on 12/22/2009 by c:62 for Project 139WJP                    =======================*
     H*============================================================================================*
     H* Revisions:                                                          =======================*
     H*                                                                     =======================*
     H*============================================================================================*
     fwadql     uf   e           k disk                                         co/fnd/st/code/efdt
     fwdell     if   e           k disk                                         co/fnd/emp/div/fyr
     fwmeml     if   e           k disk                                         co/fnd/emp/div/fyr
     fwmrsl     if   e           k disk                                         co/fnd/st/code/efdt
     fwtddl2    if   e           k disk                                         co/fnd/fyr/emp/div
     fwmfnl     if   e           k disk                                         co/fnd/fyr/emp/div
     fsmcol     if   e           k disk                                         co/fnd/fyr/emp/div
     fsmupl     if   e           k disk                                         co/fnd/fyr/emp/div
     fqsysprt   o    f  132        printer oflind(*inoa)                        Deletion
     fqsysprt1  o    f  132        printer oflind(*inob)                        Pos Del
     fqsysprt2  o    f  132        printer oflind(*inoc)                        Not Deleted

     d psds          esds                  extname(szq1p)

     d kkst            s                   like(RSJRST)
     d kkcode          s                   like(ELDCDE)
     d kkefdt          s                   like(ELDCEF)

     d @@PrtUpdt       s              1a
     d updateFlag      s               n   inz(*off)
     d line            s            132    inz(*all'_')
     d hdfnd           s                   like(dqfnd) inz(*zeros)
     d xxname          s                   like(coname)
     D ##data          s             80
     D ##leng          s              3  0 INZ(40)
     D groupCnt        s              9  0 INZ(0)
     D groupCnt1       s              9  0 INZ(0)
     D groupCnt2       s              9  0 INZ(0)

     d firstCode       s                   like(ELDCDE)
     d secondCode      s                   like(ELDCD2)
     d firstType       s                   like(DDTYPE)
     d secondType      s                   like(DDTYPE)
     d firstEffDt      s                   like(ELDCEF)
     d secondEffDt     s                   like(EL2DEF)

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d xxtitl          c                   const('    Fix Net Deductible Recov-
     d                                     ery File    ')
     d x0titl          c                   const('         Records to be Delet-
     d                                     ed          ')
     d x1titl          c                   const('  Records to be Reviewed (No-
     d                                     t Deleted)  ')
     d x2titl          c                   const('          Records Not Delete-
     d                                     d           ')

     c     *entry        plist
     c                   parm                    @@PrtUpdt

     c     coky          klist
     c                   kfld                    upco#

     c     ddky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    kkst
     c                   kfld                    kkcode
     c                   kfld                    kkefdt

     c     ddky2         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    kkst
     c                   kfld                    kkcode

     c     elky1         klist
     c                   kfld                    dqco#
     c                   kfld                    dqfnd
     c                   kfld                    dqemp#
     c                   kfld                    emdiv
     c                   kfld                    dqfyr

     c     elky2         klist
     c                   kfld                    dqco#
     c                   kfld                    dqfnd
     c                   kfld                    dqemp#
     c                   kfld                    dqdiv
     c                   kfld                    dqfyr

     c     emky          klist
     c                   kfld                    dqco#
     c                   kfld                    dqfnd
     c                   kfld                    dqemp#

     c     fnky          klist
     c                   kfld                    dqco#
     c                   kfld                    dqfnd

     c     rsky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd

     c*
     c                   exsr      pgmstr
     c*
     c                   read      pdq01
     c                   dow       not(%eof)
     c*
     c* Set Defaults
     c*
     c                   eval      kkst=*blanks
     c                   eval      kkefdt=*zeros
     c*
     c                   if        dqfnd<>hdfnd
     c     fnky          chain     pfn01
     c                   eval      groupCnt=0
     c                   eval      groupCnt1=0
     c                   eval      groupCnt2=0
     c                   eval      hdfnd=dqfnd
     c                   endif
     c*
     c* Get the state reporting division
     c*
     c     emky          chain     pem01
     c                   if        %found
     c                   dow       not %eof
     c*
     c                   if        emstrp='Y'
     c                   leave
     c                   endif
     c*
     c     emky          reade     pem01
     c                   enddo
     c                   endif
     c*
     c* Get the effective date
     c*
     c     elky1         chain     pel01
     c                   if        %found
     c                   eval      kkefdt=elpfdt
     c                   endif
     c*
     c* Reset Deductible values
     c*
     c                   clear                   firstCode
     c                   clear                   secondCode
     c                   clear                   firstType
     c                   clear                   secondType
     c                   clear                   firstEffDt
     c                   clear                   secondEffDt
     c*
     c* Now let's access the employer/division of deductible recovery
     c*
     c     elky2         chain     pel01
     c                   if        %found
     c*
     c* Get the rating state
     c*
     c     rsky          chain     prs01
     c                   if        %found
     c                   eval      kkst=rsjrst
     c                   endif
     c*
     c* Get type for first deductible code
     c*
     c                   if        eldcde<>*blanks
     c                   eval      kkcode=eldcde
     c     ddky1         setgt     pdd01
     c     ddky2         readpe    pdd01
     c                   if        not %eof
     c                   eval      firstType=ddtype
     c                   endif
     c                   endif
     c*
     c                   eval      firstCode=eldcde
     c                   eval      firstEffDt=eldcef
     c*
     c* Get type for second deductible code
     c*
     c                   if        eldcd2<>*blanks
     c                   eval      kkcode=eldcd2
     c     ddky1         setgt     pdd01
     c     ddky2         readpe    pdd01
     c                   if        not %eof
     c                   eval      secondType=ddtype
     c                   endif
     c                   endif
     c*
     c                   eval      secondCode=eldcd2
     c                   eval      secondEffDt=el2def
     c                   endif
     c*
     c* All information has been gathered so lets print and delete (if ok)
     c*
     c                   select
     c*
     c* List on Deleted Report
     c*
     c                   when      firstCode=*blanks or
     c                             (firstType='G' and secondType='G') or
     c                             (firstType='G' and secondCode=*blanks)
     c                   exsr      printDelete
     c*
     c* Delete if called in update mode
     c*
     c                   if        updateFlag
     c                   delete    pdq01
     c                   endif
     c*
     c* List on Possible Deletion Report
     c*
     c                   when      (firstType='G' and secondType='N') or
     c                             (firstType='N' and secondType='G') or
     c                             (firstCode<>' ' and firstType=' ') or
     c                             (secondCode<>' ' and secondType=' ')
     c                   exsr      printPosDel
     c*
     c* List on Not Deleted Report
     c*
     c                   when      (firstType='N' and secondType='N') or
     c                             (firstType='N' and secondType=' ')
     c                   exsr      printNotDel
     c                   endsl
     c*
     c                   read      pdq01
     c                   enddo

     c                   seton                                        lr
     c*=====================================================================
     c* pgmstr - Program Start Area
     c*=====================================================================
     c     pgmstr        begsr
     c*
     c*
     c* Prepare for Print/Update
     c*
     c                   eval      updateFlag=*off
     c                   if        @@PrtUpdt='U'
     c                   eval      updateFlag=*on
     c                   endif
     c*
     c     q1user        chain     pup01
     c     coky          chain     pco01
     c*
     C                   movel(p)  coname        ##data
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        xxname
     c*
     c                   time                    dsdttm
     c*
     c                   endsr
     c*=====================================================================
     c* printDelete - Prints To Deletion Report
     c*=====================================================================
     c     printDelete   begsr
     c*
     c                   if        *inoa  or groupCnt=0
     c                   except    head
     c                   eval      *inoa=*off
     c                   endif
     c*
     c                   except    det
     c                   eval      groupCnt=groupCnt+1
     c*
     c                   endsr
     c*=====================================================================
     c* printPosDel - Prints To Possible Deletion Report
     c*=====================================================================
     c     printPosDel   begsr
     c*
     c                   if        *inob or groupCnt1=0
     c                   except    head1
     c                   eval      *inob=*off
     c                   endif
     c*
     c                   except    det1
     c                   eval      groupCnt1=groupCnt1+1
     c*
     c                   endsr
     c*=====================================================================
     c* printNotDel - Prints to Not Deleted Report
     c*=====================================================================
     c     printNotDel   begsr
     c*
     c                   if        *inoc or groupCnt2=0
     c                   except    head2
     c                   eval      *inoc=*off
     c                   endif
     c*
     c                   except    det2
     c                   eval      groupCnt2=groupCnt2+1
     c*
     c                   endsr
     O*=====================================================================
     O* Deletion Report
     O*=====================================================================
     Oqsysprt   e            head           1 03
     O                       dstime               8 '  :  :  '
     O                       xxname              91
     O                       dsdate        y    132
     *
     O          e            head           1
     O                       q1pgm               10
     O                       xxtitl              91
     O                                          127 'PAGE:'
     O                       page          z    132
     *
     O          e            head           1
     O                       x0titl              91
     *
     O          e            head        1  2
     O                                         +000 'Group:'
     O                       fnfnd             +001
     O                       fnname            +002
     *
     O          e            head           0
     O                       line              +000
     *
     O          e            head           1
     O                                         +000 ' Employer'
     O                                         +002 'Division'
     O                                         +002 'Year'
     O                                         +002 ' Check#'
     O                                         +006 'Amount'
     O                                         +002 'Remain Amt'
     *
     O          e            det            1
     O                       dqemp#        z   +000
     O                       dqdiv             +005 '   0 '
     O                       dqfyr             +003
     O                       dqchk#        4   +002
     O                       dqamt             +002 '     0 .  '
     O                       dqamtr            +002 '     0 .  '
     O*=====================================================================
     O* Possible Deletion Report
     O*=====================================================================
     Oqsysprt1  e            head1          1 03
     O                       dstime               8 '  :  :  '
     O                       xxname              91
     O                       dsdate        y    132
     *
     O          e            head1          1
     O                       q1pgm               10
     O                       xxtitl              91
     O                                          127 'PAGE:'
     O                       page1         z    132
     *
     O          e            head1          1
     O                       x1titl              91
     *
     O          e            head1       1  2
     O                                         +000 'Group:'
     O                       fnfnd             +001
     O                       fnname            +002
     *
     O          e            head1          0
     O                       line              +000
     *
     O          e            head1          1
     O                                         +000 ' Employer'
     O                                         +002 'Division'
     O                                         +002 'Year'
     O                                         +002 ' Check#'
     O                                         +006 'Amount'
     O                                         +002 'Remain Amt'
     *
     O          e            det1           1
     O                       dqemp#        z   +000
     O                       dqdiv             +005 '   0 '
     O                       dqfyr             +003
     O                       dqchk#        4   +002
     O                       dqamt             +002 '     0 .  '
     O                       dqamtr            +002 '     0 .  '
     O*=====================================================================
     O* Not Deleted Report
     O*=====================================================================
     Oqsysprt2  e            head2          1 03
     O                       dstime               8 '  :  :  '
     O                       xxname              91
     O                       dsdate        y    132
     *
     O          e            head2          1
     O                       q1pgm               10
     O                       xxtitl              91
     O                                          127 'PAGE:'
     O                       page2         z    132
     *
     O          e            head2          1
     O                       x2titl              91
     *
     O          e            head2       1  2
     O                                         +000 'Group:'
     O                       fnfnd             +001
     O                       fnname            +002
     *
     O          e            head2          0
     O                       line              +000
     *
     O          e            head2          1
     O                                         +000 ' Employer'
     O                                         +002 'Division'
     O                                         +002 'Year'
     O                                         +002 ' Check#'
     O                                         +006 'Amount'
     O                                         +002 'Remain Amt'
     *
     O          e            det2           1
     O                       dqemp#        z   +000
     O                       dqdiv             +005 '   0 '
     O                       dqfyr             +003
     O                       dqchk#        4   +002
     O                       dqamt             +002 '     0 .  '
     O                       dqamtr            +002 '     0 .  '
