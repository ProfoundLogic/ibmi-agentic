     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H/TITLE FS911R                                                                         *
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H**************************************************************************************
     H*  Description:  Convert Taxonomy Codes                                              *
     H*                                                                                    *
     H**************************************************************************************
     H*  Created on 05/07/09 by c:62 for Project MC332                                     *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*   09/09/25 C:342 Project: MC453                                                    *
     H*                  Provider Taxonomy Codes Update                                    *
     H*                                                                                    *
     H**************************************************************************************
     FFTTCL1    UF A E           K DISK
     FTAXCOD    IF   E             DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkcode          s                   like(tccode)
     D kkefdt          s                   like(tcefdt)
     D dateIn          s             10
     D y               s              2  0
     D day             s              2    varying
     D month           s              2    varying
     D year            s              4    varying
     D dayN            s              2  0
     D monthN          s              2  0
     D yearN           s              4  0
     D date            s             10
     D dateCVT         s               D   datfmt(*USA)
     D ##co#           s              3

     DPSDS           ESDS                  Extname(SZQ1P)

     C     *entry        plist
     C                   parm                    ##co#

     C     tcky          klist
     C                   kfld                    kkcode
     C*** Beg Del *** 09/09/25 ****************************************
     C***                kfld                    kkefdt
     C*** End Del *** 09/09/25 ****************************************
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      ptx01                                  99
     C                   dow       not *in99
     C*
     C*** Beg Del *** 09/09/25 ****************************************
     C***               if        txnote<>'Notes'
     C*** End Del *** 09/09/25 ****************************************
     C*
     C                   eval      kkcode = txcode
     C*** Beg Del *** 09/09/25 ****************************************
     c***                eval      dateIn =txefdt
     C***                exsr      convertDate
     C***  *cymd         move      dateCVT       kkefdt
     C*** End Del *** 09/09/25 ****************************************
     C     tcky          chain     ptc01                              99
     C                   if        *in99 = *off
     C                   exsr      update_fttcp
     C                   else
     C                   exsr      write_fttcp
     C                   endif
     C*
     C*** Beg Del *** 09/09/25 ****************************************
     C***                endif
     C*** End Del *** 09/09/25 ****************************************
     C*
     C                   read      ptx01                                  99
     C                   enddo                                                  *in60=*off
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* ConvertDate - Converts date
     C*=====================================================================
     C     ConvertDate   begsr
     C*
     c                   clear                   monthN
     c                   clear                   dayN
     c                   clear                   yearN
     C*
     c                   eval      y=%scan('/':dateIn:1)
     c                   eval      month=%subst(datein:1:y-1)
     c                   eval      datein=%subst(dateIn:y+1:%len(dateIn)-y)
     c                   eval      y=%scan('/':dateIn:1)
     c                   eval      day=%subst(dateIn:1:y-1)
     c                   eval      datein=%subst(dateIn:y+1:%len(dateIn)-y)
     c                   eval      y=%scan(' ':dateIn:1)
     c                   eval      year=%subst(dateIn:1:y-1)
     c                   move      month         monthN
     c                   move      day           dayN
     c                   move      year          yearN
     C*
     c                   eval      date=%char(monthN) + '/' +
     c                                  %char(dayN) + '/' + %char(yearN)
     c                   move      date          dateCVT
     C*
     C                   endsr
     C*=====================================================================
     C* Move_data   - Moves data for update and write
     C*=====================================================================
     C     move_data     begsr
     C*
     c                   movel(p)  txcode        tccode
     c                   movel(p)  txclas        tcclas
     c                   movel(p)  txspcl        tcspcl
     c*
     C*** Beg Del *** 09/09/25 ****************************************
     c***                eval      dateIn =txefdt
     C***                exsr      convertDate
     C***  *cymd         move      dateCVT       tcefdt
     c*
     c***                if        txdedt<>*blanks
     c***                eval      dateIn =txdedt
     C***                exsr      convertDate
     C***  *cymd         move      dateCVT       tcdedt
     c***                endif
     C*** End Del *** 09/09/25 ****************************************
     C**** Beg Add *** 09/09/25 ****************************************
     c                   eval      TCdspn = TXdspn
     c                   eval      TCsect = TXsect
     C**** End Add *** 09/09/25 ****************************************
     C*
     C                   endsr
     C*=====================================================================
     C* Update_FTTCP - Updates FTTCP
     C*=====================================================================
     C     update_fttcp  begsr
     C*
     C                   exsr      move_data
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      tcdt
     C                   eval      tcpgm=q1pgm
     C                   eval      tcuser=q1user
     C                   move      sys_time      tctime
     C*
     C                   update    ptc01
     C*
     C                   endsr
     C*=====================================================================
     C* Write_fttcptx - Writes FTTCP
     C*=====================================================================
     C     write_fttcp   begsr
     C*
     C                   clear                   ptc01
     C*
     C                   exsr      move_data
     C*
     c                   eval      tcstat='A'
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      tcdt
     C     *cymd         move      sys_date      tcidt
     C                   eval      tcpgm=q1pgm
     C                   eval      tcipgm=q1pgm
     C                   eval      tcuser=q1user
     C                   eval      tciusr=q1user
     C                   move      sys_time      tctime
     C                   move      sys_time      tcitim
     C*
     C                   write     ptc01
     C*
     C                   endsr
