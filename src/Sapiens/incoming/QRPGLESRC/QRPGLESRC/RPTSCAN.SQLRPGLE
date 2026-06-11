     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************************************
     h/TITLE rptscan - # of scanned docs
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:                                                                              *
     h*                                                                                            *
     h*                                                                                            *
     h*  Called from:RPTQTRC                                                                       *
     h*  Calls      :                                                                              *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 16/29/2011 by c:235 for Project SY246                                          *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
    H*                                                                   *
     H*  07/18/14 C:300 PROJECT: SY246                                    *
     H*                 Total number of pages is 0. Fixing it.            *
     h*                                                                                            *
     H* 12/08/11 C:189 PROJECT:SY246 update
     H*                As per the revised specs only count needs to be printed
     h*
     H* 10/28/11 C:189 PROJECT:SY246 corrections                                                   *
     H*                Date format not being printed correctly                                     *
     H*                                                                                            *
     h******************************************************************************************** *
     fqsysprt   o    f  132        printer oflind(*inof)
     d eFromDate       s              8  0
     d eToDate         s              8  0
     d eFromDat        s              8
     d eToDat          s              8
     d eFromDT1        s              8  0
     d eToDT1          s              8  0
      *** begin chg ***  07/18/14  ***************************************************
     d********* @count          s              4  0
     d @count          s              9  0
      *** End chg *****  07/18/14  ***************************************************
     d TITLE           s             40
     d*
     D                 ds
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
    D*
      *=============================================================================================
      * Define
      *=============================================================================================
     d @DODCID         s             12
     d @DOWSID         s              2
     d @DOSDT          s              8  0
     d @DOSTMD         s              6  0
     d @DOSREP         s              8
     d @DOBCHI         s             12
      *** begin chg ***  07/18/14  ***************************************************
     d********* @DOPGS          s              4  0
     d @DOPGS          s              9  0
      *** End chg *****  07/18/14  ***************************************************
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    eFromDat
     c                   parm                    eToDat
     c*
     C*** begin add ***  10/28/11  ***************************************************
     C     DTCYMD1       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** end   add ***  10/28/11  ***************************************************
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     C*** begin chg ***  10/28/11  ***************************************************
     C****************   Move      eFromDat      eFromDate
     C****************   Move      eToDat        eToDate
     C                   Move      eFromDat      eFromDT1
     C                   Move      eToDat        eToDT1
     C                   Move      eFromDat      srdti
     C                   Call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         eFromDate
     C*
     C                   Move      eToDat        srdti
     C                   Call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         eToDate
     C*
     C*** end   chg ***  10/28/11  ***************************************************
     C*** beg   add ***  12/08/11  ***************************************************
     c/exec SQL
     c+  select count(*) into :@count from  IMDOP
     c+
     C+ WHERE  DOSDT >= :eFromDT1   AND
     C+        DOSDT <= :eToDT1
     c/end-exec
     C*
     c/exec SQL
     c+  select sum(dopgs) into :@dopgs  from  IMDOP
     c+
     C+ WHERE  DOSDT >= :eFromDT1   AND
     C+        DOSDT <= :eToDT1
     c/end-exec
     C*** end   add ***  12/08/11  ***************************************************
     C*** beg   del ***  12/08/11  ***************************************************
     c***********/exec SQL
     c***********+  declare PriNtrprt  scroll cursor for
     C************+ SELECT DODCID,DOWSID,DOSDT,DOSTMD,DOSREP,DOBCHI,DOPGS
     C*************+ FROM IMDOP
     C*************+ WHERE  DOSDT >= :eFromDate  AND
     C*************+        DOSDT <= :eToDate
     c************/end-exec

     c***********/exec SQL
     c**********+    open  PriNtrprt
     c***********/end-exec

     c********           eval      *inof    = *on
     c********           move      '0'           count
     c********           dow       SQLstt <> '02000'

     c********       /exec SQL
     c********+    fetch next from PriNtrprt
     c***********+               into :@DODCID, :@DOWSID, :@DOSDT, :@DOSTMD, :@DOSREP,
     c*************+                    :@DOBCHI,:@DOPGS
     c************/end-exec
     C*** end   del ***  12/08/11  ***************************************************

     c                   exsr      PriNt
     C*** beg   del ***  12/08/11  ***************************************************
     C************ begin add ***  10/28/11  ***************************************************
     c**************                   If        SQLstt <> '02000'
     C************ end   add ***  10/28/11  ***************************************************
     c***********                   eval      count = count+1
     C*** begin add ***  10/28/11  ***************************************************
     c****************                   endif
     C*** end   add ***  10/28/11  ***************************************************
     c****************                   enddo
     c*
     c***********                   If        SQLstt = '02000'
     C*** end   del ***  12/08/11  ***************************************************
     c                   except    total
     C*** beg   del ***  12/08/11  ***************************************************
     c**********         endif
     c*

     c**************/exec SQL
     c***************+    close PriNtrprt
     c*************/end-exec
     C*** end   del ***  12/08/11  ***************************************************

     c                   seton                                        lr
     c**********************************************************************************
     c* Print Report
     c**********************************************************************************
     c*
     c     PRINT         BEGSR
     c*
     C*** beg   del ***  12/08/11  ***************************************************
     c************       if        *inof
     C*** end   del ***  12/08/11  ***************************************************
     C                   TIME                    DSDTTM
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     c                   except    head
     C*** beg  del ***  12/08/11  ***************************************************
     c***********        eval      *inof = *off
     c***********        endif
     c*
     c***********        except    detail
     C*** end  del ***  12/08/11  ***************************************************
     c*
     c*
     c     printe        endsr
     c******************************************************************************************

     oqsysprt   e            head        6  2
     o                       title               60
     O                       dstime              08 '  :  :  '
     O                                           79 'Scanned Documents report'
     O                       dsdate             128 '  /  /    '
     O          E            head           1
     O                                           12 'DATE RANGE:'
     C*** begin chg ***  10/28/11  ***************************************************
     O******************     eFromDate           23 '    /  /  '
     O                       eFromDate           23 '  /  /    '
     C*** end   chg ***  10/28/11  ***************************************************
     O                                           26 'to'
     C*** begin chg ***  10/28/11  ***************************************************
     O******************     eToDate             37 '    /  /  '
     O                       eToDate             37 '  /  /    '
     C*** end   chg ***  10/28/11  ***************************************************
     O                                           69 'CLTID -'
     O                       ##CLT#              73
     O                                          124 'PAGE:'
     O                       PAGE               128
     O          E            head           1
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     O                                              '--------------------------'
     o*** beg  del ***  12/08/11  ***************************************************
     o**********e            head           2
     o**********                                 +2 'DOCUMENT ID'
     o**********                                 +2 'SCANNING WS ID'
     o**********                                 +2 'SCAN DATE'
     o**********                                 +2 'SCAN TIME'
     o**********                                 +2 'SCANNING USR ID'
     o**********                                 +2 'SCANNING BCH ID'
     o**********                                 +2 '# OF PAGES THIS DOC'
     O**********E            head           1
     O**********                                    '--------------------------'
     O**********                                    '--------------------------'
     O**********                                    '--------------------------'
     O**********                                    '--------------------------'
     O**********                                    '--------------------------'
     o**********e            detail         1
     o**********             @doDcid             +2
     o**********             @DOWSID             +6
     o**********             @DOSDT              +9
     o**********             @DOSTMD             +4
     o**********             @DOSREP             +7
     o**********             @DOBCHI             +8
     o**********             @DOPGS              +7
     o*** end  del ***  12/08/11  ***************************************************
     O          e            total          1
     o                                           +2 '  Count:'
     o
     o                       @count        3     +2
     o                                           +2 '  Pages:'
     o                       @DOPGS        3     +2
