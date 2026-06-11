     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************************************
     h/TITLE rptattach2:-# of document attachments
     h*option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:                                                                              *
     h*                                                                                            *
     h*  Called from:RPTQTRC                                                                       *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 16/29/2011 by c:235 for Project SY246                                          *
     h**********************************************************************************************
     h* Revisions:
     h*
     H* 12/08/11 C:189 PROJECT:SY246 update
     H*                As per the revised specs only count needs to be printed
     h*
     H* 10/28/11 C:189 PROJECT:SY246 corrections
     H*                Date format not being printed correctly
     H*
     H* 08/06/11 C:189 PROJECT:SY246 PRISS 33
     H*                PROGRAM GOING TO ENDLESS LOOP FOR BLANK OFFICE DOCUMENT ID
     H*
     h**********************************************************************************************
     fqsysprt   o    f  210        printer oflind(*inof)
     d eFromDat        s              8
     d eToDat          s              8
     d eFromDate       s              8  0
     d eToDate         s              8  0
     d xxcnt           s              7  0
     d xxcnt1          s              7  0
     d xxtot           s              7  0
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
     d @Frdt           s              7  0
     d @ToDt           s              7  0
     d @CMKEY          s             40
     d @CMSEQ#         s              5  0
     d @CMDCID         s             12
     d @CMDCTP         s              5
     d @CMIDOC         s             12
     d @CMFOLD         s             80
     d @CMIDT          s              7  0
     d @CMIMGF         s              1
     d @CMqpty         s              1
     D*
     d @MDKEY          s             40
     d @MDSEQ#         s              5  0
     d @MDOID          s             40
     d @MDDOCT         s              8
     d @MDOFLR         s             60
     d @MDIDT          s              7  0
     d @MDOTYP         s              1
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    eFromDat
     c                   parm                    eToDat
     c*
     C     DTCYMD2       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
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
     C                   Move      eFromDat      srdti
     C                   Call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         eFromDate
     C*
     C                   Move      eToDat        srdti
     C                   Call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         eToDate
     C*
     C*** end   chg ***  10/28/11  ***************************************************
     C                   Move      eFromDat      srdti
     C                   Call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         @frdt
     C                   Move      eToDat        srdti
     C                   Call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         @Todt
     C                   eval      flsmdp = *off
     c/exec SQL
     c+  declare PriNtrprt  scroll cursor for
     C+ SELECT CMKEY,CMSEQ#,CMDCID,CMDCTP,CMIDOC,CMFOLD,CMIDT,CMIMGF,CMQPTY
     C+ FROM SDCMP
     C+ WHERE  CMIDT >= :@Frdt  AND
     C+        CMIDT <= :@ToDt
     C+ ORDER BY CMKEY,CMSEQ#
     c/end-exec

     c/exec SQL
     c+    open  PriNtrprt
     c/end-exec

     c/exec SQL
     c+    fetch next from PriNtrprt
     c+               into :@CMKEY, :@CMSEQ#, :@CMDCID, :@CMDCTP, :@CMIDOC,
     c+                    :@CMFOLD, :@CMIDT, :@CMIMGF, :@CMQPTY
     c/end-exec
     c                   eval      *inof    = *on
     c                   move      '0'           xxcnt
     c                   dow       SQLstt <> '02000'

     c                   If        @CMDCID =  ' '  and @CMQPTY =  ' '
     c                   goto      skip
     c                   endif
     c*
     c                   If        @CMDCID <> ' '  and
     C                             %SUBST(@cmdcid:%LEN(%TRIM(@cmdcid))-3:1)  =
     C                             '.'
     c*
     c                   exsr      PriNt
     c                   eval      xxcnt = xxcnt+1
     c*
     c                   If        @CMQPTY <> ' '
     c                   exsr      srSMMDP
     c                   endif
     c                   endif
     C*** begin add ***  08/06/11  ***************************************************
     C     skip          tag
     C*** End   add ***  08/06/11  ***************************************************
     c/exec SQL
     c+    fetch next from PriNtrprt
     c+               into :@CMKEY, :@CMSEQ#, :@CMDCID, :@CMDCTP, :@CMIDOC,
     c+                    :@CMFOLD, :@CMIDT, :@CMIMGF, :@CMQPTY
     c/end-exec
     C*** begin del ***  08/06/11  ***************************************************
     C*************     skip          tag
     C*** End   del ***  08/06/11  ***************************************************
     c                   enddo
     c*
     c                   If        SQLstt = '02000'
     c                   except    total
     c                   endif
     c*

     c/exec SQL
     c+    close PriNtrprt
     c/end-exec
     c                   seton                                        lr
     c**********************************************************************************
     c* Print Report
     c**********************************************************************************
     c*
     c     PRINT         BEGSR
     c*
     c                   if        *inof
     C                   TIME                    DSDTTM
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c*
     C*** begin add ***  12/08/11  ***************************************************
     c***********        if        flsmdp = *off
     c***********        except    detail
     c***********        else
     c***********        except    detail1
     c***********        eval      flsmdp= *off
     c***********        endif
     C*** end   add ***  12/08/11  ***************************************************
     c*
     c*
     c     printe        endsr
     c******************************************************************************************
     c**********************************************************************************
     c* Query on SMMDP
     c**********************************************************************************
     c*
     c     srSMMDP       BEGSR

     c/exec SQL
     C+ declare PriNtrprt2 scroll cursor for SELECT
     C+ MDKEY,MDSEQ#,MDOID,MDDOCT,MDOFLR,MDIDT,MDOTYP
     C+ FROM SMMDP
     C+ WHERE  MDKEY = :@CMKEY  AND
     C+        MDSEQ# = :@CMSEQ#
     C*+ ORDER BY MDKEY,MDSEQ#
     c/end-exec

     c/exec SQL
     c+    open  PriNtrprt2
     c/end-exec
     c*                  eval      *inof    = *on
     c*                  move      '0'           xxcnt1
     c/exec SQL
     c+    fetch next from PriNtrprt2
     c+               into :@MDKEY, :@MDSEQ#, :@MDOID, :@MDDOCT, :@MDOFLR,
     c+                    :@MDIDT, :@MDOTYP
     c/end-exec
     c*
     c                   dow       SQLstt <> '02000'
     c                   move      '1'           flsmdp            1
     c*
     c                   exsr      PriNt
     c                   eval      xxcnt = xxcnt+1
     c/exec SQL
     c+    fetch next from PriNtrprt2
     c+               into :@MDKEY, :@MDSEQ#, :@MDOID, :@MDDOCT, :@MDOFLR,
     c+                    :@MDIDT, :@MDOTYP
     c/end-exec
     c                   enddo
     c
     c/exec SQL
     c+    close PriNtrprt2
     c/end-exec
     c     srSMMDPe      endsr
     c**********************************************************************************

     oqsysprt   e            head        6  2
     o                       title               60
     O                       dstime              08 '  :  :  '
     O                                          105 'Document attachments '
     O                                          112 'report '
     O*                      dsdate             128 '  /  /    '
     O                       dsdate             206 '  /  /    '
     O          E            head           1
     O                                           12 'DATE RANGE:'
     C*** begin chg ***  10/28/11  ***************************************************
     O***************        eFromDate           23 '    /  /  '
     O                       eFromDate           23 '  /  /    '
     C*** end   chg ***  10/28/11  ***************************************************
     O                                           26 'to'
     C*** begin chg ***  10/28/11  ***************************************************
     O****************       eToDate             37 '    /  /  '
     O                       eToDate             37 '  /  /    '
     C*** end   chg ***  10/28/11  ***************************************************
     O                                          100 'CLTID -'
     O                       ##CLT#             105
     O                                          202 'PAGE:'
     O                       PAGE               206
     O*** begin del ***  12/08/11  ***************************************************
     O********* E            head           1
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '------------------------'
     o********* e            head           2
     o*********                                     'KEY        '
     o*********                                  46 'SEQ#'
     o*********                                  55 'DOC ID'
     o*********                                  69 'DOC TYPE'
     o*********                                  84 'IMAGE DOC NAME'
     o*********                                  91 'FOLDER'
     o*********                                 177 'INTIAL DATE'
     o*********                                 192 'TYPE FLAG'
     O********* E            head           1
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '--------------------------'
     O*********                                     '------------------------'
     o********* e            detail         1
     o*********              @CMKEY              40
     o*********              @CMSEQ#             47
     o*********              @CMDCID             61
     o*********              @CMDCTP             68
     o*********              @CMIDOC             82
     o*********              @CMFOLD            164
     o*********              @CMIDT             173
     o*********              @CMIMGF            188
     o*
     o********* e            detail1        1
     o*********              @MDKEY              40
     o*********              @MDSEQ#             47
     o*********              @MDOID              89
     o*********              @MDDOCT             99
     o*********              @MDOFLR            164
     o*********              @MDIDT             173
     o*********              @MDOTYP            188
     o*** end   del ***  12/08/11  ***************************************************
     O          e            total          1
     o                                           +2 '  Count:'
     o                       xxcnt         3     +2
