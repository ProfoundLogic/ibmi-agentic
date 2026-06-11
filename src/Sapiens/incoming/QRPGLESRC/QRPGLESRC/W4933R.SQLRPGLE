      * (C) Copyright Progressive Data Solutions, Inc., 2006
      *=========================================================================
      /title W4933R - FL Premium with Deductible Mass Bill Submitter
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *=========================================================================
      *  CALLED FROM: Command line
      *
      *  CREATED 08/19/06   C:136   PROJECT: 139WEE
      *=========================================================================
      * REVISIONS:
      *    Needs to be developed to accept entry parms
      *    call with typeFlag ('1' = Orig Bill, '2' = Audit Bill) and
      *              rptDate ('MMDDYYYY')
      *=========================================================================

     d xxfnd           s              3  0

     c     *entry        plist
     c                   parm                    typeFlag          1
     c                   parm                    rptDate           8

     c/exec SQL
     c+                  declare selectPolicies cursor
     c+                    for select distinct blfnd
     c+                          from wablp
     c+                          order by blfnd
     c/end-exec

     c/exec sql
     c+                  open selectPolicies
     c/end-exec

     c                   eval      SqlStt = '00000'
     c                   dow       SqlStt = '00000'
     c/exec sql
     c+                  fetch next
     c+                    from selectPolicies
     c+                    into :xxfnd
     c/end-exec

     c                   if        SqlStt <> '00000'
     c                   leave
     c                   endif

     c                   move(p)   xxfnd         QQFND
     c                   if        typeFlag = '1'
     c                   call      'W8001C'
     c                   PARM      '777'         QQCO#             3
     c                   PARM                    QQFND             3
     c                   PARM      '000'         QQFYR             3
     c                   PARM      '000000000'   QQEMP#            9
     c                   PARM      '00000'       QQDIV             5
     c                   PARM      rptDate       QQDATE            8
     c                   PARM      'OB'          QQCODE            2
     c                   PARM                    QQCOM1           30
     c                   PARM                    QQCOM2           30
     c                   PARM                    QQCOM3           30
     c                   PARM      '00000000'    QQDUE             8
     c                   PARM      'Y'           QQDEP             1
     c                   PARM                    QQSTYL            1
     c                   PARM      '3'           QQCALL            1
     c                   PARM                    QQOVR             1
     c                   PARM      'DB'          QQSRCD            2
     c                   PARM      'DA'          QQAJCD            2
     c                   PARM      'W'           QQQTE             1
     c                   PARM      'QPRINT'      QQOUTQ           10
     c                   PARM      'B'           QQRNB             1
     c                   PARM      'N'           QQDART            1
     c                   PARM      'N'           QQHARD            1
     c                   PARM      'N'           QQAGTO            1
     c                   PARM      'N'           QQFQT             1
     c                   PARM      ' '           QQRMCD            1
     c                   PARM      ' '           QQAR              2
     c                   PARM      '00000000'    QQEFDT            8
     c                   PARM                    QQFCM1           30
     c                   PARM                    QQFCM2           30
     c                   PARM                    QQFCM3           30
     c                   endif

     c                   if        typeFlag = '2'
     C                   CALL      'W7710C'
     C                   PARM      '777'         QQCO#
     C                   PARM                    QQFND
     C                   PARM      '000'         QQFYR
     C                   PARM      '000000000'   QQEMP#
     C                   PARM      '00000'       QQDIV
     C                   PARM      rptDate       QQDATE
     C                   PARM      'AT'          QQCODE
     C                   PARM                    QQCOM1
     C                   PARM                    QQCOM2
     C                   PARM                    QQCOM3
     C                   PARM      '3'           QQREV             1
     C                   PARM      'QPRINT'      QQOUTQ
     C                   PARM                    QQRMCD
     C                   PARM                    QQFCM1
     C                   PARM                    QQFCM2
     C                   PARM                    QQFCM3
     c                   endif

     c                   enddo

     c/exec sql
     c+                  close selectPolicies
     c/end-exec

     c                   seton                                        lr
