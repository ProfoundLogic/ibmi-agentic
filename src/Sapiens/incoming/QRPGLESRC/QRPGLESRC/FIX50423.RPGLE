    Hcopyright ('(c) Copyright Fiserv, Inc., 2005 - 2007')
     *********************************************************************
    H/TITLE FIX50423 - Populate CMDCID with Correct Image Name
     *********************************************************************
     * CALLED FROM:                                                      *
     *   COMMAND LINE                                                    *
     * CALLS TO:                                                         *
     *   NONE                                                            *
     *                                                                   *
     * CREATED:  12/21/05     C:167    SUPPORT: 50423                    *
     *********************************************************************
     * REVISIONS:                                                        *
     *                                                                   *
     * 01/05/06  C:33  Seq#: VI114/56258                                 *
     *                 Account for records with an initial date less than*
     *                  the remark date.                                 *
     *********************************************************************
     Fsdcmp     uf   e             disk

     Fqsysprt   o    f  132        printer oflind(*inoa)

     D ocmfold         s             25
     D qqdcid          s             12
     D qqfold          s             80
     D qqrtcd          s              1
     D totalrecs       s              6  0 inz(0)
     D totalmod        s              6  0 inz(0)

     D wkdcid          ds
     D  wkdcid12               1      2
     D  wkdcid35               3      5
     D  wkdcid68               6      8
     D  wkdcid9                9      9
     D  wkdcidpdf             10     12

     C                   except    Head

     C                   eval      wkdcid9 = '.'
     C                   eval      wkdcidpdf = 'PDF'

     C     *hival        setgt     pcm01

     C                   dow       *inlr = *off
     C                   readp     pcm01
     C                   if        cmidt <= 1051104
     C***  Begin Add  ***  01/05/06  **************************************
     C                              and CMIDT >= CMCDAT
     C***  End   Add  ***  01/05/06  **************************************
     C                   eval      *inlr = *on
     C                   leave
     C                   endif
     C                   eval      totalrecs = totalrecs + 1
     C                   if        cmimgf <> 'O'
     C                   iter
     C                   endif
     C                   if        %subst(cmdcid:10:3) = 'PDF'
     C                   iter
     C                   endif
     C                   eval      wkdcid12 = %subst(cmdcid:1:2)
     C                   eval      wkdcid35 = %subst(cmdcid:6:3)
     C                   eval      wkdcid68 = %subst(cmdcid:10:3)
     C                   movel     cmfold        ocmfold
     C                   eval      qqrtcd = *blank
     C                   eval      qqdcid = wkdcid
     C                   eval      qqfold = cmfold
     C                   call      'SVALDOC'
     C                   parm                    qqdcid
     C                   parm                    qqfold
     C                   parm                    qqrtcd

     C                   if        qqrtcd = ' '
     C                   eval      totalmod = totalmod + 1
     C                   if        *inoa = *on
     C                   except    Head
     C                   eval       *inoa = *off
     C                   endif
     C                   except    Detail
     C                   clear                   cmdcid
     C                   eval      cmdcid = wkdcid
     C                   update    pcm01
     C                   endif

     C                   enddo

     C                   except    TotalLine
     C                   eval      *inlr = *on

     OQSYSPRT   E            Head           1  3
     O                                           80 'Comment Records with Incor-
     O                                              rect Extensions'
     O          E            Head           1
     O                                           13 'Prior'
     O                                           25 'New'
     O                                           40 'Date'
     O                                           49 'Flag'
     O                                           57 'Folder'
     O                                           82 'RTCD'
     O                                           88 'Key'
     O                                          112 'Page:'
     O                       PAGE
     O          E            Detail         1
     O                       totalmod      Z      6
     O                       cmdcid              20
     O                       wkdcid              34
     O                       cmidt               43
     O                       cmimgf              47
     O                       ocmfold        B    76
     O                       qqrtcd         B    80
     O                       cmkey              125
     O          E            TotalLine      1
     O                                            8 'Total In'
     O                       totalrecs     Z     20
     O          E            TotalLine      1
     O                                            9 'Total Mod'
     O                       totalmod      Z     20
