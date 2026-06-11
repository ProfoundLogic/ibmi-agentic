     *  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     *********************************************************************
    H/TITLE FIX50423 - Populate CMIMGF with Correct Code for PDF objects
     *********************************************************************
     * CALLED FROM:                                                      *
     *   COMMAND LINE                                                    *
     * CALLS TO:                                                         *
     *   NONE                                                            *
     *                                                                   *
     * CREATED:  09/03/06     C:132    SUPPORT: 54161                    *
     *********************************************************************
     * REVISIONS:                                                        *
     *                                                                   *
     *********************************************************************
     Fsdcmp     uf   e             disk

     Fqsysprt   o    f  132        printer oflind(*inoa)

     D updtit          s              1    inz(' ')
     D xximgf          s              1    inz(' ')
     D totalrecs       s             11  0 inz(0)
     D totalmod        s              6  0 inz(0)
      *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0

      * If UPDTIT equal "Y", then update file and print changed records,
      * else if not equal "Y", just list records to be changed.
      *
     C                   eval      Updtit = 'Y'
     C                   time                    dsdttm
     C                   except    Head

     C                   dow       *inlr = *off
     C                   read      pcm01                                  60

     C                   if        *in60 = *on
     C                   eval      *inlr = *on
     C                   leave
     C                   endif

     C                   if        cmidt <= 1020701
     C                   iter
     C                   endif

     C                   eval      totalrecs = totalrecs + 1
     C                   if        cmimgf = 'O'
     C                   iter
     C                   endif

     C                   if        %subst(cmdcid:10:3) = 'PDF'
     C                             AND cmimgf <> 'O'
     C                   eval      totalmod = totalmod + 1

     C                   if        *inoa = *on
     C                   except    Head
     C                   eval       *inoa = *off
     C                   endif

     C                   move      cmimgf        xximgf
     C                   eval      cmimgf = 'O'
     C                   except    Detail

     C                   IF        UPDTIT = 'Y'
     C                   update    pcm01
     C                   ENDIF

     C                   endif

     C                   enddo

     C                   except    TotalLine
     C                   eval      *inlr = *on

     OQSYSPRT   E            Head           1  3
     O                       dsdttm              20 '  :  :  &  /  /    '
     O                                           70 'Correct Records with Incor-
     O                                              rect PDF Codes '
     O                                           90 'Update file:'
     O                       Updtit              92
     O                       PAGE               130
     O                                          124 'Page:'
     O          E            Head           1
     O                                           12 'Count'
     O                                           20 'CMIMGF'
     O                                           30 'CMDCID'
     O                                           44 'Date'
     O                                           54 'User'
     O                                           66 'Pgm '
     O                                           88 'Key'
     O          E            Detail         1
     O                       totalrecs     Z     12
     O                       xximgf              16
     O                       cmimgf              18
     O                       cmdcid              36
     O                       cmidt               47 '   /  /  '
     O                       cmuser              60
     O                       cmpgm               72
     O                       cmkey              125
     O          E            TotalLine   1  1
     O                                            8 'Total In'
     O                       totalrecs     Z     20
     O          E            TotalLine      1
     O                                            9 'Total Mod'
     O                       totalmod      Z     20
