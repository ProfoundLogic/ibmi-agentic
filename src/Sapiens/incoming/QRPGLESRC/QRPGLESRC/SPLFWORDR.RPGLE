     Fspool     if   f  301        disk

     DnewDocName       s             30
     DnotFirst         s               n
     DtmpltFile        c                   'templates\spoolFile.dot"'
     D
     D/copy sprlnchr
     D
     Ispool     ns
     I                                  1    1  code
     I                                  2  113  data
     C     *entry        plist
     C                   parm                    newDocName
     C
     C                   callp     openLnchr
     C
     C                   callp     openWord
     C                   callp     newDocument(%trim(getShlDir) + tmpltFile)
     C
     C                   read      spool                                  99
     C                   dow       *in99 = *off
     C                   select
     C                   when      code = '+'
     C                   eval      data = ' '
     C                   when      code = '-'
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   when      code = '0'
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   when      code = ' '
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   when      code = '1'
     C                   if        notFirst
     C                   callp     rplBookMrk('*DOWN' : '%PARA%')
     C                   callp     pageBreak
     C                   endif
     C                   eval      notFirst = *on
     C                   endsl
     C                   if        data <> ' '
     C                   callp     insertText(data)
     C                   endif
     C                   read      spool                                  99
     C                   enddo
     C
     C                   callp     saveAs(%trim(getGenDir) +
     C                             %trim(newDocName) + '"')
     C                   callp     closeWord
     C                   callp     endComm
     C                   callp     closeLnchr
     C
     C                   eval      *inlr = *on
     C
