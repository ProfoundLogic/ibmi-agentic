     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
      ***************************************************************
     D subject         s             55
     D address         s             40
     D attachPth       s             40
     D text            s            255
     D returnCode      s              1
     D
     D description     s             70
     D message         s            270
     D toUsrID         s             60
     D position        s              3  0
     D start           s                   like(position)
     D end             s                   like(position)
     D doc             s             20
     D docName         s             12
     D flr             s             40
     D flrName         s             30
     D command         s           1024
     D
     D/copy sprruncmd
     D
     C     *entry        plist
     C                   parm                    subject
     C                   parm                    address
     C                   parm                    attachPth
     C                   parm                    text
     C                   parm                    returnCode
     C
     C                   eval      start = 1
     C                   dou       end = 0
     C                   eval      start = end + 1
     C                   eval      position = end
     C                   eval      end = %scan('/':attachPth:start)
     C                   enddo
     C
     C                   eval      docname = %subst(attachPth:position+1)
     C                   eval      flrname = %subst(attachPth:2:position-2)
     C
     C                   eval      toUsrID = 'TOINTNET((' + %trim(address)
     C                             + '))'
     C                   eval      description = 'DSTD(''' + %trim(subject)
     C                             + ''')'
     C                   eval      message = 'MSG(''' + %trim(text) + ''')'
     C                   eval      doc = 'DOC(' + %trim(docName) + ')'
     C                   eval      flr = 'FLR(''' + %trim(flrName) + ''')'
     C
     C                   eval      command = 'SNDDST TYPE(*DOC) '
     C                             + %trim(toUsrID) + ' '
     C                             + %trim(description) + ' '
     C                             + %trim(message) + ' '
     C                             + %trim(doc) + ' '
     C                             + %trim(flr)
     C
     C                   callp(e)  runcmd(command : %len(command))
     C                   if        %error
     C                   eval      returnCode = '1'
     C                   endif
     C
     C                   eval      *inlr = *on
