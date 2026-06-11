     H**********************************************************************
     H* Revisions:
     H*
     H* 05/24/04 C:65  Project: sy100
     H*                Change all the folder definitions to 40 to match
     H*                WXICP.
     H*
     H**********************************************************************
     Fwxicp     if   e             disk
     D
     D/copy sprruncmd
     DgetDocNam        PR            12
     D incStmt                       78    Const
     D*** BEGIN CHG ***  05/24/04  *****************************************
     D*****getFlrNam        PR            12
     DgetFlrNam        PR            40
     D*** END CHG *****  05/24/04  *****************************************
     D incStmt                       78    Const
     DrtvDoc           PR
     D docName                       12    Const
     D*** BEGIN CHG ***  05/24/04  *****************************************
     D***** flrName                       12    Const
     D flrName                       40    Const
     D*** END CHG *****  05/24/04  *****************************************
     D
     C                   read      wxicp                                  99
     C                   dow       *in99 = *off
     C                   callp(e)  rtvDoc(getDocNam(icstmt):getFlrNam(icstmt))
     C                   if        %error
     C                   endif
     C                   read      wxicp                                  99
     C                   enddo
     C                   eval      *inlr = *on
     P*=====================================================================
     PgetDocNam        B
     DgetDocNam        PI            12
     D incStmt                       78    Const
     D
     D docName         s             12
     D pos             s              5  0
     D endPos          s                   like(pos)
     D length          s                   like(pos)
     C
     C                   movel     *blanks       docName
     C     '('           scan      incStmt       pos
     C                   if        pos > 0
     C                   add       1             pos
     C     ','           scan      incStmt:pos   endPos
     C     endPos        sub       pos           length
     C                   eval      docName = %subst(incStmt:pos:length)
     C                   endif
     C*
     C                   return    docName
     PgetDocNam        E

     P*=====================================================================
     PgetFlrNam        B
     D*** BEGIN CHG ***  05/24/04  *****************************************
     D*****getFlrNam        PI            12
     DgetFlrNam        PI            40
     D*** END CHG *****  05/24/04  *****************************************
     D incStmt                       78    Const
     D
     D*** BEGIN CHG ***  05/24/04  *****************************************
     D***** flrName         s             12
     D flrName         s             40
     D*** END CHG *****  05/24/04  *****************************************
     D pos             s              5  0
     D endPos          s                   like(pos)
     D length          s                   like(pos)
     C
     C                   movel     *blanks       flrName
     C     ','           scan      incStmt       pos
     C                   if        pos > 0
     C                   add       1             pos
     C     ','           scan      incStmt:pos   endPos
     C     endPos        sub       pos           length
     C                   eval      flrName = %subst(incStmt:pos:length)
     C                   endif
     C*
     C                   return    flrName
     PgetFlrNam        E

     P*=====================================================================
     PrtvDoc           B
     DrtvDoc           PI
     D doc                           12    Const
     D*** BEGIN CHG ***  05/24/04  *****************************************
     D***** flr                           12    Const
     D flr                           40    Const
     D*** END CHG *****  05/24/04  *****************************************
     D
     D command         s            200
     D
     D rtv             c                   'RTVDOC'
     D outFile         c                   ' OUTFILE(QTEMP/@RTVDOC)'
     D outMbr          c                   ' OUTMBR(*FIRST *ADD)'
     D outDtaTyp       c                   ' OUTDTATYP(*DOC)'
     C
     C*** BEGIN CHG ***  05/24/04  *****************************************
     C**********         eval      command = rtv + ' FROMDOC('
     C**********                   + %trim(doc) + ') FROMFLR('
     C**********                   + %trim(flr) + ')'
     C**********                   + outFile
     C**********                   + outMbr
     C**********                   + outDtaTyp
     C                   eval      command = rtv + ' FROMDOC('
     C                             + %trim(doc) + ') FROMFLR('''
     C                             + %trim(flr) + ''')'
     C                             + outFile
     C                             + outMbr
     C                             + outDtaTyp
     C*** END CHG *****  05/24/04  *****************************************
     C                   callp     runCmd(%trim(command):%len(%trim(command)))
     C                   return
     PrtvDoc           E

