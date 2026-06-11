     H copyright('Progressive Data Solutions, Inc 2000')
     Fqaofenr2  uf a e           k disk    usropn

     DuserID           s              8
     Daddress          s              8
     DrunOnce          s              1n
     D psds          esds                  extname(szq1p)
     DgetDirInfo       PR                  extproc('SS025C')
     D dirUsr                         8    Const
     D dirAddr                        8    Const
     D

     C                   if        not runOnce
     C                   open      qaofenr2
     C     q1user        chain(n)  qaofenr2                           99
     C                   if        *in99 = *off
     C                   if        ocalwc <> 'Y'
     C     q1user        chain     qaofenr2                           99
     C                   movel(p)  'Y'           ocalwc
     C                   update    enrprec
     C                   endif
     C                   else
     C                   callp     getDirInfo(userid:address)
     C                   movel(p)  userID        enrden
     C                   movel(p)  address       enrdgn
     C                   movel(p)  q1user        uname
     C                   movel(p)  q1user        icnme
     C                   movel(p)  userID        iden
     C                   movel(p)  address       idgn
     C                   movel(p)  q1user        scclnm
     C                   movel(p)  q1user        idname
     C                   movel(p)  'Y'           ocalwc
     C                   write     enrprec
     C                   endif
     C                   close     qaofenr2
     C                   eval      runOnce = *on
     C                   endif

     C                   return
