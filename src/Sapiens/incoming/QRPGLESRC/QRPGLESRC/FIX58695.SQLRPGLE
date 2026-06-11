     H* (C) Copyright Progressive Data Solutions, 2007
     H/TITLE FIX58659                                                                       *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
    H*  CREATED: 07/02/07  C:188 Support: 58659 Remove precert/vendor bills sent          *
     H*                                    state in error.                                 *
     H*                                                                                    *
     H**************************************************************************************

     Ffelgl     uf   e           k disk
     Ffaeolg    if   e           k disk

     D LOGEOB          S                   like(LGEOB#)

     D kkco#           S                   like(lgco#)
     D kksub#          S                   like(lgsub#)
     D kkfnd           S                   like(lgfnd)
     D kkfyr           S                   like(lgfyr)
     D kkcase          S                   like(lgcase)
     D kkeob#          S                   like(lgeob#)

     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    
     C     *loval        setll     plg01
     C                   read      plg01
     C                   dow       not %eof(felgl)

     C                   eval      kkco# = lgco#
     C                   eval      kksub# = lgsub#
     C                   eval      kkfnd = lgfnd
     C                   eval      kkfyr = lgfyr
     C                   eval      kkcase = lgcase
     C                   eval      kkeob# = lgeob#

     C     eoky          chain     peo01
     C                   if        %found and
     C                             (eosys = 'U' or eosys = 'V')
     C                   eval      LOGEOB = lgeob#
     C                   exsr      ProcFiles
     C                   delete    plg01
     C                   endif
     C                   read      plg01
     C                   enddo
     C                   eval      *inlr = *on

     C     ProcFiles     begsr
     C/exec sql
     C+ delete from FEEHP where EHEOB# = :LOGEOB
     C/end-exec
     C/exec sql
     C+ delete from FEEUP where EUEOB# = :LOGEOB
     C/end-exec
     C/exec sql
     C+ delete from FEEPP where EPEOB# = :LOGEOB
     C/end-exec
     C/exec sql
     C+ delete from FEEDP where EDEOB# = :LOGEOB
     C/end-exec
     C/exec sql
     C+ delete from FEDHP where DHEOB# = :LOGEOB
     C/end-exec
     C/exec sql
     C+ delete from FEDUP where DUEOB# = :LOGEOB
     C/end-exec
     C/exec sql
     C+ delete from FEDPP where DPEOB# = :LOGEOB
     C/end-exec
     C                   endsr
