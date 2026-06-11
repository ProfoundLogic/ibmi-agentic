    H*  (C) Copyright Progressive Data Solutions, Inc., 2006         *
    H*                                                               *
    H*  F8041RFX - Fix eob notesd                                    *
    H*****************************************************************
    H*  Created on 10/08/2007 by c:62 for project 60491              *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*****************************************************************
    H*  Indicators used:                                             *
    H*                                                               *
    H*****************************************************************
    F*
    F*  Physical file definition (for maintaining record)
     Ffau0p     if   e             disk
     Ffau2l3    uf   e           k disk
    F*
     D i9i             s              1    dim(10)
     D i9o             s              1    dim(10)
     D ini             s              1    dim(18)
     D ino             s              1    dim(18)
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*================================================================
    D* Constants
    D*================================================================
     D lo              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D hi              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D test                          24A
     D  xxtype                        1A   overlay(test)
     D  kktrid                       15A   overlay(test:2)
     D  kklin#                        8S 0 overlay(test:17)

     D header        e ds                  extname(fau1p)
     D detail        e ds                  extname(fau2p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##name          s             10
     D ##rrn           s              9  0
     D #1rrn           s              9  0
     D @@mbr#          s             10
     D @@name          s             10
     D @@rrn           s              9
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxdcnt          s              9  0
     D xxtcnt          s              9  0
     D data500         s            500
     d*** BEGIN ADD *** 12/12/06 **************************************
     D c144            s              1
     d*** end   ADD *** 12/12/06 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    I*================================================================
    I*  /COPY required constants
    I*================================================================
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy2        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     u2ky          klist
     C                   kfld                    kktrid
     C                   kfld                    kklin#
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*****************************************************************
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   exsr      mainsr
    C*
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
    C*
     C                   exsr      pgmstr
    C*
     C                   read      pu001                                  98
     C                   dow       *in98 = *off
     C                   exsr      mvffwk
     C                   read      pu001                                  98
     C                   enddo
    C*
     C                   exsr      pgmend
    C*
     C                   endsr
    C*================================================================
    C* MVFFWK - Move flat file field to work files
    C*================================================================
     C     mvffwk        begsr
    C*
     C                   movel(p)  u0data        test
     C                   movel(p)  u0data        data500
    C*
     C                   exsr      writer
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   select
     C                   when      xxtype = '2'
     C     u2ky          chain     pu201
     C                   if        %found
     C                   eval      %subst(detail:510) = %subst(u0data:501)
     C                   eval      u2prfl = 'Q'
     C                   update    pu201
     C                   endif
     C                   endsl
    C*
     C                   endsr
