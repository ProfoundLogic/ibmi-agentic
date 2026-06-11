    H* (c) 2009 StoneRiver, Inc. All rights Reserved.                     *
    H* StoneRiver is a trademark of StoneRiver, Inc.                      *
    H*********************************************************************
    H/TITLE FIXAGRFL -  Fix program for Agent Rate flag
    H*********************************************************************
    H* CALLED FROM:  WCPOSTCV                                            *
    H*                                                                   *
    H* CALLS TO:                                                         *
    H*   NONE                                                            *
    H*                                                                   *
    H* CREATED:  07/08/09     C:189    PROJECT: 114WUD                   *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fwmexl     uf   e           k disk
     Dpsds           esds                  extname(szq1p)
     D qqdev           S             10
     D qqerr           S              1
     D qqtype          S              7
     D @@lib           S             10    inz('PDSOBJ')
     D @@cmd1          S             17    inz('CRTDTAARA DTAARA(')
     D @@cmd2          S             29    inz('/FIXAGRFL) TYPE(*CHAR) LEN(1)')
     D @@cmd           S             60


     C     *dtaara       define    FIXAGRFL      FIXDTA            1
     C                   movel(p)  'FIXAGRFL'    qqdev
     C                   movel(p)  '*DTAARA'     qqtype

     C*Check for the existance of data area

     C                   call      'SRCHKOBJ'
     C                   parm                    qqdev
     C                   parm                    qqtype
     C                   parm                    qqerr
     C*
     C                   if        qqerr = *on
     C*create the data area
     C*
     c                   call      'SA030R'
     c                   parm      *blanks       @suffix           3
     C*
     C                   Eval      @@lib = %trim(@@lib) + @suffix
     C*
     C                   Eval      @@cmd = @@cmd1 + %trim(@@lib) + @@cmd2
     C*
     C                   call      'QCMDEXC'
     C                   parm      @@cmd         cmdstr           60
     C                   parm      60            length           15 5
     C*
     C                   endif
     C*
     C*load the data area
     C*
     C     *lock         in        FIXDTA
     C*
     C                   if        FIXDTA   = ' '
     C*
     C     *loval        setll     pex01
     C                   read      pex01
     C*
     C                   dow       not %eof
     C                   If        exrafl <> *BLANKS
     C                   eval      exaafl = exrafl
     C                   update    pex01
     C                   endif
     C*
     C                   read      pex01
     C                   enddo
     C*
     C*Update the data area value
     C                   eval       FIXDTA = 'Y'
     C                   out       FIXDTA
     C*
     C                   eval      *inlr = *on
     C                   else
     C                   eval      *inlr = *on
     C                   endif
