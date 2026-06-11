    H*  (C) Copyright Progressive Data Solutions, Inc., 2001
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
    F*
     Fmmopl     uf   e           k disk    rename(pop01:pop00)
    F*
     D psds          esds                  extname(szq1p)

     D/copy sprdate
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@mnam          s                   like(OPMNAM)
     D @@optn          s                   like(OPOPTN)
     D @@pgm           s                   like(OPPGM)
     D kkmnam          s                   like(OPMNAM)
     D kkoptn          s                   like(OPOPTN)
     D sysDate         s               d
     D sysTime         s               t
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     C     *entry        plist
     C                   parm                    @@mnam
     C                   parm                    @@optn
     C                   parm                    @@pgm
     C*
     C* NOTE: If @@pgm is entered, it will be loaded to OPPGM instead of
     C*       q1pgm.
     C*____________________________________________________________________
     C*
     C     moky          klist
     C                   kfld                    kkmnam
     C                   kfld                    kkoptn
     C*____________________________________________________________________
     C*
     C                   clear                   kkmnam
     C                   clear                   kkoptn
     C*
     C                   if        %parms >= 1
     C                   eval      kkmnam = @@mnam
     C                   endif
     C*
     C                   if        %parms >= 2
     C                   eval      kkoptn = @@optn
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      q1pgm  = @@pgm
     C                   endif
     C*____________________________________________________________________
     C*
     C     moky          chain     pop00                              60
     C*
     C                   if        not *in60
     C                   eval      OPSTAT = 'I'
     C                   exsr      maintFlds
     C                   update    pop00
     C                   endif
     C*____________________________________________________________________
     C*
     C                   eval      *inlr = *on
     C*____________________________________________________________________
     C*
    C*====================================================================
    C* maintFlds - Update maintenance fields
    C*====================================================================
     C*
     C     maintFlds     begsr
     C*
     C                   eval      sysDate = systemDate
     C                   eval      sysTime = systemTime
     C*
     C                   eval      OPUSER = q1user
     C                   eval      OPPGM  = q1pgm
     C     *cymd         move      sysDate       OPDT
     C     *hms          move      sysTime       OPTIME
     C*
     C                   endsr
