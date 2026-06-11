     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
      *==================================================================================*
      *  DESCRIPTION: Direct Debit Information                                           *
      *  CALLED FROM: W1251R                                                             *
      *                                                                                  *
      *  CREATED: 01/14/2004  Matt Dawson  Project: WC1116                               *
      *==================================================================================*
      *  REVISIONS:                                                                      *
      *                                                                                  *
      *  06/14/04 C:136 Support: WC1115                                                  *
      *                 Modify edit to not invoke if merely changing an effective date   *
      *                 on the same record if the change doesn't overlap another record. *
      *==================================================================================*
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw1251f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmdbp     uf a e             disk
     F                                     rename(pdb01:dbrcdf)
     Fwmemp     if   e             disk
     Fwmfnl     if   e           k disk
     Fwzexl     if   e           k disk
     Fwmdbl3    uf   e           k disk    rename(pdb01:pdb03) prefix(l3_)
      *** beg add ***** 06/14/03 ***********************************************
     F                                     infds(dbinfds)
      *** end add ***** 06/14/03 ***********************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Direct Debit Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##emp#          s                   like(ememp#)
     D ##div           s                   like(emdiv)
     D ##name          s                   like(emname)
     D ##edn           s             80
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D sys_date        s               d   datfmt(*usa)
     D count           s              1  0
     D x               s              3  0
     D vdrCount        s              1  0
     D svefdt          s                   like(dbefdt)
      *** beg add ***** 06/14/04 **********************************************
     D kkefdt          s                   like(dbefdt)
      *** end add ***** 06/14/04 **********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
      *** beg add ***** 06/14/04 ***********************************************
     D dbinfds         ds
     D  dbrrn                397    400b 0
      *** end add ***** 06/14/04 ***********************************************
    C*
     C/copy sprstring
     C/copy sprdate
     C/copy scopymt41c
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     c     dbky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    ememp#
     C                   kfld                    emdiv
      *** beg chg ***** 06/14/04 **********************************************
     C*****************  kfld                    dbefdt
     C                   kfld                    kkefdt
      *** beg chg ***** 06/14/04 **********************************************

     c     emky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    ememp#
     C                   kfld                    emdiv

     c     fnky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
      * Validate bank acct
      *
     c                   if        scdfia = *blanks
     c                   seton                                        71  90
     c                   endif
      *
      * Validate check or savings flag
      *
     c  n90              if        sccsfl <> 'C' and
     c                             sccsfl <> 'S'
     c                   seton                                        72  90
     c                   endif
      *
      * Validate DFI number
      *
     c  n90              if        scdfii <= 0
     c                   seton                                        73  90
     c                   endif
      *
      * Validate check digit
      *
     c  n90              if        sctrcd < 0
     c                   seton                                        74  90
     c                   endif
      *
      * Validate effective date
      *
     c                   if        not *in90
     c                   eval      scefdt = dtCheck(scefdt)
     c                   if        scefdt = *zero
     c                   seton                                        75  90
     c                   endif
     c                   endif
      *
      * Endsure that new record doesn't overlap preexisting records
      *
     c                   if        not *in90
      *** beg add ***** 06/14/04 **********************************************
     c*****************  eval      dbefdt = dtCYMD(scefdt)
     c                   eval      kkefdt = dtCYMD(scefdt)
      *** end add ***** 06/14/04 **********************************************
     c     dbky          setgt     pdb03
     c     emky          reade(n)  pdb03
      *** beg chg ***** 06/14/04 **********************************************
     c*****************  if        not %eof
     c                   if        not %eof and @@mode = 'A' or
     c                             not %eof and @@mode = 'C' and
     c                             @@rrn <> dbrrn and kkefdt >= l3_dbefdt
      *** end chg ***** 06/14/04 **********************************************
     c                   seton                                        76  90
     c                   endif

     c                   if        not *in90
     c     dbky          setll     pdb03
     c     emky          readpe(n) pdb03

      *** beg chg ***** 06/14/04 **********************************************
     c*****************  if        not %eof
     c*****************  if        dbefdt < l3_dbefdt or
     c*****************            dbefdt < l3_dbexdt
     c                   if        not %eof and @@mode = 'A' or
     c                             not %eof and @@mode = 'C' and
     c                             @@rrn <> dbrrn

     c                   if        kkefdt < l3_dbefdt or
     c                             kkefdt < l3_dbexdt
      *** end chg ***** 06/14/04 **********************************************
     c                   seton                                        76  90
     c                   endif

     c                   endif                                                  not %eof

     c                   endif                                                  not *in90

     c                   endif                                                  not *in90
      *
      * Validate expiration date
      *
     c                   if        not *in90 and scexdt <> 0
     c                   eval      scexdt = dtCheck(scexdt)

     c                   if        scexdt = *zero
     c                   seton                                        77  90
     c                   endif

     c                   if        not *in90
      *
      * Ensure expiration date is not less than effective date
      *
     c                   eval      dbexdt = dtCymd(scexdt)
     c                   eval      dbefdt = dtCymd(scefdt)
     c                   if        dbexdt < dbefdt
     c                   seton                                        78  90
     c                   endif

     c                   endif

     c                   endif
      *
      * Validate prenotification date
      *
     c                   if        not *in90 and scpred <> 0
     c                   eval      scpred = dtCheck(scpred)
     c                   if        scpred = *zero
     c                   seton                                        79  90
     c                   endif
     c                   endif
      *
      * Informational message if clearing a prenote date
      *
     c                   if        not *in90 and scpred = 0 and count = 0
     c                   eval      count  = 1
     c                   if        scpred = 0 and dbpred <> 0
     c                   seton                                        80  90
     c                   endif
     c                   endif

     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      svefdt = dbefdt
     c                   eval      scdfia = dbdfia
     c                   eval      sccsfl = dbcsfl
     c                   eval      scdfii = dbdfii
     c                   eval      sctrcd = dbtrcd
     c                   eval      scefdt = dtMDYY(dbefdt)
     c                   eval      scexdt = dtMDYY(dbexdt)
     c                   eval      scpred = dtMDYY(dbpred)
     c                   eval      sc1std = dtMDYY(db1std)
     c                   eval      sclstd = dtMDYY(dblstd)
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c                   eval      dbco#  = emco#
     c                   eval      dbfnd  = emfnd
     c                   eval      dbemp# = ememp#
     c                   eval      dbdiv  = emdiv
     c                   eval      dbdfia = scdfia
     c                   eval      dbcsfl = sccsfl
     c                   eval      dbdfii = scdfii
     c                   eval      dbtrcd = sctrcd
     c                   eval      dbefdt = dtCYMD(scefdt)
     c                   eval      dbexdt = dtCYMD(scexdt)
     c                   eval      dbpred = dtCYMD(scpred)
     c                   eval      db1std = dtCYMD(sc1std)
     c                   eval      dblstd = dtCYMD(sclstd)
     c                   if        @@mode = 'A'
     c                   eval      dbstat = 'A'
     c                   endif
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     *usa          move      *date         sys_date
     c     @@rrn2        chain     wmemp
     C     fnky          chain     wmfnl
     c                   eval      scfnd  = fnfnd
     c                   eval      scname = fnname

     c                   call      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c     scedn         parm                    ##edn

     C     emky          chain     wzexl                              63
     c                   if        not *in63
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv
     C                   endif

     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     c                   eval      dbuser = q1user
     c                   eval      dbpgm  = q1pgm
     C     *cymd         move      sys_date      dbdt
     C                   time                    dbtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   eval      dbuser = q1user
     c                   eval      dbiusr = q1user
     c                   eval      dbpgm  = q1pgm
     c                   eval      dbipgm = q1pgm
     C     *cymd         move      sys_date      dbdt
     c                   eval      dbidt  = dbdt
     C                   time                    dbtime
     c                   eval      dbitm  = dbtime
     C                   write     dbrcdf
      *
      * If adding an additional record to an employer update the expiration date on the previous
      * record if the expiration date is zero. Two readpe in case previous record has same
      * effective date which would make a setll and readpe miss the record.
      *
     C     dbky          setgt     pdb03
     C     emky          readpe    pdb03
     C     emky          readpe    pdb03
     C                   if        not %eof and l3_dbexdt = 0
     C                   eval      l3_dbexdt = dbefdt
     C                   update    pdb03
     C                   endif

     C                   feod      wmdbp
     C                   endsr
