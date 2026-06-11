     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *============================================================================================*
      /title W2143R02 - Commission Mod Factor Info                                                 *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      *  CALLED FROM: W2413R                                                                       *
      *                                                                                            *
      *  CREATED 12/18/05   C:136   PROJECT: 136WBH                                                *
      *============================================================================================*
      *  REVISIONS:                                                                                *
     H*                                                                                            *
     H* 01/13/14 C:190 Project: 136WFZ                                                             *
     H*                - Convert to CompSuite.                                                     *
     H*                                                                                            *
      *=============================================================================================
     Fw2143f02  cf   e             workstn
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
    
    F*  Physical file definition (for maintaining record)
     Fwtmfp     uf a e             disk    rename(pmf01 : dbrcdf)
    
     Fwtmfl     if   e           k disk    rename(pmf01 : pmf0l)
     Fwmfnp     if   e             disk    rename(pfn01 : pfn0p)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Commission Mod Factor'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*================================================================
    D* Stand Alone Fields - Required by copy code
    D*================================================================
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D kkefdt          s                   like(MFEFDT)
    D*================================================================
    D* Stand Alone Fields - Unique to program
    D*================================================================
     D/copy SPRDATE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
     D/copy WPRGROUP
     C/copy SCOPYMT41C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn1
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
     C     mfky          klist
     C                   kfld                    ##co#
     C                   kfld                    fnfnd
     C                   kfld                    scrate
     C                   kfld                    kkefdt
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

     C                   if        scrate < 0
     C                   seton                                        71  90
     C                   endif

     C  n90              if        scfctr < 0
     C                   seton                                        72  90
     C                   endif

     C  n90              if        DtCheck(Dateto8(CvtToDate(scefdt))) = 0
     C                   seton                                        74  90
     C                   else
     C                   eval      scefdt = DtCheck(scefdt)
     C                   endif

     C  n90              if        @@mode = 'A'
     C                   eval      kkefdt = DtCYMD(scefdt)
     C     mfky          chain     pmf0l
     C                   if        %found
     C                   seton                                        73  90
     C                   endif
     C                   endif

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
     C                   eval      scrate = MFRATE
     C                   eval      scfctr = MFFCTR
     C                   eval      scefdt = DtMDYY(MFEFDT)
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      MFCO#  = ##co#
     C                   eval      MFFND  = scfnd
     C                   eval      MFRATE = scrate
     C                   eval      MFFCTR = scfctr
     C                   eval      MFEFDT = DtCYMD(scefdt)
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
     C                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@rrn1        chain     pfn0p
     C                   eval      scfnd  = fnfnd
     C                   eval      scfname = GetGrpName(FNFND)
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
     C                   eval      MFUSER = Q1USER
     C                   eval      MFPGM  = Q1PGM
     C                   eval      MFDT   = dateTo7(SystemDate)
     C                   eval      MFTIME = timeTo6(SystemTime)
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      MFUSER = Q1USER
     C                   eval      MFPGM  = Q1PGM
     C                   eval      MFDT   = dateTo7(SystemDate)
     C                   eval      MFTIME = timeTo6(SystemTime)
     C                   eval      MFIDT  = dateTo7(SystemDate)
     C                   eval      MFIPGM = Q1PGM
     C                   write     dbrcdf
     C                   feod      wtmfp
     C                   endsr
