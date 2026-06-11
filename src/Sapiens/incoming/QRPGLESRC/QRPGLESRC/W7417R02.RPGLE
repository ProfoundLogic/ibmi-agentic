      * (C) Copyright Progressive Data Solutions, Inc., 2005
      *
      *****************************************************************
      *  DESCRIPTION:  group selection maintenance program            *
      *                                                               *
      *                                                               *
      *  CREATED: 04/03/06  programmer: c:168  project: 114WLK        *
      *    COPY OF:  01/10/05  programmer: c:65 project: 93wew        *
      *****************************************************************
      * Revised:
      *
      *  04/03/06   c:168   project 114WLK
      *                     - copied this program from client 93 to base
      *
      *****************************************************************
     *
    H*
     Fw7417f02  cf   e             workstn
    F*
    F*  Physical file definition (for maintaining record)
     Fwxpgp     uf a e             disk
     F                                     rename(xpg01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Group Selection Maintenance'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

    
     C/COPY SCOPYMT01C
    
    
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
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


    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================


    C*================================================================
    C* CHGSTS - UPDATE WXPGP WITH NEW STATUS FLAG
    C*================================================================
     C     Chgsts        Begsr

     C     @@rrn         Chain     Wxpgp

     C                   If        %found

     C                   If        Pgflag = 'E'
     C                   Eval      Pgflag = 'I'

     c                   else

     C                   Eval      Pgflag = 'E'
     C                   Endif

     C                   Update    dbrcdf
     C                   Endif

     C                   endsr


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
     C                   endsr


    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
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
     c                   move      *on           *inlr
     c                   return
     C                   endsr


    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    
    
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
      *
     C                   If        @@mode = 'S'
     C                   Exsr      Chgsts
     c                   move      *on           *inlr
     C                   Return
     C                   Endif

     C                   endsr
    
    
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    
    
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wxpgp
     C                   endsr
    
    
