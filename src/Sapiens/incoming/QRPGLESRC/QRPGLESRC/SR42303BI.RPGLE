     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H/TITLE SR42303BI - ADDITIONAL ENTRY - Texas Endorsements WC420303B
      *----------------------------------------------------------------- *
      *                                                                   *
      * Called From:SR42303B                                            *
      *                                                                   *
      *----------------------------------------------------------------- *
      *Revisions:                                                        *
      *                                                                   *
      * 03/25/13  C:287 PROJECT WC2194                                    *
      *                 Created.                                          *
      *                                                                   *
      *----------------------------------------------------------------- *

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR42303BF cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

      **Physical file definition (for maintaining record)
     Fwledp     uf a e             disk
     F                                     rename(ped01:dbrcdf)
     Fwdrrl     if   e           k disk
     Fwledl     if   e           k disk    infds(edfds)

      **Group Master File
     Fwmfnl     if   e           k disk

      **Employer Master File
     Fwmeml     if   e           k disk

      **Employer Cross Reference
     Fwzexl     if   e           k disk

      **Employer Policy Year
     Fwdelp     if   e             disk

      **Employer Endorsement Forms
     Fwmmul     if   e           k disk

     d edfds           ds
     d  edrrn                397    400b 0

      *----------------------------------------------------------------- *
      * /COPY required constants
      *----------------------------------------------------------------- *

     D AddInF          c                   const('Additional +
     D                                     Information for ')
     D comapi          c                   'SA005R'
     D #ofscr          c                   3

      *----------------------------------------------------------------- *
      *Stand Alone Fields
      *----------------------------------------------------------------- *

     D optnm           s             40
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@mod2          s              1
     D @@rrn           s              9  0

     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3
     D @@form          s                   like(fmform) inz('WC420303B ')

     D kkco#           s                   like(fmco#)
     D kkfnd           s                   like(fmfnd)
     D kkemp#          s                   like(fmemp#)
     D kkdiv           s                   like(fmdiv)
     D kkfyr           s              3  0
     D kkagt#          s              5  0
     D kkform          s                   like(fmform)
     D ##conm          s             40
     D ##edn           s             80
     D ##anam          s             60
     D ##prod          s              5  0
     D ##pnam          s             60
     D endefd          s                   like(mueedt)

     C/COPY SCOPYMT01C

      *----------------------------------------------------------------- *
      * PLIST definitions
      *----------------------------------------------------------------- *

      **Entry parameters
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@mod2
    
      **Additional input program parameters
     C*    addpl         pList
     C*                  parm      *blanks       @@prds
     C*                  parm      *blanks       @@call
     C*                  parm                    @@co#
     C*                  parm                    @@fnd
     C*                  parm                    @@emp#
     C*                  parm                    @@div
     C*                  parm                    @@fyr

      **Company file/fund (group)
     C     compl         plist
     C                   parm                    kkco#
     C                   parm                    ##conm

      **Get Agent Information
     C     agtpl         plist
     C                   PARM                    kkco#
     C                   PARM                    kkfnd
     C                   PARM                    kkemp#
     C                   PARM                    kkdiv
     C                   PARM                    kkfyr
     C                   PARM                    kkagt#
     C                   PARM                    ##ANAM
     C                   PARM                    ##PROD
     C                   PARM                    ##PNAM

      *----------------------------------------------------------------- *
      * KLIST definitions
      *----------------------------------------------------------------- *

      **Employer Master file
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

      **Additional Information file
     C     edky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkform

      **Group Master file
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd

      **Employer Endorsement File
     C     muky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkform

      **Employer Cross Reference
     C     zexky         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv

      /Free
        //----------------------------------------------------------------- *
        // *LIKE DEFN statements
        //----------------------------------------------------------------- *

        //----------------------------------------------------------------- *
        //DLTPRE- Pre-processing before delete occurs
        //----------------------------------------------------------------- *
           Begsr dltpre ;

           Endsr ;

        //----------------------------------------------------------------- *
        //DSPLYR- Processing after displaying information
        //----------------------------------------------------------------- *
           Begsr dsplyr ;

           Endsr ;

        //----------------------------------------------------------------- *
        //ERRCK1- Error checking for srceen 1 (SCR01)
        //----------------------------------------------------------------- *
           Begsr  errck1 ;

           Endsr ;

        //----------------------------------------------------------------- *
        //ERRCK2- Error checking for srceen 2 (SCR02)
        //----------------------------------------------------------------- *
           Begsr errck2 ;

           Endsr ;

        //----------------------------------------------------------------- *
        //ERRCK3- Error checking for srceen 3 (SCR03)
        //----------------------------------------------------------------- *
           Begsr errck3 ;

           Endsr ;

        //----------------------------------------------------------------- *
        //MVDBSC- Move data base fields to screen fields
        //----------------------------------------------------------------- *
           Begsr mvdbsc ;

        //Default values if add mode
            If  @@mode = 'A' ;
              clear dbrcdf ;
            Else ;
              Chain edky ped01 ;
               If %found(wledl) ;
                SC2001 = %subst( FM2001 : 1 : 75 ) ; //use only the first 75 spaces from
                SC2002 = %subst( FM2002 : 1 : 75 ) ; //the 200 character fields
                SC2003 = %subst( FM2003 : 1 : 75 ) ;
                SC2004 = %subst( FM2004 : 1 : 75 ) ;
                SC2005 = %subst( FM2005 : 1 : 75 ) ;
                SC2006 = %subst( FM2006 : 1 : 75 ) ;
                SC2007 = %subst( FM2007 : 1 : 75 ) ;
                SC2008 = %subst( FM2008 : 1 : 75 ) ;
                SC2009 = %subst( FM2009 : 1 : 75 ) ;
                SC2010 = %subst( FM2010 : 1 : 75 ) ;
                SC2011 = %subst( FM2011 : 1 : 75 ) ;
                SC2012 = %subst( FM2012 : 1 : 75 ) ;
                SC2013 = %subst( FM2013 : 1 : 75 ) ;
                SC2014 = %subst( FM2014 : 1 : 75 ) ;
                SC2015 = %subst( FM2015 : 1 : 75 ) ;
                SC2016 = %subst( FM2016 : 1 : 75 ) ;
                SC2017 = %subst( FM2017 : 1 : 75 ) ;
                SC2018 = %subst( FM2018 : 1 : 75 ) ;
                SC2019 = %subst( FM2019 : 1 : 75 ) ;
                SC2020 = %subst( FM2020 : 1 : 75 ) ;
                SC2021 = %subst( FM2021 : 1 : 75 ) ;
                SC2022 = %subst( FM2022 : 1 : 75 ) ;
                SC2023 = %subst( FM2023 : 1 : 75 ) ;
                SC2024 = %subst( FM2024 : 1 : 75 ) ;
                SC2025 = %subst( FM2025 : 1 : 75 ) ;
                SC2026 = %subst( FM2026 : 1 : 75 ) ;
                SC2027 = %subst( FM2027 : 1 : 75 ) ;
                SC2028 = %subst( FM2028 : 1 : 75 ) ;
                SC2029 = %subst( FM2029 : 1 : 75 ) ;
                SC2030 = %subst( FM2030 : 1 : 75 ) ;

               Endif ;

            Endif ;

           Endsr ;

        //----------------------------------------------------------------- *
        //MVSCDB- Move screen fields to data base fields
        //----------------------------------------------------------------- *
           Begsr mvscdb ;

             FMCO# = %dec( @@co#: 3 : 0 ) ;
             FMFND = scfnd ;
             FMFYR = scfyr ;
             FMEMP# = %dec( @@emp# : 9 : 0 );
             FMDIV =  %dec( @@div : 5 : 0 ) ;
             FMFORM = @@form ;
             FM2001 = SC2001 ;
             FM2002 = SC2002 ;
             FM2003 = SC2003 ;
             FM2004 = SC2004 ;
             FM2005 = SC2005 ;
             FM2006 = SC2006 ;
             FM2007 = SC2007 ;
             FM2008 = SC2008 ;
             FM2009 = SC2009 ;
             FM2010 = SC2010 ;
             FM2011 = SC2011 ;
             FM2012 = SC2012 ;
             FM2013 = SC2013 ;
             FM2014 = SC2014 ;
             FM2015 = SC2015 ;
             FM2016 = SC2016 ;
             FM2017 = SC2017 ;
             FM2018 = SC2018 ;
             FM2019 = SC2019 ;
             FM2020 = SC2020 ;
             FM2021 = SC2021 ;
             FM2022 = SC2022 ;
             FM2023 = SC2023 ;
             FM2024 = SC2024 ;
             FM2025 = SC2025 ;
             FM2026 = SC2026 ;
             FM2027 = SC2027 ;
             FM2028 = SC2028 ;
             FM2029 = SC2029 ;
             FM2030 = SC2030 ;

           Endsr ;

        //----------------------------------------------------------------- *
        //OTHERR- Processing for other modes
        //----------------------------------------------------------------- *
           Begsr  otherr ;

           Endsr ;

        //----------------------------------------------------------------- *
        //OTHFKS- Processing for other functions keys
        //----------------------------------------------------------------- *
           Begsr othfks ;

           Endsr ;

        //----------------------------------------------------------------- *
        //PGMEND- End of program processing
        //----------------------------------------------------------------- *
           Begsr  pgmend ;

             If  *inkc = *On or *inkl = *on ;
                @@call = 'F' ;
             Endif ;

           Endsr ;

        //----------------------------------------------------------------- *
        //PGMSTR- Start of program processing
        //----------------------------------------------------------------- *
           Begsr  pgmstr ;

        //Get Mode
            kkco# = %dec( @@co#: 3 : 0 ) ;
            kkfnd = %dec( @@fnd : 3 : 0 ) ;
            kkemp# = %dec( @@emp# : 9 : 0 );
            kkdiv =  %dec( @@div : 5 : 0 ) ;
            kkfyr =  %dec( @@fyr : 3 : 0 ) ;
            kkform = @@form ;

           chain edky ped01 ;

            If %found( wledl ) ;
              @@mode = 'C' ;
              @@rrn  = edrrn ;
            Else ;
              @@mode = 'A' ;
              @@rrn  = 0 ;
            Endif ;

        //Get Heading Information
           Chain @@rrn pel01 ;
           Chain emky pem01 ;
             scfyr  = elfyr ;
             scfnd  = emfnd ;

        //Get Endorsement Effective Date
           Chain muky pmu01 ;
             endefd = mueedt ;

        //Get Fund Name
           scfnnm = *blanks ;
           Chain fnky pfn01 ;
            If %found(wmfnl) ;
               If fnanam <> *blanks ;
                  scfnnm = fnanam ;
               Else ;
                  scfnnm = fnname ;
               Endif ;
            Endif ;

            If elfaf = 'A' ;
              scpyst = 'Audited' ;
            Else ;
              scpyst = 'Estimated' ;
            Endif ;

      /End-Free

      **Get Agent/Producer Information
     c                   call      'WA002R'      AGTPL
      /Free
            scagnm = ##anam ;
            scpnam = ##pnam ;
            scagt# = kkagt# ;
            scprod = ##prod ;


        //Get Employer Cross Reference info for header

            Chain zexky pex01 ;

              If %found(wzexl) ;
                 scofnd = exofnd ;
                 scoemp = exoemp ;
                 scodiv = exodiv ;
               *In64 = *On ;
              Else ;
               *In64 = *Off ;
              Endif ;
    
            optnm = AddInf + %trim( @@form ) ;

            If q1prms >= 1 and @@prds <> *blanks ;
              Eval ##data = @@prds ;
            Else ;
              Eval ##data = optnm ;
            Endif ;

      /End-Free
      **Callp SRCENTER (Data : Len) ;
     C                   Call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
      /Free

            eval scprds = ##data;
            eval scfnd = kkfnd;
            eval scfyr = kkfyr;

            Chain fnky pfn01 ;
             scfnnm = fnname ;

            Chain emky pem01 ;
      /End-Free
      **Callp WA005R (ememp# : emdiv : emname : ##edn) ;

     C                   call      'WA005R'
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm                    emname
     C                   parm                    ##edn

      /Free
              eval scedn = ##edn ;

           Endsr ;

        //-----------------------------------------------------
        //PREOTH -Pre-processing for other modes
        //-----------------------------------------------------
           Begsr preoth ;

           Endsr ;

        //-----------------------------------------------------
        //UPDATR- Update data base record after processing
        //-----------------------------------------------------
           Begsr updatr ;
              Chain @@rrn dbrcdf ;
B01            If %Found(wledp) ;
                 Exsr mvscdb ;
                 fmuser = q1user ;
                 fmpgm = q1pgm ;
                 Exsr Getdtm ;
                 Eval fmdt = %dec( srdto : 7 : 0 ) ;
                 fmeedt = endefd ;
                 Update dbrcdf ;
E01            Endif ;
             outflg  = *on ;
        Endsr ;

        //----------------------------------------------------
        //WRITER- Write data base record after processing
        //----------------------------------------------------
           Begsr Writer ;
             Clear dbrcdf ;
             Exsr mvscdb ;
             fmuser = q1user ;
             fmiusr = q1user ;
             fmpgm = q1pgm ;
             fmipgm = q1pgm ;
               Exsr Getdtm ;
             Eval fmdt = %dec( srdto : 7 : 0 ) ;
             Eval fmidt = %dec( srdto : 7 : 0 ) ;
             fmeedt = endefd ;
             Write dbrcdf ;
             Feod wledp ;
             outflg = *on ;
           Endsr ;
      /End-Free

